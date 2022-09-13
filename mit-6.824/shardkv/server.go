package shardkv

import (
	"fmt"
	"strconv"
	"sync"
	"time"

	"6.824/labgob"
	"6.824/labrpc"
	"6.824/raft"
	"6.824/shardctrler"
)

func (kv *ShardKV) IsDuplicate(clientId int64, msgId int64) bool {
	lastProcessed, ok := kv.lastProcessed[clientId]
	if !ok || lastProcessed < msgId {
		kv.lastProcessed[clientId] = msgId
		return false
	}
	return true
}

func (kv *ShardKV) checkShard(command Op) bool {
	if command.OpCode == GET || command.OpCode == PUT || command.OpCode == APPEND {
		return kv.lastConfig.Num > 0 &&
			//command.VersionId == kv.version[key2shard(command.Key)] &&
			kv.newConfig.Num == kv.version[key2shard(command.Key)]
	}
	return true
}

func (kv *ShardKV) getValue(command Op) (string, Err) {
	if command.OpCode == GET {
		if val, ok := kv.data[key2shard(command.Key)][command.Key]; ok {
			return val, OK
		} else {
			return "", ErrNoKey
		}
	}
	return "", OK
}

func (kv *ShardKV) Commit(command Op) (string, Err) {
	kv.Lock("Commit")
	defer kv.Unlock()
	if command.Key != "" {
		command.VersionId = kv.version[key2shard(command.Key)]
	}
	if !kv.checkShard(command) {
		return "", ErrWrongGroup
	}

	if kv.lastProcessed[command.ClientId] >= command.MsgId {
		return kv.getValue(command)
	}

	index, _, is_leader := kv.rf.Start(command)
	if !is_leader {
		return "", ErrWrongLeader
	}
	currentTime := time.Now()
	for index > kv.lastAppliedIndex {
		kv.stateChange.Wait()
		if _, is_leader = kv.rf.GetState(); !is_leader || time.Since(currentTime) > 4*time.Second {
			return "", ErrWrongLeader
		}
		if !kv.checkShard(command) {
			return "", ErrWrongGroup
		}
	}
	return kv.getValue(command)
}

func (kv *ShardKV) Get(args *GetArgs, reply *GetReply) {
	op := Op{OpCode: GET, Key: args.Key, ClientId: args.ClientId, MsgId: args.MsgId}
	reply.Value, reply.Err = kv.Commit(op)
}

func (kv *ShardKV) PutAppend(args *PutAppendArgs, reply *PutAppendReply) {
	op := Op{OpCode: args.Op, Key: args.Key, ClientId: args.ClientId, MsgId: args.MsgId, Value: args.Value}
	_, reply.Err = kv.Commit(op)
}

func (kv *ShardKV) DeleteShards(args *GetShardsArgs, reply *GetShardsReply) {
	_, reply.Err = kv.Commit(Op{ClientId: args.ClientId,
		DeleteShardVersion: args.ShardVersion, MsgId: args.MsgId, OpCode: DELETESHARDS, ShardIds: args.ShardIds})
}

func (kv *ShardKV) anyOld(shardIds []int, v int) bool {
	myShards := find_indices(shardIds, kv.gid)
	init := false
	for _, shardId := range myShards {
		init = init || kv.version[shardId] < v
	}
	return init
}

// what is the meaning of kv.newConfig.Num < args.ShardVersion+1 ?
// receiver of GetShards message must see new configuration before replying
// when it sees newconfiguration it stops replying to requests for shards it does not own in new configuration
// therefore it is safe to send these shards to new owners
// moreover if it seen new configuration - shards from old configuration must be complete
func (kv *ShardKV) GetShards(args *GetShardsArgs, reply *GetShardsReply) {
	kv.Lock("GetShards")
	defer kv.Unlock()

	for kv.newConfig.Num < args.ShardVersion+1 {
		kv.stateChange.Wait()
	}
	data := make(map[int]map[string]string)
	for _, shardid := range args.ShardIds {
		data[shardid] = cloneData(kv.data[shardid])
	}
	*reply = GetShardsReply{OK, cloneLastProcessed(kv.lastProcessed), data}
}

func (kv *ShardKV) sendToPeers(name string, servers []string, args *GetShardsArgs) GetShardsReply {
	for {
		for si := 0; si < len(servers); si++ {
			var reply GetShardsReply
			srv := kv.make_end(servers[si])
			ok := srv.Call(name, args, &reply)
			if ok && (reply.Err == OK) {
				return reply
			}
		}
		time.Sleep(50 * time.Millisecond)
	}
}

func (kv *ShardKV) sendGetShards(lastConfig *shardctrler.Config, newConfig *shardctrler.Config) {
	newShards := NewShards(newConfig, lastConfig, kv.gid)
	var m = make(map[int][]int)
	for _, shardId := range newShards {
		gid := lastConfig.Shards[shardId]
		m[gid] = append(m[gid], shardId)
	}
	var wg sync.WaitGroup
	wg.Add(len(m))
	for groupid, shardIds := range m {
		go func(gid int, sids []int) {
			args := &GetShardsArgs{Header: Header{int64(kv.gid), int64(newConfig.Num)}, ShardIds: sids, ShardVersion: lastConfig.Num}
			reply := kv.sendToPeers("ShardKV.GetShards", lastConfig.Groups[gid], args)
			clientId, _ := strconv.Atoi(fmt.Sprint(kv.gid) + fmt.Sprint(gid)) // in order to avoid duplicate detection
			kv.Commit(mkNewShardOp(clientId, newConfig, reply.Data, reply.LastProcessed))
			wg.Done()
			kv.sendToPeers("ShardKV.DeleteShards", lastConfig.Groups[gid],
				&GetShardsArgs{Header: Header{int64(kv.gid), int64(newConfig.Num)}, ShardIds: sids, ShardVersion: newConfig.Num})
		}(groupid, append([]int(nil), shardIds...))
	}
	wg.Wait()

}

func (kv *ShardKV) configTracker() {
	time.Sleep(50 * time.Millisecond)
	if _, is_leader := kv.rf.GetState(); !is_leader {
		return
	}
	kv.Lock("configTracker1")
	lastConfig := CloneConfig(&kv.lastConfig)
	newConfig := CloneConfig(&kv.newConfig)
	old := kv.anyOld(newConfig.Shards[:], newConfig.Num)
	kv.Unlock()

	newShards := NewShards(&newConfig, &lastConfig, kv.gid)
	if lastConfig.Num != 0 && old && len(newShards) > 0 {
		kv.sendGetShards(&lastConfig, &newConfig)
	} else if lastConfig.Num == 0 || (lastConfig.Num == newConfig.Num) {
		newConfig2 := kv.sm.Query(newConfig.Num + 1)
		if newConfig2.Num != newConfig.Num {
			command := Op{OpCode: NEWCONFIG, ClientId: int64(kv.gid), MsgId: int64(newConfig2.Num), NewConfig: newConfig2}
			kv.Commit(command)
		}
	}
}

func (kv *ShardKV) processSnapshot(msg *raft.ApplyMsg) {
	kv.Lock("processSnapshot")
	defer kv.Unlock()
	if kv.rf.CondInstallSnapshot(msg.SnapshotTerm, msg.SnapshotIndex, msg.Snapshot) {
		kv.installSnapshot(msg.Snapshot)
		kv.lastAppliedIndex = msg.SnapshotIndex
		kv.stateChange.Broadcast()
	}
}

func (kv *ShardKV) processCommand(command Op) {
	if command.OpCode == PUT {
		kv.data[key2shard(command.Key)][command.Key] = command.Value
	} else if command.OpCode == APPEND {
		kv.data[key2shard(command.Key)][command.Key] += command.Value
	}
}
func (kv *ShardKV) processConfig(msg Op) {
	if kv.lastConfig.Num == 0 {
		for _, shardid := range find_indices(msg.NewConfig.Shards[:], kv.gid) {
			kv.version[shardid] = msg.NewConfig.Num
		}
		kv.newConfig = msg.NewConfig
		kv.lastConfig = msg.NewConfig
	} else {
		sameShards := SameShards(&msg.NewConfig, &kv.lastConfig, kv.gid)
		for _, sameShardId := range sameShards {
			kv.version[sameShardId] = msg.NewConfig.Num
		}
		newShards := NewShards(&msg.NewConfig, &kv.lastConfig, kv.gid)
		if len(newShards) == 0 {
			kv.lastConfig = msg.NewConfig
		}
		kv.newConfig = msg.NewConfig
	}
}

func (kv *ShardKV) processNewShard(msg Op) {
	for shard_id, data := range msg.Data {
		kv.data[shard_id] = cloneData(data)
		kv.version[shard_id] = kv.newConfig.Num
	}
	for clientId, msg_id := range msg.LastProcessed {
		if kv.lastProcessed[clientId] < msg_id {
			kv.lastProcessed[clientId] = msg_id
		}
	}
	if !kv.anyOld(kv.newConfig.Shards[:], kv.newConfig.Num) {
		kv.lastConfig = kv.newConfig
	}
}

func (kv *ShardKV) processDeletion(msg Op) {
	for _, shard := range msg.ShardIds {
		if kv.version[shard] < msg.DeleteShardVersion {
			kv.data[shard] = make(map[string]string)
		}
	}
}

func (kv *ShardKV) process(msg *raft.ApplyMsg) {
	command := msg.Command.(Op)
	kv.Lock("processC")
	defer kv.Unlock()
	kv.lastAppliedIndex = msg.CommandIndex

	if kv.checkShard(command) && !kv.IsDuplicate(command.ClientId, command.MsgId) {
		if command.OpCode == NEWSHARD {
			kv.processNewShard(command)
		} else if command.OpCode == DELETESHARDS {
			kv.processDeletion(command)
		} else if command.OpCode == NEWCONFIG {
			kv.processConfig(command)
		} else {
			kv.processCommand(command)
		}
		if (kv.maxraftstate) != -1 && kv.persister.RaftStateSize() > kv.maxraftstate {
			buffer := kv.createSnapshot()
			kv.rf.Snapshot(kv.lastAppliedIndex, buffer)
		}
		kv.stateChange.Broadcast()
	}
}

func (kv *ShardKV) applier() {
	for {
		select {
		case <-time.After(300 * time.Millisecond):
			kv.stateChange.Broadcast()

		case msg := <-kv.applyCh:
			{
				if msg.CommandValid {
					kv.process(&msg)
				} else if msg.SnapshotValid {
					kv.processSnapshot(&msg)
				} else {
					panic("unidentified message")
				}
			}
		}
	}
}

func StartServer(servers []*labrpc.ClientEnd, me int, persister *raft.Persister,
	maxraftstate int, gid int, ctrlers []*labrpc.ClientEnd, make_end func(string) *labrpc.ClientEnd) *ShardKV {

	labgob.Register(Op{})
	labgob.Register(PutAppendArgs{})
	labgob.Register(PutAppendReply{})
	labgob.Register(GetArgs{})
	labgob.Register(GetReply{})

	kv := new(ShardKV)
	kv.me = me
	kv.maxraftstate = maxraftstate
	kv.make_end = make_end
	kv.gid = gid
	kv.ctrlers = ctrlers

	kv.applyCh = make(chan raft.ApplyMsg)
	kv.rf = raft.Make(servers, me, persister, kv.applyCh)
	kv.persister = persister

	kv.sm = shardctrler.MakeClerk(kv.ctrlers)

	kv.lastConfig = kv.sm.Query(0)
	kv.newConfig = kv.lastConfig
	for i := 0; i < shardctrler.NShards; i++ {
		kv.data = append(kv.data, make(map[string]string))
	}

	kv.stateChange = *sync.NewCond(&kv.mu)
	kv.lastAppliedIndex = 0

	kv.lastProcessed = make(map[int64]int64)
	go loop(kv.configTracker)
	go loop(kv.applier)

	return kv
}
