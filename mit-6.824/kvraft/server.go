package kvraft

import (
	"bytes"
	"log"
	"sync"
	"sync/atomic"
	"time"

	"6.824/labgob"
	"6.824/labrpc"
	"6.824/raft"
)

type Op struct {
	ClientId int64
	MsgId    int64
	Key      string
	Value    string
	Op       string // "Put" or "Append"
}

type CacheItem struct {
	MsgId    int64
	LogIndex int64
}

type KVServer struct {
	mu               sync.Mutex
	me               int
	rf               *raft.Raft
	applyCh          chan raft.ApplyMsg
	dead             int32 // set by Kill()
	maxraftstate     int   // snapshot if log grows this big
	data             map[string]string
	lastAppliedIndex int
	lastProcessed    map[int64]CacheItem
	stateChange      sync.Cond
}

func (kv *KVServer) SetLastProcessed(clientId int64, item CacheItem) bool {
	lastProcessed, ok := kv.lastProcessed[clientId]
	if !ok || lastProcessed.MsgId < item.MsgId {
		kv.lastProcessed[clientId] = item
		return true
	}
	return false
}

func (kv *KVServer) Helper(command Op) (string, Err) {
	_, is_leader := kv.rf.GetState()
	if !is_leader {
		return "", ErrWrongLeader
	}
	kv.Lock("Helper")
	defer kv.Unlock()
	item := kv.lastProcessed[command.ClientId]
	index := int(item.LogIndex)
	if item.MsgId < command.MsgId {
		if index, _, is_leader = kv.rf.Start(command); !is_leader {
			return "", ErrWrongLeader
		}
	}
	currentTime := time.Now()
	for index > kv.lastAppliedIndex {
		kv.stateChange.Wait()
		if _, is_leader = kv.rf.GetState(); !is_leader || kv.killed() || time.Since(currentTime) > 2*time.Second {
			return "", ErrWrongLeader
		}
	}

	if val, ok := kv.data[command.Key]; ok {
		return val, OK
	} else {
		return val, ErrNoKey
	}
}

func (kv *KVServer) Get(args *GetArgs, reply *GetReply) {
	value, err := kv.Helper(Op{Op: "Get", Key: args.Key, ClientId: args.ClientId, MsgId: args.MsgId})
	*reply = GetReply{int64(kv.me), err, value}
}

func (kv *KVServer) PutAppend(args *PutAppendArgs, reply *PutAppendReply) {
	_, err := kv.Helper(Op{Op: args.Op, Key: args.Key, Value: args.Value, ClientId: args.ClientId, MsgId: args.MsgId})
	if err == ErrNoKey {
		*reply = PutAppendReply{int64(kv.me), OK}
	} else {
		*reply = PutAppendReply{int64(kv.me), err}
	}
}

func (kv *KVServer) Kill() {
	atomic.StoreInt32(&kv.dead, 1)
	kv.rf.Kill()
	kv.stateChange.Broadcast()
}

func (kv *KVServer) killed() bool {
	z := atomic.LoadInt32(&kv.dead)
	return z == 1
}

func (kv *KVServer) processCommand(commandIndex int, command Op) (int, []byte) {
	kv.Lock("processCommand")
	defer kv.Unlock()
	kv.lastAppliedIndex = commandIndex
	if kv.SetLastProcessed(command.ClientId, CacheItem{MsgId: command.MsgId, LogIndex: int64(commandIndex)}) {
		if command.Op == "Put" {
			kv.data[command.Key] = command.Value
		} else if command.Op == "Append" {
			kv.data[command.Key] += command.Value
		}
	}
	if (kv.maxraftstate) != -1 && kv.lastAppliedIndex%(kv.maxraftstate/10) == 0 {
		var buffer bytes.Buffer
		encoder := labgob.NewEncoder(&buffer)
		encoder.Encode(kv.data)
		encoder.Encode(kv.lastProcessed)
		return kv.lastAppliedIndex, buffer.Bytes()
	}
	kv.stateChange.Broadcast()
	return 0, nil
}

func (kv *KVServer) processSnapshot(msg *raft.ApplyMsg) {
	kv.Lock("processSnapshot")
	defer kv.Unlock()
	if kv.rf.CondInstallSnapshot(msg.SnapshotTerm, msg.SnapshotIndex, msg.Snapshot) {
		r := bytes.NewBuffer(msg.Snapshot)
		decoder := labgob.NewDecoder(r)
		var data map[string]string
		var lastProcessed map[int64]CacheItem
		if decoder.Decode(&data) != nil || decoder.Decode(&lastProcessed) != nil {
			panic("cannot decode")
		} else {
			kv.data = data
			kv.lastProcessed = lastProcessed
		}
		kv.lastAppliedIndex = msg.SnapshotIndex
		kv.stateChange.Broadcast()
	}
}

func (kv *KVServer) applier() {
	select {
	case <-time.After(100 * time.Millisecond):
		kv.stateChange.Broadcast()
	case msg := <-kv.applyCh:
		{
			if msg.CommandValid {
				kv.DPrintln(msg)
				lastIndex, data := kv.processCommand(msg.CommandIndex, msg.Command.(Op))
				if lastIndex != 0 {
					kv.rf.Snapshot(kv.lastAppliedIndex, data)
				}
			} else if msg.SnapshotValid {
				kv.processSnapshot(&msg)
			} else {
				panic("unidentified message")
			}
		}
	}
}

func StartKVServer(servers []*labrpc.ClientEnd, me int, persister *raft.Persister, maxraftstate int) *KVServer {
	labgob.Register(Op{})
	labgob.Register(PutAppendArgs{})
	labgob.Register(PutAppendReply{})
	labgob.Register(GetArgs{})
	labgob.Register(GetReply{})

	kv := new(KVServer)
	kv.me = me
	kv.maxraftstate = maxraftstate
	kv.applyCh = make(chan raft.ApplyMsg)
	kv.rf = raft.Make(servers, me, persister, kv.applyCh)

	kv.data = make(map[string]string)
	kv.stateChange = *sync.NewCond(&kv.mu)
	kv.lastAppliedIndex = 0
	kv.lastProcessed = make(map[int64]CacheItem)
	go kv.run_until_killed(kv.applier)
	return kv
}
func (rf *KVServer) run_until_killed(f func()) {
	for !rf.killed() {
		f()
	}
}
func (rf *KVServer) Lock(i string) {
	rf.mu.Lock()
}

func (rf *KVServer) Unlock() {
	rf.mu.Unlock()
}

func (kv *KVServer) DPrintln(a ...interface{}) (n int, err error) {
	if false {
		_, is_leader := kv.rf.GetState()
		if is_leader {
			xs := []interface{}{"node id", kv.me, is_leader}
			xs = append(xs, a...)
			log.Println(xs...)
		}
	}
	return
}
