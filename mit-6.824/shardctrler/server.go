package shardctrler

import (
	"fmt"
	"sort"
	"sync"
	"time"

	"6.824/labgob"
	"6.824/labrpc"
	"6.824/raft"
)

type CacheItem struct {
	MsgId    int64
	LogIndex int64
}
type ShardCtrler struct {
	mu      sync.Mutex
	me      int
	rf      *raft.Raft
	applyCh chan raft.ApplyMsg

	// Your data here.

	configs          []Config // indexed by config num
	lastAppliedIndex int
	lastProcessed    map[int64]CacheItem
	stateChange      sync.Cond
}

type Op struct {
	OpCode string
	Header
	Servers map[int][]string // join
	GIDs    []int            // leave
	Shard   int              // move
	GID     int              // move
	Num     int              // query

}

func (kv *ShardCtrler) SetLastProcessed(clientId int64, item CacheItem) bool {
	lastProcessed, ok := kv.lastProcessed[clientId]
	if !ok || lastProcessed.MsgId < item.MsgId {
		kv.lastProcessed[clientId] = item
		return true
	}
	return false
}
func (rf *ShardCtrler) Lock(i string) {
	rf.mu.Lock()
}

func (rf *ShardCtrler) Unlock() {
	rf.mu.Unlock()
}
func (sc *ShardCtrler) Helper(command Op) (Config, bool) {
	_, is_leader := sc.rf.GetState()
	if !is_leader {
		return Config{}, true
	}

	sc.Lock("Helper")
	defer sc.Unlock()
	item := sc.lastProcessed[command.ClientId]
	index := int(item.LogIndex)
	if item.MsgId < command.MsgId {
		if index, _, is_leader = sc.rf.Start(command); !is_leader {
			return Config{}, true
		}
	}

	currentTime := time.Now()
	for index > sc.lastAppliedIndex {
		sc.stateChange.Wait()
		if _, is_leader = sc.rf.GetState(); !is_leader || time.Since(currentTime) > 2*time.Second {
			return Config{}, true
		}
	}
	if command.OpCode == QUERY {
		var config Config
		if command.Num == -1 || command.Num >= len(sc.configs) {
			config = sc.configs[len(sc.configs)-1]
		} else {
			config = sc.configs[command.Num]
		}
		return config, false
	}

	return Config{}, false
}

func (sc *ShardCtrler) Join(args *JoinArgs, reply *JoinReply) {
	_, reply.WrongLeader = sc.Helper(Op{OpCode: JOIN, Header: args.Header, Servers: args.Servers})
	if !reply.WrongLeader {
		reply.Err = OK
	}
}

func (sc *ShardCtrler) Leave(args *LeaveArgs, reply *LeaveReply) {
	_, reply.WrongLeader = sc.Helper(Op{OpCode: LEAVE, Header: args.Header, GIDs: args.GIDs})
	if !reply.WrongLeader {
		reply.Err = OK
	}
}

func (sc *ShardCtrler) Move(args *MoveArgs, reply *MoveReply) {
	_, reply.WrongLeader = sc.Helper(Op{OpCode: MOVE, Header: args.Header, Shard: args.Shard, GID: args.GID})
	if !reply.WrongLeader {
		reply.Err = OK
	}
}

func (sc *ShardCtrler) Query(args *QueryArgs, reply *QueryReply) {
	reply.Config, reply.WrongLeader = sc.Helper(Op{OpCode: QUERY, Header: args.Header, Num: args.Num})
	if !reply.WrongLeader {
		reply.Err = OK
	}
}

func (sc *ShardCtrler) Kill() {
	sc.rf.Kill()
}

func (sc *ShardCtrler) Raft() *raft.Raft {
	return sc.rf
}

func (sc *ShardCtrler) processCommand(commandIndex int, command Op) {
	sc.Lock("Applier")
	defer sc.Unlock()
	sc.lastAppliedIndex = commandIndex
	if !sc.SetLastProcessed(command.ClientId, CacheItem{MsgId: command.MsgId, LogIndex: int64(commandIndex)}) {
		return
	}

	if command.OpCode == JOIN {
		sc.configs = append(sc.configs, sc.configs[len(sc.configs)-1].Add(command.Servers))
	} else if command.OpCode == LEAVE {
		sc.configs = append(sc.configs, sc.configs[len(sc.configs)-1].Remove(command.GIDs))
	} else if command.OpCode == MOVE {
		sc.configs = append(sc.configs, sc.configs[len(sc.configs)-1].Move(command.Shard, command.GID))
	}

	sc.stateChange.Broadcast()
}

func (sc *ShardCtrler) applier() {
	for {
		select {
		case <-time.After(300 * time.Millisecond):
			sc.stateChange.Broadcast()
		case msg := <-sc.applyCh:
			{
				if msg.CommandValid {
					sc.processCommand(msg.CommandIndex, msg.Command.(Op))

				} else if msg.SnapshotValid {
					fmt.Println("snapshot!!")
				} else {
					panic("unidentified message")
				}
			}
		}
	}
}

func StartServer(servers []*labrpc.ClientEnd, me int, persister *raft.Persister) *ShardCtrler {

	sc := new(ShardCtrler)
	sc.me = me
	sc.configs = make([]Config, 1)
	sc.configs[0].Groups = map[int][]string{}

	labgob.Register(Op{})
	labgob.Register(JoinArgs{})
	labgob.Register(JoinReply{})
	labgob.Register(LeaveArgs{})
	labgob.Register(LeaveReply{})
	labgob.Register(MoveArgs{})
	labgob.Register(MoveReply{})
	labgob.Register(QueryArgs{})
	labgob.Register(QueryReply{})
	sc.applyCh = make(chan raft.ApplyMsg)
	sc.rf = raft.Make(servers, me, persister, sc.applyCh)

	sc.stateChange = *sync.NewCond(&sc.mu)
	sc.lastAppliedIndex = 0
	sc.lastProcessed = make(map[int64]CacheItem)
	go sc.applier()

	return sc
}

func (config *Config) Clone() Config {
	var new_conf Config = Config{config.Num + 1, [NShards]int{}, make(map[int][]string)}
	for k, v := range config.Groups {
		new_conf.Groups[k] = v
	}
	for k, v := range config.Shards {
		new_conf.Shards[k] = v
	}
	return new_conf
}

func sortedGroups(m map[int][]string) []int {
	var groups []int
	for k, _ := range m {
		groups = append(groups, k)
	}
	sort.Slice(groups, func(i, j int) bool {
		return groups[i] < groups[j]
	})
	return groups
}

func (config *Config) assign_shards() {
	if len(config.Groups) == 0 {
		for i := 0; i < NShards; i++ {
			config.Shards[i] = 0
		}
		return
	}
	var groups = sortedGroups(config.Groups)
	parts := NShards / (len(groups))
	for i, g := range groups {
		for k := 0; k < parts; k++ {
			config.Shards[i*parts+k] = g
		}
	}
	for k := (len(groups) - 1) * parts; k < NShards; k++ {
		config.Shards[k] = groups[len(groups)-1]
	}
}

func (config *Config) Add(group map[int][]string) Config {
	new_conf := config.Clone()
	for k, v := range group {
		new_conf.Groups[k] = v
	}
	new_conf.assign_shards()
	return new_conf
}

func (config *Config) Remove(groupids []int) Config {
	new_conf := config.Clone()
	for _, gid := range groupids {
		delete(new_conf.Groups, gid)
	}
	new_conf.assign_shards()
	return new_conf
}

func (config *Config) Move(sharid int, groupid int) Config {
	new_conf := config.Clone()
	new_conf.Shards[sharid] = groupid
	return new_conf
}
