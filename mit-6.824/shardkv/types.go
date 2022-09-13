package shardkv

import (
	"bytes"
	"sync"

	"6.824/labgob"
	"6.824/labrpc"
	"6.824/raft"
	"6.824/shardctrler"
)

type ShardKV struct {
	mu           sync.Mutex
	me           int
	rf           *raft.Raft
	applyCh      chan raft.ApplyMsg
	make_end     func(string) *labrpc.ClientEnd
	gid          int
	ctrlers      []*labrpc.ClientEnd
	sm           *shardctrler.Clerk
	maxraftstate int

	persister        *raft.Persister
	lastAppliedIndex int
	stateChange      sync.Cond

	data          []map[string]string // shard - key - value
	lastProcessed map[int64]int64
	lastConfig    shardctrler.Config
	newConfig     shardctrler.Config
	version       [shardctrler.NShards]int // config number
}

func (kv *ShardKV) installSnapshot(snap []byte) {
	r := bytes.NewBuffer(snap)
	decoder := labgob.NewDecoder(r)
	var data []map[string]string
	var lastProcessed map[int64]int64
	var lastConfig shardctrler.Config
	var newConfig shardctrler.Config
	var version [shardctrler.NShards]int

	if decoder.Decode(&data) != nil || decoder.Decode(&lastProcessed) != nil || decoder.Decode(&lastConfig) != nil || decoder.Decode(&newConfig) != nil || decoder.Decode(&version) != nil {
		panic("cannot decode")
	} else {
		kv.data = data
		kv.lastProcessed = lastProcessed
		kv.lastConfig = lastConfig
		kv.newConfig = newConfig
		kv.version = version
	}
}

func (kv *ShardKV) createSnapshot() []byte {
	var buffer bytes.Buffer
	encoder := labgob.NewEncoder(&buffer)
	encoder.Encode(kv.data)
	encoder.Encode(kv.lastProcessed)
	encoder.Encode(kv.lastConfig)
	encoder.Encode(kv.newConfig)
	encoder.Encode(kv.version)
	return buffer.Bytes()
}

func (kv *ShardKV) Lock(i string) {
	kv.mu.Lock()
}

func (kv *ShardKV) Unlock() {
	kv.mu.Unlock()
}

const (
	GET          = "Get"
	PUT          = "Put"
	APPEND       = "Append"
	DELETESHARDS = "DeleteShards"
	NEWSHARD     = "NEWSHARD"
	NEWCONFIG    = "NEWCONFIG"
)

type Op struct {
	ClientId int64
	MsgId    int64

	OpCode    string
	Key       string
	Value     string
	VersionId int
	NewConfig shardctrler.Config

	Data          map[int]map[string]string
	LastProcessed map[int64]int64

	ShardIds           []int // deletion
	DeleteShardVersion int   // deletion

}

func CloneConfig(config *shardctrler.Config) shardctrler.Config {
	new_conf := shardctrler.Config{Num: config.Num, Shards: [shardctrler.NShards]int{}, Groups: make(map[int][]string)}
	for k, v := range config.Groups {
		new_conf.Groups[k] = v
	}
	for k, v := range config.Shards {
		new_conf.Shards[k] = v
	}
	return new_conf
}

func (kv *ShardKV) Kill() {
	kv.rf.Kill()
}

func mkNewShardOp(clientId int, newConfig *shardctrler.Config, data map[int]map[string]string, lastProcessed map[int64]int64) Op {
	return Op{ClientId: int64(clientId), MsgId: int64(newConfig.Num), OpCode: NEWSHARD, Data: data, LastProcessed: lastProcessed,
		NewConfig: *newConfig}
}
func mkNewConfigOp(clientId int, newConfig *shardctrler.Config) Op {
	return Op{ClientId: int64(clientId), MsgId: int64(newConfig.Num), OpCode: NEWCONFIG, NewConfig: *newConfig}
}
