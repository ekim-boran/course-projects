package shardkv

//
// Sharded key/value server.
// Lots of replica groups, each running Raft.
// Shardctrler decides which group serves each shard.
// Shardctrler may change shard assignment from time to time.
//
// You will have to modify these definitions.
//

const (
	OK             = "OK"
	ErrNoKey       = "ErrNoKey"
	ErrWrongGroup  = "ErrWrongGroup"
	ErrWrongLeader = "ErrWrongLeader"
)

type Err string
type GetShardsArgs struct {
	Header
	ShardVersion int
	ShardIds     []int
}

type GetShardsReply struct {
	Err           Err
	LastProcessed map[int64]int64
	Data          map[int]map[string]string
}

type Header struct {
	ClientId int64
	MsgId    int64
}

type PutAppendArgs struct {
	Header
	Key   string
	Value string
	Op    string
}

type PutAppendReply struct {
	Err Err
}

type GetArgs struct {
	Header
	Key string
}

type GetReply struct {
	Err   Err
	Value string
}
