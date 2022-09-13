package kvraft

const (
	OK             = "OK"
	ErrNoKey       = "ErrNoKey"
	ErrWrongLeader = "ErrWrongLeader"
)

type Err string

// Put or Append
type PutAppendArgs struct {
	ClientId int64
	MsgId    int64
	Key      string

	Value string
	Op    string // "Put" or "Append"
}

type PutAppendReply struct {
	ServerId int64
	Err Err
}

type GetArgs struct {
	ClientId int64
	MsgId    int64
	Key      string
}

type GetReply struct {
	ServerId int64
	Err      Err
	Value    string
}
