package kvraft

import (
	"crypto/rand"
	"math/big"
	"time"

	"6.824/labrpc"
)

type Clerk struct {
	me            int64
	servers       []*labrpc.ClientEnd
	leader        int
	lastMessageId int64
}

func (ck *Clerk) MessageId() int64 {
	id := ck.lastMessageId
	ck.lastMessageId++
	return id
}

func (ck *Clerk) NextServer() int {
	ck.leader = (ck.leader + 1) % len(ck.servers)
	return ck.leader
}

func nrand() int64 {
	max := big.NewInt(int64(1) << 62)
	bigx, _ := rand.Int(rand.Reader, max)
	x := bigx.Int64()
	return x
}

func MakeClerk(servers []*labrpc.ClientEnd) *Clerk {
	ck := new(Clerk)
	ck.servers = servers
	ck.leader = 0
	ck.lastMessageId = 1
	ck.me = nrand()
	return ck
}

func (ck *Clerk) Get(key string) string {
	args := GetArgs{ClientId: ck.me, MsgId: ck.MessageId(), Key: key}
	reply := GetReply{0, "", ""}

	for {
		for i := 0; i < len(ck.servers); i++ {
			ok := ck.servers[ck.leader].Call("KVServer.Get", &args, &reply)
			if ok {
				switch reply.Err {
				case OK:
					return reply.Value
				case ErrNoKey:
					return ""
				case ErrWrongLeader:
					ck.NextServer()
				}
			} else {
				ck.NextServer()
			}
		}
		time.Sleep(100 * time.Millisecond)
	}
}

func (ck *Clerk) PutAppend(key string, value string, op string) {
	args := PutAppendArgs{ClientId: ck.me, MsgId: ck.MessageId(), Key: key, Value: value, Op: op}
	reply := PutAppendReply{0, ""}
	for {
		for i := 0; i < len(ck.servers); i++ {
			ok := ck.servers[ck.leader].Call("KVServer.PutAppend", &args, &reply)
			if ok {
				switch reply.Err {
				case ErrWrongLeader:
					ck.NextServer()
				default:
					return
				}
			} else {
				ck.NextServer()
			}
		}
		time.Sleep(100 * time.Millisecond)
	}
}

func (ck *Clerk) Put(key string, value string) {
	ck.PutAppend(key, value, "Put")
}
func (ck *Clerk) Append(key string, value string) {
	ck.PutAppend(key, value, "Append")
}
