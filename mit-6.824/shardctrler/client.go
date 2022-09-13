package shardctrler

//
// Shardctrler clerk.
//

import (
	"crypto/rand"
	"math/big"
	"time"

	"6.824/labrpc"
)

type Clerk struct {
	servers       []*labrpc.ClientEnd
	me            int64
	leader        int
	lastMessageId int64
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
func (ck *Clerk) MessageId() int64 {
	id := ck.lastMessageId
	ck.lastMessageId++
	return id
}

func (ck *Clerk) NextServer() int {
	ck.leader = (ck.leader + 1) % len(ck.servers)
	return ck.leader
}

func (ck *Clerk) header() Header {
	return Header{ClientId: ck.me, MsgId: ck.MessageId()}
}

func (ck *Clerk) Query(num int) Config {
	args := &QueryArgs{ck.header(), num}
	for {
		for i := 0; i < len(ck.servers); i++ {
			reply := &QueryReply{}
			ok := ck.servers[ck.leader].Call("ShardCtrler.Query", args, reply)
			if ok && !reply.WrongLeader {
				return reply.Config
			}
			ck.NextServer()
		}
		time.Sleep(100 * time.Millisecond)
	}
}

func (ck *Clerk) Join(servers map[int][]string) {
	args := &JoinArgs{ck.header(), servers}
	for {
		// try each known server.
		for i := 0; i < len(ck.servers); i++ {
			reply := &JoinReply{}
			ok := ck.servers[ck.leader].Call("ShardCtrler.Join", args, reply)
			if ok && !reply.WrongLeader {
				return
			}
			ck.NextServer()
		}
		time.Sleep(100 * time.Millisecond)
	}
}

func (ck *Clerk) Leave(gids []int) {
	args := &LeaveArgs{ck.header(), gids}
	for {
		for i := 0; i < len(ck.servers); i++ {
			reply := &LeaveReply{}
			ok := ck.servers[ck.leader].Call("ShardCtrler.Leave", args, reply)
			if ok && !reply.WrongLeader {
				return
			}
			ck.NextServer()
		}
		time.Sleep(100 * time.Millisecond)
	}
}

func (ck *Clerk) Move(shard int, gid int) {
	args := &MoveArgs{ck.header(), shard, gid}
	for {
		for i := 0; i < len(ck.servers); i++ {
			reply := &MoveReply{}
			ok := ck.servers[ck.leader].Call("ShardCtrler.Move", args, reply)
			if ok && !reply.WrongLeader {
				return
			}
			ck.NextServer()
		}
		time.Sleep(100 * time.Millisecond)
	}
}
