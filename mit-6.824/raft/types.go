package raft

import (
	"math/rand"
	"sync"
	"sync/atomic"
	"time"

	"6.824/labrpc"
)

const (
	Follower  = iota
	Candidate = iota
	Leader    = iota
)

type Raft struct {
	applyCh chan ApplyMsg

	mu        sync.Mutex          // Lock to protect shared access to this peer's state
	peers     []*labrpc.ClientEnd // RPC end points of all peers
	persister *Persister          // Object to hold this peer's persisted state
	me        int                 // this peer's index into peers[]
	dead      int32               // set by Kill()

	log RaftLog

	term     int
	lastVote int
	role     int

	electionTimer     int64
	heartBeatTimer    []int64
	nextIndex         []int //for each server, index of the next log entry to send to that server (initialized to next slot)
	lastReceivedIndex []int //for each server, index of highest log entry	 known to be replicated on server	 (initialized to 0, increases monotonically

	// volatile state
	commitIndex        int // index of highest log entry known to be committed (initialized to 0, increases monotonically)
	lastApplied        int //index of highest log entry applied to state machine (initialized to 0, increases monotonically)
	commitIndexChanged sync.Cond
}

type ApplyMsg struct {
	CommandValid bool
	Command      interface{}
	CommandIndex int

	// For 2D:
	SnapshotValid bool
	Snapshot      []byte
	SnapshotTerm  int
	SnapshotIndex int
}

func mkSnapshotMsg(snapshot []byte, snapshot_term int, snapshot_index int) ApplyMsg {
	return ApplyMsg{SnapshotValid: true, CommandValid: false, Snapshot: snapshot, SnapshotTerm: snapshot_term, SnapshotIndex: snapshot_index}
}

type AppendEntriesArgs struct {
	Term         int   //leader’s term
	LeaderId     int   //so follower can redirect clients
	StartIndex   int   //length of the log of the leader
	PrevLogTerm  int   //term of prevLogIndex entry
	Entries      []Log //log entries to store (empty for heartbeat;	may send more than one for efficiency)
	LeaderCommit int   //leader’s commitIndex
}

type AppendEntriesReply struct {
	Term    int  // currentTerm, for leader to update itself
	Success bool // true if follower contained entry matching and prevLogTerm

	XTerm  int // xterm in lecture
	XLen   int
	XIndex int
	// fast backup described in lecture

}
type RequestVoteArgs struct {
	Term        int //candidate’s term
	CandidateId int //candidate requesting vote
	LogLength   int //length of candidate’s last log entry (§5.4)
	LastLogTerm int //term of candidate’s last log entry (§5.4)
}

type RequestVoteReply struct {
	NodeId      int
	Term        int  //currentTerm, for candidate to update itself
	VoteGranted bool //true means candidate received vote}
}

type InstallSnapshotArgs struct {
	Term              int //leader’s term
	LastIncludedIndex int // length of the log of the leader
	LastIncludedTerm  int //term of prevLogIndex entry
	Data              []byte
}

type InstallSnapshotReply struct {
	Term int // currentTerm, for leader to update itself
}

func (rf *Raft) Majority() int {
	return (len(rf.peers) + 1) / 2
}

func (rf *Raft) AppendLogs(term int, command interface{}) int {
	index := rf.GetNextLogSlot()
	rf.log.Logs = append(rf.log.Logs, Log{term, command})
	return index
}

func (rf *Raft) ChangePersistentState(role int, currentTerm int, votedFor int) {
	rf.role = role
	rf.term = currentTerm
	rf.lastVote = votedFor
	rf.persist(nil)
}

func (rf *Raft) Lock(i string) {
	rf.mu.Lock()
	//fmt.Println("lock", i, rf.me)
}
func (rf *Raft) Unlock() {
	rf.mu.Unlock()
	//fmt.Println("unlock", rf.me)

}
func (rf *Raft) run_until_killed(f func()) {
	for !rf.killed() {
		f()
	}
}
func (rf *Raft) Kill() {
	atomic.StoreInt32(&rf.dead, 1)
}

func (rf *Raft) killed() bool {
	z := atomic.LoadInt32(&rf.dead)
	return z == 1
}

func resetTimer(timer *int64, ms time.Duration) {
	now := time.Now().Add(ms)
	atomic.StoreInt64(timer, now.UnixMilli())
}

func waitTimer(timer *int64, reset func()) bool {
	for {
		timer := atomic.LoadInt64(timer)
		if time.Now().Before(time.UnixMilli(timer)) {
			time.Sleep(time.Until(time.UnixMilli(timer)))
		} else {
			reset()
			return true
		}
	}
}

func (rf *Raft) ResetHeartbeatTimer(to int) {
	resetTimer(&rf.heartBeatTimer[to], 100*time.Millisecond)
}
func (rf *Raft) WaitHeartbeatTimer(to int) bool {
	return waitTimer(&rf.heartBeatTimer[to], func() { rf.ResetHeartbeatTimer(to) })
}

func (rf *Raft) ResetElectionTimer() {
	source := rand.NewSource(time.Now().UnixNano())
	resetTimer(&rf.electionTimer, time.Duration(200+rand.New(source).Intn(200))*time.Millisecond)
}

func (rf *Raft) WaitElectionTimer() bool {
	return waitTimer(&rf.electionTimer, rf.ResetElectionTimer)
}

func (rf *Raft) sendRequestVoteRPC(server int, args *RequestVoteArgs) (RequestVoteReply, bool) {
	reply := RequestVoteReply{}
	ok := rf.peers[server].Call("Raft.RequestVote", args, &reply)
	if !ok {
		reply.NodeId = -1
		reply.Term = -1
		reply.VoteGranted = false

	}
	return reply, ok
}

func (rf *Raft) sendAppendEntriesRPC(server int, args AppendEntriesArgs) (AppendEntriesReply, bool) {
	reply := AppendEntriesReply{}
	ok := rf.peers[server].Call("Raft.AppendEntries", &args, &reply)
	return reply, ok
}

func (rf *Raft) sendInstallSnapshotRPC(server int, args InstallSnapshotArgs) (InstallSnapshotReply, bool) {
	reply := InstallSnapshotReply{}
	ok := rf.peers[server].Call("Raft.InstallSnapshot", &args, &reply)
	return reply, ok
}
