package raft

import (
	"bytes"
	"sync"
	"time"

	"6.824/labgob"
	"6.824/labrpc"
)

func (rf *Raft) GetState() (int, bool) {
	rf.Lock("8")
	defer rf.Unlock()
	return rf.term, rf.role == Leader
}

func (rf *Raft) persist(snapshot []byte) {
	w := new(bytes.Buffer)
	e := labgob.NewEncoder(w)
	e.Encode(rf.lastVote)
	e.Encode(rf.term)
	e.Encode(rf.log.Logs)
	e.Encode(rf.log.SnapshotIndex1)
	e.Encode(rf.log.SnapshotTerm)
	data := w.Bytes()
	if snapshot == nil {
		rf.persister.SaveRaftState(data)
	} else {
		rf.persister.SaveStateAndSnapshot(data, snapshot)
	}
}

func (rf *Raft) readPersist(data []byte) {
	if data == nil || len(data) < 1 { // bootstrap without any state?
		return
	}
	r := bytes.NewBuffer(data)
	d := labgob.NewDecoder(r)
	var lastVote int
	var term int
	var logs []Log
	var index int
	var snapshotTerm int

	if d.Decode(&lastVote) != nil ||
		d.Decode(&term) != nil || d.Decode(&logs) != nil || d.Decode(&index) != nil || d.Decode(&snapshotTerm) != nil {
		panic("cannot decode")
	} else {
		rf.lastVote = lastVote
		rf.term = term
		rf.log.Logs = logs
		rf.log.SnapshotIndex1 = index
		rf.log.SnapshotTerm = snapshotTerm
	}
}

func (rf *Raft) CondInstallSnapshot(lastIncludedTerm int, lastIncludedIndex int, snapshot []byte) bool {
	rf.Lock("CondInstallSnapshot")
	defer rf.Unlock()
	//rf.DPrintln("Cond", lastIncludedIndex, rf.log.SnapshotIndex1, rf.log.Logs)

	if !rf.IndexInSnapshot(lastIncludedIndex) {
		rf.SnapshotTrim(lastIncludedIndex, lastIncludedTerm)
		rf.persist(snapshot)
	}
	// if it is equal it may be recovery message

	if lastIncludedIndex >= rf.log.SnapshotIndex1 {
		if rf.commitIndex < rf.log.SnapshotIndex1 {
			rf.commitIndex = rf.log.SnapshotIndex1
		}
		if rf.lastApplied < rf.log.SnapshotIndex1 {
			rf.lastApplied = rf.log.SnapshotIndex1
			return true
		}
	}

	return false
}

func (rf *Raft) Snapshot(lastIncludedIndex int, snapshot []byte) {
	rf.Lock("Snapshot")
	defer rf.Unlock()

	if !rf.IndexInSnapshot(lastIncludedIndex) {
		lastIncludedTerm := rf.GetLogTerm(lastIncludedIndex, -1)
		rf.SnapshotTrim(lastIncludedIndex, lastIncludedTerm)
		rf.persist(snapshot)
	}
}

func (rf *Raft) RequestVote(args *RequestVoteArgs, reply *RequestVoteReply) {
	*reply = rf.handleRequestVote(args)
}

func (rf *Raft) AppendEntries(args *AppendEntriesArgs, reply *AppendEntriesReply) {
	*reply = rf.handleAppendEntries(*args)
}

func (rf *Raft) InstallSnapshot(args *InstallSnapshotArgs, reply *InstallSnapshotReply) {
	*reply = rf.handleInstallSnapshot(args)
}

func (rf *Raft) Start(command interface{}) (int, int, bool) {
	rf.Lock("Start")
	defer rf.Unlock()
	if rf.role != Leader {
		return -1, -1, false
	}
	term := rf.term
	rf.Append(Log{term, command})
	rf.persist(nil)
	rf.lastReceivedIndex[rf.me] = rf.GetNextLogSlot() - 1
	rf.forAllGo(rf.sendAppendEntries)
	return (rf.GetNextLogSlot() - 1), term, true
}

func (rf *Raft) Applier() {
	var commands []ApplyMsg
	rf.Lock("Applier")
	for rf.lastApplied >= rf.commitIndex {
		rf.commitIndexChanged.Wait()
	}

	for i, log := range rf.GetLogsBetween(rf.lastApplied+1, rf.commitIndex+1) {
		commands = append(commands, ApplyMsg{CommandValid: true, Command: log.Command, CommandIndex: i + rf.lastApplied + 1})
	}

	rf.Unlock()
	for _, msg := range commands {
		rf.Lock("Applier2")

		if rf.lastApplied != msg.CommandIndex-1 { // lastApplied is changed by snapshot?
			rf.Unlock()
			break
		}
		rf.lastApplied = msg.CommandIndex
		rf.Unlock()
		rf.applyCh <- msg
	}
}

func Make(peers []*labrpc.ClientEnd, me int,
	persister *Persister, applyCh chan ApplyMsg) *Raft {
	rf := &Raft{}
	rf.peers = peers
	rf.persister = persister
	rf.me = me
	rf.applyCh = applyCh

	rf.term = 0
	rf.lastVote = -1
	rf.role = Follower

	rf.electionTimer = 0
	rf.heartBeatTimer = make([]int64, len(peers))

	rf.ResetElectionTimer()

	rf.nextIndex = make([]int, len(peers))
	rf.lastReceivedIndex = make([]int, len(peers))

	rf.log.SnapshotIndex1 = 0
	rf.log.SnapshotTerm = -1

	rf.commitIndex = 0
	rf.lastApplied = 0
	rf.commitIndexChanged = *sync.NewCond(&rf.mu)

	rf.readPersist(persister.ReadRaftState())
	snapshot := persister.ReadSnapshot()
	// because apply channel blocks at the start we need to start a new goroutine
	go func() {
		if len(snapshot) > 1 {
			rf.applyCh <- mkSnapshotMsg(snapshot, rf.log.SnapshotTerm, rf.log.SnapshotIndex1)
		}
	}()
	// need a better solution -- deadlocks otherwise
	time.Sleep(100 * time.Millisecond)

	go rf.run_until_killed(rf.ticker)
	go rf.run_until_killed(rf.Applier)
	rf.forAllGo(func(peer int) { rf.run_until_killed(func() { rf.heartbeat(peer) }) })
	return rf
}
