package raft

func (rf *Raft) handleInstallSnapshot(args *InstallSnapshotArgs) InstallSnapshotReply {
	rf.Lock("InstallSnapshot")
	reply := InstallSnapshotReply{rf.term}
	if args.Term < rf.term {
		rf.Unlock()
		return reply
	}
	if args.Term >= rf.term {
		rf.ResetElectionTimer()
	}
	if args.Term > rf.term {
		rf.ChangePersistentState(Follower, args.Term, -1)
	}
	reply.Term = rf.term
	rf.Unlock()
	rf.applyCh <- mkSnapshotMsg(args.Data, args.LastIncludedTerm, args.LastIncludedIndex)
	return reply
}

func (rf *Raft) handleAppendEntries(args AppendEntriesArgs) AppendEntriesReply {

	rf.Lock("1")
	defer rf.Unlock()
	if args.Term < rf.term {
		return AppendEntriesReply{rf.term, false, 0, 0, 0}
	}
	rf.ResetElectionTimer()
	if args.Term > rf.term {
		rf.ChangePersistentState(Follower, args.Term, -1)
	}
	// i started an election but a append entry message has came with same term
	// is it a correct thing to do
	// cancel the election somehow?
	if args.Term == rf.term && rf.role == Candidate {
		rf.role = Follower
	}

	if rf.GetNextLogSlot() < args.StartIndex {
		return AppendEntriesReply{rf.term, false, -1, rf.GetNextLogSlot(), -1}
	}
	myTermAtStartIndex := rf.GetLogTerm(args.StartIndex-1, args.PrevLogTerm)

	if myTermAtStartIndex != args.PrevLogTerm {
		index := rf.Search(func(i Log) bool { return i.Term >= myTermAtStartIndex }) // sort.search returns smallest index that satisfies predicate
		return AppendEntriesReply{rf.term, false, myTermAtStartIndex, rf.GetNextLogSlot(), index}
	}
	for i, entry := range args.Entries {
		realIndex := i + args.StartIndex
		if rf.IndexInSnapshot(realIndex) {
			// skip before snapshot
			continue
		}
		if realIndex >= rf.GetNextLogSlot() {
			rf.Append(args.Entries[i:]...)
			rf.persist(nil)
			// append
			break
		}
		if rf.GetLogTerm(realIndex, -1) != entry.Term {
			rf.TrimAfter(realIndex)
			rf.Append(args.Entries[i:]...)
			rf.persist(nil)
			break
		}
	}

	rf.UpdateCommit(args.LeaderCommit)
	return AppendEntriesReply{rf.term, true, 0, 0, 0}
}

func (rf *Raft) sendSnapshot(to int, snapshot_args InstallSnapshotArgs) bool {
	snapshot_reply, ok := rf.sendInstallSnapshotRPC(to, snapshot_args)
	rf.Lock("sendSnapshot")
	defer rf.Unlock()
	// check if it is still leader
	if rf.role != Leader {
		return true
	}
	if ok {
		if rf.term < snapshot_reply.Term {
			rf.ChangePersistentState(Follower, snapshot_reply.Term, -1)
			return true
		} else {
			rf.nextIndex[to] = snapshot_args.LastIncludedIndex + 1 // can change after rpc returns
			return false
		}
	}
	return false
}

func (rf *Raft) sendAppendEntries(to int) {
	rf.Lock("2")
	if rf.role != Leader {
		rf.Unlock()
		return
	}
	start := rf.nextIndex[to]
	if rf.IndexInSnapshot(start) {
		snapshot := rf.persister.ReadSnapshot()
		snapshot_args := InstallSnapshotArgs{Data: snapshot, Term: rf.term, LastIncludedIndex: rf.log.SnapshotIndex1, LastIncludedTerm: rf.log.SnapshotTerm}
		rf.Unlock()
		rf.sendSnapshot(to, snapshot_args)
		//if !finished {
		//	rf.sendAppendEntries(to)
		//}
		return
	}
	lastTerm := rf.GetLogTerm(start-1, -1)

	args := AppendEntriesArgs{Term: rf.term, LeaderId: rf.me, StartIndex: start, PrevLogTerm: lastTerm,
		Entries: rf.GetLogsAfter(start), LeaderCommit: rf.commitIndex}
	rf.Unlock()
	rf.ResetHeartbeatTimer(to)
	reply, ok := rf.sendAppendEntriesRPC(to, args)
	if !ok {
		return
	}
	finished := rf.processAppendEntry(to, args, &reply)
	if !finished {
		rf.sendAppendEntries(to)
	}
}

func (rf *Raft) processAppendEntry(to int, args AppendEntriesArgs, reply *AppendEntriesReply) bool {
	rf.Lock("processAppendEntry")
	defer rf.Unlock()

	if rf.role != Leader {
		return true
	}
	if rf.term < reply.Term {
		rf.ChangePersistentState(Follower, reply.Term, -1)
		return true
	}
	nextIndex := args.StartIndex + len(args.Entries)

	if rf.term == reply.Term {
		if reply.Success {
			if rf.lastReceivedIndex[to] < nextIndex-1 { // if there were a concurrent request
				rf.lastReceivedIndex[to] = nextIndex - 1
				rf.nextIndex[to] = nextIndex
			}
			middle := findMiddle(rf.lastReceivedIndex)
			// it hangs if there is no new item with currentterm
			for i := middle; i > rf.commitIndex; i-- {
				t := rf.GetLogTerm(i, -1) // if it return -1 then it does not exists it is already committed or log is empty
				if t == -1 || t == rf.term {
					rf.UpdateCommit(i)
					return true
				}
			}
		} else if reply.XTerm == -1 { // no entry in conflicting index
			rf.nextIndex[to] = reply.XLen
			return false
		} else { // there is an entry with current term
			i := rf.Search(func(i Log) bool { return i.Term > reply.XTerm })
			if rf.GetLogTerm(i-1, -1) == reply.XTerm { // xterm is present at rf.log
				rf.nextIndex[to] = i
			} else {
				rf.nextIndex[to] = reply.XIndex
			}
			return false
		}
	}
	return true
}

func (rf *Raft) UpdateCommit(newCommit int) {
	newCommit = min(newCommit, rf.GetNextLogSlot()-1)
	if newCommit > rf.commitIndex {
		rf.commitIndex = newCommit
		rf.commitIndexChanged.Signal()
	}
}

func (rf *Raft) heartbeat(peer int) {
	if rf.WaitHeartbeatTimer(peer) {
		rf.forAllGo(rf.sendAppendEntries)
	}
}
