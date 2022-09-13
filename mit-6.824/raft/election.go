package raft

func (rf *Raft) ticker() {
	if rf.WaitElectionTimer() {
		go rf.election()
	}
}

type Tuple struct {
	fst int
	snd int
}

func (rf *Raft) processVote(vote *RequestVoteReply, votes Tuple) (Tuple, bool) {
	if rf.role != Candidate {
		return votes, true
	}
	if vote.Term > rf.term {
		rf.ChangePersistentState(Follower, vote.Term, -1)
		return votes, true
	}
	if vote.VoteGranted && vote.Term == rf.term {
		votes.fst += 1
		if votes.fst >= rf.Majority() {
			rf.role = Leader
			rf.ResetElectionTimer()
			rf.lastReceivedIndex[rf.me] = rf.GetNextLogSlot()
			rf.for_all(func(i int) {
				rf.nextIndex[i] = rf.GetNextLogSlot()
				rf.lastReceivedIndex[i] = 0
			})
			return votes, true
		}
	} else {
		votes.snd += 1
		if votes.snd >= rf.Majority() {
			rf.role = Follower
			return votes, true
		}
	}
	return votes, false
}

func (rf *Raft) election() {
	rf.Lock("6")
	if rf.role != Follower {
		rf.Unlock()
		return
	}

	rf.ChangePersistentState(Candidate, rf.term+1, rf.me)
	args := &RequestVoteArgs{Term: rf.term, CandidateId: rf.me, LogLength: rf.GetNextLogSlot(), LastLogTerm: rf.GetLogTerm(rf.GetNextLogSlot()-1, -1)}
	rf.Unlock()

	votesChannel, cancelled := make(chan RequestVoteReply), make(chan bool)
	defer close(cancelled)

	rf.forAllGo(func(i int) {

		reply, _ := rf.sendRequestVoteRPC(i, args)
		select {
		case <-cancelled:
			return
		case votesChannel <- reply:
		}
	})

	voteCount := Tuple{fst: 1, snd: 0}
	for vote := range votesChannel {
		rf.Lock("5")
		newVoteCount, finished := rf.processVote(&vote, voteCount)
		voteCount = newVoteCount
		if finished {
			if voteCount.fst >= rf.Majority() {
				rf.forAllGo(rf.sendAppendEntries)
			}
			rf.Unlock()
			return
		}
		rf.Unlock()
	}
}

func (rf *Raft) handleRequestVote(args *RequestVoteArgs) RequestVoteReply {
	rf.Lock("handleRequestVote")
	defer rf.Unlock()
	if args.Term < rf.term {
		return RequestVoteReply{rf.me, rf.term, false}
	}
	if args.Term > rf.term {
		rf.ChangePersistentState(Follower, args.Term, -1)
	}

	myLength := rf.GetNextLogSlot()
	myLastTerm := rf.GetLogTerm(myLength-1, -1)
	logOk := args.LastLogTerm > myLastTerm || (args.LastLogTerm == myLastTerm && args.LogLength >= myLength)
	termOk := rf.lastVote == -1 || rf.lastVote == args.CandidateId
	if logOk && termOk {
		rf.ChangePersistentState(Follower, args.Term, args.CandidateId)
		rf.ResetElectionTimer()
		return RequestVoteReply{rf.me, rf.term, true}
	}
	return RequestVoteReply{rf.me, rf.term, false}
}
