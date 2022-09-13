package raft

import (
	"sort"
)

type Log struct {
	Term    int
	Command interface{}
}
type RaftLog struct {
	Logs           []Log
	SnapshotIndex1 int
	SnapshotTerm   int
}

func (rf *Raft) VirtualToReal(index int) int {
	return index - (rf.log.SnapshotIndex1 + 1)
}
func (rf *Raft) RealToVirtual(index int) int {
	return index + (rf.log.SnapshotIndex1 + 1)
}

// after
func (rf *Raft) GetLogsBetween(start int, end int) []Log {
	if start > end {
		panic("GetLogsBetween start is bigger than end")
	}
	realStart := rf.VirtualToReal(start)
	realEnd := rf.VirtualToReal(end)
	if realStart < 0 {
		realStart = 0
	}
	if realEnd < 0 {
		realEnd = 0
		realStart = 0
	}
	return rf.log.Logs[realStart:(realEnd)]
}

func (rf *Raft) GetNextLogSlot() int {
	return rf.RealToVirtual(len(rf.log.Logs))
}

func (rf *Raft) GetLogTerm(index int, def int) int {
	realIndex := rf.VirtualToReal(index)
	if realIndex == -1 {
		return rf.log.SnapshotTerm
	}
	if realIndex < -1 {
		return def
	}
	// if bigger panic
	return rf.log.Logs[realIndex].Term
}

func (rf *Raft) GetLogsAfter(start int) []Log {
	logs := append([]Log{}, rf.log.Logs[rf.VirtualToReal(start):]...)
	return logs
}

func (rf *Raft) Search(f func(l Log) bool) int {
	realIndex := sort.Search(len(rf.log.Logs), func(i int) bool { return f(rf.log.Logs[i]) })
	return rf.RealToVirtual(realIndex)
}

func (rf *Raft) Append(a ...Log) {
	rf.log.Logs = append(rf.log.Logs, a...)
}

func (rf *Raft) TrimAfter(index int) {
	rf.log.Logs = rf.log.Logs[:(rf.VirtualToReal(index))]
}

// index is included to final log
func (rf *Raft) SnapshotTrim(lastIncludedIndex int, lastIncludedTerm int) {
	index := rf.VirtualToReal(lastIncludedIndex + 1)

	if len(rf.log.Logs) < index {
		rf.log.Logs = rf.log.Logs[:0]
	} else {
		rf.log.Logs = rf.log.Logs[index:]
	}

	rf.log.SnapshotTerm = lastIncludedTerm
	rf.log.SnapshotIndex1 = lastIncludedIndex
}

func (rf *Raft) IndexInSnapshot(index int) bool {
	return index <= rf.log.SnapshotIndex1
}
