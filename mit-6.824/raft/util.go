package raft

import (
	"log"
	"sort"
)

// Debugging
const Debug = true

func empty(a ...interface{}) {

}

func (rf *Raft) DPrintf(format string, a ...interface{}) (n int, err error) {
	if Debug {
		log.Printf(format, a...)
	}
	return
}
func (rf *Raft) DPrintln(a ...interface{}) (n int, err error) {
	if Debug {
		xs := []interface{}{"node id", rf.me }
		xs = append(xs, a...)
		log.Println(xs...)
	}
	return
}

func findMiddle(xs []int) int {
	lengths := append([]int{}, xs...)
	sort.Ints(lengths)
	return lengths[len(lengths)/2]
}
func (rf *Raft) forAllGo(f func(int)) {
	for i := range rf.peers {
		if i == rf.me {
			continue
		}
		go f(i)
	}
}

func (rf *Raft) for_all(f func(int)) {
	for i := range rf.peers {
		if i == rf.me {
			continue
		}
		f(i)
	}
}
func min(a, b int) int {
	if a < b {
		return a
	} else {
		return b
	}
}
