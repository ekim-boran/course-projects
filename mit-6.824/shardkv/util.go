package shardkv

import (
	"fmt"

	"6.824/shardctrler"
)

func loop(f func()) {
	for {
		f()
	}
}
func (kv *ShardKV) DPrintln(a ...interface{}) (n int, err error) {
	if true {
		_, is_leader := kv.rf.GetState()
		if is_leader {
			xs := []interface{}{kv.gid, kv.me, " | "}
			xs = append(xs, a...)
			fmt.Println(xs...)
		}
	}
	return
}

func NewShards(newConfig *shardctrler.Config, oldConfig *shardctrler.Config, gid int) []int {
	old_shards := find_indices(oldConfig.Shards[:], gid)
	new_shards := find_indices(newConfig.Shards[:], gid)
	return diff(new_shards, old_shards)
}
func SameShards(newConfig *shardctrler.Config, oldConfig *shardctrler.Config, gid int) []int {
	old_shards := find_indices(oldConfig.Shards[:], gid)
	new_shards := find_indices(newConfig.Shards[:], gid)
	return same(new_shards, old_shards)
}
func diff(a, b []int) []int {
	m := make(map[int]bool)
	var diff []int
	for _, item := range b {
		m[item] = true
	}

	for _, item := range a {
		if _, ok := m[item]; !ok {
			diff = append(diff, item)
		}
	}
	return diff
}

func same(a, b []int) []int {
	m := make(map[int]bool)
	var diff []int
	for _, item := range b {
		m[item] = true
	}

	for _, item := range a {
		if _, ok := m[item]; ok {
			diff = append(diff, item)
		}
	}
	return diff
}
func find_indices(xs []int, x int) []int {
	var res []int
	for i, elem := range xs {
		if elem == x {
			res = append(res, i)
		}
	}
	return res
}

func cloneData(xs map[string]string) map[string]string {
	var m = make(map[string]string)
	for k, v := range xs {
		m[string(k)] = string(v)
	}
	return m
}
func cloneLastProcessed(xs map[int64]int64) map[int64]int64 {
	var m = make(map[int64]int64)
	for k, v := range xs {
		m[k] = v
	}
	return m
}


 