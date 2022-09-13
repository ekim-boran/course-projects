//===----------------------------------------------------------------------===//
//
//                         BusTub
//
// lru_replacer.h
//
// Identification: src/include/buffer/lru_replacer.h
//
// Copyright (c) 2015-2019, Carnegie Mellon University Database Group
//
//===----------------------------------------------------------------------===//

#pragma once
#include <list>
#include <map>
#include <mutex>   // NOLINT
#include <thread>  // NOLINT

#include <queue>
#include <tuple>
#include <vector>
#include "buffer/replacer.h"
#include "common/config.h"
namespace bustub {

class SpinLock {
  std::atomic_flag locked_ = ATOMIC_FLAG_INIT;

 public:
  void lock() {  // NOLINT
    while (locked_.test_and_set(std::memory_order_acquire)) {
      std::this_thread::yield();
    }
  }
  void unlock() { locked_.clear(std::memory_order_release); }  // NOLINT
};

/**
 * LRUReplacer implements the lru replacement policy, which approximates the Least Recently Used policy.
 */
class LRUReplacer : public Replacer {
 public:
  /**
   * Create a new LRUReplacer.
   * @param num_pages the maximum number of pages the LRUReplacer will be required to store
   */
  explicit LRUReplacer(size_t num_pages);

  ~LRUReplacer() override;

  bool Victim(frame_id_t *frame_id) override;

  void Pin(frame_id_t frame_id) override;

  void Unpin(frame_id_t frame_id) override;

  size_t Size() override;

 private:
  // addition date
  std::queue<std::tuple<frame_id_t, size_t>> victims_;
  std::vector<size_t> unpin_times_;
  size_t time_;  // time counter
  std::atomic<size_t> size_;
  SpinLock lock_;
};

}  // namespace bustub
