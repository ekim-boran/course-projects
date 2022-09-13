//===----------------------------------------------------------------------===//
//
//                         BusTub
//
// lru_replacer.cpp
//
// Identification: src/buffer/lru_replacer.cpp
//
// Copyright (c) 2015-2019, Carnegie Mellon University Database Group
//
//===----------------------------------------------------------------------===//

#include "buffer/lru_replacer.h"
#include <tuple>
#include "common/logger.h"
namespace bustub {

LRUReplacer::LRUReplacer(size_t num_pages) : unpin_times_(num_pages, 0) {
  time_ = 1;
  size_ = 0;
}

LRUReplacer::~LRUReplacer() = default;

bool LRUReplacer::Victim(frame_id_t *frame_id) {
  std::scoped_lock l(this->lock_);
  while (true) {
    if (victims_.empty()) {
      return false;
    }
    auto tuple = victims_.front();
    auto fid = std::get<0>(tuple);
    auto ftime = std::get<1>(tuple);

    victims_.pop();
    if (unpin_times_[fid] == ftime) {
      *frame_id = fid;
      unpin_times_[fid] = 0;
      size_--;
      return true;
    }
  }
}

void LRUReplacer::Pin(frame_id_t frame_id) {
  std::scoped_lock l(this->lock_);
  if (unpin_times_[frame_id] != 0) {
    size_--;
  }
  unpin_times_[frame_id] = 0;  // pinned
}

void LRUReplacer::Unpin(frame_id_t frame_id) {
  std::scoped_lock l(this->lock_);
  size_t unpin_time = time_++;
  if (unpin_times_[frame_id] == 0) {  // if it is pinned
    unpin_times_[frame_id] = unpin_time;
    size_++;
    victims_.emplace(std::make_tuple(frame_id, unpin_time));
  }
}

size_t LRUReplacer::Size() { return size_; }

}  // namespace bustub
