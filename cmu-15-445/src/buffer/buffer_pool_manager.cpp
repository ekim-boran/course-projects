//===----------------------------------------------------------------------===//
//
//                         BusTub
//
// buffer_pool_manager.cpp
//
// Identification: src/buffer/buffer_pool_manager.cpp
//
// Copyright (c) 2015-2019, Carnegie Mellon University Database Group
//
//===----------------------------------------------------------------------===//

#include "buffer/buffer_pool_manager.h"

#include <list>
#include <unordered_map>

namespace bustub {

BufferPoolManager::BufferPoolManager(size_t pool_size, DiskManager *disk_manager, LogManager *log_manager)
    : pool_size_(pool_size), disk_manager_(disk_manager), log_manager_(log_manager) {
  if (pool_size > 10) {
    num_instances_ = NUMPOOLS;
  } else {
    num_instances_ = 1;
  }
  // We allocate a consecutive memory space for the buffer pool.
  start_index_ = 0;
  auto *pages = new Page[pool_size];

  for (int i = 0; i < num_instances_; i++) {
    pages_[i] = pages + ((i * pool_size) / num_instances_);
    replacer_[i] = new LRUReplacer(pool_size / num_instances_);
    for (size_t j = 0; j < pool_size_ / num_instances_; ++j) {
      free_list_[i].emplace_back(static_cast<int>(j));
    }
    last_ids[i] = i;
  }
}

BufferPoolManager::~BufferPoolManager() {
  delete[] pages_[0];
  for (int i = 0; i < num_instances_; i++) {
    delete replacer_[i];
  }
}

bool BufferPoolManager::GetPageTable(page_id_t page_id, frame_id_t *frame_id) {
  auto index = page_id % num_instances_;

  auto entry = page_table_[index].find(page_id);
  if (page_table_[index].end() != entry) {
    *frame_id = entry->second;
    return true;
  }
  return false;
}

bool BufferPoolManager::GetFreeList(frame_id_t *frame_id, int index) {
  if (!free_list_[index].empty()) {
    *frame_id = free_list_[index].front();
    free_list_[index].pop_front();
    return true;
  }
  return false;
}

Page *BufferPoolManager::FetchPageImpl(page_id_t page_id) {
  auto index = page_id % num_instances_;
  const std::scoped_lock lock(latch_[index]);
  frame_id_t frame_id;
  if (GetPageTable(page_id, &frame_id)) {
    Page *page = &pages_[index][frame_id];
    page->pin_count_ += 1;
    replacer_[index]->Pin(frame_id);

    return page;
  }
  // page table does not contain the page
  if (GetFreeList(&frame_id, index)) {
    page_table_[index][page_id] = frame_id;
    Page *page = &pages_[index][frame_id];
    page->pin_count_ = 1;
    page->page_id_ = page_id;
    ReadPage(page_id, page->data_);
    return page;
  }

  if (replacer_[index]->Victim(&frame_id)) {
    Page *page = &pages_[index][frame_id];
    page_table_[index].erase(page->page_id_);
    if (page->IsDirty()) {
      WritePage(page->page_id_, page->data_);
    }
    ReadPage(page_id, page->data_);
    page_table_[index][page_id] = frame_id;
    page->page_id_ = page_id;
    page->pin_count_ = 1;
    page->is_dirty_ = false;
    return page;
  }

  return nullptr;
}

bool BufferPoolManager::UnpinPageImpl(page_id_t page_id, bool is_dirty) {
  auto index = page_id % num_instances_;
  const std::scoped_lock lock(latch_[index]);

  frame_id_t frame_id;
  if (GetPageTable(page_id, &frame_id) && pages_[index][frame_id].pin_count_ > 0) {
    Page *page = &pages_[index][frame_id];
    page->pin_count_ -= 1;
    page->is_dirty_ = is_dirty || page->is_dirty_;
    if (page->pin_count_ == 0) {
      replacer_[index]->Unpin(frame_id);
    }
    return true;
  }
  return false;
}

bool BufferPoolManager::FlushPageImpl(page_id_t page_id) {
  auto index = page_id % num_instances_;

  const std::scoped_lock lock(latch_[index]);
  frame_id_t frame_id;
  bool exists = GetPageTable(page_id, &frame_id);
  if (exists) {
    Page *page = &pages_[index][frame_id];
    WritePage(page->page_id_, page->data_);
    page->is_dirty_ = false;
  }

  return exists;
}

Page *BufferPoolManager::NewPageImpl1(page_id_t *page_id, int index) {
  const std::scoped_lock lock(latch_[index]);
  frame_id_t frame_id = -1;
  if (GetFreeList(&frame_id, index)) {
    page_id_t pid = AllocatePage(index);
    Page *page = pages_[index] + frame_id;
    page_table_[index][pid] = frame_id;
    page->ResetMemory();
    page->page_id_ = pid;
    page->is_dirty_ = false;
    page->pin_count_ = 1;
    *page_id = page->page_id_;
    return page;
  }
  if (replacer_[index]->Victim(&frame_id)) {
    Page *page = &pages_[index][frame_id];
    page_table_[index].erase(page->page_id_);
    if (page->IsDirty()) {
      WritePage(page->page_id_, page->data_);
    }
    // page->ResetMemory();
    page_id_t pid = AllocatePage(index);
    page_table_[index][pid] = frame_id;
    page->page_id_ = pid;
    page->is_dirty_ = false;
    page->pin_count_ = 1;
    *page_id = page->page_id_;

    return page;
  }
  return nullptr;
}

Page *BufferPoolManager::NewPageImpl(page_id_t *page_id) {
  u_int32_t index = start_index_.fetch_add(1) % num_instances_;
  const u_int32_t start = index;
  Page *result = nullptr;
  do {
    result = NewPageImpl1(page_id, index);
    index = (index + 1) % num_instances_;
  } while (result == nullptr && index != start);

  return result;
}

bool BufferPoolManager::DeletePageImpl(page_id_t page_id) {
  auto index = page_id % num_instances_;

  if (page_id == INVALID_PAGE_ID) {
    return true;
  }
  const std::scoped_lock lock(latch_[index]);
  frame_id_t frame_id;
  if (GetPageTable(page_id, &frame_id)) {
    Page *page = &pages_[index][frame_id];
    if (page->pin_count_ > 0) {
      return false;
    }
    page_table_[index].erase(page_id);
    free_list_[index].emplace_back(frame_id);
    replacer_[index]->Pin(frame_id);
    // page->ResetMemory();
    page->pin_count_ = 0;
    page->is_dirty_ = false;
    page->page_id_ = INVALID_PAGE_ID;
  }
  disk_manager_->DeallocatePage(page_id);

  return true;
}

void BufferPoolManager::FlushAllPagesImpl() {
  for (int i = 0; i < num_instances_; i++) {
    latch_[i].lock();
    for (auto tuple : page_table_[0]) {
      auto frame_id = std::get<1>(tuple);
      Page *page = &pages_[i][frame_id];
      WritePage(page->page_id_, page->data_);
      page->is_dirty_ = false;
    }
    latch_[i].unlock();
  }
}

void BufferPoolManager::WritePage(page_id_t page_id, const char *page_data) {
  std::scoped_lock l(db_io_latch_);
  disk_manager_->WritePage(page_id, page_data);
}

void BufferPoolManager::ReadPage(page_id_t page_id, char *page_data) {
  std::scoped_lock l(db_io_latch_);
  disk_manager_->ReadPage(page_id, page_data);
}

int BufferPoolManager::AllocatePage(int index) {
  const page_id_t next_page_id = last_ids[index];
  last_ids[index] += num_instances_;
  return next_page_id;
}

}  // namespace bustub
