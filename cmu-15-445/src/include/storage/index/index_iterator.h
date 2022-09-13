//===----------------------------------------------------------------------===//
//
//                         CMU-DB Project (15-445/645)
//                         ***DO NO SHARE PUBLICLY***
//
// Identification: src/include/index/index_iterator.h
//
// Copyright (c) 2018, Carnegie Mellon University Database Group
//
//===----------------------------------------------------------------------===//
/**
 * index_iterator.h
 * For range scan of b+ tree
 */
#pragma once
#include <utility>
#include "storage/page/b_plus_tree_leaf_page.h"
namespace bustub {

#define INDEXITERATOR_TYPE IndexIterator<KeyType, ValueType, KeyComparator>

INDEX_TEMPLATE_ARGUMENTS
class IndexIterator {
 public:
  IndexIterator();
  IndexIterator(Page *leaf_page, int offset, BufferPoolManager *bpm);
  ~IndexIterator();
  IndexIterator &operator=(IndexIterator &&o) noexcept {
    leaf_page_ = (std::move(o.leaf_page_));
    offset_ = (std::move(o.offset_));
    bpm_ = (std::move(o.bpm_));
    o.offset_ = -1;
    o.bpm_ = nullptr;
    o.leaf_page_ = nullptr;
    // LOG_INFO("boran");
    return *this;
  }
  bool isEnd();

  const MappingType &operator*();

  IndexIterator &operator++();

  bool operator==(const IndexIterator &itr) const {
    bool r = leaf_page_ == itr.leaf_page_ && offset_ == itr.offset_ && bpm_ == itr.bpm_;
    return r;
  }

  bool operator!=(const IndexIterator &itr) const { return !(*this == itr); }

 private:
  Page *leaf_page_;
  int offset_;
  BufferPoolManager *bpm_;

  // add your own private member variables here
};

}  // namespace bustub
