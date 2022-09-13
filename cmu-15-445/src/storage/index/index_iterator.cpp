/**
 * index_iterator.cpp
 */
#include "storage/index/index_iterator.h"
#include <cassert>
#include "common/logger.h"
namespace bustub {

INDEX_TEMPLATE_ARGUMENTS
INDEXITERATOR_TYPE::IndexIterator() : IndexIterator(nullptr, -12, nullptr) {}

/*
 * NOTE: you can change the destructor/constructor method here
 * set your own input parameters
 */
INDEX_TEMPLATE_ARGUMENTS
INDEXITERATOR_TYPE::IndexIterator(Page *leaf_page, int offset, BufferPoolManager *bpm)
    : leaf_page_(leaf_page), offset_(offset), bpm_(bpm) {
  if (leaf_page_ != nullptr) {
    // LOG_INFO("constructor %d %p %p %d", offset_, this, bpm_, leaf_page_->GetPageId());
  }
}

INDEX_TEMPLATE_ARGUMENTS
INDEXITERATOR_TYPE::~IndexIterator() {
  if (offset_ != -1) {
    // LOG_INFO("--->ddd");
    // LOG_INFO("offset   %d %p %p", offset_, this, bpm_);

    leaf_page_->RUnlatch();
    bpm_->UnpinPage(leaf_page_->GetPageId(), false);
  }
}

INDEX_TEMPLATE_ARGUMENTS
bool INDEXITERATOR_TYPE::isEnd() { return offset_ == -1; }

INDEX_TEMPLATE_ARGUMENTS
const MappingType &INDEXITERATOR_TYPE::operator*() {
  if (offset_ == -1) {
    throw std::out_of_range("operation not permitted");
  }
  // LOG_INFO("access");
  auto *lp = reinterpret_cast<B_PLUS_TREE_LEAF_PAGE_TYPE *>(leaf_page_->GetData());
  return lp->GetItem(offset_);
}

INDEX_TEMPLATE_ARGUMENTS
INDEXITERATOR_TYPE &INDEXITERATOR_TYPE::operator++() {
  if (offset_ == -1) {
    throw std::out_of_range("operation not permitted");
  }
  auto *lp = reinterpret_cast<B_PLUS_TREE_LEAF_PAGE_TYPE *>(leaf_page_->GetData());

  offset_++;
  if (lp->GetSize() != offset_) {
    return *this;
  }
  page_id_t next = lp->GetNextPageId();

  if (next == INVALID_PAGE_ID) {
    leaf_page_->RUnlatch();
    bpm_->UnpinPage(leaf_page_->GetPageId(), false);
    leaf_page_ = nullptr;
    offset_ = -1;
    bpm_ = nullptr;
    return *this;
  }
  auto *new_page = bpm_->FetchPage(next);
  if (new_page == nullptr) {
    leaf_page_->RUnlatch();
    bpm_->UnpinPage(leaf_page_->GetPageId(), false);
    leaf_page_ = nullptr;
    offset_ = -1;
    bpm_ = nullptr;
    return *this;
  }
  new_page->RLatch();
  leaf_page_->RUnlatch();
  bpm_->UnpinPage(leaf_page_->GetPageId(), false);
  leaf_page_ = new_page;
  offset_ = 0;
  return *this;
}

template class IndexIterator<GenericKey<4>, RID, GenericComparator<4>>;

template class IndexIterator<GenericKey<8>, RID, GenericComparator<8>>;

template class IndexIterator<GenericKey<16>, RID, GenericComparator<16>>;

template class IndexIterator<GenericKey<32>, RID, GenericComparator<32>>;

template class IndexIterator<GenericKey<64>, RID, GenericComparator<64>>;

}  // namespace bustub
