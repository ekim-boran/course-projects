//===----------------------------------------------------------------------===//
//
//                         CMU-DB Project (15-445/645)
//                         ***DO NO SHARE PUBLICLY***
//
// Identification: src/page/b_plus_tree_leaf_page.cpp
//
// Copyright (c) 2018, Carnegie Mellon University Database Group
//
//===----------------------------------------------------------------------===//

#include "storage/page/b_plus_tree_leaf_page.h"
#include <sstream>
#include "common/exception.h"
#include "common/logger.h"
#include "common/rid.h"
#include "storage/page/b_plus_tree_internal_page.h"

namespace bustub {

/*****************************************************************************
 * HELPER METHODS AND UTILITIES
 *****************************************************************************/

/**
 * Init method after creating a new leaf page
 * Including set page type, set current size to zero, set page id/parent id, set
 * next page id and set max size
 */
INDEX_TEMPLATE_ARGUMENTS
void B_PLUS_TREE_LEAF_PAGE_TYPE::Init(page_id_t page_id, page_id_t parent_id, int max_size) {
  SetPageId(page_id);
  SetParentPageId(parent_id);
  SetMaxSize(max_size);
  SetNextPageId(INVALID_PAGE_ID);
  SetSize(0);
  SetPageType(IndexPageType::LEAF_PAGE);
}

/**
 * Helper methods to set/get next page id
 */
INDEX_TEMPLATE_ARGUMENTS
std::tuple<bool, int> B_PLUS_TREE_LEAF_PAGE_TYPE::GetPosition(const KeyType &key,
                                                              const KeyComparator &comparator) const {
  int end = GetSize();
  int start = 0;
  int index = 0;
  int compareResult = -1;

  while (start < end) {
    index = (start + end) / 2;
    compareResult = comparator(key, array[index].first);
    if (compareResult == 0) {
      return std::make_tuple(true, index);
    }
    if (compareResult < 0) {
      end = index;
    } else {
      start = index + 1;
    }
  }
  return std::make_tuple(false, start);
}

INDEX_TEMPLATE_ARGUMENTS
page_id_t B_PLUS_TREE_LEAF_PAGE_TYPE::GetNextPageId() const { return next_page_id_; }

INDEX_TEMPLATE_ARGUMENTS
void B_PLUS_TREE_LEAF_PAGE_TYPE::SetNextPageId(page_id_t next_page_id) { next_page_id_ = next_page_id; }

/**
 * Helper method to find the first index i so that array[i].first >= key
 * NOTE: This method is only used when generating index iterator
 */
INDEX_TEMPLATE_ARGUMENTS
int B_PLUS_TREE_LEAF_PAGE_TYPE::KeyIndex(const KeyType &key, const KeyComparator &comparator) const {
  auto [found, index] = GetPosition(key, comparator);
  if (!found && (index == GetSize() || index == 0)) {
    return -1;
  }  // not found and it does not exists
  return index;
}

/*
 * Helper method to find and return the key associated with input "index"(a.k.a
 * array offset)
 */
INDEX_TEMPLATE_ARGUMENTS
KeyType B_PLUS_TREE_LEAF_PAGE_TYPE::KeyAt(int index) const {
  // replace with your own code
  return array[index].first;
}

/*
 * Helper method to find and return the key & value pair associated with input
 * "index"(a.k.a array offset)
 */
INDEX_TEMPLATE_ARGUMENTS
const MappingType &B_PLUS_TREE_LEAF_PAGE_TYPE::GetItem(int index) {
  // replace with your own code
  return array[index];
}

/*****************************************************************************
 * INSERTION
 *****************************************************************************/

INDEX_TEMPLATE_ARGUMENTS
int B_PLUS_TREE_LEAF_PAGE_TYPE::Insert(const KeyType &key, const ValueType &value, const KeyComparator &comparator) {
  auto [found, index] = GetPosition(key, comparator);
  int old_size = GetSize();
  if (found) {
    return old_size;
  }
  for (int i = old_size; i > index; i--) {
    array[i] = array[i - 1];
  }
  array[index] = std::make_pair(key, value);
  IncreaseSize(1);
  return GetSize();
}

/*****************************************************************************
 * SPLIT
 *****************************************************************************/
/*
 * Remove half of key & value pairs from this page to "recipient" page
 */
INDEX_TEMPLATE_ARGUMENTS
void B_PLUS_TREE_LEAF_PAGE_TYPE::MoveHalfTo(BPlusTreeLeafPage *recipient) {
  int mysize = GetSize() / 2;
  int othersize = GetSize() - mysize;
  SetSize(mysize);
  recipient->SetSize(0);
  recipient->CopyNFrom(&array[mysize], othersize);
}

/*
 * Copy starting from items, and copy {size} number of elements into me.
 */
INDEX_TEMPLATE_ARGUMENTS void B_PLUS_TREE_LEAF_PAGE_TYPE::CopyNFrom(const MappingType *items, int size) {
  int start = GetSize();
  for (int i = 0; i < size; i++) {
    array[i + start] = items[i];
  }
  SetSize(start + size);
}

/*****************************************************************************
 * LOOKUP
 *****************************************************************************/
/*
 * For the given key, check to see whether it exists in the leaf page. If it
 * does, then store its corresponding value in input "value" and return true.
 * If the key does not exist, then return false
 */
INDEX_TEMPLATE_ARGUMENTS
bool B_PLUS_TREE_LEAF_PAGE_TYPE::Lookup(const KeyType &key, ValueType *value, const KeyComparator &comparator) const {
  auto [found, index] = GetPosition(key, comparator);
  if (found) {
    *value = array[index].second;
  }
  return found;
}

/*****************************************************************************
 * REMOVE
 *****************************************************************************/
/*
 * First look through leaf page to see whether delete key exist or not. If
 * exist, perform deletion, otherwise return immediately.
 * NOTE: store key&value pair continuously after deletion
 * @return   page size after deletion
 */
INDEX_TEMPLATE_ARGUMENTS
int B_PLUS_TREE_LEAF_PAGE_TYPE::RemoveAndDeleteRecord(const KeyType &key, const KeyComparator &comparator) {
  auto [found, index] = GetPosition(key, comparator);
  if (!found) {
    return GetSize();
  }
  // std::memmove(reinterpret_cast<void *>(&array[index]), reinterpret_cast<void *>(&array[index + 1]),
  //             (GetSize() - index - 1) * sizeof(MappingType));
  for (int i = index; i < GetSize() - 1; i++) {
    array[i] = array[i + 1];
  }
  IncreaseSize(-1);
  return GetSize();
}

/*****************************************************************************
 * MERGE
 *****************************************************************************/
/*
 * Remove all of key & value pairs from this page to "recipient" page. Don't forget
 * to update the next_page id in the sibling page
 */
INDEX_TEMPLATE_ARGUMENTS
void B_PLUS_TREE_LEAF_PAGE_TYPE::MoveAllTo(BPlusTreeLeafPage *recipient, const KeyType &middle_key,
                                           BufferPoolManager *buffer_pool_manager) {
  //      LOG_DEBUG("xxxxxxxxxxxx");
  recipient->CopyNFrom(array, GetSize());
  // is it correct place
  recipient->SetNextPageId(GetNextPageId());
}

/*****************************************************************************
 * REDISTRIBUTE
 *****************************************************************************/
/*
 * Remove the first key & value pair from this page to "recipient" page.
 */
INDEX_TEMPLATE_ARGUMENTS
void B_PLUS_TREE_LEAF_PAGE_TYPE::MoveFirstToEndOf(BPlusTreeLeafPage *recipient) {
  auto first = array[0];
  for (int i = 0; i < GetSize() - 1; i++) {
    array[i] = array[i + 1];
  }
  IncreaseSize(-1);
  recipient->CopyLastFrom(first);
}

/*
 * Copy the item into the end of my item list. (Append item to my array)
 */
INDEX_TEMPLATE_ARGUMENTS
void B_PLUS_TREE_LEAF_PAGE_TYPE::CopyLastFrom(const MappingType &item) {
  array[GetSize()] = item;
  IncreaseSize(1);
}

INDEX_TEMPLATE_ARGUMENTS
void B_PLUS_TREE_LEAF_PAGE_TYPE::Redistribute(BPlusTreeLeafPage *neighbor_node, int index,
                                              BufferPoolManager *buffer_pool_manager) {
  page_id_t parent_id = this->GetParentPageId();
  Page *parent_page = buffer_pool_manager->FetchPage(parent_id);
  BPlusTreeInternalPage<KeyType, page_id_t, KeyComparator> *parent =
      reinterpret_cast<BPlusTreeInternalPage<KeyType, page_id_t, KeyComparator> *>(parent_page->GetData());

  if (index == 0) {
    neighbor_node->MoveFirstToEndOf(this);
    parent->SetKeyAt(1, neighbor_node->KeyAt(0));
  } else {
    neighbor_node->MoveLastToFrontOf(this);
    parent->SetKeyAt(index, this->KeyAt(0));
  }
  buffer_pool_manager->UnpinPage(parent_id, true);
}

/*
 * Remove the last key & value pair from this page to "recipient" page.
 */
INDEX_TEMPLATE_ARGUMENTS
void B_PLUS_TREE_LEAF_PAGE_TYPE::MoveLastToFrontOf(BPlusTreeLeafPage *recipient) {
  auto last = array[GetSize() - 1];
  IncreaseSize(-1);
  recipient->CopyFirstFrom(last);
}

/*
 * Insert item at the front of my items. Move items accordingly.
 */
INDEX_TEMPLATE_ARGUMENTS
void B_PLUS_TREE_LEAF_PAGE_TYPE::CopyFirstFrom(const MappingType &item) {
  for (int i = GetSize(); i > 0; i--) {
    array[i] = array[i - 1];
  }
  array[0] = item;
  IncreaseSize(1);
}

template class BPlusTreeLeafPage<GenericKey<4>, RID, GenericComparator<4>>;
template class BPlusTreeLeafPage<GenericKey<8>, RID, GenericComparator<8>>;
template class BPlusTreeLeafPage<GenericKey<16>, RID, GenericComparator<16>>;
template class BPlusTreeLeafPage<GenericKey<32>, RID, GenericComparator<32>>;
template class BPlusTreeLeafPage<GenericKey<64>, RID, GenericComparator<64>>;
}  // namespace bustub
