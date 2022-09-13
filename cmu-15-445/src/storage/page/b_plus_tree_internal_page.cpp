//===----------------------------------------------------------------------===//
//
//                         CMU-DB Project (15-445/645)
//                         ***DO NO SHARE PUBLICLY***
//
// Identification: src/page/b_plus_tree_internal_page.cpp
//
// Copyright (c) 2018, Carnegie Mellon University Database Group
//
//===----------------------------------------------------------------------===//

#include <iostream>
#include <sstream>

#include "common/exception.h"
#include "common/logger.h"
#include "storage/page/b_plus_tree_internal_page.h"
namespace bustub {
/*****************************************************************************
 * HELPER METHODS AND UTILITIES
 *****************************************************************************/
INDEX_TEMPLATE_ARGUMENTS
void B_PLUS_TREE_INTERNAL_PAGE_TYPE::Init(page_id_t page_id, page_id_t parent_id, int max_size) {
  SetPageId(page_id);
  SetParentPageId(parent_id);
  SetMaxSize(max_size);
  SetSize(0);
  SetPageType(IndexPageType::INTERNAL_PAGE);
}
/*
 * Helper method to get/set the key associated with input "index"(a.k.a
 * array offset)
 */
INDEX_TEMPLATE_ARGUMENTS
KeyType B_PLUS_TREE_INTERNAL_PAGE_TYPE::KeyAt(int index) const { return array[index].first; }

INDEX_TEMPLATE_ARGUMENTS
void B_PLUS_TREE_INTERNAL_PAGE_TYPE::SetKeyAt(int index, const KeyType &key) { array[index].first = key; }

INDEX_TEMPLATE_ARGUMENTS
int B_PLUS_TREE_INTERNAL_PAGE_TYPE::ValueIndex(const ValueType &value) const {
  for (int i = 0; i < GetSize(); i++) {
    if (array[i].second == value) {
      return i;
    }
  }
  return -1;
}

INDEX_TEMPLATE_ARGUMENTS
ValueType B_PLUS_TREE_INTERNAL_PAGE_TYPE::ValueAt(int index) const { return array[index].second; }

/*
 * Find and return the child pointer(page_id) which points to the child page
 * that contains input "key"
 * Start the search from the second key(the first key should always be invalid)
 */

INDEX_TEMPLATE_ARGUMENTS
ValueType B_PLUS_TREE_INTERNAL_PAGE_TYPE::Lookup(const KeyType &key, const KeyComparator &comparator) const {
  int end = GetSize();
  int start = 1;
  int index = 1;
  int compareResult = 0;

  while (start < end) {
    index = (start + end) / 2;
    compareResult = comparator(key, array[index].first);
    if (compareResult == 0) {
      return array[index].second;
    }
    if (compareResult < 0) {
      end = index;
    } else {
      start = index + 1;
    }
  }

  if (index == end) {
    return array[index - 1].second;
  }
  return array[index].second;
}

/*****************************************************************************
 * INSERTION
 *****************************************************************************/
/*
 * Populate new root page with old_value + new_key & new_value
 * When the insertion cause overflow from leaf page all the way upto the root
 * page, you should create a new root page and populate its elements.
 * NOTE: This method is only called within InsertIntoParent()(b_plus_tree.cpp)
 */
INDEX_TEMPLATE_ARGUMENTS
void B_PLUS_TREE_INTERNAL_PAGE_TYPE::PopulateNewRoot(const ValueType &old_value, const KeyType &new_key,
                                                     const ValueType &new_value) {
  array[0] = std::make_pair(KeyType{}, old_value);
  array[1] = std::make_pair(new_key, new_value);
  SetSize(2);
}
/*
 * Insert new_key & new_value pair right after the pair with its value ==
 * old_value
 * @return:  new size after insertion
 */
INDEX_TEMPLATE_ARGUMENTS
int B_PLUS_TREE_INTERNAL_PAGE_TYPE::InsertNodeAfter(const ValueType &old_value, const KeyType &new_key,
                                                    const ValueType &new_value) {
  int value_index = ValueIndex(old_value);
  for (int i = GetSize(); i > value_index + 1; i = i - 1) {
    array[i] = array[i - 1];
  }
  array[value_index + 1] = std::make_pair(new_key, new_value);
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
void B_PLUS_TREE_INTERNAL_PAGE_TYPE::MoveHalfTo(BPlusTreeInternalPage *recipient,
                                                BufferPoolManager *buffer_pool_manager) {
  int size = GetSize();
  int mysize = size / 2;
  int othersize = size - mysize;
  SetSize(mysize);
  recipient->SetSize(0);
  recipient->CopyNFrom(&array[mysize], othersize, buffer_pool_manager);
}

/* Copy entries into me, starting from {items} and copy {size} entries.
 * Since it is an internal page, for all entries (pages) moved, their parents page now changes to me.
 * So I need to 'adopt' them by changing their parent page id, which needs to be persisted with BufferPoolManger
 */

void changeChildsParentId(page_id_t child_pid, page_id_t my_pid, BufferPoolManager *buffer_pool_manager) {
  Page *page = buffer_pool_manager->FetchPage(child_pid);
  if (page == nullptr) {
    throw std::bad_alloc();
  }
  BPlusTreePage *child = reinterpret_cast<BPlusTreePage *>(page->GetData());
  child->SetParentPageId(my_pid);
  buffer_pool_manager->UnpinPage(child_pid, true);
}

INDEX_TEMPLATE_ARGUMENTS
void B_PLUS_TREE_INTERNAL_PAGE_TYPE::CopyNFrom(const MappingType *items, int size,
                                               BufferPoolManager *buffer_pool_manager) {
  int start = GetSize();  // zero when merging
  for (int i = 0; i < size; i++) {
    array[start + i] = items[i];
  }
  for (int i = 0; i < size; i++) {
    page_id_t pid = items[i].second;
    changeChildsParentId(pid, GetPageId(), buffer_pool_manager);
  }
  SetSize(start + size);
}

/*****************************************************************************
 * REMOVE
 *****************************************************************************/
/*
 * Remove the key & value pair in internal page according to input index(a.k.a
 * array offset)
 * NOTE: store key&value pair continuously after deletion
 */
INDEX_TEMPLATE_ARGUMENTS
void B_PLUS_TREE_INTERNAL_PAGE_TYPE::Remove(int index) {
  for (int i = index; i < GetSize() - 1; i++) {
    array[i] = array[i + 1];
  }
  IncreaseSize(-1);
}

/*
 * Remove the only key & value pair in internal page and return the value
 * NOTE: only call this method within AdjustRoot()(in b_plus_tree.cpp)
 */
INDEX_TEMPLATE_ARGUMENTS
ValueType B_PLUS_TREE_INTERNAL_PAGE_TYPE::RemoveAndReturnOnlyChild() { return INVALID_PAGE_ID; }
/*****************************************************************************
 * MERGE
 *****************************************************************************/
/*
 * Remove all of key & value pairs from this page to "recipient" page.
 * The middle_key is the separation key you should get from the parent. You need
 * to make sure the middle key is added to the recipient to maintain the invariant.
 * You also need to use BufferPoolManager to persist changes to the parent page id for those
 * pages that are moved to the recipient
 */
INDEX_TEMPLATE_ARGUMENTS
void B_PLUS_TREE_INTERNAL_PAGE_TYPE::MoveAllTo(BPlusTreeInternalPage *recipient, const KeyType &middle_key,
                                               BufferPoolManager *buffer_pool_manager) {
  recipient->CopyNFrom(array, GetSize(), buffer_pool_manager);
}

/*****************************************************************************
 * REDISTRIBUTE
 *****************************************************************************/
/*
 * Remove the first key & value pair from this page to tail of "recipient" page.
 *
 * The middle_key is the separation key you should get from the parent. You need
 * to make sure the middle key is added to the recipient to maintain the invariant.
 * You also need to use BufferPoolManager to persist changes to the parent page id for those
 * pages that are moved to the recipient
 */
INDEX_TEMPLATE_ARGUMENTS
void B_PLUS_TREE_INTERNAL_PAGE_TYPE::MoveFirstToEndOf(BPlusTreeInternalPage *recipient, const KeyType &middle_key,
                                                      BufferPoolManager *buffer_pool_manager) {
  auto item = array[0];
  Remove(0);
  item.first = middle_key;
  recipient->CopyLastFrom(item, buffer_pool_manager);
}

/* Append an entry at the end.
 * Since it is an internal page, the moved entry(page)'s parent needs to be updated.
 * So I need to 'adopt' it by changing its parent page id, which needs to be persisted with BufferPoolManger
 */
INDEX_TEMPLATE_ARGUMENTS
void B_PLUS_TREE_INTERNAL_PAGE_TYPE::CopyLastFrom(const MappingType &pair, BufferPoolManager *buffer_pool_manager) {
  array[GetSize()] = pair;
  IncreaseSize(1);
  page_id_t pid = pair.second;
  changeChildsParentId(pid, GetPageId(), buffer_pool_manager);
}

/*
 * Remove the last key & value pair from this page to head of "recipient" page.
 * You need to handle the original dummy key properly, e.g. updating recipient’s array to position the middle_key at the
 * right place.
 * You also need to use BufferPoolManager to persist changes to the parent page id for those pages that are
 * moved to the recipient
 */
INDEX_TEMPLATE_ARGUMENTS
void B_PLUS_TREE_INTERNAL_PAGE_TYPE::MoveLastToFrontOf(BPlusTreeInternalPage *recipient, const KeyType &middle_key,
                                                       BufferPoolManager *buffer_pool_manager) {
  auto last = array[GetSize() - 1];
  IncreaseSize(-1);
  recipient->CopyFirstFrom(last, buffer_pool_manager);
  recipient->SetKeyAt(1, middle_key);
}

/* Append an entry at the beginning.
 * Since it is an internal page, the moved entry(page)'s parent needs to be updated.
 * So I need to 'adopt' it by changing its parent page id, which needs to be persisted with BufferPoolManger
 */
INDEX_TEMPLATE_ARGUMENTS
void B_PLUS_TREE_INTERNAL_PAGE_TYPE::CopyFirstFrom(const MappingType &pair, BufferPoolManager *buffer_pool_manager) {
  for (int i = GetSize(); i > 0; i--) {
    array[i] = array[i - 1];
  }
  array[0] = pair;
  IncreaseSize(1);
  page_id_t pid = array[0].second;
  changeChildsParentId(pid, GetPageId(), buffer_pool_manager);
}

INDEX_TEMPLATE_ARGUMENTS
void B_PLUS_TREE_INTERNAL_PAGE_TYPE::Redistribute(BPlusTreeInternalPage *neighbor_node, int index,
                                                  BufferPoolManager *buffer_pool_manager) {
  page_id_t parent_id = this->GetParentPageId();
  Page *parent_page = buffer_pool_manager->FetchPage(parent_id);
  B_PLUS_TREE_INTERNAL_PAGE_TYPE *parent = reinterpret_cast<B_PLUS_TREE_INTERNAL_PAGE_TYPE *>(parent_page->GetData());
  if (index == 0) {
    auto key = parent->KeyAt(1);
    neighbor_node->MoveFirstToEndOf(this, key, buffer_pool_manager);
    parent->SetKeyAt(1, neighbor_node->KeyAt(0));
  } else {
    auto key = parent->KeyAt(index);
    neighbor_node->MoveLastToFrontOf(this, key, buffer_pool_manager);
    parent->SetKeyAt(index, this->KeyAt(0));
  }
  buffer_pool_manager->UnpinPage(parent_id, true);
}

// valuetype for internalNode should be page id_t
template class BPlusTreeInternalPage<GenericKey<4>, page_id_t, GenericComparator<4>>;
template class BPlusTreeInternalPage<GenericKey<8>, page_id_t, GenericComparator<8>>;
template class BPlusTreeInternalPage<GenericKey<16>, page_id_t, GenericComparator<16>>;
template class BPlusTreeInternalPage<GenericKey<32>, page_id_t, GenericComparator<32>>;
template class BPlusTreeInternalPage<GenericKey<64>, page_id_t, GenericComparator<64>>;
}  // namespace bustub
