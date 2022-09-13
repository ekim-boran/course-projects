//===----------------------------------------------------------------------===//
//
//                         CMU-DB Project (15-445/645)
//                         ***DO NO SHARE PUBLICLY***
//
// Identification: src/index/b_plus_tree.cpp
//
// Copyright (c) 2018, Carnegie Mellon University Database Group
//
//===----------------------------------------------------------------------===//

#include <string>

#include <deque>
#include "common/exception.h"
#include "common/rid.h"
#include "storage/index/b_plus_tree.h"
#include "storage/page/header_page.h"
namespace bustub {
#define InternalType BPlusTreeInternalPage<KeyType, page_id_t, KeyComparator>

INDEX_TEMPLATE_ARGUMENTS
BPLUSTREE_TYPE::BPlusTree(std::string name, BufferPoolManager *buffer_pool_manager, const KeyComparator &comparator,
                          int leaf_max_size, int internal_max_size)
    : index_name_(std::move(name)),
      root_page_id_(INVALID_PAGE_ID),
      buffer_pool_manager_(buffer_pool_manager),
      comparator_(comparator),
      leaf_max_size_(leaf_max_size),
      internal_max_size_(internal_max_size) {}

/*
 * Helper function to decide whether current b+tree is empty
 */
INDEX_TEMPLATE_ARGUMENTS
bool BPLUSTREE_TYPE::IsEmpty() const { return true; }
/*****************************************************************************
 * SEARCH
 *****************************************************************************/
/*
 * Return the only value that associated with input key
 * This method is used for point query
 * @return : true means key exists
 */
INDEX_TEMPLATE_ARGUMENTS
bool BPLUSTREE_TYPE::GetValue(const KeyType &key, std::vector<ValueType> *result, Transaction *transaction) {
  Page *p = FindLeafOptimistic(key, Operation::Read);
  B_PLUS_TREE_LEAF_PAGE_TYPE *page = reinterpret_cast<B_PLUS_TREE_LEAF_PAGE_TYPE *>(p->GetData());
  ValueType v;
  auto r = page->Lookup(key, &v, comparator_);
  if (r) {
    result->push_back(v);
  }
  p->RUnlatch();
  buffer_pool_manager_->UnpinPage(p->GetPageId(), false);
  return r;
}

/*
 * Insert constant key & value pair into b+ tree
 * if current tree is empty, start new tree, update root page id and insert
 * entry, otherwise insert into leaf page.
 * @return: since we only support unique key, if user try to insert duplicate
 * keys return false, otherwise return true.
 */
INDEX_TEMPLATE_ARGUMENTS
bool BPLUSTREE_TYPE::Insert(const KeyType &key, const ValueType &value, Transaction *transaction) {
  if (!initialized.exchange(true)) {
    StartNewTree();
  }

  while (root_page_id_ == INVALID_PAGE_ID) {
  }

  bool modified = InsertIntoLeaf(key, value, transaction);
  commit_all(transaction, modified);
  return modified;
}

/*
 * Insert constant key & value pair into an empty tree
 * User needs to first ask for new page from buffer pool manager(NOTICE: throw
 * an "out of memory" exception if returned value is nullptr), then update b+
 * tree's root page id and insert entry directly into leaf page.
 */
INDEX_TEMPLATE_ARGUMENTS
void BPLUSTREE_TYPE::StartNewTree() {
  page_id_t root_id;
  Page *page = buffer_pool_manager_->NewPage(&root_id);
  if (page == nullptr) {
    throw std::bad_alloc();
  }
  root_page_id_ = root_id;
  B_PLUS_TREE_LEAF_PAGE_TYPE *leaf_page = reinterpret_cast<B_PLUS_TREE_LEAF_PAGE_TYPE *>(page->GetData());
  leaf_page->Init(root_page_id_, INVALID_PAGE_ID, leaf_max_size_);
  buffer_pool_manager_->UnpinPage(root_page_id_, true);
  UpdateRootPageId(true);
}

/*
 * Insert constant key & value pair into leaf page
 * User needs to first find the right leaf page as insertion target, then look
 * through leaf page to see whether insert key exist or not. If exist, return
 * immdiately, otherwise insert entry. Remember to deal with split if necessary.
 * @return: since we only support unique key, if user try to insert duplicate
 * keys return false, otherwise return true.
 */

INDEX_TEMPLATE_ARGUMENTS
bool BPLUSTREE_TYPE::InsertIntoLeaf(const KeyType &key, const ValueType &value, Transaction *transaction) {
  Page *p = FindLeafPage(key, transaction, Operation::Insert);
  B_PLUS_TREE_LEAF_PAGE_TYPE *leaf_page = reinterpret_cast<B_PLUS_TREE_LEAF_PAGE_TYPE *>(p->GetData());
  int old_size = leaf_page->GetSize();
  int new_size = leaf_page->Insert(key, value, comparator_);
  if (old_size == new_size) {
    return false;
  }
  if (leaf_page->GetSize() >= leaf_page->GetMaxSize()) {
    B_PLUS_TREE_LEAF_PAGE_TYPE *sibling = Split(leaf_page, transaction);
    leaf_page->MoveHalfTo(sibling);
    sibling->SetNextPageId(leaf_page->GetNextPageId());
    leaf_page->SetNextPageId(sibling->GetPageId());
    InsertIntoParent(leaf_page, sibling->KeyAt(0), sibling, transaction);
  }
  return true;
}

/*
 * Split input page and return newly created page.
 * Using template N to represent either internal page or leaf page.
 * User needs to first ask for new page from buffer pool manager(NOTICE: throw
 * an "out of memory" exception if returned value is nullptr), then move half
 * of key & value pairs from input page to newly created page
 */
INDEX_TEMPLATE_ARGUMENTS
template <typename N>
N *BPLUSTREE_TYPE::Split(N *node, Transaction *txn) {
  page_id_t pid;
  Page *page = buffer_pool_manager_->NewPage(&pid);
  if (page == nullptr) {
    throw std::bad_alloc();
  }
  page->WLatch();
  txn->AddIntoPageSet(page);
  N *new_page = reinterpret_cast<N *>(page->GetData());
  new_page->Init(pid, node->GetParentPageId(), node->GetMaxSize());
  return new_page;
}

/*
 * Insert key & value pair into internal page after split
 * @param   old_node      input page from split() method
 * @param   key
 * @param   new_node      returned page from split() method
 * User needs to first find the parent page of old_node, parent node must be
 * adjusted to take info of new_node into account. Remember to deal with split
 * recursively if necessary.
 */

INDEX_TEMPLATE_ARGUMENTS
void BPLUSTREE_TYPE::InsertIntoParent(BPlusTreePage *old_node, const KeyType &key, BPlusTreePage *new_node,
                                      Transaction *transaction) {
  if (old_node->GetParentPageId() == INVALID_PAGE_ID) {
    page_id_t pid;
    Page *page = buffer_pool_manager_->NewPage(&pid);
    page->WLatch();
    transaction->AddIntoPageSet(page);
    InternalType *new_root = reinterpret_cast<InternalType *>(page->GetData());

    new_root->Init(pid, INVALID_PAGE_ID, internal_max_size_);
    old_node->SetParentPageId(pid);
    new_node->SetParentPageId(pid);
    new_root->PopulateNewRoot(old_node->GetPageId(), key, new_node->GetPageId());
    root_page_id_ = pid;
    UpdateRootPageId(false);

    return;
  }
  // already in transaction
  Page *parent = fetch_page(transaction, old_node->GetParentPageId());
  InternalType *parent_internal = reinterpret_cast<InternalType *>(parent->GetData());
  parent_internal->InsertNodeAfter(old_node->GetPageId(), key, new_node->GetPageId());

  if (parent_internal->GetSize() == parent_internal->GetMaxSize()) {
    InternalType *sibling = Split(parent_internal, transaction);
    parent_internal->MoveHalfTo(sibling, buffer_pool_manager_);
    InsertIntoParent(parent_internal, sibling->KeyAt(0), sibling, transaction);
  }
}

INDEX_TEMPLATE_ARGUMENTS
void BPLUSTREE_TYPE::Remove(const KeyType &key, Transaction *transaction) {
  // LOG_INFO("%ld", key.ToString());
  Page *p = FindLeafPage(key, transaction, Operation::Remove);

  B_PLUS_TREE_LEAF_PAGE_TYPE *leaf_page = reinterpret_cast<B_PLUS_TREE_LEAF_PAGE_TYPE *>(p->GetData());

  int old_size = leaf_page->GetSize();
  int new_size = leaf_page->RemoveAndDeleteRecord(key, comparator_);
  if (old_size == new_size) {
    // LOG_INFO("remove not exists %d %ld", leaf_page->GetPageId(), key.ToString());

    commit_all(transaction, false);
    return;
  }

  if (leaf_page->GetSize() < leaf_page->GetMinSize()) {
    CoalesceOrRedistribute(leaf_page, transaction);
  }
  commit_all(transaction, true);
  // LOG_INFO("remove end %ld", key.ToString());
}

/*
 * User needs to first find the sibling of input page. If sibling's size + input
 * page's size > page's max size, then redistribute. Otherwise, merge.
 * Using template N to represent either internal page or leaf page.
 * @return: true means target leaf page should be deleted, false means no
 * deletion happens
 */

// return: a page is deleted or not
INDEX_TEMPLATE_ARGUMENTS
template <typename N>
bool BPLUSTREE_TYPE::CoalesceOrRedistribute(N *node, Transaction *transaction) {
  if (node->IsRootPage()) {
    if (node->GetSize() == 1 || node->GetSize() == 0) {
      bool is_delete = AdjustRoot(node, transaction);
      if (is_delete) {
        buffer_pool_manager_->DeletePage(node->GetPageId());
      }
      return is_delete;
    }
    return false;
  }

  Page *parent_page = fetch_page(transaction, node->GetParentPageId());
  InternalType *parent = reinterpret_cast<InternalType *>(parent_page->GetData());
  page_id_t mypageid = node->GetPageId();
  int index = parent->ValueIndex(mypageid);
  if (index != 0) {  // try to redistribute from left
    page_id_t left_sibling_id = parent->ValueAt(index - 1);
    Page *left_sibling_page = fetch_page(transaction, left_sibling_id);
    N *left_sibling = reinterpret_cast<N *>(left_sibling_page->GetData());
    if (left_sibling->GetSize() - 1 >= left_sibling->GetMinSize()) {
      Redistribute(left_sibling, node, index);
      return false;
    }
    Coalesce(&left_sibling, &node, &parent, index, transaction);
    return true;
  }
  page_id_t right_sibling_id = parent->ValueAt(index + 1);
  Page *right_sibling_page = fetch_page(transaction, right_sibling_id);

  N *right_sibling = reinterpret_cast<N *>(right_sibling_page->GetData());
  if (right_sibling->GetSize() - 1 >= right_sibling->GetMinSize()) {
    Redistribute(right_sibling, node, 0);
    return false;
  }
  Coalesce(&right_sibling, &node, &parent, 0, transaction);
  return true;
}

/*
 * Move all the key & value pairs from one page to its sibling page, and notify
 * buffer pool manager to delete this page. Parent page must be adjusted to
 * take info of deletion into account. Remember to deal with coalesce or
 * redistribute recursively if necessary.
 * Using template N to represent either internal page or leaf page.
 * @param   neighbor_node      sibling page of input "node"
 * @param   node               input from method coalesceOrRedistribute()
 * @param   parent             parent page of input "node"
 * @return  true means parent node should be deleted, false means no deletion
 * happend
 */
INDEX_TEMPLATE_ARGUMENTS
template <typename N>
bool BPLUSTREE_TYPE::Coalesce(N **neighbor_node, N **node,
                              BPlusTreeInternalPage<KeyType, page_id_t, KeyComparator> **parent, int index,
                              Transaction *transaction) {
  // always delete right node ?
  KeyType v;
  if (index == 0) {
    (*neighbor_node)->MoveAllTo(*node, v, buffer_pool_manager_);
    index = 1;
    transaction->AddIntoDeletedPageSet((*neighbor_node)->GetPageId());
  } else {
    (*node)->MoveAllTo(*neighbor_node, v, buffer_pool_manager_);
    transaction->AddIntoDeletedPageSet((*node)->GetPageId());
  }
  (*parent)->Remove(index);

  if ((*parent)->GetSize() < (*parent)->GetMinSize()) {
    return CoalesceOrRedistribute(*parent, transaction);
  }
  return false;
}

/*
 * Redistribute key & value pairs from one page to its sibling page. If index ==
 * 0, move sibling page's first key & value pair into end of input "node",
 * otherwise move sibling page's last key & value pair into head of input
 * "node".
 * Using template N to represent either internal page or leaf page.
 * @param   neighbor_node      sibling page of input "node"
 * @param   node               input from method coalesceOrRedistribute()
 */
INDEX_TEMPLATE_ARGUMENTS
template <typename N>
void BPLUSTREE_TYPE::Redistribute(N *neighbor_node, N *node, int index) {
  node->Redistribute(neighbor_node, index, buffer_pool_manager_);
}
/*
 * Update root page if necessary
 * NOTE: size of root page can be less than min size and this method is only
 * called within coalesceOrRedistribute() method
 * case 1: when you delete the last element in root page, but root page still
 * has one last child
 * case 2: when you delete the last element in whole b+ tree
 * @return : true means root page should be deleted, false means no deletion
 * happend
 */
INDEX_TEMPLATE_ARGUMENTS
bool BPLUSTREE_TYPE::AdjustRoot(BPlusTreePage *old_root_node, Transaction *txn) {
  if (old_root_node->IsLeafPage() && old_root_node->GetSize() == 0) {
    // root_page_id_ = INVALID_PAGE_ID;
    // UpdateRootPageId(0);
    // return true;
    return false;
  }
  if (!old_root_node->IsLeafPage() && old_root_node->GetSize() == 1) {
    auto root = reinterpret_cast<InternalType *>(old_root_node);
    page_id_t new_root_id = root->ValueAt(0);
    root_page_id_ = new_root_id;
    auto *new_root_page = fetch_page(txn, root_page_id_);
    UpdateRootPageId(false);

    B_PLUS_TREE_LEAF_PAGE_TYPE *new_root = reinterpret_cast<B_PLUS_TREE_LEAF_PAGE_TYPE *>(new_root_page->GetData());
    new_root->SetParentPageId(INVALID_PAGE_ID);
    new_root->SetNextPageId(INVALID_PAGE_ID);
    return true;
  }

  return false;
}

/*****************************************************************************
 * INDEX ITERATOR
 *****************************************************************************/
/*
 * Input parameter is void, find the leaftmost leaf page first, then construct
 * index iterator
 * @return : index iterator
 */
INDEX_TEMPLATE_ARGUMENTS
INDEXITERATOR_TYPE BPLUSTREE_TYPE::begin() {
  KeyType k;

  Page *page = FindLeafOptimistic(k, Operation::LeftMostRead);

  if (page == nullptr) {
    return end();
  }
  B_PLUS_TREE_LEAF_PAGE_TYPE *leaf = reinterpret_cast<B_PLUS_TREE_LEAF_PAGE_TYPE *>(page->GetData());
  if (leaf->GetSize() == 0) {
    return end();
  }

  return INDEXITERATOR_TYPE(page, 0, buffer_pool_manager_);
}

/*
 * Input parameter is low key, find the leaf page that contains the input key
 * first, then construct index iterator
 * @return : index iterator
 */
INDEX_TEMPLATE_ARGUMENTS
INDEXITERATOR_TYPE BPLUSTREE_TYPE::Begin(const KeyType &key) {
  Page *page = FindLeafOptimistic(key, Operation::Read);
  B_PLUS_TREE_LEAF_PAGE_TYPE *cur = reinterpret_cast<B_PLUS_TREE_LEAF_PAGE_TYPE *>(page->GetData());
  int offset = cur->KeyIndex(key, comparator_);
  if (offset == -1) {
    page->RUnlatch();
    buffer_pool_manager_->UnpinPage(page->GetPageId(), false);
    return INDEXITERATOR_TYPE(nullptr, -1, nullptr);
  }
  return INDEXITERATOR_TYPE(page, offset, buffer_pool_manager_);
}

/*
 * Input parameter is void, construct an index iterator representing the end
 * of the key/value pair in the leaf node
 * @return : index iterator
 */
INDEX_TEMPLATE_ARGUMENTS
INDEXITERATOR_TYPE BPLUSTREE_TYPE::end() { return INDEXITERATOR_TYPE(nullptr, -1, nullptr); }

INDEX_TEMPLATE_ARGUMENTS
Page *BPLUSTREE_TYPE::fetch_page(Transaction *txn, page_id_t page_id) {
  auto page_set = txn->GetPageSet();
  auto iter =
      std::find_if((*page_set).begin(), (*page_set).end(), [page_id](Page *px) { return px->GetPageId() == page_id; });
  if (iter != (*page_set).end()) {
    return *iter;
  }

  Page *page = buffer_pool_manager_->FetchPage(page_id);
  if (page == nullptr) {
    throw std::bad_alloc();
  }
  page->WLatch();
  txn->AddIntoPageSet(page);
  return page;
}

// empty except last lock
INDEX_TEMPLATE_ARGUMENTS
void BPLUSTREE_TYPE::flush_old(Transaction *txn) {
  auto page_set = txn->GetPageSet();
  while (((*page_set).size() > 1)) {
    auto page = (*page_set).front();
    page->WUnlatch();

    (*page_set).pop_front();

    buffer_pool_manager_->UnpinPage(page->GetPageId(), false);
  }
}

INDEX_TEMPLATE_ARGUMENTS
void BPLUSTREE_TYPE::commit_all(Transaction *txn, bool dirty) {
  auto page_set = txn->GetPageSet();

  while (!(*page_set).empty()) {
    auto page = (*page_set).front();
    page->WUnlatch();

    (*page_set).pop_front();

    buffer_pool_manager_->UnpinPage(page->GetPageId(), dirty);
  }
  auto deleted_ids = txn->GetDeletedPageSet();
  for (auto id : (*deleted_ids)) {
    buffer_pool_manager_->DeletePage(id);
  }
}
/*****************************************************************************
 * UTILITIES AND DEBUG
 *****************************************************************************/
/*
 * Find leaf page containing particular key, if leftMost flag == true, find
 * the left most leaf page
 */

INDEX_TEMPLATE_ARGUMENTS
Page *BPLUSTREE_TYPE::FindLeafPage(const KeyType &key, Transaction *txn, Operation operation) {
  Page *leaf_page = FindLeafOptimistic(key, operation);
  if (operation == Operation::Read || operation == Operation::LeftMostRead) {
    return leaf_page;
  }

  auto leaf_btree_page = reinterpret_cast<BPlusTreePage *>(leaf_page->GetData());
  auto leaf_size = leaf_btree_page->GetSize();
  auto max_size = leaf_btree_page->GetMaxSize();
  auto min_size = leaf_btree_page->GetMinSize();

  if ((operation == Operation::Insert && leaf_size < max_size - 1) ||
      (operation == Operation::Remove && leaf_size > min_size + 1)) {
    txn->AddIntoPageSet(leaf_page);
    return leaf_page;
  }
  leaf_page->WUnlatch();
  buffer_pool_manager_->UnpinPage(leaf_page->GetPageId(), false);

  Page *current_page = get_root_page(operation, true);
  txn->AddIntoPageSet(current_page);
  auto temp = reinterpret_cast<BPlusTreePage *>(current_page->GetData());

  while (!temp->IsLeafPage()) {
    InternalType *current = static_cast<InternalType *>(temp);
    page_id_t pid = current->Lookup(key, comparator_);
    current_page = fetch_page(txn, pid);
    temp = reinterpret_cast<BPlusTreePage *>(current_page->GetData());

    if ((operation == Operation::Insert && temp->GetSize() < temp->GetMaxSize() - 1) ||
        (operation == Operation::Remove && temp->GetSize() > temp->GetMinSize() + 1)) {
      flush_old(txn);
    }
  }
  return current_page;
}

INDEX_TEMPLATE_ARGUMENTS
Page *BPLUSTREE_TYPE::get_root_page(Operation operation, bool writeLock) {
  while (true) {
    page_id_t root_id = root_page_id_;

    Page *current_page = buffer_pool_manager_->FetchPage(root_id);
    if (current_page == nullptr) {
      throw std::bad_alloc();
    }
    auto temp = reinterpret_cast<BPlusTreePage *>(current_page->GetData());

    bool leaf = writeLock || ((operation == Operation::Insert || operation == Operation::Remove) && temp->IsLeafPage());

    if (leaf) {
      current_page->WLatch();
    } else {
      current_page->RLatch();
    }
    if (root_id == root_page_id_) {
      return current_page;
    }

    if (leaf) {
      current_page->WUnlatch();
    } else {
      current_page->RUnlatch();
    }
    buffer_pool_manager_->UnpinPage(root_id, false);
  }
}

INDEX_TEMPLATE_ARGUMENTS
Page *BPLUSTREE_TYPE::FindLeafOptimistic(const KeyType &key, Operation operation) {
  Page *page = get_root_page(operation, false);
  auto btree_page = reinterpret_cast<BPlusTreePage *>(page->GetData());

  while (!btree_page->IsLeafPage()) {
    auto *internal_page = static_cast<InternalType *>(btree_page);
    page_id_t pid =
        operation == Operation::LeftMostRead ? internal_page->ValueAt(0) : internal_page->Lookup(key, comparator_);

    Page *child_page = buffer_pool_manager_->FetchPage(pid);
    if (child_page == nullptr) {
      throw std::bad_alloc();
    }
    btree_page = reinterpret_cast<BPlusTreePage *>(child_page->GetData());

    if ((operation == Operation::Insert || operation == Operation::Remove) && btree_page->IsLeafPage()) {
      child_page->WLatch();
    } else {
      child_page->RLatch();
    }
    page->RUnlatch();
    buffer_pool_manager_->UnpinPage(page->GetPageId(), false);
    page = child_page;
  }
  return page;
}

/*
 * Update/Insert root page id in header page(where page_id = 0, header_page is
 * defined under include/page/header_page.h)
 * Call this method everytime root page id is changed.
 * @parameter: insert_record      defualt value is false. When set to true,
 * insert a record <index_name, root_page_id> into header page instead of
 * updating it.
 */
INDEX_TEMPLATE_ARGUMENTS
void BPLUSTREE_TYPE::UpdateRootPageId(int insert_record) {
  HeaderPage *header_page = static_cast<HeaderPage *>(buffer_pool_manager_->FetchPage(HEADER_PAGE_ID));
  if (insert_record != 0) {
    // create a new record<index_name + root_page_id> in header_page
    header_page->InsertRecord(index_name_, root_page_id_);
  } else {
    // update root_page_id in header_page
    header_page->UpdateRecord(index_name_, root_page_id_);
  }
  buffer_pool_manager_->UnpinPage(HEADER_PAGE_ID, true);
}

/*
 * This method is used for test only
 * Read data from file and insert one by one
 */
INDEX_TEMPLATE_ARGUMENTS
void BPLUSTREE_TYPE::InsertFromFile(const std::string &file_name, Transaction *transaction) {
  int64_t key;
  std::ifstream input(file_name);
  while (input) {
    input >> key;

    KeyType index_key;
    index_key.SetFromInteger(key);
    RID rid(key);
    Insert(index_key, rid, transaction);
  }
}
/*
 * This method is used for test only
 * Read data from file and remove one by one
 */
INDEX_TEMPLATE_ARGUMENTS
void BPLUSTREE_TYPE::RemoveFromFile(const std::string &file_name, Transaction *transaction) {
  int64_t key;
  std::ifstream input(file_name);
  while (input) {
    input >> key;
    KeyType index_key;
    index_key.SetFromInteger(key);

    Remove(index_key, transaction);
  }
}

/**
 * This method is used for debug only, You don't  need to modify
 * @tparam KeyType
 * @tparam ValueType
 * @tparam KeyComparator
 * @param page
 * @param bpm
 * @param out
 */
INDEX_TEMPLATE_ARGUMENTS
void BPLUSTREE_TYPE::ToGraph(BPlusTreePage *page, BufferPoolManager *bpm, std::ofstream &out) const {
  std::string leaf_prefix("LEAF_");
  std::string internal_prefix("INT_");
  if (page->IsLeafPage()) {
    LeafPage *leaf = reinterpret_cast<LeafPage *>(page);
    // Print node name
    out << leaf_prefix << leaf->GetPageId();
    // Print node properties
    out << "[shape=plain color=green ";
    // Print data of the node
    out << "label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">\n";
    // Print data
    out << "<TR><TD COLSPAN=\"" << leaf->GetSize() << "\">P=" << leaf->GetPageId() << "</TD></TR>\n";
    out << "<TR><TD COLSPAN=\"" << leaf->GetSize() << "\">"
        << "max_size=" << leaf->GetMaxSize() << ",min_size=" << leaf->GetMinSize() << "</TD></TR>\n";
    out << "<TR>";
    for (int i = 0; i < leaf->GetSize(); i++) {
      out << "<TD>" << leaf->KeyAt(i) << "</TD>\n";
    }
    out << "</TR>";
    // Print table end
    out << "</TABLE>>];\n";
    // Print Leaf node link if there is a next page
    if (leaf->GetNextPageId() != INVALID_PAGE_ID) {
      out << leaf_prefix << leaf->GetPageId() << " -> " << leaf_prefix << leaf->GetNextPageId() << ";\n";
      out << "{rank=same " << leaf_prefix << leaf->GetPageId() << " " << leaf_prefix << leaf->GetNextPageId() << "};\n";
    }

    // Print parent links if there is a parent
    if (leaf->GetParentPageId() != INVALID_PAGE_ID) {
      out << internal_prefix << leaf->GetParentPageId() << ":p" << leaf->GetPageId() << " -> " << leaf_prefix
          << leaf->GetPageId() << ";\n";
    }
  } else {
    InternalPage *inner = reinterpret_cast<InternalPage *>(page);
    // Print node name
    out << internal_prefix << inner->GetPageId();
    // Print node properties
    out << "[shape=plain color=pink ";  // why not?
    // Print data of the node
    out << "label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">\n";
    // Print data
    out << "<TR><TD COLSPAN=\"" << inner->GetSize() << "\">P=" << inner->GetPageId() << "</TD></TR>\n";
    out << "<TR><TD COLSPAN=\"" << inner->GetSize() << "\">"
        << "max_size=" << inner->GetMaxSize() << ",min_size=" << inner->GetMinSize() << "</TD></TR>\n";
    out << "<TR>";
    for (int i = 0; i < inner->GetSize(); i++) {
      out << "<TD PORT=\"p" << inner->ValueAt(i) << "\">";
      if (i > 0) {
        out << inner->KeyAt(i);
      } else {
        out << " ";
      }
      out << "</TD>\n";
    }
    out << "</TR>";
    // Print table end
    out << "</TABLE>>];\n";
    // Print Parent link
    if (inner->GetParentPageId() != INVALID_PAGE_ID) {
      out << internal_prefix << inner->GetParentPageId() << ":p" << inner->GetPageId() << " -> " << internal_prefix
          << inner->GetPageId() << ";\n";
    }
    // Print leaves
    for (int i = 0; i < inner->GetSize(); i++) {
      auto child_page = reinterpret_cast<BPlusTreePage *>(bpm->FetchPage(inner->ValueAt(i))->GetData());
      ToGraph(child_page, bpm, out);
      if (i > 0) {
        auto sibling_page = reinterpret_cast<BPlusTreePage *>(bpm->FetchPage(inner->ValueAt(i - 1))->GetData());
        if (!sibling_page->IsLeafPage() && !child_page->IsLeafPage()) {
          out << "{rank=same " << internal_prefix << sibling_page->GetPageId() << " " << internal_prefix
              << child_page->GetPageId() << "};\n";
        }
        bpm->UnpinPage(sibling_page->GetPageId(), false);
      }
    }
  }
  bpm->UnpinPage(page->GetPageId(), false);
}

/**
 * This function is for debug only, you don't need to modify
 * @tparam KeyType
 * @tparam ValueType
 * @tparam KeyComparator
 * @param page
 * @param bpm
 */
INDEX_TEMPLATE_ARGUMENTS
void BPLUSTREE_TYPE::ToString(BPlusTreePage *page, BufferPoolManager *bpm) const {
  if (page->IsLeafPage()) {
    LeafPage *leaf = reinterpret_cast<LeafPage *>(page);
    std::cout << "Leaf Page: " << leaf->GetPageId() << " parent: " << leaf->GetParentPageId()
              << " next: " << leaf->GetNextPageId() << std::endl;
    for (int i = 0; i < leaf->GetSize(); i++) {
      std::cout << leaf->KeyAt(i) << ",";
    }
    std::cout << std::endl;
    std::cout << std::endl;
  } else {
    InternalPage *internal = reinterpret_cast<InternalPage *>(page);
    std::cout << "Internal Page: " << internal->GetPageId() << " parent: " << internal->GetParentPageId() << std::endl;
    for (int i = 0; i < internal->GetSize(); i++) {
      std::cout << internal->KeyAt(i) << ": " << internal->ValueAt(i) << ",";
    }
    std::cout << std::endl;
    std::cout << std::endl;
    for (int i = 0; i < internal->GetSize(); i++) {
      ToString(reinterpret_cast<BPlusTreePage *>(bpm->FetchPage(internal->ValueAt(i))->GetData()), bpm);
    }
  }
  bpm->UnpinPage(page->GetPageId(), false);
}

template class BPlusTree<GenericKey<4>, RID, GenericComparator<4>>;
template class BPlusTree<GenericKey<8>, RID, GenericComparator<8>>;
template class BPlusTree<GenericKey<16>, RID, GenericComparator<16>>;
template class BPlusTree<GenericKey<32>, RID, GenericComparator<32>>;
template class BPlusTree<GenericKey<64>, RID, GenericComparator<64>>;

}  // namespace bustub
