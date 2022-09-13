/**
 * b_plus_tree_insert_test.cpp
 */

#include <algorithm>
#include <cstdio>

#include "b_plus_tree_test_util.h"  // NOLINT
#include "buffer/buffer_pool_manager.h"
#include "gtest/gtest.h"
#include "storage/index/b_plus_tree.h"
#define TEST_TIMEOUT_BEGIN                           \
  std::promise<bool> promisedFinished;               \
  auto futureResult = promisedFinished.get_future(); \
                              std::thread([](std::promise<bool>& finished) {
#define TEST_TIMEOUT_FAIL_END(X)                                                                  \
  finished.set_value(true);                                                                       \
  }, std::ref(promisedFinished)).detach();                                                        \
  EXPECT_TRUE(futureResult.wait_for(std::chrono::milliseconds(X)) != std::future_status::timeout) \
      << "Test Failed Due to Time Out";
namespace bustub {

TEST(BPlusTreeTests, InsertTest1) {
  // create KeyComparator and index schema
  Schema *key_schema = ParseCreateStatement("a bigint");
  GenericComparator<8> comparator(key_schema);

  DiskManager *disk_manager = new DiskManager("test.db");
  BufferPoolManager *bpm = new BufferPoolManager(50, disk_manager);
  // create b+ tree
  BPlusTree<GenericKey<8>, RID, GenericComparator<8>> tree("foo_pk", bpm, comparator, 2, 3);
  GenericKey<8> index_key;
  RID rid;
  // create transaction
  Transaction *transaction = new Transaction(0);

  // create and fetch header_page
  page_id_t page_id;
  auto header_page = bpm->NewPage(&page_id);
  (void)header_page;

  std::vector<int64_t> keys = {1, 2, 3, 4, 5};
  for (auto key : keys) {
    int64_t value = key & 0xFFFFFFFF;
    rid.Set(static_cast<int32_t>(key >> 32), value);
    index_key.SetFromInteger(key);
    tree.Insert(index_key, rid, transaction);
  }

  std::vector<RID> rids;
  for (auto key : keys) {
    rids.clear();
    index_key.SetFromInteger(key);
    tree.GetValue(index_key, &rids);
    EXPECT_EQ(rids.size(), 1);

    int64_t value = key & 0xFFFFFFFF;
    EXPECT_EQ(rids[0].GetSlotNum(), value);
  }

  int64_t start_key = 1;
  int64_t current_key = start_key;
  index_key.SetFromInteger(start_key);
  for (auto iterator = tree.Begin(index_key); iterator != tree.end(); ++iterator) {
    auto location = (*iterator).second;
    EXPECT_EQ(location.GetPageId(), 0);
    EXPECT_EQ(location.GetSlotNum(), current_key);
    current_key = current_key + 1;
  }

  EXPECT_EQ(current_key, keys.size() + 1);

  bpm->UnpinPage(HEADER_PAGE_ID, true);
  delete key_schema;
  delete transaction;
  delete disk_manager;
  delete bpm;
  remove("test.db");
  remove("test.log");
}

TEST(BPlusTreeTests, InsertTest2) {
  // create KeyComparator and index schema
  Schema *key_schema = ParseCreateStatement("a bigint");
  GenericComparator<8> comparator(key_schema);

  DiskManager *disk_manager = new DiskManager("test.db");
  BufferPoolManager *bpm = new BufferPoolManager(50, disk_manager);
  // create b+ tree
  BPlusTree<GenericKey<8>, RID, GenericComparator<8>> tree("foo_pk", bpm, comparator);
  GenericKey<8> index_key;
  RID rid;
  // create transaction
  Transaction *transaction = new Transaction(0);

  // create and fetch header_page
  page_id_t page_id;
  auto header_page = bpm->NewPage(&page_id);
  (void)header_page;

  std::vector<int64_t> keys = {5, 4, 3, 2, 1};
  for (auto key : keys) {
    int64_t value = key & 0xFFFFFFFF;
    rid.Set(static_cast<int32_t>(key >> 32), value);
    index_key.SetFromInteger(key);
    tree.Insert(index_key, rid, transaction);
  }

  std::vector<RID> rids;
  for (auto key : keys) {
    rids.clear();
    index_key.SetFromInteger(key);
    tree.GetValue(index_key, &rids);
    EXPECT_EQ(rids.size(), 1);

    int64_t value = key & 0xFFFFFFFF;
    EXPECT_EQ(rids[0].GetSlotNum(), value);
  }

  int64_t start_key = 1;
  int64_t current_key = start_key;
  index_key.SetFromInteger(start_key);
  for (auto iterator = tree.Begin(index_key); iterator != tree.end(); ++iterator) {
    auto location = (*iterator).second;
    // LOG_INFO("%d %d", location.GetPageId(), location.GetSlotNum());
    EXPECT_EQ(location.GetPageId(), 0);
    EXPECT_EQ(location.GetSlotNum(), current_key);
    current_key = current_key + 1;
  }

  EXPECT_EQ(current_key, keys.size() + 1);

  start_key = 3;
  current_key = start_key;
  index_key.SetFromInteger(start_key);
  auto iterator = tree.Begin(index_key);
  for (; iterator != tree.end(); ++iterator) {
    auto location = (*iterator).second;
    LOG_INFO("%d %d", location.GetPageId(), location.GetSlotNum());
    EXPECT_EQ(location.GetPageId(), 0);
    EXPECT_EQ(location.GetSlotNum(), current_key);
    current_key = current_key + 1;
  }

  bpm->UnpinPage(HEADER_PAGE_ID, true);
  delete key_schema;
  delete transaction;
  delete disk_manager;
  delete bpm;
  remove("test.db");
  remove("test.log");
}

template <typename... Args>
void LaunchParallelTest(uint64_t num_threads, uint64_t txn_id_start, Args &&... args) {
  std::vector<std::thread> thread_group;

  // Launch a group of threads
  for (uint64_t thread_itr = 0; thread_itr < num_threads; ++thread_itr) {
    thread_group.push_back(std::thread(args..., txn_id_start + thread_itr, thread_itr));
  }

  // Join the threads with the main thread
  for (uint64_t thread_itr = 0; thread_itr < num_threads; ++thread_itr) {
    thread_group[thread_itr].join();
  }
}

// helper function to insert
void InsertHelper(BPlusTree<GenericKey<8>, RID, GenericComparator<8>> *tree, const std::vector<int64_t> &keys,
                  uint64_t tid, __attribute__((unused)) uint64_t thread_itr = 0) {
  GenericKey<8> index_key;
  RID rid;
  // create transaction
  Transaction *transaction = new Transaction(tid);
  for (auto key : keys) {
    int64_t value = key & 0xFFFFFFFF;
    rid.Set(static_cast<int32_t>(key >> 32), value);
    index_key.SetFromInteger(key);
    tree->Insert(index_key, rid, transaction);
  }
  delete transaction;
}

// helper function to seperate insert
void InsertHelperSplit(BPlusTree<GenericKey<8>, RID, GenericComparator<8>> *tree, const std::vector<int64_t> &keys,
                       int total_threads, uint64_t tid, __attribute__((unused)) uint64_t thread_itr) {
  GenericKey<8> index_key;
  RID rid;
  // create transaction
  Transaction *transaction = new Transaction(tid);
  for (auto key : keys) {
    if (static_cast<uint64_t>(key) % total_threads == thread_itr) {
      int64_t value = key & 0xFFFFFFFF;
      rid.Set(static_cast<int32_t>(key >> 32), value);
      index_key.SetFromInteger(key);
      tree->Insert(index_key, rid, transaction);
    }
  }
  delete transaction;
}

// helper function to delete
void DeleteHelper(BPlusTree<GenericKey<8>, RID, GenericComparator<8>> *tree, const std::vector<int64_t> &remove_keys,
                  uint64_t tid, __attribute__((unused)) uint64_t thread_itr = 0) {
  GenericKey<8> index_key;
  // create transaction
  Transaction *transaction = new Transaction(tid);
  for (auto key : remove_keys) {
    index_key.SetFromInteger(key);
    tree->Remove(index_key, transaction);
  }
  delete transaction;
}

// helper function to seperate delete
void DeleteHelperSplit(BPlusTree<GenericKey<8>, RID, GenericComparator<8>> *tree,
                       const std::vector<int64_t> &remove_keys, int total_threads, uint64_t tid,
                       __attribute__((unused)) uint64_t thread_itr) {
  GenericKey<8> index_key;
  // create transaction
  Transaction *transaction = new Transaction(tid);
  for (auto key : remove_keys) {
    if (static_cast<uint64_t>(key) % total_threads == thread_itr) {
      index_key.SetFromInteger(key);
      tree->Remove(index_key, transaction);
    }
  }
  delete transaction;
}

void LookupHelper(BPlusTree<GenericKey<8>, RID, GenericComparator<8>> *tree, const std::vector<int64_t> &keys,
                  uint64_t tid, __attribute__((unused)) uint64_t thread_itr = 0) {
  Transaction *transaction = new Transaction(tid);
  GenericKey<8> index_key;
  RID rid;
  for (auto key : keys) {
    int64_t value = key & 0xFFFFFFFF;
    rid.Set(static_cast<int32_t>(key >> 32), value);
    index_key.SetFromInteger(key);
    std::vector<RID> result;
    bool res = tree->GetValue(index_key, &result, transaction);
    EXPECT_EQ(res, true);
    EXPECT_EQ(result.size(), 1);
    EXPECT_EQ(result[0], rid);
  }
  delete transaction;
}

// number of iterations that the benchmark test runs
const size_t NUM_ITERS = 20;

void BPlusTreeBenchmarkCall() {
  size_t num_threads = 4;
  // our slow implementation takes about 50s for running 20 iterations
  // Hence allow 5 times slower
  const std::chrono::minutes timeout(5);
  std::chrono::milliseconds time_total(0);
  // bool success = true;
  std::stringstream ss;
  // construct keys to insert, delete and remain
  std::vector<int64_t> insert_keys;
  std::vector<int64_t> delete_keys;
  std::vector<int64_t> remain_keys;

  size_t total_keys = 10000;
  size_t sieve = 2;
  for (size_t i = 1; i <= total_keys; i++) {
    insert_keys.push_back(i);
    if (i % sieve == 0) {
      delete_keys.push_back(i);
    } else {
      remain_keys.push_back(i);
    }
  }
  ss << "[BENCHMARK: BPlusTreeTest.BPlusTreeBenchmark] ";

  for (size_t iter = 0; iter < NUM_ITERS; iter++) {
    // If benchmark is running too long, timeout
    if (time_total > timeout) {
      ss << "TIMEOUT";
      std::cout << ss.str() << std::endl;
      return;
    }

    // create KeyComparator and index schema
    Schema *key_schema = ParseCreateStatement("a bigint");
    GenericComparator<8> comparator(key_schema);
    DiskManager *disk_manager = new DiskManager("test.db");
    BufferPoolManager *bpm = new BufferPoolManager(50, disk_manager);
    // create b+ tree
    BPlusTree<GenericKey<8>, RID, GenericComparator<8>> tree("foo_pk", bpm, comparator);
    // create and fetch header_page
    page_id_t page_id;
    auto header_page = bpm->NewPage(&page_id);
    (void)header_page;

    // start counting time for insertion, deletion and point look up
    auto start = std::chrono::high_resolution_clock::now();

    size_t txn_start_id = 0;
    // insert all the keys
    LaunchParallelTest(num_threads, txn_start_id, InsertHelperSplit, &tree, insert_keys, num_threads);
    txn_start_id += num_threads;
    // delete all even keys
    LaunchParallelTest(num_threads, txn_start_id, DeleteHelperSplit, &tree, delete_keys, num_threads);
    txn_start_id += num_threads;
    // lookup all odd keys
    LookupHelper(&tree, remain_keys, txn_start_id);
    // iterate through all the keys in BPlusTree
    size_t size = 0;
    for (auto &pair : tree) {
      if ((pair.first).ToString() % sieve == 1) {
        size++;
      } else {
        // success = false;
        break;
      }
    }
    // Get End time and add to running total
    auto end = std::chrono::high_resolution_clock::now();
    time_total += std::chrono::duration_cast<std::chrono::milliseconds>(end - start);

    bpm->UnpinPage(HEADER_PAGE_ID, true);
    delete key_schema;
    delete disk_manager;
    delete bpm;
    remove("test.db");
    remove("test.log");
  }
  // if (success) {
  //   ss << (time_total.count() / static_cast<double>(NUM_ITERS));
  // } else {
  //   ss << "FAIL";
  // }
  // std::cout << ss.str() << std::endl;
}

/*
 * Score: 0
 * Description: Benchmark that first insert 3000 keys using
 * num_thread of threads. After insertion is done, using the
 * same number of threads to delete all the even keys. Check
 * each key twice using GetValue and iterator
 *
 */
TEST(BPlusTreeTest, BPlusTreeBenchmark) {
  TEST_TIMEOUT_BEGIN
  BPlusTreeBenchmarkCall();
  remove("test.db");
  remove("test.log");
  TEST_TIMEOUT_FAIL_END(1000 * 300)
}
}  // namespace bustub
