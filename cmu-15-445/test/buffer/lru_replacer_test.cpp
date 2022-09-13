//===----------------------------------------------------------------------===//
//
//                         BusTub
//
// lru_replacer_test.cpp
//
// Identification: test/buffer/lru_replacer_test.cpp
//
// Copyright (c) 2015-2019, Carnegie Mellon University Database Group
//
//===----------------------------------------------------------------------===//

#include <cstdio>
#include <thread>  // NOLINT
#include <vector>

#include "buffer/lru_replacer.h"
#include "common/logger.h"
#include "gtest/gtest.h"
namespace bustub {

void run(int start, LRUReplacer *replacer) {
  for (int i = start; i < start + 10000; i++) {
    replacer->Unpin(i);
  }
  int value;

  for (int i = start; i < start + 10000; i++) {
    EXPECT_EQ(true, replacer->Victim(&value));
  }
}

TEST(LRUReplacerTest, ConcurrentSampleTest) {
  LRUReplacer lru_replacer(200000);

  std::vector<std::thread> threads(20);
  for (int i = 0; i < 20; i++) {
    threads[i] = (std::thread(run, i * 10000, &(lru_replacer)));
  }
  for (int i = 0; i < 20; i++) {
    threads[i].join();
  }
  EXPECT_EQ(0, lru_replacer.Size());
}

TEST(LRUReplacerTest, SampleTest) {
  LRUReplacer lru_replacer(7);

  // Scenario: unpin six elements, i.e. add them to the replacer.
  lru_replacer.Unpin(1);
  lru_replacer.Unpin(2);
  lru_replacer.Unpin(3);
  lru_replacer.Unpin(4);
  lru_replacer.Unpin(5);
  lru_replacer.Unpin(6);
  lru_replacer.Unpin(1);
  EXPECT_EQ(6, lru_replacer.Size());

  // Scenario: get three victims from the lru.
  int value;
  lru_replacer.Victim(&value);
  EXPECT_EQ(1, value);
  lru_replacer.Victim(&value);
  EXPECT_EQ(2, value);
  lru_replacer.Victim(&value);
  EXPECT_EQ(3, value);

  // Scenario: pin elements in the replacer.
  // Note that 3 has already been victimized, so pinning 3 should have no effect.
  lru_replacer.Pin(3);
  lru_replacer.Pin(4);
  EXPECT_EQ(2, lru_replacer.Size());

  // Scenario: unpin 4. We expect that the reference bit of 4 will be set to 1.
  lru_replacer.Unpin(4);

  // Scenario: continue looking for victims. We expect these victims.
  lru_replacer.Victim(&value);
  EXPECT_EQ(5, value);
  lru_replacer.Victim(&value);
  EXPECT_EQ(6, value);
  lru_replacer.Victim(&value);
  EXPECT_EQ(4, value);
}
TEST(LRUReplacerTest, SampleTest2) {
  LRUReplacer lru_replacer(7);

  // Scenario: unpin six elements, i.e. add them to the replacer.
  lru_replacer.Unpin(0);
  lru_replacer.Unpin(1);
  lru_replacer.Unpin(2);
  lru_replacer.Unpin(3);
  lru_replacer.Unpin(4);
  lru_replacer.Unpin(5);
  lru_replacer.Unpin(6);
  lru_replacer.Pin(1);

  // Scenario: get three victims from the lru.
  int value;
  lru_replacer.Victim(&value);
  EXPECT_EQ(0, value);
}
}  // namespace bustub
