//===----------------------------------------------------------------------===//
//
//                         BusTub
//
// lock_manager.cpp
//
// Identification: src/concurrency/lock_manager.cpp
//
// Copyright (c) 2015-2019, Carnegie Mellon University Database Group
//
//===----------------------------------------------------------------------===//

#include "concurrency/lock_manager.h"
#include <set>

#include <unordered_map>
#include <utility>
#include <vector>
#include "concurrency/transaction.h"
#include "concurrency/transaction_manager.h"

namespace bustub {
// auto &req = queue.request_queue_.back();
// while (std::find_if(queue.request_queue_.begin(), queue.request_queue_.end(), [](LockRequest req) {
//         return req.granted_ && req.lock_mode_ == LockMode::EXCLUSIVE;
//       }) != queue.request_queue_.end()) {
//  queue.cv_.wait(unique);
//}

void LockManager::Process_Abort(Transaction *txn) {
  txn_id_t tid = txn->GetTransactionId();
  blocked_on.erase(tid);
  if (aborted_txns.find(tid) != aborted_txns.end()) {
    LOG_INFO("ABORTED %d", tid);

    aborted_txns.erase(tid);
    txn->SetState(TransactionState::ABORTED);
    throw TransactionAbortException(tid, AbortReason::DEADLOCK);
  }
}

bool LockManager::LockShared(Transaction *txn, const RID &rid) {
  if (txn->GetState() == TransactionState::SHRINKING || txn->GetIsolationLevel() == IsolationLevel::READ_UNCOMMITTED) {
    txn->SetState(TransactionState::ABORTED);
    throw TransactionAbortException(txn->GetTransactionId(), AbortReason::LOCK_ON_SHRINKING);
  }
  if (txn->IsSharedLocked(rid) || txn->IsExclusiveLocked(rid)) {
    return true;
  }

  std::unique_lock<std::mutex> unique(latch_);
  txn_id_t tid = txn->GetTransactionId();
  LOG_INFO("Try Shared Lock on %d %d ", tid, rid.GetSlotNum());

  auto &queue = lock_table_[rid];
  while (queue.upgrading_ ||
         (!queue.request_queue_.empty() && queue.request_queue_.front().lock_mode_ == LockMode::EXCLUSIVE)) {
    blocked_on.insert({tid, rid});
    LOG_INFO("add edge %d %d", tid, queue.request_queue_.front().txn_id_);

    AddEdge(tid, queue.request_queue_.front().txn_id_);
    queue.cv_.wait(unique);
    LOG_INFO("Wake up Shared Lock on %d %d ", tid, rid.GetSlotNum());

    Process_Abort(txn);
  }
  LOG_INFO("Shared Lock on %d %d ", tid, rid.GetSlotNum());

  txn->GetSharedLockSet()->emplace(rid);

  queue.request_queue_.emplace_back(LockRequest{txn->GetTransactionId(), LockMode::SHARED, true});
  return true;
}

bool LockManager::LockExclusive(Transaction *txn, const RID &rid) {
  if (txn->GetState() == TransactionState::SHRINKING || txn->GetIsolationLevel() == IsolationLevel::READ_UNCOMMITTED) {
    txn->SetState(TransactionState::ABORTED);
    throw TransactionAbortException(txn->GetTransactionId(), AbortReason::LOCK_ON_SHRINKING);
  }
  if (txn->IsExclusiveLocked(rid)) {
    return true;
  }

  std::unique_lock<std::mutex> unique(latch_);
  auto &queue = lock_table_[rid];
  txn_id_t tid = txn->GetTransactionId();

  while (!queue.request_queue_.empty()) {
    blocked_on.insert({tid, rid});
    LOG_INFO("blocked on %d %d ", tid, rid.GetSlotNum());
    for (auto &entry : queue.request_queue_) {
      LOG_INFO("add edge %d %d", tid, entry.txn_id_);

      AddEdge(tid, entry.txn_id_);
    }

    queue.cv_.wait(unique);
    Process_Abort(txn);
  }
  LOG_INFO("Exclusive Lock on %d %d ", tid, rid.GetSlotNum());
  txn->GetExclusiveLockSet()->emplace(rid);

  queue.request_queue_.emplace_back(LockRequest{txn->GetTransactionId(), LockMode::EXCLUSIVE, true});
  return true;
}

bool LockManager::LockUpgrade(Transaction *txn, const RID &rid) {
  if (txn->GetState() == TransactionState::SHRINKING) {
    txn->SetState(TransactionState::ABORTED);
    throw TransactionAbortException(txn->GetTransactionId(), AbortReason::LOCK_ON_SHRINKING);
  }
  if (txn->IsExclusiveLocked(rid)) {
    return true;
  }
  std::unique_lock<std::mutex> unique(latch_);
  auto &queue = lock_table_[rid];
  queue.upgrading_ = true;
  txn_id_t tid = txn->GetTransactionId();
  IsolationLevel level = txn->GetIsolationLevel();

  // only i left -- no shared is accepted when upgrading is true
  while (true) {
    if (level == IsolationLevel::REPEATABLE_READ && queue.request_queue_.size() == 1 &&
        queue.request_queue_.front().txn_id_ == tid) {
      break;
    }
    if (level == IsolationLevel::READ_COMMITTED && queue.request_queue_.empty()) {
      break;
    }
    blocked_on.insert({tid, rid});
    LOG_INFO("blocked on %d %d ", tid, rid.GetSlotNum());
    for (auto &entry : queue.request_queue_) {
      if (entry.txn_id_ != tid) {
        LOG_INFO("add edge %d %d", tid, entry.txn_id_);
        AddEdge(tid, entry.txn_id_);
      }
    }
    queue.cv_.wait(unique);
    Process_Abort(txn);
  }
  LOG_INFO("Upgrade Lock on %d %d ", tid, rid.GetSlotNum());

  txn->GetSharedLockSet()->erase(rid);
  txn->GetExclusiveLockSet()->emplace(rid);

  queue.upgrading_ = false;
  queue.request_queue_.front().lock_mode_ = LockMode::EXCLUSIVE;

  return true;
}

bool LockManager::Unlock(Transaction *txn, const RID &rid) {
  txn->GetSharedLockSet()->erase(rid);
  txn->GetExclusiveLockSet()->erase(rid);
  if (txn->GetState() == TransactionState::GROWING && txn->GetIsolationLevel() == IsolationLevel::REPEATABLE_READ) {
    txn->SetState(TransactionState::SHRINKING);
  }
  std::unique_lock<std::mutex> unique(latch_);
  auto &queue = lock_table_[rid];
  auto iterator = std::find_if(queue.request_queue_.begin(), queue.request_queue_.end(), [txn](LockRequest req) {
    return req.granted_ && req.txn_id_ == txn->GetTransactionId();
  });
  if (iterator != queue.request_queue_.end()) {
    LOG_INFO("erased");
    queue.request_queue_.erase(iterator);
  }
  for (auto &entry : blocked_on) {
    if (entry.second == rid) {
      RemoveEdge(entry.first, txn->GetTransactionId());
    }
  }
  LOG_INFO("UNLOCK %d %d", txn->GetTransactionId(), rid.GetSlotNum());
  queue.cv_.notify_all();
  return true;
}

void LockManager::AddEdge(txn_id_t t1, txn_id_t t2) { waits_for_[t1].emplace_back(t2); }

void LockManager::RemoveEdge(txn_id_t t1, txn_id_t t2) {
  auto &v = waits_for_[t1];
  std::vector<int>::iterator position = std::find(v.begin(), v.end(), t2);
  if (position != v.end()) {
    v.erase(position);
  }
  if (waits_for_[t1].empty()) {
    waits_for_.erase(t1);
  }
}

bool LockManager::HasCycle(txn_id_t *txn_id) {
  std::vector<txn_id_t> front;
  std::vector<txn_id_t> stack;
  if (waits_for_.empty()) {
    return false;
  }
  std::set<txn_id_t> txn_ids;
  for (auto &entry : waits_for_) {
    txn_ids.insert(entry.first);
    for (auto &id : entry.second) {
      txn_ids.insert(id);
    }
  }
  // for (auto &entry : waits_for_) {
  //  LOG_INFO("from %d", entry.first);
  //  for (auto &id : entry.second) {
  //    LOG_INFO("  to %d", id);
  //  }
  //}
  while (!txn_ids.empty()) {
    front.clear();
    stack.clear();
    front.push_back(*txn_ids.begin());
    while (!front.empty()) {
      txn_id_t current = front.back();
      front.pop_back();
      if (current == -1) {
        stack.pop_back();
        continue;
      }

      if (txn_ids.find(current) == txn_ids.end()) {
        continue;
      }
      txn_ids.erase(current);
      stack.push_back(current);
      front.push_back(-1);
      auto &vs = waits_for_[current];
      for (auto tix = vs.rbegin(); tix != vs.rend(); ++tix) {
        if (std::find(stack.begin(), stack.end(), *tix) != stack.end()) {
          *txn_id = *std::max_element(stack.begin(), stack.end());
          for (auto a : stack) {
            LOG_INFO("cycle %d", a);
          }

          return true;
        }
        front.push_back(*tix);
      }
    }
  }
  return false;
}

std::vector<std::pair<txn_id_t, txn_id_t>> LockManager::GetEdgeList() {
  std::vector<std::pair<txn_id_t, txn_id_t>> edges;
  for (auto &from : waits_for_) {
    for (auto &to : from.second) {
      edges.emplace_back(from.first, to);
    }
  }
  return edges;
}

void LockManager::RunCycleDetection() {
  while (enable_cycle_detection_) {
    std::this_thread::sleep_for(cycle_detection_interval);
    {
      std::unique_lock<std::mutex> l(latch_);
      txn_id_t tid;
      if (HasCycle(&tid)) {
        aborted_txns.insert(tid);
        RID rid = blocked_on[tid];
        lock_table_[rid].cv_.notify_all();
      }

      continue;
    }
  }
}

}  // namespace bustub
