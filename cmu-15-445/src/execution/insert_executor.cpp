//===----------------------------------------------------------------------===//
//
//                         BusTub
//
// insert_executor.cpp
//
// Identification: src/execution/insert_executor.cpp
//
// Copyright (c) 2015-19, Carnegie Mellon University Database Group
//
//===----------------------------------------------------------------------===//
#include <memory>

#include "execution/executors/insert_executor.h"

namespace bustub {

InsertExecutor::InsertExecutor(ExecutorContext *exec_ctx, const InsertPlanNode *plan,
                               std::unique_ptr<AbstractExecutor> &&child_executor)
    : AbstractExecutor(exec_ctx), plan_(plan), child_executor_(std::move(child_executor)) {}

void InsertExecutor::Init() {
  if (!plan_->IsRawInsert()) {
    child_executor_->Init();
  }
  auto catalog = GetExecutorContext()->GetCatalog();
  auto metadata = catalog->GetTable(plan_->TableOid());
  indexes = catalog->GetTableIndexes(metadata->name_);
}

bool InsertExecutor::Next([[maybe_unused]] Tuple *tuple, RID *rid) {
  auto catalog = GetExecutorContext()->GetCatalog();
  auto metadata = catalog->GetTable(plan_->TableOid());
  auto heap = metadata->table_.get();
  auto txn = exec_ctx_->GetTransaction();
  auto lock_manager = exec_ctx_->GetLockManager();

  while (true) {
    if (plan_->IsRawInsert() && plan_->RawValues().size() <= index) {
      return false;
    }

    if (plan_->IsRawInsert()) {
      *tuple = Tuple{plan_->RawValuesAt(index), &metadata->schema_};
      index++;
    } else if (!child_executor_->Next(tuple, rid)) {
      return false;
    }

    if (heap->InsertTuple(*tuple, rid, txn)) {
      if (txn->GetIsolationLevel() != IsolationLevel::READ_UNCOMMITTED) {
        lock_manager->LockExclusive(txn, *rid);
      }

      for (auto index_info = indexes.begin(); index_info < indexes.end(); index_info++) {
        auto key =
            tuple->KeyFromTuple(metadata->schema_, (*index_info)->key_schema_, (*index_info)->index_->GetKeyAttrs());
        txn->AppendTableWriteRecord(
            IndexWriteRecord{*rid, plan_->TableOid(), WType::INSERT, key, (*index_info)->index_oid_, catalog});

        (*index_info)->index_->InsertEntry(key, *rid, txn);
      }
    }
  }
  return false;
}

}  // namespace bustub
