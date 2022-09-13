//===----------------------------------------------------------------------===//
//
//                         BusTub
//
// seq_scan_executor.cpp
//
// Identification: src/execution/seq_scan_executor.cpp
//
// Copyright (c) 2015-19, Carnegie Mellon University Database Group
//
//===----------------------------------------------------------------------===//
#include "execution/executors/seq_scan_executor.h"

namespace bustub {

SeqScanExecutor::SeqScanExecutor(ExecutorContext *exec_ctx, const SeqScanPlanNode *plan)
    : AbstractExecutor(exec_ctx), plan_(plan) {}

void SeqScanExecutor::Init() {}

Tuple Projection(const Tuple *tuple, const Schema &schema, const Schema *out_schema) {
  std::vector<Value> values;
  auto cols = out_schema->GetColumns();
  values.reserve(cols.size());
  for (auto &col : cols) {
    values.emplace_back(col.GetExpr()->Evaluate(tuple, &schema));
  }
  return Tuple(values, out_schema);
}

bool SeqScanExecutor::Next(Tuple *tuple, RID *rid) {
  auto catalog = GetExecutorContext()->GetCatalog();
  auto metadata = catalog->GetTable(plan_->GetTableOid());
  auto heap = metadata->table_.get();
  auto txn = exec_ctx_->GetTransaction();
  auto lock_manager = exec_ctx_->GetLockManager();

  if (!started) {
    started = true;
    iterator = new TableIterator(heap->Begin(exec_ctx_->GetTransaction()));
  }
  while ((*iterator) != heap->End()) {
    RID rid1 = (*iterator)->GetRid();
    if (txn->GetIsolationLevel() != IsolationLevel::READ_UNCOMMITTED) {
      lock_manager->LockShared(txn, rid1);
    }

    auto predicate = plan_->GetPredicate();
    if (predicate == nullptr || plan_->GetPredicate()->Evaluate(&**iterator, &metadata->schema_).GetAs<bool>()) {
      *rid = rid1;
      *tuple = Projection(&**iterator, metadata->schema_, GetOutputSchema());
      (*iterator)++;
      if (txn->GetIsolationLevel() == IsolationLevel::READ_COMMITTED) {
        lock_manager->Unlock(txn, rid1);
      }
      return true;
    }
    if (txn->GetIsolationLevel() == IsolationLevel::READ_COMMITTED) {
      lock_manager->Unlock(txn, rid1);
    }
    (*iterator)++;
  }

  return false;
}

}  // namespace bustub
