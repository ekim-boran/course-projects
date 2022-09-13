//===----------------------------------------------------------------------===//
//
//                         BusTub
//
// update_executor.cpp
//
// Identification: src/execution/update_executor.cpp
//
// Copyright (c) 2015-20, Carnegie Mellon University Database Group
//
//===----------------------------------------------------------------------===//
#include <memory>

#include "execution/executors/update_executor.h"

namespace bustub {

UpdateExecutor::UpdateExecutor(ExecutorContext *exec_ctx, const UpdatePlanNode *plan,
                               std::unique_ptr<AbstractExecutor> &&child_executor)
    : AbstractExecutor(exec_ctx), plan_(plan), child_executor_(std::move(child_executor)) {
  auto catalog = GetExecutorContext()->GetCatalog();
  auto metadata = catalog->GetTable(plan_->TableOid());

  table_info_ = metadata;
}

void UpdateExecutor::Init() {
  child_executor_->Init();
  auto catalog = GetExecutorContext()->GetCatalog();
  auto metadata = catalog->GetTable(plan_->TableOid());
  indexes = catalog->GetTableIndexes(metadata->name_);
}

bool UpdateExecutor::Next([[maybe_unused]] Tuple *tuple, RID *rid) {
  auto catalog = GetExecutorContext()->GetCatalog();
  auto metadata = catalog->GetTable(plan_->TableOid());
  auto heap = metadata->table_.get();
  auto transaction = exec_ctx_->GetTransaction();

  while (child_executor_->Next(tuple, rid)) {
    exec_ctx_->GetLockManager()->LockUpgrade(transaction, *rid);
    Tuple t = GenerateUpdatedTuple(*tuple);

    if (heap->UpdateTuple(t, *rid, transaction)) {
      for (auto index_info = indexes.begin(); index_info < indexes.end(); index_info++) {
        bool found = false;
        for (auto idx : (*index_info)->index_->GetKeyAttrs()) {
          if (plan_->GetUpdateAttr()->find(idx) != plan_->GetUpdateAttr()->end()) {
            found = true;
            break;
          }
        }
        if (found) {
          auto old_key =
              tuple->KeyFromTuple(metadata->schema_, (*index_info)->key_schema_, (*index_info)->index_->GetKeyAttrs());
          auto new_key =
              t.KeyFromTuple(metadata->schema_, (*index_info)->key_schema_, (*index_info)->index_->GetKeyAttrs());
          auto record = IndexWriteRecord{*rid, plan_->TableOid(), WType::UPDATE, t, (*index_info)->index_oid_, catalog};
          record.old_tuple_ = *tuple;
          transaction->AppendTableWriteRecord(record);

          (*index_info)->index_->DeleteEntry(old_key, *rid, transaction);
          (*index_info)->index_->InsertEntry(new_key, *rid, transaction);
        }
      }
    }
  }

  return false;
}
}  // namespace bustub
