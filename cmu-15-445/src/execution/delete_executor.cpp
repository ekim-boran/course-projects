//===----------------------------------------------------------------------===//
//
//                         BusTub
//
// delete_executor.cpp
//
// Identification: src/execution/delete_executor.cpp
//
// Copyright (c) 2015-19, Carnegie Mellon University Database Group
//
//===----------------------------------------------------------------------===//
#include <memory>

#include "execution/executors/delete_executor.h"

namespace bustub {

DeleteExecutor::DeleteExecutor(ExecutorContext *exec_ctx, const DeletePlanNode *plan,
                               std::unique_ptr<AbstractExecutor> &&child_executor)
    : AbstractExecutor(exec_ctx), plan_(plan), child_executor_(std::move(child_executor)) {}

void DeleteExecutor::Init() {
  child_executor_->Init();
  auto catalog = GetExecutorContext()->GetCatalog();
  auto metadata = catalog->GetTable(plan_->TableOid());
  indexes = catalog->GetTableIndexes(metadata->name_);
}

bool DeleteExecutor::Next(Tuple *tuple, RID *rid) {
  auto catalog = GetExecutorContext()->GetCatalog();
  auto metadata = catalog->GetTable(plan_->TableOid());
  auto heap = metadata->table_.get();
  auto transaction = exec_ctx_->GetTransaction();
  if (!child_executor_->Next(tuple, rid)) {
    return false;
  }
  if (heap->MarkDelete(*rid, transaction)) {
    for (auto index_info = indexes.begin(); index_info < indexes.end(); index_info++) {
      auto key =
          tuple->KeyFromTuple(metadata->schema_, (*index_info)->key_schema_, (*index_info)->index_->GetKeyAttrs());

      transaction->AppendTableWriteRecord(
          IndexWriteRecord{*rid, plan_->TableOid(), WType::DELETE, key, (*index_info)->index_oid_, catalog});
      (*index_info)->index_->DeleteEntry(key, *rid, transaction);
    }
    return true;
  }
  return false;
}

}  // namespace bustub
