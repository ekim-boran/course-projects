//===----------------------------------------------------------------------===//
//
//                         BusTub
//
// index_scan_executor.cpp
//
// Identification: src/execution/index_scan_executor.cpp
//
// Copyright (c) 2015-19, Carnegie Mellon University Database Group
//
//===----------------------------------------------------------------------===//
#include "execution/executors/index_scan_executor.h"
#include "execution/executors/seq_scan_executor.h"
#include "execution/expressions/comparison_expression.h"
#include "storage/index/b_plus_tree_index.h"

namespace bustub {
IndexScanExecutor::IndexScanExecutor(ExecutorContext *exec_ctx, const IndexScanPlanNode *plan)
    : AbstractExecutor(exec_ctx), plan_(plan) {}

void IndexScanExecutor::Init() {
  auto catalog = GetExecutorContext()->GetCatalog();
  auto index_metadata = catalog->GetIndex(plan_->GetIndexOid());

  BPlusTreeIndex<GenericKey<8>, RID, GenericComparator<8>> *c =
      dynamic_cast<BPlusTreeIndex<GenericKey<8>, RID, GenericComparator<8>> *>(index_metadata->index_.get());
  iterator_ = dynamic_cast<BPlusTreeIndex<GenericKey<8>, RID, GenericComparator<8>> *>(index_metadata->index_.get())
                  ->GetBeginIterator();

  enditerator_ = c->GetEndIterator();
}

Tuple Projection1(const Tuple *tuple, const Schema &schema, const Schema *out_schema) {
  std::vector<Value> values;
  auto cols = out_schema->GetColumns();
  values.reserve(cols.size());
  for (auto &col : cols) {
    values.emplace_back(col.GetExpr()->Evaluate(tuple, &schema));
  }
  return Tuple(values, out_schema);
}
bool IndexScanExecutor::Next(Tuple *tuple, RID *rid) {
  auto catalog = GetExecutorContext()->GetCatalog();
  auto index_metadata = catalog->GetIndex(plan_->GetIndexOid());

  auto table_metadata = catalog->GetTable(index_metadata->table_name_);
  auto heap = table_metadata->table_.get();

  while ((iterator_) != enditerator_) {
    auto predicate = plan_->GetPredicate();

    auto value = (*iterator_).second;
    heap->GetTuple(value, tuple, exec_ctx_->GetTransaction());
    if (predicate == nullptr || plan_->GetPredicate()->Evaluate(tuple, &table_metadata->schema_).GetAs<bool>()) {
      *rid = value;
      *tuple = Projection1(tuple, table_metadata->schema_, GetOutputSchema());
      ++(iterator_);

      return true;
    }

    ++(iterator_);
  }

  return false;
}

}  // namespace bustub
