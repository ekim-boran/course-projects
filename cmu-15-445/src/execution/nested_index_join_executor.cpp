//===----------------------------------------------------------------------===//
//
//                         BusTub
//
// nested_index_join_executor.cpp
//
// Identification: src/execution/nested_index_join_executor.cpp
//
// Copyright (c) 2015-19, Carnegie Mellon University Database Group
//
//===----------------------------------------------------------------------===//

#include "execution/executors/nested_index_join_executor.h"

namespace bustub {

NestIndexJoinExecutor::NestIndexJoinExecutor(ExecutorContext *exec_ctx, const NestedIndexJoinPlanNode *plan,
                                             std::unique_ptr<AbstractExecutor> &&child_executor)
    : AbstractExecutor(exec_ctx), plan_(plan), child_executor_(std::move(child_executor)) {}

void NestIndexJoinExecutor::Init() {
  child_executor_->Init();
  auto catalog = GetExecutorContext()->GetCatalog();
  auto table_metadata = catalog->GetTable(plan_->GetInnerTableOid());
  auto index_metadata = catalog->GetIndex(plan_->GetIndexName(), table_metadata->name_);
  BPlusTreeIndex<GenericKey<8>, RID, GenericComparator<8>> *c =
      dynamic_cast<BPlusTreeIndex<GenericKey<8>, RID, GenericComparator<8>> *>(index_metadata->index_.get());
  enditerator_ = c->GetEndIterator();
  iterator_ = c->GetEndIterator();
}

// void NestIndexJoinExecutor::Init1(GenericKey<8> key) {
//  auto catalog = GetExecutorContext()->GetCatalog();
//  auto table_metadata = catalog->GetTable(plan_->GetInnerTableOid());
//  auto index_metadata = catalog->GetIndex(plan_->GetIndexName(), table_metadata->name_);
//  iterator_ = dynamic_cast<BPlusTreeIndex<GenericKey<8>, RID, GenericComparator<8>> *>(index_metadata->index_.get())
//                  ->GetBeginIterator(key);
//}
void NestIndexJoinExecutor::Init1() {
  auto catalog = GetExecutorContext()->GetCatalog();
  auto table_metadata = catalog->GetTable(plan_->GetInnerTableOid());
  auto index_metadata = catalog->GetIndex(plan_->GetIndexName(), table_metadata->name_);
  iterator_ = dynamic_cast<BPlusTreeIndex<GenericKey<8>, RID, GenericComparator<8>> *>(index_metadata->index_.get())
                  ->GetBeginIterator();
}
Tuple Unify1(const Tuple *tuple1, const Schema *schema1, const Tuple *tuple2, const Schema *schema2,
             const Schema *out_schema) {
  std::vector<Value> values;
  auto out_cols = out_schema->GetColumns();
  values.reserve(out_cols.size());
  for (auto &col : out_cols) {
    values.emplace_back(col.GetExpr()->EvaluateJoin(tuple1, schema1, tuple2, schema2));
  }
  return Tuple(values, out_schema);
}

bool NestIndexJoinExecutor::Next(Tuple *tuple, RID *rid) {
  Tuple right_tuple;
  auto catalog = GetExecutorContext()->GetCatalog();
  auto table_metadata = catalog->GetTable(plan_->GetInnerTableOid());
  auto heap = table_metadata->table_.get();

  while (true) {
    while (iterator_ == enditerator_) {
      if (!child_executor_->Next(&left_tuple, rid)) {
        return false;
      }
      // auto predicate1 = plan_->Predicate();
      // auto left = predicate1->GetChildAt(0)->Evaluate(&left_tuple, child_executor_->GetOutputSchema());
      // GenericKey<8> key;
      // key.SetFromInteger(left.GetAs<int>());
      // Init1(key);
      Init1();
    }
    auto l_schema = child_executor_->GetOutputSchema();
    auto r_schema = &table_metadata->schema_;

    heap->GetTuple((*iterator_).second, &right_tuple, exec_ctx_->GetTransaction());
    auto predicate = plan_->Predicate();

    if (predicate == nullptr || predicate->EvaluateJoin(&left_tuple, l_schema, &right_tuple, r_schema).GetAs<bool>()) {
      *tuple = Unify1(&left_tuple, l_schema, &right_tuple, r_schema, GetOutputSchema());
      ++iterator_;
      return true;
    }
    ++iterator_;
  }
  return false;
}

}  // namespace bustub
