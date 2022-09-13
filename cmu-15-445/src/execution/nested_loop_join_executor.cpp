//===----------------------------------------------------------------------===//
//
//                         BusTub
//
// nested_loop_join_executor.cpp
//
// Identification: src/execution/nested_loop_join_executor.cpp
//
// Copyright (c) 2015-19, Carnegie Mellon University Database Group
//
//===----------------------------------------------------------------------===//

#include "execution/executors/nested_loop_join_executor.h"
#include "execution/executor_factory.h"

namespace bustub {

NestedLoopJoinExecutor::NestedLoopJoinExecutor(ExecutorContext *exec_ctx, const NestedLoopJoinPlanNode *plan,
                                               std::unique_ptr<AbstractExecutor> &&left_executor,
                                               std::unique_ptr<AbstractExecutor> &&right_executor)
    : AbstractExecutor(exec_ctx), plan_(plan), left_executor_(std::move(left_executor)) {}

void NestedLoopJoinExecutor::Init() { left_executor_->Init(); }

Tuple Unify(const Tuple *tuple1, const Schema *schema1, const Tuple *tuple2, const Schema *schema2,
            const Schema *out_schema) {
  std::vector<Value> values;
  auto out_cols = out_schema->GetColumns();
  values.reserve(out_cols.size());
  for (auto &col : out_cols) {
    values.emplace_back(col.GetExpr()->EvaluateJoin(tuple1, schema1, tuple2, schema2));
  }
  return Tuple(values, out_schema);
}

bool NestedLoopJoinExecutor::Next(Tuple *tuple, RID *rid) {
  Tuple right_tuple;
  while (true) {
    if (current_right == nullptr) {
      if (!left_executor_->Next(&left_tuple, rid)) {
        return false;
      }
      current_right = ExecutorFactory::CreateExecutor(exec_ctx_, plan_->GetRightPlan());
      current_right->Init();
    }
    auto l_schema = plan_->GetLeftPlan()->OutputSchema();
    auto r_schema = plan_->GetRightPlan()->OutputSchema();

    if (current_right->Next(&right_tuple, rid)) {
      auto predicate = plan_->Predicate();

      if (predicate == nullptr ||
          predicate->EvaluateJoin(&left_tuple, l_schema, &right_tuple, r_schema).GetAs<bool>()) {
        *tuple = Unify(&left_tuple, l_schema, &right_tuple, r_schema, GetOutputSchema());
        return true;
      }
    } else {
      current_right = nullptr;
    }
  }
  return false;
}

}  // namespace bustub
