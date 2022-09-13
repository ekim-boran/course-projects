//===----------------------------------------------------------------------===//
//
//                         BusTub
//
// aggregation_executor.cpp
//
// Identification: src/execution/aggregation_executor.cpp
//
// Copyright (c) 2015-19, Carnegie Mellon University Database Group
//
//===----------------------------------------------------------------------===//
#include <memory>
#include <vector>

#include "execution/executors/aggregation_executor.h"

namespace bustub {

AggregationExecutor::AggregationExecutor(ExecutorContext *exec_ctx, const AggregationPlanNode *plan,
                                         std::unique_ptr<AbstractExecutor> &&child)
    : AbstractExecutor(exec_ctx),
      plan_(plan),
      child_(std::move(child)),
      aht_{plan->GetAggregates(), plan->GetAggregateTypes()},
      aht_iterator_{aht_.Begin()} {}

const AbstractExecutor *AggregationExecutor::GetChildExecutor() const { return child_.get(); }

void AggregationExecutor::Init() {}
// no having is implemented

bool AggregationExecutor::Next(Tuple *tuple, RID *rid) {
  if (!started) {
    Tuple t;

    while (child_->Next(&t, rid)) {
      AggregateKey key = MakeKey(&t);
      auto val = MakeVal(&t);
      aht_.InsertCombine(key, val);
    }
    aht_iterator_ = aht_.Begin();
    started = true;
  }
  auto having = plan_->GetHaving();

  while (aht_iterator_ != aht_.End()) {
    std::vector<Value> vs;
    for (auto &col : GetOutputSchema()->GetColumns()) {
      vs.emplace_back(
          col.GetExpr()->EvaluateAggregate(aht_iterator_.Key().group_bys_, aht_iterator_.Val().aggregates_));
    }
    *tuple = Tuple(vs, GetOutputSchema());
    if (having == nullptr ||
        having->EvaluateAggregate(aht_iterator_.Key().group_bys_, aht_iterator_.Val().aggregates_).GetAs<bool>()) {
      ++aht_iterator_;
      return true;
    }
    ++aht_iterator_;
  }
  return false;
}

}  // namespace bustub
