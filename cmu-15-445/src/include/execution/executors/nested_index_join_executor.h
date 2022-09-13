//===----------------------------------------------------------------------===//
//
//                         BusTub
//
// nested_index_join_executor.h
//
// Identification: src/include/execution/executors/nested_index_join_executor.h
//
// Copyright (c) 2015-19, Carnegie Mellon University Database Group
//
//===----------------------------------------------------------------------===//

#pragma once

#include <memory>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

#include "execution/executor_context.h"
#include "execution/executors/abstract_executor.h"
#include "execution/expressions/abstract_expression.h"
#include "execution/plans/nested_index_join_plan.h"
#include "storage/table/tmp_tuple.h"
#include "storage/table/tuple.h"

namespace bustub {

/**
 * IndexJoinExecutor executes index join operations.
 */
class NestIndexJoinExecutor : public AbstractExecutor {
 public:
  /**
   * Creates a new nested index join executor.
   * @param exec_ctx the context that the hash join should be performed in
   * @param plan the nested index join plan node
   * @param outer table child
   */
  NestIndexJoinExecutor(ExecutorContext *exec_ctx, const NestedIndexJoinPlanNode *plan,
                        std::unique_ptr<AbstractExecutor> &&child_executor);

  const Schema *GetOutputSchema() override { return plan_->OutputSchema(); }

  void Init() override;
  // void Init1(GenericKey<8> key);
  void Init1();

  bool Next(Tuple *tuple, RID *rid) override;

 private:
  /** The nested index join plan node. */
  const NestedIndexJoinPlanNode *plan_;
  std::unique_ptr<AbstractExecutor> child_executor_;
  IndexIterator<bustub::GenericKey<8UL>, bustub::RID, bustub::GenericComparator<8UL>> iterator_;
  IndexIterator<bustub::GenericKey<8UL>, bustub::RID, bustub::GenericComparator<8UL>> enditerator_;
  Tuple left_tuple;
};
}  // namespace bustub
