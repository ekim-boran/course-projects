//===----------------------------------------------------------------------===//
//
//                         BusTub
//
// limit_executor.cpp
//
// Identification: src/execution/limit_executor.cpp
//
// Copyright (c) 2015-19, Carnegie Mellon University Database Group
//
//===----------------------------------------------------------------------===//

#include "execution/executors/limit_executor.h"

namespace bustub {

LimitExecutor::LimitExecutor(ExecutorContext *exec_ctx, const LimitPlanNode *plan,
                             std::unique_ptr<AbstractExecutor> &&child_executor)
    : AbstractExecutor(exec_ctx), plan_(plan), child_executor_(std::move(child_executor)) {}

void LimitExecutor::Init() { child_executor_->Init(); }

bool LimitExecutor::Next(Tuple *tuple, RID *rid) {
  if (taken >= plan_->GetLimit()) {
    return false;
  }
  if (!skipped) {
    size_t c = 0;
    while (c < plan_->GetOffset()) {
      if (!child_executor_->Next(tuple, rid)) {
        return false;
      }
      c++;
    }
  }
  skipped = true;
  if (!child_executor_->Next(tuple, rid)) {
    return false;
  }
  taken++;
  return true;
}

}  // namespace bustub
