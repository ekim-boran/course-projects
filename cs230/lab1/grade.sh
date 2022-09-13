#!/usr/bin/env bash

tests=("DoublyLinkedListTest.AllocateHeadTail" "DoublyLinkedListTest.IsInitiallyEmpty" 
        "DoublyLinkedListTest.InsertNodeAfterTest" "DoublyLinkedListTest.InsertFrontTest"
         "DoublyLinkedListTest.InsertThenSearch" "DoublyLinkedListTest.InsertThenSearchDelete"
         "DoublyLinkedListTest.InsertDeleteAndCount" "DoublyLinkedListTest.InsertRandomSortedCheckSorted"
        )
score=0

for test in ${tests[@]}; do
   timeout 2s ./test --gtest_filter=$test
   if [ $? -eq 0 ]; then
        score=$((score + 10))
   else
        echo $output
   fi
done

echo Score = $score/80
