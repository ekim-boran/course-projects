#include "list.c"
#include <gtest/gtest.h>

void setup_head_tail(struct list_node **head, struct list_node **tail) {
  *head = allocate_node_with_key(0);
  *tail = allocate_node_with_key(0);
  initialize_list_head_tail(*head, *tail);
}

void free_list(struct list_node *head) {
  struct list_node *cur, *tail;
  cur = head;
  tail = head->prev;
  while (cur != tail) {
    struct list_node *next = cur->next;
    free(cur);
    cur = next;
  }
  free(tail);
}

TEST(DoublyLinkedListTest, AllocateHeadTail) {
  struct list_node *head, *tail;

  setup_head_tail(&head, &tail);

  // Empty Check
  ASSERT_EQ(head->next, tail);
  ASSERT_EQ(tail->prev, head);

  // Doubly linked list invariant
  ASSERT_EQ(head->prev, tail);
  ASSERT_EQ(tail->next, head);

  // free the list
  free_list(head);
}

TEST(DoublyLinkedListTest, IsInitiallyEmpty) {
  struct list_node *head, *tail;

  setup_head_tail(&head, &tail);

  ASSERT_TRUE(is_list_empty(head));

  // free the list
  free_list(head);
}

TEST(DoublyLinkedListTest, InsertNodeAfterTest) {
  struct list_node *head, *tail;

  setup_head_tail(&head, &tail);

  struct list_node *new_node;
  struct list_node *insertion_point = head;
  for (int i = 0; i < 10; i++) {
    new_node = allocate_node_with_key(i + 1);
    insert_node_after(insertion_point, new_node);

    ASSERT_EQ(insertion_point->next, new_node);
    insertion_point = insertion_point->next;
  }

  struct list_node *node = head->next;
  int idx = 1;
  // Check forward
  while (node != tail) {
    ASSERT_EQ(idx, node->key);
    idx++;
    node = node->next;
  }

  // Check backward
  idx = 10;
  node = tail->prev;
  while (node != head) {
    ASSERT_EQ(idx, node->key);
    idx--;
    node = node->prev;
  }

  // Doubly linked list invariant
  ASSERT_EQ(head->prev, tail);
  ASSERT_EQ(tail->next, head);

  // free the list
  free_list(head);
}

TEST(DoublyLinkedListTest, InsertFrontTest) {
  struct list_node *head, *tail;

  setup_head_tail(&head, &tail);

  struct list_node *new_node;
  struct list_node *insertion_point = head;
  for (int i = 0; i < 10; i++) {
    new_node = allocate_node_with_key(i + 1);
    insert_node_after(insertion_point, new_node);

    ASSERT_EQ(insertion_point->next, new_node);
  }

  struct list_node *node = head->next;
  int idx = 10;
  // Check forward
  while (node != tail) {
    ASSERT_EQ(idx, node->key);
    idx--;
    node = node->next;
  }

  // Check backward
  idx = 1;
  node = tail->prev;
  while (node != head) {
    ASSERT_EQ(idx, node->key);
    idx++;
    node = node->prev;
  }

  // Doubly linked list invariant
  ASSERT_EQ(head->prev, tail);
  ASSERT_EQ(tail->next, head);

  // free the list
  free_list(head);
}

TEST(DoublyLinkedListTest, InsertThenSearch) {
  struct list_node *head, *tail;

  setup_head_tail(&head, &tail);

  struct list_node *new_node;
  struct list_node *insertion_point = head;
  for (int i = 0; i < 10; i++) {
    new_node = allocate_node_with_key(i + 1);
    insert_node_after(insertion_point, new_node);

    ASSERT_EQ(insertion_point->next, new_node);
    insertion_point = insertion_point->next;
  }

  struct list_node *node = NULL;

  // Check Search functionality
  for (int i = 0; i < 10; i++) {
    node = search_list(head, i + 1);
    ASSERT_TRUE(node);
    ASSERT_EQ(node->key, i + 1);
  }

  // Doubly linked list invariant
  ASSERT_EQ(head->prev, tail);
  ASSERT_EQ(tail->next, head);

  // free the list
  free_list(head);
}

TEST(DoublyLinkedListTest, InsertThenSearchDelete) {
  struct list_node *head, *tail;

  setup_head_tail(&head, &tail);

  struct list_node *new_node;
  struct list_node *insertion_point = head;
  for (int i = 0; i < 10; i++) {
    new_node = allocate_node_with_key(i + 1);
    insert_node_after(insertion_point, new_node);

    ASSERT_EQ(insertion_point->next, new_node);
    insertion_point = insertion_point->next;
  }

  struct list_node *node = NULL;

  // Check Search functionality
  for (int i = 10; i > 0; i--) {
    node = search_list(head, i);
    ASSERT_TRUE(node); // Checks search_list does not return NULL
    ASSERT_EQ(node->key, i);
    del_node(node);
  }

  // Doubly linked list invariant
  ASSERT_EQ(head->prev, tail);
  ASSERT_EQ(tail->next, head);

  // free the list
  free_list(head);
}

TEST(DoublyLinkedListTest, InsertDeleteAndCount) {
  struct list_node *head, *tail;

  setup_head_tail(&head, &tail);

  struct list_node *new_node;
  struct list_node *insertion_point = head;
  for (int i = 0; i < 10; i++) {
    new_node = allocate_node_with_key(i + 1);
    insert_node_after(insertion_point, new_node);

    ASSERT_EQ(insertion_point->next, new_node);
    insertion_point = insertion_point->next;
  }

  ASSERT_EQ(count_list_length(head), 10);

  struct list_node *node = NULL;

  // Check Search functionality
  for (int i = 10; i > 0; i--) {
    node = search_list(head, i);
    ASSERT_TRUE(node); // Checks search_list does not return NULL
    ASSERT_EQ(node->key, i);
    del_node(node);

    ASSERT_EQ(count_list_length(head), i - 1);
  }
  ASSERT_EQ(count_list_length(head), 0);
  ASSERT_TRUE(is_list_empty(head));

  // Doubly linked list invariant
  ASSERT_EQ(head->prev, tail);
  ASSERT_EQ(tail->next, head);

  // free the list
  free_list(head);
}

TEST(DoublyLinkedListTest, InsertRandomSortedCheckSorted) {
  int random_vals[] = {17, 20, 8,  3,  11, 2, 5,  14, 7,  9,
                       13, 1,  16, 15, 4,  6, 10, 12, 19, 18};
  int num_random_vals = 20;
  int sum_of_inserted = 0;

  struct list_node *head, *tail;

  setup_head_tail(&head, &tail);

  for (int i = 0; i < num_random_vals; i++) {
    struct list_node *new_node = allocate_node_with_key(random_vals[i]);
    insert_sorted_by_key(head, new_node);

    sum_of_inserted += new_node->key;
  }
  ASSERT_EQ(count_list_length(head), 20);

  struct list_node *node = head->next;
  int prev_key = head->key;
  int sum_inserted_keys = 0;

  while (node != tail) {
    ASSERT_LT(prev_key, node->key);

    sum_inserted_keys += node->key;

    prev_key = node->key;
    node = node->next;
  }

  ASSERT_EQ(sum_of_inserted, sum_inserted_keys);

  // Doubly linked list invariant
  ASSERT_EQ(head->prev, tail);
  ASSERT_EQ(tail->next, head);

  // free the list
  free_list(head);
}

int main(int argc, char **argv) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
