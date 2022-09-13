typedef struct list_node {
  struct list_node *next;
  struct list_node *prev;
  int key;
} list_node;

// Basic list functions
list_node *allocate_node_with_key(int key);
void initialize_list_head_tail(list_node *head, list_node *tail);
void insert_node_after(list_node *node, list_node *new_node);
void del_node(list_node *node);
list_node *search_list(list_node *head, int search_key);

int count_list_length(list_node *head);
int is_list_empty(list_node *head);
void iterate_print_keys(list_node *head);

// Sorted list functions
void insert_sorted_by_key(list_node *head, list_node *new_node);
