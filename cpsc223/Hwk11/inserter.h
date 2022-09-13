// An Inserter object manages an ordered linked list
// of null-terminated strings.

struct element {
    struct element *next;  // next element in list
    char *string;          // string stored in this element
};

// Opaque Inserter data type
typedef struct inserter *Inserter;

// Create a new Inserter to manage the list 
// whose head is stored in *head.
// This list should start empty.
Inserter inserterCreate(struct element **head);

// Add a string to he managed list if not
// already present.
// The new string will be a malloc'd copy
// of s.
void inserterAdd(Inserter, const char *s);

// Free all space used by the given Inserter.
// This will *not* free any space used by
// the managed linked list.
void inserterDestroy(Inserter);
