// interface for arrays keyed on fixed-length strings
// of lowercase letters

typedef struct wordArray *WordArray;

// some constants for key elements
#define MIN_KEY_LETTER ('a')
#define MAX_KEY_LETTER ('z')
#define NUM_KEY_LETTERS (MAX_KEY_LETTER - MIN_KEY_LETTER + 1)

// create an array indexed by strings of length n
WordArray wordArrayCreate(unsigned int n);

// free all space used by array
void wordArrayDestroy(WordArray w);
//incKeyOverflow:./testIncKey zzzzzzzzzzzzz

// get the key length n for an array
int wordArrayKeyLength(WordArray w);

// Returns a pointer to location in word array
// indexed by string key.
//
// If key is the wrong length, or contains
// characters that are not between MIN_KEY and MAX_KEY,
// returns 0.
int *wordArrayRef(WordArray w, const char *key);

// returns malloc'd minimum key for w
// as a null-terminated string
char *wordArrayMinKey(WordArray w);

// Increments a string in place, returning 1 on overflow
// and 0 otherwise
// 
// Examples:
//
// incKey("aaa") -> "aab"
// incKey("abc") -> "abd"
// incKey("abz") -> "aca"
// incKey("zzz") -> "aaa", returns 1
int wordArrayIncKey(char *s);

// print the contents of an array,
// with one key and value on each line, as in
//
// aa 0
// ab 0
// ac 0
// ...
// 
void wordArrayPrint(WordArray w);
