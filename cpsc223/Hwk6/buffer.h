// Abstract data type for an editable string buffer.
//
// A buffer is a sequence of zero or more non-null characters
// followed by a null.
//
// It also tracks n cursor positions, each of which is a pointer
// to some character.
//
// Initially, the buffer is empty, and all cursors point to the null.
//
// For the functions bufferInsert, bufferCursorForward,
// and BufferCursorBack, specifying a cursor index that is
// too large results in the function having no effect.
#include <stddef.h>

typedef struct buffer *Buffer;

// Make a buffer with n cursors.
Buffer bufferCreate(size_t n);

// Free all space used by a buffer.
void bufferDestroy(Buffer b);

// Insert a new non-null character before the i-th cursor position.
// All cursors continue to point to the same character
// they pointed to before.
// If i is out of range or c is '\0', has no effect.
void bufferInsert(Buffer b, size_t i, char c);

// Advance cursor i one position.
// If i is out of range, or the i-th cursor
// already points to the final '\0',
// has no effect.
void bufferCursorForward(Buffer b, size_t i);

// Move cursor i back one position.
// If i is out of range, or the i-th cursor
// already points to the first character,
// has no effect.
void bufferCursorBack(Buffer b, size_t i);

// Return the number of cursors in a buffer
size_t bufferCursors(Buffer b);

// Return the number of characters in a buffer,
// not including the final null.
size_t bufferSize(Buffer b);

// Return the characters in a buffer
// as a null-terminated sequence of chars.
// Return value is malloc'd and should be freed by caller.
char *bufferContents(Buffer b);
