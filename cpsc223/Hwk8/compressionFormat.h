// Constants for the compressed file format.
//
// Each file starts with a table of
// COMPRESSED_CHARS * COMPRESSED_EXPANSION bytes,
// representing the expanded values of
// COMPRESSED_CHAR_MIN through COMPRESSED_CHAR_MAX.
//
// The rest of the file is a mix of ordinary characters
// (which are passed through unmodified),
// characters in the range COMPRESSED_CHAR_MIN
// through COMPRESSED_CHAR_MAX
// (which are replaced by their two-character expansions),
// and escape sequences consisting of
// COMPRESSED_CHAR_ESCAPE followed by an arbitrary
// character
// (which are replaced by the second character alone).
//
// An incomplete escape sequence at end of file is ignored.

#define COMPRESSED_CHAR_MIN (0x80)
#define COMPRESSED_CHAR_MAX (0xfe)
#define COMPRESSED_CHAR_ESCAPE (0xff)

// Each compressed char represents this many
// uncompressed chars.
#define COMPRESSED_EXPANSION (2)

// Number of compressed chars.
#define COMPRESSED_CHARS (COMPRESSED_CHAR_MAX - COMPRESSED_CHAR_MIN + 1)
