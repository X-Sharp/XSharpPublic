#include "test.xh"
#define dddd 1

// 443. error XS9003: Pre-processor: Identifier expected
// note that if a line of the prg above the #define is a comment, then the error does not occur!

FUNCTION Start( ) AS VOID
	? dddd
RETURN

