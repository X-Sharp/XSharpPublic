// 476. Compiler endless loop wiht circular #translate commands

#translate aaa => bbb
#translate bbb => aaa

// also this one results to the compiler never finishing:
#define true .t.

FUNCTION Start() AS VOID
aaa
RETURN

