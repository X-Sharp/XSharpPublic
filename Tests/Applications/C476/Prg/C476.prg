// 476. Compiler endless loop wiht circular #translate commands

#translate aaa => bbb
#translate bbb => aaa

FUNCTION Start() AS VOID
aaa
RETURN

