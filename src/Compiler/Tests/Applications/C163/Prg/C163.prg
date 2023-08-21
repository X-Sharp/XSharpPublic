// 163. Compiler crash with RECOVER statement
FUNCTION Start() AS VOID
BEGIN SEQUENCE
   NOP
RECOVER
	? "recover"
END SEQUENCE

