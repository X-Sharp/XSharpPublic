// 908. Internal Compiler Error with duplicate declaring of fields/memvars 
// https://github.com/X-Sharp/XSharpPublic/issues/1475

#pragma options (memvar, on)

//memvar+
MEMVAR Foo
PUBLIC Foo

FIELD Bar
MEMVAR Bar

MEMVAR Test
MEMVAR Test

FIELD Another
FIELD Another


FUNCTION Start() AS VOID
