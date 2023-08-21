// 311. Compiler crash with #define that already is defined by using the /define compiler option
// /define:TestDefine

#pragma warnings(9012, off) // duplicate define with another value
#define TestDefine

#define AnotherDefine 123



FUNCTION Start() AS VOID
#ifdef TestDefine
? "defined"
#endif

#ifdef AnotherDefine
? "defined"
#endif

