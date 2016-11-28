// 311. Compiler crash with #define that already is defined by using the /define compiler option
// /define:TestDefine

#define TestDefine

#define AnotherDefine 123

FUNCTION Start() AS VOID
#ifdef TestDefine
? "defined"
#endif

#ifdef AnotherDefine
? "defined"
#endif

