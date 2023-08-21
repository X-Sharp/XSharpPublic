// 213. compiler crash
// /vo8+
#define CLR2
#define CLR4

#ifdef CLR2
	#ifdef CLR4
	#endif
#endif
FUNCTION Start() AS VOID
