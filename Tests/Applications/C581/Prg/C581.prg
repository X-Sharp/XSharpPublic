// 582. Compiler crash.
// The following was part of the harbour project. Replacing with a #translate works fine,
// but in any case the compiler shoud not crash

// compiler crash:
#define COMPILE(c) &("{||" + c + "}")

// ok:
//#translate COMPILE(<c>) => &("{||" + <c> + "}")
FUNCTION Start() AS VOID
LOCAL cTest AS STRING
LOCAL u AS USUAL
cTest := "1+2"
u := COMPILE(cTest)
? Eval(u)
RETURN
