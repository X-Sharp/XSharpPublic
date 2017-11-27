// 582. Compiler crash.
// The following was part of the harbour project. Replacing with a #translate works fine,
// but in any case the compiler shoud not crash

// compiler crash:
#define COMPILE(c) &("{||" + c + "}")

// ok:
//#translate COMPILE(c) => &("{||" + c + "}")
FUNCTION Start() AS VOID
LOCAL c AS STRING
LOCAL u AS USUAL
c := "1+2"
u := COMPILE(c)
? Eval(u)
RETURN
