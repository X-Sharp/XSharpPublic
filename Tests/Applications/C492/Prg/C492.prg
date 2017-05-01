// 492. #error and #warning messages are output by the compiler with whitespace removed
FUNCTION Start() AS VOID
#error where did the spaces go in this error message?
#warning !tabs		and  s p a c e s also not respected in this warning!
RETURN
