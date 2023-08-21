#pragma options("vo7", on)
FUNCTION Start() AS VOID
LOCAL n AS INT
n := 123      
test(@n)
? n   



PROCEDURE test(u AS USUAL)
    ? u
u := 345




