// 313. error XS0172: Type of conditional expression cannot be determined because '__Usual' and 'string' implicitly convert to one another
// this compiles in vulcan without /vo10
FUNCTION Start() AS VOID
LOCAL u := 1 AS USUAL
LOCAL l := TRUE AS LOGIC
u := iif(l , u , "")
? u
u := iif(l , 4 , u)
? u
u := iif(l , {1,2,3} , u)
? u[3]
