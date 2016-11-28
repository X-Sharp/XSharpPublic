// 309. Cannot implicitly convert type 'Vulcan.__VODate' to 'dword'. An explicit conversion exists (are you missing a cast?)
FUNCTION Start() AS VOID
LOCAL dw AS DWORD
LOCAL n AS INT
LOCAL dt AS DATE
LOCAL u AS USUAL
dt := Today()
u := Today() - 1
dw := dt - u
? dw
dw := u - dt
? dw
n := dt - u
? n         
n := u - dt
? n

