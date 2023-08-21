// 332. error XS0266: Cannot implicitly convert type 'int' to 'dword'. An explicit conversion exists (are you missing a cast?)
// relatively new issue
FUNCTION Start() AS VOID
LOCAL d := 1 AS DWORD
d := d + d // OK
d := d + 1 // OK
d := 1 + d
d := 123 * d
d := _Or(d,1) // OK
d := _Or(1,d,1) // OK
d := _Or(1,d)

