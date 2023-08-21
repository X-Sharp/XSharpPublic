// 44. warning XS0165: Use of unassigned local variable 'n'
// I think this should be disabled, until for now, as it bloats the compiler warning reporting. Vulcan does not report a warning on this.
#pragma warnings(165, off)   // use of unassigned variable
FUNCTION Start() AS VOID
LOCAL n AS INT
n++

