// 555. Compiler crash with invalid pass by reference (old problem)
// vulvan reports that there's no overload for these argument types (and a couple more error messages)
#pragma warnings(165, off) // unassigned

FUNCTION Start() AS VOID
LOCAL u AS USUAL
Test(REF u)
RETURN

FUNCTION Test(u)
RETURN u

