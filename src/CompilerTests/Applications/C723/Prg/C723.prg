// 723. XS9021: Signed/unsigned conversions from 'sbyte' to 'dword' may lead to loss of data or overflow errors
// warnings as errors enabled
FUNCTION Start() AS VOID
LOCAL l := TRUE AS LOGIC
LOCAL dw AS DWORD
dw := iif(l,1,2) // warning 9021
dw := iif(l,1U,2U) // no warning
? l, dw


