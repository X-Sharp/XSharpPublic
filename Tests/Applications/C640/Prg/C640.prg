// 640. Failed to emit module 'C640'.
// problem is caused because /unsafe was not specified, but there's no pointer (pun intended :)) about it. Enabling it, the code compiles fine.
#pragma warnings(9023, off) // address of fixed
#pragma warnings(219, off) // assigned but not used

CLASS TestClass
	EXPORT fld1 AS INT
	PROTECT fld2 AS STRING
	METHOD Test() AS VOID
		LOCAL pI AS IntPtr
		pI := @SELF:fld1
		LOCAL pT AS PTR
		pT := @SELF:fld1
	RETURN

END CLASS
