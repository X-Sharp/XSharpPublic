// 467. error XS0229: Ambiguity between 'Functions.WM_USER' and 'Functions.WM_USER'

// defined also in C467_helper.dll
#pragma warnings(219, off) // assigned but not used
#pragma warnings(9043, off) // ambiguous
DEFINE WM_USER := 555

FUNCTION Start() AS VOID
	// no errors here
	? WM_USER
	LOCAL n AS INT
	n := WM_USER

	LOCAL o AS TestClass
	o := TestClass{WM_USER}

	? DEFINE_IN_TWO_DLLS_ONLY
RETURN


CLASS TestClass
	// when using the DEFINE inside the class code, XS0229 is being reported
	CONSTRUCTOR(n AS INT)
		? n
		? WM_USER
		n := WM_USER

		? DEFINE_IN_TWO_DLLS_ONLY
	RETURN

	METHOD Test() AS INT
		LOCAL n AS INT
		n := WM_USER
	RETURN WM_USER

END CLASS

