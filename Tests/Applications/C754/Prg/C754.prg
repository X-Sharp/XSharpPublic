// 754. error XS0037: Cannot convert null to 'PSZ' because it is a non-nullable value type
// Following is used in a lot of places in the VOGUI classes
#pragma warnings(165, off) //   unassigned local
#pragma warnings(219, off) //   assigned but not used
FUNCTION Start() AS VOID
	LOCAL o AS Foo
	o := Foo{}
	? o:Bar
	o:Test()
RETURN

CLASS Foo

ACCESS Bar
	LOCAL p := NULL AS PSZ // error XS0037
	? p == NULL // error XS0037
	? p != NULL // error XS0037
	RETURN 123

METHOD Test() AS VOID
	LOCAL pszBuf := NULL AS PSZ // error XS0037
	LOCAL dummyButNeeded AS OBJECT
	IF (dummyButNeeded == NULL_OBJECT)
		? 456
	ENDIF
RETURN

END CLASS
