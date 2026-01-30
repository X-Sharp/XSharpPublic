// 955. Bug in GetObjectByHandle() #1794
// https://github.com/X-Sharp/XSharpPublic/issues/1794

FUNCTION Start() AS VOID STRICT
LOCAL hWnd	AS PTR
LOCAL oObj	AS OBJECT
LOCAL n		AS DWORD

FOR n := 1 UPTO 10
	? n
	hWnd	:= GetForeGroundWindow()
	oObj	:= GetObjectByHandle(hWnd) // System.AccessViolationException here when no VO object is represented by handle
	IF oObj = NULL_OBJECT
		sleep(50)
	ELSE
		? AsString(oObj)
		EXIT
	ENDIF
NEXT

