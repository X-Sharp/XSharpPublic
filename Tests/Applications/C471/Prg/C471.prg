// 471. Parser errors in VO dialect

FUNCTION Start() AS VOID
LOCAL @@_init AS INT
@@_init := 1
? @@_init

LOCAL @@other AS INT
@@other := 1
? @@other

LOCAL _code AS INT
_code := 1
? _code

LOCAL l AS LOGIC
l := (TRUE) .and. (FALSE) // ok
l := ((TRUE) .and. (FALSE)) // error
l := ((FALSE) || (TRUE)) // ok
IF ((l != FALSE) .and. (l == TRUE))
	? l
ENDIF

RETURN

CLASS TestClass
	CONSTRUCTOR()
		SELF:_Init()
	RETURN
	METHOD _Init() AS VOID
	RETURN
END CLASS

