// 470. Parser errors in VULCAN dialect

FUNCTION Start() AS VOID
LOCAL _init AS INT
_init := 1
? _init

LOCAL other AS INT
other := 1
? other

LOCAL _code AS INT
_code := 1
? _code

LOCAL code AS INT // ok to be a keyword in VO, but should not be in vulcan dialect
code := 1
? code

LOCAL l AS LOGIC
l := (TRUE) && (FALSE) // ok
l := ((TRUE) && (FALSE)) // error
l := ((FALSE) || (TRUE)) // ok
IF ((l != FALSE) && (l == TRUE))
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

