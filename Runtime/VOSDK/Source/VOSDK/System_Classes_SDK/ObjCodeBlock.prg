PARTIAL CLASS ObjCodeBlock
	HIDDEN oOwner       AS OBJECT
	HIDDEN symMethod    AS SYMBOL

METHOD Eval (xParam1)               

	LOCAL aArgs AS ARRAY
	LOCAL n     AS DWORD
	LOCAL i     AS DWORD
	LOCAL xRet  AS USUAL

	n := (DWORD) PCount()

	aArgs := ArrayCreate( n )

	FOR i := 1 TO n
		aArgs[i] := _GETMPARAM(i)
	NEXT

	xRet := _SendClassParams(SELF:oOwner, SELF:symMethod, aArgs)

	RETURN xRet


CONSTRUCTOR ( xOwner, xMethod )   
	SELF:oOwner    := xOwner
	SELF:symMethod := xMethod
	RETURN 
END CLASS

