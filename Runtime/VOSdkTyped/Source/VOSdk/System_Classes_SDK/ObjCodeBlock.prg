PARTIAL CLASS ObjCodeBlock
	HIDDEN oOwner       AS OBJECT
	HIDDEN symMethod    AS SYMBOL

METHOD Eval (args PARAMS ARRAY[])               
	LOCAL xRet  AS USUAL
	xRet := __InternalSend(SELF:oOwner, SELF:symMethod, args)
	RETURN xRet


CONSTRUCTOR ( xOwner AS OBJECT, xMethod AS SYMBOL)   
	SELF:oOwner    := xOwner
	SELF:symMethod := xMethod
	RETURN 
END CLASS

