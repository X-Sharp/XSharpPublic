PARTIAL CLASS DBSelectionIndex
	PROTECT oServer AS DBSERVER
	PROTECT cExpression AS STRING
	PROTECT cbExpression AS _CODEBLOCK
	PROTECT nSelect AS INT

METHOD Eval( ) 
	LOCAL nOldSelect AS DWORD
	LOCAL xRet AS USUAL

	VODBSelect( DWORD(SELF:nSelect), @nOldSelect )
	xRet := Eval( SELF:cbExpression )
	VODBSetSelect( LONGINT(nOldSelect) )
	RETURN xRet

CONSTRUCTOR( oDBServer, cExp, nWorkArea ) 

	SELF:oServer := oDBServer
	SELF:cExpression := cExp
	SELF:cbExpression := MExec( MCompile( "{ || " + cExp + " }" ) )
	SELF:nSelect := nWorkArea
	RETURN 

END CLASS

