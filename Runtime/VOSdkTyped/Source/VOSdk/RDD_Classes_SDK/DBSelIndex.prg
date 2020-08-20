CLASS DBSelectionIndex
	PROTECT oServer AS DbServer
	PROTECT cExpression AS STRING
	PROTECT cbExpression AS _CODEBLOCK
	PROTECT nSelect AS INT

METHOD Eval( ) AS USUAL
	LOCAL nOldSelect := 0 AS DWORD
	LOCAL xRet AS USUAL

	VoDbSelect( DWORD(SELF:nSelect), REF nOldSelect )
	xRet := Eval( SELF:cbExpression )
	VoDbSetSelect( LONGINT(nOldSelect) )
	RETURN xRet

CONSTRUCTOR( oDBServer as DbServer, cExp as string, nWorkArea as int) 

	SELF:oServer := oDBServer
	SELF:cExpression := cExp
	SELF:cbExpression := MExec( MCompile( "{ || " + cExp + " }" ) )
	SELF:nSelect := nWorkArea
	RETURN 

END CLASS

