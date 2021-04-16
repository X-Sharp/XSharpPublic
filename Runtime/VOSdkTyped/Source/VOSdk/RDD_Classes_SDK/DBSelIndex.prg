//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
#pragma options ("enforceself", on)
/// <include file="Rdd.xml" path="doc/DBSelectionIndex/*" />
[XSharp.Internal.TypesChanged];
CLASS DBSelectionIndex
	PROTECT oServer AS DbServer
	PROTECT cExpression AS STRING
	PROTECT cbExpression AS _CODEBLOCK
	PROTECT nSelect AS INT


/// <include file="Rdd.xml" path="doc/DBSelectionIndex.Eval/*" />
METHOD Eval( ) AS USUAL
	LOCAL nOldSelect := 0 AS DWORD 
	LOCAL xRet AS USUAL


	VoDbSelect( DWORD(SELF:nSelect), REF nOldSelect )
	xRet := Eval( SELF:cbExpression )
	VoDbSetSelect( LONGINT(nOldSelect) )
	RETURN xRet


/// <include file="Rdd.xml" path="doc/DBSelectionIndex.ctor/*" />
CONSTRUCTOR( oDBServer as DbServer, cExp as string, nWorkArea as int) 


	SELF:oServer := oDBServer
	SELF:cExpression := cExp
	SELF:cbExpression := MExec( MCompile( "{ || " + cExp + " }" ) )
	SELF:nSelect := nWorkArea
	RETURN 


END CLASS


