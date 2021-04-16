//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


/// <include file="System.xml" path="doc/ObjCodeBlock/*" />
CLASS ObjCodeBlock
	HIDDEN oOwner       AS OBJECT
	HIDDEN symMethod    AS SYMBOL


/// <include file="System.xml" path="doc/ObjCodeBlock.Eval/*" />
METHOD Eval (args PARAMS ARRAY[]) AS USUAL               
	LOCAL xRet  AS USUAL
	xRet := __InternalSend(SELF:oOwner, SELF:symMethod, args)
	RETURN xRet




/// <include file="System.xml" path="doc/ObjCodeBlock.ctor/*" />
CONSTRUCTOR ( xOwner AS OBJECT, xMethod AS SYMBOL)   
	SELF:oOwner    := xOwner
	SELF:symMethod := xMethod
	RETURN 
END CLASS


