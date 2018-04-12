//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

// functions used by the compiler


// _FIELD->Name
FUNCTION __FieldGet( fieldName AS STRING ) AS USUAL
	return NIL

// CUSTOMER->NAME
FUNCTION __FieldGetWa( alias AS STRING, fieldName AS STRING ) AS USUAL
	return NIL

// _FIELD->Name := "Foo"
FUNCTION __FieldSet( fieldName AS STRING, uValue AS USUAL ) AS USUAL
	return uValue

// CUSTOMER->Name := "Foo"
FUNCTION __FieldSetWa( alias AS STRING, fieldName AS STRING, uValue AS USUAL ) AS USUAL
	return uValue


// MEMVAR myName
// ? MyName
function __MemVarGet(cName as string) as USUAL
	return NIL

// MEMVAR myName
// MyName := "NewValue"
function __MemVarPut(cName as string, uValue as usual) as usual
	return uValue


// ALIAS->(DoSomething())
// is translated to
// __pushWorkarea( alias ) ; DoSomething() ; __popWorkArea()

FUNCTION __pushWorkarea( alias AS USUAL ) AS VOID
	return 

FUNCTION __popWorkarea() AS VOID
   RETURN
