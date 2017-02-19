//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
BEGIN NAMESPACE XSharp
INTERFACE ICodeBlock
	METHOD		EvalBlock( args params OBJECT[]) AS OBJECT
	PROPERTY	PCount AS LONG GET
END INTERFACE
INTERFACE IDate
	PROPERTY Year		AS INT GET SET
	PROPERTY Month		AS INT GET SET
	PROPERTY Day		AS INT GET SET
	PROPERTY Value		AS DateTime GET SET
	PROPERTY IsEmpty	AS LOGIC GET
END INTERFACE
INTERFACE IFloat
	PROPERTY Value    AS REAL8 GET SET
	PROPERTY Digits	  AS INT  GET SET
	PROPERTY Decimals AS INT  GET SET
END INTERFACE

INTERFACE IMacroCompiler
	METHOD Compile(macro as STRING) AS ICodeBlock
END INTERFACE
END NAMESPACE