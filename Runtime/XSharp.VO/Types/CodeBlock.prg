//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System.Collections
USING System.Collections.Generic
USING System.Linq
USING System.Diagnostics
USING XSharp
BEGIN NAMESPACE XSharp	
	ABSTRACT CLASS @@CodeBlock IMPLEMENTS ICodeBlock
		PRIVATE INITONLY _pcount AS INT
		PROPERTY PCount AS INT GET _pcount

		PUBLIC CONSTRUCTOR (pCount AS int)
			_pcount := pCount

		PUBLIC ABSTRACT METHOD Eval(args PARAMS __Usual[] ) AS __Usual

		PUBLIC METHOD EvalBlock(args PARAMS object[] ) AS OBJECT
			VAR num := args:Length
			VAR uArgs := <__Usual>{num}
			FOR VAR i := 1 TO num
				uArgs[i] := (__Usual) args[i]
			NEXT
			return SELF:Eval(uArgs)

		PUBLIC OVERRIDE METHOD ToString() AS STRING
			return "{|" + SELF:_pcount:ToString() + "| ... }"

	END CLASS


	PUBLIC CLASS @@_CodeBlock INHERIT @@CodeBlock
		PROTECT _innerBlock AS ICodeBlock 
		PROTECT _cMacro		AS STRING
		PUBLIC CONSTRUCTOR(innerBlock AS ICodeBlock, cMacro AS STRING)
			SUPER(innerBlock:Pcount)
			_innerBlock := innerBlock
			_cMacro		:= cMacro

		PUBLIC OVERRIDE METHOD Eval(args PARAMS __Usual[]) AS __Usual
			VAR num := args:Length
			VAR oArgs := <Object>{num}
			FOR VAR i := 1 TO num
				oArgs[i] := (Object) args[i]
			NEXT
			RETURN (__Usual) SELF:_innerBlock:EvalBlock(oArgs)
			
		PUBLIC OVERRIDE METHOD ToString() AS STRING
			RETURN _cMacro
	END CLASS

END NAMESPACE