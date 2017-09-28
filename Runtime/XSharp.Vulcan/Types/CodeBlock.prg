//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System.Collections
using System.Collections.Generic
using System.Linq
using System.Diagnostics
using XSharp
begin namespace XSharp	
	ABSTRACT CLASS @@CodeBlock IMPLEMENTS ICodeBlock
		private initonly _pcount as INT
		property PCount as INT GET _pcount

		PUBLIC CONSTRUCTOR (pCount as int)
			_pcount := pCount

		public abstract METHOD Eval(args params __Usual[] ) as __Usual

		PUBLIC METHOD EvalBlock(args params object[] ) as OBJECT
			var num := args:Length
			var uArgs := <__Usual>{num}
			FOR VAR i := 1 to num
				uArgs[i] := (__Usual) args[i]
			NEXT
			return SELF:Eval(uArgs)

		PUBLIC OVERRIDE METHOD ToString() as STRING
			return "{|" + SELF:_pcount:ToString() + "| ... }"

	END CLASS


	PUBLIC CLASS @@_CodeBlock INHERIT @@CodeBlock
		PROTECT _innerBlock as ICodeBlock 
		PROTECT _cMacro		as STRING
		PUBLIC CONSTRUCTOR(innerBlock as ICodeBlock, cMacro as STRING)
			SUPER(innerBlock:Pcount)
			_innerBlock := innerBlock
			_cMacro		:= cMacro

		PUBLIC OVERRIDE METHOD Eval(args params __Usual[]) as __Usual
			var num := args:Length
			var oArgs := <Object>{num}
			FOR VAR i := 1 to num
				oArgs[i] := (Object) args[i]
			NEXT
			return (__Usual) SELF:_innerBlock:EvalBlock(oArgs)
			
		PUBLIC OVERRIDE METHOD ToString() as STRING
			return _cMacro
	END CLASS

end namespace