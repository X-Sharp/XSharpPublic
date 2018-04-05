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

// Base class for runtime compiled macros and compiletime codeblocks
[DebuggerDisplay( "{ToString(),nq}", Type := "CODEBLOCK" )] ;
abstract class XSharp.CodeBlock implements ICodeBlock
	private initonly _pcount as int
	property PCount as int get _pcount
	
	[DebuggerStepThrough] ;
	public constructor (pCount as int)
		_pcount := pCount
		
   /// <summary>
   /// Executes the codeblock.</summary>
   /// <param name="args">Zero or more arguments to pass to the codeblock.</param>
   /// <returns>The value of the last expression within the codeblock as a USUAL.
   /// If the last expression in the codeblock returns VOID, the codeblock
   /// returns NIL.</returns>
   /// <remarks>This method is abstract and is implemented in the derived class
   /// created by the compiler.</remarks>
	public abstract method Eval(args params usual[] ) as usual
		
	public method EvalBlock(args params object[] ) as object
		var num := args:Length
		var uArgs := usual[]{num}
		for var i := 1 to num
			uArgs[i] := (usual) args[i]
		next
		return self:Eval(uArgs)
		
	public override method ToString() as string
		return "{|" + self:_pcount:ToString() + "| ... }"
		
	end class

// Base class for runtime compiled macros	
[DebuggerDisplay( "{_cMacro}", Type := "_CODEBLOCK" )] ;
public class XSharp._CodeBlock inherit XSharp.CodeBlock
	protect _innerBlock as ICodeBlock 
	protect _cMacro		as string

	public constructor(innerBlock as ICodeBlock, cMacro as string)
		super(innerBlock:Pcount)
		_innerBlock := innerBlock
		_cMacro		:= cMacro
		
	public override method Eval(args params usual[]) as usual
		var num := args:Length
		var oArgs := object[]{num}
		for var i := 1 to num
			oArgs[i] := (object) args[i]
		next
		return (usual) self:_innerBlock:EvalBlock(oArgs)
	
	public override method ToString() as string
		return _cMacro
end class

