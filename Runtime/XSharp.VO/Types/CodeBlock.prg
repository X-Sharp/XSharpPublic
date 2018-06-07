//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System.Collections
using System.Collections.Generic
using System.Linq
using System.Diagnostics

using System.Runtime.CompilerServices
	/// <summary>Internal type that implements the VO Compatible CODEBLOCK type<br/>
	/// This type has methods that normally are never directly called from user code.
	/// </summary>
	/// <seealso cref="T:XSharp.ICodeBlock"/>
[DebuggerDisplay( "{ToString(),nq}", Type := "CODEBLOCK" )] ;
abstract class XSharp.CodeBlock implements ICodeBlock
	private initonly _pcount as int
	/// <summary>Returns the number of parameters in the codeblock</summary>
	public virtual method PCount as int 
		return _Pcount
	
	/// <summary>This constructor is used by the Compiler for compile time codeblocks.</summary>
	/// <param name="pCount">Number of parameters defined in the compile time codeblock.</param>
	[DebuggerStepThrough] ;
	PROTECTED constructor (pCount as int)
		_pcount := pCount
		
   /// <summary>
   /// Executes the codeblock.</summary>
   /// <param name="args">Zero or more arguments to pass to the codeblock.</param>
   /// <returns>The value of the last expression within the codeblock as a USUAL.
   /// If the last expression in the codeblock is of type VOID, then the codeblock
   /// returns NIL.</returns>
   /// <remarks>This method is abstract and is implemented in the derived class
   /// created by the compiler.</remarks>
	public abstract method Eval(args params const usual[] ) as usual
	
	
   /// <summary>
   /// Eval method that can be called from code that does not "know" about the USUAL type.
   /// </summary>
	public VIRTUAL method EvalBlock(args params object[] ) as object
		var num := args:Length
		var uArgs := __ObjectArrayToUsualArray(args)
		return self:Eval(uArgs)

	/// <exclude />		
	public override method ToString() as string
		return "{|" + self:_pcount:ToString() + "| ... }"

    // This method is used in the compiled codeblocks to get the arguments
	// from the parameter list
	/// <exclude />
    protected static method _BlockArg( args as CONST usual[], index as const int ) as usual
         return iif( index < args:Length, args[index + 1], NIL )
	end class


/// <summary>Internal type that is the base class for runtime macros.
/// </summary>
/// <seealso cref="T:XSharp.ICodeBlock"/>
/// <seealso cref="T:XSharp.IMacroCompiler"/>
[DebuggerDisplay( "{_cMacro}", Type := "_CODEBLOCK" )] ;
PUBLIC CLASS XSharp._CodeBlock INHERIT XSharp.CodeBlock
	/// <exclude />
	PROTECT _innerBlock AS ICodeBlock 
	/// <exclude />
	protect _cMacro		AS STRING
	/// <exclude />
	protect _lIsBlock   as LOGIC

	/// <summary>This constructor is used by the Macro Compiler</summary>
	/// <param name="innerBlock">Compiled codeblock created by the macro compiler.</param>
	/// <param name="cMacro">Macro string that was used to create the codeblock.</param>
	/// <param name="lIsBlock">Did the macro string start with "{|".</param>
	public constructor(innerBlock as ICodeBlock, cMacro as string, lIsBlock as LOGIC)
		super(innerBlock:Pcount())
		_innerBlock := innerBlock
		_cMacro		:= cMacro
		_lIsBlock   := lIsBlock
		
	/// <summary>
	/// Executes the codeblock.</summary>
	/// <param name="args">Zero or more arguments to pass to the codeblock.</param>
	/// <returns>The value of the last expression within the codeblock as a USUAL.
	/// If the last expression in the codeblock is of type VOID, then the codeblock
	/// returns NIL.</returns>
	PUBLIC OVERRIDE METHOD Eval(args PARAMS USUAL[]) AS USUAL
		LOCAL uRes AS USUAL
		LOCAL oRes as OBJECT
		VAR oArgs := __UsualArrayToObjectArray(args)
		oRes := SELF:_innerBlock:EvalBlock(oArgs)
		uRes := __Usual{oRes}
		return uRes
	
	public override method ToString() as string
		RETURN _cMacro
	/// <summary>Was the codeblock created from a string that started with "{|" </summary>
	public property IsBlock as LOGIC GET _lIsBlock
end class

