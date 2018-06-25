//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System.Collections
USING System.Collections.Generic
USING System.Linq
USING System.Diagnostics

USING System.Runtime.CompilerServices
/// <summary>Internal type that implements the VO Compatible CODEBLOCK type<br/>
/// This type has methods that normally are never directly called from user code.
/// </summary>
/// <seealso cref="T:XSharp.ICodeBlock"/>
[DebuggerDisplay( "{ToString(),nq}", Type := "CODEBLOCK" )] ;
ABSTRACT CLASS XSharp.CodeBlock IMPLEMENTS ICodeBlock
	PRIVATE INITONLY _pcount AS INT
	/// <summary>Returns the number of parameters in the codeblock</summary>
	PUBLIC VIRTUAL METHOD PCount AS INT 
		RETURN _Pcount
		
	/// <summary>This constructor is used by the Compiler for compile time codeblocks.</summary>
	/// <param name="pCount">Number of parameters defined in the compile time codeblock.</param>
	[DebuggerStepThrough] ;
	PROTECTED CONSTRUCTOR (pCount AS INT)
		_pcount := pCount
		
	/// <summary>
	/// Executes the codeblock.</summary>
	/// <param name="args">Zero or more arguments to pass to the codeblock.</param>
	/// <returns>The value of the last expression within the codeblock as a USUAL.
	/// If the last expression in the codeblock is of type VOID, then the codeblock
	/// returns NIL.</returns>
	/// <remarks>This method is abstract and is implemented in the derived class
	/// created by the compiler.</remarks>
	PUBLIC ABSTRACT METHOD Eval(args PARAMS CONST USUAL[] ) AS USUAL
	
	
	/// <summary>
	/// Eval method that can be called from code that does not "know" about the USUAL type.
	/// </summary>
	PUBLIC VIRTUAL METHOD EvalBlock(args PARAMS OBJECT[] ) AS OBJECT
		VAR num := args:Length
		VAR uArgs := __ObjectArrayToUsualArray(args)
		RETURN SELF:Eval(uArgs)
		
		
	/// <summary>
	/// Return a string that contains the # of parameters for display in the debugger.
	/// </summary>
	PUBLIC OVERRIDE METHOD ToString() AS STRING
		RETURN "{|" + SELF:_pcount:ToString() + "| ... }"
		
	// This method is used in the compiled codeblocks to get the arguments
	// from the parameter list
	/// <exclude />
	PROTECTED STATIC METHOD _BlockArg( args AS CONST USUAL[], index AS CONST INT ) AS USUAL
		RETURN IIF( index < args:Length, args[index + 1], NIL )
		END CLASS
		
		
/// <summary>Internal type that is the base class for macro compiled codeblocks.
/// </summary>
/// <seealso cref="T:XSharp.ICodeBlock"/>
/// <seealso cref="T:XSharp.IMacroCompiler"/>
/// <seealso cref="T:XSharp.CodeBlock"/>
[DebuggerDisplay( "{_cMacro}", Type := "_CODEBLOCK" )] ;
PUBLIC CLASS XSharp._CodeBlock INHERIT XSharp.CodeBlock
	/// <exclude />
	PROTECT _innerBlock AS ICodeBlock 
	/// <exclude />
	PROTECT _cMacro		AS STRING
	/// <exclude />
	PROTECT _lIsBlock   AS LOGIC
	
	/// <summary>This constructor is used by the Macro Compiler</summary>
	/// <param name="innerBlock">Compiled codeblock created by the macro compiler.</param>
	/// <param name="cMacro">Macro string that was used to create the codeblock.</param>
	/// <param name="lIsBlock">Did the macro string start with "{|".</param>
	PUBLIC CONSTRUCTOR(innerBlock AS ICodeBlock, cMacro AS STRING, lIsBlock AS LOGIC)
		SUPER(iif (lIsBlock, innerBlock:Pcount(), -1))
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
		LOCAL oRes AS OBJECT
		VAR oArgs := __UsualArrayToObjectArray(args)
		oRes := SELF:_innerBlock:EvalBlock(oArgs)
		uRes := __Usual{oRes}
		RETURN uRes
		
	/// <summary>
	/// Returns the original string that was used to create the macro compiled codeblock.
	/// </summary>
	PUBLIC OVERRIDE METHOD ToString() AS STRING
		RETURN _cMacro

	/// <summary>Was the codeblock created from a string that started with "{|" </summary>
	PUBLIC PROPERTY IsBlock AS LOGIC GET _lIsBlock
END CLASS

