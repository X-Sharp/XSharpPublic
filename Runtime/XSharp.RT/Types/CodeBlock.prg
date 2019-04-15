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
/// <seealso cref="T:XSharp.ICodeblock"/>
[DebuggerDisplay( "{ToString(),nq}", Type := "CODEBLOCK" )] ;
ABSTRACT CLASS XSharp.Codeblock IMPLEMENTS ICodeblock
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
		VAR uArgs := _ObjectArrayToUsualArray(args)
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
/// <seealso cref="T:XSharp.ICodeblock"/>
/// <seealso cref="T:XSharp.IMacroCompiler"/>
/// <seealso cref="T:XSharp.Codeblock"/>
[DebuggerDisplay( "{_cMacro}", Type := "_Codeblock" )] ;
PUBLIC CLASS XSharp._Codeblock INHERIT XSharp.Codeblock
	/// <exclude />
	PROTECT _innerBlock AS ICodeblock 
	/// <exclude />
	PROTECT _cMacro		AS STRING
	/// <exclude />
	PROTECT _lIsBlock   AS LOGIC
	/// <exclude />
	PROTECT _addsMemVars AS LOGIC
	
	/// <summary>This constructor is used by the Macro Compiler</summary>
	/// <param name="innerBlock">Compiled codeblock created by the macro compiler.</param>
	/// <param name="cMacro">Macro string that was used to create the codeblock.</param>
	/// <param name="lIsBlock">Did the macro string start with "{|".</param>
	/// <param name="lAddsMemvars">Does the macro create Memvars .</param>
	PUBLIC CONSTRUCTOR(innerBlock AS ICodeblock, cMacro AS STRING, lIsBlock AS LOGIC, lAddsMemvars AS LOGIC)
		SUPER(IIF (lIsBlock, innerBlock:Pcount(), -1))
		_innerBlock := innerBlock
		_cMacro		:= cMacro
		_lIsBlock   := lIsBlock
        _addsMemVars := lAddsMemVars
		
	/// <summary>
	/// Executes the codeblock.</summary>
	/// <param name="args">Zero or more arguments to pass to the codeblock.</param>
	/// <returns>The value of the last expression within the codeblock as a USUAL.
	/// If the last expression in the codeblock is of type VOID, then the codeblock
	/// returns NIL.</returns>
	PUBLIC OVERRIDE METHOD Eval(args PARAMS USUAL[]) AS USUAL
		LOCAL uRes      AS USUAL
		LOCAL oRes      AS OBJECT
        LOCAL iLevel    AS INT
        IF _addsMemVars
            iLevel := XSharp.MemVar.InitPrivates()
        ELSE
            iLevel := 0
        ENDIF
        TRY
		    VAR oArgs := _UsualArrayToObjectArray(args)
		    oRes := SELF:_innerBlock:EvalBlock(oArgs)
		    uRes := __Usual{oRes}
        FINALLY
            IF _addsMemVars
                XSharp.MemVar.ReleasePrivates(iLevel)
            ENDIF
        END TRY
		RETURN uRes
		
	/// <summary>
	/// Returns the original string that was used to create the macro compiled codeblock.
	/// </summary>
	PUBLIC OVERRIDE METHOD ToString() AS STRING
        IF _lIsBlock
		    RETURN _cMacro
        ELSE
            RETURN "{|| "+_cMacro+" }"
        ENDIF

	/// <summary>Was the codeblock created from a string that started with "{|" </summary>
	PUBLIC PROPERTY IsBlock AS LOGIC GET _lIsBlock
END CLASS

