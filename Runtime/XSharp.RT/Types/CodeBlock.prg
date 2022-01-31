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
/// <seealso cref="ICodeblock"/>
//[DebuggerDisplay( "{ToString(),nq}", Type := "CODEBLOCK" )] ;
ABSTRACT CLASS XSharp.Codeblock IMPLEMENTS ICodeblock2
	PRIVATE INITONLY _pcount AS INT

    [DebuggerBrowsable(DebuggerBrowsableState.Never)];
    PRIVATE STATIC nullArgs AS USUAL[]

    STATIC CONSTRUCTOR
        nullArgs := USUAL[]{0}
        RETURN

	/// <summary>Returns the number of parameters in the codeblock</summary>
	PUBLIC VIRTUAL METHOD PCount AS INT
		RETURN _pcount

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
	/// <remarks>This method is abstract and is implemented in the derived classes
	/// created by the compiler.</remarks>
    PUBLIC ABSTRACT METHOD Eval(args PARAMS CONST USUAL[] ) AS USUAL

    /// <inheritdoc />
    PROPERTY ResultType as __UsualType AUTO

	/// <summary>
	/// Eval method that can be called from code that does not "know" about the USUAL type.
    /// such as the code in the RDD classes.
	/// </summary>
	PUBLIC VIRTUAL METHOD EvalBlock(args PARAMS OBJECT[] ) AS OBJECT
        LOCAL uArgs as USUAL[]
        if args:Length == 0
            uArgs := nullArgs
        ELSE
		    uArgs := _ObjectArrayToUsualArray(args)
        ENDIF
        var result := SELF:Eval(uArgs)
        SELF:ResultType := result:Type
        IF IsNil(result) .and. RuntimeState.Dialect == XSharpDialect.FoxPro
            result := FALSE
        ENDIF
        RETURN result


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
/// <seealso cref="ICodeblock"/>
/// <seealso cref="IMacroCompiler"/>
/// <seealso cref="Codeblock"/>
[DebuggerDisplay( "{_cMacro}", Type := "_Codeblock" )] ;
PUBLIC CLASS XSharp._Codeblock INHERIT XSharp.Codeblock IMPLEMENTS IRtCodeblock
	/// <exclude />
	INITONLY PROTECT _innerBlock AS ICodeblock
	/// <exclude />
	INITONLY PROTECT _cMacro		AS STRING
	/// <exclude />
	INITONLY PROTECT _lIsBlock   AS LOGIC
	/// <exclude />
	INITONLY PROTECT _addsMemVars AS LOGIC

    [DebuggerBrowsable(DebuggerBrowsableState.Never)];
    STATIC PRIVATE nullArgs AS OBJECT[]

    STATIC CONSTRUCTOR
        nullArgs := OBJECT[]{0}
        RETURN

	/// <summary>This constructor is used by the Macro Compiler</summary>
	/// <param name="innerBlock">Compiled codeblock created by the macro compiler.</param>
	/// <param name="cMacro">Macro string that was used to create the codeblock.</param>
	/// <param name="lIsBlock">Did the macro string start with "{|".</param>
	/// <param name="lAddsMemvars">Does the macro create Memvars .</param>
	PUBLIC CONSTRUCTOR(innerBlock AS ICodeblock, cMacro AS STRING, lIsBlock AS LOGIC, lAddsMemvars AS LOGIC)
		SUPER(IIF (lIsBlock, innerBlock:PCount(), -1))
		_innerBlock := innerBlock
		_cMacro		:= cMacro
		_lIsBlock   := lIsBlock
        _addsMemVars := lAddsMemvars

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
            iLevel := XSharp.MemVar.InitPrivates(TRUE)
        ELSE
            iLevel := 0
        ENDIF
        TRY
            LOCAL oArgs as OBJECT[]
            if args:Length == 0
                oArgs := nullArgs
            ELSE
                oArgs := _UsualArrayToObjectArray(args)
            ENDIF
		    oRes := SELF:_innerBlock:EvalBlock(oArgs)
		    uRes := __Usual{oRes}
            SELF:ResultType := if(oRes == NULL , __UsualType:Void, uRes:Type)
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

