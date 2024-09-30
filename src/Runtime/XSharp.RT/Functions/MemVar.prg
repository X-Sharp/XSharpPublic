//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.Collections.Generic
USING XSharp.Internal

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/massign/*" />
/// <include file="RTComments.xml" path="Comments/Memvar/*" />
[NeedsAccessToLocals(TRUE)];
FUNCTION MAssign(cExp AS STRING,uValue AS USUAL) AS USUAL
    RETURN MemVarPutSym(cExp, uValue)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/memvarblock/*" />
/// <include file="RTComments.xml" path="Comments/Memvar/*" />
FUNCTION MemVarBlock(cMemvarName AS STRING) AS OBJECT
    RETURN {| uValue| IIF (uValue == NIL, MemVarGet(cMemvarName), MemVarPut(cMemvarName, uValue))}


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/memvarget/*" />
/// <include file="RTComments.xml" path="Comments/Memvar/*" />
[NeedsAccessToLocals(FALSE)];
FUNCTION MemVarGet(cVarName AS STRING) AS USUAL
    RETURN XSharp.MemVar.Get(cVarName)

[NeedsAccessToLocals(FALSE)];
FUNCTION MemVarTryGet(cVarName AS STRING, uValue OUT USUAL) AS LOGIC
    RETURN XSharp.MemVar.TryGet(cVarName, OUT uValue)



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/memvarput/*" />
/// <include file="RTComments.xml" path="Comments/Memvar/*" />
[NeedsAccessToLocals(TRUE)];
FUNCTION MemVarPut(cVarName AS STRING,uValue AS USUAL) AS USUAL
    RETURN XSharp.MemVar.Put(cVarName, uValue)



/// <summary>
/// Release a memory variable
/// </summary>
/// <param name="symVar">The name of the variable you want to clear. </param>
/// <remarks>
/// The value of this variable will be set to NIL. The variable is NOT deleted.
/// </remarks>
/// <include file="RTComments.xml" path="Comments/Memvar/*" />
[NeedsAccessToLocals(TRUE)];
FUNCTION MemVarClear(symVar AS STRING) AS VOID
	XSharp.MemVar.Put(symVar, NIL)
	RETURN



/// <summary>
/// Release a memory variable
/// </summary>
/// <param name="symVar">The name of the variable you want to release. </param>
/// <include file="RTComments.xml" path="Comments/Memvar/*" />
[NeedsAccessToLocals(TRUE)];
FUNCTION MemVarRelease(symVar AS STRING) AS VOID
	XSharp.MemVar.Release(symVar)
	RETURN



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/varget/*" />
/// <include file="RTComments.xml" path="Comments/Memvar/*" />
[NeedsAccessToLocals(FALSE)];
FUNCTION VarGet(cVarName AS STRING) AS USUAL
    RETURN __VarGet(cVarName)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/varput/*" />
/// <include file="RTComments.xml" path="Comments/Memvar/*" />
[NeedsAccessToLocals(TRUE)];
FUNCTION VarPut(cVarName AS STRING,uValue AS USUAL) AS USUAL
    RETURN __VarPut(cVarName, uValue)


/// <inheritdoc cref='VarGet' />
/// <param name="symVar">The name of the variable .</param>
[NeedsAccessToLocals(FALSE)];
FUNCTION VarGetSym(symVar AS SYMBOL) AS USUAL
    RETURN __VarGet(symVar)

/// <inheritdoc cref='VarPut' />
/// <param name="symVar">The name of the variable .</param>
[NeedsAccessToLocals(TRUE)];
FUNCTION VarPutSym(symVar AS SYMBOL,uValue AS USUAL) AS USUAL
    RETURN __VarPut(symVar, uValue)

/// <inheritdoc cref='MemVarBlock' />
/// <param name="symVar">The name of the variable .</param>
FUNCTION MemVarBlockSym(symMemvarName AS SYMBOL) AS OBJECT
    RETURN {| uValue| IIF (uValue == NIL, MemVarGetSym(symMemvarName), MemVarPutSym(symMemvarName, uValue))}

/// <inheritdoc cref='MemVarGet' />
/// <param name="symVar">The name of the variable .</param>
[NeedsAccessToLocals(FALSE)];
FUNCTION MemVarGetSym(symVar AS SYMBOL) AS USUAL
    RETURN XSharp.MemVar.Get(symVar)

/// <inheritdoc cref='MemVarPut' />
/// <param name="symVar">The name of the variable you want to assign to.</param>
[NeedsAccessToLocals(TRUE)];
FUNCTION MemVarPutSym(symVar AS SYMBOL, uValue AS USUAL) AS USUAL
    RETURN XSharp.MemVar.Put(symVar, uValue)


/// <summary>
/// Clear all memory variables (all public variables and the private variables of the current thread)
/// </summary>
/// <include file="RTComments.xml" path="Comments/Memvar/*" />
[NeedsAccessToLocals(TRUE)];
FUNCTION _MClear() AS VOID STRICT
	XSharp.MemVar.ClearAll()
	RETURN

/// <summary>
/// Release one or more memory variables variables.
/// </summary>
/// <param name="var1">Variable 1</param>
/// <param name="var2">Variable 2</param>
/// <param name="var3">Variable 3</param>
/// <param name="var4">Variable 4</param>
/// <param name="varn">Variable n</param>
/// <remarks>
/// The variables are not removed but their values are replaced with NIL.
/// </remarks>
/// <include file="RTComments.xml" path="Comments/Memvar/*" />
[NeedsAccessToLocals(TRUE)];
FUNCTION _MxRelease (var1, var2, var3, var4, varn) AS VOID CLIPPER
	LOCAL nCount AS LONG
	LOCAL name AS USUAL
	nCount := PCount()
	FOR VAR i := 1 TO nCount
		name := _GetFParam(i)
		IF name:IsString
			MemVarRelease(name)
		ELSEIF name:IsSymbol
			MemVarRelease(name)
		ELSE
			// throw argument error
            NOP
		ENDIF
	NEXT
	RETURN

/// <summary>
/// Release variables that match a certain wildcard pattern
/// </summary>
/// <param name="cMask">The wildcard pattern to use when releasing the memvars. May contain * and ? characters.</param>
/// <param name="lMatch">Indicates if the variables that need to be released should match (TRUE) or NOT match (FALSE) the pattern.</param>
/// <remarks>
/// For most dialects the variables are not removed but their values are replaced with NIL.
/// </remarks>
/// <include file="RTComments.xml" path="Comments/Memvar/*" />
[NeedsAccessToLocals(TRUE)];
FUNCTION _MRelease(cMask AS STRING, lMatch AS LOGIC)	AS VOID
	LOCAL cName AS STRING
    LOCAL lFoxPro as LOGIC
    lFoxPro := XSharp.RuntimeState.Dialect == XSharpDialect.FoxPro
	// Case INsensitive comparison. Symbols are all in UPPER case
	cMask := Upper(cMask)
	cName := _PrivateFirst(TRUE)
	DO WHILE cName != NULL
		IF _Like(cMask, cName) == lMatch
            IF lFoxPro
                MemVarRelease(cName)
            ELSE
			    MemVarClear(cName)
            ENDIF
		ENDIF
		cName := _PrivateNext()
	ENDDO
	RETURN



/// <summary>
/// Enumerate private variables
/// </summary>
FUNCTION _PrivateFirst(lCurrentOnly := FALSE AS LOGIC) AS STRING
	RETURN XSharp.MemVar.PrivatesFirst(lCurrentOnly)


/// <summary>
/// Enumerate private variables
/// </summary>
FUNCTION _PrivateNext() AS STRING STRICT
	RETURN XSharp.MemVar.PrivatesNext()


/// <summary>
/// Enumerate public variables
/// </summary>
FUNCTION _PublicFirst() AS STRING STRICT
	RETURN XSharp.MemVar.PublicsFirst()


/// <summary>
/// Enumerate public variables
/// </summary>
FUNCTION _PublicNext() AS STRING STRICT
	RETURN XSharp.MemVar.PublicsNext()

/// <summary>
/// Count private variables
/// </summary>

FUNCTION _PrivateCount(lCurrentOnly := FALSE AS LOGIC) AS INT
	RETURN XSharp.MemVar.PrivatesCount(lCurrentOnly)

/// <summary>
/// Count public variables
/// </summary>

FUNCTION _PublicCount() AS INT STRICT
	RETURN XSharp.MemVar.PublicsCount()

/// <summary>
/// Enumerate private variables
/// </summary>
FUNCTION _PrivateEnum(lCurrentOnly := FALSE AS LOGIC) AS IEnumerator<STRING>
	RETURN XSharp.MemVar.PrivatesEnum(lCurrentOnly)

/// <summary>
/// Enumerate public variables
/// </summary>
FUNCTION _PublicEnum AS IEnumerator<STRING>
	RETURN XSharp.MemVar.PublicsEnum()

