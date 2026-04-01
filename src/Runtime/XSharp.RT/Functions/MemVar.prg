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



/// <include file="XSharp.RT.Docs.xml" path="doc/MemVarClear/*" />
[NeedsAccessToLocals(TRUE)];
FUNCTION MemVarClear(symVar AS STRING) AS VOID
	XSharp.MemVar.Put(symVar, NIL)
	RETURN


/// <include file="XSharp.RT.Docs.xml" path="doc/MemVarRelease/*" />
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
[NeedsAccessToLocals(FALSE)];
FUNCTION VarGetSym(symVar AS SYMBOL) AS USUAL
    RETURN __VarGet(symVar)

/// <inheritdoc cref='VarPut' />
[NeedsAccessToLocals(TRUE)];
FUNCTION VarPutSym(symVar AS SYMBOL,uValue AS USUAL) AS USUAL
    RETURN __VarPut(symVar, uValue)

/// <include file="XSharp.RT.Docs.xml" path="doc/MemVarBlockSym/*" />
FUNCTION MemVarBlockSym(symMemvarName AS SYMBOL) AS OBJECT
    RETURN {| uValue| IIF (uValue == NIL, MemVarGetSym(symMemvarName), MemVarPutSym(symMemvarName, uValue))}

/// <inheritdoc cref='MemVarGet' />
[NeedsAccessToLocals(FALSE)];
FUNCTION MemVarGetSym(symVar AS SYMBOL) AS USUAL
    RETURN XSharp.MemVar.Get(symVar)

/// <inheritdoc cref='MemVarPut' />
[NeedsAccessToLocals(TRUE)];
FUNCTION MemVarPutSym(symVar AS SYMBOL, uValue AS USUAL) AS USUAL
    RETURN XSharp.MemVar.Put(symVar, uValue)


/// <include file="XSharp.RT.Docs.xml" path="doc/_MClear/*" />
[NeedsAccessToLocals(TRUE)];
FUNCTION _MClear() AS VOID STRICT
	XSharp.MemVar.ClearAll()
	RETURN

/// <include file="XSharp.RT.Docs.xml" path="doc/_MxRelease/*" />
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

/// <include file="XSharp.RT.Docs.xml" path="doc/_MRelease/*" />
[NeedsAccessToLocals(TRUE)];
FUNCTION _MRelease(cMask AS STRING, lMatch AS LOGIC)	AS VOID
	LOCAL cName AS STRING
    LOCAL lFoxPro as LOGIC
    lFoxPro := XSharp.RuntimeState.Dialect == XSharpDialect.FoxPro

    // #<skel> preprocessor wraps the mask in double quotes.
    // if the user provided their own quotation ("c*", 'c*', [c*]),
    // they will be nested. This loop strips them all away
    DO WHILE cMask:Length >= 2
        VAR cFirst := cMask[0]
        VAR cLast := cMask[cMask:Length - 1]

        IF (cFirst == c'"' .AND. cLast == c'"') .OR. ;
            (cFirst == c'\'' .AND. cLast == c'\'') .OR. ;
            (cFirst == c'[' .AND. cLast == c']')
            cMask := cMask:Substring(1, cMask:Length - 2)
        ELSE
            EXIT
        ENDIF
    ENDDO

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



/// <include file="XSharp.RT.Docs.xml" path="doc/_PrivateFirst/*" />
FUNCTION _PrivateFirst(lCurrentOnly := FALSE AS LOGIC) AS STRING
	RETURN XSharp.MemVar.PrivatesFirst(lCurrentOnly)


/// <include file="XSharp.RT.Docs.xml" path="doc/_PrivateNext/*" />
FUNCTION _PrivateNext() AS STRING STRICT
	RETURN XSharp.MemVar.PrivatesNext()


/// <include file="XSharp.RT.Docs.xml" path="doc/_PublicFirst/*" />
FUNCTION _PublicFirst() AS STRING STRICT
	RETURN XSharp.MemVar.PublicsFirst()


/// <include file="XSharp.RT.Docs.xml" path="doc/_PublicNext/*" />
FUNCTION _PublicNext() AS STRING STRICT
	RETURN XSharp.MemVar.PublicsNext()


/// <include file="XSharp.RT.Docs.xml" path="doc/_PrivateCount/*" />
FUNCTION _PrivateCount(lCurrentOnly := FALSE AS LOGIC) AS INT
	RETURN XSharp.MemVar.PrivatesCount(lCurrentOnly)


/// <include file="XSharp.RT.Docs.xml" path="doc/_PublicCount/*" />
FUNCTION _PublicCount() AS INT STRICT
	RETURN XSharp.MemVar.PublicsCount()

/// <include file="XSharp.RT.Docs.xml" path="doc/_PrivateEnum/*" />
FUNCTION _PrivateEnum(lCurrentOnly := FALSE AS LOGIC) AS IEnumerator<STRING>
	RETURN XSharp.MemVar.PrivatesEnum(lCurrentOnly)

/// <include file="XSharp.RT.Docs.xml" path="doc/_PublicEnum/*" />
FUNCTION _PublicEnum AS IEnumerator<STRING>
	RETURN XSharp.MemVar.PublicsEnum()

