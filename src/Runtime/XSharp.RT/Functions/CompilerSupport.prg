//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
// functions used by the compiler

USING XSharp.Internal
USING System.Collections

/// <exclude />
FUNCTION __StringCompare(strLHS AS STRING, strRHS AS STRING) AS INT
    RETURN RuntimeState.StringCompare(strLHS, strRHS)


/// <exclude />
FUNCTION  __StringEquals(strLHS AS STRING, strRHS AS STRING) AS LOGIC
    LOCAL IsEqual:= FALSE AS LOGIC
    LOCAL lengthRHS AS INT
    IF Object.ReferenceEquals(strLHS, strRHS)
        IsEqual := TRUE
    ELSEIF RuntimeState.Exact
        IsEqual := String.Equals(strLHS , strRHS)
    ELSEIF strLHS != NULL .AND. strRHS != NULL
        lengthRHS := strRHS:Length
        IF lengthRHS == 0
            IsEqual := TRUE        // With SetExact(FALSE) then "aaa" = "" returns TRUE
        ELSEIF lengthRHS <= strLHS:Length

            IsEqual := String.Compare( strLHS, 0, strRHS, 0, lengthRHS, StringComparison.Ordinal ) == 0
        ENDIF
    ELSEIF strLHS == NULL .AND. strRHS == NULL
        IsEqual := TRUE
    ENDIF
    RETURN IsEqual

/// <exclude />
FUNCTION  __StringNotEquals(strLHS AS STRING, strRHS AS STRING) AS LOGIC
    LOCAL notEquals := FALSE AS LOGIC
    LOCAL lengthRHS AS INT
    IF Object.ReferenceEquals(strLHS, strRHS)
        notEquals := FALSE
    ELSEIF RuntimeState.Exact
        notEquals := !String.Equals(strLHS , strRHS)
    ELSEIF strLHS != NULL .AND. strRHS != NULL
        // shortcut: chec first char
        lengthRHS := strRHS:Length
        IF lengthRHS == 0
            notEquals := FALSE        // With SetExact(FALSE) then "aaa" = "" returns TRUE
        ELSEIF lengthRHS <= strLHS:Length
            notEquals := String.Compare( strLHS, 0, strRHS, 0, lengthRHS, StringComparison.Ordinal ) != 0
        ELSE
            notEquals := TRUE
        ENDIF
    ELSEIF strLHS == NULL .AND. strRHS == NULL
        notEquals := FALSE
    ELSE
        notEquals := TRUE
    ENDIF
    RETURN notEquals

/// <exclude />
FUNCTION __FieldGet( fieldName AS STRING ) AS USUAL
    LOCAL fieldpos := FieldPos( fieldName ) AS DWORD
    LOCAL ret := NULL AS OBJECT
    IF fieldpos == 0
        THROW Error.VoDbError( EG_ARG, EDB_FIELDNAME, __FUNCTION__,  NAMEOF(fieldName), 1, <OBJECT>{fieldName}  )
    ELSE
        _DbThrowErrorOnFailure(__FUNCTION__, CoreDb.FieldGet( fieldpos, REF ret ))
    ENDIF
    IF ret == NULL
        CoreDb.FieldInfo(DBS_BLANK, fieldpos, REF ret)
    ENDIF
    RETURN ret


/// <exclude />
FUNCTION __FieldGetWa( area AS USUAL, fieldName AS STRING ) AS USUAL
    // XBase defines that 'M' in 'M->Name' means a Memvar
    IF area:IsNil
        RETURN __FieldGet(fieldName)
    ENDIF
    IF area:IsString .AND. ((STRING) area):ToUpper() == "M"
        RETURN __MemVarGet(fieldName)
    ENDIF
    IF area:IsSymbol .AND. ((STRING) area):ToUpper() == "M"
        RETURN __MemVarGet(fieldName)
    ENDIF
    RETURN FieldGetSelect(area,fieldName)

/// <exclude />
FUNCTION __FieldGetWa2(wa AS STRING, fldName AS STRING, lAllowUndeclared AS LOGIC) AS USUAL
    LOCAL nArea := VoDbGetSelect(wa) AS DWORD
    IF nArea != 0 .OR. ! lAllowUndeclared
        RETURN __FieldGetWa(wa, fldName)
    ENDIF
    LOCAL uObject := __MemVarGet(wa) AS USUAL
    RETURN IVarGet(uObject, fldName)

/// <exclude />
FUNCTION __FieldSet( fieldName AS STRING, uValue AS USUAL ) AS USUAL
    LOCAL fieldpos := FieldPos( fieldName ) AS DWORD
    IF fieldpos == 0
        THROW Error.VoDbError( EG_ARG, EDB_FIELDNAME, __FUNCTION__,  NAMEOF(fieldName), 1, <OBJECT>{fieldName}  )
    ELSE
        _DbThrowErrorOnFailure(__FUNCTION__, CoreDb.FieldPut( fieldpos, uValue))
    ENDIF
    // We return the original value to allow chained expressions
    RETURN uValue


/// <exclude />
FUNCTION __FieldSetWa( area AS USUAL, fieldName AS STRING, uValue AS USUAL ) AS USUAL
    IF area:IsNil
        RETURN __FieldSet(fieldName,uValue)
    ENDIF
    IF area:IsString .AND. ((STRING) area):ToUpper() == "M"
        RETURN __MemVarPut(fieldName,uValue)
    ENDIF
    IF area:IsSymbol .AND. ((STRING) area):ToUpper() == "M"
        RETURN __MemVarPut(fieldName,uValue)
    ENDIF
    FieldPutSelect(area,fieldName,uValue)
    RETURN uValue


/// <exclude />
FUNCTION __FieldSetWa2(wa AS STRING, fldName AS STRING, uValue AS USUAL,lAllowUndeclared AS LOGIC) AS USUAL
    LOCAL nArea := VoDbGetSelect(wa) AS DWORD
    IF nArea != 0 .OR. ! lAllowUndeclared
        RETURN __FieldSetWa(wa, fldName,uValue)
    ENDIF
    LOCAL uObject := __MemVarGet(wa) AS USUAL
    RETURN IVarPut(uObject, fldName, uValue)


/// <exclude />
FUNCTION __AreaEval<T>(area AS USUAL, action AS @@Func<T>) AS T
    LOCAL curArea := RuntimeState.CurrentWorkarea AS DWORD
    LOCAL newArea := _Select( area ) AS DWORD
    LOCAL result  := DEFAULT(T) AS T
    IF newArea > 0
        RuntimeState.CurrentWorkarea := newArea

        TRY
            result := action()
        FINALLY
            RuntimeState.CurrentWorkarea := curArea
        END TRY
    ELSE
        THROW Error.VoDbError( EG_ARG, EDB_BADALIAS, __FUNCTION__, NAMEOF(area),1, <OBJECT>{area}  )
    ENDIF
    RETURN result


/// <exclude />
FUNCTION __AreaEval(area AS USUAL, action AS System.Action) AS LOGIC
    LOCAL curArea := RuntimeState.CurrentWorkarea AS DWORD
    LOCAL newArea := _Select( area ) AS DWORD
    LOCAL result  := FALSE AS LOGIC
    IF newArea > 0
        RuntimeState.CurrentWorkarea := newArea
        TRY
            action()
            result := TRUE
        FINALLY
            RuntimeState.CurrentWorkarea := curArea
        END TRY
    ELSE
        THROW Error.VoDbError( EG_ARG, EDB_BADALIAS, __FUNCTION__, NAMEOF(area),1, <OBJECT>{area}  )
    ENDIF
    RETURN result


/// <exclude />
[NeedsAccessToLocals(FALSE)];
FUNCTION __MemVarGet(cName AS STRING) AS USUAL
    RETURN XSharp.MemVar.Get(cName)


/// <exclude />
[NeedsAccessToLocals(FALSE)];
FUNCTION __MemVarGetSafe(cName AS STRING) AS USUAL
    RETURN XSharp.MemVar.GetSafe(cName)


/// <exclude />
[NeedsAccessToLocals(TRUE)];
FUNCTION __MemVarPut(cName AS STRING, uValue AS USUAL) AS USUAL
    RETURN XSharp.MemVar.Put(cName, uValue)


/// <exclude />
[NeedsAccessToLocals(FALSE)];
FUNCTION __VarGet(cName AS STRING) AS USUAL
    // first we try to access a field with this name
    VAR nPos := FieldPos(cName)
    LOCAL result AS USUAL
    IF  nPos > 0
        #pragma warnings (165, off) // result is not initialized
        VoDb.FieldGet(nPos, REF result)
        #pragma warnings (165, default)
        RETURN result
    ENDIF
    // Then a global
    IF Globals.Get(cName, OUT result)
        RETURN result
    ENDIF
    // And finally a memory variable
    RETURN __MemVarGet(cName)

/// <exclude />
[NeedsAccessToLocals(FALSE)];
FUNCTION __VarGetSafe(cName AS STRING) AS USUAL
    // first we try to access a field with this name
    IF FieldPos(cName) > 0
        RETURN __FieldGet(cName)
    ENDIF
    // Then a global
    IF Globals.Get(cName, OUT VAR result)
        RETURN result
    ENDIF
    // And finally a memory variable
    RETURN __MemVarGetSafe(cName)

/// <exclude />
[NeedsAccessToLocals(TRUE)];
FUNCTION __VarPut(cName AS STRING, uValue AS USUAL) AS USUAL
    // first we try to access a field with this name
    IF FieldPos(cName) > 0
        RETURN __FieldSet(cName, uValue)
    ENDIF
    // Then a global
    IF Globals.Put(cName, uValue)
        RETURN uValue
    ENDIF
    // And finally a memory variable
    RETURN __MemVarPut(cName, uValue)



    // ALIAS->(DoSomething())
    // is sometimes translated to
    // __pushWorkarea( alias ) ; DoSomething() ; __popWorkArea()
    // can also be come __AreaEval(..)

/// <exclude />
FUNCTION __pushWorkarea( alias AS USUAL ) AS VOID
    LOCAL newArea := @@Select( alias ) AS DWORD
    IF newArea > 0
        RuntimeState.PushCurrentWorkarea( newArea )
    ELSE
        THROW Error.VoDbError( EG_ARG, EDB_BADALIAS, <OBJECT>{ alias } )
    ENDIF
    RETURN

/// <exclude />
FUNCTION __popWorkarea() AS VOID
    RuntimeState.PopCurrentWorkarea()
    RETURN


/// <exclude />
FUNCTION __MemVarInit() AS INT STRICT
    RETURN XSharp.MemVar.InitPrivates(FALSE)


/// <exclude />
FUNCTION __MemVarInit(lFromRuntime AS LOGIC) AS INT STRICT
    RETURN XSharp.MemVar.InitPrivates(lFromRuntime)


/// <exclude />
FUNCTION __MemVarRelease(nLevel AS INT) AS LOGIC  STRICT
    RETURN XSharp.MemVar.ReleasePrivates(nLevel)



/// <exclude />
FUNCTION __MemVarDecl(name AS STRING, _priv AS LOGIC) AS VOID  STRICT
    XSharp.MemVar.Add(name, _priv)
    RETURN



/// <exclude />
FUNCTION __LocalPut(name AS STRING, uValue IN USUAL) AS VOID STRICT
    XSharp.MemVar.LocalPut(name, uValue)


/// <exclude />
FUNCTION __LocalsClear() AS VOID STRICT
    XSharp.MemVar.ClearLocals()


/// <exclude />
FUNCTION __LocalsUpdated() AS LOGIC STRICT
    RETURN XSharp.MemVar.LocalsUpdated()

/// <exclude />
FUNCTION __LocalGet(name AS STRING) AS USUAL STRICT
    RETURN XSharp.MemVar.LocalGet(name)


/// <exclude />
FUNCTION __UsualEnumerator(u IN USUAL) AS ARRAY
    IF u:IsArray
        RETURN (ARRAY) u
    ENDIF
    LOCAL o := __Usual.ToObject(u) AS OBJECT
    IF o IS System.Collections.IEnumerable VAR list
        LOCAL a AS ARRAY
        a := ArrayNew()
        FOREACH VAR oElement IN list
            AAdd(a, oElement)
        NEXT
        RETURN a
    ENDIF
    VAR error := __Usual.ConversionError("ARRAY",TYPEOF(ARRAY),u)
    error:Description := "The usual value cannot be enumerated because it does not contain an array"
    THROW error
