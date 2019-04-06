//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
// functions used by the compiler


/// <inheritdoc cref="M:XSharp.RuntimeState.StringCompare(System.String,System.String)" />
FUNCTION __StringCompare(strLHS AS STRING, strRHS AS STRING) AS INT
    RETURN RuntimeState.StringCompare(strLHS, strRHS)

    
    /// <summary>
    /// Compare 2 strings. This function is used by the compiler for string comparisons
    /// </summary>
    /// <param name="strLHS">The first string .</param>
    /// <param name="strRHS">The second string.</param>
    /// <returns>
    /// TRUE when the strings are equal, FALSE when they are not equal
    /// This function respects SetExact()
    /// </returns>
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
    
    /// <summary>
    /// Compare 2 strings. This function is used by the compiler for string comparisons
    /// </summary>
    /// <param name="strLHS">The first string .</param>
    /// <param name="strRHS">The second string.</param>
    /// <returns>
    /// TRUE when the strings are not equal, FALSE when they are equal
    /// This function respects SetExact()
    /// </returns>
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
    
    /// <summary>
    /// Remove leading and trailing spaces from a string.
    /// </summary>
    /// <param name="c">The string to be trimmed.</param>
    /// <returns>
    /// The original string without leading and trailing spaces
    /// </returns>
    // _FIELD->Name

FUNCTION __FieldGet( fieldName AS STRING ) AS USUAL
    LOCAL fieldpos := FieldPos( fieldName ) AS DWORD
    LOCAL ret := NULL AS OBJECT
    IF fieldpos == 0
        THROW Error.VODBError( EG_ARG, EDB_FIELDNAME, __FUNCTION__,  nameof(fieldName), 1, fieldName  )
    ELSE
         _DbThrowErrorOnFailure(__FUNCTION__, CoreDb.FieldGet( fieldpos, REF ret ))
    ENDIF
    RETURN ret
    
    
    // CUSTOMER->NAME
/// <exclude/>
FUNCTION __FieldGetWa( area AS USUAL, fieldName AS STRING ) AS USUAL
    // XBase defines that 'M' in 'M->Name' means a Memvar
    IF area:IsNil
        RETURN __FieldGet(fieldName)
    ENDIF
    IF area:IsString .and. ((string) area):ToUpper() == "M"
        RETURN __MemVarGet(fieldName)
    ENDIF
    LOCAL ret AS USUAL
    LOCAL newArea := _Select( area ) AS DWORD
    LOCAL curArea := RuntimeState.CurrentWorkarea AS DWORD
    IF newArea > 0
        RuntimeState.CurrentWorkarea := newArea
        TRY
            ret := __FieldGet( fieldName )
        FINALLY
            RuntimeState.CurrentWorkarea := curArea
        END TRY   
    ELSE
        THROW Error.VODBError( EG_ARG, EDB_BADALIAS, __FUNCTION__, nameof(area), 1, area  )
    ENDIF
    RETURN ret

    // _FIELD->Name := "Foo"
/// <exclude/>
FUNCTION __FieldSet( fieldName AS STRING, oValue AS USUAL ) AS USUAL
    LOCAL fieldpos := FieldPos( fieldName ) AS DWORD
    IF fieldpos == 0
        THROW Error.VODBError( EG_ARG, EDB_FIELDNAME, __FUNCTION__,  nameof(fieldName), 1, fieldName  )
    ELSE
        _DbThrowErrorOnFailure(__FUNCTION__, CoreDb.FieldPut( fieldpos, oValue))
    ENDIF
    // We return the original value to allow chained expressions
    RETURN oValue
    
    
    // CUSTOMER->Name := "Foo"
    // (nArea)->Name := "Foo"
/// <exclude/>
FUNCTION __FieldSetWa( area AS usual, fieldName AS STRING, uValue AS USUAL ) AS USUAL
    IF area:IsNil
        RETURN __FieldGet(fieldName)
    ENDIF
    IF area:IsString .and. ((string) area):ToUpper() == "M"
        RETURN __MemVarGet(fieldName)
    ENDIF
    LOCAL newArea := _Select( area ) AS DWORD
    LOCAL curArea := RuntimeState.CurrentWorkarea AS DWORD
    IF newArea > 0
        RuntimeState.CurrentWorkarea := newArea
        
        TRY
            __FieldSet( fieldName, uValue )
        FINALLY
            RuntimeState.CurrentWorkarea := curArea
        END TRY   
    ELSE
        THROW Error.VODBError( EG_ARG, EDB_BADALIAS, __FUNCTION__, nameof(area),1, area  )
    ENDIF
    // Note: must return the same value passed in, to allow chained assignment expressions
    RETURN uValue

FUNCTION __AreaEval<T>(area as usual, action as @@Func<T>) as T
    LOCAL newArea := _Select( area ) AS DWORD
    LOCAL curArea := RuntimeState.CurrentWorkarea AS DWORD
    LOCAL result  := default(T) as T
    IF newArea > 0
        RuntimeState.CurrentWorkarea := newArea
        
        TRY
            result := action()
        FINALLY
            RuntimeState.CurrentWorkarea := curArea
        END TRY   
    ELSE
        THROW Error.VODBError( EG_ARG, EDB_BADALIAS, __FUNCTION__, nameof(area),1, area  )
    ENDIF
    return result
    
    
    // MEMVAR myName
    // ? MyName
/// <exclude/>
FUNCTION __MemVarGet(cName AS STRING) AS USUAL
    RETURN XSharp.MemVar.Get(cName)
    
    
    // MEMVAR myName
    // MyName := "NewValue"
/// <exclude/>
FUNCTION __MemVarPut(cName AS STRING, uValue AS USUAL) AS USUAL
    RETURN XSharp.MemVar.Put(cName, uValue)
    

/// <exclude/>
FUNCTION __VarGet(cName AS STRING) AS USUAL
    IF FieldPos(cName) > 0
        RETURN __FieldGet(cName)
    ENDIF
    RETURN __MemVarGet(cName)
    
/// <exclude/>
FUNCTION __VarPut(cName AS STRING, uValue AS USUAL) AS USUAL
    IF FieldPos(cName) > 0
        RETURN __FieldSet(cName, uValue)
    ENDIF
    RETURN __MemVarPut(cName, uValue)
    
    
    
    // ALIAS->(DoSomething())
    // is translated to
    // __pushWorkarea( alias ) ; DoSomething() ; __popWorkArea()
    
/// <exclude/>
FUNCTION __pushWorkarea( alias AS USUAL ) AS VOID
    LOCAL newArea := SELECT( alias ) AS DWORD
    IF newArea > 0
        RuntimeState.PushCurrentWorkarea( newArea )
    ELSE
        THROW Error.VODBError( EG_ARG, EDB_BADALIAS, { alias } )
    ENDIF
    RETURN
    
    // This is used by the -> operator to restore the previous workarea
/// <exclude/>
FUNCTION __popWorkarea() AS VOID
    RuntimeState.PopCurrentWorkarea()
    RETURN
    

// this is used to initialize the privates stack at a certain level
/// <exclude/>
FUNCTION __MemVarInit() AS INT STRICT 
	RETURN XSharp.MemVar.InitPrivates()   

// this is used to release the privates stack
/// <exclude/>
FUNCTION __MemVarRelease(nLevel AS INT) AS LOGIC  STRICT
	RETURN XSharp.MemVar.ReleasePrivates(nLevel)	


// this is used to declare a private or public variable. The variable is initialized with NIL.
/// <exclude/>
FUNCTION __MemVarDecl(name AS STRING, _priv AS LOGIC) AS VOID  STRICT
	XSharp.MemVar.Add(name, _priv)
	RETURN 
