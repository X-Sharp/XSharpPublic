//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

// Use UDC below to standardize the NULL checks for the array arguments
#command ARRAYNOTNULL <aArray> => IF <aArray> == NULL ; THROW Error.NullArgumentError(__FUNCTION__,nameof(<aArray>), 1) ; ENDIF
#command ARRAYNULL <aArray>    => IF <aArray> == NULL ; return <aArray> ; ENDIF

INTERNAL STATIC CLASS ArrayHelpers

    STATIC METHOD AScan<T>(aTarget AS __ArrayBase<T>, element  AS T , nStart AS LONG, nCount AS LONG) AS DWORD WHERE T IS NEW()
        LOCAL nItem AS LONG
        LOCAL nLen  AS LONG
        ARRAYNOTNULL aTarget
        nLen := (INT) aTarget:Length
        FOR nItem := nStart TO nLen
            IF object.Equals(aTarget[ nItem], element)
                RETURN (DWORD) nItem
            ENDIF
            nCount -= 1  
            IF nCount == 0
                EXIT
            ENDIF
        NEXT
        RETURN 0
        
    STATIC METHOD AScan<T>(aTarget AS __ArrayBase<T>, bAction AS @@Func<T, LOGIC> , nStart AS LONG, nCount AS LONG) AS DWORD WHERE T IS NEW()
        LOCAL nItem AS LONG
        LOCAL nLen  AS LONG
        ARRAYNOTNULL aTarget
        nLen := (INT) aTarget:Length
        FOR nItem := nStart TO nLen
            LOCAL oElement := aTarget[ nItem] AS T
            IF bAction(oElement)
                RETURN  (DWORD) nItem
            ENDIF
            nCount -= 1
            IF nCount == 0
                EXIT
            ENDIF
        NEXT
        RETURN 0
        
        
    STATIC METHOD AScan( aTarget AS USUAL, x AS USUAL, uStart AS USUAL, uCount AS USUAL, lExact AS LOGIC ) AS DWORD
        LOCAL nSize		AS DWORD
        IF ! ArrayHelpers.ValidateArrayParams(REF aTarget, REF uStart, REF uCount, OUT nSize)
            RETURN 0
        ENDIF
        LOCAL a := aTarget AS ARRAY
        LOCAL nStart := uStart	AS DWORD
        LOCAL nCount := uCount	AS DWORD
        LOCAL nRet := 0			AS DWORD
        LOCAL i					AS DWORD
        LOCAL cb				AS CODEBLOCK
        IF IsCodeBlock( x )
            cb := x
        ELSE
            IF lExact
                cb   := {|element| element == x } 
            ELSE
                cb   := {|element| element = x } 
            ENDIF
        ENDIF
        IF nStart < nSize
            FOR i := nStart UPTO nSize
                IF nCount > 0
                    IF Eval( cb, a[i] )
                        nRet := i
                        EXIT
                    ENDIF
                    nCount--
                ELSE
                    EXIT
                ENDIF
            NEXT
        ELSE
            FOR i := nStart DOWNTO nSize
                IF nCount > 0
                    IF Eval( cb, a[i] )
                        nRet := i
                        EXIT
                    ENDIF
                    nCount--
                ELSE
                    EXIT
                ENDIF
            NEXT
        ENDIF
        
        RETURN nRet
        
    STATIC METHOD AScanBin(cFuncName AS STRING, a AS ARRAY, seekVal AS USUAL, lExact AS LOGIC ) AS DWORD
        LOCAL dwLow        AS DWORD
        LOCAL dwHigh       AS DWORD
        LOCAL x            AS DWORD
        LOCAL dwRet        AS DWORD
        LOCAL iSave        AS DWORD
        LOCAL lIsCodeBlock AS LOGIC
        LOCAL iCBRet       AS USUAL 
        LOCAL iComp        AS INT
        LOCAL cb			  AS CODEBLOCK
        
        dwLow  := 1
        dwHigh := (DWORD) ALen(a)
        iSave := dwRet := 0
        iComp := 0
        lIsCodeblock := IsCodeBlock( seekVal )
        IF lIsCodeBlock
            cb := seekVal
        ELSEIF lExact
            cb := {|element| IIF( element == seekVal , 0, IIF( seekVal < element, -1, 1 ) ) }
        ELSE
            cb := {|element| IIF( element = seekVal , 0, IIF( seekVal < element, -1, 1 ) ) }
        ENDIF
        
        DO WHILE dwLow <= dwHigh
            x := dwLow + ( ( dwHigh - dwLow ) / 2 )
            
            IF iSave == x
                IF dwHigh - dwLow == 1
                    x++
                    dwLow++
                ELSE
                    EXIT
                ENDIF
            ENDIF
            
            iSave := x
            IF lIsCodeBlock      
                iCBRet := Eval( cb, a[x] )
                IF ! iCBRet:IsNumeric
                    THROW Error.DataTypeError(cFuncName, "Return value of Codeblock", 0)
                ELSE
                    iComp := iCBRet
                ENDIF
            ELSE
                iComp := Eval( cb, a[x] )
            ENDIF
            IF iComp < 0
                dwHigh := x
            ELSEIF iComp > 0
                dwLow := x
            ELSE
                dwRet := x
                x--
                
                DO WHILE dwLow <= x
                
                    IF lIsCodeBlock      
                        iCBRet := Eval( cb, a[x] )
                        IF ! iCBRet:IsNumeric
                            THROW Error.DataTypeError(cFuncName, "Return value of Codeblock", 0)
                        ELSE
                            iComp := iCBRet
                        ENDIF
                    ELSE
                        iComp := Eval( cb, a[x] )
                    ENDIF
                    
                    IF iComp != 0
                        EXIT
                    ENDIF
                    
                    dwRet := x
                    x--
                ENDDO
                dwLow := dwHigh + 1
            ENDIF
        ENDDO
        
        RETURN dwRet
        
        
        
    STATIC METHOD AEval(aArray AS ARRAY, cbBlock AS ICodeblock, nStart AS DWORD, nCount AS DWORD, bUpdateArray AS CONST LOGIC )  AS ARRAY
        LOCAL elements := ALen(aArray) AS DWORD
        LOCAL last   AS DWORD
        LOCAL result AS USUAL
        LOCAL x      AS DWORD
        IF elements == 0
            RETURN aArray
        ENDIF
        ARRAYNOTNULL aArray
        IF nStart == 0
            THROW  Error.ArgumentError( __FUNCTION__, nameof(nStart), 3, __CavoStr( VOErrors.ArgCannotBeZero ), { nStart } )
        ENDIF
        
        IF nCount != 0
        
            IF nStart < 0
                nStart := elements + 1 + nStart
            ENDIF
            
            IF nCount > 0
            
                last := Math.Min( nStart + ( nCount - 1 ), elements )
                
                FOR x := nStart UPTO last
                    result := Eval( cbBlock, aArray[x], x )
                    
                    IF ( bUpdateArray )
                        aArray[x] := result
                    ENDIF
                NEXT
            ELSE
            
                last := Math.Max( 1, nStart + nCount + 1 )
                
                FOR x := nStart DOWNTO last
                    result := Eval( cbBlock, aArray[x], x )
                    
                    IF ( bUpdateArray )
                        aArray[x] := result
                    ENDIF
                NEXT
            ENDIF
        ENDIF
        
        RETURN aArray
        
    STATIC METHOD AEvalCheckArgs(aArray AS ARRAY, cb AS ICodeblock, iStart REF USUAL, iCount REF USUAL,cFuncName AS STRING) AS VOID
        IF aArray == NULL_ARRAY
            RETURN
        ENDIF
        IF cb == NULL
            THROW Error.NullArgumentError( cFuncName, NAMEOF(cb),2)
        ENDIF
        DEFAULT( REF iStart, 1)
        DEFAULT( REF iCount, ALen(aArray))
        IF ! iStart:IsNumeric
            THROW Error.ArgumentError( cFuncName, NAMEOF(iStart), 3 , <OBJECT>{iStart})
        ENDIF
        IF ! iCount:IsNumeric
            THROW Error.ArgumentError( cFuncName, NAMEOF(iCount), 4 , <OBJECT>{iCount})
        ENDIF
        RETURN
        
    STATIC METHOD ValidateArrayParams(aTarget REF USUAL, nStart REF USUAL,nCount REF USUAL, nSize OUT DWORD) AS LOGIC
        nSize := 0
        IF aTarget:IsArray
            nSize := ALen( (ARRAY) aTarget )
            IF nSize == 0
                RETURN FALSE
            ENDIF
        ELSE
            RETURN FALSE
        ENDIF
        
        IF ! nCount:IsNumeric
            IF ! nStart:IsNumeric
                nStart := 1
            END IF
            nCount := ALen(aTarget) - nStart + 1
        ELSE
            IF nCount >= 0
                IF ! nStart:IsNumeric
                    nStart := 1
                ENDIF
                IF nStart > nSize
                    RETURN FALSE
                END IF
            ELSE
                IF ! nStart:IsNumeric
                    nStart := ALen(aTarget)
                ENDIF
                nSize := 1
                nCount := - nCount
                IF nStart < nSize
                    RETURN FALSE
                END IF
            END IF
        END IF
        RETURN TRUE
        
        END CLASS
        
        
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/arraycreate/*" /> 
FUNCTION ArrayCreate(dwElements AS DWORD) AS ARRAY 
    RETURN __Array{dwElements, TRUE}
    
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/arraycreate/*" />
    /// <typeparam name="T">The type of the array elements</typeparam>
FUNCTION ArrayCreate<T>(dwElements AS DWORD) AS __ArrayBase<T> WHERE T IS NEW()
    RETURN __ArrayBase<T>{dwElements}
    
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/arrayinit/*" /> 
FUNCTION ArrayInit(wElements AS DWORD, avalues REF USUAL[]) AS ARRAY 
    LOCAL aTemp AS ARRAY
    LOCAL x AS DWORD
    
    IF wElements > (DWORD) aValues:Length
        THROW Error.ArgumentError( __FUNCTION__, NAMEOF(wElements), "Element too big")
    ENDIF
    
    aTemp := ArrayNew(aValues:Length)
    FOR x := 1 UPTO aValues:Length
        aTemp [x] := aValues[x] 
    NEXT
    RETURN aTemp
    
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/aadd/*" /> 
FUNCTION AAdd(aTarget AS ARRAY,uNewElement AS USUAL) AS USUAL
    ARRAYNOTNULL aTarget
    RETURN AAdd<USUAL>(aTarget, uNewElement)
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/aadd/*" />
    /// <typeparam name="T">The type of the array elements</typeparam>
FUNCTION AAdd<T>(aTarget AS __ArrayBase<T>,uNewElement AS T) AS T WHERE T IS NEW()
    ARRAYNOTNULL aTarget
    aTarget:Add(uNewElement)
    RETURN uNewElement 	
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/aclone/*" /> 
FUNCTION AClone(aSource AS ARRAY) AS ARRAY
    IF aSource == NULL_ARRAY
        RETURN aSource
    END IF
    RETURN (ARRAY) aSource:Clone()
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/aclone/*" />
    /// <typeparam name="T">The type of the array elements</typeparam>
FUNCTION AClone<T>(aSource AS __ArrayBase<T>) AS __ArrayBase<T> WHERE T IS NEW()
    IF aSource == NULL
        RETURN aSource
    END IF
    RETURN  aSource:Clone()
    
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/acloneshallow/*" /> 
FUNCTION ACloneShallow(aSource AS ARRAY) AS ARRAY
    IF aSource == NULL_ARRAY
        RETURN aSource
    END IF
    RETURN (ARRAY) aSource:CloneShallow()
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/acloneshallow/*" />
    /// <typeparam name="T">The type of the array elements</typeparam>
FUNCTION ACloneShallow<T>(aSource AS __ArrayBase<T>) AS __ArrayBase<T> WHERE T IS NEW()
    IF aSource == NULL
        RETURN aSource
    END IF
    RETURN aSource:Clone()
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/adel/*" /> 
FUNCTION ADel(aTarget AS ARRAY,dwPosition AS DWORD) AS ARRAY
    ARRAYNOTNULL aTarget
    aTarget:Delete((INT) dwPosition)
    RETURN aTarget
    
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/adel/*" />
    /// <typeparam name="T">The type of the array elements</typeparam>
FUNCTION ADel<T>(aTarget AS __ArrayBase<T>,dwPosition AS DWORD) AS __ArrayBase<T> WHERE T IS NEW()
    ARRAYNOTNULL aTarget
    aTarget:Delete((INT) dwPosition)   
    RETURN aTarget
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/adel/*" /> 
FUNCTION ATrueDel(aTarget AS ARRAY,dwPosition AS DWORD) AS ARRAY
    ARRAYNOTNULL aTarget
    aTarget:RemoveAt((INT) dwPosition)  
    RETURN aTarget
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/adel/*" />
    /// <typeparam name="T">The type of the array elements</typeparam>
FUNCTION ATrueDel<T>(aTarget AS __ArrayBase<T>,dwPosition AS DWORD) AS __ArrayBase<T> WHERE T IS NEW()
    ARRAYNOTNULL aTarget
    aTarget:RemoveAt((INT) dwPosition)  
    RETURN aTarget
    
    /// <summary>Calculate the # of dimensions in an array</summary>
        /// <param name="a"></param>
    /// <returns>Number of dimensions in an array</returns>
FUNCTION ADim(a AS ARRAY) AS DWORD
    LOCAL dwDims AS DWORD
    dwDims := 1
    DO WHILE a != NULL_ARRAY 
        IF ALen(a) > 0 
            IF IsArray(a[1])
                dwDims += 1 
                a := a[1]
            ELSE
                EXIT
            ENDIF
        ELSE
            EXIT
        ENDIF
    ENDDO
    RETURN dwDims
    
    /// <summary>Calculate a string that represents the dimensions in an array</summary>
        /// <param name="a"></param>
    /// <returns>String that displays the dimensions in an array</returns>
FUNCTION ADimPic(a AS ARRAY) AS STRING
RETURN repl("[]", aDim(a))


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ains/*" /> 
FUNCTION AIns(aTarget AS ARRAY,dwPosition AS DWORD) AS ARRAY 
    ARRAYNOTNULL aTarget
    aTarget:Insert((INT) dwPosition) 
    RETURN aTarget
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ains/*" />
    /// <typeparam name="T">The type of the array elements</typeparam>
FUNCTION AIns<T>(aTarget AS __ArrayBase<T>,dwPosition AS DWORD) AS __ArrayBase<T> WHERE T IS NEW()
    ARRAYNOTNULL aTarget
    aTarget:Insert((INT) dwPosition) 
    RETURN aTarget
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/alen/*" />
    /// <typeparam name="T">The type of the array elements</typeparam>
FUNCTION ALen<T>(aTarget AS __ArrayBase<T>) AS DWORD WHERE T IS NEW()
    IF aTarget != NULL
        RETURN aTarget:Length
    ELSE
        RETURN 0
    ENDIF
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/alen/*" /> 
FUNCTION ALen(aTarget AS ARRAY) AS DWORD 
    IF aTarget != NULL
        RETURN aTarget:Length
    ELSE
        RETURN 0
    ENDIF
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/arraydeprotect/*" />
    /// <typeparam name="T">The type of the array elements</typeparam>
FUNCTION ArrayDeProtect<T>(aTarget AS __ArrayBase<T>) AS LOGIC WHERE T IS NEW()
    ARRAYNOTNULL aTarget
    RETURN aTarget:Lock(FALSE)
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/arraydeprotect/*" /> 
FUNCTION ArrayDeProtect(aTarget AS ARRAY) AS LOGIC 
    ARRAYNOTNULL aTarget
    RETURN aTarget:Lock(FALSE)
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/arrayget/*" /> 
FUNCTION ArrayGet(aTarget AS ARRAY,dwElement AS DWORD) AS USUAL
    ARRAYNOTNULL aTarget
    RETURN aTarget:__GetElement( (INT) dwElement-1)
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/arrayget/*" />
    /// <typeparam name="T">The type of the array elements</typeparam>
FUNCTION ArrayGet<T>(aTarget AS __ArrayBase<T>,dwElement AS DWORD) AS T WHERE T IS NEW()
    ARRAYNOTNULL aTarget
    RETURN aTarget:__GetElement( (INT) dwElement-1)
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/arrayprotect/*" /> 
FUNCTION ArrayProtect(aTarget AS ARRAY) AS LOGIC
    ARRAYNOTNULL aTarget
    RETURN aTarget:Lock(TRUE)
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/arrayprotect/*" />
    /// <typeparam name="T">The type of the array elements</typeparam>
FUNCTION ArrayProtect<T>(aTarget AS __ArrayBase<T>) AS LOGIC WHERE T IS NEW()
    ARRAYNOTNULL aTarget
    RETURN aTarget:Lock(TRUE)	
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/arrayput/*" />
    /// <typeparam name="T">The type of the array elements</typeparam>
FUNCTION ArrayPut<T>(aTarget AS __ArrayBase<T>,dwElement AS DWORD,uValue AS T) AS T WHERE T IS NEW()
    ARRAYNOTNULL aTarget
    aTarget:__SetElement(uValue, (INT)dwElement -1)
    RETURN uValue
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/arrayput/*" /> 
FUNCTION ArrayPut(aTarget AS ARRAY,dwElement AS DWORD,uValue AS USUAL) AS USUAL
    ARRAYNOTNULL aTarget
    aTarget:__SetElement(uValue, (INT)dwElement -1)
    RETURN uValue
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/arraystore/*" /> 
FUNCTION ArrayStore(aSource AS ARRAY,Buff AS USUAL PTR,dwLen AS DWORD) AS DWORD
    LOCAL i AS DWORD
    LOCAL nLen AS DWORD
    nLen := ALen(aSource)
    dwLen := Math.Min(dwLen, nLen)
    FOR i := 1 TO dwLen
        buff[i] := aSource[i]
    NEXT
    RETURN dwLen
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/arraystore/*" />
    /// <typeparam name="T">The type of the array elements</typeparam>
FUNCTION ArrayStore<T>(aSource AS __ArrayBase<T>,Buff AS T PTR,dwLen AS DWORD) AS DWORD WHERE T IS NEW()
    ARRAYNOTNULL aSource
    LOCAL i, nLen AS DWORD
    nLen := aSource:Length
    dwLen := Math.Min(dwLen, nLen)
    FOR i := 1 TO dwLen
        buff[i] := aSource[(INT) i]
    NEXT
    RETURN dwLen
    
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/arrayswap/*" /> 
FUNCTION ArraySwap(aTarget AS ARRAY,dwElement AS DWORD,uNewValue AS USUAL) AS USUAL
    ARRAYNOTNULL aTarget
    RETURN aTarget:Swap((INT) dwElement, uNewValue)
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/arrayswap/*" />
    /// <typeparam name="T">The type of the array elements</typeparam>
FUNCTION ArraySwap<T>(aTarget AS __ArrayBase<T>,dwElement AS DWORD,uNewValue AS T) AS T  WHERE T IS NEW()
    ARRAYNOTNULL aTarget
    RETURN aTarget:Swap((INT) dwElement, uNewValue)
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ascan/*" /> 
FUNCTION AScan(aTarget AS ARRAY, uSearch AS USUAL,nStart := NIL AS USUAL,nCount := NIL AS USUAL) AS DWORD
    ARRAYNOTNULL aTarget
    RETURN ArrayHelpers.AScan( aTarget, uSearch, nStart, nCount, SetExact()) 
    
    
    /// <inheritdoc cref='M:XSharp.RT.Functions.AScan(XSharp.__Array,XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)'/>
FUNCTION AScan(aTarget AS ARRAY, uSearch AS USUAL,nStart AS USUAL) AS DWORD 
    ARRAYNOTNULL aTarget
    RETURN ArrayHelpers.AScan( aTarget, uSearch, nStart, NIL, SetExact()) 
    
    
    /// <inheritdoc cref='M:XSharp.RT.Functions.AScan(XSharp.__Array,XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)'/>
FUNCTION AScan(aTarget AS ARRAY, uSearch AS USUAL) AS DWORD 
    ARRAYNOTNULL aTarget
    RETURN ArrayHelpers.Ascan( aTarget, uSearch, 1, NIL, SetExact()) 
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ascanexact/*" /> 
FUNCTION AScanExact( aTarget AS ARRAY, uSearch AS USUAL, nStart := NIL AS USUAL, nCount := NIL AS USUAL) AS DWORD
    ARRAYNOTNULL aTarget
    RETURN ArrayHelpers.Ascan( aTarget, uSearch, nStart, nCount, TRUE )
    
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ascanexact/*" /> 
FUNCTION AScanExact( aTarget AS ARRAY, uSearch AS USUAL, nStart AS USUAL) AS DWORD 
    ARRAYNOTNULL aTarget
    RETURN ArrayHelpers.Ascan( aTarget, uSearch, nStart, NIL, TRUE )
    
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ascanexact/*" /> 
FUNCTION AScanExact( aTarget AS ARRAY, uSearch AS USUAL) AS DWORD 
    ARRAYNOTNULL aTarget
    RETURN ArrayHelpers.Ascan( aTarget, uSearch, 1, NIL, TRUE )
    
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ascanbin/*" />
FUNCTION AScanBin(aTarget AS ARRAY,uSearch AS USUAL) AS DWORD
    ARRAYNOTNULL aTarget
    RETURN ArrayHelpers.AScanBin( "AscanBin" , aTarget, uSearch, FALSE )
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ascanbinexact/*" />
FUNCTION AScanBinExact(aTarget AS ARRAY,uSearch AS USUAL) AS DWORD
    ARRAYNOTNULL aTarget
    RETURN ArrayHelpers.AScanBin( "AscanBin" , aTarget, uSearch, TRUE )
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ascan/*" /> 
    /// <typeparam name="T">The type of the array elements</typeparam>
FUNCTION AScan<T>(aTarget AS __ArrayBase<T>, uSearch AS T) AS DWORD WHERE T IS NEW()
    ARRAYNOTNULL aTarget
    RETURN ArrayHelpers.Ascan( aTarget, uSearch,1, (INT) aTarget:Length) 
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ascan/*" /> 
        /// <typeparam name="T">The type of the array elements</typeparam>
    /// <param name="act">A lambda expression that will be evaluated for every element in the array.</param>
FUNCTION AScan<T>(aTarget AS __ArrayBase<T>, act AS @@Func<T,LOGIC>) AS DWORD WHERE T IS NEW()
    ARRAYNOTNULL aTarget
    RETURN ArrayHelpers.Ascan( aTarget, act,1, (INT) aTarget:Length) 
    
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ascan/*" /> 
    /// <typeparam name="T">The type of the array elements</typeparam>
FUNCTION AScan<T>(aTarget AS __ArrayBase<T>, uSearch AS T, nStart AS LONG) AS DWORD WHERE T IS NEW()
    ARRAYNOTNULL aTarget
    RETURN ArrayHelpers.Ascan( aTarget, uSearch, nStart, (INT) aTarget:Length- nStart +1) 
    
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ascan/*" /> 
        /// <typeparam name="T">The type of the array elements</typeparam>
    /// <param name="act">A lambda expression that will be evaluated for every element in the array.</param>
FUNCTION AScan<T>(aTarget AS __ArrayBase<T>, act AS @@Func<T,LOGIC>, nStart AS LONG) AS DWORD WHERE T IS NEW()
    ARRAYNOTNULL aTarget
    RETURN ArrayHelpers.Ascan( aTarget, act, nStart, (INT) aTarget:Length - nStart +1) 
    
    
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ascan/*" /> 
    /// <typeparam name="T">The type of the array elements</typeparam>
FUNCTION AScan<T>(aTarget AS __ArrayBase<T>, uSearch AS T, nStart AS LONG, nCount AS LONG) AS DWORD WHERE T IS NEW()
    ARRAYNOTNULL aTarget
    RETURN ArrayHelpers.Ascan( aTarget, uSearch, nStart, nCount) 
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ascan/*" /> 
    /// <typeparam name="T">The type of the array elements</typeparam>
FUNCTION AScan<T>(aTarget AS __ArrayBase<T>, act AS @@Func<T,LOGIC>, nStart AS LONG, nCount  AS LONG) AS DWORD WHERE T IS NEW()
    ARRAYNOTNULL aTarget
    RETURN ArrayHelpers.Ascan( aTarget, act, nStart, nCount) 
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/asize/*" /> 
FUNCTION ASize(aTarget AS ARRAY,dwLength AS DWORD) AS ARRAY
    ARRAYNOTNULL aTarget
    aTarget:Resize((INT) dwLength) 
    RETURN aTarget  
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/asize/*" /> 
    /// <typeparam name="T">The type of the array elements</typeparam>
FUNCTION ASize<T>(aTarget AS __ArrayBase<T>,dwLength AS DWORD) AS __ArrayBase<T> WHERE T IS NEW()
    ARRAYNOTNULL aTarget
    aTarget:Resize((INT) dwLength) 
    RETURN aTarget  
    
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/atail/*" /> 
FUNCTION ATail(aTarget AS ARRAY) AS USUAL
    ARRAYNOTNULL aTarget
    RETURN aTarget:Tail()
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/atail/*" /> 
    /// <typeparam name="T">The type of the array elements</typeparam>
FUNCTION ATail<T>(aTarget AS __ArrayBase<T>) AS T WHERE T IS NEW()
    ARRAYNOTNULL aTarget
    RETURN aTarget:Tail()
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/acopy/*" /> 
FUNCTION ACopy(aSource ,aTarget ,nStart ,nCount ,nTargetPos ) AS ARRAY CLIPPER
    LOCAL sourceLen  AS DWORD
    LOCAL start AS DWORD
    LOCAL count AS DWORD
    IF !aSource:IsArray
        THROW Error.ArgumentError( __FUNCTION__, NAMEOF(aSource), 1, <OBJECT>{ aSource } )
    ENDIF
    
    IF !aTarget:IsArray
        THROW Error.ArgumentError( __FUNCTION__, NAMEOF(aTarget), 2, <OBJECT>{ aTarget } )
    ENDIF
    start := 1
    sourceLen  := ALen(aSource)
    IF pCount() > 2
        IF nStart:IsNumeric
            start := nStart
        ELSE
            THROW Error.ArgumentError( __FUNCTION__, NAMEOF(nStart), 3, <OBJECT>{ nStart } )
        ENDIF
        IF start < 0
            start := sourceLen
        ELSE
            start := Math.Min(start, sourceLen)
        ENDIF
    ENDIF
    IF pCount() > 3
        IF nCount:IsNumeric
            count := nCount
        ELSE
            THROW Error.ArgumentError( __FUNCTION__, NAMEOF(nCount), 4, <OBJECT>{ nCount } )
        ENDIF
        IF count > 0
            sourceLen := Math.Min(sourceLen, start+count-1)
        ELSE
            sourceLen := Math.Max(1, start+count-1)
        ENDIF
    ENDIF
    LOCAL offSet		:= 1 AS DWORD
    LOCAL targetLen	:= ALen(aTarget) AS DWORD
    IF pCount() > 4
        IF nTargetPos:IsNumeric
            offSet := nTargetPos
            offSet := Math.Min( offSet, targetLen )
            offSet := Math.Max( 1, offSet )
        ELSE
            THROW Error.ArgumentError( __FUNCTION__, NAMEOF(nTargetPos), 5, <OBJECT>{ nTargetPos } )
        ENDIF
    ENDIF
    XSharp.__Array.Copy(aSource, aTarget, start, sourceLen, offSet, targetLen)
    
    RETURN aTarget   
    
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/afill/*" /> 
FUNCTION AFill(aTarget AS ARRAY,uValue := NIL AS USUAL, nStart := NIL AS USUAL, nCount := NIL AS USUAL) AS ARRAY 
    // The behavior of AFill() in VO is different than what is descibed in the help file
    // - if start <= 0 throws an error
    // - if start == NIL, then start becomes 1
    // - if count < 0 then it does nothing, unless start == nil, in which case start becomes 1 and count becomes 1, too (yeah, crap!)
    // - if count == nil, then it fills from start to lenght of array
    
    // warning, with the current definition of the function, it is not possible for the user to omit the start param
    LOCAL nLen := ALen( aTarget ) AS DWORD
    IF nLen > 0
    
        LOCAL lStartWasNil := FALSE AS LOGIC
        IF nStart == NIL
            nStart := 1
            lStartWasNil := TRUE
        ENDIF
        
        IF nStart > nLen .OR. nStart <= 0
            THROW Error.BoundError( "AFill", nameof(nStart), 3, <OBJECT>{ nStart } )
        ENDIF
        IF nCount == NIL
            nCount := (INT)nLen - nStart + 1
        ELSEIF nCount > 0
            // VO does not throw an error if count is longer than the array
            IF nStart + nCount - 1 > nLen
                nCount := (INT)nLen - nStart + 1
            END IF
        ELSE
            IF lStartWasNil
                nCount := 1
            ELSE
                RETURN aTarget
            END IF
        END IF
        
        FOR LOCAL x := nStart AS INT UPTO nStart + nCount - 1
            aTarget[(DWORD) x] := uValue
        NEXT
    ENDIF
    RETURN aTarget
    
    
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/arraybuild/*" /> 
FUNCTION ArrayBuild() AS ARRAY
    RETURN ARRAY{}
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/arraynew/*" /> 
FUNCTION ArrayNew() AS ARRAY
    RETURN ArrayNew(0)
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/arraynew/*" />  
FUNCTION ArrayNew(wElementList PARAMS USUAL[]) AS ARRAY
    LOCAL aDimInt AS INT[]
    LOCAL i AS INT
    aDimInt := INT[]{wElementList:Length}
    FOR i := 1 TO wElementList:Length
        aDimInt[i] := (INT) wElementList[i]
    NEXT
    RETURN __Array.ArrayCreate(aDimInt)
    
    
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/arraynew/*" /> 
    /// <typeparam name="T">The type of the array elements</typeparam>
FUNCTION ArrayNew<T>(wElementList AS DWORD) AS __ArrayBase<T> WHERE T IS NEW()
    RETURN __ArrayBase<T>{wElementList}
    
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/arraynew/*" /> 
    /// <typeparam name="T">The type of the array elements</typeparam>
FUNCTION ArrayNew<T>(wElementList AS INT) AS __ArrayBase<T> WHERE T IS NEW()
    RETURN __ArrayBase<T>{(DWORD) wElementList}
    
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/areplicate/*" /> 
FUNCTION AReplicate(xFill AS USUAL,nElements AS DWORD) AS ARRAY
    VAR a:= __Array{nElements, TRUE}
    LOCAL i AS DWORD
    FOR i := 1 UPTO nElements
        a[i] := xFill
    NEXT
    RETURN a
    
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/asort/*" /> 
FUNCTION ASort(aTarget AS ARRAY, nStart := NIL AS USUAL,nCount := NIL AS USUAL,cbOrder := NIL AS USUAL) AS ARRAY 
    LOCAL nLen AS DWORD
    DEFAULT( REF nStart, 1 )
    
    nLen := ALen(aTarget) 
    IF nLen == 0 // Let it execute if nLen == 1, maybe the codeblock is important to be executed in this case for some (user) reason
        RETURN aTarget
    END IF
    
    EnforceNumeric( nStart )
    DEFAULT( REF nCount, nLen - nStart + 1 )
    EnforceNumeric( nCount )
    
    // Note: ASort() in VO accepts arguments out of bounds and translates them this way:
    IF nStart <= 0
        nStart := 1
    ELSEIF nStart > nLen
        RETURN aTarget
    END IF
    
    IF nCount <= 0 .OR. nStart + nCount - 1 > nLen
        nCount := nLen - nStart + 1
    ENDIF
    
    /*	IF startIndex < 1 .or. nStart > nLen
    THROW Error.ArgumentError( __FUNCTION__, NAMEOF(nStart), 2, <OBJECT>{ nStart } )
    ENDIF 
    IF nCount + nStart > ALen((ARRAY)aArray)+1
    THROW Error.ArgumentError( __FUNCTION__, NAMEOF(nCount), 3, <OBJECT>{ nCount } )
    ENDIF */
    
    
    IF cbOrder != NIL
        VAR ok := cbOrder:IsCodeBlock
        IF ok
            VAR cb := (CODEBLOCK) cbOrder
            ok := cb:PCount() >= 2
            IF ! Ok
                THROW Error.ArgumentError( __FUNCTION__, nameof(cbOrder), "ASort Codeblock must have at least 2 arguments" )
            ENDIF
        ELSE
            THROW Error.ArgumentError( __FUNCTION__, nameof(cbOrder), 4, <OBJECT>{ cbOrder } )
        ENDIF
        
    ENDIF
    
    
    IF cbOrder == NIL
        aTarget:Sort( nStart, nCount, NULL ) // this uses __Usual.ICompareTo()
    ELSE
        aTarget:Sort( nStart, nCount, ArraySortComparer{ cbOrder } )
    ENDIF   
    
    RETURN aTarget
    
    // This wraps a codeblock and provides an IComparer implementation so
    // we can use ArrayList:Sort() with a codeblock.
    
INTERNAL STRUCTURE ArraySortComparer  IMPLEMENTS System.Collections.Generic.IComparer<USUAL> 

    PRIVATE _cb AS ICodeblock
    
    CONSTRUCTOR( cb AS ICodeblock)
        _cb := cb
        RETURN
        
    METHOD Compare( x AS USUAL, y AS USUAL ) AS INT
        IF x == y
            RETURN 0
        ENDIF
        LOCAL u AS USUAL
        u := _cb:EvalBlock( x, y )
        RETURN IIF (  (LOGIC) u , -1, 1 ) 
        
        
        END STRUCTURE
        
INTERNAL STRUCTURE ArraySortComparer<T, U>  IMPLEMENTS System.Collections.Generic.IComparer<T> WHERE T IS NEW()

    PRIVATE _cb AS @@Func<T,T,LOGIC>
    
    CONSTRUCTOR( cb AS @@Func<T,T,LOGIC> )
        _cb := cb
        RETURN
        
    METHOD Compare( x AS T, y AS T ) AS INT
        IF Object.ReferenceEquals(x, y )  
            RETURN 0
        ENDIF
        LOCAL u AS LOGIC
        u := _cb( x, y )
        RETURN IIF (  u , -1, 1 ) 
        
        
        END STRUCTURE
        
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/asort/*" /> 
    /// <typeparam name="T">The type of the array elements</typeparam>
FUNCTION ASort<T>(aTarget AS __ArrayBase<T> ,nStart AS INT,nCount AS INT,cbOrder AS @@Func<T,T,LOGIC>) AS __ArrayBase<T> WHERE T IS NEW()
    ARRAYNULL aTarget
    aTarget:Sort( nStart, nCount, ArraySortComparer<T, LOGIC> { cbOrder } )
    RETURN aTarget
    
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/asort/*" /> 
    /// <typeparam name="T">The type of the array elements</typeparam>
FUNCTION ASort<T>(aTarget AS __ArrayBase<T> ,cbOrder AS @@Func<T,T,LOGIC>) AS __ArrayBase<T> WHERE T IS NEW()
    ARRAYNULL aTarget
    aTarget:Sort( ArraySortComparer<T, LOGIC> { cbOrder } )
    RETURN aTarget
    
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/aeval/*" /> 
FUNCTION AEval<T>(aArray AS __ArrayBase<T>, cbBlock AS Action<T>) AS __ArrayBase<T> WHERE T IS NEW()
    ARRAYNULL aArray
    RETURN AEval(aArray, cbBlock, 1, ALen(aArray) )
    
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/aeval/*" /> 
FUNCTION AEval<T>(aArray AS __ArrayBase<T>, cbBlock AS Action<T>,nStart AS DWORD) AS __ArrayBase<T> WHERE T IS NEW()
    ARRAYNULL aArray
    RETURN AEval(aArray, cbBlock, nStart, ALen(aArray) - nStart +1)
    
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/aeval/*" /> 
    /// <typeparam name="T">The type of the array elements</typeparam>
FUNCTION AEval<T>(aArray AS __ArrayBase<T>, cbBlock AS Action<T>,nStart AS DWORD,nCount  AS DWORD) AS __ArrayBase<T> WHERE T IS NEW()
    ARRAYNULL aArray
    LOCAL nEnd AS DWORD
    nEnd := nStart + nCount -1
    FOR VAR nX := nStart TO nEnd
        cbBlock(aArray[ (INT) nX])
    NEXT
    RETURN aArray
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/aeval/*" />
FUNCTION AEval(aArray AS ARRAY,cbBlock AS ICodeblock ) AS ARRAY 
    ARRAYNULL aArray
    LOCAL uCount    := NIL AS USUAL
    LOCAL uStart	:= NIL AS USUAL
    ArrayHelpers.AEvalCheckArgs(aArray, cbBlock, REF uStart, REF uCount, "AEval")
    RETURN ArrayHelpers.AEval( aArray, cbBlock, uStart, uCount, FALSE)
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/aeval/*" />
FUNCTION AEval(aArray AS ARRAY,cbBlock AS ICodeblock ,nStart AS USUAL ) AS ARRAY 
    ARRAYNULL aArray
    LOCAL uCount    := NIL AS USUAL
    ArrayHelpers.AEvalCheckArgs(aArray, cbBlock, REF nStart, REF uCount, "AEval")
    RETURN ArrayHelpers.AEval( aArray, cbBlock, nStart, uCount, FALSE)
    
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/aeval/*" />
FUNCTION AEval(aArray AS ARRAY,cbBlock AS ICodeblock ,nStart AS USUAL ,nCount AS USUAL) AS ARRAY 
    ARRAYNULL aArray
    ArrayHelpers.AEvalCheckArgs(aArray, cbBlock, REF nStart, REF nCount, "AEval")
    RETURN ArrayHelpers.AEval( aArray, cbBlock, nStart, nCount , FALSE )
    
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/aevala/*" />
FUNCTION AEvalA(aArray AS ARRAY ,cbBlock AS ICodeblock) AS ARRAY
    ARRAYNULL aArray
    LOCAL uCount    := NIL AS USUAL
    LOCAL uStart	:= NIL AS USUAL
    ArrayHelpers.AEvalCheckArgs(aArray, cbBlock, REF uStart, REF uCount, "AEvalA")
    RETURN ArrayHelpers.AEval( aArray, cbBlock, uStart,uCount , TRUE)
    
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/aevala/*" />
FUNCTION AEvalA(aArray AS ARRAY ,cbBlock AS ICodeblock, nStart AS USUAL ) AS ARRAY
    ARRAYNULL aArray
    LOCAL uCount    := NIL AS USUAL
    ArrayHelpers.AEvalCheckArgs(aArray, cbBlock, REF nStart, REF uCount, "AEvalA")
    RETURN ArrayHelpers.AEval( aArray, cbBlock, nStart,uCount , TRUE)
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/aevala/*" />
FUNCTION AEvalA(aArray AS ARRAY ,cbBlock AS ICodeblock, nStart  AS USUAL ,nCount AS USUAL) AS ARRAY
    ARRAYNULL aArray
    ArrayHelpers.AEvalCheckArgs(aArray, cbBlock, REF nStart, REF nCount, "AEvalA")
    RETURN ArrayHelpers.AEval( aArray, cbBlock, nStart,nCount , TRUE)
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/aevala/*" /> 
    /// <typeparam name="T">The type of the array elements</typeparam>
FUNCTION AEvalA<T>(aArray AS __ArrayBase<T>, cbBlock AS @@Func<T,T>) AS __ArrayBase<T> WHERE T IS NEW()
    ARRAYNULL aArray
    RETURN AEvalA(aArray, cbBlock, 1, ALen(aArray))
    
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/aevala/*" /> 
    /// <typeparam name="T">The type of the array elements</typeparam>
FUNCTION AEvalA<T>(aArray AS __ArrayBase<T>, cbBlock AS @@Func<T,T>,nStart AS DWORD) AS __ArrayBase<T> WHERE T IS NEW()
    ARRAYNULL aArray
    RETURN AEvalA(aArray, cbBlock, nStart, ALen(aArray) - nStart +1)
    
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/aevala/*" /> 
    /// <typeparam name="T">The type of the array elements</typeparam>
FUNCTION AEvalA<T>(aArray AS __ArrayBase<T>, cbBlock AS @@Func<T,T>,nStart AS DWORD, nCount AS DWORD) AS __ArrayBase<T> WHERE T IS NEW()
    ARRAYNULL aArray
    LOCAL nEnd  AS DWORD
    nEnd := nStart + nCount -1
    FOR VAR nX := nStart TO nEnd
        aArray[ (INT) nX] := cbBlock( aArray[(INT)  nX])
    NEXT
    RETURN aArray
    
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/aevalold/*" />
FUNCTION AEvalOld(aArray AS ARRAY ,cbBlock AS ICodeblock,nStart  AS USUAL ,nCount AS USUAL) AS ARRAY
    ARRAYNULL aArray
    ArrayHelpers.AEvalCheckArgs(aArray, cbBlock, REF nStart, REF nCount, "AEvalOld")
    RETURN ArrayHelpers.AEval( aArray, cbBlock, nStart,nCount , FALSE)
    
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/aevalold/*" />
FUNCTION AEvalOld(aArray AS ARRAY ,cbBlock AS ICodeblock,nStart  AS USUAL ) AS ARRAY
    ARRAYNULL aArray
    LOCAL uCount	 := NIL AS USUAL
    ArrayHelpers.AEvalCheckArgs(aArray, cbBlock, REF nStart, REF uCount, "AEvalOld")
    RETURN ArrayHelpers.AEval( aArray, cbBlock, nStart,uCount , FALSE)
    
    /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/aevalold/*" />
FUNCTION AEvalOld(aArray AS ARRAY ,cbBlock AS ICodeblock) AS ARRAY
    ARRAYNULL aArray
    LOCAL uStart	 := NIL AS USUAL
    LOCAL uCount	 := NIL AS USUAL
    ArrayHelpers.AEvalCheckArgs(aArray, cbBlock, REF uStart, REF uCount, "AEvalOld")
    RETURN ArrayHelpers.AEval( aArray, cbBlock, uStart,uCount , FALSE)
    
    
    
