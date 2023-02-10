//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Collections
USING System.Collections.Generic
USING System.Linq
USING System.Diagnostics
USING System.Reflection
USING System.Text
USING System.Runtime.Serialization
USING XSharp

#define USEATTRIB
#ifdef USEATTRIB
#XTRANSLATE \[NOSHOW\] => \[DebuggerBrowsable(DebuggerBrowsableState.Never)\]
#XTRANSLATE \[INLINE\] => \[MethodImpl(MethodImplOptions.AggressiveInlining)\]
#XTRANSLATE \[NODEBUG\] => \[DebuggerStepThroughAttribute\]
#else
#XTRANSLATE \[NOSHOW\] =>
#XTRANSLATE \[INLINE\] =>
#XTRANSLATE \[NODEBUG\] =>
#endif

BEGIN NAMESPACE XSharp
/// <summary>Internal type that implements the VO Compatible ARRAY type.<br/>
/// This type has methods and properties that normally are never directly called from user code.
/// </summary>
/// <seealso cref='IIndexer' />
/// <include file="RTComments.xml" path="Comments/ZeroBasedIndex/*" />
//[DebuggerTypeProxy(TYPEOF(ArrayDebugView))];
[DebuggerDisplay("{DebuggerString(),nq}")] ;
[Serializable];
PUBLIC CLASS __Array INHERIT __ArrayBase<USUAL> IMPLEMENTS IIndexer, ISerializable
    [NOSHOW];
    PRIVATE CONST FoxArrayName := "XSharp.__FoxArray" AS STRING
    [NOSHOW];
    STATIC FoxArrayHelpers := FoxArrayHelpers {} AS FoxArrayHelpers
    [NOSHOW];
    INTERNAL PROPERTY __IsFoxArray AS LOGIC GET SELF:GetType():FullName == FoxArrayName

        [NOSHOW];
            INTERNAL STATIC SuppressArrayIndexErrors := FALSE AS LOGIC  // used for Get_Element to emulate strange VO behaviour

    /// <inheritdoc />
    CONSTRUCTOR()
        SUPER()

    /// <inheritdoc />
    CONSTRUCTOR(capacity AS DWORD)
        SUPER(capacity)

    /// <inheritdoc />
    CONSTRUCTOR(capacity AS DWORD, fill AS LOGIC)
        SUPER(capacity, fill)

    /// <summary>Create an array and fill it with elements from an existing .Net array of USUALS</summary>
    CONSTRUCTOR( elements AS USUAL[] )
        SELF()
        IF elements == NULL
            THROW Error{ArgumentNullException{NAMEOF(elements)}}
        ENDIF
        _internalList:Capacity := elements:Length
        _internalList:AddRange(elements)
        RETURN

    /// <inheritdoc />
    CONSTRUCTOR( elements AS OBJECT[] )
        SELF()
        IF elements == NULL
            RETURN // empty array
        ENDIF
        FOREACH element AS OBJECT IN elements
            IF element == NULL
                _internalList:Add(NIL)
            ELSEIF element IS OBJECT[]
                LOCAL objects AS OBJECT[]
                objects := (OBJECT[]) element
                _internalList:Add( __Array{ objects})
            ELSE
                _internalList:Add( USUAL{ element})
            ENDIF
        NEXT
        RETURN

    /// <inheritdoc />
    CONSTRUCTOR( collection AS IEnumerable<USUAL>)
        SUPER(collection)
        RETURN

    /// <inheritdoc />
    CONSTRUCTOR( collection AS IEnumerable<OBJECT>)
        SELF(collection:ToArray())
        RETURN

#region ISerializable
    /// <inheritdoc/>
    PUBLIC OVERRIDE METHOD GetObjectData(info AS SerializationInfo, context AS StreamingContext) AS VOID
        SUPER:GetObjectData(info, context)
        RETURN

    /// <include file="RTComments.xml" path="Comments/SerializeConstructor/*" />
    CONSTRUCTOR (info AS SerializationInfo, context AS StreamingContext)
        SUPER(info, context)
#endregion
    INTERNAL STATIC METHOD ArrayCreate(dimensions PARAMS INT[] ) AS ARRAY
        LOCAL count := dimensions:Length AS INT
        IF count <= 0
            THROW Error{ArgumentException{"No dimensions provided.",nameof(dimensions)}}
        ENDIF
        LOCAL initializer := OBJECT[]{dimensions[1]} AS OBJECT[]
        LOCAL arrayNew AS ARRAY
        arrayNew := ARRAY{initializer}

        IF count > 1
            LOCAL i AS INT
            FOR i:=0+__ARRAYBASE__  UPTO dimensions[1]-1+__ARRAYBASE__
                LOCAL newParams := INT[]{count-1} AS INT[]
                Array.Copy(dimensions,1,newParams,0,count-1)
                arrayNew:_internalList[(INT) i-__ARRAYBASE__ ] := __ArrayNew(newParams)
            NEXT
        ENDIF
        RETURN arrayNew

    /// <exclude/>
    STATIC METHOD __ArrayNew( dimensions PARAMS INT[] ) AS __Array
        LOCAL newArray AS ARRAY
        IF dimensions:Length != 0
            newArray := __ArrayNewHelper(dimensions,1)
        ELSE
            newArray := ARRAY{}
        ENDIF
        RETURN newArray

    INTERNAL STATIC METHOD __ArrayNewHelper(dimensions AS INT[], currentDim AS INT) AS ARRAY
        LOCAL size AS DWORD
        LOCAL newArray AS ARRAY
        size := (DWORD) dimensions[currentDim]
        newArray := ARRAY{size, TRUE}
        IF currentDim != dimensions:Length
            LOCAL nextDim := currentDim+1 AS INT
            LOCAL index   := 1 AS INT
            DO WHILE index <= size
                newArray[index - 1 + __ARRAYBASE__] := USUAL{__ArrayNewHelper(dimensions,nextDim)}
                index+=1
            ENDDO
            RETURN newArray
        ENDIF
        RETURN newArray

    INTERNAL NEW METHOD Clone() AS ARRAY
        LOCAL aResult AS ARRAY
        LOCAL nCount AS DWORD
        nCount := (DWORD) _internalList:Count
        aResult := ARRAY{nCount, TRUE}
        IF nCount == 0
            // warning, nCount-1 below will become MAXDWORD for nCount == 0
            RETURN aResult
        END IF
        FOR VAR i := 0 UPTO nCount-1
            VAR u := _internalList[i]
            IF u:IsArray
                VAR aElement := (ARRAY) u
                IF aElement != NULL_ARRAY
                    aResult:_internalList[i] := aElement:Clone()
                ELSE
                    aResult:_internalList[i] := aElement
                ENDIF
            ELSE
                aResult:_internalList[i] := u
            ENDIF
        NEXT
        RETURN aResult

    INTERNAL METHOD CloneShallow() AS ARRAY
        RETURN (ARRAY) SUPER:Clone()


    /// <include file="RTComments.xml" path="Comments/ZeroBasedIndexProperty/*" />
    /// <param name="index"><include file="RTComments.xml" path="Comments/ZeroBasedIndexParam/*" /></param>
    /// <returns>The value of the property of the element stored at the indicated location in the array.</returns>
    NEW VIRTUAL PUBLIC PROPERTY SELF[index AS INT] AS USUAL
    GET
        RETURN SELF:__GetElement(index)
    END GET
    SET
        SELF:__SetElement(value,index)
    END SET
    END PROPERTY


    /// <include file="RTComments.xml" path="Comments/ZeroBasedIndexProperty/*" />
    /// <param name="index"><include file="RTComments.xml" path="Comments/ZeroBasedIndexParam/*" /></param>
    /// <param name="index2"><include file="RTComments.xml" path="Comments/ZeroBasedIndexParam/*" /></param>
    /// <returns>The value of the property of the element stored at the indicated location in the array.</returns>
    NEW VIRTUAL PUBLIC PROPERTY SELF[index AS INT, index2 AS INT] AS USUAL
    GET
        RETURN SELF:__GetElement(index,index2)
    END GET
    SET
        SELF:__SetElement(value,index,index2)
    END SET
    END PROPERTY



    /// <include file="RTComments.xml" path="Comments/ZeroBasedIndexProperty/*" />
    /// <param name="indices"><include file="RTComments.xml" path="Comments/ZeroBasedIndexParam/*" /></param>
    /// <returns>The value of the property of the element stored at the indicated location in the array.</returns>
    VIRTUAL PUBLIC PROPERTY SELF[indices PARAMS INT[]] AS USUAL
    GET
        RETURN SELF:__GetElement(indices)
    END GET
    SET
        SELF:__SetElement(value,indices)
    END SET
    END PROPERTY

    /// <summary>Returns the default value for array elements when arrays are resized or initialized. This is NIL.</summary>
    [NOSHOW];
    PUBLIC OVERRIDE PROPERTY DefaultValue AS USUAL GET NIL

    NEW INTERNAL METHOD Swap(position AS INT, element AS USUAL) AS USUAL
        RETURN SUPER:Swap(position, element)

    PROTECTED METHOD __CheckArrayElement(a AS ARRAY, index AS INT, name AS STRING, pos AS INT) AS VOID
        IF index < 0 .OR. index >= a:_internalList:Count
            VAR err := Error.BoundError(ProcName(1),name, (DWORD) pos, {index+1})
            err:Stack   := ErrorStack(1)
            VAR length := a:_internalList:Count
            err:Description := i"Bound error: Index ({index+1}) exceeds length of (Sub)Array ({length})"
            THROW err
        ENDIF
        RETURN

    PRIVATE STATIC METHOD __NotAnArray(name AS STRING , pos AS INT, args AS OBJECT[]) AS Exception
        VAR err         := Error.BoundError(ProcName(1),name, (DWORD) pos, args)
        err:Description := "Bound error: "+VO_Sprintf(VOErrors.USUALNOTINDEXED, typeof(IIndexedProperties):FullName)
        err:Stack   := ErrorStack(1)
        RETURN err

    /// <include file="RTComments.xml" path="Comments/ZeroBasedIndexProperty/*" />
    /// <param name="index"><include file="RTComments.xml" path="Comments/ZeroBasedIndexParam/*" /></param>
    /// <returns>The element stored at the specified location in the array.</returns>
    NEW PUBLIC METHOD __GetElement(index AS INT) AS USUAL
        // VO always throws an error when a single dimension is passed and this dimension is not correct
        SELF:__CheckArrayElement(SELF, index, nameof(index),1)
        RETURN SELF:_internalList[ index ]

    /// <include file="RTComments.xml" path="Comments/ZeroBasedIndexProperty/*" />
    /// <param name="index"><include file="RTComments.xml" path="Comments/ZeroBasedIndexParam/*" /></param>
    /// <returns>The element stored at the specified location in the array.</returns>
    PUBLIC VIRTUAL METHOD __GetElement(index AS INT, index2 AS INT) AS USUAL
        // VO Throws an exception when the first dimension is incorrect but not
        // when another dimension is incorrect. That is why we have a TRY CATCH for the second dimension
        SELF:__CheckArrayElement(SELF, index, nameof(index),1)
        VAR u := SELF:_internalList[ index ]
        TRY
            IF u:IsArray
                VAR a := (ARRAY) u
                SELF:__CheckArrayElement(a, index2, nameof(index2),2)
                RETURN a:_internalList [index2]
            ELSEIF u:IsIndexed
                // not an array, so we call the index operation on the usual,
                // this will handle special cases such as indexing a string for Xbase++
                RETURN u[index2+1]
            ENDIF
            THROW __NotAnArray(nameof(index2), 2, <OBJECT>{index+1, index2+1})

        CATCH AS Exception
            IF !SuppressArrayIndexErrors
                THROW
            ENDIF
            // This does not make sense, but that is the way  VO does it.
            // when aTest := {1,2,3}
            // ? aTest[1,1] is allowed and returns NIL !
            // but when aTest is not an array at all then it fails
            RETURN NIL
        END TRY


    /// <include file="RTComments.xml" path="Comments/ZeroBasedIndexProperty/*" />
    /// <param name="indices"><include file="RTComments.xml" path="Comments/ZeroBasedIndexParam/*" /></param>
    /// <returns>The element stored at the specified location in the array.</returns>
    PUBLIC VIRTUAL METHOD __GetElement(indices PARAMS INT[]) AS USUAL
        LOCAL length := indices:Length AS INT
        LOCAL currentArray AS ARRAY
        LOCAL i AS INT
        LOCAL u AS USUAL
        LOCAL firstDimension AS LOGIC
        u := SELF
        firstDimension := TRUE
        TRY
            LOCAL index AS INT

            FOR i:= 1  UPTO length  -1 // walk all but the last level
                currentArray := (ARRAY) u
                index := indices[i]
                SELF:__CheckArrayElement(currentArray, index, nameof(indices),1)
                u := currentArray:_internalList[ index ]
                firstDimension := FALSE
                IF (OBJECT) u IS IIndexedProperties .AND. i == length-1
                    LOCAL o := (IIndexedProperties) (OBJECT) u AS IIndexedProperties
                    RETURN o[indices[length]]
                ENDIF
                IF !u:IsArray
                    THROW __NotAnArray(nameof(indices), i+1, SELF:_adjustArguments(indices))
                ENDIF
            NEXT
            index := indices[length]
            IF u:IsArray
                currentArray := (ARRAY) u
                SELF:__CheckArrayElement(currentArray, index, nameof(indices),length)
                RETURN currentArray:_internalList[ index ]
            ELSEIF u:IsIndexed
                // Call the array operator on the usual class to support substring and bittest operations.
                RETURN u[index +1]
            ENDIF

            THROW __NotAnArray(nameof(indices), i, SELF:_adjustArguments(indices))
        CATCH AS Exception
            IF !SuppressArrayIndexErrors .or. firstDimension
                THROW
            ENDIF
            // This does not make sense, but that is the way  VO does it.
            // when aTest := {1,2,3}
            // ? aTest[1,1] is allowed and returns NIL !
            // but when aTest is not an array at all then it fails
            RETURN NIL
        END TRY

    PRIVATE METHOD _adjustArguments(indices AS INT[], u := NIL AS USUAL) AS OBJECT[]
        VAR result := List<OBJECT>{}
        IF! u:IsNil
            result:Add(u)
        ENDIF
        FOREACH VAR index IN indices
            result:Add(index+1)
        NEXT
        RETURN result:ToArray()

    PROTECTED VIRTUAL METHOD  ElementsToString() AS STRING
        LOCAL sb AS StringBuilder
        LOCAL cnt, tot AS LONG
        sb := StringBuilder{}
        sb:Append("{")
        tot := _internalList:Count
        cnt := 0
        FOREACH VAR element IN SELF:_internalList
            IF cnt > 0
                sb:Append(",")
            ENDIF
            sb:Append(element:ToString())
            cnt++
            IF cnt > 5
                IF cnt < tot
                    sb:Append(",..")
                ENDIF
                EXIT
            ENDIF
        NEXT
        sb:Append("}")
        RETURN sb:ToString()
    PROTECTED OVERRIDE METHOD DebuggerString() AS STRING
        LOCAL sb AS StringBuilder
        sb := StringBuilder{}
        sb:Append(SELF:ToString())
        sb:Append(SELF:ElementsToString())
        RETURN sb:ToString()


    /// <include file="RTComments.xml" path="Comments/ZeroBasedIndexProperty/*" />
    /// <param name="index"><include file="RTComments.xml" path="Comments/ZeroBasedIndexParam/*" /></param>
    /// <returns>The element stored at the specified location in the array.</returns>
    NEW PUBLIC VIRTUAL METHOD __SetElement(u AS USUAL, index AS INT) AS USUAL
        IF SELF:CheckLock()
            SELF:__CheckArrayElement(SELF, index, nameof(index),1)
            SELF:_internalList[ index ] := u
        ENDIF
        RETURN u


    /// <include file="RTComments.xml" path="Comments/ZeroBasedIndexProperty/*" />
    /// <param name="index"><include file="RTComments.xml" path="Comments/ZeroBasedIndexParam/*" /></param>
    /// <param name="index2"><include file="RTComments.xml" path="Comments/ZeroBasedIndexParam/*" /></param>
    /// <returns>The element stored at the specified location in the array.</returns>
    PUBLIC VIRTUAL METHOD __SetElement(u AS USUAL, index AS INT, index2 AS INT) AS USUAL
        IF SELF:CheckLock()
            SELF:__CheckArrayElement(SELF, index, nameof(index),1)
            VAR uElement := SELF:_internalList[ index ]
            IF !uElement:IsArray
                THROW __NotAnArray(nameof(index2), 3, <OBJECT>{u, index+1, index2+1})
            ENDIF
            LOCAL a := NULL AS ARRAY
            a := (ARRAY) uElement
            SELF:__CheckArrayElement(a, index2, nameof(index2),2)
            a:_internalList [index2] := u
        ENDIF
        RETURN u



    /// <include file="RTComments.xml" path="Comments/ZeroBasedIndexProperty/*" />
    /// <param name="indices"><include file="RTComments.xml" path="Comments/ZeroBasedIndexParam/*" /></param>
    /// <param name='u'>New element to store in the array at the position specified</param>
    /// <returns>The new element</returns>
    PUBLIC VIRTUAL METHOD __SetElement(u AS USUAL, indices PARAMS INT[] ) AS USUAL
        // indices are 0 based
        IF SELF:CheckLock()
            LOCAL length := indices:Length AS INT
            LOCAL currentArray := SELF AS ARRAY
            FOR VAR i := 1 UPTO length-1
                VAR index := indices[i]
                SELF:__CheckArrayElement(currentArray, index, nameof(indices),i+1)
                LOCAL uArray := currentArray:_internalList[index] AS USUAL
                IF (OBJECT) u IS IIndexedProperties .AND. i == length-1
                    LOCAL o := (IIndexedProperties) (OBJECT) u AS IIndexedProperties
                    o[indices[length]] := u
                    RETURN u
                ENDIF
                IF ! uArray:IsArray
                    THROW __NotAnArray(nameof(indices), i+1, SELF:_adjustArguments(indices,u))
                ENDIF
                currentArray := (ARRAY) uArray
            NEXT
            currentArray:_internalList[indices[length]] := u
        ENDIF
        RETURN u

    /// <summary>Implicit conversion to OBJECT[]. SubArrays become nested OBJECT[] arrays.</summary>
    STATIC OPERATOR IMPLICIT ( a AS __Array) AS OBJECT[]
        LOCAL aResult := List<OBJECT>{} AS List<OBJECT>
        FOREACH uElement AS USUAL IN a
            IF uElement:IsArray
                LOCAL aSubArray AS ARRAY
                aSubArray := (ARRAY) uElement
                aResult:Add( (OBJECT[]) aSubArray)
            ELSE
                aResult:Add(  (OBJECT) uElement)
            ENDIF
        NEXT
        RETURN aResult:ToArray()

    /// <exclude/>
    STATIC OPERATOR IMPLICIT ( a AS OBJECT[]) AS __Array
        RETURN __Array{a}

    INTERNAL STATIC METHOD Copy(aSource AS ARRAY,aTarget AS ARRAY,;
            start AS LONG, sourceLen AS LONG, offSet AS LONG, targetLen AS LONG ) AS VOID
        LOCAL x AS LONG
        // Adjust
        IF start > 0 .and. sourceLen > 0
            IF start < sourceLen
                FOR x := start UPTO sourceLen
                    IF offSet > targetLen
                        EXIT
                    ENDIF
                    aTarget:_internalList[offSet-1] := aSource:_internalList[ x -1]
                    offSet++
                NEXT
            ELSE
                FOR x := start DOWNTO sourceLen
                    aTarget:_internalList[offSet-1] := aSource:_internalList[x-1]
                    offSet++
                    IF offSet > targetLen
                        EXIT
                    ENDIF
                NEXT
            ENDIF
        ENDIF
        RETURN

    NEW INTERNAL METHOD Sort(startIndex AS INT, count AS INT, comparer AS IComparer<__Usual>) AS VOID
        IF startIndex <= 0
            startIndex := 1
        ENDIF
        IF count < 0
            count := _internalList:Count - startIndex + __ARRAYBASE__
        ENDIF
        _internalList:Sort(startIndex-__ARRAYBASE__ ,count,comparer)
        RETURN
    /// <exclude/>
    PUBLIC METHOD Invoke(index PARAMS INT[]) AS USUAL
        FOR VAR i := 1 UPTO index:Length
            index[i] -= 1
        NEXT
        RETURN SELF:__GetElement(index)

    INTERNAL CLASS ArrayDebugView
        PRIVATE _value AS ARRAY
        PUBLIC CONSTRUCTOR (a AS ARRAY)
            _value := a
            //[DebuggerBrowsable(DebuggerBrowsableState.RootHidden)] ;
        PUBLIC PROPERTY Elements AS List<USUAL> GET _value:_internalList


    END CLASS

END	CLASS


/// <exclude/>
CLASS FoxArrayHelpers
    DELEGATE FoxADel(ArrayName AS ARRAY, nElementNumber AS LONG, nDeleteType AS LONG) AS DWORD
    DELEGATE FoxALen(a as ARRAY) AS DWORD
    DELEGATE FoxAIns(ArrayName AS ARRAY, nElementNumber AS DWORD, nInsertType AS DWORD) AS DWORD
    DELEGATE FoxShowArray(a as Array, c as string) as void
    PUBLIC ALen AS FoxALen
    PUBLIC AIns AS FoxAIns
    PUBLIC ADel AS FoxADel
    PUBLIC ShowArray AS FoxShowArray
    PUBLIC METHOD Reset() AS VOID
        ALen := NULL
        AIns := NULL
        ADel := NULL
        ShowArray := NULL
END CLASS

END NAMESPACE

/// <summary>Suppress Array Index check to be compatible with Visual Objects</summary>
/// <param name="lCheck">TRUE to enable the array index checks. </param>
/// <returns>The previous setting of the flag</returns>
/// <summary>In some situations Visual Objects did not throw a runtime error when you were accessing a non existing array element. <br/>
/// You can enable this (mis)behaviour in X# by disabling the array index checks.<br/>
/// The default behavior is to generate an error when you access array indices out of the existing range.
/// </summary>
/// <example>
/// // The following code does not throw a runtime error but displays NIL for u[1,1]
/// // To get the same behavior in X# you need to call EnableArrayIndexCheck(FALSE)
/// FUNCTION Start
/// LOCAL u AS USUAL
/// u := {1,2,3}
/// ? u[1,1]
/// WAIT
/// RETURN TRUE
/// </example>

FUNCTION EnableArrayIndexCheck(lCheck AS LOGIC) AS LOGIC
    LOCAL lOld AS LOGIC
    lOld := ! XSharp.__Array.SuppressArrayIndexErrors
    XSharp.__Array.SuppressArrayIndexErrors := !lCheck
    RETURN lOld

