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
USING XSharp
BEGIN NAMESPACE XSharp
    /// <summary>Internal type that implements the VO Compatible ARRAY type.<br/>
    /// This type has methods and properties that normally are never directly called from user code.
    /// </summary>
    [DebuggerDisplay("{DebuggerString(),nq}", Type := "ARRAY")] ;
    [DebuggerTypeProxy(TYPEOF(ArrayDebugView))];
    PUBLIC SEALED CLASS __Array INHERIT __ArrayBase<USUAL>

        /// <inheritdoc />
        CONSTRUCTOR()
            SUPER()

            /// <inheritdoc />
        CONSTRUCTOR(capacity AS DWORD)
            SUPER(capacity)

            /// <summary>Create an array and fill it with elements from an existing .Net array of USUALS</summary>
        CONSTRUCTOR( elements AS USUAL[] )
            SELF()
            IF elements == NULL
                THROW ArgumentNullException{NAMEOF(elements)}
            ENDIF
            _internalList:Capacity := elements:Length
            _internalList:AddRange(elements)
            RETURN

            /// <inheritdoc />
        CONSTRUCTOR( elements AS OBJECT[] )
            SELF()
            IF elements == NULL
                THROW ArgumentNullException{NAMEOF(elements)}
            ENDIF
            FOREACH element AS OBJECT IN elements
                IF element == NULL
                    _internalList:add(DEFAULT(USUAL))
                ELSEIF element IS OBJECT[]
                    LOCAL objects AS OBJECT[]
                    objects := (OBJECT[]) element
                    _internalList:Add( __Array{ objects})
                ELSE
                    _internalList:Add( USUAL{ element})
                ENDIF
            NEXT
            RETURN

        INTERNAL STATIC METHOD ArrayCreate(dimensions PARAMS INT[] ) AS ARRAY
            LOCAL count := dimensions:Length AS INT
            IF count <= 0
                THROW ArgumentException{"No dimensions provided."}
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


        STATIC METHOD __ArrayNew( dimensions PARAMS INT[] ) AS __Array
            LOCAL newArray AS ARRAY
            IF dimensions:Length != 0
                newArray := __ArrayNewHelper(dimensions,1)
            ELSE
                newArray := ARRAY{}
            ENDIF
            RETURN newArray

        INTERNAL STATIC METHOD __ArrayNewHelper(dimensions AS INT[], currentDim AS INT) AS ARRAY
            LOCAL capacity  AS DWORD // one based ?
            LOCAL newArray AS ARRAY
            capacity := (DWORD) dimensions[currentDim]
            newArray := ARRAY{capacity}
            IF currentDim != dimensions:Length
                LOCAL nextDim := currentDim+1 AS INT
                LOCAL index   := 1 AS INT
                DO WHILE index <= capacity
                    newArray:Add(USUAL{__ArrayNewHelper(dimensions,nextDim)})
                    index+=1
                ENDDO
                RETURN newArray
            ENDIF
            RETURN newArray

        INTERNAL NEW METHOD Clone() AS ARRAY
            LOCAL aResult AS ARRAY
            LOCAL nCount AS DWORD
            nCount := (DWORD) _internalList:Count
            aResult := ARRAY{nCount}
            IF nCount == 0
                // warning, nCount-1 below will become MAXDWORD for nCount == 0
                RETURN aResult
            END IF
            FOR VAR I := 0 TO nCount-1
                VAR u := _internalList[i]
                IF u:isArray
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

            /// Note: Zero based, compiler handles subtraction
            /// <summary>Get/Set array elements with ZERO based array indexes.</summary>
        PUBLIC PROPERTY SELF[index PARAMS INT[]] AS USUAL
            GET
                RETURN __GetElement(index)
            END GET
            SET
                SELF:__SetElement(VALUE,index)
            END SET
        END PROPERTY

        NEW INTERNAL METHOD Swap(position AS INT, element AS USUAL) AS USUAL
            RETURN SUPER:Swap(position, element)

            ///
            /// <summary>Access the array element using ZERO based array indexes</summary>
            ///
        PUBLIC METHOD __GetElement(index PARAMS INT[]) AS USUAL
            LOCAL indexLength := index:Length AS INT
            LOCAL currentArray AS ARRAY
            LOCAL i AS INT
            currentArray := SELF
            FOR i:= 1  UPTO indexLength  -1 // walk all but the last level
                LOCAL u := currentArray:_internalList[ index[i] ] AS USUAL
                IF u:IsNil
                    RETURN u
                ENDIF
                IF !u:IsArray
                    THROW InvalidOperationException{"out of range error."}
                ENDIF
                currentArray := (ARRAY) u
            NEXT
            RETURN currentArray:_internalList[ index[i] ]

        INTERNAL METHOD DebuggerString() AS STRING
            LOCAL sb AS StringBuilder
            LOCAL cnt, tot AS LONG
            sb := StringBuilder{}
            sb:Append(SELF:ToString())
            sb:Append("{")
            tot := _internallist:Count
            cnt := 0
            FOREACH VAR element IN SELF:_internallist
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


            ///
            /// <summary>Assign the array element using ZERO based array indexes</summary>
            ///
        PUBLIC METHOD __SetElement(u AS USUAL, index PARAMS INT[] ) AS USUAL
            // indices are 0 based
            IF SELF:CheckLock()
                LOCAL length := index:Length AS INT
                LOCAL currentArray := SELF AS ARRAY
                FOR VAR i := 1 UPTO length-1
                    LOCAL uArray := _internalList[index[i]] AS USUAL
                    IF !(uArray:IsArray)
                        THROW InvalidOperationException{"Out of range error."}
                    ENDIF
                    currentArray := (ARRAY) uArray
                NEXT
                currentArray:_internalList[index[length]] := u
            ENDIF
            RETURN u

       /// <summary>Implicit conversion to OBJECT[]. SubArrays become nested OBJECT[] arrays.</summary>
        STATIC OPERATOR IMPLICIT ( a AS __Array) AS OBJECT[]
            LOCAL aResult := List<OBJECT>{} AS List<OBJECT>
            FOREACH VAR uElement IN a
                IF uElement:ISArray
                    LOCAL aSubArray AS ARRAY
                    aSubArray := uElement
                    aResult:Add( (OBJECT[]) aSubArray)
                ELSE
                    aResult:Add(  (OBJECT) uElement)
                ENDIF
            NEXT
            RETURN aResult:ToArray()

        STATIC OPERATOR IMPLICIT ( a AS OBJECT[]) AS __Array
            RETURN __Array{a}

        INTERNAL STATIC METHOD Copy(aSource AS ARRAY,aTarget AS ARRAY,;
            start AS DWORD, sourceLen AS DWORD, offSet AS DWORD, targetLen AS DWORD ) AS VOID
            LOCAL x AS DWORD
            // Adjust
            start-=1
            offSet-=1
            sourceLen-=1
            targetLen-=1
            IF start < sourceLen
                FOR x := start UPTO sourceLen
                    aTarget:_InternalList[(INT) offSet] := aSource:_InternalList[(INT) x]
                    offSet++
                    IF offSet > targetLen
                        EXIT
                    ENDIF
                NEXT
            ELSE
                FOR x := start DOWNTO sourceLen
                    aTarget:_InternalList[(INT) offSet] := aSource:_InternalList[(INT) x]
                    offSet++
                    IF offSet > targetLen
                        EXIT
                    ENDIF
                NEXT
            ENDIF
            RETURN

        NEW INTERNAL METHOD Sort(startIndex AS INT, count AS INT, comparer AS IComparer<__USUAL>) AS VOID
            _internalList:Sort(startIndex-__ARRAYBASE__ ,count,comparer)
            RETURN


        INTERNAL CLASS ArrayDebugView
            PRIVATE _value AS ARRAY
            PUBLIC CONSTRUCTOR (a AS ARRAY)
                _value := a
                //[DebuggerBrowsable(DebuggerBrowsableState.RootHidden)] ;
                PUBLIC PROPERTY Elements AS List<USUAL> GET _value:_internalList


        END CLASS

    END	CLASS


END NAMESPACE
