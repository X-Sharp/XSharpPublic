//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Collections
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING System.Diagnostics
USING System.Reflection
USING XSharp
USING System.Runtime.Serialization
USING System.Runtime.CompilerServices

#include "attributes.xh"
BEGIN NAMESPACE XSharp
/// <summary>Internal type that implements the new TYPED ARRAY type.<br/>
/// This type has methods and properties that normally are never directly called from user code.
/// </summary>
/// <typeparam name="T">Type of the elements inside the array </typeparam>
[DebuggerDisplay("{DebuggerString(),nq}")] ;
[Serializable];
PUBLIC CLASS __ArrayBase<T> ;
        IMPLEMENTS INamedIndexer, IEnumerable<T>, ISerializable

    [NOSHOW] PROTECTED INTERNAL _internalList AS List<T>
    [NOSHOW] PRIVATE _islocked AS LOGIC
#region constructors
    /// <summary>Create an empty array</summary>
    CONSTRUCTOR()
        _internalList := List<T>{}
        RETURN

    /// <summary>Create an array with a certain capacity.</summary>
    CONSTRUCTOR(capacity AS DWORD)
        _internalList := List<T>{ (INT) capacity}
        //_internalList:AddRange(Enumerable.Repeat(DEFAULT(T),(INT) capacity))
        RETURN

    /// <summary>Create an array with a certain capacity and specify if it should be filled with default values.</summary>
    CONSTRUCTOR(capacity AS DWORD, fill AS LOGIC)
        _internalList := List<T>{ (INT) capacity}
        IF fill
            _internalList:AddRange(Enumerable.Repeat(SELF:DefaultValue,(INT) capacity))
        ENDIF
        RETURN

    /// <summary>Create an array and fill it with elements from an existing collection.</summary>
    CONSTRUCTOR( collection AS IEnumerable<T>)
        _internalList := List<T>{collection}
        RETURN

    /// <summary>Create an array and fill it with elements from an existing .Net array of objects. Note that the objects must be of the right type.</summary>
    CONSTRUCTOR( elements AS OBJECT[] )
        SELF()
        IF elements == NULL
            THROW Error{ArgumentNullException{NAMEOF(elements)}}
        ENDIF
        FOREACH element AS OBJECT IN elements
            IF element == NULL
                _internalList:Add(SELF:DefaultValue)
            ELSEIF element IS T
                _internalList:Add( (T) element)
            ELSE
                THROW Error{ArgumentException{"Object array contains element of incorrect type "+element:GetType():FullName}}
            ENDIF
        NEXT
        RETURN

    /// <summary>Create an array and fill it with elements from an existing .Net array.</summary>
    CONSTRUCTOR( elements AS T[] )
        _internalList := List<T>{elements}
        RETURN
#endregion

#region properties
    /// <summary>Is the array empty.</summary>
    [NOSHOW] PUBLIC PROPERTY IsEmpty AS LOGIC [INLINE] GET _internalList:Count == 0
    /// <summary>Length of the array.</summary>
    PUBLIC PROPERTY Length AS DWORD [INLINE] GET (DWORD) _internalList:Count
    /// <summary>Length of the array as integer.</summary>
    [NOSHOW] PUBLIC PROPERTY Count AS INT [INLINE] GET _internalList:Count

    /// <summary>Returns the default value for array elements when arrays are resized or initialized.</summary>
    [NOSHOW] PUBLIC VIRTUAL PROPERTY DefaultValue AS T GET DEFAULT(T)
#endregion

#region Enumerators
    PUBLIC METHOD IEnumerable<T>.GetEnumerator() AS IEnumerator<T>
        RETURN ((IList<T> ) _internalList:ToArray()):GetEnumerator()

    PUBLIC METHOD IEnumerable.GetEnumerator() AS IEnumerator
        RETURN _internalList:ToArray():GetEnumerator()

#endregion


#region Cloning

    INTERNAL METHOD Clone() AS __ArrayBase<T>
        LOCAL aResult AS __ArrayBase<T>
        LOCAL nCount AS DWORD
        nCount := (DWORD) _internalList:Count
        aResult := (__ArrayBase<T>) Activator.CreateInstance(SELF:GetType(), NULL)
        aResult:Resize( _internalList:Count )
        // warning, nCount-1 below will become MAXDWORD for nCount == 0
        IF nCount == 0
            RETURN aResult
        ENDIF
        FOR VAR i := 0 TO nCount-1
            aResult:_internalList[i] := _internalList[i]
        NEXT
        RETURN aResult

#endregion


#region Indexers and TO GET / SET Elements.
    /// <include file="RTComments.xml" path="Comments/ZeroBasedIndexProperty/*" />
    /// <param name="index"><include file="RTComments.xml" path="Comments/ZeroBasedIndexParam/*" /></param>
    /// <returns>The element stored at the specified location in the array.</returns>
    PUBLIC VIRTUAL METHOD __GetElement(index AS INT) AS T
        RETURN SELF:_internalList[ index ]

    /// <include file="RTComments.xml" path="Comments/ZeroBasedIndexProperty/*" />
    /// <param name="index"><include file="RTComments.xml" path="Comments/ZeroBasedIndexParam/*" /></param>
    /// <param name='e'>New element to store in the array at the position specified</param>
    /// <returns>The new element</returns>
    PUBLIC METHOD __SetElement(e AS T,index AS INT) AS T
        IF SELF:CheckLock()
            _internalList[index]:=e
        ENDIF
        RETURN e

    PRIVATE METHOD __GetProperty(sName AS STRING) AS PropertyInfo
        LOCAL type AS System.Type
        LOCAL oProp AS PropertyInfo
        type := TYPEOF(T)
        oProp := type:GetProperty(sName, BindingFlags.IgnoreCase | BindingFlags.Instance | BindingFlags.Public)
        IF oProp == NULL_OBJECT
            THROW Error{ArgumentException{"Unknown property: "+sName}}
        ENDIF
        RETURN oProp

    PRIVATE METHOD __GetIndexer(lNumeric AS LOGIC) AS PropertyInfo
        LOCAL type AS System.Type
        LOCAL aProps AS PropertyInfo[]
        type := TYPEOF(T)
        aProps := type:GetProperties(BindingFlags.IgnoreCase | BindingFlags.Instance | BindingFlags.Public)
        FOREACH oProp AS PropertyInfo IN aProps
            IF !oProp:Name:ToLower() == "item"
                LOOP
            ENDIF
            LOCAL pars := oProp:GetIndexParameters() AS ParameterInfo[]
            IF pars:Length > 0
                LOCAL par := pars[__ARRAYBASE__] AS ParameterInfo
                SWITCH Type.GetTypeCode(par:ParameterType)
                CASE TypeCode.Int32
                CASE TypeCode.Int64
                CASE TypeCode.UInt32
                CASE TypeCode.UInt64
                    IF (lNumeric)
                        RETURN oProp
                    ENDIF
                CASE TypeCode.String
                    IF (!lNumeric)
                        RETURN oProp
                    ENDIF
                END SWITCH
            ENDIF

        NEXT
        RETURN NULL

    /// <include file="RTComments.xml" path="Comments/ZeroBasedIndexProperty/*" />
    /// <param name="index"><include file="RTComments.xml" path="Comments/ZeroBasedIndexParam/*" /></param>
    /// <returns>The element stored at the indicated location in the collection.</returns>
    PUBLIC PROPERTY SELF[index AS INT] AS T
    GET
        IF  index > _internalList:Count-1
            THROW Error{ArgumentOutOfRangeException{nameof(index)}}
        ENDIF
        RETURN _internalList[index ]
    END GET
    SET
        IF SELF:CheckLock()
            IF index > _internalList:Count-1
                THROW Error{ArgumentOutOfRangeException{nameof(index)}}
            ENDIF
            _internalList[index] := value
        ENDIF
    END SET
    END PROPERTY

    /// <include file="RTComments.xml" path="Comments/ZeroBasedIndexProperty/*" />
    /// <param name="index"><include file="RTComments.xml" path="Comments/ZeroBasedIndexParam/*" /></param>
    /// <param name="index2"><include file="RTComments.xml" path="Comments/ZeroBasedIndexParam/*" /></param>
    /// <returns>The value of the property of the element stored at the indicated location in the array.</returns>
    PUBLIC PROPERTY SELF[index AS INT, index2 AS INT] AS USUAL
    GET
        LOCAL oElement AS T
        IF  index > _internalList:Count-1
            THROW Error{ArgumentOutOfRangeException{nameof(index)}}
        ENDIF
        oElement := _internalList[index ]
        IF oElement IS IIndexedProperties
            VAR oIndex := (IIndexedProperties) oElement
            RETURN oIndex[index2]
        ENDIF
        LOCAL oProp    AS PropertyInfo
        oProp    := SELF:__GetIndexer(TRUE)
        IF oProp != NULL
            RETURN oProp:GetValue(oElement, <OBJECT>{index2})
        ENDIF
        THROW Error{ArgumentException{"Indexed property missing for type: "+oElement:GetType():FullName}}
    END GET
    SET
        IF  index > _internalList:Count-1
            THROW Error{ArgumentOutOfRangeException{nameof(index)}}
        ENDIF
        IF SELF:CheckLock()
            LOCAL oElement AS T
            oElement := _internalList[index ]
            IF oElement IS IIndexedProperties
                VAR oIndex := (IIndexedProperties) oElement
                oIndex[index2] := value
            ENDIF
            LOCAL oProp    AS PropertyInfo
            oProp    := SELF:__GetIndexer(TRUE)
            IF oProp != NULL
                oProp:SetValue(oElement, OOPHelpers.ValueConvert(value, oProp:PropertyType), <OBJECT>{index2} )
                RETURN
            ENDIF
            THROW Error{ArgumentException{"Indexed property missing for type: "+oElement:GetType():FullName}}
        ENDIF
    END SET
    END PROPERTY

    /// <include file="RTComments.xml" path="Comments/ZeroBasedIndexProperty/*" />
    /// <param name="index"><include file="RTComments.xml" path="Comments/ZeroBasedIndexParam/*" /></param>
    /// <param name="name"><include file="RTComments.xml" path="Comments/NameBasedIndexParam/*" /></param>
    /// <returns>The value of the property of the element stored at the indicated location in the array.</returns>
    PUBLIC PROPERTY SELF[index AS INT, name AS STRING] AS USUAL
    GET
        LOCAL oElement AS T
        IF  index > _internalList:Count-1
            THROW Error{ArgumentOutOfRangeException{}}
        ENDIF
        oElement := _internalList[index ]
        IF oElement IS IIndexedProperties
            VAR oIndex := (IIndexedProperties) oElement
            RETURN oIndex[name]
        ENDIF
        LOCAL oProp    AS PropertyInfo
        oProp    := SELF:__GetIndexer(FALSE)
        IF oProp != NULL
            RETURN oProp:GetValue(oElement, <OBJECT>{name})
        ENDIF
        oProp	 := SELF:__GetProperty( name)
        RETURN oProp:GetValue(oElement, NULL)
    END GET
    SET
        IF  index > _internalList:Count-1
            THROW Error{ArgumentOutOfRangeException{}}
        ENDIF
        IF SELF:CheckLock()
            LOCAL oElement AS T
            oElement := _internalList[index ]
            IF oElement IS IIndexedProperties
                VAR oIndex := (IIndexedProperties) oElement
                oIndex[name] := value
            ENDIF
            LOCAL oProp    AS PropertyInfo
            oProp    := SELF:__GetIndexer(FALSE)
            IF oProp != NULL
                oProp:SetValue(oElement, OOPHelpers.ValueConvert(value, oProp:PropertyType), <OBJECT>{name} )
                RETURN
            ENDIF
            oProp	 := SELF:__GetProperty( name)
            oProp:SetValue(oElement, NULL, value)
        ENDIF
    END SET
    END PROPERTY

#endregion

#region Insert and Delete elements
    /// <exclude />
    PRIVATE METHOD __Add(u as T) AS VOID
        IF SELF:CheckLock()
            _internalList:Add(u)
        ENDIF

    PUBLIC VIRTUAL METHOD Add(u AS T) AS VOID
        SELF:__Add(u)
        RETURN

    /// <exclude />
    PUBLIC METHOD Delete(position AS INT) AS __ArrayBase<T>
        SELF:RemoveAt(position)
        SELF:__Add(SELF:DefaultValue)
        RETURN SELF

    /// <exclude />
    PUBLIC METHOD Insert(index AS INT,u AS T) AS VOID
        IF SELF:CheckLock()
            _internalList:RemoveAt(_internalList:Count - 1)
            _internalList:Insert((INT) index-__ARRAYBASE__ ,u)
        ENDIF
        RETURN

    /// <exclude />
    PUBLIC METHOD Insert(position AS INT) AS __ArrayBase<T>
        SELF:Insert( position, SELF:DefaultValue)
        RETURN SELF

    /// <exclude />
    PUBLIC METHOD RemoveAt(index AS INT) AS VOID
        IF SELF:CheckLock()
            _internalList:RemoveRange(index-__ARRAYBASE__,1 )
        ENDIF
        RETURN

    /// <exclude />
    PUBLIC VIRTUAL METHOD Resize(newSize AS INT) AS VOID
        IF SELF:CheckLock()
            IF newSize == 0
                _internalList:Clear()
            ELSE
                LOCAL count := _internalList:Count AS INT
                IF newSize <= count
                    _internalList:RemoveRange(newSize, count - newSize)
                ELSE
                    count+=1
                    DO WHILE count <= newSize
                        _internalList:Add(SELF:DefaultValue)
                        count++
                    ENDDO
                ENDIF
            ENDIF
        ENDIF
        RETURN



#endregion
    /// <inheritdoc />
    PUBLIC OVERRIDE METHOD ToString() AS STRING
        RETURN String.Format("[{0}]",_internalList:Count)

    INTERNAL METHOD Sort(startIndex AS INT, count AS INT, comparer AS IComparer<T>) AS VOID
        IF startIndex <= 0
            startIndex := 1
        ENDIF
        IF count < 0
            count := _internalList:Count - startIndex + __ARRAYBASE__
        ENDIF
        _internalList:Sort(startIndex-__ARRAYBASE__ ,count,comparer)
        RETURN

    INTERNAL METHOD Sort(comparer AS IComparer<T>) AS VOID
        _internalList:Sort(comparer)
        RETURN

    INTERNAL METHOD Swap(position AS INT, element AS T) AS T
        LOCAL original := _internalList[position - __ARRAYBASE__] AS T
        _internalList[ position - __ARRAYBASE__]:=element
        RETURN original

    INTERNAL METHOD Swap(position AS INT, element AS OBJECT) AS T
        //try
        VAR elementT := (T) (OBJECT) element
        RETURN SELF:Swap( position, elementT)
        //catch
        //	throw ArgumentException{"Parameter is of incorrect type "+element:GetType():FullName,nameof(element)}
        //end try


    INTERNAL METHOD Tail() AS T
        RETURN _internalList:LastOrDefault()

#region locking
    INTERNAL METHOD Lock(lLocked AS LOGIC) AS LOGIC
        LOCAL wasLocked AS LOGIC
        wasLocked := SELF:_islocked
        SELF:_islocked := lLocked
        RETURN wasLocked
    /// <summary>Is the array locked?</summary>
    [NOSHOW];
    PROPERTY Locked AS LOGIC GET _islocked

    INTERNAL METHOD CheckLock AS LOGIC
        IF SELF:_islocked
            THROW Error{Gencode.EG_PROTECTION}
        ENDIF
        RETURN ! SELF:_islocked

#endregion


#region operators
    /// <summary>Implicitely convert an array of USUALs to a typed array. Note that the usuals must contain a value of the correct type.</summary>
    STATIC OPERATOR IMPLICIT ( a AS ARRAY) AS __ArrayBase<T>
        IF a == NULL
            RETURN NULL
        ENDIF
        VAR aResult := __ArrayBase<T>{}
        LOCAL oErr AS Error
        FOREACH VAR u IN a
            LOCAL o := u AS OBJECT
            IF o IS T
                aResult:Add( (T) o)
            ELSE
                LOCAL nArg AS INT
                LOCAL oActType AS System.Type
                oActType := o:GetType()
                nArg := a:_internalList:IndexOf(u)+1
                oErr := Error{Gencode.EG_DATATYPE}
                oErr:Arg := "Array Element : "+nArg:ToString()
                oErr:Description := "Cannot convert array element " +nArg:ToString() + " from type "+oActType:ToString()+" to type "+TYPEOF(T):ToString()
                THROW oErr
            ENDIF
        NEXT
        RETURN aResult


    /// <summary>Implicitely convert a typed array to an array of USUALs.</summary>
    STATIC OPERATOR EXPLICIT ( a AS __ArrayBase<T> ) AS ARRAY
        VAR aResult := __Array{}
        FOREACH VAR o IN a
            aResult:Add(  o)
        NEXT
        RETURN aResult

    /// <summary>Implicitely convert a typed Array to an OBJECT[].</summary>
    STATIC OPERATOR IMPLICIT ( a AS __ArrayBase<T> ) AS OBJECT[]
        LOCAL aResult := List<OBJECT>{} AS List<OBJECT>
        FOREACH VAR o IN a
            aResult:Add( o)
        NEXT
        RETURN aResult:ToArray()

#endregion

    PROTECTED VIRTUAL METHOD DebuggerString() AS STRING
        LOCAL sb AS StringBuilder
        LOCAL cnt, tot AS LONG
        sb := StringBuilder{}
        sb:Append(SELF:ToString())
        sb:Append("{")
        tot := _internalList:Count
        cnt := 0
        FOREACH element AS T IN SELF:_internalList
            IF cnt > 0
                sb:Append(",")
            ENDIF
            if element != null
                sb:Append(element:ToString())
            else
                sb:Append("NULL")
            endif
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


#region ISerializable
    /// <inheritdoc/>
    PUBLIC VIRTUAL METHOD GetObjectData(info AS SerializationInfo, context AS StreamingContext) AS VOID
        IF info == NULL
            THROW System.ArgumentException{"info"}
        ENDIF
        info:AddValue("Count", SELF:Count)
        FOR VAR i := 1 TO SELF:Count
            info:AddValue("Item"+i:ToString(), _internalList[i-1])
        NEXT
        RETURN

    /// <include file="RTComments.xml" path="Comments/SerializeConstructor/*" />
    CONSTRUCTOR (info AS SerializationInfo, context AS StreamingContext)
        IF info == NULL
            THROW System.ArgumentException{"info"}
        ENDIF
        VAR count := info:GetInt32("Count")
        _internalList := List<T>{  }
        FOR VAR i := 1 TO count
            _internalList:Add( (T) info:GetValue("Item"+i:ToString(), typeof(T)))
        NEXT


#endregion

END	CLASS
END NAMESPACE
