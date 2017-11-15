//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System.Collections
using System.Collections.Generic
using System.Linq
using System.Diagnostics
USING XSharp
begin namespace XSharp	

[DebuggerDisplay("USUAL{ToString(),nq}", Type := "ARRAY")];
[DebuggerTypeProxy(typeof(ArrayDebugView))];
public sealed class __Array INHERIT __ArrayBase<__Usual>

	CONSTRUCTOR()
		SUPER()

	CONSTRUCTOR(capacity AS INT)
		SUPER(capacity)

	CONSTRUCTOR( elements AS object[] )
		self()
		if elements == null
			throw ArgumentNullException{nameof(elements)}
		endif
		foreach element AS object in elements
			if element == null
				_internalList:add(default(__Usual))
			else
				_internalList:Add( __Usual{ element})
			endif
		next
	return

	public static method ArrayCreate(dimensions params int[] ) AS __Array
		local count := dimensions:Length AS int
		if count <= 0
			throw ArgumentException{"No dimensions provided."}
		endif
		local initializer := object[]{dimensions[1]} AS object[]
		local arrayNew AS __Array
		arrayNew := __Array{initializer}

		if count > 1
			local i AS int
			for i:=0+__ARRAYBASE__  upto dimensions[1]-1+__ARRAYBASE__
				local newParams := int[]{count-1} AS int[]
				Array.Copy(dimensions,1,newParams,0,count-1)
				arrayNew:_internalList[i-__ARRAYBASE__ ] := ArrayCreate(newParams)
			next
		endif
	return arrayNew

	public static method __ArrayNew( dimensions params int[] ) AS __Array
		local newArray AS __Array
		if dimensions:Length != 0 
			newArray := __ArrayNewHelper(dimensions,1)
		else
			newArray := __Array{}
		endif
	return newArray

	PUBLIC STATIC METHOD __ArrayNewHelper(dimensions AS INT[], currentDim AS INT) AS __Array
		local capacity := dimensions[currentDim-1] AS int // one based ?
		local newArray := __Array{capacity} AS __Array

		if currentDim != dimensions:Length
			local nextDim := currentDim+1 AS int
			local index   := 1 AS int
			do while index <= capacity
			    newArray:Add(__Usual{__ArrayNewHelper(dimensions,nextDim)})
				index+=1
			enddo
			return newArray
		endif
		local i AS int
		for i:=1 upto capacity
			newArray:Add(default(__Usual))
		next
	return newArray

    ///
    /// <Summary>Access the array element using ZERO based array index</Summary>
    ///
	public method __GetElement(index params int[]) AS __USUAL
		local indexLength := index:Length AS int
		local currentArray := self AS __Array
		local i AS int

		for i:= 1  upto indexLength  -1 // walk all but the last level
			local u := currentArray:_internalList[ index[i] ] AS __Usual
			if u:IsNil 
				return u
			endif
			if u:UsualType != __UsualType.ARRAY
				throw InvalidOperationException{"out of range error."}
			endif
			currentArray := (__Array) u
		next
		return currentArray:_internalList[ index[i] ]

	PUBLIC METHOD __SetElement(u AS __Usual, index PARAMS INT[] ) AS __Usual
		// indices are 0 based
		IF SELF:CheckLock()
			local length := index:Length AS int
			local currentArray := self AS __Array

			For var i := 1 upto length-1
				local uArray := _internalList[index[i]] AS __Usual
				if !(uArray:UsualType == __UsualType.ARRAY)
					throw InvalidOperationException{"Out of range error."}
				endif
				currentArray := (__Array) uArray
			next
			currentArray:_internalList[index[length]] := u
		endif
	return u

	internal class ArrayDebugView
		private _value AS __Array
		public constructor (a AS __Array)
			_value := a
		//[DebuggerBrowsable(DebuggerBrowsableState.RootHidden)] ;
		public property Elements AS List<__Usual> GET _value:_internalList

	end class

END CLASS


PUBLIC CLASS __ArrayBase<T> IMPLEMENTS IEnumerable<T>, IArray where T IS NEW()
	INTERNAL _internalList AS List<T> 
	private _islocked AS LOGIC 
	#region constructors
	CONSTRUCTOR()
		_internalList := List<T>{}
	return  

	CONSTRUCTOR(capacity AS int)
		_internalList := List<T>{capacity}
		_internalList:AddRange(Enumerable.Repeat(default(T),capacity))
	return 

	CONSTRUCTOR( collection AS IEnumerable<T>)
		_internalList := List<T>{collection}
	return 

	CONSTRUCTOR( elements AS object[] )
		self()
		if elements == null
			throw ArgumentNullException{nameof(elements)}
		endif
		foreach element AS object in elements
			if element == null
				_internalList:add(default(T))
			else
				_internalList:Add( (T) element)
			endif
		next
	return

	constructor( elements AS T[] )
		_internalList := List<T>{elements}
	return
	#endregion

	#region properties
	public Property IsEmpty AS logic
		get
			return (_internalList:Count == 0)
		end get 
	end property

	public Property Length AS dword
		get
			return (dword)_internalList:Count
		end get
	end property
	#endregion

	#region Enumerators
	public method GetEnumerator() AS IEnumerator<T>
	return _internalList:GetEnumerator()

	public method IEnumerable.GetEnumerator() AS IEnumerator
	RETURN _internalList:GetEnumerator()

	#endregion


	#region Cloning
	public method Clone() AS IArray
		local aResult AS __ArrayBase<T>
		LOCAL nCount AS INT
		nCount := _internalList:Count
		aResult := __ArrayBase<T>{nCount}
		FOR VAR I := 0 to nCount-1
			var u := _internalList[i]
			aResult:_internalList[i] := u
		NEXT
		return aResult

	public method CloneShallow() AS IArray
		local aResult AS __ArrayBase<T>
		LOCAL nCount AS INT
		nCount := _internalList:Count
		aResult := __ArrayBase<T>{nCount}
		FOR VAR I := 0 to nCount-1
			aResult:_internalList[i] := _internalList[i]
		NEXT
		return aResult

	#endregion


	#region Indexers and to Get / Set Elements. 
    ///
    /// <Summary>Access the array element using ZERO based array index</Summary>
    ///
	public method __GetElement(index AS int) AS T
		return SELF:_internalList[ index ]

	public Method __SetElement(u AS T,index AS int) AS T
		IF SELF:CheckLock()
			_internalList[index]:=u
		ENDIF
	return u


	public Property self[index AS dword] AS T
		GET
			return self[ (INT) index]
		end get
		set
			SELF[ (int) index] := value
		end set
	end property

	public Property self[i AS int] AS T
		get
			if i<__ARRAYBASE__ || i > _internalList:Count
				throw ArgumentOutOfRangeException{}
			endif
			return _internalList[i - __ARRAYBASE__ ]
		end get
		set
			IF SELF:CheckLock()
				if i<__ARRAYBASE__|| i > _internalList:Count
					throw ArgumentOutOfRangeException{}
				endif
				_internalList[i-__ARRAYBASE__] := value
			ENDIF
		end set
	end property

	#endregion

	#region Insert and Delete elements
	public method Add(u AS T) AS void
		_internalList:Add(u)
	return

	public method Add(o AS object) AS void
		SELF:Add((T)o)
	return

	public method Insert(index AS int,o AS object) AS void
		SELF:Insert(index, (T)o)
	return

	public method Insert(index AS dword,o AS object) AS void
		SELF:Insert((int)index, (T)o)
	return

	public method Insert(index AS int,u AS T) AS void
		IF SELF:CheckLock()
			_internalList:Insert(index-__ARRAYBASE__ ,u)
		ENDIF
	return

	public method Insert(index AS dword,u AS T) AS void
		SELF:Insert( (int) index, u)
	return
		
	public method Insert(position AS int) AS __ArrayBase<T>
		self:Insert(position,default(T))
	return self

	public method Insert(position AS dword) AS __ArrayBase<T>
		self:Insert((int) position, default(T))
	return self

	public method RemoveAt(index AS int , count AS int) AS void
		IF SELF:CheckLock()
			_internalList:RemoveRange(index-__ARRAYBASE__ ,count)
		ENDIF
	return

	PUBLIC METHOD RemoveAt(index AS dword , count AS int) AS void 
		SELF:RemoveAt( (int) index, count)
	RETURN

	public method RemoveAt(index AS int) AS void
		IF SELF:CheckLock()
			_internalList:RemoveRange(index-__ARRAYBASE__,1 )
		ENDIF
	return

	public method RemoveAt(index AS dword) AS void
		SELF:RemoveAt( (int) index)
	return

 	public method Resize(newSize AS dword) AS void
		SELF:Resize( ( int) newSize)

	public method Resize(newSize AS int) AS void
		local count := self:Length AS dword
		IF SELF:CheckLock()
			if newSize < 0 
				throw ArgumentException{"Size must be greater or equal than zero."}
			endif
			if newSize == 0 
				_internalList:Clear()
			else
				if newSize <= count 
					_internalList:RemoveRange((int)newSize, (int)(count - newSize))
				else
					count+=1
					DO WHILE count <= newSize
						local u := T{} AS T
						_internalList:Add(u)
						count++
					enddo
				endif
			endif
		ENDIF
	RETURN



	#endregion

	public method ToString() AS string
	return string.Format("[{0}]",_internalList:Count)

	public method Sort(startIndex AS int, count AS int, comparer AS IComparer<T>) AS void
		_internalList:Sort(startIndex-__ARRAYBASE__ ,count,comparer)
	return

	public Method Swap(position AS int, element AS T) AS T
		RETURN Swap( (DWORD) position, element)

	public Method Swap(position AS dword, element AS T) AS T
		local original := _internalList[(int) position - __ARRAYBASE__] AS T
		_internalList[(int) position - __ARRAYBASE__]:=element
	return original



	public method Tail() AS T
		return _internalList.LastOrDefault()

	#region static function
	public static Method Copy(aSource AS __Array,aTarget AS __Array,parameter params int[] ) AS __Array
		throw NotImplementedException{"__Array.Copy is not implemented yet."}

	public Method Delete(position AS dword) AS __ArrayBase<T>
		SELF:RemoveAt(position)
		SELF:Add(T{})
	return SELF	



	#endregion

	#region locking
	METHOD Lock(lLocked AS LOGIC) AS LOGIC
		LOCAL wasLocked AS LOGIC
		wasLocked := SELF:_islocked
		SELF:_islocked := lLocked
		RETURN wasLocked

	PROPERTY Locked AS LOGIC GET _islocked
	PROTECTED METHOD CheckLock AS LOGIC
	IF SELF:_islocked
		THROW Error{Gencode.Protection}
	ENDIF
	RETURN ! SELF:_islocked

	#endregion

END CLASS

PUBLIC INTERFACE IArray
	public method Clone() AS IArray
	public method CloneShallow() AS IArray
	public method RemoveAt(pos as INT) as void
END INTERFACE
END NAMESPACE
