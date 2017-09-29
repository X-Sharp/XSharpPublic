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
PUBLIC SEALED CLASS __Array IMPLEMENTS IEnumerable<__Usual>
	PRIVATE _internalList AS List<__Usual> 
	private _islocked as LOGIC 
	#region constructors
	CONSTRUCTOR()
		_internalList := List<__Usual>{}
	return  

	CONSTRUCTOR(capacity as int)
		_internalList := List<__Usual>{capacity}
		_internalList:AddRange(Enumerable.Repeat(default(__Usual),capacity))
	return 

	CONSTRUCTOR( collection as IEnumerable<__Usual>)
		_internalList := List<__Usual>{collection}
	return 

	CONSTRUCTOR( elements as object[] )
		self()
		if elements == null
			throw ArgumentNullException{nameof(elements)}
		endif
		foreach element as object in elements
			_internalList:Add(__Usual{element})
		next
	return

	constructor( elements as __Usual[] )
		_internalList := List<__Usual>{elements}
	return
	#endregion

	#region properties
	public Property IsEmpty as logic
		get
			return (_internalList:Count == 0)
		end get 
	end property

	public Property Length as dword
		get
			return (dword)_internalList:Count
		end get
	end property
	#endregion

	#region Enumerators
	public method GetEnumerator() as IEnumerator<__Usual>
	return _internalList:GetEnumerator()

	public method IEnumerable.GetEnumerator() as IEnumerator
	RETURN _internalList:GetEnumerator()

	#endregion

	#region Helpers for array creation

	public static method __ArrayNew( dimensions params int[] ) as __Array 
		local newArray as __Array
		if dimensions:Length != 0 
			newArray := __ArrayNewHelper(dimensions,1)
		else
			newArray := __Array{}
		endif
	return newArray

	public static method __ArrayNewHelper(dimensions as int[], currentDim as int) as __Array
		local capacity := dimensions[currentDim-1] as int // one based ?
		local newArray := __Array{capacity} as __Array

		if currentDim != dimensions:Length
			local nextDim := currentDim+1 as int
			local index   := 1 as int
			do while index <= capacity
			    newArray:Add(__Usual{__ArrayNewHelper(dimensions,nextDim)})
				index+=1
			enddo
			return newArray
		endif
		local i as int
		for i:=1 upto capacity
			newArray:Add(__Usual{})
		next
	return newArray
	#endregion

	#region Cloning
	public method Clone() as __Array
		local aResult as __Array
		LOCAL nCount as INT
		nCount := _internalList:Count
		aResult := __Array{nCount}
		FOR VAR I := 0 to nCount-1
			var u := _internalList[i]
			if u:UsualType == __UsualType.Array
				u := ((__Array) u):Clone()
			endif
			aResult:_internalList[i] := u
		NEXT
		return aResult

	public method CloneShallow() as __Array
		local aResult as __Array
		LOCAL nCount as INT
		nCount := _internalList:Count
		aResult := __Array{nCount}
		FOR VAR I := 0 to nCount-1
			aResult:_internalList[i] := _internalList[i]
		NEXT
		return aResult

	#endregion


	#region Indexers and to Get / Set Elements. 
    ///
    /// <Summary>Access the array element using ZERO based array index</Summary>
    ///
	public method __GetElement(index as int) as __Usual
		return SELF:_internalList[ index ]

    ///
    /// <Summary>Access the array element using ZERO based array index</Summary>
    ///
	public method __GetElement(index params int[]) as __Usual
		local indexLength := index:Length as int
		local currentArray := self as __Array
		local i as int

		for i:= 1  upto indexLength  -1 // walk all but the last level
			local u := currentArray:_internalList[ index[i] ] as __Usual
			if u:IsNil 
				return u
			endif
			if u:UsualType != __UsualType.ARRAY
				throw InvalidOperationException{"out of range error."}
			endif
			currentArray := (__Array) u
		next
		return currentArray:_internalList[ index[i] ]

	public Method __SetElement(u as __Usual,index as int) AS __Usual
		IF SELF:CheckLock()
			_internalList[index]:=u
		ENDIF
	return u

	public Method __SetElement(u as __Usual, index params int[] ) as __Usual
		// indices are 0 based
		IF SELF:CheckLock()
			local length := index:Length as int
			local currentArray := self as __Array

			For var i := 1 upto length-1
				local uArray := _internalList[index[i]] as __Usual
				if !(uArray:UsualType == __UsualType.ARRAY)
					throw InvalidOperationException{"Out of range error."}
				endif
				currentArray := (__Array) uArray
			next
			currentArray:_internalList[index[length]] := u
		endif
	return u

	public Property self[index as dword] as __Usual 
		GET
			return self[ (INT) index]
		end get
		set
			SELF[ (int) index] := value
		end set
	end property

	public Property self[i as int] AS __USUAL
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
	public method Add(u as __Usual) as void
		_internalList:Add(u)
	return

	public method Add(o as object) as void
		SELF:Add(__Usual{o})
	return

	public method Insert(index as int,o as object) as void
		SELF:Insert(index, __usual{o})
	return

	public method Insert(index as dword,o as object) as void
		SELF:Insert((int)index, __usual{o})
	return

	public method Insert(index as int,u as __Usual) as void
		IF SELF:CheckLock()
			_internalList:Insert(index-__ARRAYBASE__ ,u)
		ENDIF
	return

	public method Insert(index as dword,u as __Usual) as void
		SELF:Insert( (int) index, u)
	return
		
	public method Insert(position as int) as __Array
		self:Insert(position,default(__Usual))
	return self

	public method Insert(position as dword) as __Array
		self:Insert((int) position, default(__Usual))
	return self

	public method RemoveAt(index as int , count as int) as void
		IF SELF:CheckLock()
			_internalList:RemoveRange(index-__ARRAYBASE__ ,count)
		ENDIF
	return

	PUBLIC METHOD RemoveAt(index as dword , count as int) as void 
		SELF:RemoveAt( (int) index, count)
	RETURN

	public method RemoveAt(index as int) as void
		IF SELF:CheckLock()
			_internalList:RemoveRange(index-__ARRAYBASE__,1 )
		ENDIF
	return

	public method RemoveAt(index as dword) as void
		SELF:RemoveAt( (int) index)
	return

 	public method Resize(newSize as dword) as void
		SELF:Resize( ( int) newSize)

	public method Resize(newSize as int) as void
		local count := self:Length as dword
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
						local u := __Usual{} as __Usual
						_internalList:Add(u)
						count++
					enddo
				endif
			endif
		ENDIF
	RETURN



	#endregion

	public method ToString() as string
	return string.Format("[{0}]",_internalList:Count)

	public method Sort(startIndex as int, count as int, comparer as IComparer<__Usual>) as void
		_internalList:Sort(startIndex-__ARRAYBASE__ ,count,comparer)
	return

	public Method Swap(position as int, element as __Usual) AS __Usual
		RETURN Swap( (DWORD) position, element)

	public Method Swap(position as dword, element as __Usual) AS __Usual
		local original := _internalList[(int) position - __ARRAYBASE__] AS __Usual
		_internalList[(int) position - __ARRAYBASE__]:=element
	return original



	public method Tail() as __Usual
		return _internalList.LastOrDefault()

	#region static function
	public static Method Copy(aSource as __Array,aTarget as __Array,parameter params int[] ) as __Array
		throw NotImplementedException{"__Array.Copy is not implemented yet."}

	public Method Delete(position as dword) AS __Array
		SELF:RemoveAt(position)
		SELF:Add(__Usual{})
	return SELF	


	public static method ArrayCreate(dimensions params int[] ) as __Array
		local count := dimensions:Length as int
		if count <= 0
			throw ArgumentException{"No dimensions provided."}
		endif
		local initializer := object[]{dimensions[1]} as object[]
		local arrayNew as __Array
		arrayNew := __Array{initializer}

		if count > 1
			local i as int
			for i:=0+__ARRAYBASE__  upto dimensions[1]-1+__ARRAYBASE__
				local newParams := int[]{count-1} as int[]
				Array.Copy(dimensions,1,newParams,0,count-1)
				arrayNew:_internalList[i-__ARRAYBASE__ ] := ArrayCreate(newParams)
			next
		endif
	return arrayNew

	#endregion

	#region locking
	METHOD Lock(lLocked AS LOGIC) AS LOGIC
		LOCAL wasLocked AS LOGIC
		wasLocked := SELF:_islocked
		SELF:_islocked := lLocked
		RETURN wasLocked

	PROPERTY Locked AS LOGIC GET _islocked
	PRIVATE METHOD CheckLock AS LOGIC
	IF SELF:_islocked
		THROW Error{Gencode.Protection}
	ENDIF
	RETURN ! SELF:_islocked

	#endregion

	internal class ArrayDebugView
		private _value as __Array
		public constructor (a as __Array)
			_value := a
		//[DebuggerBrowsable(DebuggerBrowsableState.RootHidden)] ;
		public property Elements as List<__Usual> GET _value:_internalList
				

	end class

END CLASS
END NAMESPACE
