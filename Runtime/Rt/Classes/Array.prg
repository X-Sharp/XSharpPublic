//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System.Collections
using System.Collections.Generic
using System.Linq
using Vulcan

BEGIN NAMESPACE Vulcan
	PUBLIC SEALED CLASS __Array IMPLEMENTS IEnumerable<__Usual>
		private internalList as List<__Usual> 
		#region constructors
		CONSTRUCTOR()
			internalList := List<__Usual>{}
		return 

		CONSTRUCTOR(capacity as int)
			internalList := List<__Usual>{capacity}
			internalList:AddRange(Enumerable.Repeat(__Usual._NIL,capacity))
		return 

		CONSTRUCTOR( collection as IEnumerable<__Usual>)
			internalList := List<__Usual>{collection}
		return 

		CONSTRUCTOR( elements as object[] )
		    self()
			if elements == null
				throw ArgumentNullException{"elemenst"}
			endif
			foreach element as object in elements
				internallist:Add(__Usual{element})
			next
		return

		constructor( elements as __Usual[] )
			internallist := List<__Usual>{elements}
		return
		#endregion
		#region properties
		public Property IsEmpty as logic
			get
				return (internallist:Count == 0)
			end get 
		end property

		public Property Length as dword
			get
				return (dword)internallist:Count
			end get
		end property
		#endregion
		#region helper functions
		public method Add(u as __Usual) as void
			internallist:Add(u)
		return
		#endregion
		public method GetEnumerator() as IEnumerator<__Usual>
		return internallist:GetEnumerator()

		public method IEnumerable.GetEnumerator() as IEnumerator
		return internallist:GetEnumerator()

		public static method __ArrayNew( dimensions params int[] ) as __Array 
			local new__Array as __Array
			if dimensions:Length != 0 
			   new__Array := __ArrayNewHelper(dimensions,1)
			else
			   new__Array := __Array{}
			endif
		return new__Array

		public static method __ArrayNewHelper(dimensions as int[], currentDim as int) as __Array
			local capacity := dimensions[currentDim-1] as int // one based ?
			local new__Array := __Array{capacity} as __Array

			if currentDim != dimensions:Length
			  local nextDim := currentDim+1 as int
			  local index   := 1 as int
			  do while index <= capacity
			     new__Array:Add(__Usual{__ArrayNewHelper(dimensions,nextDIm)})
				 index+=1
			  enddo
			  return new__Array
			endif
			local i as int
			for i:=1 upto capacity
				new__Array:Add(__Usual{})
			next
		return new__Array

		public method Clone() as __Array
			throw NotImplementedException{"__Array.Clone is not implemented yet."}

		public method CloneShallow() as __Array
			throw NotImplementedException{"__Array.CloneShallow is not implemented yet."}


		public method __GetElement(index params int[]) as __Usual
			local indexLength := index:Length as int
			local current__Array := self as __Array
			local i as int

			for i:=0+__ARRAYBASE__  upto indexLength-2+__ARRAYBASE__ 
				local u := current__Array:internalList[ index[i]-__ARRAYBASE__] as __Usual
				if u:IsNil
				   return __Usual._NIL
				endif
				if u:UsualType != UsualDataType.__Array
				   throw InvalidOperationException{"out of range error."}
				endif
				current__Array := (__Array) u
			next
			return current__Array:internalList[ index[i]-__ARRAYBASE__]

		public method Insert(index as dword,o as object) as void
			internallist:Insert((int)index-__ARRAYBASE__ ,__Usual{o})
		return

		public method Insert(index as dword,u as __Usual) as void
			internallist:Insert((int)index-__ARRAYBASE__ ,u)
		return
		
		public method Insert(position as dword) as __Array
			self:Insert(position,__Usual._NIL)
		return self

		public method RemoveAt(index as dword , count as int) as void
			internallist:RemoveRange((int)index-__ARRAYBASE__ ,count)
		return

		public method RemoveAt(index as dword) as void
			internallist:RemoveRange((int)index-__ARRAYBASE__,1 )
		return

		public method Resize(newSize as dword) as void
			local count := self:Length as dword
			if newSize == 0 
			   internallist:Clear()
			else
				if newSize <= count 
				   internallist:RemoveRange((int)newSize, (int)(count - newSize))
				else
				   count+=1
				   do while count <= newSize
					   local u := __Usual{} as __Usual
					   internallist:Add(u)
					   count++
			       enddo
				endif
			endif
		return

		public method ToString() as string
		return string.Format("{{[{0}]}}",internallist:Count)

		public method Sort(startIndex as int, count as int, comparer as IComparer<__Usual>) as void
			internallist:Sort(startIndex-__ARRAYBASE__ ,count,comparer)
		return

		public method Size(size as dword) as __Array
			if size < 0 
			   throw ArgumentException{"Size must be greate or equal zero."}
			endif
			if size > self:Length
			   local i as int
			   for i:=1-__ARRAYBASE__  upto size-__ARRAYBASE__ 
				   self:Add(__Usual{})
			   next
			else
			   do while self:Length > size
			      self:RemoveAt(self:Length)
			   enddo
			endif
	    return self

		public Method Swap(position as dword, element as __Usual) as __Usual
			local original := internallist[(int) position - __ARRAYBASE__] as __Usual
			internallist[(int) position - __ARRAYBASE__]:=element
		return original

		public Method __SetElement(u as __Usual,index as int) as __Usual
			internalList[index]:=u
		return u

		public Method __SetElement(u as __Usual, index params int[] ) as __Usual
			// indices are 0 based
			local length := index:Length as int
			local current__Array := self as __Array
			local i := 1 as int

			do while i <= length-__ARRAYBASE__ 
			   local u__Array := internalList[index[i - __ARRAYBASE__ ]] as __Usual
			   if !(u__Array:UsualType == UsualDataType.__Array)
				  throw InvalidOperationException{"Out of range error."}
			   endif
			   current__Array := (__Array)u__Array
			   i += 1
			enddo
			current__Array:internalList[index[i-1]] := u
		return u

		public Property self[index as dword] as __Usual 
			get
				VAR i := (int) index
				if i<__ARRAYBASE__ || i > System.Int32.MaxValue
					throw ArgumentOutOfRangeException{}
				endif
				return internalList[i - __ARRAYBASE__ ]
			end get
			set
				VAR i := (int) index
				if i<__ARRAYBASE__|| i > System.Int32.MaxValue
					throw ArgumentOutOfRangeException{}
				endif
				internalList[i-__ARRAYBASE__] := value
			end set
		end property

		public Property self[i as int] as __Usual
			get
				if i<__ARRAYBASE__ || i > System.Int32.MaxValue
					throw ArgumentOutOfRangeException{}
				endif
				return internalList[i - __ARRAYBASE__ ]
			end get
			set
				if i<__ARRAYBASE__|| i > System.Int32.MaxValue
					throw ArgumentOutOfRangeException{}
				endif
				internalList[i-__ARRAYBASE__] := value
			end set
		end property

		public method Tail() as __Usual
			if self:Length == 0 
			   return __Usual._NIL
			endif
		return internalList[internalList:Count-1]
		#endregion

		#region static function
		public static Method Copy(aSource as __Array,aTarget as __Array,parameter params int[] ) as __Array
			throw NotImplementedException{"__Array.Copy is not implemented yet."}

		public static Method __ArrayDelete(__ArrayToModify as __Array,position as dword) as __Array
			__ArrayToModify:RemoveAt(position)
			__ArrayToModify:Add(__Usual{})
		return __ArrayToModify	

		public static method __ArrayCreate(dimensions params int[] ) as __Array
			local count := dimensions:Length as int
			if count <= 0
			   throw ArgumentException{"No dimensions provided."}
			endif
			local initializer := object[]{dimensions[1]} as object[]
			local __ArrayNew as __Array
			__ArrayNew := __Array{initializer}

			if count > 1
			   local i as int
			   for i:=0+__ARRAYBASE__  upto dimensions[1]-1+__ARRAYBASE__
					local newParams := int[]{count-1} as int[]
					Array.Copy(dimensions,1,newParams,0,count-1)
					__ArrayNew:internalList[i-__ARRAYBASE__ ] := __ArrayCreate(newParams)
			   next
			endif
		return __ArrayNew

		public static method __ArrayFill(__ArraytoFill as __Array,elementValue as __Usual) as __Array
			return __ArrayFill(__ArrayToFill, elementValue, 0,  __ArrayToFill:internalList:Count)

		public static method __ArrayFill(__ArraytoFill as __Array,elementValue as __Usual,start as int) as __Array
			return __ArrayFill(__ArrayToFill, elementValue, start,  __ArrayToFill:internalList:Count- start)

		public static method __ArrayFill(__ArraytoFill as __Array,elementValue as __Usual,start as int, count as int) as __Array
			if start < 0 
				throw ArgumentException{"Start index must be greater or equal zero."}
			endif
			if count < 0 
				throw ArgumentException{"Count index must be greater or equal zero."}
			endif
			if __ArrayToFill:internalList:Count > 0
				local i as int
				for i:= start  upto start + count
					__ArraytoFill:internalList[i-__ARRAYBASE__] := elementValue
				next
			endif
		return __ArraytoFill
		#endregion

	END CLASS
END NAMESPACE