//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System.Collections
using System.Collections.Generic
using System.Linq

BEGIN NAMESPACE Vulcan
	PUBLIC SEALED CLASS __Array IMPLEMENTS IEnumerable<__Usual>
		private internalList as List<__Usual> 
		#region constructors
		CONSTRUCTOR()
			internalList := List<__Usual>{}
		return 

		CONSTRUCTOR(capacity as int)
			internalList := List<__Usual>{capacity}
			internalList:AddRange(Enumerable.Repeat(nil,capacity))
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

		public static method ArrayNew( dimensions params int[] ) as __Array 
			local newArray as __Array
			if dimensions:Length != 0 
			   newArray := ArrayNewHelper(dimensions,1)
			else
			   newArray := __Array{}
			endif
		return newArray

		public static method ArrayNewHelper(dimensions as int[], currentDim as int) as __Array
			local capacity := dimensions[currentDim-1] as int // one based ?
			local newArray := __Array{capacity} as __Array

			if currentDim != dimensions:Length
			  local nextDim := currentDim+1 as int
			  local index   := 1 as int
			  do while index <= capacity
			     newArray:Add(__Usual{ArrayNewHelper(dimensions,nextDIm)})
				 index+=1
			  enddo
			  return newArray
			endif
			local i as int
			for i:=1 upto capacity
				newArray:Add(__Usual{})
			next
		return newArray

		public method Clone() as __Array
			throw NotImplementedException{"__Array.Clone is not implemented yet."}

		public method CloneShallow() as __Array
			throw NotImplementedException{"__Array.CloneShallow is not implemented yet."}

		//public method GetElement(index as dword) as __Usual
		//return self[index]

		public method GetElement(index params dword[]) as __Usual
			local length := index:Length as int
			local currentArray := self as __Array
			local i as int

			for i:=0+__ARRAYBASE__  upto length-2+__ARRAYBASE__ 
			    local ind := index[i] as dword
				local u := currentArray[index[i]] as __Usual
				if u:IsNil
				   return nil
				endif
				if u:UsualType !=UsualDataType.ARRAY
				   throw InvalidOperationException{"out of range error."}
				endif
				currentArray := (__Array)u
			next
		return currentArray[index[length]]

		public method Insert(index as dword,o as object) as void
			internallist:Insert((int)index-__ARRAYBASE__ ,__Usual{o})
		return

		public method Insert(index as dword,u as __Usual) as void
			internallist:Insert((int)index-__ARRAYBASE__ ,u)
		return
		public method Insert(position as dword) as __Array
			self:Insert(position,nil)
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

		public Method Swap(position as dword,element as __Usual)
			local original := self[position] as __Usual
			self[position]:=element
		return original

		public Method SetElement(u as __Usual,index as int)
			if u == 0
			   internalList[index] := __Usual{}
			   return u
			endif
			internalList[index]:=u
		return u

		public Method SetElement(u as __Usual, index params int[] ) as __Usual
			local length := index:Length as int
			local currentArray := self as __Array
			local i := 1 as int

			do while i <= length-__ARRAYBASE__ 
			   local uArray := (__Usual)internalList[index[i - __ARRAYBASE__ ]] as __Usual
			   if !(uArray:UsualType == UsualDataType.ARRAY)
				  throw InvalidOperationException{"Out of range error."}
			   endif
			   currentArray := (__Array)uArray
			   i += 1
			enddo
			currentArray:SetElement(u,index[i-1])
		return u

		public Property self[i as dword] as __Usual
			get
				if i<__ARRAYBASE__ || i > System.Int32.MaxValue
					throw ArgumentOutOfRangeException{}
				endif
				return internalList[(int)i - __ARRAYBASE__ ]
			end get
			set
				if i<__ARRAYBASE__|| i > System.Int32.MaxValue
					throw ArgumentOutOfRangeException{}
				endif
				internalList[(int)i-__ARRAYBASE__] := value
			end set
		end property

		public method Tail() as __Usual
			if self:Length == 0 
			   return nil
			endif
		return self[self:Length]
		#endregion
		#region static function
		public static Method Copy(aSource as __Array,aTarget as __Array,parameter params int[] ) as __Array
			throw NotImplementedException{"__Array.Copy is not implemented yet."}

		public static Method ArrayDelete(arrayToModify as __Array,position as dword)
			arrayToModify:RemoveAt(position)
			arrayToModify:Add(__Usual{})
		return arrayToModify	

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

			        //local newParams := int[]{count-1} as int[]
					//local j as int
					//j := __ARRAYBASE__
					//for j:=1+__ARRAYBASE__  upto count-1+__ARRAYBASE__
						//newParams[j-1+__ARRAYBASE__]:=dimensions[j+__ARRAYBASE__]
					//next
					arrayNew[(dword)i] := ArrayCreate(newParams)

			   next
			endif
		return arrayNew

		public static method ArrayFill(arraytoFill as __Array,elementValue as __Usual) as __Array
			local i as dword
			if arrayToFill:Length > 0
				for i:=0+__ARRAYBASE__ upto arrayToFill:Length-1+__ARRAYBASE__
					arraytoFill[i]:=(__Usual)elementValue
				next
			endif
		return arraytoFill

		public static method ArrayFill(arraytoFill as __Array,elementValue as __Usual,start as dword) as __Array
			local i as dword
			if start >= 0
				if arrayToFill:Length > 0
					for i:=start+__ARRAYBASE__  upto arrayToFill:Length-1+__ARRAYBASE__ 
						arraytoFill[i]:=(__Usual)elementValue
					next
				endif
			else
				throw ArgumentException{"Start index must be greater or equal zero."}
			endif
		return arraytoFill

		public static method ArrayFill(arraytoFill as __Array,elementValue as __Usual,start as dword,count as int) as __Array
			if start < 0 
				throw ArgumentException{"Start index must be greater or equal zero."}
			endif
			if count < 0 
				throw ArgumentException{"Count index must be greater or equal zero."}
			endif
			if arrayToFill:Length > 0
				local i as dword
				for i:=start+__ARRAYBASE__  upto start + count-1+__ARRAYBASE__ 
					arraytoFill[i]:=(__Usual)elementValue
				next
			endif
		return arraytoFill
		#endregion
	END CLASS
END NAMESPACE