//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System.Collections
using System.Collections.Generic
using System.Linq
using System.Diagnostics
using XSharp
begin namespace XSharp	
	/// <summary>Internal type that implements the new TYPED ARRAY type.<br/>
	/// This type has methods and properties that normally are never directly called from user code.
	/// </summary>
	public class __ArrayBase<T> implements IEnumerable<T> where T is new()
		internal _internalList as List<T> 
		private _islocked as logic 
		#region constructors
			/// <summary>Create an empty array</summary>
			constructor()
				_internalList := List<T>{}
				return  
			
			/// <summary>Create an array with a certain number of elements. Each element will be filled with a default value.</summary>
			constructor(capacity as dword)
				_internalList := List<T>{ (int) capacity}
				_internalList:AddRange(Enumerable.Repeat(default(T),(int) capacity))
				return 
			
			/// <summary>Create an array and fill it with elements from an existing collection.</summary>
			constructor( collection as IEnumerable<T>)
				_internalList := List<T>{collection}
				return 
			
			/// <summary>Create an array and fill it with elements from an existing .Net array of objects. Note that the objects must be of the right type.</summary>
			constructor( elements as object[] )
				self()
				if elements == null
					throw ArgumentNullException{nameof(elements)}
				endif
				foreach element as object in elements
					if element == null
						_internalList:add(default(T))
					elseif element is T
						_internalList:Add( (T) element)
					else
						throw ArgumentException{"Object array contains element of incorrect type "+element:GetType():FullName}
					endif
				next
				return
			
			/// <summary>Create an array and fill it with elements from an existing .Net array.</summary>
			constructor( elements as T[] )
				_internalList := List<T>{elements}
				return
		#endregion
		
		#region properties
			/// <summary>Is the array empty.</summary>
			public property IsEmpty as logic
				get
					return (_internalList:Count == 0)
				end get 
			end property
			/// <summary>Length of the array.</summary>
			public property Length as dword
				get
					return (dword)_internalList:Count
				end get
			end property
		#endregion
		
		#region Enumerators
			public method IEnumerable<T>.GetEnumerator() as IEnumerator<T>
				return _internalList:GetEnumerator()
			
			public method IEnumerable.GetEnumerator() as IEnumerator
				return _internalList:GetEnumerator()
			
		#endregion
		
		
		#region Cloning
			
			internal method Clone() as __ArrayBase<T>
				local aResult as __ArrayBase<T>
				local nCount as dword
				nCount := (dword) _internalList:Count
				aResult := (__ArrayBase<T>) Activator.CreateInstance(SELF:GetType(), NULL)
				ASIze(aResult, nCount)
				FOR var I := 0 TO nCount-1
					aResult:_internalList[i] := _internalList[i]
				next
				return aResult
			
		#endregion
		
		
		#region Indexers and to Get / Set Elements. 
			///
			/// <summary>Access the array element using ZERO based array index</summary>
			///
			public method __GetElement(index as int) as T
				return self:_internalList[ index ]
			
			/// <summary>Set array elements with a ZERO based array index</summary>
			public method __SetElement(u as T,index as int) as T
				if self:CheckLock()
					_internalList[index]:=u
				endif
				return u
			
			// Note: Zero based !			
			/// <summary>Get/Set array elements with a ZERO based array index</summary>
			public property self[i as dword] as T
				get
					if  i > _internalList:Count-1
						throw ArgumentOutOfRangeException{}
					endif
					return _internalList[(int) i ]
				end get
				set
					if self:CheckLock()
						if i > _internalList:Count-1
							throw ArgumentOutOfRangeException{}
						endif
						_internalList[(int) i] := value
					endif
				end set
			end property
			
		#endregion
		
		#region Insert and Delete elements
			internal method Add(u as T) as void
				if self:CheckLock()
					_internalList:Add(u)
				endif
				return
			
			internal method Delete(position as dword) as __ArrayBase<T>
				self:RemoveAt(position)
				self:Add(T{})
				return self	
			
			internal method Insert(index as dword,u as T) as void
				if self:CheckLock()
					_internalList:Insert((int) index-__ARRAYBASE__ ,u)
				endif
				return
			
			internal method Insert(position as dword) as __ArrayBase<T>
				self:Insert( position, default(T))
				return self
			
			
			internal method RemoveAt(index as dword) as void
				if self:CheckLock()
					_internalList:RemoveRange((int) index-__ARRAYBASE__,1 )
				endif
				return
			
			internal method Resize(newSize as dword) as void
				local count := self:Length as dword
				if self:CheckLock()
					if newSize == 0 
						_internalList:Clear()
					else
						if newSize <= count 
							_internalList:RemoveRange((int)newSize, (int)(count - newSize))
						else
							count+=1
							do while count <= newSize
								local u := T{} as T
								_internalList:Add(u)
								count++
							enddo
						endif
					endif
				endif
				return
			
			
			
		#endregion
		
		public method ToString() as string
			return string.Format("[{0}]",_internalList:Count)
		
		internal method Sort(startIndex as int, count as int, comparer as IComparer<T>) as void
			_internalList:Sort(startIndex-__ARRAYBASE__ ,count,comparer)
			return
		
		internal method Swap(position as int, element as T) as T
			return Swap( (dword) position, element)
		
		internal method Swap(position as dword, element as T) as T
			local original := _internalList[(int) position - __ARRAYBASE__] as T
			_internalList[(int) position - __ARRAYBASE__]:=element
			return original
		
		internal method Swap(position as int, element as object) as T
			//try
				var elementT := (T) (object) element
				return Swap( position, elementT)
			//catch
			//	throw ArgumentException{"Parameter is of incorrect type "+element:GetType():FullName,nameof(element)}
			//end try
		
		internal method Swap(position as dword, element as object) as T
			return self:Swap((int) position, element)		

		internal method Tail() as T
			return _internalList:LastOrDefault()
		
		#region locking
			internal method Lock(lLocked as logic) as logic
				local wasLocked as logic
				wasLocked := self:_islocked
				self:_islocked := lLocked
				return wasLocked
			/// <summary>Is the array locked?</summary>
			property Locked as logic get _islocked
			internal method CheckLock as logic
				if self:_islocked
					throw Error{Gencode.EG_Protection}
				endif
				return ! self:_islocked
			
		#endregion
		
		
		#region operators
		/// <summary>Implicitely convert an array of USUALs to a typed array. Note that the usuals must contain a value of the correct type.</summary>
		STATIC OPERATOR IMPLICIT ( a AS ARRAY) AS __ArrayBase<T> 
			VAR aResult := __ArrayBase<T>{}
			LOCAL oErr AS Error
			FOREACH VAR u IN a
				LOCAL o := u AS Object
				if o is T
					aResult:Add( (T) o)
				ELSE
					LOCAL nArg AS INT
					LOCAL oActType AS System.Type
					oActType := o:GetType()
					nArg := a:_internalList:IndexOf(u)+1
					oErr := Error{GenCode.EG_DATATYPE}
					oErr:Arg := "Array Element : "+nArg:ToString()
					oErr:Description := "Cannot convert array element " +nArg:ToString() + " from type "+oActType:ToString()+" to type "+TYPEOF(T):ToString()
					throw oErr 
				endif
			NEXT
			RETURN aResult			


		/// <summary>Implicitely convert a typed array to an array of USUALs.</summary>
		STATIC OPERATOR IMPLICIT ( a AS __ArrayBase<T> ) AS ARRAY
			VAR aResult := __Array{}
			FOREACH VAR o IN a
				aResult:Add(  o)
			NEXT
			RETURN aResult			
			
		#endregion
		
	end	class
end namespace
