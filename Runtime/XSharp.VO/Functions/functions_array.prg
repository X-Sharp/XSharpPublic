//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using XSharp

/// <summary>
/// Create an uninitialized, one-dimensional __Array.
/// </summary>
/// <param name="dwDim">The number of elements in the new __Array.</param>
/// <returns>
/// An uninitialized of the given length.
/// </returns>
FUNCTION __ArrayCreate(dwDim AS DWORD) AS __Array 
	RETURN __Array{(int)dwDim}

/// <summary>
/// Create an initialized __Array.
/// </summary>
/// <param name="dwDim"></param>
/// <param name="ptrBuff"></param>
/// <returns>
/// </returns>
unsafe FUNCTION __ArrayInit(dwDim AS DWORD,ptrBuff AS PTR) AS __Array
	/// THROW NotImplementedException{}
RETURN NULL


	/// <summary>
	/// Add a new element to the end of an __Array.
	/// </summary>
	/// <param name="a"></param>
	/// <param name="x"></param>
	/// <returns>
	/// </returns>
	FUNCTION AAdd(a AS __Array,x AS __USUAL) AS __USUAL
		a:Add(x)
	RETURN x 

	/// <summary>
	/// Duplicate a multidimensional __Array.
	/// </summary>
	/// <param name="a"></param>
	/// <returns>
	/// </returns>
	FUNCTION AClone(a AS __Array) AS __Array
		RETURN (__Array) a:Clone()

	/// <summary>
	/// Duplicate an __Array without its sub__Arrays.
	/// </summary>
	/// <param name="a"></param>
	/// <returns>
	/// </returns>
	FUNCTION ACloneShallow(a AS __Array) AS __Array
		RETURN (__Array) a:CloneShallow()

	/// <summary>
	/// Delete an __Array element.
	/// </summary>
	/// <param name="a"></param>
	/// <param name="dwEl"></param>
	/// <returns>
	/// </returns>
	FUNCTION ADel(a AS __Array,dwEl AS DWORD) AS __Array
		a:RemoveAt(dwEl)  
	RETURN a

	/// <summary>
	/// </summary>
	/// <param name="a"></param>
	/// <returns>
	/// </returns>
	FUNCTION ADim(a AS __Array) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="a"></param>
	/// <returns>
	/// </returns>
	FUNCTION ADimPic(a AS __Array) AS STRING
		/// THROW NotImplementedException{}
	RETURN String.Empty

	/// <summary>
	/// Insert an element into an __Array and assign it a NIL value.
	/// </summary>
	/// <param name="a"></param>
	/// <param name="dwEl"></param>
	/// <returns>
	/// </returns>
	FUNCTION AIns(a AS __Array,dwEl AS DWORD) AS __Array
		a:Insert(dwEl)
	RETURN a

	/// <summary>
	/// Return the number of elements in an __Array.
	/// </summary>
	/// <param name="a"></param>
	/// <returns>
	/// </returns>
	FUNCTION ALen(a AS __Array) AS DWORD
		RETURN a:Length

	/// <summary>
	/// </summary>
	/// <param name="a"></param>
	/// <returns>
	/// </returns>
	FUNCTION AMemSize(a AS __Array) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="a"></param>
	/// <returns>
	/// </returns>
	FUNCTION APageCount(a AS __Array) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Removes write protection from an entire __Array.
	/// </summary>
	/// <param name="a"></param>
	/// <returns>
	/// </returns>
	FUNCTION __ArrayDeProtect(a AS __Array) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Read an __Array element.
	/// </summary>
	/// <param name="a"></param>
	/// <param name="dwEl"></param>
	/// <returns>
	/// </returns>
    
	FUNCTION __ArrayGet(a AS __Array,dwEl AS DWORD) AS __USUAL
	return a:__GetElement( (int) dwEl-1)
    
	/// <summary>
	/// </summary>
	/// <param name="a"></param>
	/// <param name="dwEl"></param>
	/// <returns>
	/// </returns>
	UNSAFE FUNCTION __ArrayGetPtr(a AS __Array,dwEl AS DWORD) AS PTR
		/// THROW NotImplementedException{}
	RETURN NULL   

	/// <summary>
	/// Protect an __Array from change in all functions except the one in which it was declared.
	/// </summary>
	/// <param name="a"></param>
	/// <returns>
	/// </returns>
	FUNCTION __ArrayProtect(a AS __Array) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Write a value to an __Array element.
	/// </summary>
	/// <param name="a"></param>
	/// <param name="dwEl"></param>
	/// <param name="u"></param>
	/// <returns>
	/// </returns>
	FUNCTION __ArrayPut(a AS __Array,dwEl AS DWORD,u AS __USUAL) AS __USUAL
		a:__SetElement(u, (int)dwEl -1)
	RETURN u

	/// <summary>
	/// Store an __Array to a buffer.
	/// </summary>
	/// <param name="a"></param>
	/// <param name="Buff"></param>
	/// <param name="dwLen"></param>
	/// <returns>
	/// </returns>
	UNSAFE FUNCTION __ArrayStore(a AS __Array,Buff AS PTR,dwLen AS DWORD) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Replace an __Array element with a new value and return the old value.
	/// </summary>
	/// <param name="a"></param>
	/// <param name="dwEl"></param>
	/// <param name="u"></param>
	/// <returns>
	/// </returns>
	FUNCTION __ArraySwap(a AS __Array,dwEl AS DWORD,u AS __USUAL) AS __USUAL
	RETURN a:Swap(dwEl, u)

	/// <summary>
	/// Scan a sorted __Array until a value is found or a code block returns 0.
	/// </summary>
	/// <param name="a"></param>
	/// <param name="x"></param>
	/// <returns>
	/// </returns>
	FUNCTION AScanBin(a AS __Array,x AS __USUAL) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Scan a sorted __Array until there is an exact match or a code block returns 0.
	/// </summary>
	/// <param name="a"></param>
	/// <param name="x"></param>
	/// <returns>
	/// </returns>
	FUNCTION AScanBinExact(a AS __Array,x AS __USUAL) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Grow or shrink an __Array.
	/// </summary>
	/// <param name="a"></param>
	/// <param name="dwDim"></param>
	/// <returns>
	/// </returns>
	FUNCTION ASize(a AS __Array,dwDim AS DWORD) AS __Array
		a:Resize(dwDim) 
		RETURN a  

	FUNCTION ASize<T>(a AS __ArrayBase<T>,dwDim AS DWORD) AS __ArrayBase<T> WHERE T IS NEW()
		a:Resize(dwDim) 
		RETURN a  


	/// <summary>
	/// Return the highest numbered element of an __Array.
	/// </summary>
	/// <param name="a"></param>
	/// <returns>
	/// </returns>
	FUNCTION ATail(a AS __Array) AS __USUAL
		RETURN a:Tail()

	/// <summary>
	/// Return the highest numbered element of an __Array.
	/// </summary>
	/// <param name="a"></param>
	/// <returns>
	/// </returns>
	FUNCTION ATail<T>(a AS __ArrayBase<T>) AS T where T is new()
		RETURN a:Tail()


