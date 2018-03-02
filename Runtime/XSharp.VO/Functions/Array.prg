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
FUNCTION ArrayCreate(dwDim AS DWORD) AS __Array 
	RETURN __Array{(int)dwDim}

/// <summary>
/// Create an initialized __Array.
/// </summary>
/// <param name="dwDim"></param>
/// <param name="ptrBuff"></param>
/// <returns>
/// </returns>
unsafe FUNCTION ArrayInit(dwDim AS DWORD,ptrBuff AS PTR) AS __Array 
	/// THROW NotImplementedException{}
RETURN NULL


	/// <summary>
	/// Add a new element to the end of an Array.
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
	/// Duplicate an Array without its sub__Arrays.
	/// </summary>
	/// <param name="a"></param>
	/// <returns>
	/// </returns>
	FUNCTION ACloneShallow(a AS __Array) AS __Array
		RETURN (__Array) a:CloneShallow()

	/// <summary>
	/// Delete an Array element.
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
	/// Insert an element into an Array and assign it a NIL value.
	/// </summary>
	/// <param name="a"></param>
	/// <param name="dwEl"></param>
	/// <returns>
	/// </returns>
	FUNCTION AIns(a AS __Array,dwEl AS DWORD) AS __Array
		a:Insert(dwEl)
	RETURN a

	/// <summary>
	/// Return the number of elements in an Array.
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
	FUNCTION ArrayDeProtect(a AS __Array) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Read an Array element.
	/// </summary>
	/// <param name="a"></param>
	/// <param name="dwEl"></param>
	/// <returns>
	/// </returns>
    
	FUNCTION ArrayGet(a AS __Array,dwEl AS DWORD) AS __USUAL
	return a:__GetElement( (int) dwEl-1)
    
	/// <summary>
	/// </summary>
	/// <param name="a"></param>
	/// <param name="dwEl"></param>
	/// <returns>
	/// </returns>
	UNSAFE FUNCTION ArrayGetPtr(a AS __Array,dwEl AS DWORD) AS PTR
		/// THROW NotImplementedException{}
	RETURN NULL   

	/// <summary>
	/// Protect an Array from change in all functions except the one in which it was declared.
	/// </summary>
	/// <param name="a"></param>
	/// <returns>
	/// </returns>
	FUNCTION ArrayProtect(a AS __Array) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Write a value to an Array element.
	/// </summary>
	/// <param name="a"></param>
	/// <param name="dwEl"></param>
	/// <param name="u"></param>
	/// <returns>
	/// </returns>
	FUNCTION ArrayPut(a AS __Array,dwEl AS DWORD,u AS __USUAL) AS __USUAL
		a:__SetElement(u, (int)dwEl -1)
	RETURN u

	/// <summary>
	/// Store an Array to a buffer.
	/// </summary>
	/// <param name="a"></param>
	/// <param name="Buff"></param>
	/// <param name="dwLen"></param>
	/// <returns>
	/// </returns>
	UNSAFE FUNCTION ArrayStore(a AS __Array,Buff AS PTR,dwLen AS DWORD) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Replace an Array element with a new value and return the old value.
	/// </summary>
	/// <param name="a"></param>
	/// <param name="dwEl"></param>
	/// <param name="u"></param>
	/// <returns>
	/// </returns>
	FUNCTION ArraySwap(a AS __Array,dwEl AS DWORD,u AS __USUAL) AS __USUAL
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
	/// Grow or shrink an Array.
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
	/// Return the highest numbered element of an Array.
	/// </summary>
	/// <param name="a"></param>
	/// <returns>
	/// </returns>
	FUNCTION ATail(a AS __Array) AS __USUAL
		RETURN a:Tail()

	/// <summary>
	/// Return the highest numbered element of an Array.
	/// </summary>
	/// <param name="a"></param>
	/// <returns>
	/// </returns>
	FUNCTION ATail<T>(a AS __ArrayBase<T>) AS T where T is new()
		RETURN a:Tail()




	/// <summary>
	/// Copy elements from one __Array to another.
	/// </summary>
	/// <param name="a"></param>
	/// <param name="aDest"></param>
	/// <param name="nStart"></param>
	/// <param name="nCount"></param>
	/// <param name="nStartDest"></param>
	/// <returns>
	/// </returns>
	FUNCTION ACopy(a AS __Usual,aDest AS __Usual,nStart AS __Usual,nCount AS __Usual,nStartDest AS __Usual) AS __Array
		/// THROW NotImplementedException{}
	RETURN NULL_ARRAY   


	/// <summary>
	/// Fill __Array elements with a specified value.
	/// </summary>
	/// <param name="a"></param>
	/// <param name="x"></param>
	/// <param name="iStart"></param>
	/// <param name="iCount"></param>
	/// <returns>
	/// </returns>
	FUNCTION AFill(a AS __Usual,x AS __Usual,iStart AS __Usual,iCount AS __Usual) AS __Array
		/// THROW NotImplementedException{}
	RETURN NULL_ARRAY   


	/// <summary>
	/// Create an empty __Array.
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION ArrayBuild() AS __Array
	RETURN __Array{}

		/// <summary>
	/// Create an uninitialized __Array with the specified number of elements and dimensions.
	/// </summary>
	/// <param name="nDim"></param>
	/// <returns>
	/// </returns>
	FUNCTION ArrayNew(nDim AS __Usual) AS __Array
		/// THROW NotImplementedException{}
	RETURN NULL_ARRAY   


	/// <summary>
	/// To create an Array and fill its elements with a default value.
	/// </summary>
	/// <param name="x"></param>
	/// <param name="nCount"></param>
	/// <returns>
	/// </returns>
	FUNCTION AReplicate<T>(x AS __Usual,nCount AS DWORD) AS __ArrayBase<T> WHERE T IS NEW()
		var a:=__ArrayBase<T>{(int)nCount} 
		//Todo
		//__Array.ArrayFill(a,x)
	RETURN a




	/// <summary>
	/// Sort an Array.
	/// </summary>
	/// <param name="a"></param>
	/// <param name="iStart"></param>
	/// <param name="iCount"></param>
	/// <param name="cb"></param>
	/// <returns>
	/// </returns>
	FUNCTION ASort(a AS __Usual,iStart AS __Usual,iCount AS __Usual,cb AS __Usual) AS __Array
		/// THROW NotImplementedException{}
	return null_array  

/// <summary>
/// </summary>
/// <param name="a"></param>
/// <param name="n1"></param>
/// <returns>
/// </returns>
function _ArrayGetPoly(a as __Usual,n1 as __Usual) as __Usual
	/// THROW NotImplementedException{}
return	 __Usual._NIL   


/// <summary>
/// </summary>
/// <param name="a"></param>
/// <param name="x"></param>
/// <param name="n1"></param>
/// <returns>
/// </returns>
function _ArrayPutCollection(a as __Usual,x as __Usual,n1 as __Usual) as __Usual
	/// THROW NotImplementedException{}
return	 __Usual._NIL   


/// <summary>
/// </summary>
/// <param name="a"></param>
/// <param name="x"></param>
/// <param name="n1"></param>
/// <returns>
/// </returns>
function _ArrayPutPoly(a as __Usual,x as __Usual,n1 as __Usual) as __Usual
	/// THROW NotImplementedException{}
return	 __Usual._NIL  


	/// <summary>
	/// Execute a code block for each element in an __Array.
	/// </summary>
	/// <param name="a"></param>
	/// <param name="cb"></param>
	/// <param name="iStart"></param>
	/// <param name="iCount"></param>
	/// <returns>
	/// </returns>
	FUNCTION AEval(a AS __Usual,cb AS __Usual,iStart AS __Usual,iCount AS __Usual) AS __Usual
		/// THROW NotImplementedException{}
	RETURN __Usual._NIL   

	/// <summary>
	/// Execute a code block for each element in an __Array and assign the return value to each element in the __Array.
	/// </summary>
	/// <param name="a"></param>
	/// <param name="cb"></param>
	/// <param name="iStart"></param>
	/// <param name="iCount"></param>
	/// <returns>
	/// </returns>
	FUNCTION AEvalA(a AS __Usual,cb AS __Usual,iStart AS __Usual,iCount AS __Usual) AS __Usual
		/// THROW NotImplementedException{}
	RETURN __Usual._NIL   

	/// <summary>
	/// Execute a code block for each element in an __Array.
	/// </summary>
	/// <param name="c"></param>
	/// <param name="cod"></param>
	/// <param name="nStart"></param>
	/// <param name="nCount"></param>
	/// <returns>
	/// </returns>
	FUNCTION AEvalOld(c AS __Usual,cod AS __Usual,nStart AS __Usual,nCount AS __Usual) AS __Usual
		/// THROW NotImplementedException{}
	RETURN __Usual._NIL   


