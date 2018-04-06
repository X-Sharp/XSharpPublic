//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using XSharp

/// <summary>
/// Create an uninitialized, one-dimensional Array.
/// </summary>
/// <param name="dwDim">The number of elements in the new Array.</param>
/// <returns>
/// An uninitialized of the given length.
/// </returns>
FUNCTION ArrayCreate(dwDim AS DWORD) AS Array 
	RETURN Array{(int)dwDim}


/// <summary>
/// Create an initialized Array.
/// </summary>
/// <param name="dwDim"></param>
/// <param name="ptrBuff"></param>
/// <returns>
/// </returns>
FUNCTION ArrayInit(dwDim AS DWORD, avalues REF USUAL[]) AS Array 
   LOCAL aTemp AS ARRAY
   LOCAL x AS INT
   
   IF dwDim > (DWORD) aValues:Length
      Throw Error.ArgumentError( nameof(dwDim), "Element too big")
   ENDIF
   
   aTemp := ArrayNew(aValues:Length)
   FOR x := 1 UPTO aValues:Length
      aTemp [x] := aValues[x] 
   NEXT
   RETURN aTemp

	/// <summary>
	/// Add a new element to the end of an Array.
	/// </summary>
	/// <param name="a"></param>
	/// <param name="x"></param>
	/// <returns>
	/// </returns>
	FUNCTION AAdd(a AS Array,x AS Usual) AS Usual
		a:Add(x)
	RETURN x 

	/// <summary>
	/// Duplicate a multidimensional Array.
	/// </summary>
	/// <param name="a"></param>
	/// <returns>
	/// </returns>
	FUNCTION AClone(a AS Array) AS Array
		RETURN (Array) a:Clone()

	/// <summary>
	/// Duplicate an Array without its subArrays.
	/// </summary>
	/// <param name="a"></param>
	/// <returns>
	/// </returns>
	FUNCTION ACloneShallow(a AS Array) AS Array
		RETURN (Array) a:CloneShallow()

	/// <summary>
	/// Delete an Array element.
	/// </summary>
	/// <param name="a"></param>
	/// <param name="dwEl"></param>
	/// <returns>
	/// </returns>
	FUNCTION ADel(a AS Array,dwEl AS DWORD) AS Array
		a:RemoveAt(dwEl)  
	RETURN a

	/// <summary>
	/// </summary>
	/// <param name="a"></param>
	/// <returns>
	/// </returns>
	FUNCTION ADim(a AS Array) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="a"></param>
	/// <returns>
	/// </returns>
	FUNCTION ADimPic(a AS Array) AS STRING
		/// THROW NotImplementedException{}
	RETURN String.Empty

	/// <summary>
	/// Insert an element into an Array and assign it a NIL value.
	/// </summary>
	/// <param name="a"></param>
	/// <param name="dwEl"></param>
	/// <returns>
	/// </returns>
	FUNCTION AIns(a AS Array,dwEl AS DWORD) AS Array
		a:Insert(dwEl)
	RETURN a

	/// <summary>
	/// Return the number of elements in an Array.
	/// </summary>
	/// <param name="a"></param>
	/// <returns>
	/// </returns>
	FUNCTION ALen(a AS Array) AS DWORD
		RETURN a:Length
	

	/// <summary>
	/// Removes write protection from an entire Array.
	/// </summary>
	/// <param name="a"></param>
	/// <returns>
	/// </returns>
	FUNCTION ArrayDeProtect(a AS Array) AS LOGIC
		return a:Lock(FALSE)

	/// <summary>
	/// Read an Array element.
	/// </summary>
	/// <param name="a"></param>
	/// <param name="dwEl"></param>
	/// <returns>
	/// </returns>
    
	FUNCTION ArrayGet(a AS Array,dwEl AS DWORD) AS Usual
		return a:__GetElement( (int) dwEl-1)
    
	
	/// <summary>
	/// Protect an Array from change in all functions except the one in which it was declared.
	/// </summary>
	/// <param name="a"></param>
	/// <returns>
	/// </returns>
	FUNCTION ArrayProtect(a AS Array) AS LOGIC
		return a:Lock(TRUE)

	/// <summary>
	/// Write a value to an Array element.
	/// </summary>
	/// <param name="a"></param>
	/// <param name="dwEl"></param>
	/// <param name="u"></param>
	/// <returns>
	/// </returns>
	FUNCTION ArrayPut(a AS Array,dwEl AS DWORD,u AS Usual) AS Usual
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
	UNSAFE FUNCTION ArrayStore(a AS Array,Buff AS PTR,dwLen AS DWORD) AS DWORD
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
	FUNCTION ArraySwap(a AS Array,dwEl AS DWORD,u AS Usual) AS Usual
	RETURN a:Swap(dwEl, u)

	/// <summary>
	/// Scan a sorted Array until a value is found or a code block returns 0.
	/// </summary>
	/// <param name="a"></param>
	/// <param name="x"></param>
	/// <returns>
	/// </returns>
	FUNCTION AScanBin(a AS Array,x AS Usual) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Scan a sorted Array until there is an exact match or a code block returns 0.
	/// </summary>
	/// <param name="a"></param>
	/// <param name="x"></param>
	/// <returns>
	/// </returns>
	FUNCTION AScanBinExact(a AS Array,x AS Usual) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Grow or shrink an Array.
	/// </summary>
	/// <param name="a"></param>
	/// <param name="dwDim"></param>
	/// <returns>
	/// </returns>
	FUNCTION ASize(a AS Array,dwDim AS DWORD) AS Array
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
	FUNCTION ATail(a AS Array) AS Usual
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
	/// Copy elements from one Array to another.
	/// </summary>
	/// <param name="a"></param>
	/// <param name="aDest"></param>
	/// <param name="nStart"></param>
	/// <param name="nCount"></param>
	/// <param name="nStartDest"></param>
	/// <returns>
	/// </returns>
	FUNCTION ACopy(a AS Usual,aDest AS Usual,nStart AS Usual,nCount AS Usual,nStartDest AS Usual) AS Array
		/// THROW NotImplementedException{}
	RETURN NULL_ARRAY   


	/// <summary>
	/// Fill Array elements with a specified value.
	/// </summary>
	/// <param name="a"></param>
	/// <param name="x"></param>
	/// <param name="iStart"></param>
	/// <param name="iCount"></param>
	/// <returns>
	/// </returns>
	FUNCTION AFill(a AS Usual,x AS Usual,iStart AS Usual,iCount AS Usual) AS Array
		/// THROW NotImplementedException{}
	RETURN NULL_ARRAY   


	/// <summary>
	/// Create an empty Array.
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION ArrayBuild() AS Array
		RETURN Array{}

	/// <summary>
	/// Create an uninitialized Array with the specified number of elements and dimensions.
	/// </summary>
	/// <param name="nDim"></param>
	/// <returns>
	/// </returns>
	FUNCTION ArrayNew(nDim PARAMS int[]) AS Array
		RETURN __Array.ArrayCreate(nDim)


	/// <summary>
	/// To create an Array and fill its elements with a default value.
	/// </summary>
	/// <param name="x"></param>
	/// <param name="nCount"></param>
	/// <returns>
	/// </returns>
	FUNCTION AReplicate<T>(x AS Usual,nCount AS DWORD) AS __ArrayBase<T> WHERE T IS NEW()
		var a:=__ArrayBase<T>{(int)nCount} 
		//Todo
		//Array.ArrayFill(a,x)
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
	FUNCTION ASort(a ,iStart ,iCount ,cb ) AS Array
		/// THROW NotImplementedException{}
	return null_array  

/// <summary>
/// </summary>
/// <param name="a"></param>
/// <param name="n1"></param>
/// <returns>
/// </returns>
function _ArrayGetPoly(a as Usual,n1 as Usual) as Usual
	/// THROW NotImplementedException{}
return	 NIL   

/// <summary>
/// </summary>
/// <param name="a"></param>
/// <param name="n1"></param>
/// <returns>
/// </returns>
function _ArrayGetCollection(a as Usual,n1 as Usual) as Usual
	/// THROW NotImplementedException{}
return	 NIL   

/// <summary>
/// </summary>
/// <param name="a"></param>
/// <param name="x"></param>
/// <param name="n1"></param>
/// <returns>
/// </returns>
function _ArrayPutCollection(a as Usual,x as Usual,n1 as Usual) as Usual
	/// THROW NotImplementedException{}
return	 NIL   


/// <summary>
/// </summary>
/// <param name="a"></param>
/// <param name="x"></param>
/// <param name="n1"></param>
/// <returns>
/// </returns>
function _ArrayPutPoly(a as Usual,x as Usual,n1 as Usual) as Usual
	/// THROW NotImplementedException{}
return	 NIL  


	/// <summary>
	/// Execute a code block for each element in an Array.
	/// </summary>
	/// <param name="a"></param>
	/// <param name="cb"></param>
	/// <param name="iStart"></param>
	/// <param name="iCount"></param>
	/// <returns>
	/// </returns>
	FUNCTION AEval(a AS Usual,cb AS Usual,iStart AS Usual,iCount AS Usual) AS Usual
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// Execute a code block for each element in an Array and assign the return value to each element in the Array.
	/// </summary>
	/// <param name="a"></param>
	/// <param name="cb"></param>
	/// <param name="iStart"></param>
	/// <param name="iCount"></param>
	/// <returns>
	/// </returns>
	FUNCTION AEvalA(a AS Usual,cb AS Usual,iStart AS Usual,iCount AS Usual) AS Usual
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// Execute a code block for each element in an Array.
	/// </summary>
	/// <param name="c"></param>
	/// <param name="cod"></param>
	/// <param name="nStart"></param>
	/// <param name="nCount"></param>
	/// <returns>
	/// </returns>
	FUNCTION AEvalOld(c AS Usual,cod AS Usual,nStart AS Usual,nCount AS Usual) AS Usual
		/// THROW NotImplementedException{}
	RETURN NIL   


