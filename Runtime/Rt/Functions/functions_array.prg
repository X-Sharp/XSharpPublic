//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

begin namespace XSharp.Runtime
	#region functions
	/// <summary>
	/// Add a new element to the end of an array.
	/// </summary>
	/// <param name="a"></param>
	/// <param name="x"></param>
	/// <returns>
	/// </returns>
	FUNCTION AAdd(a AS ARRAY,x AS USUAL) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// Duplicate a multidimensional array.
	/// </summary>
	/// <param name="a"></param>
	/// <returns>
	/// </returns>
	FUNCTION AClone(a AS ARRAY) AS ARRAY
		/// THROW NotImplementedException{}
	RETURN NULL_ARRAY   

	/// <summary>
	/// Duplicate an array without its subarrays.
	/// </summary>
	/// <param name="a"></param>
	/// <returns>
	/// </returns>
	FUNCTION ACloneShallow(a AS ARRAY) AS ARRAY
		/// THROW NotImplementedException{}
	RETURN NULL_ARRAY   

	/// <summary>
	/// Delete an array element.
	/// </summary>
	/// <param name="a"></param>
	/// <param name="dwEl"></param>
	/// <returns>
	/// </returns>
	FUNCTION ADel(a AS ARRAY,dwEl AS DWORD) AS ARRAY
		/// THROW NotImplementedException{}
	RETURN NULL_ARRAY   

	/// <summary>
	/// </summary>
	/// <param name="a"></param>
	/// <returns>
	/// </returns>
	FUNCTION ADim(a AS ARRAY) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="a"></param>
	/// <returns>
	/// </returns>
	FUNCTION ADimPic(a AS ARRAY) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Insert an element into an array and assign it a NIL value.
	/// </summary>
	/// <param name="a"></param>
	/// <param name="dwEl"></param>
	/// <returns>
	/// </returns>
	FUNCTION AIns(a AS ARRAY,dwEl AS DWORD) AS ARRAY
		/// THROW NotImplementedException{}
	RETURN NULL_ARRAY   

	/// <summary>
	/// Return the number of elements in an array.
	/// </summary>
	/// <param name="a"></param>
	/// <returns>
	/// </returns>
	FUNCTION ALen(a AS ARRAY) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="a"></param>
	/// <returns>
	/// </returns>
	FUNCTION AMemSize(a AS ARRAY) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="a"></param>
	/// <returns>
	/// </returns>
	FUNCTION APageCount(a AS ARRAY) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Removes write protection from an entire array.
	/// </summary>
	/// <param name="a"></param>
	/// <returns>
	/// </returns>
	FUNCTION ArrayDeProtect(a AS ARRAY) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Read an array element.
	/// </summary>
	/// <param name="a"></param>
	/// <param name="dwEl"></param>
	/// <returns>
	/// </returns>
	FUNCTION ArrayGet(a AS ARRAY,dwEl AS DWORD) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// </summary>
	/// <param name="a"></param>
	/// <param name="dwEl"></param>
	/// <returns>
	/// </returns>
	UNSAFE FUNCTION ArrayGetPtr(a AS ARRAY,dwEl AS DWORD) AS PTR
		/// THROW NotImplementedException{}
	RETURN NULL   

	/// <summary>
	/// Protect an array from change in all functions except the one in which it was declared.
	/// </summary>
	/// <param name="a"></param>
	/// <returns>
	/// </returns>
	FUNCTION ArrayProtect(a AS ARRAY) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Write a value to an array element.
	/// </summary>
	/// <param name="a"></param>
	/// <param name="dwEl"></param>
	/// <param name="u"></param>
	/// <returns>
	/// </returns>
	FUNCTION ArrayPut(a AS ARRAY,dwEl AS DWORD,u AS USUAL) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// Store an array to a buffer.
	/// </summary>
	/// <param name="a"></param>
	/// <param name="Buff"></param>
	/// <param name="dwLen"></param>
	/// <returns>
	/// </returns>
	FUNCTION ArrayStore(a AS ARRAY,Buff AS PTR,dwLen AS DWORD) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Replace an array element with a new value and return the old value.
	/// </summary>
	/// <param name="a"></param>
	/// <param name="dwEl"></param>
	/// <param name="u"></param>
	/// <returns>
	/// </returns>
	FUNCTION ArraySwap(a AS ARRAY,dwEl AS DWORD,u AS USUAL) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// Scan a sorted array until a value is found or a code block returns 0.
	/// </summary>
	/// <param name="a"></param>
	/// <param name="x"></param>
	/// <returns>
	/// </returns>
	FUNCTION AScanBin(a AS ARRAY,x AS USUAL) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Scan a sorted array until there is an exact match or a code block returns 0.
	/// </summary>
	/// <param name="a"></param>
	/// <param name="x"></param>
	/// <returns>
	/// </returns>
	FUNCTION AScanBinExact(a AS ARRAY,x AS USUAL) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Grow or shrink an array.
	/// </summary>
	/// <param name="a"></param>
	/// <param name="dwDim"></param>
	/// <returns>
	/// </returns>
	FUNCTION ASize(a AS ARRAY,dwDim AS DWORD) AS ARRAY
		/// THROW NotImplementedException{}
	RETURN NULL_ARRAY   

	/// <summary>
	/// Return the highest numbered element of an array.
	/// </summary>
	/// <param name="a"></param>
	/// <returns>
	/// </returns>
	FUNCTION ATail(a AS ARRAY) AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// Copy a typed dynamic object to static allocated memory.
	/// </summary>
	/// <param name="a"></param>
	/// <returns>
	/// </returns>
	FUNCTION DynToOldSpaceArray(a AS ARRAY) AS ARRAY
		/// THROW NotImplementedException{}
	RETURN NULL_ARRAY   

	/// <summary>
	/// Check to see if a typed dynamic object is static.
	/// </summary>
	/// <param name="a"></param>
	/// <returns>
	/// </returns>
	FUNCTION IsOldSpaceArray(a AS ARRAY) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <param name="a"></param>
	/// <returns>
	/// </returns>
	FUNCTION OldSpaceFreeArray(a AS ARRAY) AS VOID
		/// THROW NotImplementedException{}
	RETURN 

	#endregion
end namespace