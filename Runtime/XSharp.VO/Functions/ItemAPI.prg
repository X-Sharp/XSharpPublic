//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

// Is this needed ?

using XSharp


/// <summary>
/// </summary>
/// <param name="ptrAny"></param>
/// <param name="dwElem"></param>
/// <returns>
/// </returns>
unsafe FUNCTION ItemArrayGet(ptrAny AS PTR,dwElem AS DWORD) AS __Usual
	/// THROW NotImplementedException{}
RETURN __Usual._NIL   

/// <summary>
/// </summary>
/// <param name="dwSize"></param>
/// <returns>
/// </returns>
FUNCTION ItemArrayNew(dwSize AS DWORD) AS __Usual
	/// THROW NotImplementedException{}
RETURN __Usual._NIL   

/// <summary>
/// </summary>
/// <param name="px"></param>
/// <param name="dw"></param>
/// <param name="pa"></param>
/// <returns>
/// </returns>
unsafe FUNCTION ItemArrayPut(px AS PTR,dw AS DWORD,pa AS PTR) AS PTR
	/// THROW NotImplementedException{}
RETURN IntPtr.Zero


/// <summary>
/// </summary>
/// <param name="pszVal"></param>
/// <param name="ptrAny"></param>
/// <param name="dwLen"></param>
/// <returns>
/// </returns>
FUNCTION ItemCopyC(pszVal AS Psz,ptrAny AS IntPtr,dwLen AS DWORD) AS DWORD
	/// THROW NotImplementedException{}
RETURN 0   

/// <summary>
/// </summary>
/// <param name="pszVal"></param>
/// <returns>
/// </returns>
FUNCTION ItemFreeC(pszVal AS Psz) AS LOGIC
	/// THROW NotImplementedException{}
RETURN FALSE   

/// <summary>
/// </summary>
/// <param name="ptrAny"></param>
/// <returns>
/// </returns>
FUNCTION ItemGetC(ptrAny AS IntPtr) AS Psz
	/// THROW NotImplementedException{}
RETURN NULL_PSZ

/// <summary>
/// </summary>
/// <param name="ptrAny"></param>
/// <param name="pszDate"></param>
/// <returns>
/// </returns>
FUNCTION ItemGetDS(ptrAny AS IntPtr,pszDate AS Psz) AS Psz
	/// THROW NotImplementedException{}
RETURN NULL_PSZ

	/// <summary>
/// </summary>
/// <param name="ptrAny"></param>
/// <returns>
/// </returns>
unsafe FUNCTION ItemGetDL(ptrAny AS PTR) AS LONG
	/// THROW NotImplementedException{}
RETURN 0   

/// <summary>
/// </summary>
/// <param name="ptrAny"></param>
/// <returns>
/// </returns>
unsafe FUNCTION ItemGetL(ptrAny AS PTR) AS LOGIC
	/// THROW NotImplementedException{}
RETURN FALSE   

/// <summary>
/// </summary>
/// <param name="ptrAny"></param>
/// <returns>
/// </returns>
unsafe FUNCTION ItemGetND(ptrAny AS PTR) AS REAL8
	/// THROW NotImplementedException{}
RETURN 0   

/// <summary>
/// </summary>
/// <param name="ptrAny"></param>
/// <returns>
/// </returns>
unsafe FUNCTION ItemGetNL(ptrAny AS PTR) AS LONG
	/// THROW NotImplementedException{}
RETURN 0   

/// <summary>
/// </summary>
/// <param name="ptrAny"></param>
/// <param name="dwLen"></param>
/// <param name="dwDec"></param>
/// <returns>
/// </returns>
FUNCTION ItemGetNS(ptrAny AS IntPtr,dwLen AS DWORD,dwDec AS DWORD) AS Psz
	/// THROW NotImplementedException{}
RETURN NULL_PSZ

/// <summary>
/// </summary>
/// <param name="ptrAny"></param>
/// <param name="pszVal"></param>
/// <returns>
/// </returns>
FUNCTION ItemPutC(ptrAny AS IntPtr,pszVal AS Psz) AS IntPtr
	/// THROW NotImplementedException{}
RETURN IntPtr.Zero

/// <summary>
/// </summary>
/// <param name="ptrAny"></param>
/// <param name="pszVal"></param>
/// <param name="uilen"></param>
/// <returns>
/// </returns>
FUNCTION ItemPutCL(ptrAny AS IntPtr,pszVal AS Psz,uilen AS DWORD) AS IntPtr
	/// THROW NotImplementedException{}
RETURN IntPtr.Zero

/// <summary>
/// </summary>
/// <param name="ptrAny"></param>
/// <param name="pszDate"></param>
/// <returns>
/// </returns>
FUNCTION ItemPutDS(ptrAny AS IntPtr,pszDate AS Psz) AS IntPtr
	/// THROW NotImplementedException{}
RETURN IntPtr.Zero

/// <summary>
/// </summary>
/// <param name="ptrAny"></param>
/// <param name="pszVal"></param>
/// <param name="dwLen"></param>
/// <param name="dwDec"></param>
/// <returns>
/// </returns>
FUNCTION ItemPutNS(ptrAny AS IntPtr,pszVal AS Psz,dwLen AS DWORD,dwDec AS DWORD) AS IntPtr
	/// THROW NotImplementedException{}
RETURN IntPtr.Zero


/// <summary>
/// </summary>
/// <param name="ptrAny"></param>
/// <param name="lDate"></param>
/// <returns>
/// </returns>
unsafe FUNCTION ItemPutDL(ptrAny AS PTR,lDate AS LONG) AS PTR
	/// THROW NotImplementedException{}
RETURN IntPtr.Zero




/// <summary>
/// </summary>
/// <param name="ptrAny"></param>
/// <param name="fVal"></param>
/// <returns>
/// </returns>
unsafe FUNCTION ItemPutL(ptrAny AS PTR,fVal AS LOGIC) AS PTR
	/// THROW NotImplementedException{}
RETURN IntPtr.Zero

/// <summary>
/// </summary>
/// <param name="ptrAny"></param>
/// <param name="r8"></param>
/// <returns>
/// </returns>
unsafe FUNCTION ItemPutND(ptrAny AS PTR,r8 AS REAL8) AS PTR
	/// THROW NotImplementedException{}
RETURN IntPtr.Zero

/// <summary>
/// </summary>
/// <param name="ptrAny"></param>
/// <param name="lNum"></param>
/// <returns>
/// </returns>
unsafe FUNCTION ItemPutNL(ptrAny AS PTR,lNum AS LONG) AS PTR
	/// THROW NotImplementedException{}
RETURN IntPtr.Zero

/// <summary>
/// </summary>
/// <param name="ptrAny"></param>
/// <returns>
/// </returns>
unsafe FUNCTION ItemRegister(ptrAny AS PTR) AS LOGIC
	/// THROW NotImplementedException{}
RETURN FALSE   

/// <summary>
/// </summary>
/// <param name="ptrAny"></param>
/// <returns>
/// </returns>
unsafe FUNCTION ItemRelease(ptrAny AS PTR) AS LOGIC
	/// THROW NotImplementedException{}
RETURN FALSE   


/// <summary>
/// </summary>
/// <param name="ptrAny"></param>
/// <returns>
/// </returns>
unsafe FUNCTION ItemReturn(ptrAny AS PTR) AS __Usual
	/// THROW NotImplementedException{}
RETURN __Usual._NIL   

/// <summary>
/// </summary>
/// <param name="ptrAny"></param>
/// <returns>
/// </returns>
unsafe FUNCTION ItemSize(ptrAny AS PTR) AS DWORD
	/// THROW NotImplementedException{}
RETURN 0   

/// <summary>
/// </summary>
/// <param name="ptrAny"></param>
/// <returns>
/// </returns>
unsafe FUNCTION ItemType(ptrAny AS PTR) AS DWORD
	/// THROW NotImplementedException{}
RETURN 0   


