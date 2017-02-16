

using XSharp

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
/// <param name="ptrAny"></param>
/// <param name="dwElem"></param>
/// <returns>
/// </returns>
unsafe FUNCTION ItemArrayGet(ptrAny AS PTR,dwElem AS DWORD) AS __Usual
	/// THROW NotImplementedException{}
RETURN __Usual._NIL   



/// <summary>
/// </summary>
/// <param name="pszVal"></param>
/// <param name="ptrAny"></param>
/// <param name="dwLen"></param>
/// <returns>
/// </returns>
FUNCTION ItemCopyC(pszVal AS __Psz,ptrAny AS IntPtr,dwLen AS DWORD) AS DWORD
	/// THROW NotImplementedException{}
RETURN 0   

/// <summary>
/// </summary>
/// <param name="pszVal"></param>
/// <returns>
/// </returns>
FUNCTION ItemFreeC(pszVal AS __Psz) AS LOGIC
	/// THROW NotImplementedException{}
RETURN FALSE   

/// <summary>
/// </summary>
/// <param name="ptrAny"></param>
/// <returns>
/// </returns>
FUNCTION ItemGetC(ptrAny AS IntPtr) AS __Psz
	/// THROW NotImplementedException{}
RETURN __PSZ._NULL_PSZ 

/// <summary>
/// </summary>
/// <param name="ptrAny"></param>
/// <param name="pszDate"></param>
/// <returns>
/// </returns>
FUNCTION ItemGetDS(ptrAny AS IntPtr,pszDate AS __Psz) AS __Psz
	/// THROW NotImplementedException{}
RETURN __PSZ._NULL_PSZ 

/// <summary>
/// </summary>
/// <param name="ptrAny"></param>
/// <param name="dwLen"></param>
/// <param name="dwDec"></param>
/// <returns>
/// </returns>
FUNCTION ItemGetNS(ptrAny AS IntPtr,dwLen AS DWORD,dwDec AS DWORD) AS __Psz
	/// THROW NotImplementedException{}
RETURN __PSZ._NULL_PSZ 

/// <summary>
/// </summary>
/// <param name="ptrAny"></param>
/// <param name="pszVal"></param>
/// <returns>
/// </returns>
FUNCTION ItemPutC(ptrAny AS IntPtr,pszVal AS __Psz) AS IntPtr
	/// THROW NotImplementedException{}
RETURN IntPtr.Zero

/// <summary>
/// </summary>
/// <param name="ptrAny"></param>
/// <param name="pszVal"></param>
/// <param name="uilen"></param>
/// <returns>
/// </returns>
FUNCTION ItemPutCL(ptrAny AS IntPtr,pszVal AS __Psz,uilen AS DWORD) AS IntPtr
	/// THROW NotImplementedException{}
RETURN IntPtr.Zero

/// <summary>
/// </summary>
/// <param name="ptrAny"></param>
/// <param name="pszDate"></param>
/// <returns>
/// </returns>
FUNCTION ItemPutDS(ptrAny AS IntPtr,pszDate AS __Psz) AS IntPtr
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
FUNCTION ItemPutNS(ptrAny AS IntPtr,pszVal AS __Psz,dwLen AS DWORD,dwDec AS DWORD) AS IntPtr
	/// THROW NotImplementedException{}
RETURN IntPtr.Zero


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