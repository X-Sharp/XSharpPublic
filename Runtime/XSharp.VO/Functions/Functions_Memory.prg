//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using XSharp

/// <summary>
/// </summary>
/// <param name="cFile"></param>
/// <param name="nOptions"></param>
/// <returns>
/// </returns>
FUNCTION DynMemDump(cFile AS STRING,nOptions AS DWORD) AS LOGIC
	/// THROW NotImplementedException{}
RETURN FALSE   

/// <summary>
/// Copy a typed dynamic object to static allocated memory.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION DynToOldSpaceString(c AS STRING) AS STRING
	/// THROW NotImplementedException{}
RETURN String.Empty   



	/// <summary>
	/// Check to see if a value is in dynamic memory.
	/// </summary>
	/// <param name="ptrVar"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION IsDynPtr(ptrVar AS PTR) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   


/// <summary>
/// Check to see if a typed dynamic object is static.
/// </summary>
/// <param name="f"></param>
/// <returns>
/// </returns>
FUNCTION IsOldSpaceFloat(f AS __VOFloat) AS LOGIC
	/// THROW NotImplementedException{}
RETURN FALSE   



/// <summary>
/// Check to see if a typed dynamic object is static.
/// </summary>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION IsOldSpaceString(c AS STRING) AS LOGIC
	/// THROW NotImplementedException{}
RETURN FALSE 



/// <summary>
/// </summary>
/// <param name="f"></param>
/// <returns>
/// </returns>
FUNCTION OldSpaceFreeFloat(f AS __VOFloat) AS VOID
	/// THROW NotImplementedException{}
RETURN



/// <summary>
/// Allocate a static memory buffer of a specified size.
/// </summary>
/// <param name="cb"></param>
/// <returns>
/// </returns>
unsafe FUNCTION MemAlloc(cb AS DWORD) AS PTR
	/// THROW NotImplementedException{}
RETURN IntPtr.Zero

/// <summary>
/// </summary>
/// <param name="cb"></param>
/// <returns>
/// </returns>
unsafe FUNCTION MemAllocBlk(cb AS DWORD) AS PTR
	/// THROW NotImplementedException{}
RETURN IntPtr.Zero





/// <summary>
/// Allocate static memory buffers of a specified size.
/// </summary>
/// <param name="ui"></param>
/// <param name="cbCell"></param>
/// <returns>
/// </returns>
unsafe FUNCTION MemCAlloc(ui AS DWORD,cbCell AS DWORD) AS PTR
	/// THROW NotImplementedException{}
RETURN IntPtr.Zero

/// <summary>
/// Allocate a new memory buffer in a group.
/// </summary>
/// <param name="dwGroup"></param>
/// <param name="cb"></param>
/// <returns>
/// </returns>
unsafe FUNCTION MemGrpAlloc(dwGroup AS DWORD,cb AS DWORD) AS PTR
	/// THROW NotImplementedException{}
RETURN IntPtr.Zero

/// <summary>
/// </summary>
/// <param name="dwGroup"></param>
/// <param name="cb"></param>
/// <param name="cbCell"></param>
/// <returns>
/// </returns>
unsafe FUNCTION MemGrpCAlloc(dwGroup AS DWORD,cb AS DWORD,cbCell AS DWORD) AS PTR
	/// THROW NotImplementedException{}
RETURN IntPtr.Zero




/// <summary>
/// Close a memory group.
/// </summary>
/// <param name="dwGroup"></param>
/// <returns>
/// </returns>
FUNCTION MemGrpClose(dwGroup AS DWORD) AS DWORD
	/// THROW NotImplementedException{}
RETURN 0   

/// <summary>
/// </summary>
/// <param name="dwGroup"></param>
/// <returns>
/// </returns>
FUNCTION MemGrpCompact(dwGroup AS DWORD) AS DWORD
	/// THROW NotImplementedException{}
RETURN 0   

/// <summary>
/// </summary>
/// <param name="dwGroup"></param>
/// <param name="pFunction"></param>
/// <returns>
/// </returns>
unsafe FUNCTION MemGrpEnum(dwGroup AS DWORD,pFunction AS PTR) AS LONG
	/// THROW NotImplementedException{}
RETURN 0   
     



/// <summary>
/// Register a dynamic object for update after a garbage collection.
/// </summary>
/// <param name="ptrKid"></param>
/// <param name="dwCount"></param>
/// <param name="lItem"></param>
/// <returns>
/// </returns>
unsafe FUNCTION RegisterKid(ptrKid AS PTR,dwCount AS DWORD,lItem AS LOGIC) AS VOID
	/// THROW NotImplementedException{}
RETURN
