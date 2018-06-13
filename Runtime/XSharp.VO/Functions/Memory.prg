//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System.Runtime.CompilerServices
USING System.Runtime.InteropServices
USING System.Reflection
USING System.Reflection.Emit
USING System.Collections.Generic
USING System.Diagnostics

#region Basic Memory Allocation


FUNCTION MemTrace(lSet AS LOGIC) AS LOGIC
	LOCAL lOld AS LOGIC
	lOld := FixedMemory.MemTrace
	FixedMemory.MemTrace := lSet
	RETURN lOld

FUNCTION MemTrace() AS LOGIC
	LOCAL lOld AS LOGIC
	lOld := FixedMemory.MemTrace
	RETURN lOld


/// <summary>
/// Allocate a static memory buffer of a specified size.
/// </summary>
/// <param name="cb"></param>
/// <returns>
/// </returns>
FUNCTION MemAlloc(cb AS DWORD) AS IntPtr
	RETURN FixedMemory.Alloc(1, cb)


/// <summary>
/// Deallocate a specified memory buffer.
/// </summary>
/// <param name="cb">A pointer to a previously allocated memory buffer.</param>
/// <returns>0 if successful; otherwise, 65,535.</returns>
FUNCTION MemFree(pMem as IntPtr) AS WORD
	RETURN FixedMemory.Free(pMem)


/// <summary>Allocate static memory buffers of a specified size.</summary>
/// <param name="ui">The number of items to allocate memory for.</param>
/// <param name="cbCell">The number of bytes to allocate for each item.</param>
/// <returns>
/// A pointer to the allocated space if there is sufficient memory available; otherwise, a NULL_PTR.  You should always check the return value for a successful allocation.
/// </returns>
FUNCTION MemCAlloc(ui AS DWORD,cbCell AS DWORD) AS IntPtr
	RETURN FixedMemory.Alloc(1, ui * cbCell)



/// <summary>
/// ReAllocate a static memory buffer of a specified size.
/// </summary>
/// <param name="pBuffer"></param>
/// <param name="nSize"></param>
/// <returns>Returns the original pointer when the nSize parameter is smaller or equal to the current size.<br/>
//  Extraneous bytes are zeroed out. Returns a new buffer when the requesed size is bigger than the original size.
/// </returns>
FUNCTION MemRealloc( pBuffer AS IntPtr, nSize AS DWORD ) AS IntPtr
	RETURN FixedMemory.Realloc(pBuffer, nSize)


/// <summary>
/// Report the total number of bytes used by other memory manager functions.
/// </summary>
/// <returns>The total memory consumed by memory manager functions.  This value does not include the overhead used buy the memory manager
/// </returns>
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION MemTotal() AS DWORD
	RETURN FixedMemory.Total

#endregion

#region Memory Groups

[DebuggerDisplay("Group {ID}")];
INTERNAL CLASS XSharp.MemGroup
	EXPORT ID			as DWORD
	EXPORT Allocated	AS DWORD	

	CONSTRUCTOR(nID AS DWORD)
		SELF:Id			:= nID
		SELF:Allocated  := 0

	METHOD Free() AS VOID
		// Does nothing for now
		// Could free the list of blocks allocated for this group
		RETURN 

	METHOD Compact() AS VOID
		// Does nothing for now
		RETURN 

	METHOD Enum() AS VOID
		// Does nothing for now
		RETURN 

END CLASS


/// <summary>
/// Allocate a new memory buffer in a group.
/// </summary>
/// <param name="dwGroup"></param>
/// <param name="cb"></param>
/// <returns>
/// </returns>
FUNCTION MemGrpAlloc(dwGroup AS DWORD,cb AS DWORD) AS IntPtr
	RETURN FixedMemory.Alloc(dwGroup, cb)



/// <summary>
/// Open up a new memory group.
/// </summary>
/// <returns>
/// If successful, the handle of the new group; otherwise, 0.  You should always check for a valid group handle.
/// </returns>
FUNCTION MemGrpOpen() AS DWORD
	LOCAL oGroup AS MemGroup
	oGroup := FixedMemory.AddGroup()
	IF oGroup != NULL_OBJECT
		RETURN oGroup:ID
	ENDIF
	RETURN 0



/// <summary>
/// </summary>
/// <param name="dwGroup"></param>
/// <param name="cb"></param>
/// <param name="cbCell"></param>
/// <returns>
/// </returns>
FUNCTION MemGrpCAlloc(dwGroup AS DWORD,cb AS DWORD,cbCell AS DWORD) AS IntPtr
	RETURN FixedMemory.Alloc(dwGroup, cb * cbCell)

/// <summary>
/// Close a memory group.
/// </summary>
/// <param name="dwGroup"></param>
/// <returns>
/// </returns>
FUNCTION MemGrpClose(dwGroup AS DWORD) AS WORD
	LOCAL oGroup AS MemGroup
	LOCAL result as WORD
	oGroup := FixedMemory.FindGroup(dwGroup)
	IF oGroup != NULL_OBJECT
		FixedMemory.DeleteGroup(dwGroup)
		result := FixedMemory.SUCCESS
	ELSE
		result := FixedMemory.FAILURE
	ENDIF
RETURN result


/// <summary>
/// Enumerate all the pointers allocated in a memory group
/// </summary>
/// <param name="dwGroup">The group you want to compact</param>
/// <param name="pEnum">MemWalker Delegate</param>
/// <returns>TRUE when all delegate calls return TRUE</returns>

FUNCTION MemGrpEnum(dwGroup AS DWORD, pEnum AS MemWalker) AS LOGIC
	LOCAL lOk AS LOGIC
	lOk := TRUE
	FOREACH VAR element IN FixedMemory.AllocatedBlocks
		if FixedMemory:GetGroup(element:Key) == dwGroup
			IF ! pEnum(element:Key, element:Value)
				lOk := FALSE
				EXIT
			ENDIF
		ENDIF
	NEXT
RETURN lOk



#endregion

#region Obsolete Memory functions    



[ObsoleteAttribute( "'MemExit()' is not supported and and always returns 0" )] ;
FUNCTION MemExit() AS DWORD
	RETURN FixedMemory.SUCCESS


[ObsoleteAttribute( "'MemInit()' is not supported and and always returns 0" )] ;
FUNCTION MemInit() AS DWORD
	RETURN FixedMemory.SUCCESS



[ObsoleteAttribute( "'MemCompact()' is not supported and has no effect" )] ;
FUNCTION MemCompact() AS DWORD
	RETURN MemGrpCompact(0)



[ObsoleteAttribute( "'MemGrpCompact()' is not supported and has no effect" )] ;
FUNCTION MemGrpCompact(dwGroup AS DWORD) AS DWORD
	VAR result := FixedMemory.SUCCESS
RETURN result


#endregion


#region Memory Manipulation

/// <summary>Get the location of the first special console character in a buffer.</summary>
/// <param name="pMemory">A pointer to a buffer. </param>
/// <param name="dwCount">The number of bytes in the buffer to check. </param>
/// <returns>The location of the first special console character within the specified portion of pMemory.  
/// If a special console character does not exist, MemAtSpecial() returns 0.</returns>
FUNCTION MemAtSpecial( pMemory AS IntPtr, dwCount AS DWORD ) AS DWORD
	
	LOCAL ret := 0 AS DWORD
	IF pMemory == NULL_PTR
	   Throw Error.NullArgumentError( __ENTITY__, nameof(pMemory), 1 )
	ENDIF
	VAR pBytes := (BYTE PTR) pMemory
	LOCAL x as DWORD
	FOR X := 1 to dwCount
      IF pBytes[x] <= 13  // Note: indexer on PSZ class is 0-based
         ret := x       // Return value is 1-based
         EXIT
      ENDIF
   NEXT
   RETURN ret

/// <summary>Get a pointer to a byte in a memory buffer.</summary>
/// <param name="pMemory">A pointer to a buffer. </param>
/// <param name="bChar">The byte value to match. </param>
/// <param name="dwCount">The number of bytes in the buffer to check. </param>
/// <returns>A pointer to the first occurrence of bChar within the first dwCount bytes of pMemory.  
/// If bChar is not matched, MemChr() returns a NULL_PTR.</returns>
FUNCTION MemByte( pMemory AS IntPtr, bChar AS BYTE, dwCount AS DWORD ) AS IntPtr
	IF pMemory == IntPtr.Zero
		THROW Error.NullArgumentError(__ENTITY__,nameof(pMemory), 1)
	ENDIF
   RETURN MemChr( pMemory, bChar, dwCount )

/// <summary>Get a pointer to a byte in a memory buffer.</summary>
/// <param name="pMemory">A pointer to a buffer. </param>
/// <param name="bChar">The byte value to match. </param>
/// <param name="dwCount">The number of bytes in the buffer to check. </param>
/// <returns>A pointer to the first occurrence of bChar within the first dwCount bytes of pMemory.  
/// If bChar is not matched, MemChr() returns a NULL_PTR.</returns>
FUNCTION MemChr( pMemory AS IntPtr, bChar AS BYTE, dwCount AS DWORD ) AS IntPtr
	LOCAL pChr   AS BYTE PTR
	LOCAL pRet   as BYTE PTR
	IF pMemory == IntPtr.Zero
		THROW Error.NullArgumentError(__ENTITY__,nameof(pMemory), 1)
	ENDIF

	pRet	:= NULL_PTR
	pChr	:= (BYTE PTR) pMemory
	FOR VAR x := 1 TO dwCount
		IF pChr[x] == bChar
			pRet := @pChr[x]
			EXIT
		ENDIF
	NEXT
	RETURN pRet

/// <summary>Fill a memory buffer with null characters.</summary>
/// <param name="pMemory">A pointer to the memory buffer to fill.</param>
/// <param name="dwCount">The number of bytes to fill.</param>
/// <returns>A pointer to the filled memory buffer.</returns>
FUNCTION MemClear( pMemory AS IntPtr, dwCount AS DWORD ) AS IntPtr
	IF pMemory == IntPtr.Zero
		THROW Error.NullArgumentError(__ENTITY__,nameof(pMemory), 1)
	ENDIF
	RETURN FixedMemory.Clear(pMemory, (INT) dwCount)

/// <summary>Compare bytes in two memory buffers.</summary>
/// <param name="pMem1">A pointer to the first memory buffer.</param>
/// <param name="pMem2">A pointer to the second memory buffer. </param>
/// <param name="dwCount">The number of bytes to compare.</param>
/// <returns>-1, 0, or 1 if the first dwCount bytes of pMem1 are less than, equal to, 
/// or greater than the first dwCount bytes of pMem2, respectively.</returns>
FUNCTION MemComp( pMem1 AS IntPtr, pMem2 AS IntPtr, dwCount AS DWORD ) AS INT
	LOCAL pByte1 AS BYTE PTR
	LOCAL pByte2 AS BYTE PTR
	local result as INT
	// Validate ptr1 and ptr2
	IF pMem1 == IntPtr.Zero
		THROW Error.NullArgumentError(__ENTITY__,nameof(pMem1), 1)
	ENDIF
	IF pMem2 == IntPtr.Zero
		THROW Error.NullArgumentError(__ENTITY__,nameof(pMem2), 2)
	ENDIF

	pByte1 := (BYTE PTR) pMem1
	pByte2 := (BYTE PTR) pMem2
	result := 0
	FOR VAR x  := 1 TO dwCount
		IF pByte1[x] < pByte2[x]
			result := -1
			EXIT
		ELSEIF pByte1[x] > pByte2[x]
			result := 1
			EXIT
		ELSE
			// Equal, compare next
		ENDIF
	NEXT
	RETURN result

/// <summary>Copy one memory buffer to another.</summary>
/// <param name="pDestination"> A pointer to the destination memory buffer. </param>
/// <param name="pSource"> A pointer to the source to copy. </param>
/// <param name="dwCount">The number of bytes to copy.</param>
/// <returns>A pointer to the destination memory buffer.</returns>
/// <remarks>MemCopy() copies the specified number of bytes from the source memory buffer to the destination memory buffer.  
/// If portions of memory occupied by the source string overlap with portions in the destination, the overlapping region 
/// is overwritten.  Use MemMove() to copy overlapping regions before they are overwritten.</remarks>
FUNCTION MemCopy( pDestination AS IntPtr, pSource AS IntPtr, dwCount AS DWORD ) AS IntPtr
	IF pDestination == IntPtr.Zero
		THROW Error.NullArgumentError(__ENTITY__,nameof(pDestination), 1)
	ENDIF
	IF pSource == IntPtr.Zero
		THROW Error.NullArgumentError(__ENTITY__,nameof(pSource), 2)
	ENDIF

	RETURN FixedMemory.Copy(pDestination, pSource, (int) dwCount)

/// <summary>Copy one memory buffer to another.</summary>
/// <param name="pDestination"> A pointer to the destination memory buffer. </param>
/// <param name="pSource"> A pointer to the source string to copy. </param>
/// <param name="dwCount">The number of bytes to copy.</param>
/// <returns>NOTHING</returns>
/// <remarks>MemCopyString() copies the specified number of bytes from the source string to the destination memory buffer.  
/// If the number of bytes in the source memory buffer is less than dwCount, the rest of the destination memory buffer is filled with blanks (character 0).  
/// </remarks>
FUNCTION MemCopyString( pDestination AS IntPtr, cSource AS STRING, dwCount AS DWORD ) AS VOID
   // Convert the String to Ansi before copying
   IF pDestination == NULL_PTR
      THROW Error.NullArgumentError(__ENTITY__,nameof(pDestination), 1)
   ENDIF
   IF cSource == NULL
      THROW Error.NullArgumentError( __ENTITY__, nameof(cSource), 2 )
   ENDIF
   VAR pszList := List<IntPtr>{}
   TRY
	   Var pszSrc := XSharp.Internal.CompilerServices.String2Psz(cSource,pszList)
	   VAR srcLen := (DWORD) cSource:Length 
   
	   IF srcLen < dwCount
		  FixedMemory.Set( pDestination, 0, (int) dwCount )
	   ENDIF
   
		dwCount := Math.Min( srcLen, dwCount )
		FixedMemory.Copy(pDestination, pszSrc, (int)dwCount)
   FINALLY
		XSharp.Internal.CompilerServices.String2PszRelease(pszList)
   END TRY
   RETURN

/// <summary>Get a pointer to a dword in a memory buffer.</summary>
/// <param name="pMemory">A pointer to a buffer. </param>
/// <param name="dwValue">The dword value to match. </param>
/// <param name="dwCount">The number of bytes in the buffer to check. </param>
/// <returns>A pointer to the first occurrence of dwValue within the first dwCount bytes of pMemory.  
/// If dwValue is not matched, MemDWord() returns a NULL_PTR.</returns>
FUNCTION MemDWord( pMemory AS IntPtr, dwValue AS DWORD, dwCount AS DWORD ) AS IntPtr
	LOCAL pDword AS DWORD PTR
	LOCAL pRet   as DWORD PTR
	IF pMemory == IntPtr.Zero
		THROW Error.NullArgumentError(__ENTITY__,nameof(pMemory), 1)
	ENDIF
	pRet   := NULL_PTR
	pDword := (DWORD PTR) pMemory
	FOR VAR x := 1 TO dwCount
		IF pDword[x] == dwValue
			pRet := @pDword[x]
			EXIT
		ENDIF
	NEXT
	RETURN pRet

/// <summary>Get a pointer to a int in a memory buffer.</summary>
/// <param name="pMemory">A pointer to a buffer. </param>
/// <param name="iValue">The int value to match. </param>
/// <param name="dwCount">The number of bytes in the buffer to check. </param>
/// <returns>A pointer to the first occurrence of iValue within the first dwCount bytes of pMemory.  
/// If iValue is not matched, MemInt() returns a NULL_PTR.</returns>
FUNCTION MemInt( pMemory AS IntPtr, iValue AS INT, dwCount AS DWORD ) AS IntPtr
	LOCAL pInt   AS INT PTR
	LOCAL pRet   as INT PTR
	IF pMemory == IntPtr.Zero
		THROW Error.NullArgumentError(__ENTITY__,nameof(pMemory), 1)
	ENDIF
	pRet	:= NULL_PTR
	pInt	:= (INT PTR) pMemory
	FOR VAR x := 1 TO dwCount
		IF pInt[x] == iValue
			pRet := @pInt[x]
			EXIT
		ENDIF
	NEXT
	RETURN pRet

/// <summary>
/// </summary>
/// <param name="dwGroup"></param>
/// <returns>
/// </returns>
FUNCTION MemLen( pMemory AS IntPtr ) AS DWORD
	IF pMemory == IntPtr.Zero
		THROW Error.NullArgumentError(__ENTITY__,nameof(pMemory), 1)
	ENDIF
	RETURN FixedMemory.BlockSize(pMemory)

/// <summary>Get a pointer to a long in a memory buffer.</summary>
/// <param name="pMemory">A pointer to a buffer. </param>
/// <param name="liValue">The long value to match. </param>
/// <param name="dwCount">The number of bytes in the buffer to check. </param>
/// <returns>A pointer to the first occurrence of liValue within the first dwCount bytes of pMemory.  
/// If liValue is not matched, MemLong() returns a NULL_PTR.</returns>
FUNCTION MemLong( pMemory AS IntPtr, liValue AS INT, dwCount AS DWORD ) AS IntPtr
	IF pMemory == IntPtr.Zero
		THROW Error.NullArgumentError(__ENTITY__,nameof(pMemory), 1)
	ENDIF
   RETURN MemInt( pMemory, liValue, dwCount )



/// <summary>
/// </summary>
/// <param name="dwGroup"></param>
/// <returns>
/// </returns>
FUNCTION MemLower( pMemory AS IntPtr, dwCount AS DWORD ) AS IntPtr
	// Ansi based lower casing
	LOCAL pChr   AS BYTE PTR
	IF pMemory == IntPtr.Zero
		THROW Error.NullArgumentError(__ENTITY__,nameof(pMemory), 1)
	ENDIF
	pChr	:= (BYTE PTR) pMemory
	FOR VAR x := 1 TO dwCount
		IF pChr[x] >= 'A' && pChr[x] <= 'Z'
			pChr[x] += c'a' - c'A' 
		ENDIF
	NEXT
	RETURN pMemory

/// <summary>Move one memory buffer to another. </summary>
/// <param name="pDestination">A pointer to the destination memory buffer. </param>
/// <param name="pSource"> A pointer to the source memory buffer. </param>
/// <param name="nSize">The number of bytes to copy.</param>
/// <returns>A pointer to the destination memory buffer.</returns>
/// <remarks>MemMove() copies the specified number of bytes from the source memory buffer 
/// to the destination memory buffer.  If portions of the source buffer overlap with portions 
/// of the destination buffer, the overlapping region is copied and kept for the duration of 
/// the operation before it is overwritten.</remarks>
FUNCTION MemMove( pDestination AS IntPtr, pSource AS IntPtr, nSize AS DWORD ) AS IntPtr
   LOCAL dst AS BYTE PTR
   LOCAL src AS BYTE PTR

	IF pDestination == IntPtr.Zero
		THROW Error.NullArgumentError(__ENTITY__,nameof(pDestination), 1)
	ENDIF
	IF pSource == IntPtr.Zero
		THROW Error.NullArgumentError(__ENTITY__,nameof(pSource), 2)
	ENDIF
	dst := (BYTE PTR) pDestination 
	src := (BYTE PTR) pSource      
	
	IF (dst <= src) || dst >= (BYTE PTR) ( (DWORD) src + (DWORD) nSize)
		// copy from source to dest from lower to higher bound
		FixedMemory.Copy(pDestination, pSource, (int) nSize)
	ELSE
		// overlapping
		// copy from higher address to lower address
		FOR VAR x := nSize downto 1 
			dst[x] := src[x]
		NEXT
	ENDIF
	RETURN pDestination

/// <summary>Fill a memory buffer with a specified character.</summary>
/// <param name="pMemory">A pointer to the memory buffer to fill. </param>
/// <param name="bValue">The code for the character, as a number from 0 to 255.</param>
/// <param name="dwCount">The number of bytes to fill.</param>
/// <returns>A pointer to the filled memory buffer.</returns>
FUNCTION MemSet( pMemory AS IntPtr, bValue AS BYTE, dwCount AS DWORD ) AS IntPtr
	IF pMemory == IntPtr.Zero
		THROW Error.NullArgumentError(__ENTITY__,nameof(pMemory), 1)
	ENDIF
	RETURN FixedMemory.Set(pMemory, bValue, (int) dwCount)

/// <summary>Get a pointer to a short in a memory buffer.</summary>
/// <param name="pMemory">A pointer to a buffer. </param>
/// <param name="siValue">The short value to match. </param>
/// <param name="dwCount">The number of bytes in the buffer to check. </param>
/// <returns>A pointer to the first occurrence of siValue within the first dwCount bytes of pMemory.  
/// If siValue is not matched, MemShort() returns a NULL_PTR.</returns>
FUNCTION MemShort( pMemory AS IntPtr, siValue AS SHORT, dwCount AS DWORD ) AS IntPtr
	LOCAL pShort  AS SHORT PTR
	LOCAL pRet   as SHORT PTR
	IF pMemory == IntPtr.Zero
		THROW Error.NullArgumentError(__ENTITY__,nameof(pMemory), 1)
	ENDIF
	pRet	:= NULL_PTR
	pShort	:= (SHORT PTR) pMemory
	FOR VAR x := 1 TO dwCount
		IF pShort[x] == siValue
			pRet := @pShort[x]
			EXIT
		ENDIF
	NEXT
	RETURN pRet

/// <summary>
/// </summary>
/// <param name="dwGroup"></param>
/// <returns>
/// </returns>
FUNCTION MemUpper( pMemory AS IntPtr, dwCount AS DWORD ) AS IntPtr
	// Ansi based upper casing
	LOCAL pChr   AS BYTE PTR
	IF pMemory == IntPtr.Zero
		THROW Error.NullArgumentError(__ENTITY__,nameof(pMemory), 1)
	ENDIF

	pChr	:= (BYTE PTR) pMemory
	FOR VAR x := 1 TO dwCount
		IF pChr[x] >= c'a' && pChr[x] <= 'z'
			pChr[x] -= c'a' - c'A' 
		ENDIF
	NEXT
	RETURN pMemory

/// <summary>Get a pointer to a word in a memory buffer.</summary>
/// <param name="pMemory">A pointer to a buffer. </param>
/// <param name="wValue">The word value to match. </param>
/// <param name="dwCount">The number of bytes in the buffer to check. </param>
/// <returns>A pointer to the first occurrence of wValue within the first dwCount bytes of pMemory.  
/// If wValue is not matched, MemWord() returns a NULL_PTR.</returns>
FUNCTION MemWord( pMemory AS IntPtr, wValue AS WORD, dwCount AS DWORD ) AS IntPtr
	LOCAL pWord  AS WORD PTR
	LOCAL pRet   as WORD PTR
	IF pMemory == IntPtr.Zero
		THROW Error.NullArgumentError(__ENTITY__,nameof(pMemory), 1)
	ENDIF
	pRet	:= NULL_PTR
	pWord	:= (WORD PTR) pMemory
	FOR VAR x := 1 TO dwCount
		IF pWord[x] == wValue
			pRet := @pWord[x]
			EXIT
		ENDIF
	NEXT
	RETURN pRet

FUNCTION MemWalk(pEnum AS MemWalker) AS LOGIC
	LOCAL lOk AS LOGIC
	lOk := TRUE
	FOREACH VAR element IN FixedMemory.AllocatedBlocks
		IF ! pEnum(element:Key, element:Value)
			lOk := FALSE
			EXIT
		ENDIF
	NEXT
	RETURN lOk   


#endregion




