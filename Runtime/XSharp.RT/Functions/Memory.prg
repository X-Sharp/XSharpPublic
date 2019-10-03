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

/// <summary>Enable / disable memory tracing</summary>
FUNCTION MemTrace(lSet AS LOGIC) AS LOGIC
	LOCAL lOld AS LOGIC
	lOld := FixedMemory.MemTrace
	FixedMemory.MemTrace := lSet
	RETURN lOld
/// <summary>Retrieve memory tracing state.</summary>
FUNCTION MemTrace() AS LOGIC
	LOCAL lOld AS LOGIC
	lOld := FixedMemory.MemTrace
	RETURN lOld


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/memalloc/*" />
/// <include file="RTComments.xml" path="Comments/StaticMemory/*" />
FUNCTION MemAlloc(wBytes AS DWORD) AS IntPtr
	RETURN FixedMemory.Alloc(1, wBytes)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/memfree/*" />
/// <include file="RTComments.xml" path="Comments/StaticMemory/*" />
FUNCTION MemFree(ptrBuffer AS IntPtr) AS WORD
	RETURN FixedMemory.Free(ptrBuffer)



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/memcalloc/*" />
/// <include file="RTComments.xml" path="Comments/StaticMemory/*" />
FUNCTION MemCAlloc(wItems AS DWORD,wBytes AS DWORD) AS IntPtr
	RETURN FixedMemory.Alloc(1, wItems * wBytes)



/// <summary>
/// ReAllocate a static memory buffer of a specified size.
/// </summary>
/// <param name="pBuffer"></param>
/// <param name="nSize"></param>
/// <returns>Returns the original pointer when the nSize parameter is smaller or equal to the current size.<br/>
//  Extraneous bytes are zeroed out. Returns a new buffer when the requesed size is bigger than the original size.
/// </returns>
/// <include file="RTComments.xml" path="Comments/StaticMemory/*" />
FUNCTION MemRealloc( pBuffer AS IntPtr, nSize AS DWORD ) AS IntPtr
	RETURN FixedMemory.Realloc(pBuffer, nSize)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/memtotal/*" />
[MethodImpl(MethodImplOptions.AggressiveInlining)];
FUNCTION MemTotal() AS DWORD
	RETURN FixedMemory.Total

#endregion

#region Memory Groups

[DebuggerDisplay("Group {ID}")];
INTERNAL CLASS XSharp.MemGroup
	EXPORT ID			AS DWORD
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

	METHOD ENUM() AS VOID
		// Does nothing for now
		RETURN 

END CLASS


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/memgrpalloc/*" />
/// <include file="RTComments.xml" path="Comments/StaticMemory/*" />
FUNCTION MemGrpAlloc(wGroup AS DWORD,wBytes AS DWORD) AS IntPtr
	RETURN FixedMemory.Alloc(wGroup, wBytes)



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/memgrpopen/*" />
/// <include file="RTComments.xml" path="Comments/StaticMemory/*" />
FUNCTION MemGrpOpen() AS DWORD
	LOCAL oGroup AS MemGroup
	oGroup := FixedMemory.AddGroup()
	IF oGroup != NULL_OBJECT
		RETURN oGroup:ID
	ENDIF
	RETURN 0


/// <inheritdoc cref="M:XSharp.RT.Functions.MemCAlloc(System.UInt32,System.UInt32)" />
/// <param name="wGroup">The group to which the newly allocated memory buffer will belong.  This group should have already been opened by MemGrpOpen().</param>
/// <include file="RTComments.xml" path="Comments/StaticMemory/*" />
FUNCTION MemGrpCAlloc(wGroup AS DWORD,wItems AS DWORD,wBytes AS DWORD) AS IntPtr
	RETURN FixedMemory.Alloc(wGroup, wItems * wBytes)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/memgrpclose/*" />
FUNCTION MemGrpClose(wGroup AS DWORD) AS WORD
	LOCAL oGroup AS MemGroup
	LOCAL result AS WORD
	oGroup := FixedMemory.FindGroup(wGroup)
	IF oGroup != NULL_OBJECT
		FixedMemory.DeleteGroup(wGroup)
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
		IF FixedMemory:GetGroup(element:Key) == dwGroup
			IF ! pEnum(element:Key, element:Value)
				lOk := FALSE
				EXIT
			ENDIF
		ENDIF
	NEXT
RETURN lOk



#endregion

#region Obsolete Memory functions    


/// <exclude/>
[ObsoleteAttribute( "'MemExit()' is not supported and and always returns 0" )] ;
FUNCTION MemExit() AS DWORD
	RETURN FixedMemory.SUCCESS

/// <exclude/>
[ObsoleteAttribute( "'MemInit()' is not supported and and always returns 0" )] ;
FUNCTION MemInit() AS DWORD
	RETURN FixedMemory.SUCCESS


/// <exclude/>
[ObsoleteAttribute( "'MemCompact()' is not supported and has no effect" )] ;
FUNCTION MemCompact() AS DWORD
	RETURN MemGrpCompact(0)


/// <exclude/>
[ObsoleteAttribute( "'MemGrpCompact()' is not supported and has no effect" )] ;
FUNCTION MemGrpCompact(dwGroup AS DWORD) AS DWORD
	VAR result := FixedMemory.SUCCESS
RETURN result


#endregion


#region Memory Manipulation

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/mematspecial/*" />
FUNCTION MemAtSpecial( ptrBuffer AS IntPtr, dwCount AS DWORD ) AS DWORD
	
	LOCAL ret := 0 AS DWORD
	IF ptrBuffer == IntPtr.Zero
	   THROW Error.NullArgumentError( __FUNCTION__, NAMEOF(ptrBuffer), 1 )
	ENDIF
	VAR pBytes := (BYTE PTR) ptrBuffer:ToPointer()
	LOCAL x AS DWORD
	FOR X := 1 TO dwCount
      IF pBytes[x] <= 13  // Note: indexer on PSZ class is 0-based
         ret := x       // Return value is 1-based
         EXIT
      ENDIF
   NEXT
   RETURN ret

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/membyte/*" />
FUNCTION MemByte( ptrBuffer AS IntPtr, bChar AS BYTE, dwCount AS DWORD ) AS IntPtr
	IF ptrBuffer == IntPtr.Zero
		THROW Error.NullArgumentError(__FUNCTION__,NAMEOF(ptrBuffer), 1)
	ENDIF
   RETURN MemChr( ptrBuffer, bChar, dwCount )

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/memchr/*" />
FUNCTION MemChr( ptrBuffer AS IntPtr, dwChar AS BYTE, dwCount AS DWORD ) AS IntPtr
	LOCAL pChr   AS BYTE PTR
	LOCAL pRet   AS IntPtr
	IF ptrBuffer == IntPtr.Zero
		THROW Error.NullArgumentError(__FUNCTION__,NAMEOF(ptrBuffer), 1)
	ENDIF

	pRet	:= IntPtr.Zero
	pChr	:= ptrBuffer:ToPointer()
	FOR VAR x := 1 TO dwCount
		IF pChr[x] == dwChar
			pRet := IntPtr{@pChr[x]}
			EXIT
		ENDIF
	NEXT
	RETURN pRet

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/memclear/*" />
FUNCTION MemClear( ptrDest AS IntPtr, dwCount AS DWORD ) AS IntPtr
	IF ptrDest == IntPtr.Zero
		THROW Error.NullArgumentError(__FUNCTION__,NAMEOF(ptrDest), 1)
	ENDIF
	RETURN FixedMemory.Clear(ptrDest, (INT) dwCount)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/memcomp/*" />
FUNCTION MemComp( ptrFirst AS IntPtr, ptrSecond AS IntPtr, dwCount AS DWORD ) AS INT
	LOCAL pByte1 AS BYTE PTR
	LOCAL pByte2 AS BYTE PTR
	LOCAL result AS INT
	// Validate ptr1 and ptr2
	IF ptrFirst == IntPtr.Zero
		THROW Error.NullArgumentError(__FUNCTION__,NAMEOF(ptrFirst), 1)
	ENDIF
	IF ptrSecond == IntPtr.Zero
		THROW Error.NullArgumentError(__FUNCTION__,NAMEOF(ptrSecond), 2)
	ENDIF

	pByte1 := ptrFirst:ToPointer()
	pByte2 := ptrSecond:ToPointer()
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

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/memcopy/*" />
FUNCTION MemCopy( ptrDest AS IntPtr, ptrSource AS IntPtr, dwCount AS DWORD ) AS IntPtr
	IF ptrDest == IntPtr.Zero
		THROW Error.NullArgumentError(__FUNCTION__,NAMEOF(ptrDest), 1)
	ENDIF
	IF ptrSource == IntPtr.Zero
		THROW Error.NullArgumentError(__FUNCTION__,NAMEOF(ptrSource), 2)
	ENDIF

	RETURN FixedMemory.Copy(ptrDest, ptrSource, (INT) dwCount)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/memcopystring/*" />
FUNCTION MemCopyString( ptrDest AS IntPtr, cSource AS STRING, dwCount AS DWORD ) AS VOID
   // Convert the String to Ansi before copying
   IF ptrDest == IntPtr.Zero
      THROW Error.NullArgumentError(__FUNCTION__,NAMEOF(ptrDest), 1)
   ENDIF
   IF cSource == NULL
      THROW Error.NullArgumentError( __FUNCTION__, NAMEOF(cSource), 2 )
   ENDIF
   VAR pszList := List<IntPtr>{}
   TRY
	   VAR pszSrc := XSharp.Internal.CompilerServices.String2Psz(cSource,pszList)
	   VAR srcLen := (DWORD) cSource:Length 
   
	   IF srcLen < dwCount
		  FixedMemory.Set( ptrDest, 0, (INT) dwCount )
	   ENDIF
   
		dwCount := Math.Min( srcLen, dwCount )
		FixedMemory.Copy(ptrDest, pszSrc, (INT)dwCount)
   FINALLY
		XSharp.Internal.CompilerServices.String2PszRelease(pszList)
   END TRY
   RETURN

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/memdword/*" />
FUNCTION MemDWord( ptrBuffer AS IntPtr, dwValue AS DWORD, dwCount AS DWORD ) AS IntPtr
	LOCAL pDword AS DWORD PTR
	LOCAL pRet   AS IntPtr 
	IF ptrBuffer == IntPtr.Zero
		THROW Error.NullArgumentError(__FUNCTION__,NAMEOF(ptrBuffer), 1)
	ENDIF
	pRet   := IntPtr.Zero
	pDword := ptrBuffer:ToPointer()
	FOR VAR x := 1 TO dwCount
		IF pDword[x] == dwValue
			pRet := IntPtr{@pDword[x]}
			EXIT
		ENDIF
	NEXT
	RETURN pRet

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/memint/*" />
FUNCTION MemInt( ptrBuffer AS IntPtr, iValue AS INT, dwCount AS DWORD ) AS IntPtr
	LOCAL pInt   AS INT PTR
	LOCAL pRet   AS IntPtr
	IF ptrBuffer == IntPtr.Zero
		THROW Error.NullArgumentError(__FUNCTION__,NAMEOF(ptrBuffer), 1)
	ENDIF
	pRet	:= IntPtr.Zero
	pInt	:= (INT PTR) ptrBuffer:ToPointer()
	FOR VAR x := 1 TO dwCount
		IF pInt[x] == iValue
			pRet := IntPtr{@pInt[x]}
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
		THROW Error.NullArgumentError(__FUNCTION__,NAMEOF(pMemory), 1)
	ENDIF
	RETURN FixedMemory.BlockSize(pMemory)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/memlong/*" />
FUNCTION MemLong( ptrBuffer AS IntPtr, liValue AS INT, dwCount AS DWORD ) AS IntPtr
	IF ptrBuffer == IntPtr.Zero
		THROW Error.NullArgumentError(__FUNCTION__,NAMEOF(ptrBuffer), 1)
	ENDIF
   RETURN MemInt( ptrBuffer, liValue, dwCount )


/// <summary>
/// </summary>
/// <param name="pMemory"></param>
/// <param name="dwCount"></param>
/// <returns>
/// </returns>
FUNCTION MemLower( pMemory AS IntPtr, dwCount AS DWORD ) AS IntPtr
	// Ansi based lower casing
	LOCAL pChr   AS BYTE PTR
	IF pMemory == IntPtr.Zero
		THROW Error.NullArgumentError(__FUNCTION__,NAMEOF(pMemory), 1)
	ENDIF
	pChr := pMemory:ToPointer() 
	FOR VAR x := 1 TO dwCount
        IF pChr[x] >= c'A' .AND. pChr[x] <= c'Z'
			pChr[x] |= (BYTE) 32
		ENDIF
	NEXT
	RETURN pMemory

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/memmove/*" />
FUNCTION MemMove( ptrDest AS IntPtr, ptrSource AS IntPtr, dwCount AS DWORD ) AS IntPtr

	IF ptrDest == IntPtr.Zero
		THROW Error.NullArgumentError(__FUNCTION__,NAMEOF(ptrDest), 1)
	ENDIF
	IF ptrSource == IntPtr.Zero
		THROW Error.NullArgumentError(__FUNCTION__,NAMEOF(ptrSource), 2)
	ENDIF
    IF dwCount > 0
	    IF (ptrDest:ToInt32() <= ptrSource:ToInt32() || ptrDest:ToInt32() >= (ptrSource:ToInt32() + (INT) dwCount))
		    // copy from source to dest from lower to higher bound
		    FixedMemory.Copy(ptrDest, ptrSource, (INT) dwCount)
	    ELSE
		    // overlapping
		    // copy from higher address to lower address
            LOCAL dst AS BYTE PTR
            LOCAL src AS BYTE PTR
	        dst := ptrDest:ToPointer()
	        src := ptrSource:ToPointer()
		    FOR VAR x := dwCount DOWNTO 1 
			    dst[x] := src[x]
		    NEXT
	    ENDIF
    ENDIF
	RETURN ptrDest

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/memset/*" />
FUNCTION MemSet( ptrBuffer AS IntPtr, bValue AS BYTE, wCount AS DWORD ) AS IntPtr
	IF ptrBuffer == IntPtr.Zero
		THROW Error.NullArgumentError(__FUNCTION__,NAMEOF(ptrBuffer), 1)
	ENDIF
	RETURN FixedMemory.Set(ptrBuffer, bValue, (INT) wCount)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/memshort/*" />
FUNCTION MemShort( ptrBuffer AS IntPtr, siValue AS SHORT, dwCount AS DWORD ) AS IntPtr
	LOCAL pShort  AS SHORT PTR
	LOCAL pRet   AS IntPtr
	IF ptrBuffer == IntPtr.Zero
		THROW Error.NullArgumentError(__FUNCTION__,NAMEOF(ptrBuffer), 1)
	ENDIF
	pRet	:= IntPtr.Zero
	pShort	:= ptrBuffer:ToPointer()
	FOR VAR x := 1 TO dwCount
		IF pShort[x] == siValue
			pRet := IntPtr{@pShort[x]}
			EXIT
		ENDIF
	NEXT
	RETURN pRet

/// <summary>
/// </summary>
/// <param name="pMemory"></param>
/// <param name="dwCount"></param>
/// <returns>
/// </returns>
FUNCTION MemUpper( pMemory AS IntPtr, dwCount AS DWORD ) AS IntPtr
	// Ansi based upper casing
	LOCAL pChr   AS BYTE PTR
	IF pMemory == IntPtr.Zero
		THROW Error.NullArgumentError(__FUNCTION__,NAMEOF(pMemory), 1)
	ENDIF
    pChr := pMemory:ToPointer() 
	FOR VAR x := 1 TO dwCount
        IF pChr[x] >= c'a' .AND. pChr[x] <= c'z'
			pChr[x] -= (BYTE) 32
		ENDIF
	NEXT
	RETURN pMemory

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/memword/*" />
FUNCTION MemWord( ptrBuffer AS IntPtr, dwValue AS WORD, dwCount AS DWORD ) AS IntPtr
	LOCAL pWord  AS WORD PTR
	LOCAL pRet   AS IntPtr
	IF ptrBuffer == IntPtr.Zero
		THROW Error.NullArgumentError(__FUNCTION__,NAMEOF(ptrBuffer), 1)
	ENDIF
	pRet	:= IntPtr.Zero
	pWord	:= ptrBuffer:ToPointer()
	FOR VAR x := 1 TO dwCount
		IF pWord[x] == dwValue
			pRet := IntPtr{@pWord[x]}
			EXIT
		ENDIF
	NEXT
	RETURN pRet
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/memwalk/*" />
/// <remarks>Only memory blocks that were allocated while MemTrace was set to TRUE will be included.</remarks>
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




