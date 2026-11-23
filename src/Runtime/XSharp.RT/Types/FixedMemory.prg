//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.Runtime.InteropServices
USING System.Collections.Generic
USING System.Collections.Concurrent
USING System.Runtime.CompilerServices

/// <include file="XSharp.RT.Docs.xml" path="doc/MemWalker/*" />
DELEGATE MemWalker(pMem AS IntPtr, nSize AS DWORD) AS LOGIC

INTERNAL STATIC CLASS XSharp.FixedMemory
    PUBLIC CONST FAILURE := 65535 AS WORD
    PUBLIC CONST SUCCESS := 0 AS WORD
    INTERNAL INITONLY STATIC Is32Bits AS LOGIC
    INTERNAL STATIC Groups		AS ConcurrentDictionary<DWORD, MemGroup>
    INTERNAL STATIC LastGroup	AS DWORD
    INTERNAL STATIC Total		AS DWORD
    INTERNAL STATIC MemTrace	AS LOGIC
    INTERNAL STATIC AllocatedBlocks AS ConcurrentDictionary<IntPtr, DWORD>
    STATIC CONSTRUCTOR()
        Groups			:= ConcurrentDictionary<DWORD, MemGroup>{}
        AllocatedBlocks := ConcurrentDictionary<IntPtr, DWORD>{}
        AddGroup(1)
        LastGroup := 1
        Total	  := 0
        MemTrace  := FALSE
        Is32Bits  := IntPtr.Size == 4

    INTERNAL STATIC METHOD AddGroup(nGroup AS DWORD) AS MemGroup
        LOCAL oGroup AS MemGroup
        oGroup := MemGroup{nGroup}
        Groups:TryAdd(nGroup, oGroup)
        RETURN oGroup

    INTERNAL STATIC METHOD AddGroup() AS MemGroup
        LastGroup += 1
        RETURN FixedMemory.AddGroup(LastGroup)


    INTERNAL STATIC METHOD FindGroup(nGroup AS DWORD) AS MemGroup
        IF Groups:TryGetValue(nGroup, OUT VAR ret)
            RETURN ret
        ENDIF
        RETURN NULL_OBJECT

    INTERNAL STATIC METHOD DeleteGroup(nGroup AS DWORD) AS LOGIC
        LOCAL oGroup AS MemGroup
        LOCAL lOk	:= FALSE AS LOGIC
        oGroup := FindGroup(nGroup)
        IF oGroup != NULL_OBJECT
            oGroup:Free()
            Groups:TryRemove(nGroup, out var _)
            lOk := TRUE
        ENDIF
        RETURN lOk

    INTERNAL STATIC METHOD GetGroup(pMemory AS IntPtr) AS DWORD
        VAR pMemBlockStart := _GetMemBlockStart(pMemory)
        IF pMemBlockStart:IsValid()
            RETURN pMemBlockStart:dwGroup
        ENDIF
        RETURN 0

    [MethodImpl(MethodImplOptions.AggressiveInlining)];
    PRIVATE STATIC METHOD _GetMemBlockStart (pMemory AS IntPtr) AS FixedMemBlockStart PTR
        LOCAL pMemBlockStart  AS FixedMemBlockStart PTR
        LOCAL i64 as Int64
        i64 := IIF( Is32Bits, pMemory:ToInt32(), pMemory:ToInt64() ) - SIZEOF(FixedMemBlockStart)
        pMemBlockStart := (FixedMemBlockStart PTR) IntPtr{i64}:ToPointer()
        RETURN pMemBlockStart

    [MethodImpl(MethodImplOptions.AggressiveInlining)];
    PRIVATE STATIC METHOD _GetMemBlockEnd (pMemory AS IntPtr ) AS FixedMemBlockEnd PTR
        VAR pMemBlockStart := _GetMemBlockStart (pMemory)
        LOCAL i64 as Int64
        i64 := IIF( Is32Bits, pMemory:ToInt32(), pMemory:ToInt64() )
        RETURN ( FixedMemBlockEnd PTR) IntPtr{i64 + pMemBlockStart:dwSize}:ToPointer()


    STATIC METHOD Alloc(nGroup AS DWORD, nSize AS DWORD) AS IntPtr
        LOCAL pResult AS IntPtr
        LOCAL pBlock  AS IntPtr
        LOCAL nTotal  AS DWORD
        LOCAL pMemBlockStart AS FixedMemBlockStart PTR
        LOCAL oGroup	AS MemGroup
        oGroup := FindGroup(nGroup)
        IF oGroup == NULL_OBJECT
            RETURN IntPtr.Zero
        ENDIF
        nTotal	:= nSize + SIZEOF(FixedMemBlockStart) + SIZEOF(FixedMemBlockEnd)
        pBlock := Marshal.AllocHGlobal( (INT) nTotal)
        IF pBlock != IntPtr.Zero
            // Clear whole block
            Clear(pBlock, nTotal)
            // Keep track of allocated memory per Group and Total
            oGroup:Allocated += nSize
            Total 			 += nSize
            pMemBlockStart := (FixedMemBlockStart PTR) pBlock
            pMemBlockStart:Initialize(nGroup, nSize)
            IF Is32Bits
                pResult		 := (IntPtr) (pBlock:ToInt32() + SIZEOF(FixedMemBlockStart) )
            ELSE
                pResult		 := (IntPtr) (pBlock:ToInt64() + SIZEOF(FixedMemBlockStart) )
            ENDIF
            VAR pMemBlockEnd := _GetMemBlockEnd(pResult)
            pMemBlockEnd:Initialize()
        ELSE
            pResult := NULL
        ENDIF
        IF MemTrace
            AllocatedBlocks:TryAdd(pResult, nSize)
        ENDIF
        RETURN pResult

    STATIC METHOD Free(pMem AS IntPtr) AS WORD // For compatibility with VO
        LOCAL result	AS WORD
        LOCAL oGroup	AS MemGroup
        result := FixedMemory.FAILURE
        TRY
            IF Validate(pMem)
                LOCAL nSize AS DWORD
                IF MemTrace .AND. AllocatedBlocks:ContainsKey(pMem)
                    AllocatedBlocks:TryRemove(pMem, out nSize)
                ENDIF
                VAR pMemBlockStart  := _GetMemBlockStart (pMem)
                nSize 				:= pMemBlockStart:dwSize
                VAR nTotal			:= nSize + SIZEOF(FixedMemBlockStart) + SIZEOF(FixedMemBlockEnd)
                Total -= nSize
                oGroup := FindGroup(pMemBlockStart:dwGroup)
                // Overwrite memory including header and footer so it will not be valid anymore
                FixedMemory.Set(pMemBlockStart, 0xFF, nTotal)
                IF oGroup != NULL_OBJECT
                    oGroup:Allocated -= nSize
                    Marshal.FreeHGlobal(pMemBlockStart)
                    result := FixedMemory.SUCCESS
                ENDIF
            ELSE
                // TODO: Throw an exception or log the result when FixedMemory.Free fails
                NOP
            ENDIF
        CATCH
            result := FixedMemory.FAILURE
        END TRY
        RETURN result

    STATIC METHOD Validate(pMem AS IntPtr) AS LOGIC
        LOCAL lValid := FALSE	AS LOGIC
        TRY
            IF (pMem != IntPtr.Zero)
                VAR pMemBlockStart := _GetMemBlockStart (pMem)
                IF pMemBlockStart:IsValid()
                    VAR pMemBlockEnd   := _GetMemBlockEnd (pMem)
                    IF pMemBlockEnd:IsValid()
                        lValid := TRUE
                    ENDIF
                ENDIF
            ENDIF
        CATCH
            lValid := FALSE
        END TRY
        RETURN lValid

    STATIC METHOD BlockSize(pMem AS IntPtr) AS DWORD
        LOCAL nSize  := 0 AS DWORD
        TRY
            IF (pMem != IntPtr.Zero)
                VAR pMemBlockStart := _GetMemBlockStart( pMem )
                IF pMemBlockStart:IsValid()
                    nSize := pMemBlockStart:dwSize
                ENDIF
            ENDIF
        CATCH
            nSize := 0
        END TRY
        RETURN nSize

    STATIC METHOD Realloc(pMem AS IntPtr, nNewSize AS DWORD) AS IntPtr
        LOCAL pResult := IntPtr.Zero AS IntPtr
        TRY
            IF (pMem != IntPtr.Zero)
                VAR pMemBlockStart := _GetMemBlockStart(pMem)
                IF pMemBlockStart:IsValid()
                    VAR pMemBlockEnd := _GetMemBlockEnd(pMem)
                    IF (pMemBlockEnd:IsValid())
                        LOCAL nOldSize AS DWORD
                        nOldSize := pMemBlockStart:dwSize
                        IF nOldSize == nNewSize
                            // Nothing to do.
                            RETURN pMem
                        ENDIF
                        VAR nGroup := pMemBlockStart:dwGroup
                        IF nOldSize > nNewSize
                            // Adjust end of block marker and size in header
                            // Adjust allocated sizes
                            var oGroup := FindGroup(nGroup)
                            IF oGroup != NULL_OBJECT
                                oGroup:Allocated -= nOldSize
                            ENDIF
                            Total -= nOldSize
                            pResult := pMem
                            // Block start was not changed
                            pMemBlockStart:Initialize(nGroup, nNewSize )
                            pMemBlockEnd := _GetMemBlockEnd(pResult)
                            pMemBlockEnd:Initialize()
                            IF oGroup != NULL_OBJECT
                                oGroup:Allocated += nNewSize
                            ENDIF
                            IF MemTrace
                                AllocatedBlocks[pResult] := nNewSize
                            ENDIF
                            Total += nNewSize
                        ELSE
                            // allocate new block
                            pResult := Alloc(pMemBlockStart:dwGroup, nNewSize)
                            // copy data over
                            Copy(pResult, pMem, nOldSize)
                            // free old block
                            Free(pMem)
                        ENDIF
                    ENDIF
                ENDIF
            ELSE
                pResult := Alloc(0, nNewSize)
            ENDIF
        CATCH
            pResult := NULL
        END TRY
        RETURN pResult

        [MethodImpl(MethodImplOptions.AggressiveInlining)];
    INTERNAL STATIC METHOD Clear(pMemory AS IntPtr, iCount AS DWORD) AS IntPtr
        // No pointer validation for speed. Should be done in wrapper function
        FixedMemory.Set(pMemory, 0, iCount)
        RETURN pMemory


    INTERNAL STATIC METHOD Copy( pDestination AS IntPtr, pSource AS IntPtr, iCount AS DWORD ) AS IntPtr
        // No pointer validation for speed. Should be done in wrapper function
        // Buffer.MemoryCopy handles overlapping buffers
        Buffer.MemoryCopy(pSource, pDestination, iCount, iCount)
        RETURN pDestination

    INTERNAL STATIC METHOD Set( pMemory AS IntPtr, b AS BYTE, iCount AS DWORD ) AS IntPtr
#ifdef NET5_0_OR_GREATER
        Unsafe.InitBlockUnaligned((BYTE PTR) pMemory, b,  iCount)
#else
        LOCAL pByte := (BYTE PTR) pMemory AS BYTE PTR
        IF iCount >= 32
            // Replicate the byte into all 8 bytes of a 64 bit value
            LOCAL u64 := (UINT64) b * (UINT64) 0x0101010101010101 AS UINT64
            // Write the leading bytes so the 64 bit writes are aligned
            LOCAL nLead := (DWORD) ((8 - (pMemory:ToInt64() & 7)) & 7) AS DWORD
            DO WHILE nLead > 0
                pByte[1] := b
                pByte += 1
                nLead -= 1
                iCount -= 1
            ENDDO
            LOCAL pU64 := (UINT64 PTR) pByte AS UINT64 PTR
            IF iCount >= 256
                // Large block: seed the first 64 bytes and then keep doubling the filled
                // area with Buffer.MemoryCopy. Source and target never overlap.
                LOCAL nFilled := 0 AS DWORD
                DO WHILE nFilled < 64
                    pU64[1] := u64
                    pU64 += 1
                    nFilled += 8
                ENDDO
                DO WHILE nFilled < iCount
                    LOCAL nCopy := Math.Min(nFilled, iCount - nFilled) AS DWORD
                    Buffer.MemoryCopy(pByte, pByte + nFilled, (INT64) nCopy, (INT64) nCopy)
                    nFilled += nCopy
                ENDDO
                RETURN pMemory
            ENDIF
            // 32 bytes per iteration
            DO WHILE iCount >= 32
                pU64[1] := u64
                pU64[2] := u64
                pU64[3] := u64
                pU64[4] := u64
                pU64 += 4
                iCount -= 32
            ENDDO
            DO WHILE iCount >= 8
                pU64[1] := u64
                pU64 += 1
                iCount -= 8
            ENDDO
            pByte := (BYTE PTR) pU64
        ENDIF
        // remaining bytes
        DO WHILE iCount > 0
            pByte[1] := b
            pByte += 1
            iCount -= 1
        ENDDO
#endif
        RETURN pMemory

END CLASS


/// <include file="XSharp.RT.Docs.xml" path="doc/FixedMemBlockStart/*" />
[StructLayout(LayoutKind.Explicit)];
STRUCTURE XSharp.FixedMemBlockStart
    /// <include file="XSharp.RT.Docs.xml" path="doc/FixedMemBlockStart.dwMagic/*" />
    [FieldOffset(00)] EXPORT dwMagic AS DWORD	// Checksum
    /// <include file="XSharp.RT.Docs.xml" path="doc/FixedMemBlockStart.dwCargo/*" />
    [FieldOffset(04)] EXPORT dwCargo AS DWORD    // Can be used by them
    /// <include file="XSharp.RT.Docs.xml" path="doc/FixedMemBlockStart.dwGroup/*" />
    [FieldOffset(08)] EXPORT dwGroup AS DWORD    // Group Number
    /// <include file="XSharp.RT.Docs.xml" path="doc/FixedMemBlockStart.dwSize/*" />
    [FieldOffset(12)] EXPORT dwSize  AS DWORD	// Size of Data Block excluding Guard Blocks
    /// <exclude />
    CONST MAGIC  := 0x21522358 AS DWORD  // !R#X

    /// <exclude />
    [MethodImpl(MethodImplOptions.AggressiveInlining)];
    METHOD Initialize(nGroup AS DWORD, nSize AS DWORD) AS VOID
        dwMagic := MAGIC
        dwCargo := 0
        dwGroup := nGroup
        dwSize  := nSize

    /// <exclude />
    [MethodImpl(MethodImplOptions.AggressiveInlining)];
    METHOD IsValid() AS LOGIC
        RETURN SELF:dwMagic == MAGIC
END STRUCTURE

/// <include file="XSharp.RT.Docs.xml" path="doc/FixedMemBlockEnd/*" />
[StructLayout(LayoutKind.Explicit)];
STRUCTURE XSharp.FixedMemBlockEnd
    /// <include file="XSharp.RT.Docs.xml" path="doc/FixedMemBlockEnd.dwZero/*" />
    [FieldOffset(00)] EXPORT dwZero  AS DWORD			// Give them 1 extra DWORD to protect against overflows
    /// <include file="XSharp.RT.Docs.xml" path="doc/FixedMemBlockEnd.dwMagic/*" />
    [FieldOffset(04)] EXPORT dwMagic AS DWORD			//
    /// <exclude />
    CONST MAGIC  := 0x524E4643 AS DWORD  // Chris, Fabrice, Nikos, Robert


    /// <exclude/>
    [MethodImpl(MethodImplOptions.AggressiveInlining)];
    METHOD Initialize() AS VOID
        dwMagic := MAGIC
        dwZero := 0
    /// <exclude/>
    [MethodImpl(MethodImplOptions.AggressiveInlining)];
    METHOD IsValid() AS LOGIC
        RETURN SELF:dwMagic == MAGIC

END STRUCTURE
