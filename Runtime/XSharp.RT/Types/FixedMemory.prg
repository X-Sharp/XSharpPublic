//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.Runtime.InteropServices
USING System.Reflection
USING System.Reflection.Emit
USING System.Collections.Generic
USING System.Runtime.CompilerServices


/// <summary>Delegate that a client needs to implement to use MemWalk.</summary>
DELEGATE MemWalker(pMem AS IntPtr, nSize AS DWORD) AS LOGIC

INTERNAL STATIC CLASS XSharp.FixedMemory
    PUBLIC CONST FAILURE := 65535 AS WORD
    PUBLIC CONST SUCCESS := 0 AS WORD
    INTERNAL INITONLY STATIC Is32Bits AS LOGIC
    INTERNAL STATIC Groups		AS Dictionary<DWORD, MemGroup>
    INTERNAL STATIC LastGroup	AS DWORD
    INTERNAL STATIC Total		AS DWORD
    INTERNAL STATIC MemTrace	AS LOGIC
    INTERNAL STATIC AllocatedBlocks AS Dictionary<IntPtr, DWORD>
    //PRIVATE STATIC _memSetDelegate  AS Action<IntPtr, BYTE, DWORD>
    //PRIVATE STATIC _memCopyDelegate AS Action<IntPtr, IntPtr, DWORD>

    STATIC CONSTRUCTOR()
        Groups			:= Dictionary<DWORD, MemGroup>{}
        AllocatedBlocks := Dictionary<IntPtr, DWORD>{}
        AddGroup(1)
        LastGroup := 1
        Total	  := 0
        MemTrace  := FALSE
        Is32Bits  := IntPtr.Size == 4

        // Generate 2 dynamic methods for speedy MemSet and MemCopy
        // using IL instructions that C# and X# do not have.
        //VAR atts := MethodAttributes.Public | MethodAttributes.Static
        //        VAR dm := DynamicMethod{"Memset", atts, CallingConventions.Standard, NULL,  <System.Type> { TYPEOF(IntPtr), TYPEOF(BYTE), TYPEOF(DWORD) }, TYPEOF(FixedMemory), TRUE}
        //        VAR generator	  := dm:GetILGenerator()
        //        generator:Emit(OpCodes.Ldarg_0)
        //        generator:Emit(OpCodes.Ldarg_1)
        //        generator:Emit(OpCodes.Ldarg_2)
        //        generator:Emit(OpCodes.Initblk)
        //        generator:Emit(OpCodes.Ret)
        //        _memSetDelegate := (Action<IntPtr, BYTE, DWORD>) dm:CreateDelegate(TYPEOF(Action<IntPtr, BYTE, DWORD>))
        //
        //        VAR dm := DynamicMethod{"Memcopy", atts, CallingConventions.Standard, NULL,  <System.Type> { TYPEOF(IntPtr), TYPEOF(IntPtr), TYPEOF(DWORD) }, TYPEOF(FixedMemory), TRUE}
        //        VAR generator := dm:GetILGenerator()
        //        generator:Emit(OpCodes.Ldarg_0)
        //        generator:Emit(OpCodes.Ldarg_1)
        //        generator:Emit(OpCodes.Ldarg_2)
        //        generator:Emit(OpCodes.Cpblk)
        //        generator:Emit(OpCodes.Ret)
        //        _memCopyDelegate := (Action<IntPtr, IntPtr, DWORD>) dm:CreateDelegate(TYPEOF(Action<IntPtr, IntPtr, DWORD>))


    INTERNAL STATIC METHOD AddGroup(nGroup AS DWORD) AS MemGroup
        LOCAL oGroup AS MemGroup
        oGroup := MemGroup{nGroup}
        Groups:Add(nGroup, oGroup)
        RETURN oGroup

    INTERNAL STATIC METHOD AddGroup() AS MemGroup
        LastGroup += 1
        RETURN FixedMemory.AddGroup(LastGroup)


    INTERNAL STATIC METHOD FindGroup(nGroup AS DWORD) AS MemGroup
        IF Groups:ContainsKey(nGroup)
            RETURN Groups[nGroup]
        ENDIF
        RETURN NULL_OBJECT

    INTERNAL STATIC METHOD DeleteGroup(nGroup AS DWORD) AS LOGIC
        LOCAL oGroup AS MemGroup
        LOCAL lOk	:= FALSE AS LOGIC
        oGroup := FindGroup(nGroup)
        IF oGroup != NULL_OBJECT
            oGroup:Free()
            Groups:Remove(nGroup)
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
            Clear(pBlock, (INT) nTotal)
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
            AllocatedBlocks:Add(pResult, nSize)
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
                    AllocatedBlocks:Remove(pMem)
                ENDIF
                VAR pMemBlockStart  := _GetMemBlockStart (pMem)
                nSize 				:= pMemBlockStart:dwSize
                VAR nTotal			:= nSize + SIZEOF(FixedMemBlockStart) + SIZEOF(FixedMemBlockEnd)
                Total -= nSize
                oGroup := FindGroup(pMemBlockStart:dwGroup)
                // Overwrite memory including header and footer so it will not be valid anymore
                FixedMemory.Set(pMemBlockStart, 0xFF, (INT) nTotal)
                IF oGroup != NULL_OBJECT
                    oGroup:Allocated -= nSize
                    Marshal.FreeHGlobal(pMemBlockStart)
                    result := FixedMemory.SUCCESS
                ENDIF
            ELSE
                // TODO: Throw an exception or log the result when FixedMemory.Free fails
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
                            Copy(pResult, pMem, (INT) nOldSize)
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
    INTERNAL STATIC METHOD Clear(pMemory AS IntPtr, iCount AS INT) AS IntPtr
        // No pointer validation for speed. Should be done in wrapper function
        FixedMemory.Set(pMemory, 0, iCount)
        RETURN pMemory


    INTERNAL STATIC METHOD Copy( pDestination AS IntPtr, pSource AS IntPtr, iCount AS INT ) AS IntPtr
        // No pointer validation for speed. Should be done in wrapper function
        // check if we need a temporary buffer when the pointers overlap
        LOCAL lDirect as LOGIC
        if pSource + iCount < pDestination
            // source is completely before destination
            lDirect := TRUE
        elseif pDestination + iCount < pSource
            lDirect := TRUE
        ELSE // buffers Overlap
            lDirect := FALSE
        ENDIF
        IF lDirect
            Buffer.MemoryCopy(pSource, pDestination, iCount, iCount)
        ELSE
            local bTemp := BYTE[]{iCount} AS BYTE[]
            Marshal.Copy(pSource, bTemp, 0, iCount)
            Marshal.Copy(bTemp, 0,  pDestination, iCount)
        ENDIF
        RETURN pDestination

    INTERNAL STATIC METHOD Set( pMemory AS IntPtr, b AS BYTE, iCount AS INT) AS IntPtr
        // Write 8 bytes at the same time when possible
        VAR pInt64 := (INT64 PTR) pMemory
        IF iCount > 24
            LOCAL i64 AS INT64
            // The most common values that we are using are 0 (MemAlloc), 255 (MemFree) and 32 (for strings)
            SWITCH b
            CASE 0
                i64 := 0
            CASE 255
                i64 := -1
            CASE 32
                i64 := 0x2020202020202020
            OTHERWISE
                i64 := BitConverter.ToInt64( <Byte>{b,b,b,b,b,b,b,b},0)
            END SWITCH
            // copy 8 bytes per iteration
            DO WHILE iCount >= 8
                pInt64[1] := i64
                pInt64 += 1
                iCount -= 8
            ENDDO
        ENDIF
        IF iCount > 0
            VAR pByte  := (BYTE PTR) pInt64
            DO WHILE iCount > 0
                pByte[1] := b
                pByte += 1
                iCount -= 1
            ENDDO
        ENDIF
        RETURN pMemory


END CLASS


/// <summary>Guard Block preceding MemAlloc return value</summary>
[StructLayout(LayoutKind.Explicit)];
STRUCTURE	 XSharp.FixedMemBlockStart
    /// <summary>Checksum</summary>
    [FieldOffset(00)] EXPORT dwMagic AS DWORD	// Checksum
    /// <summary>Cargo slot</summary>
    [FieldOffset(04)] EXPORT dwCargo AS DWORD    // Can be used by them
    /// <summary>Group number</summary>
    [FieldOffset(08)] EXPORT dwGroup AS DWORD    // Group Number
    /// <summary>Size</summary>
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

/// <summary>Guard Block following MemAlloc return value</summary>
[StructLayout(LayoutKind.Explicit)];
STRUCTURE XSharp.FixedMemBlockEnd
    /// <summary>Zero terminator</summary>
    [FieldOffset(00)] EXPORT dwZero  AS DWORD			// Give them 1 extra DWORD to protect against overflows
    /// <summary>Checksum</summary>
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
