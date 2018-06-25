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


/// <summary>Class that holds the Fixed Memory allocation support</summary>
///

DELEGATE MemWalker(pMem AS IntPtr, nSize AS DWORD) AS LOGIC


STATIC UNSAFE CLASS XSharp.FixedMemory
    PUBLIC CONST FAILURE := 65535 AS WORD
    PUBLIC CONST SUCCESS := 0 AS WORD
    INTERNAL INITONLY STATIC Is32Bits AS LOGIC
    INTERNAL STATIC Groups		AS Dictionary<DWORD, MemGroup>
    INTERNAL STATIC LastGroup	AS DWORD
    INTERNAL STATIC Total		AS DWORD
    INTERNAL STATIC MemTrace	AS LOGIC
    INTERNAL STATIC AllocatedBlocks AS Dictionary<IntPtr, DWORD> 
    PRIVATE STATIC _memSetDelegate  AS Action<IntPtr, BYTE, INT>		
    PRIVATE STATIC _memCopyDelegate AS Action<IntPtr, IntPtr, INT>		
    
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
        VAR atts := MethodAttributes.Public | MethodAttributes.Static
        VAR dm := DynamicMethod{"Memset", atts, CallingConventions.Standard, NULL,  <System.Type> { TYPEOF(IntPtr), TYPEOF(BYTE), TYPEOF(INT) }, TYPEOF(FixedMemory), TRUE}
        VAR generator	  := dm:GetILGenerator()
        generator:Emit(OpCodes.Ldarg_0)
        generator:Emit(OpCodes.Ldarg_1)
        generator:Emit(OpCodes.Ldarg_2)
        generator:Emit(OpCodes.Initblk)
        generator:Emit(OpCodes.Ret)
        _memsetDelegate := (Action<IntPtr, BYTE, INT>) dm:CreateDelegate(TYPEOF(Action<IntPtr, BYTE, INT>))
        dm := DynamicMethod{"Memcopy", atts, CallingConventions.Standard, NULL,  <System.Type> { TYPEOF(IntPtr), TYPEOF(IntPtr), TYPEOF(INT) }, TYPEOF(FixedMemory), TRUE}
        
        generator := dm:GetILGenerator()
        generator:Emit(OpCodes.Ldarg_0)
        generator:Emit(OpCodes.Ldarg_1)
        generator:Emit(OpCodes.Ldarg_2)
        generator:Emit(OpCodes.Cpblk)
        generator:Emit(OpCodes.Ret)
        _memCopyDelegate := (Action<IntPtr, IntPtr, INT>) dm:CreateDelegate(TYPEOF(Action<IntPtr, IntPtr, INT>))
        
        
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
        IF Is32Bits
            pMemBlockStart := (FixedMemBlockStart PTR) (pMemory:ToInt32() - SIZEOF(FixedMemBlockStart))
        ELSE
            pMemBlockStart := (FixedMemBlockStart PTR) (pMemory:ToInt64() - SIZEOF(FixedMemBlockStart))
        ENDIF
        RETURN pMemBlockStart
        
     [MethodImpl(MethodImplOptions.AggressiveInlining)];
     PRIVATE STATIC METHOD _GetMemBlockEnd (pMemory AS IntPTR ) AS FixedMemBlockEnd PTR
        VAR pMemBlockStart := _GetMemBlockStart (pMemory)
        IF Is32Bits
            RETURN ( FixedMemBlockEnd PTR) ( pMemory:ToInt32() + pMemBlockStart:dwSize)
        ELSE
            RETURN ( FixedMemBlockEnd PTR) (pMemory:ToInt64() + pMemBlockStart:dwSize)
        ENDIF
        
        
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
            // Clear data Part of the Block
            _memSetDelegate(pResult, 0, (INT) nSize)
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
                IF AllocatedBlocks:ContainsKey(pMem)
                    AllocatedBlocks:Remove(pMem)
                ENDIF
                VAR pMemBlockStart  := _GetMemBlockStart (pMem)
                nSize 				:= pMemBlockStart:dwSize
                VAR nTotal			:= nSize + SIZEOF(FixedMemBlockStart) + SIZEOF(FixedMemBlockEnd)
                Total -= nSize
                oGroup := FindGroup(pMemBlockStart:dwGroup)
                // Overwrite memory so it will not be valid anymore
                SET(pMemBlockStart, 0xFF, (INT) nTotal)			
                IF oGroup != NULL_OBJECT
                    oGroup:Allocated -= nSize
                    Marshal.FreeHGlobal(pMemBlockStart)
                    result := FixedMemory.SUCCESS
                ENDIF
			ELSE
				// TODO: Throw an exception or log the result
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
        
    STATIC METHOD ValidateSize(pMem AS IntPtr, nSize AS DWORD) AS LOGIC
        LOCAL lValid := FALSE	AS LOGIC
        TRY
            IF (pMem != IntPtr.Zero)
                VAR pMemBlockStart := _GetMemBlockStart (pMem)
                IF pMemBlockStart:IsValid() 
                    VAR pMemBlockEnd   := _GetMemBlockEnd (pMem)
                    IF pMemBlockEnd:IsValid() .AND. pMemBlockStart:dwSize == nSize
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
                            pResult := pMem
                        ELSEIF nOldSize > nNewSize
                            // clear end of block
                            LOCAL pClear AS IntPtr
                            IF Is32Bits
                                pClear := (IntPtr) pMem:ToInt32()+ nOldSize
                            ELSE
                                pClear := (IntPtr) pMem:ToInt64()+ nOldSize
                            ENDIF
                            Clear( pClear, (INT) (nOldSize - (INT) nNewSize))
                            pResult := pMem
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
    INTERNAL STATIC METHOD Clear(pMemory AS IntPtr, iCount AS INT) AS IntPTR
        // No pointer validation for speed. Should be done in wrapper function
        _memSetDelegate(pMemory, 0, iCount)
        RETURN pMemory
        
        
    [MethodImpl(MethodImplOptions.AggressiveInlining)];        
    INTERNAL STATIC METHOD Copy( pDestination AS IntPtr, pSource AS IntPtr, iCount AS INT ) AS IntPtr
        // No pointer validation for speed. Should be done in wrapper function
        _memCopyDelegate(pDestination, pSource, iCount)
        RETURN pDestination
        
    [MethodImpl(MethodImplOptions.AggressiveInlining)];        
    INTERNAL STATIC METHOD SET( pMemory AS IntPtr, b AS BYTE, iCount AS INT ) AS IntPtr
        // No pointer validation for speed. Should be done in wrapper function
        _memSetDelegate(pMemory, b, iCount)
        RETURN pMemory
        
        
END CLASS
        
        
/// <summary>Guard Block preceding MemAlloc return value</summary>
[StructLayout(LayoutKind.Explicit)];
STRUCTURE	 XSharp.FixedMemBlockStart
    [FieldOffSet(00)] EXPORT dwMagic AS DWORD	// Checksum
    [FieldOffSet(04)] EXPORT dwCargo AS DWORD    // Can be used by them
    [FieldOffSet(08)] EXPORT dwGroup AS DWORD    // Group Number
    [FieldOffSet(12)] EXPORT dwSize  AS DWORD	// Size of Data Block excluding Guard Blocks
    CONST MAGIC  := 0x21522358 AS DWORD  // !R#X
    
   [MethodImpl(MethodImplOptions.AggressiveInlining)];
    METHOD Initialize(nGroup AS DWORD, nSize AS DWORD) AS VOID
        dwMagic := MAGIC
        dwCargo := 0
        dwGroup := nGroup
        dwSize  := nSize

   [MethodImpl(MethodImplOptions.AggressiveInlining)];
    METHOD IsValid() AS LOGIC
        RETURN SELF:dwMagic == MAGIC
        
        
END         STRUCTURE
        
/// <summary>Guard Block following MemAlloc return value</summary>
[StructLayout(LayoutKind.Explicit)];
STRUCTURE	 XSharp.FixedMemBlockEnd
    [FieldOffSet(00)] EXPORT dwZero  AS DWORD			// Give them 1 extra DWORD to protect against overflows
    [FieldOffSet(04)] EXPORT dwMagic AS DWORD			// Checksum
    CONST MAGIC  := 0x524E4643 AS DWORD  // Chris, Fabrice, Nikos, Robert 
    
    
   [MethodImpl(MethodImplOptions.AggressiveInlining)];
    METHOD Initialize() AS VOID
        dwMagic := MAGIC
		dwZero := 0        
   [MethodImpl(MethodImplOptions.AggressiveInlining)];
    METHOD IsValid() AS LOGIC
        RETURN SELF:dwMagic == MAGIC
        
END STRUCTURE
