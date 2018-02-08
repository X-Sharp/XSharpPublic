//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Runtime.InteropServices
USING System.Reflection
USING System.Reflection.Emit
USING System.Collections.Generic

#define CLEARBLOCK


/// <Summary>Class that holds the Fixed Memory allocation support</Summary>
///


STATIC UNSAFE CLASS XSharp.FixedMemory
	PUBLIC CONST FAILURE := 65535 AS WORD
	PUBLIC CONST SUCCESS := 0 AS WORD
	INTERNAL STATIC Groups		AS Dictionary<DWORD, MemGroup>
	INTERNAL STATIC LastGroup	AS DWORD
	INTERNAL STATIC Total		AS DWORD
	PRIVATE STATIC _memSetDelegate  AS Action<IntPtr, BYTE, INT>		
	PRIVATE STATIC _memCopyDelegate AS Action<IntPtr, IntPtr, INT>		

	STATIC CONSTRUCTOR()
		Groups := Dictionary<DWORD, MemGroup>{}
		AddGroup(0)
		LastGroup := 0
		Total	  := 0


		// Generate 2 dynamic methods for speedy MemSet and MemCopy
		var atts := MethodAttributes.Public | MethodAttributes.Static
		var dm := DynamicMethod{"Memset", atts, CallingConventions.Standard, null,  <System.Type> { typeof(IntPtr), typeof(byte), typeof(int) }, typeof(FixedMemory), true}
        var generator	  := dm:GetILGenerator()
        generator:Emit(OpCodes.Ldarg_0)
        generator:Emit(OpCodes.Ldarg_1)
        generator:Emit(OpCodes.Ldarg_2)
        generator:Emit(OpCodes.Initblk)
        generator:Emit(OpCodes.Ret)
		_memsetDelegate := (Action<IntPtr, byte, int>) dm:CreateDelegate(typeof(Action<IntPtr, byte, int>))
		dm := DynamicMethod{"Memcopy", atts, CallingConventions.Standard, null,  <System.Type> { typeof(IntPtr), typeof(IntPtr), typeof(int) }, typeof(FixedMemory), true}

        generator := dm:GetILGenerator()
        generator:Emit(OpCodes.Ldarg_0)
        generator:Emit(OpCodes.Ldarg_1)
        generator:Emit(OpCodes.Ldarg_2)
        generator:Emit(OpCodes.Cpblk)
        generator:Emit(OpCodes.Ret)
		_memCopyDelegate := (Action<IntPtr, IntPtr, int>) dm:CreateDelegate(typeof(Action<IntPtr, IntPtr, int>))


	INTERNAL STATIC METHOD AddGroup(nGroup as DWORD) AS MemGroup
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
		LOCAL lOk	:= FALSE as LOGIC
		oGroup := FindGroup(nGroup)
		IF oGroup != NULL_OBJECT
			oGroup:Free()
			Groups:Remove(nGroup)
			lOk := TRUE
		ENDIF
		RETURN lOk


	STATIC UNSAFE METHOD Alloc(nGroup AS DWORD, nSize AS DWORD) AS IntPtr
		LOCAL pResult AS BYTE PTR
		LOCAL pBlock  AS BYTE PTR
		LOCAL nTotal  AS DWORD
		LOCAL pMemBlockStart AS MemBlockStart PTR
		LOCAL pMemBlockEnd AS MemBlockEnd PTR
		LOCAL oGroup	AS MemGroup
		oGroup := FindGroup(nGroup)
		IF oGroup == NULL_OBJECT
			RETURN IntPtr.Zero
		ENDIF 
		nTotal	:= nSize + SIZEOF(MemBlockStart) + SIZEOF(MemBlockEnd)
		pBlock := Marshal.AllocHGlobal( (INT) nTotal)
		IF pBlock != IntPtr.Zero
			oGroup:Allocated += nSize
			Total += nSize				// Keep track of total allocated memory
			pMemBlockStart := (MemBlockStart PTR) ((DWORD) pBlock )
			
			pMemBlockStart:Initialize(nGroup, nSize)
			pResult		 := (Byte PTR)        ((INT) pBlock + SIZEOF(MemBlockStart) )
			pMemBlockEnd := (MemBlockEnd Ptr) ((INT) pBlock + SIZEOF(MemBlockStart) + nSize )
			pMemBlockEnd:Initialize()
			_memSetDelegate(pResult, 0, (int) nSize)
		ELSE
			pResult := NULL
		ENDIF
		RETURN pResult

	STATIC UNSAFE METHOD Free(pMem AS IntPtr) AS WORD // For compatibility with VO
		LOCAL pMemBlockStart  AS MemBlockStart PTR
		LOCAL result	AS WORD
		LOCAL oGroup	as MemGroup
		result := FixedMemory.FAILURE	// VO defines this as error
		TRY
			IF Validate(pMem)
				LOCAL pStart	as IntPtr
				pMemBlockStart := (MemBlockStart PTR) ((INT) pMem - SIZEOF(MemBlockStart))

				pStart := pMemBlockStart

				LOCAL nSize AS DWORD
				nSize := pMemBlockStart:dwSize
				VAR nTotal	:= nSize + SIZEOF(MemBlockStart) + SIZEOF(MemBlockEnd)
				Total -= nSize
				oGroup := FindGroup(pMemBlockStart:dwGroup)
#ifdef CLEARBLOCK
					Set(pMemBlockStart, 0, (INT) nTotal)			// Clear memory so it will not be valid anymore
#endif
				IF oGroup != NULL_OBJECT
					oGroup:Allocated -= nSize
					Marshal.FreeHGlobal(pStart)
					result := FixedMemory.SUCCESS
				ENDIF
			ENDIF
		CATCH
			result := FixedMemory.FAILURE
		END TRY
		RETURN result

	STATIC UNSAFE METHOD Validate(pMem AS IntPtr) AS LOGIC
		LOCAL pMemBlockStart AS MemBlockStart PTR
		LOCAL pMemBlockEnd AS MemBlockEnd PTR
		LOCAL lValid := FALSE	AS LOGIC
		TRY
			IF (pMem != NULL_PTR)
				pMemBlockStart := (MemBlockStart PTR) ((INT) pMem - SizeOf(MemBlockStart))
				IF pMemBlockStart:IsValid()
					pMemBlockEnd := (MemBlockEnd PTR) ((INT) pMem + pMemBlockStart:dwSize) 
					IF (pMemBlockEnd:IsValid())
						lValid := TRUE
					ENDIF
				ENDIF
			ENDIF
		CATCH
			lValid := FALSE
		END TRY
		RETURN lValid

	STATIC UNSAFE METHOD ValidateSize(pMem AS IntPtr, nSize as DWORD) AS LOGIC
		LOCAL pMemBlockStart AS MemBlockStart PTR
		LOCAL pMemBlockEnd AS MemBlockEnd PTR
		LOCAL lValid := FALSE	AS LOGIC
		TRY
			IF (pMem != NULL_PTR)
				pMemBlockStart := (MemBlockStart PTR) ((INT) pMem - SIZEOF(MemBlockStart))
				IF pMemBlockStart:IsValid()
					pMemBlockEnd := (MemBlockEnd PTR) ((INT) pMem + pMemBlockStart:dwSize) 
					IF (pMemBlockEnd:IsValid())
						IF pMemBlockStart:dwSize == nSize
							lValid := TRUE
						ENDIF
					ENDIF
				ENDIF
			ENDIF
		CATCH
			lValid := FALSE
		END TRY
		RETURN lValid

	STATIC UNSAFE METHOD BlockSize(pMem AS IntPtr) AS DWORD
		LOCAL pMemBlockStart AS MemBlockStart PTR
		LOCAL nSize  := 0 as DWORD
		TRY
			IF (pMem != NULL_PTR)
				pMemBlockStart := (MemBlockStart PTR) ((INT) pMem - SIZEOF(MemBlockStart))
				IF pMemBlockStart:IsValid()
					nSize := pMemBlockStart:dwSize
				ENDIF
			ENDIF
		CATCH
			nSize := 0
		END TRY
		RETURN nSize

	STATIC UNSAFE METHOD Realloc(pMem AS IntPtr, nNewSize AS DWORD) AS IntPtr
		LOCAL pMemBlockStart AS MemBlockStart PTR
		LOCAL pMemBlockEnd AS MemBlockEnd PTR
		LOCAL pResult := NULL	AS IntPtr
		TRY
			IF (pMem != NULL_PTR)
				pMemBlockStart := (MemBlockStart PTR) ((INT) pMem - SIZEOF(MemBlockStart))
				IF pMemBlockStart:IsValid()
					pMemBlockEnd := (MemBlockEnd PTR) ((INT) pMem + pMemBlockStart:dwSize) 
					IF (pMemBlockEnd:IsValid())
						LOCAL nOldSize as DWORD
						nOldSize := pMemBlockStart:dwSize
						IF nOldSize == nNewSize
							pResult := pMem
						ELSEIF nOldSize > nNewSize
							// clear end of block
							Clear( (PTR) ((DWORD)pMem + nOldSize), (int) (nOldSize - (int) nNewSize))
							pResult := pMem
						ELSE
							// allocate new block
							pResult := Alloc(pMemBlockStart:dwGroup, nNewSize)
							// copy data over
							Copy(pResult, pMem, (int) nOldSize)
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

		INTERNAL STATIC METHOD Clear(pMemory AS IntPtr, iCount AS INT) AS IntPTR
			// No pointer validation. Should be done in wrapper function
			_memSetDelegate(pMemory, 0, iCount)
			return pMemory
			

		INTERNAL STATIC METHOD Copy( pDestination AS IntPtr, pSource AS IntPtr, iCount AS INT ) AS IntPtr
			// No pointer validation. Should be done in wrapper function
			_memCopyDelegate(pDestination, pSource, iCount)
			RETURN pDestination

		INTERNAL STATIC METHOD Set( pMemory AS IntPtr, b AS BYTE, iCount AS INT ) AS IntPtr
			// No pointer validation. Should be done in wrapper function
			_memSetDelegate(pMemory, b, iCount)
			RETURN pMemory
 
		INTERNAL STATIC METHOD Walk() AS VOID
			#ifdef LINKEDBLOCKS
				// Use the linked list of blocks to walk the allocated memory
			#endif


END CLASS

// Guard Block structures
[StructLayout(LayoutKind.Explicit)];
STRUCTURE XSharp.MemBlockStart
   [FieldOffSet(00)] EXPORT dwMagic as DWORD
   [FieldOffSet(04)] EXPORT dwCargo as DWORD    // Can be used by them
   [FieldOffSet(08)] EXPORT dwGroup as DWORD    // Since we have no real groups
   [FieldOffSet(12)] EXPORT dwSize  as DWORD
   CONST MAGIC  := 0x21522358 AS DWORD  // !R#X

   METHOD Initialize(nGroup as DWORD, nSize as DWORD) AS VOID
	  dwMagic := MAGIC
	  dwCargo := 0
	  dwGroup := nGroup
	  dwSize  := nSize

   METHOD IsValid() AS LOGIC
		RETURN SELF:dwMagic == MAGIC
   

END STRUCTURE

[StructLayout(LayoutKind.Explicit)];
STRUCTURE XSharp.MemBlockEnd
   [FieldOffSet(00)] EXPORT dwZero  as DWORD
   [FieldOffSet(04)] EXPORT dwMagic as DWORD
   CONST MAGIC  := 0x524E4643 AS DWORD  // Chris, Fabrice, Nikos, Robert 


   METHOD Initialize() AS VOID
		dwMagic := MAGIC
		dwZero  := 0

   METHOD IsValid() AS LOGIC
		RETURN SELF:dwMagic == MAGIC

END STRUCTURE
