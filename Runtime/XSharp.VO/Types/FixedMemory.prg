//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

using System.Runtime.InteropServices
using System.Reflection
using System.Reflection.Emit
using System.Collections.Generic
using System.Runtime.CompilerServices


/// <Summary>Class that holds the Fixed Memory allocation support</Summary>
///

delegate MemWalker(pMem as IntPtr, nSize as dword) as logic


static unsafe class XSharp.FixedMemory
	public const FAILURE := 65535 as word
	public const SUCCESS := 0 as word
	internal initonly static Is32Bits as logic
	internal static Groups		as Dictionary<dword, MemGroup>
	internal static LastGroup	as dword
	internal static Total		as dword
	internal static MemTrace	as logic
	internal static AllocatedBlocks as Dictionary<IntPtr, dword> 
	private static _memSetDelegate  as Action<IntPtr, byte, int>		
	private static _memCopyDelegate as Action<IntPtr, IntPtr, int>		
	
	static constructor()
		Groups			:= Dictionary<dword, MemGroup>{}
		AllocatedBlocks := Dictionary<IntPtr, dword>{}
		AddGroup(1)
		LastGroup := 1
		Total	  := 0
		MemTrace  := false
		Is32Bits  := IntPtr.Size == 4
		
		// Generate 2 dynamic methods for speedy MemSet and MemCopy
		// using IL instructions that C# and X# do not have. 
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
	
	
	internal static method AddGroup(nGroup as dword) as MemGroup
		local oGroup as MemGroup
		oGroup := MemGroup{nGroup}
		Groups:Add(nGroup, oGroup)
		return oGroup
	
	internal static method AddGroup() as MemGroup
		LastGroup += 1
		return FixedMemory.AddGroup(LastGroup)
	
	
	internal static method FindGroup(nGroup as dword) as MemGroup
		if Groups:ContainsKey(nGroup)
			return Groups[nGroup]
		endif
		return null_object
	
	internal static method DeleteGroup(nGroup as dword) as logic
		local oGroup as MemGroup
		local lOk	:= false as logic
		oGroup := FindGroup(nGroup)
		if oGroup != null_object
			oGroup:Free()
			Groups:Remove(nGroup)
			lOk := true
		endif
		return lOk
	
	internal static method GetGroup(pMemory as IntPtr) as dword
		var pMemBlockStart := _GetMemBlockStart(pMemory)
		if pMemBlockStart:IsValid()
			return pMemBlockStart:dwGroup
		endif
		return 0
	// Only available in 4.5
	//[MethodImpl(MethodImplOptions.AggressiveInlining)];
	private static method _GetMemBlockStart (pMemory as IntPtr) as FixedMemBlockStart ptr
		local pMemBlockStart  as FixedMemBlockStart ptr
		if Is32Bits
			pMemBlockStart := (FixedMemBlockStart ptr) (pMemory:ToInt32() - sizeof(FixedMemBlockStart))
		else
			pMemBlockStart := (FixedMemBlockStart ptr) (pMemory:ToInt64() - sizeof(FixedMemBlockStart))
		endif
		return pMemBlockStart
	
	// Only available in 4.5
	//[MethodImpl(MethodImplOptions.AggressiveInlining)];
	private static method _GetMemBlockEnd (pMemory as IntPTR ) as FixedMemBlockEnd ptr
		var pMemBlockStart := _GetMemBlockStart (pMemory)
		if Is32Bits
			return ( FixedMemBlockEnd ptr) ( pMemory:ToInt32() + pMemBlockStart:dwSize)
		else
			return ( FixedMemBlockEnd ptr) (pMemory:ToInt64() + pMemBlockStart:dwSize)
		endif
	
	
	static method Alloc(nGroup as dword, nSize as dword) as IntPtr
		local pResult as IntPtr
		local pBlock  as IntPtr
		local nTotal  as dword
		local pMemBlockStart as FixedMemBlockStart ptr
		local oGroup	as MemGroup
		oGroup := FindGroup(nGroup)
		if oGroup == null_object
			return IntPtr.Zero
		endif 
		nTotal	:= nSize + sizeof(FixedMemBlockStart) + sizeof(FixedMemBlockEnd)
		pBlock := Marshal.AllocHGlobal( (int) nTotal)
		if pBlock != IntPtr.Zero
			// Keep track of allocated memory per Group and Total
			oGroup:Allocated += nSize
			Total 			 += nSize				
			pMemBlockStart := (FixedMemBlockStart ptr) pBlock 
			pMemBlockStart:Initialize(nGroup, nSize)
			if Is32Bits
				pResult		 := (IntPtr) (pBlock:ToInt32() + sizeof(FixedMemBlockStart) )
			else
				pResult		 := (IntPtr) (pBlock:ToInt64() + sizeof(FixedMemBlockStart) )
			endif
			var pMemBlockEnd := _GetMemBlockEnd(pResult)
			pMemBlockEnd:Initialize()
			// Clear data Part of the Block
			_memSetDelegate(pResult, 0, (int) nSize)
		else
			pResult := null
		endif
		if MemTrace
			AllocatedBlocks:Add(pResult, nSize)
		endif
		return pResult
	
	static method Free(pMem as IntPtr) as word // For compatibility with VO
		local result	as word
		local oGroup	as MemGroup
		result := FixedMemory.FAILURE	
		try
			if Validate(pMem)
				local nSize as dword
				if AllocatedBlocks:ContainsKey(pMem)
					AllocatedBlocks:Remove(pMem)
				endif
				var pMemBlockStart  := _GetMemBlockStart (pMem)
				nSize 				:= pMemBlockStart:dwSize
				var nTotal			:= nSize + sizeof(FixedMemBlockStart) + sizeof(FixedMemBlockEnd)
				Total -= nSize
				oGroup := FindGroup(pMemBlockStart:dwGroup)
				// Clear memory so it will not be valid anymore
				Set(pMemBlockStart, 0, (int) nTotal)			
				if oGroup != null_object
					oGroup:Allocated -= nSize
					Marshal.FreeHGlobal(pMemBlockStart)
					result := FixedMemory.SUCCESS
				endif
			endif
		catch
			result := FixedMemory.FAILURE
		end try
		return result
	
	static method Validate(pMem as IntPtr) as logic
		local lValid := false	as logic
		try
			if (pMem != IntPtr.Zero)
				var pMemBlockStart := _GetMemBlockStart (pMem)
				if pMemBlockStart:IsValid() 
					var pMemBlockEnd   := _GetMemBlockEnd (pMem)
					if pMemBlockEnd:IsValid()
						lValid := true
					endif
				endif
			endif
		catch
			lValid := false
		end try
		return lValid
	
	static method ValidateSize(pMem as IntPtr, nSize as dword) as logic
		local lValid := false	as logic
		try
			if (pMem != IntPtr.Zero)
				var pMemBlockStart := _GetMemBlockStart (pMem)
				if pMemBlockStart:IsValid() 
					var pMemBlockEnd   := _GetMemBlockEnd (pMem)
					if pMemBlockEnd:IsValid() .AND. pMemBlockStart:dwSize == nSize
						lValid := true
					endif
				endif
			endif
		catch
			lValid := false
		end try
		return lValid
	
	static method BlockSize(pMem as IntPtr) as dword
		local nSize  := 0 as dword
		try
			if (pMem != IntPtr.Zero)
				var pMemBlockStart := _GetMemBlockStart( pMem )
				if pMemBlockStart:IsValid()
					nSize := pMemBlockStart:dwSize
				endif
			endif
		catch
			nSize := 0
		end try
		return nSize
	
	static method Realloc(pMem as IntPtr, nNewSize as dword) as IntPtr
		local pResult := IntPtr.Zero as IntPtr
		try
			if (pMem != IntPtr.Zero)
				var pMemBlockStart := _GetMemBlockStart(pMem)
				if pMemBlockStart:IsValid()
					var pMemBlockEnd := _GetMemBlockEnd(pMem)
					if (pMemBlockEnd:IsValid())
						local nOldSize as dword
						nOldSize := pMemBlockStart:dwSize
						if nOldSize == nNewSize
							pResult := pMem
						elseif nOldSize > nNewSize
							// clear end of block
							local pClear as IntPtr
							if Is32Bits
								pClear := (IntPtr) pMem:ToInt32()+ nOldSize
							else
								pClear := (IntPtr) pMem:ToInt64()+ nOldSize
							endif
							Clear( pClear, (int) (nOldSize - (int) nNewSize))
							pResult := pMem
						else
							// allocate new block
							pResult := Alloc(pMemBlockStart:dwGroup, nNewSize)
							// copy data over
							Copy(pResult, pMem, (int) nOldSize)
							// free old block
							Free(pMem)
						endif
					endif
				endif
			else
				pResult := Alloc(0, nNewSize)
			endif
		catch
			pResult := null
		end try
		return pResult
	
	internal static method Clear(pMemory as IntPtr, iCount as int) as IntPTR
		// No pointer validation for speed. Should be done in wrapper function
		_memSetDelegate(pMemory, 0, iCount)
		return pMemory
	
	
	internal static method Copy( pDestination as IntPtr, pSource as IntPtr, iCount as int ) as IntPtr
		// No pointer validation for speed. Should be done in wrapper function
		_memCopyDelegate(pDestination, pSource, iCount)
		return pDestination
	
	internal static method Set( pMemory as IntPtr, b as byte, iCount as int ) as IntPtr
		// No pointer validation for speed. Should be done in wrapper function
		_memSetDelegate(pMemory, b, iCount)
		return pMemory
	
	
end class


/// <Summary>Guard Block preceding MemAlloc return value</Summary>
[StructLayout(LayoutKind.Explicit)];
structure	 XSharp.FixedMemBlockStart
	[FieldOffSet(00)] export dwMagic as dword	// Checksum
	[FieldOffSet(04)] export dwCargo as dword    // Can be used by them
	[FieldOffSet(08)] export dwGroup as dword    // Group Number
	[FieldOffSet(12)] export dwSize  as dword	// Size of Data Block excluding Guard Blocks
	const MAGIC  := 0x21522358 as dword  // !R#X
	
	method Initialize(nGroup as dword, nSize as dword) as void
		dwMagic := MAGIC
		dwCargo := 0
		dwGroup := nGroup
		dwSize  := nSize
	
	method IsValid() as logic
		return self:dwMagic == MAGIC
	
	
end structure

/// <Summary>Guard Block following MemAlloc return value</Summary>
[StructLayout(LayoutKind.Explicit)];
structure	 XSharp.FixedMemBlockEnd
	[FieldOffSet(00)] export dwZero  as dword			// Give them 1 extra DWORD to protect against overflows
	[FieldOffSet(04)] export dwMagic as dword			// Checksum
	const MAGIC  := 0x524E4643 as dword  // Chris, Fabrice, Nikos, Robert 
	
	
	method Initialize() as void
		dwMagic := MAGIC
		dwZero  := 0
	
	method IsValid() as logic
		return self:dwMagic == MAGIC
	
end structure
