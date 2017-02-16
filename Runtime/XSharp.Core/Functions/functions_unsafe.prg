//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
	#region functions
	/// <summary>
	/// Create a new thread.
	/// </summary>
	/// <param name="pSecAttr"></param>
	/// <param name="nStackSize"></param>
	/// <param name="pFunc"></param>
	/// <param name="pParam"></param>
	/// <param name="dwFlags"></param>
	/// <param name="pdwID"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION CreateVOThread(pSecAttr AS PTR,nStackSize AS DWORD,pFunc AS PTR,pParam AS PTR,dwFlags AS DWORD,pdwID REF DWORD ) AS PTR
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero

	/// <summary>
	/// </summary>
	/// <param name="hWndOwner"></param>
	/// <param name="cFiles"></param>
	/// <param name="fAsynchWork"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION DoSendMail(hWndOwner AS PTR,cFiles AS STRING,fAsynchWork AS LOGIC) AS VOID
		/// THROW NotImplementedException{}
	RETURN 

	/// <summary>
	/// Encode a file for e-mail transfer.
	/// </summary>
	/// <param name="hfIn"></param>
	/// <param name="hfOut"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION EncodeBase64(hfIn AS PTR,hfOut AS PTR) AS INT
		/// THROW NotImplementedException{}
	RETURN 0   





	/// <summary>
	/// </summary>
	/// <param name="px"></param>
	/// <param name="dw"></param>
	/// <param name="pa"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION Item__ArrayPut(px AS PTR,dw AS DWORD,pa AS PTR) AS PTR
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero


	/// <summary>
	/// Functional form of BEGIN SEQUENCE.
	/// </summary>
	/// <param name="ptrLabel"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION LabelPush(ptrLabel AS PTR) AS VOID
		/// THROW NotImplementedException{}
	RETURN 

	/// <summary>
	/// Extract a substring of a certain size from the left of a buffer.
	/// </summary>
	/// <param name="ptrSource"></param>
	/// <param name="dwSize"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION Mem2String(ptrSource AS PTR,dwSize AS DWORD) AS STRING
		/// THROW NotImplementedException{}
	RETURN String.Empty   

	/// <summary>
	/// Get the location of the first special console character in a buffer.
	/// </summary>
	/// <param name="ptrBuff"></param>
	/// <param name="wCount"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION MemAtSpecial(ptrBuff AS PTR,wCount AS DWORD) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Get a pointer to a byte in a memory buffer.
	/// </summary>
	/// <param name="ptrBuff"></param>
	/// <param name="b"></param>
	/// <param name="dwCount"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION MemByte(ptrBuff AS PTR,b AS BYTE,dwCount AS DWORD) AS PTR
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero

	/// <summary>
	/// </summary>
	/// <param name="ptrDest"></param>
	/// <param name="ptrSrc"></param>
	/// <param name="wCh"></param>
	/// <param name="wCount"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION MemCCopy(ptrDest AS PTR,ptrSrc AS PTR,wCh AS DWORD,wCount AS DWORD) AS PTR
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero

	/// <summary>
	/// Check the validity of a pointer to a memory buffer.
	/// </summary>
	/// <param name="ptrMem"></param>
	/// <param name="cb"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION MemCheckPtr(ptrMem AS PTR,cb AS DWORD) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Get a pointer to a matching character value in a memory buffer.
	/// </summary>
	/// <param name="ptrBuff"></param>
	/// <param name="wChar"></param>
	/// <param name="dwCount"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION MemChr(ptrBuff AS PTR,wChar AS DWORD,dwCount AS DWORD) AS PTR
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero

	/// <summary>
	/// Fill a memory buffer with null characters.
	/// </summary>
	/// <param name="ptrBuff"></param>
	/// <param name="wCount"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION MemClear(ptrBuff AS PTR,wCount AS DWORD) AS PTR
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero

	/// <summary>
	/// Compare bytes in two memory buffers.
	/// </summary>
	/// <param name="ptr1"></param>
	/// <param name="ptr2"></param>
	/// <param name="wCount"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION MemComp(ptr1 AS PTR,ptr2 AS PTR,wCount AS DWORD) AS INT
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Copy one memory buffer to another.
	/// </summary>
	/// <param name="ptrDest"></param>
	/// <param name="ptrSrc"></param>
	/// <param name="wCount"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION MemCopy(ptrDest AS PTR,ptrSrc AS PTR,wCount AS DWORD) AS PTR
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero

	/// <summary>
	/// Copy one memory buffer to another and fill any remaining spaces with blanks.
	/// </summary>
	/// <param name="ptrDest"></param>
	/// <param name="ptrSrc"></param>
	/// <param name="wCount"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION MemCopyString(ptrDest AS PTR,ptrSrc AS STRING,wCount AS DWORD) AS VOID
		/// THROW NotImplementedException{}
	RETURN

	/// <summary>
	/// Get a pointer to a matching double word in a memory buffer.
	/// </summary>
	/// <param name="ptrBuff"></param>
	/// <param name="dw"></param>
	/// <param name="dwCount"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION MemDWord(ptrBuff AS PTR,dw AS DWORD,dwCount AS DWORD) AS PTR
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero

	/// <summary>
	/// </summary>
	/// <param name="pFunction"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION MemEnum(pFunction AS PTR) AS LONG
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Deallocate a specified memory buffer.
	/// </summary>
	/// <param name="ptrMem"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION MemFree(ptrMem AS PTR) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="ptrMem"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION MemFreeBlk(ptrMem AS PTR) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="pMem"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION MemGetHeader(pMem AS PTR) AS PTR
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero

	/// <summary>
	/// Get a pointer to a matching integer in a memory buffer.
	/// </summary>
	/// <param name="ptrBuff"></param>
	/// <param name="i"></param>
	/// <param name="dwCount"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION MemInt(ptrBuff AS PTR,i AS INT,dwCount AS DWORD) AS PTR
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero

	/// <summary>
	/// </summary>
	/// <param name="ptrMem"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION MemLen(ptrMem AS PTR) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Get a pointer to a matching long integer in a memory buffer.
	/// </summary>
	/// <param name="ptrBuff"></param>
	/// <param name="li"></param>
	/// <param name="dwCount"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION MemLong(ptrBuff AS PTR,li AS LONG,dwCount AS DWORD) AS PTR
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero

	/// <summary>
	/// Move one memory buffer to another. 
	/// </summary>
	/// <param name="ptrDest"></param>
	/// <param name="ptrSrc"></param>
	/// <param name="wCount"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION MemMove(ptrDest AS PTR,ptrSrc AS PTR,wCount AS DWORD) AS PTR
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero

	/// <summary>
	/// </summary>
	/// <param name="ptrMem"></param>
	/// <param name="cb"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION MemRealloc(ptrMem AS PTR,cb AS DWORD) AS PTR
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero

	/// <summary>
	/// </summary>
	/// <param name="ptrMem"></param>
	/// <param name="cb"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION MemReallocBlk(ptrMem AS PTR,cb AS DWORD) AS PTR
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero

	/// <summary>
	/// Fill a memory buffer with a specified character.
	/// </summary>
	/// <param name="ptrBuff"></param>
	/// <param name="b"></param>
	/// <param name="wCount"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION MemSet(ptrBuff AS PTR,b AS BYTE,wCount AS DWORD) AS PTR
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero

	/// <summary>
	/// Get a pointer to a matching short integer in a memory buffer.
	/// </summary>
	/// <param name="ptrBuff"></param>
	/// <param name="si"></param>
	/// <param name="dwCount"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION MemShort(ptrBuff AS PTR,si AS SHORT,dwCount AS DWORD) AS PTR
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero

	/// <summary>
	/// </summary>
	/// <param name="ptrBuff"></param>
	/// <param name="wCount"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION MemUpper(ptrBuff AS PTR,wCount AS DWORD) AS PTR
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero

	/// <summary>
	/// Get a pointer to a matching word in a memory buffer.
	/// </summary>
	/// <param name="ptrBuff"></param>
	/// <param name="w"></param>
	/// <param name="dwCount"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION MemWord(ptrBuff AS PTR,w AS WORD,dwCount AS DWORD) AS PTR
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero

	/// <summary>
	/// </summary>
	/// <param name="p"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION Ptr2Bin(p AS PTR) AS STRING
		/// THROW NotImplementedException{}
	RETURN String.Empty   

	/// <summary>
	/// Determine the number of bytes that can be read at a given pointer.
	/// </summary>
	/// <param name="ptrP"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION PtrLen(ptrP AS PTR) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Determine the number of bytes that can be written at a given pointer.
	/// </summary>
	/// <param name="ptrP"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION PtrLenWrite(ptrP AS PTR) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   


	/// <summary>
	/// Display a bitmap in a window or control.
	/// </summary>
	/// <param name="hWnd"></param>
	/// <param name="cFileName"></param>
	/// <param name="cTitle"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION ShowBitmap(hWnd AS PTR,cFileName AS STRING,cTitle AS STRING) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Display a bitmap stretched or shrunk to fit a window or control.
	/// </summary>
	/// <param name="hWnd"></param>
	/// <param name="cFileName"></param>
	/// <param name="cTitle"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION StretchBitmap(hWnd AS PTR,cFileName AS STRING,cTitle AS STRING) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <param name="pH"></param>
	/// <param name="dwCode"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION TerminateVOThread(pH AS PTR,dwCode AS DWORD) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Remove the registration of an object that was registered to the garbage collector.
	/// </summary>
	/// <param name="ptrKid"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION UnRegisterKid(ptrKid AS PTR) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <param name="pItem"></param>
	/// <param name="pvarg"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION Usual2Variant(pItem AS PTR,pvarg AS PTR) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <param name="pvarg"></param>
	/// <param name="pItem"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION Variant2Usual(pvarg AS PTR,pItem AS PTR) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Send the specified message to the specified window.
	/// </summary>
	/// <param name="hwnd"></param>
	/// <param name="nMsg"></param>
	/// <param name="dwParam"></param>
	/// <param name="lParam"></param>
	/// <returns>
	/// </returns>
	unsafe FUNCTION VOSendMessage(hwnd AS PTR,nMsg AS DWORD,dwParam AS DWORD,lParam AS LONG) AS LONG
		/// THROW NotImplementedException{}
	RETURN 0   

	#endregion
