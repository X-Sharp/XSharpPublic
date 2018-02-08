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
UNSAFE FUNCTION CreateVOThread(pSecAttr AS PTR,nStackSize AS DWORD,pFunc AS PTR,pParam AS PTR,dwFlags AS DWORD,pdwID REF DWORD ) AS PTR
		/// THROW NotImplementedException{}
	RETURN IntPtr.Zero
	
	/// <summary>
	/// </summary>
	/// <param name="hWndOwner"></param>
	/// <param name="cFiles"></param>
	/// <param name="fAsynchWork"></param>
	/// <returns>
	/// </returns>
UNSAFE	 FUNCTION DoSendMail(hWndOwner AS PTR,cFiles AS STRING,fAsynchWork AS LOGIC) AS VOID
		/// THROW NotImplementedException{}
	RETURN 
	
	/// <summary>
	/// Encode a file for e-mail transfer.
	/// </summary>
	/// <param name="hfIn"></param>
	/// <param name="hfOut"></param>
	/// <returns>
	/// </returns>
UNSAFE  FUNCTION EncodeBase64(hfIn AS PTR,hfOut AS PTR) AS INT
		/// THROW NotImplementedException{}
	RETURN 0   
	
	
	/// <summary>
	/// Functional form of BEGIN SEQUENCE.
	/// </summary>
	/// <param name="ptrLabel"></param>
	/// <returns>
	/// </returns>
UNSAFE FUNCTION LabelPush(ptrLabel AS PTR) AS VOID
		/// THROW NotImplementedException{}
	RETURN 
	
	
	/// </summary>
	/// <param name="p"></param>
	/// <returns>
	/// </returns>
UNSAFE	 FUNCTION Ptr2Bin(p AS PTR) AS STRING
		/// THROW NotImplementedException{}
	RETURN String.Empty   
	
	
	
	/// <summary>
	/// Display a bitmap in a window or control.
	/// </summary>
	/// <param name="hWnd"></param>
	/// <param name="cFileName"></param>
	/// <param name="cTitle"></param>
	/// <returns>
	/// </returns>
UNSAFE	 FUNCTION ShowBitmap(hWnd AS PTR,cFileName AS STRING,cTitle AS STRING) AS LOGIC
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
UNSAFE	 FUNCTION StretchBitmap(hWnd AS PTR,cFileName AS STRING,cTitle AS STRING) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   
	
	/// <summary>
	/// </summary>
	/// <param name="pH"></param>
	/// <param name="dwCode"></param>
	/// <returns>
	/// </returns>
UNSAFE	 FUNCTION TerminateVOThread(pH AS PTR,dwCode AS DWORD) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   
	
	/// <summary>
	/// Remove the registration of an object that was registered to the garbage collector.
	/// </summary>
	/// <param name="ptrKid"></param>
	/// <returns>
	/// </returns>
UNSAFE	 FUNCTION UnRegisterKid(ptrKid AS PTR) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   
	
	/// <summary>
	/// </summary>
	/// <param name="pItem"></param>
	/// <param name="pvarg"></param>
	/// <returns>
	/// </returns>
UNSAFE	 FUNCTION Usual2Variant(pItem AS PTR,pvarg AS PTR) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   
	
	/// <summary>
	/// </summary>
	/// <param name="pvarg"></param>
	/// <param name="pItem"></param>
	/// <returns>
	/// </returns>
UNSAFE	 FUNCTION Variant2Usual(pvarg AS PTR,pItem AS PTR) AS LOGIC
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
UNSAFE	 FUNCTION VOSendMessage(hwnd AS PTR,nMsg AS DWORD,dwParam AS DWORD,lParam AS LONG) AS LONG
		/// THROW NotImplementedException{}
	RETURN 0   
	
#endregion
