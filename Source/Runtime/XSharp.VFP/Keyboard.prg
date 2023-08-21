//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

// Functions provider by Karl-Heinz Rauscher

// make sure this is compiled early bound !
#pragma options ("lb", off)

USING System.Runtime.InteropServices
#ifdef DEBUG
INTERNAL GLOBAL gcToggleText AS STRING
#endif

INTERNAL PARTIAL CLASS Win32

	CONST INTERNAL VK_INSERT := 45 AS INT
	CONST INTERNAL VK_NUMLOCK := 144 AS INT
	CONST INTERNAL VK_CAPITAL := 20 AS INT

	[DllImport("user32.dll", SetLastError := TRUE)] ;
	INTERNAL STATIC METHOD SendInput(nInputs AS DWORD , pInputs AS winInputXS[], cbSize AS LONG ) AS DWORD

	[DllImport("user32.dll", SetLastError := TRUE)] ;
	INTERNAL STATIC METHOD GetKeyState(nVirtkey AS INT) AS SHORTINT

	[StructLayout(LayoutKind.Sequential)];
	INTERNAL STRUCTURE winKeyboardInputXS
		INTERNAL wVk AS WORD
		INTERNAL wScan AS WORD
		INTERNAL dwFlags AS DWORD
		INTERNAL time AS DWORD
		INTERNAL dwExtraInfo AS IntPtr
		INTERNAL unused1XS AS DWORD
		INTERNAL unused2XS AS DWORD
	END STRUCTURE


	[StructLayout(LayoutKind.Sequential)] ;
	INTERNAL STRUCTURE winInputXS
		INTERNAL Type AS DWORD
		INTERNAL Input AS winKeyboardInputXS
	END STRUCTURE

END CLASS

/// <include file="VFPDocs.xml" path="Runtimefunctions/numlock/*" />
FUNCTION NumLock() AS LOGIC STRICT
	 RETURN __IsKeyToggled( Win32.VK_NUMLOCK )

/// <include file="VFPDocs.xml" path="Runtimefunctions/numlock/*" />
FUNCTION NumLock ( lSetOn AS LOGIC ) AS LOGIC STRICT
VAR lPrevious := NumLock()

 	#ifdef DEBUG
		gcToggleText := ""
	#endif


	IF lPrevious != lSetOn

		#ifdef DEBUG
			gcToggleText := "Toggle VK_NUMLOCK to " + AsString ( lSetOn )
		#endif

		__ToggleKey ( Win32.VK_NUMLOCK )

	ENDIF

	RETURN lPrevious

/// <include file="VFPDocs.xml" path="Runtimefunctions/capslock/*" />
FUNCTION CapsLock() AS LOGIC STRICT

	 RETURN __IsKeyToggled( Win32.VK_CAPITAL )

/// <include file="VFPDocs.xml" path="Runtimefunctions/capslock/*" />
FUNCTION CapsLock ( lSetOn AS LOGIC ) AS LOGIC STRICT
VAR lPrevious := CapsLock()

 	#ifdef DEBUG
		gcToggleText := ""
	#endif


	IF lPrevious != lSetOn

		#ifdef DEBUG
			gcToggleText := "Toggle VK_CAPITAL to " + AsString ( lSetOn )
		#endif

		__ToggleKey ( Win32.VK_CAPITAL )


	ENDIF

	RETURN lPrevious

/// <include file="VFPDocs.xml" path="Runtimefunctions/insmode/*" />
FUNCTION InsMode() AS LOGIC STRICT
	 RETURN __IsKeyToggled( Win32.VK_INSERT )

/// <include file="VFPDocs.xml" path="Runtimefunctions/insmode/*" />
FUNCTION InsMode ( lSetOn AS LOGIC ) AS LOGIC STRICT
VAR lPrevious := InsMode()

 	#ifdef DEBUG
		gcToggleText := ""
	#endif


	IF lPrevious != lSetOn

	 	#ifdef DEBUG
			gcToggleText := "Toggle VK_INSERT to " + AsString ( lSetOn )
		#endif

		__ToggleKey ( Win32.VK_INSERT )

	ENDIF

	RETURN lPrevious

STATIC FUNCTION __ToggleKey ( nKey AS INT ) AS LOGIC
VAR ip := Win32.winInputXS[]{2}

	// Press the key

	ip[1].Type := 1  // 1 for keyboard
	ip[1].Input.wScan := 0
	ip[1].Input.time := 0
	ip[1].Input.dwExtraInfo := IntPtr.Zero
	ip[1].Input.wVk := (WORD) nKey
	ip[1].Input.dwFlags :=  0 // 0 for key press

	// Release the key

	ip[2].Type := 1 // 1 for keyboard
	ip[2].Input.wScan := 0
	ip[2].Input.time := 0
	ip[2].Input.dwExtraInfo := IntPtr.Zero
	ip[2].Input.wVk := (WORD) nKey
	ip[2].Input.dwFlags := 2 //  2 for key release

	RETURN Win32.SendInput( 2 , ip,  Marshal.SizeOf ( TypeOf ( Win32.winInputXS ) ) ) == 2


STATIC FUNCTION __IsKeyToggled( nKey AS INT ) AS LOGIC

 	#ifdef DEBUG
		gcToggleText := ""
	#endif

	RETURN _AND ( Win32.GetKeyState(nKey) , 0x01) == 1



