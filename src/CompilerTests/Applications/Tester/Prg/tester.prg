// Core dialect

FUNCTION Start() AS VOID
RETURN

function XSGetProcessIdThread( nThreadId as dword ) as array
	local aWindows			as array
	local oGCHandle			as System.Runtime.InteropServices.GCHandle
	local oPtr 				as IntPtr
	local oDelegate			as EnumWindowsProc_delegate
	// static local oEnumWindowsProcDelegate := EnumWindowsProc as EnumWindowsProc_Delegate

	aWindows		:= {}
	oDelegate		:= EnumWindowsProcProc
	oPtr 		:= System.Runtime.InteropServices.Marshal.GetFunctionPointerForDelegate( oDelegate )
	oGcHandle := NULL // default( System.Runtime.InteropServices.GCHandle)
	EnumThreadWindows( nThreadId, oPtr, System.Runtime.InteropServices.GCHandle.ToIntPtr( oGCHandle ) )
	oGCHandle:Free()

	return aWindows

function EnumWindowsProcProc(hwnd as ptr, lParam as IntPtr) as logic
	local aWindows 	as array
	local gch 		as System.Runtime.InteropServices.GCHandle
	local nProcId	as dword
	local pProcId   as dword Ptr

	nProcId		:= 0
	pProcId     := @nProcId
	gch 		:= System.Runtime.InteropServices.GCHandle.FromIntPtr(LParam)
	aWindows 	:= (array) gch:Target
	GetWindowThreadProcessID( hWnd, pProcId )
	AAdd( aWindows, { hwnd, nProcId } )

	return true

delegate EnumWindowsProc_delegate( hWnd as ptr, LParam as IntPtr ) as logic
