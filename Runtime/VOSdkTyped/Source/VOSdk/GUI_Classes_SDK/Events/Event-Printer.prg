// Event_Printer.prg
#using System.Diagnostics

CLASS PrinterErrorEvent INHERIT @@Event
	ACCESS ErrorType AS DWORD STRICT 
		RETURN wParam

	[DebuggerStepThrough];
	CONSTRUCTOR(_hWnd AS IntPtr, _uMsg AS DWORD, _wParam AS DWORD, _lParam AS LONG, _oWindow := NULL_OBJECT AS Window) 
		SUPER(_hWnd , _uMsg , _wParam , _lParam , _oWindow )
		RETURN 
END CLASS

CLASS PrinterExposeEvent INHERIT @@Event
	ACCESS ExposedArea AS OBJECT STRICT 
		RETURN oWindow

	[DebuggerStepThrough];
	CONSTRUCTOR(_hWnd AS IntPtr, _uMsg AS DWORD, _wParam AS DWORD, _lParam AS LONG, _oWindow := NULL_OBJECT AS Window) 
		SUPER(_hWnd , _uMsg , _wParam , _lParam , _oWindow )
		RETURN 

	ACCESS PageNo AS DWORD STRICT 
		RETURN wParam

END CLASS
