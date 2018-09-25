// 626. Parser error when not specifying calling convention in _DLL FUNC
// error XS9002: Parser: unexpected input 'INT:'
/*
VO accepts the syntax without the calling convention specified (and I guess uses PASCAL by default?)
Found this in user code, so it's probably not too rare.
X# should either allow it, too, or report a better error explaining what's missing,
because otherwise it is difficult for the users to find out
*/

_DLL FUNCTION MessageBox(hwnd AS IntPtr, lpText AS STRING, lpCaption AS STRING, uType AS DWORD);
	AS INT:USER32.MessageBoxW // error
//	AS INT PASCAL:USER32.MessageBoxW // ok

FUNCTION Start() AS VOID
	MessageBox(IntPtr.Zero , "sucks :)", "Verstappen", 0)
RETURN
