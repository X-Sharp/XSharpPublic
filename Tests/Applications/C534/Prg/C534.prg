// error XS0119: '_CommonOLEUIInsertObjectDlgProc' is a type, which is not valid in the given context
// taken from RP2 code
DELEGATE _CommonOLEUIInsertObjectDlgProc(hWnd AS PTR,uMsg AS DWORD,wParam AS DWORD,lParam AS LONG) AS LONGINT
FUNCTION _CommonOLEUIInsertObjectDlgProc(hWnd AS PTR,uMsg AS DWORD,wParam AS DWORD,lParam AS LONG) AS LONGINT
	? hWnd,uMsg,wParam,lParam
RETURN 0


PUBLIC GLOBAL delCommonOLEUIInsertObjectDlg	AS _CommonOLEUIInsertObjectDlgProc
FUNCTION Start() AS VOID
	delCommonOLEUIInsertObjectDlg	:= _CommonOLEUIInsertObjectDlgProc{ NULL, @_CommonOLEUIInsertObjectDlgProc() }
	delCommonOLEUIInsertObjectDlg:Invoke(NULL_PTR , 1 ,2 , 3)
RETURN
