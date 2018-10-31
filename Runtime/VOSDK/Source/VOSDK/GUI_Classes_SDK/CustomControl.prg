PARTIAL CLASS CustomControl INHERIT Control

CONSTRUCTOR(oOwner, xID, oPoint, oDimension, kStyle, lDataAware) 

	SUPER(oOwner, xID, oPoint, oDimension, __WCCustomControlClass, kStyle, lDataAware)

	RETURN 

END CLASS

PROCEDURE __WCRegisterCustomControl _INIT3
	LOCAL wc IS _WINWNDclass
	wc:style := _OR(CS_DBLCLKS, CS_GLOBALCLASS)
#ifdef __VULCAN__
   LOCAL hDll := LoadLibraryW( "user32.dll" ) AS PTR
	wc:lpfnWndProc 	:= GetProcAddress( hDll, String2Psz( "DefWindowProcA" ) )
	FreeLibrary( hDll )
#else	
	wc:lpfnWndProc := PTR(_CAST, @DefWindowProc())
#endif	
	wc:hInstance := _GetInst()
	wc:hIcon := NULL_PTR
	wc:hCursor := LoadCursor(0, IDC_ARROW)
	wc:hbrBackground := (COLOR_BTNFACE + 1)
	wc:lpszClassName := String2Psz(__WCCustomControlClass)
	wc:cbWndExtra := 0
	wc:cbClsExtra := 0

	RegisterClass(@wc)

	RETURN



#region defines
DEFINE __WCCustomControlClass := "_VOCustomControl"
#endregion
