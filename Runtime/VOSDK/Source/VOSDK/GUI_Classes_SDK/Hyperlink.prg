PARTIAL CLASS CurHand INHERIT Pointer

CONSTRUCTOR () 
	SUPER(ResourceID{"CURHAND", _GetInst()})
	RETURN 

END CLASS

PARTIAL CLASS HyperLink INHERIT FixedText
	// Another generous contribution from S Ebert
	PROTECT _oPointer AS Pointer

METHOD Dispatch(oEvent) 
	LOCAL oEvt  AS @@Event
	LOCAL nMsg  AS DWORD
	LOCAL sRect IS _WinRect
	LOCAL hDc, hFont AS PTR

	oEvt := oEvent
	nMsg := oEvt:uMsg

	DO CASE
	CASE nMsg == WM_SETCURSOR .AND. LoWord(DWORD(_CAST,oEvt:lParam)) == HTClient
		SetCursor(_oPointer:Handle())
		SELF:EventReturnValue := 1l
		RETURN 1l
	CASE nMsg == WM_LBUTTONDOWN .OR. (nMsg == WM_KEYDOWN .AND. (oEvt:wParam == 32 .OR. oEvt:wParam == VK_RETURN))
		SELF:OpenLink()
		SELF:EventReturnValue := 0l
		RETURN 1l
	CASE nMsg == WM_PAINT
		IF SELF:lUseDrawText
			SUPER:Dispatch(oEvt)
		ELSE
			CallWindowProc(SELF:__lpfnDefaultProc, hWnd, nMsg, oEvt:wParam, oEvt:lParam)
		ENDIF
		IF hWnd == GetFocus()
			hdc   := GetDC(hwnd)
			hFont := SendMessage(hWnd, WM_GETFONT, 0, 0)
			IF hFont != NULL_PTR
				SelectObject(hdc, hFont)
				GetTextExtentPoint32(hDc, String2Psz(SELF:Caption), INT(SLen(SELF:Caption)), (_winSIZE PTR) @sRect:Right)
				sRect:Right  += 1
				sRect:Bottom += 1
				DrawFocusRect(hDc, @sRect)
			ENDIF
			ReleaseDC(hwnd, hdc)
		ENDIF
		SELF:EventReturnValue := 0l
		RETURN 1l
	CASE nMsg == WM_KILLFOCUS  .OR. nMsg == WM_SETFOCUS
		InvalidateRect(hWnd, NULL_PTR, FALSE)
	CASE nMsg == WM_GETDLGCODE
		IF HiWord(GetKeyState(VK_RETURN)) > 0
			SELF:EventReturnValue := DLGC_WANTALLKEYS
		ELSE
			SELF:EventReturnValue := IIF(hWnd == GetFocus(), DLGC_DEFPUSHBUTTON, DLGC_UNDEFPUSHBUTTON)
		ENDIF
		RETURN 1l
	ENDCASE

	RETURN SUPER:Dispatch(oEvt)

CONSTRUCTOR(oOwner, xID, oPoint, oDimension, cText) 
	SUPER(oOwner, xID, oPoint, oDimension, cText )
	SELF:Setstyle(SS_NOTIFY, TRUE)
	_oPointer := CurHand{}
	RETURN 

METHOD OpenLink() 
	ShellOpen(SELF:Owner, SELF:Caption)
	RETURN SELF
END CLASS

FUNCTION ShellOpen(oWindow AS Window, cFile AS STRING) AS VOID STRICT
    //SE-080603
   LOCAL hWnd AS PTR 

	cFile := AllTrim(Lower(cFile))
	IF At2("@",cFile)>1
		IF ! cFile = "mailto:"
			cFile := "mailto:" + cFile
		ENDIF
	ENDIF
	IF ! Empty(cFile)
      IF oWindow != NULL_OBJECT
          hWnd := oWindow:Handle()
      ELSE 
          hWnd := NULL_PTR //Desktop
      ENDIF 
      ShellExecute(hWnd, String2Psz("open"), String2Psz(cFile), NULL_PSZ, NULL_PSZ, SW_SHOWNORMAL)
	ENDIF
	RETURN

