CLASS __DDImp INHERIT DialogWindow

METHOD Default() 
	//PP-040410
	SUPER:Default()
	oParent:EventReturnValue := SELF:EventReturnValue
	RETURN SELF

METHOD Dispatch(oEvent) 
	//PP-040410
    LOCAL liReturn AS LONG 

    //  oEvent:oWindow := oParent
    //  oParent:Dispatch(oEvent)
    //  RETURN SELF:EventReturnValue := oParent:EventReturnValue
    oEvent:oWindow := oParent
    liReturn := oParent:Dispatch(oEvent)
    SELF:EventReturnValue := oParent:EventReturnValue 
    RETURN liReturn

CONSTRUCTOR(oOwner, lModal, lChild) 
   //SE-070906 Supports dialog styles in parameter lChild 
   //for resizable DataDialogs
    LOCAL sTemplate AS _winDLGTEMPLATE
    LOCAL hgbl      AS PTR

    hgbl      := GlobalAlloc(GMEM_ZEROINIT, _SIZEOF(_winDLGTEMPLATE) + 48)
    sTemplate := GlobalLock(hgbl)  

    IF IsLogic(lChild) .AND. lChild
       sTemplate:style := _OR(WS_CHILD,DS_3DLOOK,WS_VISIBLE) 
    ELSE 
       sTemplate:style := _OR(DS_3DLOOK,WS_POPUP,WS_CAPTION,WS_SYSMENU,WS_DLGFRAME,WS_CLIPCHILDREN)
       IF IsLong(lChild)
          sTemplate:style := _OR(sTemplate:style, (LONGINT) lChild)
       ENDIF   
    ENDIF
    sTemplate:cx := 1
    sTemplate:cy := 1   

    GlobalUnlock(hgbl)
    
    SUPER(oOwner, hgbl, lModal) 
    GlobalFree(hgbl)

    SELF:SetExStyle(WS_EX_CONTROLPARENT, FALSE)

    oParent:__Imp := SELF
    oParent:SetHandle(hWnd)

	RETURN 

METHOD Show(kShowState) 
	//LOCAL hParent := GetParent(hwnd) AS PTR
	//local liX, liY as long
	//local r is _winRECT

	

	Default(@kShowState, SHOWCENTERED)

	/* 2.0a-1
	 if (hParent != 0)
	 GetWindowRect(hParent, @r)
	 liX := r.left + ((r.right - r.left) / 2) - (self:Size:Width / 2)
	 liY := r.top + ((r.bottom - r.top) / 2) - (self:Size:Height / 2)
	 SetWindowPos(hwnd, NULL_PTR, liX, liY, 0, 0, _OR(SWP_NOZORDER, SWP_NOSIZE))
	endif */

	SUPER:Show(kShowState)

	RETURN SELF
END CLASS

