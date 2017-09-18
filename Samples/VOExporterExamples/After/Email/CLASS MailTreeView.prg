CLASS MailTreeView INHERIT TreeView

// constructor inserted by xPorter, remove superfluous arguments
CONSTRUCTOR(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9) CLIPPER
SUPER(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9)
METHOD CustomDraw(lParam) 
    LOCAL pNMCustomDraw AS _winNMLVCUSTOMDRAW
	 LOCAL dwDrawStage   AS DWORD
	
	 pNMCustomDraw := PTR(_CAST, lParam)
	 dwDrawStage   := pNMCustomDraw.nmcd.dwDrawStage
	
	 IF dwDrawStage = CDDS_PREPAINT
		 RETURN CDRF_NOTIFYITEMDRAW
	 ELSEIF dwDrawStage = CDDS_ITEMPREPAINT
		 IF _And( pNMCustomDraw.nmcd.uItemState , CDIS_SELECTED ) > 0
			 pNMCustomDraw.clrText   := DWORD(RGB(255, 255, 255))
			 pNMCustomDraw.clrTextBk := DWORD(RGB(0, 0, 128))
		 ELSE
			 pNMCustomDraw.clrText   := DWORD(RGB(0, 0, 0))
			 pNMCustomDraw.clrTextBk := DWORD(RGB(190, 255, 255))
		 ENDIF
	 ENDIF

	 RETURN CDRF_DODEFAULT


END CLASS
