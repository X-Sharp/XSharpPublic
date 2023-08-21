/// <include file="Gui.xml" path="doc/ProgressBar/*" />
CLASS ProgressBar INHERIT Control
	PROTECT dwUnitSize		AS DWORD
	PROTECT dwPosition		AS DWORD
	PROTECT dwOldPosition	AS DWORD
	PROTECT oRange				AS Range


 /// <exclude />
ASSIGN __Value(nValue AS USUAL)  STRICT 
	//PP-030828 Strong typing
	
	


	IF IsString(nValue)
		nValue := Val(nValue)
	ENDIF


	SELF:Position := LONGINT(Round(nValue, 0))
	RETURN 


/// <include file="Gui.xml" path="doc/ProgressBar.Advance/*" />
METHOD Advance(dwNewPosition) 


	// Instruct the ProgressBar to update its position and save the old position
	IF IsNil(dwNewPosition)
		dwOldPosition := DWORD(SendMessage(hWnd, PBM_STEPIT, 0, 0))
		dwNewPosition := SELF:OldPosition + SELF:UnitSize
	ELSE
		dwOldPosition := DWORD(SendMessage(hWnd, PBM_DELTAPOS, dwNewPosition, 0))
	ENDIF
   // Read the position from the control
   SELF:dwPosition := DWORD(SendMessage(SELF:Handle(), PBM_GETPOS, 0, 0L))
	//RETURN dwPosition := dwNewPosition
	RETURN SELF:dwPosition


/// <include file="Gui.xml" path="doc/ProgressBar.BackgroundColor/*" />
ASSIGN BackgroundColor(oColor) 
	SendMessage(SELF:Handle(), PBM_SETBKCOLOR, 0, LONGINT(_CAST, oColor:ColorRef))
	RETURN 


/// <include file="Gui.xml" path="doc/ProgressBar.BarColor/*" />
ASSIGN BarColor(oColor) 
	SendMessage(SELF:Handle(), PBM_SETBARCOLOR, 0, LONGINT(_CAST, oColor:ColorRef))
	RETURN 


/// <include file="Gui.xml" path="doc/ProgressBar.ctor/*" />
CONSTRUCTOR(oOwner, xID, oPoint, oDimension, kStyle, lDataAware) 


	
	


	Default(@lDataAware, TRUE)
	IF IsInstanceOfUsual(xID, #ResourceID)
		SUPER(oOwner, xID, oPoint, oDimension, , kStyle, lDataAware)
	ELSE
		SUPER(oOwner, xID, oPoint, oDimension, PROGRESS_CLASS, kStyle, lDataAware)
	ENDIF


	// Set the default single-step size to 10 range to (0, 100). Since these are the default
	// values, we don't need to call the UnitSize() and Range() assigns, since that will send
	// a message to the control; we need only set the instance variable directly.
	dwUnitSize			:= 10
	oRange				:= Range{0, 100}
	SELF:Position		:= 0
	dwOldPosition		:= 0


	RETURN 


/// <include file="Gui.xml" path="doc/ProgressBar.OldPosition/*" />
ACCESS OldPosition 
	
	


	RETURN dwOldPosition


/// <include file="Gui.xml" path="doc/ProgressBar.Position/*" />
ACCESS Position 
	
	


	//RvdH 050602 Fixed issue 12973
	//RETURN dwPosition
	RETURN SendMessage(SELF:Handle(), PBM_GETPOS, 0, 0L)




/// <include file="Gui.xml" path="doc/ProgressBar.Position/*" />
ASSIGN Position(dwNewPosition) 
	
	


	// Instruct the ProgressBar to update its position and save the old position
	dwOldPosition := DWORD(SendMessage(hWnd, PBM_SETPOS, dwNewPosition, 0))


	RETURN dwPosition := dwNewPosition


/// <include file="Gui.xml" path="doc/ProgressBar.Range/*" />
ACCESS Range 
	//RvdH 050602 Changed to retrieve real range from Control
	LOCAL DIM aInt[2] AS LONGINT
	
	
	SendMessage(hWnd, PBM_GETRANGE, 1, LONGINT(_CAST, @aInt[1]))
	SELF:oRange := Range{aInt[1], aInt[2]}


	RETURN oRange


/// <include file="Gui.xml" path="doc/ProgressBar.Range/*" />
ASSIGN Range(oNewRange) 
	LOCAL oDllVers AS WinDLLVersion
	LOCAL nVers AS FLOAT
	
	


	//RvdH 050602 Handle large ranges when ComCtrl32 > 4.70
	IF oNewRange:Max > 0xFFFF
		oDllVers := WinDLLVersion{"COMCTL32"}
		nVers := oDLLVers:MajorVersion + oDLLVers:MinorVersion /100
		IF nVers > 4.70
			SendMessage(hWnd, PBM_SETRANGE32, oNewRange:Min, oNewRange:Max)
		ELSE
			SendMessage(hWnd, PBM_SETRANGE, 0, MAKELPARAM(oNewRange:Min, 0xFFFF))
			oNewRange := Range{oNewRange:Min, 0xFFFF}
		ENDIF
	ELSE
		SendMessage(hWnd, PBM_SETRANGE, 0, MAKELPARAM(oNewRange:Min, oNewRange:Max))
	ENDIF


	RETURN oRange := oNewRange


/// <include file="Gui.xml" path="doc/ProgressBar.UnitSize/*" />
ACCESS UnitSize 
	
	


	RETURN dwUnitSize


/// <include file="Gui.xml" path="doc/ProgressBar.UnitSize/*" />
ASSIGN UnitSize(dwNewUnitSize) 
	
	


	// Instruct the ProgressBar to set the single-step increment
	SendMessage(hWnd, PBM_SETSTEP, dwNewUnitSize, 0)


	RETURN dwUnitSize := dwNewUnitSize
END CLASS


