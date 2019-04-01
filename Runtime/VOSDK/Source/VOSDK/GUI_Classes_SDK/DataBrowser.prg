#ifdef __VULCAN__
   #using System.Runtime.InteropServices
#endif

CLASS DataBrowser INHERIT Control
	PROTECT iBufferGranularity AS INT
	PROTECT iBufferMaximum AS INT
	PROTECT iFTHeight AS INT
	PROTECT iSelectionStyle AS INT
	PROTECT iBufferSize AS INT
	PROTECT iDeferPaintCount AS INT
	PROTECT iDeferNotifyCount AS INT
	PROTECT iRecordSize AS INT
	PROTECT nOldRecordNum AS INT

	PROTECT dwDeferredStyle AS DWORD

	PROTECT lCaptionSet AS LOGIC
	PROTECT lColumnTitles AS LOGIC
	PROTECT lIsReadOnly AS LOGIC
	PROTECT lIsShown AS LOGIC
	PROTECT lHasTop AS LOGIC
	PROTECT lHasBottom AS LOGIC
	PROTECT lUseDefCaption AS LOGIC
	PROTECT lUseDefColCaption AS LOGIC
	PROTECT lUseDefText AS LOGIC
	PROTECT lUseDefButton AS LOGIC
	PROTECT lUseDefColButton AS LOGIC
	PROTECT lUseDefHiText AS LOGIC
	PROTECT lUse3dLook AS LOGIC
	PROTECT lLinked AS LOGIC

	PROTECT aColumn AS ARRAY

	PROTECT hNotifyWindow AS PTR

	PROTECT ptrSelf AS PTR
	EXPORT ptrControlDefaultProc AS PTR
	PROTECT strucEditField AS PTR //_WinFieldInfo
	PROTECT strucEditRecord AS PTR //_WinRecordCore
	PROTECT strucFocusField AS PTR //_WinFieldInfo

	PROTECT oDataServer AS DataServer
	PROTECT oEditFont AS Font
	PROTECT oFontText AS Font
	PROTECT oBackgroundColCaption AS Brush
	PROTECT oBackgroundHiText AS Brush
	PROTECT oBackgroundText AS Brush
	PROTECT oBackgroundCaption AS Brush
	PROTECT oBackgroundButton AS Brush
	PROTECT oBackgroundColButton AS Brush
	PROTECT oTextColor AS Color
	PROTECT oTextPointer AS Pointer
	PROTECT oCellEdit AS Control
	PROTECT dwLastChar AS DWORD
	PROTECT ptrDataBuffer AS BYTE PTR
	
	#ifdef __VULCAN__
	HIDDEN CellEditProcDelegate AS __CellEditProcDelegate
	HIDDEN WCGBChildProcDelegate AS __WCGBChildProcDelegate
	#endif

	//PP-030828 Strong typing
	METHOD __AddColumn (oDataColumn AS DataColumn, iCol AS INT) AS LOGIC STRICT 
	//PP-030828 Strong typing
	// iCol : 1-based index in aColumn, 0 means append to tail
	LOCAL strucFI AS _WinFieldInfo
	LOCAL cCaption AS STRING
	LOCAL lRC AS LOGIC
	LOCAL oDC AS DataColumn
	LOCAL oTempCol AS DataColumn
	LOCAL oFont AS Font
	LOCAL oTempBrush AS Brush
	LOCAL oTempColor AS Color

	//PP-040410
	IF oDataColumn == NULL_OBJECT
		WCError{#AddColumn,#DataBrowser,__WCSTypeError,oDataColumn,1}:@@Throw()
	ENDIF

	oDC := oDataColumn
	IF (oDC:Owner != NULL_OBJECT) //if DataColumn already assigned to Browser
		RETURN FALSE
	ENDIF

	strucFI := oDC:__FieldInfo
	PCALL(gpfnCntFldDataAlnSet, hWnd, strucFI, DWORD(oDC:__HorzAlignment))

	//PP-041004
	IF strucFI:cxWidth == DWORD(_CAST,-1)
		strucFI:cxWidth := 16
	ELSEIF (oDataColumn:FieldSpec != NULL_OBJECT) .AND. (oDataColumn:FieldSpec:ValType == "O")
		strucFI:cxWidth := 32
	ENDIF

	cCaption := oDC:GetCaption()   
	IF (NULL_STRING != cCaption)
		PCALL(gpfnCntAttribSet, hWnd, CA_FLDTTL3D)
		IF lColumnTitles
			IF (iFTHeight < INT(strucFI:wFTitleLines))
				iFTHeight := INT(strucFI:wFTitleLines)
				PCALL(gpfnCntFldTtlHtSet, hWnd, iFTHeight)
			ENDIF
		ENDIF
		IF !lCaptionSet
			lCaptionSet := TRUE
			PCALL(gpfnCntFldTtlSepSet, hWnd)
		ENDIF
        PCALL(gpfnCntFldTtlSet, hWnd, strucFI, String2Psz(cCaption), SLen(cCaption)+1)
	ENDIF

	// Set the Visual attributes of the column
	// Background Color of Title, Text, and Button
	oTempBrush := oDC:__TtlBkgdBrsh
	IF oTempBrush != NULL_OBJECT
		oDC:__SetFldColor(SELF, oDC:__TtlBkgdLoc, __WCGetBrushColor(oTempBrush))
	ENDIF
	oTempBrush := oDC:__TxtBkgdBrsh
	IF oTempBrush != NULL_OBJECT
		oDC:__SetFldColor(SELF, oDC:__TxtBkgdLoc, __WCGetBrushColor(oTempBrush))
	ENDIF
	oTempBrush := oDC:__BtnBkgdBrsh
	IF oTempBrush != NULL_OBJECT
		oDC:__SetFldColor(SELF, oDC:__BtnBkgdLoc, __WCGetBrushColor(oTempBrush))
	ENDIF

	//Foreground Color of Title, Text, and Button
	oTempColor := oDC:__TtlClr
	IF (oTempColor != NULL_OBJECT)
		oDC:__SetFldColor(SELF, oDC:__TtlClrLoc, oTempColor:ColorRef)
	ENDIF
	oTempColor := oDC:__TxtClr
	IF (oTempColor != NULL_OBJECT)
		oDC:__SetFldColor(SELF, oDC:__TxtClrLoc, oTempColor:ColorRef)
	ENDIF
	oTempColor:=oDC:__BtnClr
	IF (oTempColor != NULL_OBJECT)
		oDC:__SetFldColor(SELF, oDC:__BtnClrLoc, oTempColor:ColorRef)
	ENDIF

	// Text Font
	oFont := oDC:__Font
	IF (oFont != NULL_OBJECT)
		PCALL(gpfnCntFontSet, hWnd, oFont:Handle(), DWORD(oDC:__FontLoc))
	ENDIF

	// if data server is present - write out and delete internal buffers
	IF (oDataServer != NULL_OBJECT)
		SELF:__ClearBuffers()
	ENDIF

	// Build Column descriptor
	// oDC:__Type := strucFI.wColType
	// oDC:__Size := strucFI.wDataBytes
	// oDC:__Offset := strucFI.wOffStruct

	oDC:__Owner := SELF

	IF (iCol == 0)
		lRC := PCALL(gpfnCntAddFldTail, hWnd, strucFI)
	ELSE
		oTempCol := SELF:GetColumn(iCol)
		IF (oTempCol == NULL_OBJECT)
			lRC := PCALL(gpfnCntAddFldTail, hWnd, strucFI)
		ELSE
			// 	strucFIHead := CntFldHeadGet(hWnd)
			// 	strucFITemp := oTempCol:__FieldInfo
			// 	if (ptr(_cast, strucFIHead) == oTempCol:__FieldInfo)
			// 		CntAddFldHead(hwnd, strucFI)
			// 	else
			lRC := PCALL(gpfnCntInsFldBefore, hWnd, oTempCol:__FieldInfo, strucFI)
			// endif
		ENDIF
	ENDIF

	IF (lRC)
		// Insert/Add Column to array
		IF (iCol != 0)
			ASize(aColumn, ALen(aColumn) + 1)
			AIns(aColumn, DWORD(iCol))
			aColumn[iCol] := oDC
		ELSE
			AAdd(aColumn, oDC)
		ENDIF
	ENDIF

	RETURN lRC

METHOD __AutoLayout() AS VOID STRICT 
    //SE-081212 optimized
    //PP-030828 Strong typing
    LOCAL iFields, iStart, iBegin, iEnd AS INT
    LOCAL iStep AS INT
    LOCAL aNewColumns AS ARRAY
    LOCAL oDataField AS DataField
    LOCAL oNewColumn AS DataColumn
    LOCAL oPropFS AS FieldSpec
    LOCAL oPropHL AS HyperLabel
    LOCAL oWindow AS Window  
    LOCAL oDatawin  AS DataWindow

    aNewColumns:={}

    IF lLinked
        iFields := oDataServer:FCount

        IF IsBiDi()
            iBegin := iFields
            iEnd := 1
            iStep := -1
        ELSE
            iBegin := 1
            iEnd := iFields
            iStep := 1
        ENDIF

        IF IsInstanceOf(oParent, #__FormFrame) 
           oWindow := oParent:Owner
        ENDIF           
        
        IF IsInstanceOf(oWindow, #DataWindow) //vor die Loop gesetzt, da sich das Ergebnis in der Loop nicht ändert
           oDatawin := OBJECT(oWindow)
        ELSE
           oDatawin := NULL_OBJECT
        ENDIF  
        FOR iStart := iBegin TO iEnd STEP iStep
            oDataField := oDataServer:DataField(iStart)

            IF oDataField == NULL_OBJECT
                LOOP
            ENDIF

            // If there are explicit properties in the DataWindow view
            // propagate them, else use the DataField Properties
            IF oDatawin == NULL_OBJECT
                oPropHL := oDataField:HyperLabel
                oPropFS := oDataField:FieldSpec
            ELSE           
                IF (oPropHL := oDatawin:__FindHyperLabel(oDataField:NameSym)) == NULL_OBJECT
                    oPropHL := oDataField:HyperLabel
                ENDIF
                IF (oPropFS := oDatawin:__FindFieldSpec(oDataField:NameSym)) == NULL_OBJECT
                    oPropFS := oDataField:FieldSpec
                ENDIF
            ENDIF
            oNewColumn := DataColumn{oPropFS, oPropHL} 
            oNewColumn:Caption := __GetDFCaption(oDataField,{})
            oNewColumn:LinkDF(oDataServer, iStart)
            AAdd(aNewColumns,oNewColumn)
        NEXT
        IF ALen(aNewColumns)     > 0
            SELF:AddColumn(aNewColumns)
        ENDIF

    ENDIF

    RETURN

METHOD __AutoResize() AS VOID STRICT 
    IF SELF:ValidateControl()
        WCMoveWindow(SELF, Point{0,0}, oParent:CanvasArea:Size, TRUE)
    ENDIF

    RETURN

METHOD __BeginEditField(hWin AS PTR, dwChar AS DWORD) AS VOID STRICT 
	//PP-030828 Strong typing
	LOCAL dw AS DWORD
	LOCAL oPoint AS Point
	LOCAL oDim AS Dimension
	LOCAL oCol AS DataColumn
	LOCAL oEdit AS OBJECT
	LOCAL hControl AS PTR
	LOCAL cType := "C" AS STRING
	LOCAL oDBStg AS USUAL

	
	// Save pointers to the focus cell.
	strucEditField := PTR(_CAST, PCALL(gpfnCntFocusFldGet, hWnd))
	StrucEditRecord := PTR(_CAST, PCALL(gpfnCntFocusRecGet, hWnd))

	SELF:__EndEditField(0)
	SELF:__RefreshData()

	IF (strucEditRecord != NULL_PTR) .AND. (strucEditField != NULL_PTR)
		dw := PCALL(gpfnCntFocusExtGet, hWin)
		oDim := Dimension{LoWord(dw), HiWord(dw)}

		dw := PCALL(gpfnCntFocusOrgGet, hWin, FALSE)
		oPoint := __WCConvertPoint(SELF,Point{LoWord(dw),HiWord(dw)})

		IF WCGetCoordinateSystem() // Cartesian Coordinate System
			oPoint:Y := oPoint:Y - oDim:Height
		ENDIF

		oCol := SELF:__GetColumn(strucEditField)
		IF (oCol:FieldSpec != NULL_OBJECT)
			cType := oCol:FieldSpec:ValType
		ENDIF

		IF (cType == "O")
			IF !IsInstanceOf(oCellEdit, #OleObject)
#ifdef USE_OLEOBJECT			
				oEdit := OleObject{ oParent, 101, Point{0,0}, Dimension{0,0}, TRUE}
#else
            oEdit := NULL_OBJECT
#endif				
				IF oEdit != NULL_OBJECT
					oCellEdit := oEdit
					oEdit:AllowInPlace :=  FALSE
					oDBStg := oDataServer:FIELDGET(oCol:NameSym)
					IF IsInstanceOfUsual(oDBStg, #OLEDBStorage)
						oEdit:CreateFromDBStorage(oDBStg)
						oEdit:Activate()
						oEdit:SetHostNames( ResourceString{__WCSHostVOApp}:AsString(),;
							ResourceString{__WCSHostDataBrowser}:AsString())
					ENDIF
				ENDIF
			ELSE
				oCellEdit:Activate()
			ENDIF
		ELSE
			oEdit := oCol:GetEditObject(SELF, 101, oPoint, oDim)
			IF IsInstanceOf(oEdit,#Control)
				oCellEdit := oEdit
				hControl := oCellEdit:Handle()
				oCellEdit:Show()
				oCellEdit:SetFocus()
				IF (hControl != NULL_PTR)
					ptrControlDefaultProc := GetWindowLong(hControl, GWL_WNDPROC)
#ifdef __VULCAN__
               IF CellEditProcDelegate == NULL
                  CellEditProcDelegate := __CellEditProcDelegate{ NULL, @__CellEditProc() }
               ENDIF
               SetWindowLong( hControl, GWL_WNDPROC, (INT) System.Runtime.InteropServices.Marshal.GetFunctionPointerForDelegate( (System.Delegate) CellEditProcDelegate ) )
#else               					
					SetWindowLong(hControl, GWL_WNDPROC, LONGINT(_CAST, @__CellEditProc()))
#endif					
					IF (dwChar != 0)
						//PP-040317 Issue 12644 SendMessage changed to PostMessage
						PostMessage(hControl, EM_SETSEL, 0, -1)
						PostMessage(hControl, WM_CHAR, dwChar, 0L) //lParam) //Riz is lParam valid?
						//SendMessage(hControl, EM_SETSEL, 0, 0)
					ENDIF
					PCALL(gpfnCntFocusFldLock, hWnd)
				ENDIF
				WCAppSetDialogWindow(NULL_PTR)
			ENDIF
		ENDIF
	ELSE
		strucEditField := NULL_PTR //make sure both structures are cleared
		StrucEditRecord := NULL_PTR
	ENDIF

	RETURN

METHOD __BuildBuffer() AS VOID STRICT 
	//PP-030828 Strong typing
	LOCAL oOldPointer AS Pointer
	LOCAL strucRecord AS _WINRecordCore
	LOCAL iRecNo AS INT

	

	IF oDataServer != NULL_OBJECT
		oOldPointer := oParent:Pointer
		oParent:Pointer:=Pointer{PointerHourGlass}

		SELF:SuspendUpdate()
		SELF:__DeferNotify()

		SELF:__ClearBuffers()

		IF !SELF:__IsDataServerEmpty()
			iRecNo := oDataServer:RecNo

			SELF:__BuildNewBuffer(iRecNo)

			strucRecord := SELF:__GetRecordAtRecNo(iRecNo)

			IF strucRecord != NULL_PTR
				PCALL(gpfnCntFocusSet, hWnd, strucRecord, PCALL(gpfnCntFocusFldGet, hwnd))
				PCALL(gpfnCntFocusRecLock, hWnd)
				IF !SELF:__IsFocusRecordInView()
					PCALL(gpfnCntTopRecSet, hWnd, strucRecord)
				ENDIF
			ELSE
				PCALL(gpfnCntFocusSet, hWnd, NULL_PTR, NULL_PTR)
			ENDIF

			oDataServer:GoTo(iRecNo)
		ENDIF

		SELF:__EnableNotify()
		SELF:RestoreUpdate()

		oParent:Pointer:=oOldPointer
	ENDIF

	RETURN

METHOD __BuildNewBuffer(iRecNo AS INT) AS VOID STRICT 
	//PP-030828 Strong typing
	LOCAL strucRecord AS _WINRecordCore
	LOCAL strucTopRec AS _WINRecordCore
	LOCAL iDeltaPos, iCurrentPos, iRangeMax, iToBottom AS INT

	

	//PP-041004
	strucTopRec := NULL_PTR

	IF oDataServer != NULL_OBJECT
		SELF:SuspendUpdate()
		SELF:__DeferNotify()

		oDataServer:GoTo(iRecNo)

		lHasTop := FALSE
		lHasBottom := FALSE
		lHasTop := SELF:__TestForTop()

		iBufferSize := 0

		DO WHILE (iBufferSize <= iBufferGranularity)
			IF oDataServer:EOF
				EXIT
			ENDIF

			SELF:__RefreshData()

			strucRecord := SELF:__BuildRecord()
			IF (strucRecord == NULL_PTR)
				EXIT
			ENDIF

			PCALL(gpfnCntAddRecTail, hWnd, strucRecord)
			iBufferSize := iBufferSize + 1

			IF (strucTopRec == NULL_PTR)
				strucTopRec := strucRecord
			ENDIF

			oDataServer:Skip(1)
		ENDDO

		lHasBottom := SELF:__TestForBottom()

		IF (iBufferSize <= iBufferGranularity)
			iToBottom := iBufferSize

			oDataServer:GoTo(iRecNo)
			oDataServer:Skip(-1)

			DO WHILE (iBufferSize <= iBufferGranularity)
				IF oDataServer:Bof
					EXIT
				ENDIF

				SELF:__RefreshData()
				strucRecord := SELF:__BuildRecord()
				IF (strucRecord == NULL_PTR)
					EXIT
				ENDIF

				PCALL(gpfnCntAddRecHead, hWnd, strucRecord)
				iBufferSize := iBufferSize + 1
				strucTopRec := strucRecord
				oDataServer:Skip(-1)
			ENDDO

			lHasTop := SELF:__TestForTop()
		ENDIF

		IF (lHasTop .AND. lHasBottom)
			PCALL(gpfnCntDeltaExSet, hWnd, 0)
			PCALL(gpfnCntRangeExSet, hWnd, 0, iBufferSize)
		ELSE
			iRangeMax := iBufferSize
			IF (!lHasTop .AND. !lHasBottom)
				// middle
				iRangeMax := iBufferSize + (2 * iBufferGranularity)
				iDeltaPos := iRangeMax - iBufferSize - iBufferGranularity
				iCurrentPos := iDeltaPos
			ELSEIF lHasTop
				// top
				iRangeMax := iBufferSize + iBufferGranularity
				iDeltaPos := 0
				iCurrentPos := iDeltaPos
			ELSE
				// bottom
				iRangeMax := iBufferSize + iBufferGranularity
				iDeltaPos := iRangeMax - iBufferSize
				// this will force the bottom
				// iCurrentPos := (iRangeMax - CntRecsDispGet(hWnd)) + 1 // wrong !?
				iCurrentPos := iRangeMax - iToBottom
			ENDIF
/*         DebOut("strucTopRec",strucTopRec)
         DebOut("strucTopRec.lpRecData",strucTopRec.lpRecData)
         DebOut("strucTopRec.lpNext",strucTopRec.lpNext)
         DebOut("strucTopRec.lpPrev",strucTopRec.lpPrev)
         DebOut("strucTopRec.lpSelFldHead",strucTopRec.lpSelFldHead)
         DebOut("strucTopRec.lpSelFldTail",strucTopRec.lpSelFldTail)         
         DebOut("strucTopRec.dwRecSize",strucTopRec.dwRecSize)         
         DebOut("IbufferSize",iBufferSize)
			DebOut("iDeltaPos",iDeltaPos)
			DebOut("iCurrentPos",iCurrentPos)
			DebOut("iRangeMax",iRangeMax) 
			DebOut("gpfnCntTopRecSet",gpfnCntTopRecSet)
*/			PCALL(gpfnCntTopRecSet, hWnd, strucTopRec)
			PCALL(gpfnCntDeltaExSet, hWnd, iBufferSize)
			PCALL(gpfnCntDeltaPosExSet, hWnd, iDeltaPos)
			PCALL(gpfnCntVScrollPosExSet, hWnd, iCurrentPos)
			PCALL(gpfnCntRangeExSet, hWnd, 0, iRangeMax)
		ENDIF

		SELF:__EnableNotify()
		SELF:RestoreUpdate()
	ENDIF

	RETURN

METHOD __BuildRecord() AS _WinRecordCore STRICT 
	//PP-030828 Strong typing
	LOCAL strucRecCore AS _WinRecordCore
	LOCAL i AS INT

	

	IF (oDataServer != NULL_OBJECT)
		strucRecCore := PCALL(gpfnCntNewRecCore, DWORD(iRecordSize))
		IF (strucRecCore != NULL_PTR)
			i := oDataServer:RecNo
			PCALL(gpfnCntRecUserSet, strucRecCore, @i, _SIZEOF(INT))
			SELF:__FillRecord(strucRecCore)
			IF lIsReadOnly
				PCALL(gpfnCntRecAttrSet, strucRecCore, CRA_RECREADONLY)
			ENDIF
		ENDIF
	ENDIF

	RETURN strucRecCore

METHOD __BuildRecordDescription() AS LOGIC STRICT 
	//PP-030828 Strong typing
	LOCAL i AS DWORD
	LOCAL iLen AS DWORD
	LOCAL oColumn AS DataColumn
	LOCAL strucFI AS _WinFieldInfo
	LOCAL dwOldRecSize AS DWORD
	LOCAL dwRecordSize 	AS DWORD

	IF ptrDataBuffer != NULL_PTR
		MemFree(ptrDataBuffer)
		ptrDataBuffer := NULL_PTR
	ENDIF

	dwOldRecSize := DWORD(iRecordSize)
	dwRecordSize := _SIZEOF(INT) 
	iLen :=  ALen(aColumn)
	FOR i:=1 UPTO iLen
		oColumn := aColumn[i]
		strucFI := oColumn:__FieldInfo
		strucFI:wOffStruct := dwRecordSize
		dwRecordSize += oColumn:__Size + 1
	NEXT

	ptrDataBuffer := MemAlloc(dwRecordSize)
	iRecordSize 	:= INT(dwRecordSize)
	// return true if we need to refresh browser
	RETURN (dwRecordSize > dwOldRecSize)

METHOD __ClearBuffers() AS VOID STRICT 
	//PP-030828 Strong typing
	

	IF oCellEdit != NULL_OBJECT
		SELF:__EndEditField(0)
	ENDIF

	PCALL(gpfnCntKillRecList, hWnd)
	PCALL(gpfnCntDeltaExSet, hWnd, 0)
	PCALL(gpfnCntRangeExSet, hWnd, 0, 0)

	lHasTop := FALSE
	lHasBottom := FALSE
	iBufferSize := 0

	RETURN

ACCESS __ContainerChildWnd AS PTR STRICT 
	//PP-030828 Strong typing
	LOCAL hChild AS PTR

	hChild := PCALL(gpfnCntCNChildWndGet, hwnd, 0)
	IF (hChild == NULL_PTR)
		hChild := GetWindow(hwnd, GW_CHILD)
	ENDIF

	RETURN hChild

METHOD __DeferNotify() AS VOID STRICT 
	//PP-030828 Strong typing
	

	IF iDeferNotifyCount==0 .AND. oDataServer != NULL_OBJECT
		oDataServer:SuspendNotification()
	ENDIF
	iDeferNotifyCount := iDeferNotifyCount + 1

	RETURN

METHOD __DeltaBuildBuffer() AS VOID STRICT 
	//PP-030828 Strong typing
	LOCAL oOldPointer AS Pointer
	LOCAL iCurrentRecNo AS INT
	LOCAL iInc AS INT
	LOCAL strucRC AS _WinRecordCore

	

	IF oDataServer != NULL_OBJECT
		oOldPointer:=oParent:Pointer
		oParent:Pointer := Pointer{PointerHourGlass}
		SELF:SuspendUpdate()
		SELF:__DeferNotify()

		iCurrentRecNo := oDataServer:RecNo
		iInc := PCALL(gpfnCntCNIncExGet, hWnd, 0)

		IF iBufferSize + iBufferGranularity <= iBufferMaximum
			IF !lHasTop .AND. !lHasBottom
				// middle
				IF iInc < 0
					SELF:__DeltaBuildBufferUp()
				ELSE
					SELF:__DeltaBuildBufferDown()
				ENDIF
			ELSEIF lHasTop
				// top
				IF iInc > 0
					SELF:__DeltaBuildBufferDown()
				ENDIF
			ELSE
				// bottom
				IF iInc < 0
					SELF:__DeltaBuildBufferUp()
				ENDIF
			ENDIF
		ELSE
			strucRC := SELF:__GetRecordAtRecNo(iCurrentRecNo)
			IF !lHasTop .AND. !lHasBottom
				// middle
				IF iInc < 0
					SELF:__DeltaReBuildBufferUp()
				ELSE
					SELF:__DeltaReBuildBufferDown()
				ENDIF
			ELSEIF lHasTop
				// top
				IF iInc > 0
					SELF:__DeltaReBuildBufferDown()
				ENDIF
			ELSE
				// bottom
				IF iInc < 0
					SELF:__DeltaReBuildBufferUp()
				ENDIF
			ENDIF

			IF strucRC == NULL_PTR
				// see if focus was paged in
				strucRC := SELF:__GetRecordAtRecNo(iCurrentRecNo)

				IF strucRC != NULL_PTR
					PCALL(gpfnCntSelectRec, hWnd, strucRC)
				ENDIF
			ENDIF
		ENDIF

		oDataServer:GoTo(iCurrentRecNo)

		SELF:__EnableNotify()
		SELF:RestoreUpdate()

		oParent:Pointer := oOldPointer
	ENDIF

	RETURN

METHOD __DeltaBuildBufferDown() AS VOID STRICT 
	//PP-030828 Strong typing
	LOCAL i AS INT
	LOCAL iRangeMax AS INT
	LOCAL strucRC AS _WinRecordCore

	

	SELF:SuspendUpdate()
	SELF:__DeferNotify()

	oDataServer:GoTo(SELF:__GetRecordNo(PCALL(gpfnCntRecTailGet, hWnd)))
	oDataServer:Skip(1)
	// 2.0a-1, changed borders
	FOR i:=0 UPTO iBufferGranularity-1
		IF oDataServer:EOF
			EXIT
		ENDIF
		SELF:__RefreshData()
		IF (strucRC := SELF:__BuildRecord()) == NULL_PTR
			EXIT
		ENDIF
		PCALL(gpfnCntAddRecTail, hWnd, strucRC)
		oDataServer:Skip(1)
	NEXT

	lHasBottom := SELF:__TestForBottom()
	iBufferSize += i
	iRangeMax := iBufferSize

	IF lHasTop .AND. lHasBottom
		PCALL(gpfnCntDeltaExSet, hWnd, 0)
		PCALL(gpfnCntRangeExSet, hWnd, 0, iRangeMax)
	ELSE
		IF !lHasTop
			iRangeMax += iBufferGranularity
		ENDIF
		IF !lHasBottom
			iRangeMax += iBufferGranularity
		ENDIF

		PCALL(gpfnCntRangeExSet, hWnd, 0, iRangeMax)
		PCALL(gpfnCntDeltaExSet, hWnd, iBufferSize)
	ENDIF

	SELF:__EnableNotify()
	SELF:RestoreUpdate()

	RETURN

METHOD __DeltaBuildBufferUp() AS VOID STRICT 
	//PP-030828 Strong typing
	LOCAL iRecNo AS INT
	LOCAL i AS INT
	LOCAL iRangeMax AS INT
	LOCAL strucRC AS _WinRecordCore

	

	SELF:SuspendUpdate()
	SELF:__DeferNotify()

	iRecNo := SELF:__GetRecordNo(PCALL(gpfnCntRecHeadGet, hWnd))
	oDataServer:GoTo(iRecNo)
	oDataServer:Skip(-1)

	// 2.0a-1, changed borders
	FOR i:=0 UPTO iBufferGranularity-1
		IF oDataServer:BOF
			EXIT
		ENDIF
		SELF:__RefreshData()
		IF (strucRC := SELF:__BuildRecord()) == NULL_PTR
			EXIT
		ENDIF
		PCALL(gpfnCntAddRecHead, hWnd, strucRC)
		oDataServer:Skip(-1)
	NEXT

	lHasTop := SELF:__TestForTop()
	iBufferSize += i
	iRangeMax := iBufferSize

	IF (lHasTop .AND. lHasBottom)
		PCALL(gpfnCntDeltaExSet, hWnd, 0)
		PCALL(gpfnCntDeltaPosExSet, hWnd, 0)
		PCALL(gpfnCntVScrollPosExSet, hWnd, i)
		PCALL(gpfnCntRangeExSet, hWnd, 0, iRangeMax)
	ELSE
		IF !lHasTop
			iRangeMax += iBufferGranularity
		ENDIF
		IF !lHasBottom
			iRangeMax += iBufferGranularity
		ENDIF

		IF !lHasTop
			i := PCALL(gpfnCntDeltaPosExGet, hWnd)

			PCALL(gpfnCntDeltaExSet, hWnd, iBufferSize)
			PCALL(gpfnCntVScrollPosExSet, hWnd, i + iBufferGranularity)
			PCALL(gpfnCntRangeExSet, hWnd, 0, iRangeMax)
		ELSE
			// 2.5b
			// i := pcall(gpfnCntCurrentPosExGet, hWnd)

			PCALL(gpfnCntDeltaExSet, hWnd, iBufferSize)
			PCALL(gpfnCntDeltaPosExSet, hWnd, 0)
			PCALL(gpfnCntVScrollPosExSet, hWnd, i)
			PCALL(gpfnCntRangeExSet, hWnd, 0L, iRangeMax)
		ENDIF
	ENDIF

	SELF:__EnableNotify()
	SELF:RestoreUpdate()

	RETURN

METHOD __DeltaRebuildBufferDown() AS VOID STRICT 
	//PP-030828 Strong typing
	LOCAL i AS INT
	LOCAL iShift AS INT
	LOCAL iRangeMax AS INT
	LOCAL iCurrentPos AS INT
	LOCAL lLostTop AS LOGIC
	LOCAL strucRC AS _WinRecordCore
	LOCAL strucTopRec AS _WinRecordCore

	

	SELF:SuspendUpdate()
	SELF:__DeferNotify()

	strucTopRec := PCALL(gpfnCntTopRecGet, hwnd)

	oDataServer:GoTo(SELF:__GetRecordNo(PCALL(gpfnCntRecTailGet, hwnd)))
	oDataServer:Skip(1)

	// 2.0a-1, changed borders
	FOR i:=0 UPTO iBufferGranularity-1
		IF oDataServer:Eof
			EXIT
		ENDIF
		SELF:__RefreshData()
		strucRC := SELF:__BuildRecord()
		IF (strucRC == NULL_PTR)
			EXIT
		ENDIF
		PCALL(gpfnCntAddRecTail, hwnd, strucRC)
		oDataServer:Skip(1)
	NEXT

	lHasBottom := SELF:__TestForBottom()
	iShift := i

	// 2.0a-1, changed borders
	FOR i:=0 UPTO iShift-1
		strucRC := PCALL(gpfnCntRemoveRecHead, hwnd)
		IF (strucRC == NULL_PTR)
			EXIT
		ENDIF
		PCALL(gpfnCntFreeRecCore, strucRC)
	NEXT

	// this assumes that records were removed and top is lost
	lLostTop := lHasTop
	lHasTop := FALSE

	iRangeMax := iBufferSize
	IF !lHasTop
		iRangeMax += iBufferGranularity
	ENDIF
	IF !lHasBottom
		iRangeMax += iBufferGranularity
	ENDIF

	IF lLostTop
		iCurrentPos := PCALL(gpfnCntCurrentPosExGet, hwnd)

		PCALL(gpfnCntTopRecSet, hwnd, strucTopRec)
		PCALL(gpfnCntDeltaExSet, hwnd, iBufferSize)
		PCALL(gpfnCntDeltaPosExSet, hwnd, ibufferGranularity)
		PCALL(gpfnCntVScrollPosExSet, hwnd, iCurrentPos)
		PCALL(gpfnCntRangeExSet, hwnd, 0, iRangeMax)
	ELSE
		iCurrentPos := PCALL(gpfnCntCurrentPosExGet, hwnd)

		PCALL(gpfnCntTopRecSet, hwnd, strucTopRec)
		PCALL(gpfnCntDeltaExSet, hwnd, iBufferSize)
		PCALL(gpfnCntDeltaPosExSet, hwnd, iBufferGranularity)
		PCALL(gpfnCntVScrollPosExSet, hwnd, iCurrentPos - iShift)
		PCALL(gpfnCntRangeExSet, hwnd, 0, iRangeMax)
	ENDIF

	SELF:__EnableNotify()
	SELF:RestoreUpdate()

	RETURN

METHOD __DeltaRebuildBufferUp() AS VOID STRICT 
	//PP-030828 Strong typing
	LOCAL i AS INT
	LOCAL iShift AS INT
	LOCAL iRangeMax AS INT
	LOCAL iCurrentPos AS INT
	LOCAL lGainTop AS LOGIC
	LOCAL strucTopRec AS _WinRecordCore
	LOCAL strucRC AS _WinRecordCore

	

	SELF:SuspendUpdate()
	SELF:__DeferNotify()

	strucTopRec := PCALL(gpfnCntTopRecGet, hwnd)

	oDataServer:GoTo(SELF:__GetRecordNo(PCALL(gpfnCntRecHeadGet, hwnd)))
	oDataServer:Skip(-1)

	// 2.0a-1, changed borders
	FOR i:=0 UPTO iBufferGranularity-1
		IF oDataServer:Bof
			EXIT
		ENDIF
		SELF:__RefreshData()
		IF (strucRC := SELF:__BuildRecord()) == NULL_PTR
			EXIT
		ENDIF
		PCALL(gpfnCntAddRecHead, hwnd, strucRC)
		oDataServer:Skip(-1)
	NEXT

	lGainTop := lHasTop := SELF:__TestForTop()
	iShift := i

	// 2.0a-1, changed borders
	FOR i:=0 UPTO iShift-1
		IF (strucRC := PCALL(gpfnCntRemoveRecTail, hwnd)) == NULL_PTR
			EXIT
		ENDIF
		PCALL(gpfnCntFreeRecCore, strucRC)
	NEXT

	// this assumes that records were removed and bottom is lost
	lHasBottom := FALSE
	iRangeMax := iBufferSize

	IF !lHasTop
		iRangeMax += iBufferGranularity
	ENDIF
	IF !lHasBottom
		iRangeMax += iBufferGranularity
	ENDIF

	IF lGainTop
		PCALL(gpfnCntTopRecSet, hwnd, strucTopRec)
		PCALL(gpfnCntDeltaExSet, hwnd, iBufferSize)
		PCALL(gpfnCntDeltaPosExSet, hwnd, 0)
		PCALL(gpfnCntVScrollPosExSet, hwnd, iShift)
		PCALL(gpfnCntRangeExSet, hwnd, 0, iRangeMax)
	ELSE
		icurrentPos := PCALL(gpfnCntCurrentPosExGet, hwnd)

		PCALL(gpfnCntTopRecSet, hwnd, strucTopRec)
		PCALL(gpfnCntDeltaExSet, hwnd, iBufferSize)
		PCALL(gpfnCntDeltaPosExSet, hwnd, iBufferGranularity)
		PCALL(gpfnCntVScrollPosExSet, hwnd, iCurrentPos + iShift)
		PCALL(gpfnCntRangeExSet, hwnd, 0, iRangeMax)
	ENDIF

	SELF:__EnableNotify()
	SELF:RestoreUpdate()

	RETURN

METHOD __DestroyEditField() AS VOID STRICT 
	//PP-030828 Strong typing
	

	IF (oCellEdit != NULL_OBJECT)
		oCellEdit:Destroy()
		oCellEdit := NULL_OBJECT
		ptrControlDefaultProc := NULL_PTR
	ENDIF

	RETURN

METHOD __EditDispatch(uMsg AS DWORD, wParam AS DWORD, lParam AS LONGINT) AS LOGIC STRICT 
	//PP-030828 Strong typing
	

	IF uMsg==WM_KeyDown
		IF wParam==VK_TAB .OR.;
			wParam==VK_ESCAPE .OR.;
			wParam==VK_RETURN .OR.;
			wParam==VK_UP .OR.;
			wParam==VK_DOWN .OR.;
			wParam==VK_PRIOR .OR.;
			wParam==VK_NEXT

			PCALL(gpfnCntNotifyAssoc, hwnd, DWORD(_CAST, CN_ENDFLDEDIT), wParam, StrucEditRecord, StrucEditField,;
				_AND(LONGINT(lParam), 0x0000ffff), (GetAsyncKeyState(VK_SHIFT)<0), (GetAsyncKeyState(VK_CONTROL)<0), NULL_PTR)
			RETURN TRUE
		ENDIF
	ELSEIF uMsg==WM_Char
		IF wParam==VK_TAB .OR.;
			wParam==VK_ESCAPE .OR.;
			wParam==VK_RETURN
			RETURN TRUE
		ENDIF
	ENDIF

	RETURN FALSE

METHOD __EnableNotify() AS VOID STRICT 
	//PP-030828 Strong typing
	

	IF iDeferNotifyCount>0
		--iDeferNotifyCount
	ENDIF

	IF iDeferNotifyCount==0 .AND. oDataServer!=NULL_OBJECT
		oDataServer:ResetNotification()
	ENDIF

	RETURN

METHOD __EnableSelection(kStyle AS INT) AS VOID STRICT 
	//PP-030828 Strong typing
	LOCAL strucRC AS _WinRecordCore

	// if no change do nothing
	

	IF iSelectionStyle == kStyle
		RETURN
	ENDIF

	// Turn off old mode
	DO CASE
	CASE iSelectionStyle == ssSingleSelection
		// Turn off single selections if any
		strucRC := PCALL(gpfnCntSelRecGet, hwnd)
		IF strucRC != NULL_PTR
			PCALL(gpfnCntUnSelectRec, hwnd, strucRC)
		ENDIF
		PCALL(gpfnCntStyleClear, hwnd, CTS_SINGLESEL)

	CASE iSelectionStyle == ssExtendedSelection
		// Turn off multiple selections if any
		strucRC := PCALL(gpfnCntRecHeadGet, hwnd)
		WHILE strucRC != NULL_PTR
			IF PCALL(gpfnCntIsRecSelected, hwnd, strucRC)
				PCALL(gpfnCntUnSelectRec, hwnd, strucRC)
			ENDIF
			strucRC := PCALL(gpfnCntNextRec, strucRC)
		ENDDO
		PCALL(gpfnCntStyleClear, hwnd, CTS_EXTENDEDSEL)

	CASE iSelectionStyle == ssBlockSelection
		strucRC := PCALL(gpfnCntRecHeadGet, hwnd)
		WHILE strucRC != NULL_PTR
			IF PCALL(gpfnCntIsRecSelected, hwnd, strucRC)
				PCALL(gpfnCntUnSelectRec, hwnd, strucRC)
			ENDIF
			PCALL(gpfnCntRecAttrClear, strucRC, CRA_SELECTED)
			PCALL(gpfnCntRecAttrClear, strucRC, CRA_FLDSELECTED)
			strucRC := PCALL(gpfnCntNextRec, strucRC)
		ENDDO
		PCALL(gpfnCntStyleClear, hwnd, CTS_BLOCKSEL)
	ENDCASE

	DO CASE
		//case kStyle==ssNoSelection
		//Do nothing
	CASE kStyle==ssSingleSelection
		PCALL(gpfnCntStyleSet, hwnd, CTS_SINGLESEL)

	CASE kStyle==ssExtendedSelection
		PCALL(gpfnCntStyleSet, hwnd, CTS_EXTENDEDSEL)

	CASE kStyle==ssBlockSelection
		PCALL(gpfnCntStyleSet, hwnd, CTS_BLOCKSEL)
	ENDCASE

	iSelectionStyle := kStyle

	RETURN

METHOD __EndEditField(dwChar AS DWORD) AS VOID STRICT 
	//PP-030828 Strong typing
	LOCAL oDCol AS DataColumn
	LOCAL c AS STRING
	//	local strucEF AS _WinFieldInfo
	//	local strucER AS _WinRecordCore

	

	IF (oCellEdit != NULL_OBJECT)
		oDCol := SELF:__GetColumn(strucEditField)

		// If the record numbers do not match then the dataServer has
		// moved (changed?). Therefore lose the edit!
		IF (dwChar != VK_ESCAPE) .AND. !IsInstanceOf(oCellEdit, #OLEObject) .AND.;
			SELF:__GetRecordNo(strucEditRecord) == oDataServer:RECNO .AND.;
			oCellEdit:Modified

			//			strucER:=strucEditRecord
			//			ptrFldBuf := strucER.lpRecData
			//			strucEF:=strucEditField
			//			ptrFldBuf := PTR(_CAST,DWORD(_CAST,ptrFldBuf)+strucEF.wOffStruct)

			oCellEdit:__Update()
			c := oCellEdit:TextValue
			//must destroy the edit cell before calling SetValue to avoid looping
			SELF:__DestroyEditField()
			oDCol:SetValue(c)
			SELF:ColumnFocusChange(oDCol, TRUE)
		ELSE
			SELF:__DestroyEditField()
		ENDIF

		PCALL(gpfnCntFocusFldUnlck, hwnd)
	ENDIF

	RETURN

METHOD __FieldChange() AS VOID STRICT 
	//PP-030828 Strong typing
	LOCAL iRecNo AS INT
	LOCAL strucRecord AS _WinRecordCore

	
	IF oDataServer != NULL_OBJECT
		iRecNo := oDataServer:RecNo
		strucRecord := SELF:__GetRecordAtRecNo(iRecNo)
		IF strucRecord != NULL_PTR
			SELF:SuspendUpdate()
			SELF:__DeferNotify()

			SELF:__FillRecord(strucRecord)
			PCALL(gpfnCntEndRecEdit, hwnd, strucRecord, PCALL(gpfnCntFocusFldGet, hwnd))

			SELF:__EnableNotify()
			SELF:RestoreUpdate()
		ENDIF
	ENDIF

	RETURN

METHOD __FillRecord(RecCore AS _WINRecordCore) AS VOID STRICT 
	//PP-030828 Strong typing
	LOCAL ptrDest AS BYTE PTR
	LOCAL cValue AS STRING
	LOCAL iLen AS INT
	LOCAL i AS INT
	LOCAL dwOff AS DWORD
	LOCAL dwSize AS DWORD
	LOCAL dwLen AS DWORD
	LOCAL strucRecCore AS _winRECORDCORE
	LOCAL oColumn AS DataColumn

	strucRecCore := RecCore

	MemSet(ptrDataBuffer, 0, DWORD(iRecordSize))

	iLen := INT(ALen(aColumn))

	FOR i:=1 UPTO iLen  
		oColumn	:= aColumn[i]
		cValue 	:= oColumn:GetValue()
		dwOff 	:= oColumn:__Offset
		ptrDest	:= ptrDataBuffer + dwOff
		dwSize 	:= oColumn:__Size // Width // wrong ??

		IF (NULL_STRING != cValue)
			dwLen := SLen(cValue)

			IF (dwLen < dwSize)
				dwSize := dwLen
			ENDIF

			MemCopy(ptrDest, String2Psz(cValue), dwSize)
		ENDIF

		_NPut(ptrDest, dwSize, 0)

		//ASSERT dword(_cast, (ptrDest+dwsize)) <= dword(_cast, (ptrDataBuffer+iRecordSize))
	NEXT

	PCALL(gpfnCntRecDataSet, strucRecCore, ptrDataBuffer)

	RETURN

METHOD __FindColumn(nIndex AS USUAL) AS DWORD STRICT 
	//PP-030828 Strong typing
	// returns Index into array
	LOCAL sIndex AS SYMBOL
	LOCAL dwType AS DWORD
	LOCAL dwI, dwCount AS DWORD
	LOCAL oDCol AS DataColumn

	dwType := UsualType(nIndex)
	DO CASE
	CASE dwType == LONGINT
		IF nIndex > 0 .AND. nIndex <= ALen(aColumn)
			RETURN nIndex
		ENDIF
		RETURN 0
	   //SE-060526
	CASE dwType == SYMBOL .OR. dwType == STRING
		sIndex := IIF(dwType == SYMBOL, nIndex, String2Symbol(nIndex))
		dwCount := ALen(aColumn)
		FOR dwI := 1 UPTO dwCount
			oDCol := aColumn[dwI]
			IF oDCol:NameSym == sIndex
				RETURN dwI
			ENDIF
		NEXT //dwI
		RETURN 0
	   //SE-060526
	CASE IsInstanceOfUsual(nIndex, #DataColumn)
		oDCol := nIndex
		dwCount := ALen(aColumn)
		FOR dwI := 1 UPTO dwCount
			IF oDCol = aColumn[dwI]
				RETURN dwI
			ENDIF
		NEXT //dwI
		RETURN 0

	CASE (dwType == VOID)
		RETURN 0

	OTHERWISE
		WCError{#__FindColumn,#DataBrowser,__WCSTypeError,nIndex,1}:@@Throw()
	ENDCASE

	RETURN 0

METHOD __GetColumn(strucFI AS _WINFieldInfo) AS OBJECT STRICT 
	//PP-030828 Strong typing
	LOCAL p AS SelfPtr
	LOCAL strucFITemp AS _WinFieldInfo
	LOCAL oRet := NULL_OBJECT AS USUAL

	strucFITemp := strucFI

	IF (strucFITemp != NULL_PTR)
	   // Note lpUserData contains a pointer to a buffer of 4 bytes with the self pointer!
		p := strucFITemp:lpUserData // pCall(gpfnCntFldUserGet(strucFI)
		p := p:ptrSelf
		oRet := __WCSelfPtr2Object(p)
	ENDIF

	RETURN oRet

METHOD __GetRecordAtRecNo(iRecNo AS INT) AS _WINRecordCore STRICT 
	//PP-030828 Strong typing
	LOCAL strucRecord AS _WINRecordCore

	

	strucRecord := PCALL(gpfnCntRecHeadGet, hwnd)
	DO WHILE strucRecord != NULL_PTR
		IF SELF:__GetRecordNo(strucRecord) == iRecNo
			EXIT
		ENDIF
		strucRecord := PCALL(gpfnCntNextRec, strucRecord)
	ENDDO

	RETURN strucRecord

METHOD __GetRecordNo(strucRC AS _winRECORDCORE) AS LONGINT STRICT 
	//PP-030828 Strong typing
	LOCAL p AS LONGINT PTR
	LOCAL strucRCTemp AS _WinRecordCore

	

	strucRCTemp := strucRC
	IF (strucRCTemp == NULL_PTR)
		RETURN 0
	ENDIF

	p := strucRCTemp:lpUserData //pCall(gpfnCntRecUserGet(strucRC)
	IF (p == NULL_PTR)
		RETURN 0
	ENDIF

	RETURN LONGINT(p)

METHOD __IsDataServerEmpty() AS LOGIC STRICT 
	//PP-030828 Strong typing
	

	RETURN oDataServer:BOF .AND. oDataServer:EOF

METHOD __IsFocusRecordInView() AS LOGIC STRICT 
	//PP-030828 Strong typing
	LOCAL strucFocusRecord AS _WinRecordCore
	LOCAL strucRecord AS _WinRecordCore
	LOCAL iDisplaySize AS INT

	

	IF (strucFocusRecord := PCALL(gpfnCntFocusRecGet, hwnd)) == NULL_PTR
		RETURN FALSE
	ENDIF

	iDisplaySize := PCALL(gpfnCntRecsDispGet, hwnd)
	strucRecord := PCALL(gpfnCntTopRecGet, hwnd)
	DO WHILE iDisplaySize > 1
		IF strucRecord == NULL_PTR
			EXIT
		ELSEIF strucRecord == strucFocusRecord
			EXIT
		ENDIF
		strucRecord := PCALL(gpfnCntNextRec, strucRecord)
		iDisplaySize--
	ENDDO

	RETURN (strucRecord == strucFocusRecord)

ASSIGN __LastChar(dwNewLastChar AS DWORD)  STRICT 
	//PP-030828 Strong typing
	

	RETURN (dwLastChar := dwNewLastChar)

METHOD __NewFocusField(strucFI AS _WINFieldInfo) AS VOID STRICT 
	//PP-030828 Strong typing
	

	IF strucFocusField != NULL_PTR
		SELF:ColumnFocusChange(SELF:__GetColumn(strucFocusField), FALSE)
	ENDIF

	IF StrucFI != NULL_PTR
		SELF:ColumnFocusChange(SELF:__GetColumn(StrucFI), TRUE)
	ENDIF

	strucFocusField := strucFI

	RETURN

METHOD __NewFocusRecord(strucRC AS _WINRecordCore, strucFI AS _WINFieldInfo) AS VOID STRICT 
	//PP-030828 Strong typing
	LOCAL iRecNo AS INT
	LOCAL strucOldFI AS _WinFieldInfo

	

	iRecNo := SELF:__GetRecordNo(strucRC)

	IF oDataServer:RecNo == 0
		oDataServer:Update()
		oDataServer:GoTo(iRecNo - 1)
	ELSE
		iDeferNotifyCount := iDeferNotifyCount + 1
		// self:__DeferNotify()
		IF oDataServer:GoTo(iRecNo)
			strucOldFI := PCALL(gpfnCntFocusFldGet, hwnd)

			PCALL(gpfnCntFocusSet, hwnd, strucRC, strucFI)
			PCALL(gpfnCntFocusRecLock, hwnd)

			IF strucOldFI != strucFI
				SELF:__NewFocusField(strucFI)
			ENDIF
		ELSE
			PCALL(gpfnCntFocusRecLock, hwnd)
		ENDIF
		iDeferNotifyCount := iDeferNotifyCount - 1
		// self:__EnableNotify()
	ENDIF

	RETURN

METHOD __NotifyChanges(kNotify AS DWORD) AS USUAL STRICT 
	//PP-030828 Strong typing
	

	IF iDeferNotifyCount == 0
		// if in the middle of an edit - end it
		IF (oCellEdit != NULL_OBJECT)
			SELF:__EndEditField(0)
		ENDIF

		SWITCH kNotify
		CASE GBNFY_INTENTTOMOVE
			RETURN SELF:Validate()
		CASE GBNFY_RECORDCHANGE
			SELF:__RecordChange()
		CASE GBNFY_DOGOTOP 
		CASE GBNFY_DOGOEND
			SELF:__RecordChange()
			SELF:__RefreshBuffer()
		CASE GBNFY_FIELDCHANGE
			SELF:__FieldChange()
		CASE GBNFY_FILECHANGE
			SELF:__RefreshBuffer()
		CASE GBNFY_DONEWROW
			SELF:__RefreshBuffer()
		CASE GBNFY_DODELETE
			SELF:__RecordDelete()
		END SWITCH
	ELSE
		IF kNotify == GBNFY_INTENTTOMOVE
			RETURN TRUE
		ELSE
			RETURN NIL
		ENDIF
	ENDIF
	RETURN NIL

ACCESS __NotifyWindow AS PTR STRICT 
	//PP-030828 Strong typing
	

	RETURN hNotifyWindow

METHOD __RecordChange() AS VOID STRICT 
	//PP-030828 Strong typing
	LOCAL iRecNo AS INT
	LOCAL iCount AS INT
	LOCAL strucRecord AS _WinRecordCore
	//local i as int

	

	IF oDataServer != NULL_OBJECT
		iRecNo := oDataServer:RecNo

		// check if record is already focused
		strucRecord := PCALL(gpfnCntFocusRecGet, hwnd)
		IF strucRecord != NULL_PTR .AND.;
			SELF:__GetRecordNo(strucRecord) == iRecNo
			// make sure that it is selected
			PCALL(gpfnCntSelectRec, hwnd, strucRecord)
		ELSE
			// check if record is in the buffer
			strucRecord := PCALL(gpfnCntRecHeadGet, hwnd)
			DO WHILE strucRecord != NULL_PTR
				IF SELF:__GetRecordNo(strucRecord) == iRecNo
					EXIT
				ENDIF
				strucRecord := PCALL(gpfnCntNextRec, strucRecord)
				iCount++
			ENDDO

			// record is in the buffer so focus it
			IF strucRecord != NULL_PTR
				PCALL(gpfnCntFocusSet, hwnd, strucRecord, PCALL(gpfnCntFocusFldGet, hwnd))
				PCALL(gpfnCntFocusRecLock, hwnd)

				IF !SELF:__IsFocusRecordInView()
					IF !lHasTop
						//i := pCall(gpfnCntDeltaPosExGet, hwnd)
						iCount += iBufferGranularity //iBufferGranularity
					ENDIF
					IF !lHasBottom
						PCALL(gpfnCntCurrentPosExSet, hwnd, iCount)
					ELSE
						// !not clean! need better solution
						SELF:__RefreshBuffer()
					ENDIF
				ENDIF
			ELSE
				SELF:__RefreshBuffer()
			ENDIF
		ENDIF
	ENDIF

	RETURN

METHOD __RecordDelete() AS VOID STRICT 
	//PP-030828 Strong typing
	LOCAL strucRecord AS _WinRecordCore
	LOCAL iRecno AS INT
	LOCAL iTopRecno AS INT

	

	iRecNo := oDataServer:RecNo
	SELF:SuspendUpdate()
	SELF:__DeferNotify()

	strucRecord := PCALL(gpfnCntTopRecGet, hwnd)
	IF strucRecord != NULL_PTR
		oDataServer:Skip(1)
		IF oDataServer:EOF
			oDataServer:Skip(-1)
		ENDIF
	ELSE
		iTopRecNo := SELF:__GetRecordNo(strucRecord)
		IF iTopRecNo != iRecNo
			oDataServer:GoTo(iTopRecNo)
		ELSE
			oDataServer:Skip(1)
			IF oDataServer:EOF
				oDataServer:Skip(-1)
			ENDIF
		ENDIF
	ENDIF

	SELF:__ClearBuffers()
	IF !SELF:__IsDataServerEmpty()
		SELF:__BuildNewBuffer(oDataServer:RecNo)
	ENDIF

	oDataServer:GoTo(iRecNo)
	SELF:__EnableNotify()
	SELF:RestoreUpdate()

	RETURN

METHOD __RefreshBuffer() AS VOID STRICT 
	//PP-030828 Strong typing
	

	SELF:SuspendUpdate()
	SELF:__BuildBuffer()
	SELF:RestoreUpdate()

	RETURN

METHOD __RefreshData() AS VOID STRICT 
	//PP-030828 Strong typing
	

	ASend(aColumn, #__Scatter)

	RETURN

METHOD __RefreshField(uFieldName AS USUAL) AS DWORD 
	//PP-030828 Strong typing
	LOCAL i, iLen AS DWORD
	LOCAL symFieldName AS SYMBOL
	LOCAL oDF AS DataField
	LOCAL oCol AS DataColumn
	IF IsSymbol(uFieldName)
		symFieldName:=uFieldName
		iLen := ALen(aColumn)
		FOR i := 1 TO iLen    
			oCol :=aColumn[i] 
			oDF := oCol:__DataField
			IF (oDF != NULL_OBJECT) .AND. (oDF:NameSym == symFieldName)
				oCol:__Scatter()
			ENDIF
		NEXT
	ENDIF

	RETURN i

METHOD __RegisterFieldLinks(oDS AS DataServer) AS LOGIC STRICT 
	//PP-030828 Strong typing
	LOCAL oDC AS DataColumn
	LOCAL i, iDF, iColumns AS INT

	// Link in columns depending on two conditions.
	// If we already have columns registered with the browser then assume
	// auto layout is not desired.

	// If no columns have been registered with the form then we
	// assume auto layout is desired.

	

	iColumns := INT(Len(aColumn))
	IF (iColumns > 0)
		FOR i:=1 UPTO iColumns
			oDC := aColumn[i]
			iDF := oDS:FieldPos(oDC:NameSym)
			IF iDF>0 .AND. IsNil(oDC:Server) // Only one datafield per column
				oDC:LinkDF(oDS, iDF) // Exit here, only one column per
				lLinked := TRUE
			ENDIF
		NEXT
	ELSE
		// We need to do an auto layout for the form
		lLinked := .T. 
		SELF:__AutoLayout()
	ENDIF

	// Register the form as a client of the Server
	IF lLinked
		oDS:RegisterClient(SELF)
	ENDIF

	RETURN lLinked

METHOD __SetMaxFTHeight(iLines AS INT) AS VOID STRICT 
	//PP-030828 Strong typing
	

	IF lColumnTitles
		IF iFTHeight < ilines
			iFTHeight := ilines
		ENDIF
	ELSE
		iFTHeight := 0
	ENDIF
	PCALL(gpfnCntFldTtlHtSet, hwnd, iFTHeight)

	RETURN

METHOD __StatusOK() AS DataColumn STRICT 
	//PP-030828 Strong typing
	//SE-060526
	LOCAL oDCInvalidColumn AS DataColumn
	LOCAL dwI, dwCount AS DWORD

	

	dwCount := ALen(aColumn)
	FOR dwI := 1 UPTO dwCount
		oDCInvalidColumn := aColumn[dwI]
		IF oDCInvalidColumn:Status != NIL
			RETURN oDCInvalidColumn
		ENDIF
	NEXT //dwI

   oDCInvalidColumn:= NULL_OBJECT

	RETURN oDCInvalidColumn

METHOD __TestForBottom() AS LOGIC STRICT 
	//PP-030828 Strong typing
	LOCAL lRetCode AS LOGIC
	LOCAL iRecNo AS INT

	

	IF oDataServer:EOF
		RETURN TRUE
	ENDIF

	iRecNo := oDataServer:RecNo
	oDataServer:Skip(1)
	lRetCode := oDataServer:EOF
	oDataServer:GoTo(iRecNo)

	RETURN lRetCode

METHOD __TestForTop() AS LOGIC STRICT 
	//PP-030828 Strong typing
	LOCAL lRetCode AS LOGIC
	LOCAL iRecNo AS INT

	

	IF oDataServer:Bof
		RETURN TRUE
	ENDIF
	iRecNo := oDataServer:RecNo

	oDataServer:Skip(-1)
	lRetCode := oDataServer:BOF
	oDataServer:GoTo(iRecno)

	RETURN lRetCode

METHOD __Unlink(oDS := NIL AS USUAL) AS Control STRICT 
	//PP-030828 Strong typing
	

	ASend(aColumn, #__UnLink, oDataServer)

	IF oDataServer != NULL_OBJECT
		oDataServer:UnRegisterClient(SELF)
		oDataServer := NULL_OBJECT
	ENDIF

	RETURN SELF

METHOD AddColumn(oGColumn, nIndex) 
	LOCAL i AS INT
	LOCAL iLen AS INT
	LOCAL dwPosition AS DWORD
	LOCAL lRetCode AS LOGIC

	

	SELF:SuspendUpdate()

	IF !IsLong(nIndex) .OR. (nIndex == 0) .OR. (nIndex > ALen(aColumn))
		dwPosition := 0 // append to tail
	ELSE
		dwPosition := SELF:__FindColumn(nIndex)
	ENDIF

	// add array of columns if parameter is array
	IF IsArray(oGColumn)
		iLen := INT(ALen(oGColumn))
		FOR i := 1 UPTO iLen
			lRetCode := SELF:__AddColumn(oGColumn[i], INT(dwPosition))
			IF !lRetcode
				EXIT
			ENDIF
			// iPosition++
		NEXT
	ELSE
		lRetCode := SELF:__AddColumn(oGColumn, INT(dwPosition))
	ENDIF

	// if data server connection - refresh columns
	IF (oDataServer != NULL_OBJECT)
		SELF:__BuildRecordDescription()
		SELF:__RefreshBuffer()
	ENDIF

	SELF:RestoreUpdate()

	RETURN lRetCode

METHOD AsString 
	// Used for ? oDB style syntax
	// Dummy value for now
	

	RETURN ResourceString{__WCSDBrowserObject}:value

ACCESS Background 
	

	RETURN oBackgroundText

ASSIGN Background(oBrush) 
	

	SELF:ChangeBackground(oBrush, gblText)

	RETURN 

METHOD CanUndo() 
	

	IF IsInstanceOf(oCellEdit,#Edit)
		RETURN Send(oCellEdit,#CanUndo)
	ENDIF

	RETURN FALSE

ASSIGN Caption(sCaption) 
	

	SELF:SetCaption(sCaption)

	RETURN 

ACCESS CellEdit 
	// DHer: 18/12/2008
RETURN SELF:oCellEdit

METHOD ChangeBackground ( oBrush, kWhere ) 
	LOCAL iCntLoc AS DWORD
	LOCAL dwNewColor AS DWORD

	

	IF !IsInstanceOfUsual(oBrush,#Brush)
		WCError{#ChangeBackground,#DataBrowser,__WCSTypeError,oBrush,1}:@@Throw()
	ENDIF
	IF !IsNil(kWhere)
		IF !IsLong(kWhere)
			WCError{#ChangeBackground,#DataBrowser,__WCSTypeError,kWhere,2}:@@Throw()
		ENDIF
	ENDIF

	DO CASE
	CASE kWhere==gblCaption
		iCntLoc := CNTCOLOR_TTLBKGD
		oBackgroundCaption := oBrush
		IF oBackgroundCaption==NULL_OBJECT
			dwNewColor := GetSysColor(COLOR_BTNFACE)
		ENDIF

	CASE kWhere==gblText
		iCntLoc := CNTCOLOR_FLDBKGD
		oBackgroundText := oBrush
		IF oBackgroundText==NULL_OBJECT
			dwnewColor := GetSysColor(COLOR_WINDOW)
		ENDIF

	CASE kWhere==gblColCaption
		iCntLoc := CNTCOLOR_FLDTTLBKGD
		oBackgroundColCaption := oBrush
		IF oBackgroundColCaption==NULL_OBJECT
			dwNewColor := GetSysColor(COLOR_BTNFACE)
		ENDIF

	CASE kWhere==gblButton
		iCntLoc := CNTCOLOR_TTLBTNBKGD
		oBackgroundButton := oBrush
		IF oBackgroundButton==NULL_OBJECT
			dwNewColor := GetSysColor(COLOR_BTNFACE)
		ENDIF

	CASE kWhere==gblColButton
		iCntLoc := CNTCOLOR_FLDBTNBKGD
		oBackgroundColButton := oBrush
		IF oBackgroundColButton==NULL_OBJECT
			dwNewColor := GetSysColor(COLOR_BTNFACE)
		ENDIF

	CASE kWhere==gblHiText
		iCntLoc := CNTCOLOR_HIGHLIGHT
		oBackgroundHiText := oBrush
		IF oBackgroundHiText==NULL_OBJECT
			dwNewColor := GetSysColor(COLOR_HIGHLIGHT)
		ENDIF
	ENDCASE

	IF oBrush != NULL_OBJECT
		dwNewColor := __WCGetBrushColor(oBrush)
	ENDIF

	PCALL(gpfnCntColorSet, hwnd, iCntLoc, dwNewColor)

	IF kWhere == gblText
		PCALL(gpfnCntColorSet, hwnd, CNTCOLOR_CNTBKGD, dwNewColor)
	ENDIF

	RETURN SELF

METHOD ChangeFont(oFont, kWhere) 
	LOCAL iCntLoc AS DWORD
	LOCAL oOldFont AS Font
	LOCAL hFont AS PTR

	

	IF !IsInstanceOfUsual(oFont,#Font)
		WCError{#ChangeFont,#DataBrowser,__WCSTypeError,oFont,1}:@@Throw()
	ENDIF
	IF !IsNil(kWhere)
		IF !IsLong(kWhere)
			WCError{#ChangeFont,#DataBrowser,__WCSTypeError,kWhere,2}:@@Throw()
		ENDIF
	ENDIF

	SWITCH (INT) kWhere
	CASE gblCaption
		iCntLoc := CF_TITLE

	CASE gblText
		iCntLoc := CF_GENERAL
		oFontText := oFont

	CASE gblColCaption
		iCntLoc := CF_FLDTITLE

	CASE gblButton
    CASE gblColButton
    CASE gblHiText
		iCntLoc := CF_GENERAL
	END SWITCH

	hFont := oFont:Handle()
	IF (hFont == NULL_PTR)
		oOldFont := oParent:Font
		oParent:Font := oFont
		oParent:SizeText("Hello")
		oParent:Font := oOldFont
	ELSE
		PCALL(gpfnCntFontSet, hwnd, hFont, iCntLoc)
		IF (iCntLoc == CF_GENERAL)
			oEditFont := oFont
		ENDIF
	ENDIF

	RETURN SELF

METHOD ChangeTextColor(oColor, kWhere) 
	LOCAL iCntLoc AS DWORD
	LOCAL ptrColor AS BYTE PTR
	LOCAL dwRGB AS DWORD

	

	IF IsNumeric(oColor)
		oColor := Color{oColor}
	ELSEIF !IsInstanceOfUsual(oColor,#Color)
		WCError{#ChangeTextColor,#DataBrowser,__WCSTypeError,oColor,1}:@@Throw()
	ENDIF
	IF !IsNil(kWhere)
		IF !IsLong(kWhere)
			WCError{#ChangeTextColor,#DataBrowser,__WCSTypeError,kWhere,2}:@@Throw()
		ENDIF
	ENDIF

	iCntLoc := CNTCOLOR_TEXT

	DO CASE
	CASE kWhere==gblCaption
		lUseDefCaption := FALSE
		iCntLoc := CNTCOLOR_TITLE

	CASE kWhere==gblColCaption
		lUseDefColCaption := FALSE
		iCntLoc := CNTCOLOR_FLDTITLES

	CASE kWhere==gblText
		lUseDefText := FALSE
		iCntLoc := CNTCOLOR_TEXT
		oTextColor := oColor

	CASE kWhere==gblButton
		lUseDefButton := FALSE
		iCntLoc := CNTCOLOR_TTLBTNTXT

	CASE kWhere==gblColButton
		lUseDefColButton := FALSE
		iCntLoc := CNTCOLOR_FLDBTNTXT

	CASE kWhere==gblHiText
		lUseDefHiText := FALSE
		iCntLoc := CNTCOLOR_HITEXT
	ENDCASE

	dwRGB := PCALL(gpfnCntColorGet, hwnd, iCntLoc)
#ifdef __VULCAN__	
	ptrColor := (BYTE PTR) @dwRGB
#else	
	ptrColor := @dwRGB
#endif	

	PCALL(gpfnCntColorSet, hwnd, iCntLoc, oColor:ColorRef)

	RETURN Color{ptrColor[1], ptrColor[3], ptrColor[2]}

METHOD Clear() 
	

	IF IsInstanceOf(oCellEdit,#Edit)
		Send(oCellEdit,#Clear)
	ENDIF

	RETURN SELF

METHOD ColPos() 
	LOCAL i, iLen AS INT
	LOCAL oDC AS DataColumn

	

	oDC := SELF:CurrentColumn
	IF oDC==NULL_OBJECT
		RETURN 0
	ENDIF

	iLen := INT(ALen(aColumn))
	FOR i:=1 UPTO iLen
		IF aColumn[i] == oDC
			RETURN i
		ENDIF
	NEXT

	RETURN 0

ACCESS ColumnCount 
	

	RETURN ALen(aColumn)

METHOD ColumnFocusChange(oDataColumn, lHasFocus) 
	LOCAL oHL AS HyperLabel

	

	oHL := oDataColumn:Status
	IF (oHL != NULL_OBJECT)
		SELF:Owner:@@StatusMessage(oHL:Description, MESSAGEERROR)
	ELSEIF lHasFocus .AND. (oDataColumn:HyperLabel != NULL_OBJECT)
		SELF:Owner:@@StatusMessage(oDataColumn:HyperLabel:Description, MESSAGECONTROL)
	ENDIF

	RETURN SELF

METHOD ColumnMoved(oColumn) 
	

	SELF:__EndEditField(0)
	RETURN SELF

METHOD ColumnReSize(oColumn) 
	

	SELF:__EndEditField(0)

	// self:__BuildRecordDescription()
	// self:__RefreshBuffer()

	RETURN SELF

METHOD Copy() 
	

	IF IsInstanceOf(oCellEdit,#Edit)
		Send(oCellEdit,#Copy)
	ENDIF

	RETURN SELF

ACCESS CurrentColumn 
	LOCAL strucFI AS _WinFieldInfo

	

	StrucFI := PCALL(gpfnCntFocusFldGet, hwnd)
	IF strucFI != NULL_PTR
		RETURN SELF:__GetColumn(strucFI)
	ENDIF
	RETURN NULL_OBJECT

METHOD Cut() 
	

	IF IsInstanceOf(oCellEdit,#Edit)
		Send(oCellEdit,#Cut)
	ENDIF

	RETURN SELF

METHOD Default(oEvent) 
	LOCAL oEvt := oEvent AS @@event

	

	SELF:EventReturnValue := DefWindowProc(oEvt:hWnd, oEvt:uMsg, oEvt:wParam, oEvt:lParam)

	RETURN SELF

METHOD Destroy() 
	LOCAL i AS DWORD
    LOCAL oCol  AS DataColumn

	SELF:__Unlink()

	IF oCellEdit != NULL_OBJECT
		oCellEdit:Destroy()
	ENDIF

	IF IsWindow(hNotifyWindow) .AND. IsWindow(hwnd)
		PCALL(gpfnCntAssociateSet, hwnd, NULL_PTR)
		DestroyWindow(hNotifyWindow)
	ENDIF

   __WCSelfPtrFree(ptrSelf)
   ptrSelf := NULL_PTR

	IF (ptrDataBuffer != NULL_PTR)
		MemFree(ptrDataBuffer)
	ENDIF

   //PP-040418 Issue 12768
   //SE-050730
	FOR i := 1 TO ALen(aColumn)
        oCol := aColumn[i] 
        oCol:destroy()
	NEXT


	IF !InCollect()
		ptrDataBuffer := NULL_PTR
		strucEditField := NULL_PTR
		strucEditRecord := NULL_PTR
		strucFocusField := NULL_PTR
		hNotifyWindow := NULL_PTR
		oCellEdit := NULL_OBJECT
		ptrControlDefaultProc := NULL_PTR
		oDataServer := NULL_OBJECT
		oEditFont := NULL_OBJECT
   //SE-050730
		// 		//PP-040418 Issue 12768
		// 		FOR i := 1 TO ALen(aColumn)
		// 			aColumn[i]:destroy()
		// 		NEXT
		aColumn := NULL_ARRAY

		oFontText := NULL_OBJECT
		oBackgroundColCaption := NULL_OBJECT
		oBackgroundHiText := NULL_OBJECT
		oBackgroundText := NULL_OBJECT
		oBackgroundCaption := NULL_OBJECT
		oBackgroundButton := NULL_OBJECT
		oBackgroundColButton := NULL_OBJECT
		oTextColor := NULL_OBJECT
		oTextPointer := NULL_OBJECT
	ENDIF

	SUPER:Destroy()

	RETURN SELF

METHOD Dispatch(oEvent) 
	//PP-040519 Update from S Ebert
	LOCAL oEvt := oEvent AS @@event
	LOCAL uMsg AS DWORD
	LOCAL lParam AS LONGINT
	LOCAL dwCode AS DWORD
	LOCAL dwChar AS DWORD
	LOCAL iMove AS DWORD
	LOCAL wFlag AS WORD
	LOCAL oDC AS DataColumn
	LOCAL strucRC AS _WinRecordCore
	LOCAL strucFI AS _WinFieldInfo

	

	//This method handles the messages of two windows
	//1.) CONTAINER_CLASS ("Ca_Container32")
	//2.) GBNotifyWindow


	uMsg := oEvt:uMsg
	SELF:EventReturnValue := 0L

	IF oEvt:hWnd = hNotifyWindow
		//Dispatcher for GBNotifyWindow
		IF uMsg == WM_COMMAND
			dwCode := HiWord(oEvt:wParam)
			SWITCH dwCode
			CASE CN_QUERYFOCUS
				SELF:__BuildBuffer()

			CASE CN_QUERYDELTA
				SELF:__DeltaBuildBuffer()

			CASE CN_NEWFOCUS
            CASE CN_NEWFOCUSREC
				strucRC := PCALL(gpfnCntCNRecGet, hwnd, oEvt:lParam)
				strucFI := PCALL(gpfnCntFocusFldGet, hwnd)
				//		if self:Validate() .and. oDataServer:Notify(NotifyIntentToMove)
				SELF:__NewFocusRecord(strucRC, strucFI)
				//		endif

			CASE CN_TAB
				IF !PCALL(gpfnCntCNShiftKeyGet, hwnd, lParam)
					iMove := CFM_RIGHT
				ENDIF
				IF IsBiDi()
					IF (iMove == CFM_LEFT)
						iMove := CFM_RIGHT
					ELSE
						iMove := CFM_LEFT
					ENDIF
				ENDIF

				IF !PCALL(gpfnCntFocusMove, hwnd, iMove)
					IF (iMove == CFM_RIGHT)
						PCALL(gpfnCntFocusMove, hwnd, CFM_DOWN)
					ELSE
						PCALL(gpfnCntFocusMove, hwnd, CFM_UP)
					ENDIF
					IF (!IsBiDi() .AND. iMove == CFM_RIGHT) .OR. (IsBiDi() .AND. iMove == CFM_LEFT)
						PCALL(gpfnCntFocusMove, hwnd, CFM_HOME)
					ELSE
						PCALL(gpfnCntFocusMove, hwnd, CFM_END)
					ENDIF
				ENDIF

			CASE CN_BEGFLDEDIT
            CASE CN_ROFLDDBLCLK
				IF IsMethod(SELF, #CellDoubleClick)
					Send(SELF, #CellDoubleClick)
				ELSEIF !PCALL(gpfnCntIsFocusCellRO, hwnd)
					SELF:__BeginEditField(PCALL(gpfnCntCNChildWndGet, hwnd, 0), 0)
				ENDIF

			CASE CN_CHARHIT
				IF !PCALL(gpfnCntIsFocusCellRO, hwnd)
					//lParam := oEvt:lParam
					//ci := CntInfoGet(hwnd)
					//dw := CntCNCharGet(hWnd, lParam)
					//self:__BeginEditField(CntCNChildWndGet(hWnd, lParam), dw, lParam)
					SELF:__BeginEditField(PCALL(gpfnCntCNChildWndGet, hwnd, lParam), dwLastChar)
				ENDIF

			CASE CN_ENTER
				IF !PCALL(gpfnCntIsFocusCellRO, hwnd)
					SELF:__BeginEditField(PCALL(gpfnCntCNChildWndGet, hwnd, lParam), 0)
				ENDIF

			CASE CN_ENDFLDEDIT
				lParam:=oEvt:lParam
				dwChar := PCALL(gpfnCntCNCharGet, hwnd, lParam)
				SELF:__EndEditField(dwChar)

				SWITCH dwChar
				CASE KeyTab
					IF !PCALL(gpfnCntCNShiftKeyGet, hwnd, lParam)
						iMove := CFM_RIGHT
					ENDIF
					PCALL(gpfnCntFocusMove, hWnd, iMove)

				CASE KeyArrowUp
					//	if self:Validate() .and. oDataServer:Notify(NotifyIntentToMove)
					PCALL(gpfnCntFocusMove, hwnd, CFM_UP)
					//		endif

				CASE KeyArrowDown
					//if self:Validate() .and. oDataServer:Notify(NotifyIntentToMove)
					PCALL(gpfnCntFocusMove, hwnd, CFM_DOWN)
					//endif

				CASE KeyArrowLeft
					PCALL(gpfnCntFocusMove, hwnd, CFM_LEFT)

				CASE KeyArrowRight
					PCALL(gpfnCntFocusMove, hwnd, CFM_RIGHT)

				CASE KeyPageUp
					IF SELF:Validate() .AND. oDataServer:Notify(NotifyIntentToMove)
						PCALL(gpfnCntFocusMove, hwnd, CFM_PAGEUP)
					ENDIF

				CASE KeyPageDown
					IF SELF:Validate() .AND. oDataServer:Notify(NotifyIntentToMove)
						PCALL(gpfnCntFocusMove, hwnd, CFM_PAGEDOWN)
					ENDIF
				END SWITCH

			CASE CN_FLDSIZED
				oDC := SELF:__GetColumn(PCALL(gpfnCntCNFldGet, hwnd, lParam))
				IF oDC!=NULL_OBJECT
					SELF:ColumnReSize(oDC)
				ENDIF

			CASE CN_FLDMOVED
				oDC := SELF:__GetColumn(PCALL(gpfnCntCNFldGet, hwnd, lParam))
				IF oDC!=NULL_OBJECT
					SELF:ColumnMoved(oDC)
				ENDIF

			CASE CN_LK_HOME
				PCALL(gpfnCntFocusSet, hwnd, PCALL(gpfnCntFocusRecGet, hwnd), NULL_PTR)

			CASE CN_LK_END
				PCALL(gpfnCntFocusSet, hwnd, PCALL(gpfnCntFocusRecGet, hwnd), PCALL(gpfnCntFldTailGet, hwnd))

			CASE CN_LK_PAGEUP
				IF SELF:Validate() .AND. oDataServer:Notify(NotifyIntentToMove)
					PCALL(gpfnCntFocusRecUnlck, hwnd)
					PCALL(gpfnCntFocusMove, hwnd, CFM_PAGEUP)
				ENDIF
			CASE CN_LK_PAGEDOWN
				IF SELF:Validate() .AND. oDataServer:Notify(NotifyIntentToMove)
					PCALL(gpfnCntFocusRecUnlck, hwnd)
					PCALL(gpfnCntFocusMove, hwnd, CFM_PAGEDOWN)
				ENDIF
			CASE CN_LK_ARROW_UP
				IF SELF:Validate() .AND. oDataServer:Notify(NotifyIntentToMove)
					PCALL(gpfnCntFocusRecUnlck, hwnd)
					PCALL(gpfnCntFocusMove, hwnd, CFM_UP)
				ENDIF

			CASE CN_LK_ARROW_DOWN
				IF SELF:Validate() .AND. oDataServer:Notify(NotifyIntentToMove)
					PCALL(gpfnCntFocusRecUnlck, hwnd)
					PCALL(gpfnCntFocusMove, hwnd, CFM_DOWN)
				ENDIF

			CASE CN_LK_ARROW_LEFT
				PCALL(gpfnCntFocusRecUnlck, hwnd)
				PCALL(gpfnCntFocusMove, hwnd, CFM_LEFT)

			CASE CN_LK_ARROW_RIGHT
				PCALL(gpfnCntFocusRecUnlck, hwnd)
				PCALL(gpfnCntFocusMove, hwnd, CFM_RIGHT)

			CASE CN_LK_VS_TOP
            CASE CN_LK_VS_BOTTOM
            CASE CN_LK_VS_PAGEUP
            CASE CN_LK_VS_PAGEDOWN
            CASE CN_LK_VS_LINEUP
            CASE CN_LK_VS_LINEDOWN
            CASE CN_LK_VS_THUMBPOS
				PCALL(gpfnCntScrollRecAreaEx, hwnd, PCALL(gpfnCntCNIncExGet, hwnd, lParam))

			CASE CN_LK_HS_PAGEUP 
			CASE CN_LK_HS_PAGEDOWN 
			CASE CN_LK_HS_LINEUP 
			CASE CN_LK_HS_LINEDOWN 
			CASE CN_LK_HS_THUMBPOS
				PCALL(gpfnCntScrollFldArea, hwnd, PCALL(gpfnCntCNIncExGet, hwnd, lParam))

			CASE CN_LK_NEWFOCUS
            CASE CN_LK_NEWFOCUSREC
				strucRC := PCALL(gpfnCntCNRecGet, hwnd, lParam)
				strucFI := PCALL(gpfnCntCNFldGet, hwnd, lParam)
				IF SELF:Validate() .AND. oDataServer:Notify(NotifyIntentToMove)
					PCALL(gpfnCntFocusRecUnlck, hwnd)
					SELF:__NewFocusRecord(strucRC, strucFI)
				ENDIF

			CASE CN_LK_NEWFOCUSFLD
				SELF:__EndEditField(0)
				PCALL(gpfnCntFocusSet, hwnd, PCALL(gpfnCntCNRecGet, hwnd, lParam), PCALL(gpfnCntCNFldGet, hwnd, lParam))

			CASE CN_NEWFOCUSFLD
				SELF:__NewFocusField(PCALL(gpfnCntCNFldGet, hwnd, lParam))

			END SWITCH
		ELSE
			SELF:Default(oEvt)
		ENDIF

		RETURN SELF:EventReturnValue
	ENDIF

	//Dispatcher for CONTAINER_CLASS ("Ca_Container32")
	IF uMsg == WM_PARENTNOTIFY
		wFlag := LoWord(oEvt:wParam)
		IF (wFlag == WM_LBUTTONDOWN) .OR. (wFlag == WM_RBUTTONDOWN) .OR. (wFlag == WM_MBUTTONDOWN) .OR. ;
			wFlag == WM_XBUTTONDOWN
			//PP-030904 XButton
			SELF:MouseButtonDown(MouseEvent{oEvt:hwnd, wFlag, 0, oEvt:lParam})
		ENDIF

	ELSEIF uMsg == WM_SIZE
		SELF:__EndEditField(0)

	ENDIF

	RETURN SUPER:Dispatch(oEvt)

ACCESS EditFont 
	

	RETURN oEditFont

METHOD EnableBorder(kBorderType) 
	

	IF !IsNil(kBorderType)
		IF !IsLong(kBorderType)
			WCError{#EnableBorder,#DataBrowser,__WCSTypeError,kBorderType,1}:@@Throw()
		ENDIF
	ENDIF

	SWITCH (INT) kBorderType

	CASE BTSIZINGBORDER
		PCALL(gpfnCntStyleSet, hwnd, WS_THICKFRAME)
	CASE BTNONSIZINGBORDER
		PCALL(gpfnCntStyleClear, hwnd, WS_THICKFRAME)
		PCALL(gpfnCntStyleSet, hwnd, WS_BORDER)
	CASE BTNOBORDER
		PCALL(gpfnCntStyleClear, hwnd, _OR(WS_THICKFRAME,WS_BORDER))
	OTHERWISE
		PCALL(gpfnCntStyleSet, hwnd, WS_THICKFRAME)
	END SWITCH

	RETURN NIL

METHOD EnableColumnMove(lAllowMove) 
	

	DEFAULT(@lAllowMove, TRUE)

	IF !IsNil(lAllowMove)
		IF !IsLogic(lAllowMove)
			WCError{#EnableColumnMove,#DataBrowser,__WCSTypeError,lAllowMove,1}:@@Throw()
		ENDIF
	ENDIF

	IF lAllowMove
		PCALL(gpfnCntAttribSet, hwnd, CA_MOVEABLEFLDS)
	ELSE
		PCALL(gpfnCntAttribClear, hwnd, CA_MOVEABLEFLDS)
	ENDIF

	RETURN SELF

METHOD EnableColumnReSize(lAllowResize) 
	

	DEFAULT(@lAllowResize, TRUE)

	IF !IsNil(lAllowResize)
		IF !IsLogic(lAllowResize)
			WCError{#EnableColumnReSize,#DataBrowser,__WCSTypeError,lAllowResize,1}:@@Throw()
		ENDIF
	ENDIF

	IF lAllowResize
		PCALL(gpfnCntAttribSet, hwnd, CA_SIZEABLEFLDS)
	ELSE
		PCALL(gpfnCntAttribClear, hwnd, CA_SIZEABLEFLDS)
	ENDIF

	RETURN SELF

METHOD EnableColumnTitles(lEnable) 
	LOCAL iLen, i AS DWORD
	LOCAL strucFI AS _WinFieldInfo
    LOCAL oColumn   AS DataColumn
	

	IF !IsNil(lEnable)
		IF !IsLogic(lEnable)
			WCError{#EnableColumnTitles,#DataBrowser,__WCSTypeError,lEnable,1}:@@Throw()
		ENDIF
	ELSE
		lEnable:=TRUE
	ENDIF

	IF lColumnTitles == lEnable
		RETURN FALSE
	ENDIF
	SELF:SuspendUpdate()

	lColumnTitles := lEnable
	iFTHeight:=0
	IF lEnable
		iLen := ALen(aColumn)
		FOR i:=1 UPTO iLen
            oColumn := aColumn[i] 
            strucFI:= oColumn:__FieldInfo
			IF iFTHeight < INT(strucFI:wFTitleLines)
				iFTHeight := INT(strucFI:wFTitleLines)
			ENDIF
		NEXT
	ENDIF

	PCALL(gpfnCntFldTtlHtSet, hwnd, iFTHeight)

	SELF:RestoreUpdate()

	RETURN TRUE

METHOD EnableGrid ( lShowGrid ) 
	

	IF !IsNil(lShowGrid)
		IF !IsLogic(lShowGrid)
			WCError{#EnableGrid,#DataBrowser,__WCSTypeError,lShowGrid,1}:@@Throw()
		ENDIF
	ELSE
		lShowGrid:=TRUE
	ENDIF

	IF lShowGrid
		PCALL(gpfnCntAttribSet, hwnd, _OR(CA_RECSEPARATOR,CA_VERTFLDSEP))
	ELSE
		PCALL(gpfnCntAttribClear, hwnd, _OR(CA_RECSEPARATOR,CA_VERTFLDSEP))
	ENDIF

	RETURN SELF

METHOD EnableHorizontalScroll ( lAllowScroll ) 
	

	IF !IsNil(lAllowScroll)
		IF !IsLogic(lAllowScroll)
			WCError{#EnableHorizontalScroll,#DataBrowser,__WCSTypeError,lAllowScroll,1}:@@Throw()
		ENDIF
	ELSE
		lAllowScroll:=TRUE
	ENDIF

	IF !lIsShown
		IF lAllowScroll
			dwDeferredStyle := _OR(dwDeferredStyle,DWORD(CTS_HORZSCROLL))
		ELSE
			dwDeferredStyle := _AND(dwDeferredStyle,_NOT(DWORD(CTS_HORZSCROLL)))
		ENDIF
	ELSE
		IF lAllowScroll
			PCALL(gpfnCntStyleSet, hwnd, CTS_HORZSCROLL)
		ELSE
			PCALL(gpfnCntStyleClear, hwnd, CTS_HORZSCROLL)
		ENDIF
	ENDIF

	RETURN SELF

METHOD EnableHorizontalSplit ( lShowSplit ) 
	

	IF !IsNil(lShowSplit)
		IF !IsLogic(lShowSplit)
			WCError{#EnableHorizontalSplit,#DataBrowser,__WCSTypeError,lShowSplit,1}:@@Throw()
		ENDIF
	ELSE
		lShowSplit:=TRUE
	ENDIF
	//Riz This was never implemented

	RETURN SELF

METHOD EnableVerticalScroll ( lAllowScroll ) 
	

	IF !IsNil(lAllowScroll)
		IF !IsLogic(lAllowScroll)
			WCError{#EnableVerticalScroll,#DataBrowser,__WCSTypeError,lAllowScroll,1}:@@Throw()
		ENDIF
	ELSE
		lAllowScroll:=TRUE
	ENDIF

	IF !lIsShown
		IF lAllowScroll
			dwDeferredStyle := _OR(dwDeferredStyle,DWORD(CTS_VERTSCROLL))
		ELSE
			dwDeferredStyle := _AND(dwDeferredStyle,_NOT(DWORD(CTS_VERTSCROLL)))
		ENDIF
	ELSE
		IF lAllowScroll
			PCALL(gpfnCntStyleSet, hwnd, CTS_VERTSCROLL)
		ELSE
			PCALL(gpfnCntStyleClear, hwnd, CTS_VERTSCROLL)
		ENDIF
	ENDIF

	RETURN SELF

METHOD EnableVerticalSplit(lShowSplit, nMode) 
	

	IF !IsNil(lShowSplit)
		IF !IsLogic(lShowSplit)
			WCError{#EnableVerticalSplit,#DataBrowser,__WCSTypeError,lShowSplit,1}:@@Throw()
		ENDIF
	ELSE
		lShowSplit := TRUE
	ENDIF

	IF !IsNil(nMode)
		IF !IsLong(nMode)
			WCError{#EnableVerticalSplit,#DataBrowser,__WCSTypeError,nMode,2}:@@Throw()
		ENDIF
	ELSE
		nMode := GBSSBMIDDLE
	ENDIF

	IF lShowSplit
		PCALL(gpfnCntStyleSet, hwnd, CTS_SPLITBAR)
		PCALL(gpfnCntSpltBarCreate, hwnd, nMode, 0)
	ELSE
		PCALL(gpfnCntSpltBarDelete, hwnd, 0, 0)
		PCALL(gpfnCntStyleClear, hwnd, CTS_SPLITBAR)
	ENDIF

	RETURN SELF

METHOD Error(oErrorObj) 
	

	RETURN SELF

ACCESS Font 
	

	RETURN oFontText

ASSIGN Font(oFont) 
	

	SELF:ChangeFont(oFont, gblText)

	RETURN 

METHOD GetColumn(xColumnID) 
	LOCAL dwPosition AS DWORD

	

	IF !IsLong(xColumnID) .AND. !IsSymbol(xColumnID) .AND. !IsString(xColumnID)
		WCError{#GetColumn,#DataBrowser,__WCSTypeError,xColumnID,1}:@@Throw()
	ENDIF

	dwPosition := SELF:__FindColumn(xColumnID)
	IF dwPosition != 0
		RETURN aColumn[dwPosition]
	ENDIF

	RETURN NULL_OBJECT

ACCESS HiBackground 
	// DHer: 18/12/2008
RETURN SELF:oBackgroundHiText


CONSTRUCTOR(oOwner, xID, oPoint, oDimension) 
	LOCAL oBB AS BoundingBox
	LOCAL oWin AS Window
	LOCAL hChild AS PTR
	LOCAL oDataWin AS DataWindow

	__LoadContainerDLL()

	IF !IsInstanceOfUsual(oOwner,#Window)
		WCError{#Init,#DataBrowser,__WCSTypeError,oOwner,1}:@@Throw()
	ENDIF
	oWin := oOwner

	// Automatically generate id if none is supplied
	DEFAULT(@xID, 1000)

	IF (IsInstanceOfUsual(oOwner, #DataWindow))
		oBB := oOwner:CanvasArea
		oPoint := Point{0,0}
		oDimension := Dimension{obb:Width,obb:Height}
	ENDIF

	IF IsInstanceOf(oWin, #DataWindow) 
	    oDataWin := (DataWindow) oWin
		oWin := oDataWin:__FormWindow
	ENDIF

	SUPER(oWin, xID, oPoint, oDimension, CONTAINER_CLASS, CTS_SPLITBAR)

	iBufferGranularity := INT(_CAST, QueryRTRegInt("Browser", "Granularity"))
	IF (iBufferGranularity == 0)
		iBufferGranularity := 64
	ENDIF
	iBufferGranularity := Max(16, iBufferGranularity)

	iBufferMaximum := INT(_CAST, QueryRTRegInt("Browser", "Maximum"))
	IF (iBufferMaximum == 0)
		iBufferMaximum := 1024
	ENDIF
	iBufferMaximum := Max(2 * iBufferGranularity, iBufferMaximum)

	lColumnTitles := TRUE

	lUseDefCaption := TRUE
	lUseDefColCaption := TRUE
	lUseDefText := TRUE
	lUseDefButton := TRUE
	lUseDefColButton := TRUE
	lUseDefHiText := TRUE
	aColumn := {}

	SELF:SetFocus() // Force Handle to be created

	IF __WCRegisterGBNotifyWindow(_GetInst())
		//Create ptr to pass for WM_Create and to be used for message processing
		ptrSelf := __WCSelfPtrAlloc(SELF)
        hNotifyWindow := CreateWindowEx(0, String2Psz(__WCGBNotifyWindowClass), NULL_PSZ, 0,;
			CW_USEDEFAULT, 0, CW_USEDEFAULT, 0,;
			0, 0, _GetInst(), ptrSelf)

		IF (hNotifyWindow != NULL_PTR)
			PCALL(gpfnCntAssociateSet, hwnd, hNotifyWindow)
		ENDIF
	ENDIF

	PCALL(gpfnCntViewSet, hwnd, CV_DETAIL)

	lUse3dLook := TRUE
	// Set up default colors so that they respond to Control Panel
	// This appears to affect over all container
	PCALL(gpfnCntColorSet, hwnd, CNTCOLOR_CNTBKGD, GetSysColor(COLOR_WINDOW))
	PCALL(gpfnCntColorSet, hwnd, CNTCOLOR_FLDBKGD,		GetSysColor(COLOR_WINDOW))
	PCALL(gpfnCntColorSet, hwnd, CNTCOLOR_TEXT,		 GetSysColor(COLOR_WINDOWTEXT))
	PCALL(gpfnCntColorSet, hwnd, CNTCOLOR_GRID,		 GetSysColor(COLOR_WINDOWFRAME))
	PCALL(gpfnCntColorSet, hwnd, CNTCOLOR_3DHIGH,		GetSysColor(COLOR_BTNHIGHLIGHT))
	PCALL(gpfnCntColorSet, hwnd, CNTCOLOR_3DSHADOW,	GetSysColor(COLOR_BTNSHADOW))
	PCALL(gpfnCntColorSet, hwnd, CNTCOLOR_HIGHLIGHT,	GetSysColor(COLOR_HIGHLIGHT))
	PCALL(gpfnCntColorSet, hwnd, CNTCOLOR_HITEXT,		GetSysColor(COLOR_HIGHLIGHTTEXT))

	//Set title defaults for 3D appearance
	PCALL(gpfnCntColorSet, hwnd, CNTCOLOR_TITLE, GetSysColor(COLOR_BTNTEXT))
	PCALL(gpfnCntColorSet, hwnd, CNTCOLOR_FLDTITLES, GetSysColor(COLOR_BTNTEXT))
	PCALL(gpfnCntColorSet, hwnd, CNTCOLOR_TTLBKGD, GetSysColor(COLOR_BTNFACE))
	PCALL(gpfnCntColorSet, hwnd, CNTCOLOR_FLDTTLBKGD, GetSysColor(COLOR_BTNFACE))

	// Colors for buttons
	PCALL(gpfnCntColorSet, hwnd, CNTCOLOR_TTLBTNTXT, GetSysColor(COLOR_BTNTEXT))
	PCALL(gpfnCntColorSet, hwnd, CNTCOLOR_TTLBTNBKGD, GetSysColor(COLOR_BTNFACE))
	PCALL(gpfnCntColorSet, hwnd, CNTCOLOR_FLDBTNTXT, GetSysColor(COLOR_BTNTEXT))
	PCALL(gpfnCntColorSet, hwnd, CNTCOLOR_FLDBTNBKGD, GetSysColor(COLOR_BTNFACE))

	// Default line spacing - Quarter Line
	PCALL(gpfnCntRowHtSet, hwnd, 1, CA_LS_MEDIUM)


	// Set default font
	PCALL(gpfnCntFontSet, hwnd, GetStockObject(DEFAULT_GUI_FONT), CF_GENERAL)

	PCALL(gpfnCntAttribSet, hwnd, CA_APPSPLITABLE)
	PCALL(gpfnCntRangeExSet, hwnd, 0, 0)

	SELF:__EnableSelection(ssSingleSelection)

	SELF:EnableColumnMove()
	SELF:EnableColumnReSize()
	SELF:EnableGrid()
	SELF:EnableHorizontalScroll()
	SELF:EnableVerticalScroll()

	IF (IsInstanceOfUsual(oOwner, #DataWindow))
		SELF:__AutoResize()
	ENDIF

	hChild := GetWindow(hwnd, GW_CHILD)
	pfGBChildProcOrg := PTR(_CAST, GetWindowLong(hChild, GWL_WNDPROC))
#ifdef __VULCAN__
   IF WCGBChildProcDelegate == NULL
      WCGBChildProcDelegate := __WCGBChildProcDelegate{ NULL, @__WCGBChildProc() }
   ENDIF
	SetWindowLong(hChild, GWL_WNDPROC, (INT) System.Runtime.InteropServices.Marshal.GetFunctionPointerForDelegate( (System.Delegate) WCGBChildProcDelegate ) )
#else	
	SetWindowLong(hChild, GWL_WNDPROC, LONGINT(_CAST, @__WCGBChildProc()))
#endif	

	RETURN 

METHOD NewRow() 
	

	RETURN FALSE

METHOD Notify(kNotification, uDescription) 
	

	DO CASE
	CASE kNotification = NOTIFYCOMPLETION
		SELF:__NotifyChanges(GBNFY_COMPLETION)
		nOldRecordNum := oDataServer:Recno
	CASE kNotification = NOTIFYINTENTTOMOVE
		RETURN SELF:__NotifyChanges(GBNFY_INTENTTOMOVE)
	CASE kNotification = NOTIFYFILECHANGE
		ASend(aColumn, #__Scatter)
		SELF:__NotifyChanges(GBNFY_FILECHANGE)
		ASend(aColumn, #__Scatter)
		nOldRecordNum := oDataServer:Recno
	CASE kNotification = NOTIFYFIELDCHANGE
		SELF:__RefreshField(uDescription)
		SELF:__NotifyChanges(GBNFY_FIELDCHANGE)
	CASE kNotification = NOTIFYCLOSE
		SELF:__Unlink()
	CASE kNotification = NOTIFYRECORDCHANGE
		ASend(aColumn, #__Scatter)

		IF nOldRecordNum != oDataServer:Recno
			SELF:__NotifyChanges(GBNFY_RECORDCHANGE)
			ASend(aColumn, #__Scatter)
		ELSE
			SELF:__NotifyChanges(GBNFY_FIELDCHANGE)
		ENDIF
		nOldRecordNum := oDataServer:Recno
	CASE kNotification = NOTIFYGOBOTTOM
		ASend(aColumn, #__Scatter)
		SELF:__NotifyChanges(GBNFY_DOGOEND)
		ASend(aColumn, #__Scatter)
		nOldRecordNum := oDataServer:Recno
	CASE kNotification = NOTIFYGOTOP
		ASend(aColumn, #__Scatter)
		SELF:__NotifyChanges(GBNFY_DOGOTOP)
		ASend(aColumn, #__Scatter)
		nOldRecordNum := oDataServer:Recno
	CASE kNotification = NOTIFYDELETE
		ASend(aColumn, #__Scatter)
		SELF:__NotifyChanges(GBNFY_DODELETE)
		ASend(aColumn, #__Scatter)
		nOldRecordNum := oDataServer:Recno
	CASE kNotification = NOTIFYAPPEND
		ASend(aColumn, #__Scatter)
		SELF:__NotifyChanges(GBNFY_DONEWROW)
		ASend(aColumn, #__Scatter)
		ASend(aColumn, #PerformValidations)
		nOldRecordNum := oDataServer:Recno
	END CASE

	RETURN NIL

ACCESS Owner 
	

	//return oParent
	IF !IsInstanceOf(oParent, #__FormFrame)
		RETURN oParent
	ENDIF
	RETURN IVarGet(oParent, #DataWindow)

METHOD Paste ( ) 
	

	IF IsInstanceOf(oCellEdit,#Edit)
		Send(oCellEdit,#Paste)
	ENDIF

	RETURN SELF

ACCESS Pointer 
	

	RETURN oTextPointer

ASSIGN Pointer(oPointer) 
	

	SELF:SetPointer(oPointer, gblText)
	RETURN 

METHOD Refresh() 
	

	IF oDataServer!=NULL_OBJECT .AND. IsInstanceOf(oDataServer,#DBServer)
		oDataServer:Goto(oDataServer:Recno) //Forces refresh if DBF was empty
	ENDIF
	SELF:__RefreshBuffer()
	ASend(aColumn, #__Scatter)

	RETURN SELF

METHOD RemoveColumn(uColumnOrIndex) 
	LOCAL i AS DWORD
	LOCAL oDC AS DataColumn

	

	i := SELF:__FindColumn(uColumnOrIndex)

	IF (i != 0)
		SELF:SuspendUpdate()

		IF oDataServer!=NULL_OBJECT
			SELF:__ClearBuffers()
		ENDIF

		oDC := aColumn[i]
		ADel(aColumn, i)
		ASize(aColumn, ALen(aColumn)-1)
		PCALL(gpfnCntRemoveFld, hwnd, oDC:__FieldInfo)
		oDC:__Owner := NULL_OBJECT

		// if data server connection - refresh columns
		IF oDataServer!=NULL_OBJECT
			SELF:__BuildRecordDescription()
			SELF:__RefreshBuffer()
		ENDIF

		SELF:RestoreUpdate()

		strucFocusField := PCALL(gpfnCntFocusFldGet, hwnd)
	ENDIF

	RETURN oDC

METHOD RestoreUpdate() 
	

	IF iDeferPaintCount!=0
		--iDeferPaintCount
	ENDIF

	IF iDeferPaintCount == 0
		PCALL(gpfnCntEndDeferPaint, hwnd, TRUE)
	ENDIF

	RETURN SELF

ACCESS RowCount 
	

	RETURN PCALL(gpfnCntRecsDispGet, hwnd)

METHOD SetCaption(cText) 
	LOCAL iLines AS INT

	

	IF IsSymbol(cText)
		cCaption := Symbol2String(cText)
	ELSEIF IsString(cText)
		cCaption := cText
	ELSEIF !IsNil(cText)
		WCError{#SetCaption,#DataBrowser,__WCSTypeError,cText,1}:@@Throw()
	ENDIF

	SELF:SuspendUpdate()
	IF NULL_STRING != cCaption

		PCALL(gpfnCntAttribSet, hwnd, CA_TITLE)
		IF lUse3dLook
			PCALL(gpfnCntAttribSet, hwnd, CA_TITLE3D)
		ENDIF
		PCALL(gpfnCntTtlSet, hWnd, String2Psz(cCaption))
		iLines := INT(Occurs(Chr(10),cCaption)) + 1
		IF iLines == 1
			PCALL(gpfnCntTtlAlignSet, hwnd, _OR(CA_TA_HCENTER, CA_TA_VCENTER))
		ELSE
			PCALL(gpfnCntTtlAlignSet, hwnd, _OR(CA_TA_HCENTER, CA_TA_TOP))
		ENDIF
		PCALL(gpfnCntTtlHtSet, hwnd, ilines)
		PCALL(gpfnCntTtlSepSet, hwnd)
	ELSE
		PCALL(gpfnCntTtlHtSet, hwnd, 0)
		IF NULL_STRING != cCaption
			cCaption := NULL_STRING
			PCALL(gpfnCntTtlSet, hWnd, String2Psz(cCaption))
		ENDIF
		PCALL(gpfnCntAttribClear, hwnd, CA_TITLE)
	ENDIF

	SELF:RestoreUpdate()

	RETURN SELF

METHOD SetColumn(oDataColumn, nColumnNumber) 
	LOCAL oDC AS DataColumn

	

	IF !IsInstanceOfUsual(oDataColumn,#DataColumn)
		WCError{#SetColumn,#DataBrowser,__WCSTypeError,oDataColumn,1}:@@Throw()
	ENDIF
	IF !IsNil(nColumnNumber)
		IF !IsLong(nColumnNumber)
			WCError{#SetColumn,#DataBrowser,__WCSTypeError,nColumnNumber,2}:@@Throw()
		ENDIF
	ENDIF

	oDC := SELF:GetColumn(nColumnNumber)

	IF oDC!=NULL_OBJECT
		SELF:SuspendUpdate()
		SELF:RemoveColumn(oDC)
		SELF:AddColumn(oDataColumn, nColumnNumber)
		SELF:RestoreUpdate()
	ENDIF

	RETURN oDC

METHOD SetColumnFocus(oColumn) 
	LOCAL oDC AS DataColumn

	IF !IsInstanceOfUsual(oColumn,#DataColumn)
		WCError{#SetColumnFocus,#DataBrowser,__WCSTypeError,oColumn,1}:@@Throw()
	ENDIF

	// Cpc 2010-03-25 strongly typed to workaround bug #852
	oDC := oColumn

	IF PCALL(gpfnCntFocusSet, hwnd, PCALL(gpfnCntFocusRecGet, hwnd), oDC:__FieldInfo)
		SELF:SetFocus()
		RETURN TRUE
	ENDIF

	RETURN FALSE

METHOD SetPointer(oPointer, kWhere) 
	LOCAL iLoc AS DWORD

	DEFAULT(@oPointer, Pointer{PointerArrow})

	IF !IsInstanceOfUsual(oPointer,#Pointer)
		WCError{#SetPointer,#Pointer,__WCSTypeError,oPointer,1}:@@Throw()
	ENDIF

	IF !IsNil(kWhere)
		IF !IsLong(kWhere)
			WCError{#SetPointer,#DataBrowser,__WCSTypeError,kWhere,2}:@@Throw()
		ENDIF
	ENDIF

	iLoc := CC_GENERAL
	DO CASE
	CASE (kWhere == GBLCAPTION)
		iLoc := CC_TITLE

	CASE (kWhere == GBLCOLCAPTION)
		iLoc := CC_FLDTITLE

	CASE (kWhere == GBLTEXT)
		iLoc := CC_GENERAL
		oTextPointer := oPointer
	ENDCASE

	PCALL(gpfnCntCursorSet, hwnd, oPointer:Handle(), iLoc)

	RETURN NIL

METHOD SetStandardStyle(kStyle) 
	

	IF !IsNil(kStyle)
		IF !IsLong(kStyle)
			WCError{#SetStandardStyle,#DataBrowser,__WCSTypeError,kStyle,1}:@@Throw()
		ENDIF
	ENDIF

	SELF:SuspendUpdate()

	IF IsNil(kStyle)
		PCALL(gpfnCntStyleClear, hwnd, CTS_READONLY)
		kStyle:=gbsControl3d
	ENDIF

	SWITCH (INT) kStyle
	CASE GBSREADONLY
		PCALL(gpfnCntStyleSet, hwnd, CTS_READONLY)

	CASE GBSEDIT
		PCALL(gpfnCntStyleClear, hwnd, CTS_READONLY)

	CASE GBSCONTROL3D
		PCALL(gpfnCntAttribSet, hwnd, CA_TITLE3D)
		PCALL(gpfnCntAttribSet, hwnd, CA_FLDTTL3D)
		lUse3dLook := TRUE
		IF lUseDefCaption
			PCALL(gpfnCntColorSet, hwnd, CNTCOLOR_TITLE, GetSysColor(COLOR_BTNTEXT))
		ENDIF
		IF lUseDefColCaption
			PCALL(gpfnCntColorSet, hwnd, CNTCOLOR_FLDTITLES, GetSysColor(COLOR_BTNTEXT))
		ENDIF
		IF (oBackgroundCaption == NULL_OBJECT)
			PCALL(gpfnCntColorSet, hwnd, CNTCOLOR_TTLBKGD, GetSysColor(COLOR_BTNFACE))
		ENDIF
		IF (oBackgroundColCaption == NULL_OBJECT)
			PCALL(gpfnCntColorSet, hwnd, CNTCOLOR_FLDTTLBKGD, GetSysColor(COLOR_BTNFACE))
		ENDIF

	CASE GBSCONTROL2D
		PCALL(gpfnCntAttribClear, hwnd, CA_TITLE3D)
		PCALL(gpfnCntAttribClear, hwnd, CA_FLDTTL3D)
		lUse3dLook := FALSE
		IF lUseDefCaption
			PCALL(gpfnCntColorSet, hwnd, CNTCOLOR_TITLE, GetSysColor(COLOR_CAPTIONTEXT))
		ENDIF
		IF lUseDefColCaption
			PCALL(gpfnCntColorSet, hwnd, CNTCOLOR_FLDTITLES, GetSysColor(COLOR_CAPTIONTEXT))
		ENDIF
		IF oBackgroundCaption == NULL_OBJECT
			PCALL(gpfnCntColorSet, hwnd, CNTCOLOR_TTLBKGD, GetSysColor(COLOR_ACTIVECAPTION))
		ENDIF
		IF oBackgroundColCaption == NULL_OBJECT
			PCALL(gpfnCntColorSet, hwnd, CNTCOLOR_FLDTTLBKGD, GetSysColor(COLOR_ACTIVECAPTION))
		ENDIF
	END SWITCH

	SELF:RestoreUpdate()

	RETURN SELF

METHOD Show() 
	

	IF dwDeferredStyle != 0
		PCALL(gpfnCntStyleSet, hwnd, dwDeferredStyle)
		dwDeferredStyle := 0
	ENDIF

	IF oDataServer != NULL_OBJECT
		IF !lIsShown
			SELF:__BuildBuffer()
		ENDIF
	ENDIF
	lIsShown := TRUE

	SUPER:Show()
	SELF:SetFocus()

	RETURN SELF

METHOD SuspendUpdate() 
	

	IF iDeferPaintCount == 0
		PCALL(gpfnCntDeferPaint, hwnd)
	ENDIF
	iDeferPaintCount := iDeferPaintCount + 1

	RETURN NIL

ACCESS TextColor 
	

	RETURN oTextColor

ASSIGN TextColor(oColor) 
	

	SELF:ChangeTextColor(oColor, gblText)

	RETURN 

METHOD Undo() 
	

	IF IsInstanceOf(oCellEdit,#Edit)
		Send(oCellEdit,#Undo)
	ENDIF

	RETURN SELF

METHOD Use(oServer) 
	

	IF !IsNil(oServer)
		IF !IsInstanceOfUsual(oServer,#DataServer)
			WCError{#Use,#DataBrowser,__WCSTypeError,oServer,1}:@@Throw()
		ENDIF
		IF oDataServer != oServer
			IF oDataServer != NULL_OBJECT
				SELF:__Unlink()
			ENDIF
			oDataServer := oServer
			SELF:__RegisterFieldLinks(oServer)
			ASend(aColumn, #__Scatter)
			IF lLinked
				SELF:SuspendUpdate()
				SELF:__ClearBuffers()
				SELF:__BuildRecordDescription()
				IF lIsShown
					SELF:__BuildBuffer()
				ENDIF
				SELF:__NewFocusField(PCALL(gpfnCntFldHeadGet, hwnd))

				SELF:RestoreUpdate()

				nOldRecordNum := oServer:Recno
			ENDIF
		ENDIF
	ELSE
		SELF:__Unlink()
	ENDIF

	RETURN lLinked

METHOD Validate() 
	

	IF IsInstanceOf(SELF:Owner, #DataWindow)
		RETURN Send(SELF:Owner, #__CheckRecordStatus)
	ENDIF


	/*
	 IF IsInstanceOf(oParent, #__FormFrame)
	 oWindow:=oParent:Owner
	 if IsInstanceOfUsual(oWindow, #DataWindow)
	 return Send(oWindow,#__CheckRecordStatus)
	 endif
	 endif
	*/
	RETURN FALSE

	// DataColumn
END CLASS

CLASS DataColumn INHERIT VObject
	PROTECT strucFI AS _WinFieldInfo
	PROTECT strucSelf AS SelfPtr
	PROTECT iTtlBkgdLoc AS INT
	PROTECT iTxtBkgdLoc AS INT
	PROTECT iBtnBkgdLoc AS INT
	PROTECT iTtlClrLoc AS INT
	PROTECT iTxtClrLoc AS INT
	PROTECT iBtnClrLoc AS INT
	PROTECT iHorzAlignment AS INT
	PROTECT iFontLoc AS INT
	PROTECT iDataField AS INT

	PROTECT lModified AS LOGIC
	PROTECT lDefaultWidth AS LOGIC
	PROTECT lUsingDrawProc AS LOGIC
	PROTECT lBaseServer AS LOGIC
	PROTECT lExplicitFS AS LOGIC
	PROTECT lExplicitHL AS LOGIC
	PROTECT lChanged AS LOGIC

	PROTECT cPicture AS STRING
	PROTECT cCaption AS STRING
	PROTECT cTextValue AS STRING

	PROTECT symUserDrawMethod AS SYMBOL
	PROTECT symDataField AS SYMBOL

	PROTECT oTtlBkgdBrsh AS Brush
	PROTECT oTxtBkgdBrsh AS Brush
	PROTECT oBtnBkgdBrsh AS Brush
	PROTECT oTtlClr AS Color
	PROTECT oTxtClr AS Color
	PROTECT oBtnClr AS Color
	PROTECT oFont AS Font
	PROTECT oParent AS DataBrowser
	PROTECT oFieldSpec AS FieldSpec
	PROTECT oDataField AS DataField
	PROTECT oCellTextColor AS Color
	PROTECT oCellBackground AS Brush
	PROTECT oServer AS DataServer
	PROTECT oHyperLabel AS HyperLabel
	PROTECT oHlStatus AS HyperLabel

	PROTECT cbGetSetBlock AS USUAL
	PROTECT uGetSetOwner AS USUAL
	PROTECT uValue AS USUAL

	#ifdef __VULCAN__
   HIDDEN DrawFldDataDelegate AS __DrawFldDataDelegate
	#endif

	//PP-030828 Strong typing
	ACCESS __BtnBkgdBrsh AS Brush STRICT 
	//PP-030828 Strong typing
	

	RETURN oBtnBkgdBrsh

ACCESS __BtnBkgdLoc AS INT STRICT 
	//PP-030828 Strong typing
	

	RETURN iBtnBkgdLoc

ACCESS __BtnClr AS Color STRICT 
	//PP-030828 Strong typing
	

	RETURN oBtnClr

ACCESS __BtnClrLoc AS INT STRICT 
	//PP-030828 Strong typing
	

	RETURN iBtnClrLoc

ACCESS __DataField AS DataField STRICT 
	//PP-030828 Strong typing
	

	RETURN oDataField

METHOD __DrawCellData(hDC AS PTR, iX AS INT, iY AS INT, dwOptions AS DWORD, ptrRect AS PTR, ;
	pszData AS PSZ, dwLength AS DWORD) AS VOID STRICT 
	LOCAL uValue AS USUAL
	LOCAL hBackgroundBrush AS PTR
	LOCAL ptrLogBrush IS _WINLOGBRUSH
	LOCAL dwOldCellTextColor, dwOldCellBackground AS DWORD
	LOCAL lRestoreTextColor, lRestoreBackground AS LOGIC

	

	IF (oFieldSpec != NULL_OBJECT)
		uValue := oFieldSpec:Val(pszData)
	ELSE
		uValue := Send(oServer:FieldSpec(SELF:NameSym), #Val, Psz2String(pszData))
	ENDIF

	IF (symUserDrawMethod != NULL_SYMBOL)
		Send(SELF, symUserDrawMethod, uValue)
	ELSE
		SELF:DrawCellData(uValue)
	ENDIF

	IF (oCellTextColor != NULL_OBJECT)
		dwOldCellTextColor := SetTextColor(hDC, oCellTextColor:ColorRef)
		lRestoreTextColor := TRUE
	ENDIF

	IF (oCellBackground != NULL_OBJECT)
		hBackgroundBrush := oCellBackground:Handle()
		GetObject(hBackgroundBrush, _SIZEOF(_WINLOGBRUSH), @ptrLogBrush)
		dwOldCellBackground := SetBkColor(hDC, ptrLogBrush:lbColor)
		lRestoreBackground := TRUE
		IF (hBackgroundBrush != NULL_PTR)
			FillRect(hDC, ptrRect, hBackgroundBrush)
		ENDIF
	ENDIF

	ExtTextOut(hDC, iX, iY, dwOptions, ptrRect, pszData, dwLength, NULL_PTR)

	IF lRestoreTextColor
		SetTextColor(hDC, dwOldCellTextColor)
	ENDIF
	IF lRestoreBackground
		SetBkColor(hDC, dwOldCellBackground)
	ENDIF

	oCellTextColor := NULL_OBJECT
	oCellBackground := NULL_OBJECT
	RETURN


ACCESS __FieldInfo AS _WINFieldInfo STRICT 
	//PP-030828 Strong typing
	

	RETURN strucFI

ACCESS __Font AS Font STRICT 
	//PP-030828 Strong typing
	

	RETURN oFont

ACCESS __FontLoc AS INT STRICT 
	//PP-030828 Strong typing
	

	RETURN iFontLoc

METHOD __Gather() AS DataColumn STRICT 
	//PP-030828 Strong typing
	LOCAL oHL AS HyperLabel
	LOCAL oWin AS window

	

	SELF:__Update()

	IF lChanged
		IF (oServer != NULL_OBJECT)
			IF lBaseServer // if not subclassing
				oServer:FIELDPUT(iDataField,SELF:Value) //use FieldPut
			ELSEIF symDataField!=NULL_SYMBOL
				IVarPut(oServer, symDataField ,SELF:Value)
			ELSE
				IVarPut(oServer, SELF:NameSym ,SELF:Value)
			ENDIF
			oHL := oServer:Status
			IF (oHL != NULL_OBJECT) .AND. (oParent != NULL_OBJECT)
				oWin := oParent:Owner
				IF IsInstanceOf(oWin,#AppWindow)
					oWin:@@StatusMessage((ResourceString{__WCSError2}:value)+oHL:Description,MESSAGEERROR)
				ENDIF
			ENDIF
		ELSEIF !IsNil(cbGetSetBlock) .AND. IsCodeBlock(cbGetSetBlock)
			Eval(cbGetSetBlock, uGetSetOwner, SELF:Value)
		ENDIF
	ENDIF

	RETURN SELF

ACCESS __HorzAlignment AS INT STRICT 
	//PP-030828 Strong typing
	

	RETURN iHorzAlignment

ACCESS __Offset AS DWORD STRICT 
	//PP-030828 Strong typing
	

	RETURN strucFI:wOffStruct

ASSIGN __Owner(oDB AS DataBrowser)  STRICT 
	//PP-030828 Strong typing
	

	IF oDB!=NULL_OBJECT
		IF oParent==NULL_OBJECT
			oParent:=oDB
		ENDIF
	ELSE
		oParent:=NULL_OBJECT
	ENDIF
	RETURN


ACCESS __Picture AS STRING STRICT 
	//PP-030828 Strong typing
	

	RETURN cPicture

METHOD __Scatter() AS VOID STRICT 
	//PP-030828 Strong typing
	

	IF IsInstanceOf(oServer, #DataServer)
		IF lBaseServer // if not subclassing
			SELF:Value := oServer:FIELDGET(symDataField) //use fieldget
		ELSEIF symDataField != NULL_SYMBOL
			SELF:Value:=IVarGet(oServer, symDataField)
		ELSE
			SELF:Value:=IVarGet(oServer, SELF:NameSym)
		ENDIF
	ELSEIF !IsNil(cbGetSetBlock) .AND. IsCodeBlock(cbGetSetBlock)
		SELF:Value := Eval(cbGetSetBlock, uGetSetOwner)
	ENDIF
	RETURN


METHOD __SetFldColor(oDataBrowser AS DataBrowser, iLoc AS INT, dwClr AS DWORD) AS VOID STRICT 
	//PP-030828 Strong typing
	

	IF oDataBrowser!=NULL_OBJECT // set color for specified Browse Control
		PCALL(gpfnCntFldColorSet, oDataBrowser:Handle(), strucFI, DWORD(iLoc), dwClr)
	ELSEIF oParent!=NULL_OBJECT // set color for Browser
		PCALL(gpfnCntFldColorSet, oParent:Handle(), strucFI, DWORD(iLoc), dwClr)
	ENDIF
	RETURN

ACCESS __Size AS DWORD STRICT 
	//PP-030828 Strong typing
	

	RETURN strucFI:wDataBytes

	/*assign __Size(dw) class DataColumn
	 

	return dwSize:=dw */

ACCESS __TtlBkgdBrsh AS Brush STRICT 
	//PP-030828 Strong typing
	

	RETURN oTtlBkgdBrsh

ACCESS __TtlBkgdLoc AS INT STRICT 
	//PP-030828 Strong typing
	

	RETURN iTtlBkgdLoc

ACCESS __TtlClr AS Color STRICT 
	//PP-030828 Strong typing
	

	RETURN oTtlClr

ACCESS __TtlClrLoc AS INT STRICT 
	//PP-030828 Strong typing
	

	RETURN iTtlClrLoc

ACCESS __TxtBkgdBrsh AS Brush STRICT 
	//PP-030828 Strong typing
	

	RETURN oTxtBkgdBrsh

ACCESS __TxtBkgdLoc AS INT STRICT 
	//PP-030828 Strong typing
	

	RETURN iTxtBkgdLoc

ACCESS __TxtClr AS Color STRICT 
	//PP-030828 Strong typing
	

	RETURN oTxtClr

ACCESS __TxtClrLoc AS INT STRICT 
	//PP-030828 Strong typing
	

	RETURN iTxtClrLoc

ACCESS __Type AS DWORD STRICT 
	//PP-030828 Strong typing
	

	RETURN strucFI:wColType

	/*assign __Type(dw) class DataColumn
	 

	return dwType:=dw */

METHOD __UnLink(oDS := NIL AS USUAL) AS VOID STRICT 
	//PP-030828 Strong typing
	

	// Do actual unlinking
	IF IsNil(oDS)
		uGetSetOwner := NIL
		cbGetSetBlock := NIL
		oDataField:= NULL_OBJECT
		oServer:= NULL_OBJECT
	ELSE
		IF (oDS == oServer)
			oDataField:= NULL_OBJECT
			oServer:= NULL_OBJECT
		ENDIF
	ENDIF
	RETURN


METHOD __Update() AS VOID STRICT 
	// force update to container
	LOCAL cText AS STRING
	LOCAL uOldValue AS USUAL

	

	IF lModified
		cText := SELF:TextValue
		uOldValue := uValue
		IF (oFieldSpec != NULL_OBJECT)
			uValue := oFieldSpec:Val(cText)
			SELF:TextValue := oFieldSpec:Transform(uValue)
		ELSE
			uValue := cText
		ENDIF
		SELF:Modified := FALSE

		IF !(uOldValue == uValue) // dont change to !=, might be STRING !!!
			SELF:lChanged := TRUE
		ENDIF
	ENDIF
	RETURN


ACCESS Alignment 
	

	RETURN strucFI:flFDataAlign

ASSIGN Alignment (nNewAlign) 
	LOCAL iAlign AS INT

	

	IF !IsLong(nNewAlign)
		WCError{#Alignment,#DataColumn,__WCSTypeError,nNewAlign,1}:@@Throw()
	ENDIF

	iAlign := nNewAlign

	DO CASE
	CASE iAlign==gbaAlignCenter
		iHorzAlignment := CA_TA_HCENTER
	CASE iAlign==gbaAlignLeft
		iHorzAlignment := CA_TA_LEFT
	CASE iAlign==gbaAlignRight
		iHorzAlignment := CA_TA_RIGHT
	ENDCASE

	IF oParent!=NULL_OBJECT
		PCALL(gpfnCntFldDataAlnSet, oParent:Handle(), strucFI, DWORD(iHorzAlignment))
	ENDIF

	RETURN 

METHOD AsString(uParam) 
	LOCAL cString AS STRING
	LOCAL uVal2Print AS USUAL
	LOCAL lHasPic AS LOGIC

	

	//RvdH 060608 optimized
	//lHasPic := (oFieldSpec != NULL_OBJECT) .and. !Empty(oFieldSpec:Picture)
	lHasPic := (oFieldSpec != NULL_OBJECT) .AND. SLen(oFieldSpec:Picture) > 0

	IF IsNil(uParam)
		uVal2Print := uValue
	ELSE
		uVal2Print := uParam
	ENDIF

	IF IsNil(uVal2Print)
		RETURN NULL_STRING
	ENDIF

	IF lHasPic
		cString := oFieldSpec:Transform(uVal2Print)
	ELSEIF (oFieldSpec != NULL_OBJECT) .AND. (oFieldSpec:ValType == "N")
		cString := Str(uVal2Print, oFieldSpec:Length, oFieldSpec:Decimals)
	ELSE
		cString := _AsString(uVal2Print)
	ENDIF

	RETURN cString

ACCESS Background 
	

	RETURN oTxtBkgdBrsh

ASSIGN Background(oBrush) 
	

	SELF:ChangeBackground(oBrush, gblText)

	RETURN 

ACCESS Block 
	

	RETURN cbGetSetBlock

ASSIGN Block(aCb) 
	

	IF !Empty(aCB) .AND. IsCodeBlock(aCB)
		cbGetSetBlock := aCb

		// reset data server connection
		oDataField:= NULL_OBJECT
		oServer:= NULL_OBJECT
		iDataField := 0
	ELSE
		cbGetSetBlock := NULL_CODEBLOCK
	ENDIF

	RETURN 

ACCESS BlockOwner 
	

	RETURN uGetSetOwner

ASSIGN BlockOwner(xOwner) 
	

	IF !IsNil(xOwner)
		uGetSetOwner := xOwner
	ELSE
		uGetSetOwner := NIL
	ENDIF

	RETURN 

ACCESS Caption 
	

	RETURN cCaption

ASSIGN Caption(cNewCaption) 
	

	IF IsSymbol(cNewCaption)
		SELF:SetCaption(Symbol2String(cNewCaption))
	ELSEIF IsString(cNewCaption)
		SELF:SetCaption(cNewCaption)
	ELSE
		SELF:SetCaption()
	ENDIF

	RETURN 

ACCESS CellBackground 
	

	RETURN oCellBackground

ASSIGN CellBackground(oBrush) 
	

	RETURN oCellBackground := oBrush

ACCESS CellTextColor 
	

	RETURN oCellTextColor

ASSIGN CellTextColor(oColor) 
	

	RETURN oCellTextColor := oColor

METHOD ChangeBackground(oBrush, kWhere) 
	LOCAL oOldBrush AS Brush
	LOCAL iLoc AS INT

	

	IF !IsInstanceOfUsual(oBrush,#Brush)
		WCError{#ChangeBackground,#DataColumn,__WCSTypeError,oBrush,1}:@@Throw()
	ENDIF
	IF !IsNil(kWhere)
		IF !IsLong(kWhere)
			WCError{#ChangeBackground,#DataColumn,__WCSTypeError,kWhere,1}:@@Throw()
		ENDIF
	ENDIF

	DO CASE
	CASE kWhere==gblCaption .OR.;
		kWhere==gblColCaption
		oOldBrush := oTtlBkgdBrsh
		oTtlBkgdBrsh := oBrush
		iTtlBkgdLoc := iLoc := CNTCOLOR_FLDTTLBKGD

	CASE kWhere==gblText
		oOldBrush := oTxtBkgdBrsh
		oTxtBkgdBrsh := oBrush
		iTxtBkgdLoc := iLoc := CNTCOLOR_FLDBKGD

	CASE kWhere==gblButton .OR.;
		kWhere==gblColButton
		oOldBrush := oBtnBkgdBrsh
		oBtnBkgdBrsh := oBrush
		iBtnBkgdLoc := iLoc := CNTCOLOR_FLDBTNBKGD
	ENDCASE
	SELF:__SetFldColor(NULL_OBJECT, iLoc, __WCGetBrushColor(oBrush))

	RETURN oOldBrush

METHOD ChangeTextColor(oColor, kWhere) 
	LOCAL oOldColor AS Color
	LOCAL iLoc AS INT
	LOCAL oNewColor AS color

	

	IF IsNumeric(oColor)
		oNewColor := Color{oColor}
	ELSEIF IsInstanceOfUsual(oColor,#Color)
		oNewColor := oColor
	ELSE
		WCError{#ChangeTextColor,#DataColumn,__WCSTypeError,oColor,1}:@@Throw()
	ENDIF
	IF !IsNil(kWhere)
		IF !IsLong(kWhere)
			WCError{#ChangeTextColor,#DataColumn,__WCSTypeError,kWhere,2}:@@Throw()
		ENDIF
	ENDIF

	DO CASE
	CASE kWhere==gblCaption .OR.;
		kWhere==gblColCaption
		oOldColor := oTtlClr
		oTtlClr := oNewColor
		iTtlClrLoc := iLoc := CNTCOLOR_FLDTITLES

	CASE kWhere==gblText
		oOldColor := oTxtClr
		oTxtClr := oNewColor
		iTxtClrLoc := iLoc := CNTCOLOR_TEXT

	CASE kWhere==gblButton .OR.;
		kWhere==gblColButton
		oOldColor := oBtnClr
		oBtnClr := oNewColor
		iBtnClrLoc := iLoc := CNTCOLOR_FLDBTNTXT
	ENDCASE
	SELF:__SetFldColor(NULL_OBJECT, iLoc, oNewColor:ColorRef)

	RETURN oOldColor

METHOD ClearStatus() 
	// DHer: 18/12/2008
	SELF:oHlStatus := NULL_OBJECT

RETURN NIL

ACCESS DataField 
	// DHer: 18/12/2008
RETURN SELF:symDataField

METHOD Destroy() 
	
    __WcSelfPtrFree(strucSelf)
	IF !InCollect()
        strucSelf := NULL_PTR
		UnregisterAxit(SELF)
		cPicture := NULL_STRING
		cCaption := NULL_STRING
		strucFI := NULL_PTR
		oTtlBkgdBrsh := NULL_OBJECT
		oTxtBkgdBrsh := NULL_OBJECT
		oBtnBkgdBrsh := NULL_OBJECT
		oTtlClr := NULL_OBJECT
		oTxtClr := NULL_OBJECT
		oBtnClr := NULL_OBJECT
		oFont := NULL_OBJECT
	ENDIF
	SUPER:Destroy()

	RETURN SELF

METHOD DisableCellDraw() 
	

	PCALL(gpfnCntFldAttrClear, strucFI, CFA_OWNERDRAW)
	lUsingDrawProc:=FALSE

	RETURN SELF

METHOD DrawCellData(uValue) 
	

	RETURN SELF

METHOD EnableCellDraw(symMethodName) 
	

	IF !IsNil(symMethodName)
		IF !IsSymbol(symMethodName)
			WCError{#EnableCellDraw,#DataColumn,__WCSTypeError,symMethodName,1}:@@Throw()
		ENDIF
		symUserDrawMethod := symMethodName
	ELSE
		symUserDrawMethod := NULL_SYMBOL
	ENDIF

	IF !lUsingDrawProc
		lUsingDrawProc := TRUE
		PCALL(gpfnCntFldAttrSet, strucFI, CFA_OWNERDRAW)
#ifdef __VULCAN__
      IF DrawFldDataDelegate == NULL
         DrawFldDataDelegate := __DrawFldDataDelegate{ NULL, @__DrawFldData() }
      ENDIF   

		PCALL(gpfnCntFldDrwProcSet, strucFI, System.Runtime.InteropServices.Marshal.GetFunctionPointerForDelegate( (System.Delegate) DrawFldDataDelegate ) )
#else		
		PCALL(gpfnCntFldDrwProcSet, strucFI, @__DrawFldData())
#endif		
	ENDIF

	IF (oParent != NULL_OBJECT)
		oParent:Refresh()
	ENDIF

	RETURN SELF

METHOD EnableColumnMove(lAllowMove) 
	

	DEFAULT(@lAllowMove, TRUE)
	IF !IsLogic(lAllowMove)
		WCError{#EnableColumnMove,#DataColumn,__WCSTypeError,lAllowMove,1}:@@Throw()
	ENDIF

	IF lAllowMove
		PCALL(gpfnCntFldAttrClear, strucFI, CFA_NONMOVEABLEFLD)
		PCALL(gpfnCntFldAttrSet, strucFI, CFA_MOVEABLEFLD)
	ELSE
		PCALL(gpfnCntFldAttrClear, strucFI, CFA_MOVEABLEFLD)
		PCALL(gpfnCntFldAttrSet, strucFI, CFA_NONMOVEABLEFLD)
	ENDIF

	RETURN SELF

METHOD EnableColumnReSize(lAllowResize) 
	

	DEFAULT(@lAllowResize, TRUE)

	IF !IsLogic(lAllowResize)
		WCError{#EnableColumnReSize,#DataColumn,__WCSTypeError,lAllowResize,1}:@@Throw()
	ENDIF

	IF lAllowResize
		PCALL(gpfnCntFldAttrClear, strucFI, CFA_NONSIZEABLEFLD)
		PCALL(gpfnCntFldAttrSet, strucFI, CFA_SIZEABLEFLD)
	ELSE
		PCALL(gpfnCntFldAttrClear, strucFI, CFA_SIZEABLEFLD)
		PCALL(gpfnCntFldAttrSet, strucFI, CFA_NONSIZEABLEFLD)
	ENDIF

	RETURN SELF

ACCESS FieldSpec 
	

	RETURN oFieldSpec

ASSIGN FieldSpec(oFS) 
	

	IF !IsInstanceOfUsual(oFS,#FieldSpec)
		WCError{#FieldSpec,#DataColumn,__WCSTypeError,oFS,1}:@@Throw()
	ENDIF

	oFieldSpec := oFS
	lExplicitFS := TRUE
	cPicture := oFieldSpec:Picture
	// nType := oFieldSpec:UsualType
	// DO CASE
	// CASE nType == FLOAT
	// iPictureType := PICTYPE_NUMERIC
	// CASE nType == DATE
	// cPicture := "@D"
	// iPictureType := PICTYPE_DATE
	// ENDCASE

	IF oFieldSpec:ValType == "N"
		IF SubStr(cPicture, 1, 2) == "@B"
			SELF:Alignment := GBAALIGNLEFT
		ELSE
			SELF:Alignment := GBAALIGNRIGHT
		ENDIF
	ENDIF

	// We need to update the column if is visible and
	// connected to the server

	IF oServer!=NULL_OBJECT .AND. oDataField!=NULL_OBJECT .OR. !IsNil(cbGetSetBlock)
		SELF:__Scatter()
	ENDIF

	RETURN 

METHOD GetCaption() 
	

	RETURN cCaption

METHOD GetEditObject(oOwner, iID, oPoint, oDim) 
	LOCAL oControl AS TextControl

	

	oControl := SingleLineEdit{oOwner, iID, oPoint, oDim, ES_AUTOHSCROLL}
	IF (oFieldSpec != NULL_OBJECT)
		oControl:FieldSpec := oFieldSpec
	ENDIF
	IVarPut(oControl, #Overwrite, OVERWRITE_ONKEY)

	oControl:SetExStyle(WS_EX_CLIENTEDGE, FALSE)
	oControl:Font(oOwner:EditFont, FALSE)
	oControl:TextValue := RTrim(SELF:TextValue)
	//SendMessage(oControl:Handle(), EM_SETSEL, 0, -1)

	RETURN oControl

METHOD GetModified() 
	

	RETURN lModified

METHOD GetValue() 
	

	RETURN cTextValue

ACCESS HyperLabel 
	

	RETURN oHyperLabel

ASSIGN HyperLabel(oNewHL) 
	

	IF IsInstanceOfUsual(oNewHL, #HyperLabel)
		oHyperLabel := oNewHL
		lExplicitHL := TRUE
		SELF:Caption := oNewHL:Caption
	ELSEIF IsString(oNewHL)
		oHyperLabel := HyperLabel{String2Symbol(oNewHL)}
		lExplicitHL := TRUE
		SELF:Caption := oNewHL:Caption
	ELSEIF IsSymbol(oNewHL)
		oHyperLabel := HyperLabel{oNewHL}
		lExplicitHL := TRUE
		SELF:Caption := oNewHL:Caption
	ELSEIF IsNil(oNewHL)
		oHyperLabel := NULL_OBJECT
		lExplicitHL := FALSE
		// Should we reset the caption ??
	ELSE
		WCError{#HyperLabel,#DataColumn,__WCSTypeError,oNewHL,1}:@@Throw()
	ENDIF

	RETURN 

CONSTRUCTOR(nWidth, xColumnID) 
	LOCAL p AS SelfPtr
	LOCAL iSize AS INT

	

	IF IsNil(nWidth)
		iSize := 16
	ELSEIF IsInstanceOfUsual(nWidth, #FieldSpec)
		iSize := __GetFSDefaultLength(nWidth)
		SELF:FieldSpec := nWidth
	ELSEIF !IsLong(nWidth)
		WCError{#Init,#DataColumn,__WCSTypeError,nWidth,1}:@@Throw()
	ELSE
		iSize := nWidth
	ENDIF

	SUPER()
	

	strucFI := PCALL(gpfnCntNewFldInfo) // Alloc a new field struct

	cTextValue := "N/A"

	IF IsInstanceOfUsual(xColumnID, #HyperLabel)
		SELF:HyperLabel := xColumnID
	ELSEIF IsString(xColumnID)
		SELF:HyperLabel := HyperLabel{String2Symbol(xColumnID),xColumnID}
	ELSEIF IsSymbol(xColumnID)
		SELF:HyperLabel := HyperLabel{xColumnID,Symbol2String(xColumnID)}
	ENDIF
    p := __WCSelfPtrAlloc(SELF)
#ifdef __VULCAN__
	// the following line is correct, since CntFldUserSet copies 4 bytes
	// from @p into its own buffer
	PCallNative<LOGIC>(gpfnCntFldUserSet, strucFI, @p, 4)
#else	
	// the following line is correct, since CntFldUserSet copies 4 bytes
	// from @p into its own buffer
	PCALL(gpfnCntFldUserSet, strucFI, @p, 4)
#endif
	StrucSelf := p

	strucFI:wFTitleLines := 1 // nbr of lines in field title
	strucFI:flFTitleAlign := _OR(CA_TA_HCENTER, CA_TA_VCENTER)

	// Set up default width
	lDefaultWidth := TRUE
	IF iSize != -1
		strucFI:cxWidth := DWORD(iSize + 2)
	ELSE
		strucFI:cxWidth := 0xFFFFFFFF
	ENDIF

	strucFI:flFDataAlign := _OR(CA_TA_LEFT, CA_TA_VCENTER)
	strucFI:wColType := CFT_STRING
	strucFI:wOffStruct := 2
	strucFI:wDataBytes := DWORD(iSize + 1)

	// variables for ChangeBackground
	iTtlBkgdLoc := -1
	iTxtBkgdLoc := -1
	iBtnBkgdLoc := -1

	// variables for ChangeTextColor
	iTtlClrLoc := -1
	iTxtClrLoc := -1
	iBtnClrLoc := -1

	// variables for ChangeFont
	iFontLoc := -1

	RETURN 

METHOD LinkDF(oDataServer, nFieldData) 
	LOCAL tmpDF AS OBJECT
	LOCAL symClassName AS SYMBOL

	

	IF !IsInstanceOfUsual(oDataServer,#DataServer)
		WCError{#LinkDF,#DataColumn,__WCSTypeError,oDataServer,1}:@@Throw()
	ENDIF

	IF !IsNil(oServer) .AND. oDataServer!=oServer
		SELF:__Unlink()
	ENDIF

	oServer := oDataServer
	iDataField := nFieldData
	symDataField := oServer:FieldSym(iDataField)
	IF IsMethod(oServer, #IsBaseField)
       lBaseServer := Send(oServer, #IsBaseField, symDataField)
    ELSE   
    	symClassName := ClassName(oServer)
	    lBaseServer  := symClassName==#DBServer .OR. symClassName==#SQLSelect .OR. symClassName==#SQLTable .OR. symClassName==#JDataServer
	ENDIF

	// Propogate data field if no explicit one
	tmpDF:=oServer:DataField(iDataField)
	IF IsInstanceOfUsual(tmpDF, #DataField)
		oDataField := tmpDF

		IF !lExplicitFS
			// propogate field spec if no explicit one
			oFieldSpec := oDataField:FieldSpec

			// propogate field spec
			IF !lExplicitHL
				// CHECK IF NameSym and hyperlabel are same
				IF !IsNil(oDataField:HyperLabel) .AND. (oDataField:NameSym == oDataField:HyperLabel:NameSym)
					oHyperLabel := oDataField:HyperLabel
				ELSE
					IF cCaption==NULL_STRING
						oHyperLabel := HyperLabel {oDataField:NameSym}
					ELSE
						oHyperLabel := HyperLabel {oDataField:NameSym,cCaption}
					ENDIF
				ENDIF
			ENDIF
		ENDIF
	ENDIF

	uGetSetOwner:= NIL
	cbGetSetBlock:= NIL

	// Get initial value
	SELF:__Scatter()

	RETURN NIL

ACCESS Modified 
	

	RETURN lModified

ASSIGN Modified(lChangedFlag) 
	

	IF IsLogic(lChangedFlag)
		lModified := lChangedFlag
	ENDIF

	RETURN 

ACCESS Name 
	

	IF oHyperLabel!=NULL_OBJECT
		RETURN oHyperLabel:Name
	ENDIF

	RETURN NULL_STRING

ACCESS NameSym 
	

	IF oHyperLabel!=NULL_OBJECT
		RETURN oHyperLabel:NameSym
	ENDIF

	RETURN NULL_SYMBOL

ACCESS Owner 
	

	RETURN oParent

METHOD PerformValidations() 
	// Perform validations for DataColumn against supplied parameter
	// if it has a data spec, otherwise just return TRUE
	

	IF (oFieldSpec != NULL_OBJECT)
		IF !oFieldSpec:PerformValidations(uValue,SELF)
			oHLStatus := oFieldSpec:Status
			RETURN FALSE
		ENDIF
	ENDIF
	oHLStatus:= NULL_OBJECT

	RETURN TRUE

ACCESS PixelWidth 
	// DHer: 18/12/2008
RETURN SELF:strucFI:cxPxlWidth

ACCESS Server 
	

	RETURN oServer

METHOD SetCaption(cText, kAlignment) 
	LOCAL iAlign AS INT
	LOCAL iLines AS INT
	LOCAL i AS INT
	LOCAL iMaxWidth AS INT
	LOCAL cTemp AS STRING

	

	IF !IsNil(cText)
		IF !IsString(cText)
			WCError{#SetCaption,#DataColumn,__WCSTypeError,cText,1}:@@Throw()
		ENDIF
	ENDIF
	IF !IsNil(kAlignment)
		IF !IsLong(kAlignment)
			WCError{#SetCaption,#DataColumn,__WCSTypeError,kAlignment,2}:@@Throw()
		ENDIF
	ELSE
		kAlignment:=gbaAlignCenter
	ENDIF

	SWITCH (INT) kAlignment
	CASE GBAALIGNCENTER
		iAlign := CA_TA_HCENTER
	CASE GBAALIGNLEFT
		iAlign := CA_TA_LEFT
	CASE GBAALIGNRIGHT
		iAlign := CA_TA_RIGHT
	END SWITCH

	// calculate width based on caption
	IF (NULL_STRING != cText)
		iLines := INT(Occurs(_CHR(10), cText)) + 1
		IF (iLines > 1) //If multiple lines
			cTemp := cText //Find the longest line
			DO WHILE TRUE
				i := INT(_CAST, At(_CHR(10), cTemp))
				IF (i > 0)
					iMaxWidth := Max(i-1, iMaxWidth)
					cTemp := SubStr(cTemp, i+1)
				ELSE
					iMaxWidth := Max(SLen(cTemp), iMaxWidth)
					EXIT
				ENDIF
			ENDDO
		ELSE
			iMaxWidth := INT(_CAST, SLen(cText))
		ENDIF

		strucFI:wFTitleLines := DWORD(iLines)
		IF (oParent != NULL_OBJECT)
			oParent:__SetMaxFTHeight(iLines)
		ENDIF

		// multi line titles are vertically aligned on the Top of the control by default
		IF (ilines > 1)
			strucFI:flFTitleAlign := _OR(iAlign,CA_TA_TOP)
		ELSE
			strucFI:flFTitleAlign := _OR(iAlign,CA_TA_VCENTER)
		ENDIF

		cCaption := cText
		IF (oParent != NULL_OBJECT)
            PCALL(gpfnCntFldTtlSet, oParent:Handle(), strucFI, String2Psz(cCaption), SLen(cCaption)+1)
		ENDIF

		// default width if necessary
		IF lDefaultWidth .AND. (INT(_CAST, strucFI:cxWidth) < (iMaxWidth + 2))
			strucFI:cxWidth := DWORD(iMaxWidth + 2)
		ENDIF
	ELSE
		cCaption := NULL_STRING
		strucFI:wFTitleLines := 0
	ENDIF

	RETURN SELF

METHOD SetModified(lModified) 
	

	IF !IsLogic(lModified)
		WCError{#SetModified,#DataColumn,__WCSTypeError,lModified,1}:@@Throw()
	ENDIF
	SELF:lModified:=lModified

	RETURN SELF

METHOD SetStandardStyle(kStyle) 
	

	IF !IsLong(kStyle)
		WCError{#SetStandardStyle,#DataColumn,__WCSTypeError,kStyle,1}:@@Throw()
	ENDIF

	DO CASE
	CASE (kStyle == GBSREADONLY)
		PCALL(gpfnCntFldAttrSet, strucFI, CFA_FLDREADONLY)

	CASE (kStyle == GBSEDIT)
		PCALL(gpfnCntFldAttrClear, strucFI, CFA_FLDREADONLY)

	CASE (kStyle == GBSCONTROL3D)
		PCALL(gpfnCntFldAttrSet, strucFI, CFA_FLDTTL3D)

	CASE (kStyle == GBSCONTROL2D)
		PCALL(gpfnCntFldAttrClear, strucFI, CA_FLDTTL3D)
	ENDCASE

	RETURN SELF

METHOD SetValue(cNewValue) 
	

	IF !IsString(cNewValue)
		WCError{#SetValue,#DataColumn,__WCSTypeError,cNewValue,1}:@@Throw()
	ENDIF

	IF !(cNewValue == cTextValue)
		//cTextValue := cNewValue
		SELF:TextValue := cNewValue
		SELF:Modified := TRUE
		SELF:__Update()
	ENDIF

	IF SELF:ValueChanged
		IF !SELF:PerformValidations()
			oHLStatus := SELF:Status
			oParent:__FieldChange()
		ELSE
			SELF:__Gather()
		ENDIF
	ENDIF

	RETURN cTextValue

ACCESS Status 
	

	RETURN oHLStatus

ACCESS TextColor 
	

	RETURN oTxtClr

ASSIGN TextColor(oColor) 
	

	SELF:ChangeTextColor(oColor, gblText)

	RETURN 

ACCESS TextValue 
	

	RETURN cTextValue

ASSIGN TextValue(sNewText) 
	

	IF !IsString(sNewText)
		WCError{#TextValue,#DataColumn,__WCSTypeError,sNewText,1}:@@Throw()
	ENDIF

	cTextValue := sNewText

	IF (oFieldSpec != NULL_OBJECT)
		uValue := oFieldSpec:Val(cTextValue)
	ELSE
		uValue := cTextValue
	ENDIF

	RETURN 

ACCESS Value 
	

	RETURN uValue

ASSIGN Value(uParm) 
	LOCAL cTemp AS STRING

	

	IF IsNil(uParm)
		uValue:= NIL
		SELF:TextValue := NULL_STRING
		oHlStatus := NULL_OBJECT
		SELF:Modified := FALSE
		SELF:lChanged := TRUE
	ELSE
		IF (oFieldSpec != NULL_OBJECT)
			cTemp := oFieldSpec:Transform(uParm)
		ELSE
			cTemp := AsString(uParm)
		ENDIF

		uValue := uParm
		SELF:TextValue := cTemp
		oHLStatus := NULL_OBJECT
		SELF:Modified := FALSE
		SELF:lChanged := TRUE
	ENDIF

	RETURN 

ACCESS ValueChanged 
	

	RETURN lChanged

ASSIGN ValueChanged(lNewFlag) 
	

	IF !IsLogic(lNewFlag)
		WCError{#ValueChanged,#DataColumn,__WCSTypeError,lNewFlag,1}:@@Throw()
	ENDIF

	RETURN lChanged := lNewFlag

ACCESS VisualPos 
	LOCAL iCount AS INT
	LOCAL strucFieldInfo AS _WinFieldInfo
	LOCAL p AS SelfPtr

	

	IF (oParent == NULL_OBJECT)
		RETURN 0
	ENDIF

	strucFieldInfo := PCALL(gpfnCntFldHeadGet, oParent:Handle())
	DO WHILE strucFieldInfo != NULL_PTR
		iCount++
	   // Note lpUserData contains a pointer to a buffer of 4 bytes with the self pointer!
		p := strucFieldInfo:lpUserData
		p := p:ptrSelf
	    IF __WCSelfPtr2Object(p) == SELF
			RETURN iCount
		ENDIF
		strucFieldInfo := strucFieldInfo:lpNext
	ENDDO

	RETURN 0

ACCESS Width 
	

	RETURN strucFI:cxWidth

ASSIGN Width(nNewWidth) 
	

	IF !IsLong(nNewWidth)
		WCError{#Width,#DataColumn,__WCSTypeError,nNewWidth,1}:@@Throw()
	ENDIF

	StrucFI:cxWidth := nNewWidth
	lDefaultWidth := FALSE
	IF (oParent != NULL_OBJECT)
		PCALL(gpfnCntFldWidthSet, oParent:Handle(), strucFI, nNewWidth)
	ENDIF

	//if (oParent != NULL_OBJECT)
	// oParent:__BuildRecordDescription()
	//endif

	RETURN 
END CLASS

STATIC GLOBAL glContainerDllLoaded := FALSE AS LOGIC
STATIC GLOBAL ghContainerDLL AS PTR
//function declarations
STATIC GLOBAL gpfnCntAddFldTail AS TCntAddFldTail PTR
STATIC GLOBAL gpfnCntAddRecHead AS TCntAddRecHead PTR
STATIC GLOBAL gpfnCntAddRecTail AS TCntAddRecTail PTR
STATIC GLOBAL gpfnCntAssociateSet AS TCntAssociateSet PTR
STATIC GLOBAL gpfnCntAttribClear AS TCntAttribClear PTR
STATIC GLOBAL gpfnCntAttribSet AS TCntAttribSet PTR
STATIC GLOBAL gpfnCntCNCharGet AS TCntCNCharGet PTR
STATIC GLOBAL gpfnCntCNChildWndGet AS TCntCNChildWndGet PTR
STATIC GLOBAL gpfnCntCNFldGet AS TCntCNFldGet PTR
STATIC GLOBAL gpfnCntCNIncExGet AS TCntCNIncExGet PTR
STATIC GLOBAL gpfnCntCNRecGet AS TCntCNRecGet PTR
STATIC GLOBAL gpfnCntCNShiftKeyGet AS TCntCNShiftKeyGet PTR
STATIC GLOBAL gpfnCntColorGet AS TCntColorGet PTR
STATIC GLOBAL gpfnCntColorSet AS TCntColorSet PTR
STATIC GLOBAL gpfnCntCurrentPosExGet AS TCntCurrentPosExGet PTR
STATIC GLOBAL gpfnCntCurrentPosExSet AS TCntCurrentPosExSet PTR
STATIC GLOBAL gpfnCntCursorSet AS TCntCursorSet PTR
STATIC GLOBAL gpfnCntDeferPaint AS TCntDeferPaint PTR
STATIC GLOBAL gpfnCntDeltaExSet AS TCntDeltaExSet PTR
STATIC GLOBAL gpfnCntDeltaPosExGet AS TCntDeltaPosExGet PTR
STATIC GLOBAL gpfnCntDeltaPosExSet AS TCntDeltaPosExSet PTR
STATIC GLOBAL gpfnCntEndDeferPaint AS TCntEndDeferPaint PTR
STATIC GLOBAL gpfnCntEndRecEdit AS TCntEndRecEdit PTR
STATIC GLOBAL gpfnCntFldAttrClear AS TCntFldAttrClear PTR
STATIC GLOBAL gpfnCntFldAttrSet AS TCntFldAttrSet PTR
STATIC GLOBAL gpfnCntFldColorSet AS TCntFldColorSet PTR
STATIC GLOBAL gpfnCntFldDataAlnSet AS TCntFldDataAlnSet PTR
STATIC GLOBAL gpfnCntFldDrwProcSet AS TCntFldDrwProcSet PTR
STATIC GLOBAL gpfnCntFldHeadGet AS TCntFldHeadGet PTR
STATIC GLOBAL gpfnCntFldTailGet AS TCntFldTailGet PTR
STATIC GLOBAL gpfnCntFldTtlHtSet AS TCntFldTtlHtSet PTR
STATIC GLOBAL gpfnCntFldTtlSepSet AS TCntFldTtlSepSet PTR
STATIC GLOBAL gpfnCntFldTtlSet AS TCntFldTtlSet PTR
STATIC GLOBAL gpfnCntFldUserSet AS TCntFldUserSet PTR
STATIC GLOBAL gpfnCntFldWidthSet AS TCntFldWidthSet PTR
STATIC GLOBAL gpfnCntFocusExtGet AS TCntFocusExtGet PTR
STATIC GLOBAL gpfnCntFocusFldGet AS TCntFocusFldGet PTR
STATIC GLOBAL gpfnCntFocusFldLock AS TCntFocusFldLock PTR
STATIC GLOBAL gpfnCntFocusFldUnlck AS TCntFocusFldUnlck PTR
STATIC GLOBAL gpfnCntFocusMove AS TCntFocusMove PTR
STATIC GLOBAL gpfnCntFocusOrgGet AS TCntFocusOrgGet PTR
STATIC GLOBAL gpfnCntFocusRecGet AS TCntFocusRecGet PTR
STATIC GLOBAL gpfnCntFocusRecLock AS TCntFocusRecLock PTR
STATIC GLOBAL gpfnCntFocusRecUnlck AS TCntFocusRecUnlck PTR
STATIC GLOBAL gpfnCntFocusSet AS TCntFocusSet PTR
STATIC GLOBAL gpfnCntFontSet AS TCntFontSet PTR
STATIC GLOBAL gpfnCntFreeRecCore AS TCntFreeRecCore PTR
STATIC GLOBAL gpfnCntInsFldBefore AS TCntInsFldBefore PTR
STATIC GLOBAL gpfnCntIsFocusCellRO AS TCntIsFocusCellRO PTR
STATIC GLOBAL gpfnCntIsRecSelected AS TCntIsRecSelected PTR
STATIC GLOBAL gpfnCntKillRecList AS TCntKillRecList PTR
STATIC GLOBAL gpfnCntNewFldInfo AS TCntNewFldInfo PTR
STATIC GLOBAL gpfnCntNewRecCore AS TCntNewRecCore PTR
STATIC GLOBAL gpfnCntNextRec AS TCntNextRec PTR
STATIC GLOBAL gpfnCntNotifyAssoc AS TCntNotifyAssoc PTR
STATIC GLOBAL gpfnCntRangeExSet AS TCntRangeExSet PTR
STATIC GLOBAL gpfnCntRecAttrClear AS TCntRecAttrClear PTR
STATIC GLOBAL gpfnCntRecAttrSet AS TCntRecAttrSet PTR
STATIC GLOBAL gpfnCntRecDataSet AS TCntRecDataSet PTR
STATIC GLOBAL gpfnCntRecHeadGet AS TCntRecHeadGet PTR
STATIC GLOBAL gpfnCntRecsDispGet AS TCntRecsDispGet PTR
STATIC GLOBAL gpfnCntRecTailGet AS TCntRecTailGet PTR
STATIC GLOBAL gpfnCntRecUserSet AS TCntRecUserSet PTR
STATIC GLOBAL gpfnCntRemoveFld AS TCntRemoveFld PTR
STATIC GLOBAL gpfnCntRemoveRecHead AS TCntRemoveRecHead PTR
STATIC GLOBAL gpfnCntRemoveRecTail AS TCntRemoveRecTail PTR
STATIC GLOBAL gpfnCntRowHtSet AS TCntRowHtSet PTR
STATIC GLOBAL gpfnCntScrollFldArea AS TCntScrollFldArea PTR
STATIC GLOBAL gpfnCntScrollRecAreaEx AS TCntScrollRecAreaEx PTR
STATIC GLOBAL gpfnCntSelectRec AS TCntSelectRec PTR
STATIC GLOBAL gpfnCntSelRecGet AS TCntSelRecGet PTR
STATIC GLOBAL gpfnCntSpltBarCreate AS TCntSpltBarCreate PTR
STATIC GLOBAL gpfnCntSpltBarDelete AS TCntSpltBarDelete PTR
STATIC GLOBAL gpfnCntStyleClear AS TCntStyleClear PTR
STATIC GLOBAL gpfnCntStyleSet AS TCntStyleSet PTR
STATIC GLOBAL gpfnCntTopRecGet AS TCntTopRecGet PTR
STATIC GLOBAL gpfnCntTopRecSet AS TCntTopRecSet PTR
STATIC GLOBAL gpfnCntTtlAlignSet AS TCntTtlAlignSet PTR
STATIC GLOBAL gpfnCntTtlHtSet AS TCntTtlHtSet PTR
STATIC GLOBAL gpfnCntTtlSepSet AS TCntTtlSepSet PTR
STATIC GLOBAL gpfnCntTtlSet AS TCntTtlSet PTR
STATIC GLOBAL gpfnCntUnSelectRec AS TCntUnSelectRec PTR
STATIC GLOBAL gpfnCntViewSet AS TCntViewSet PTR
STATIC GLOBAL gpfnCntVScrollPosExSet AS TCntVScrollPosExSet PTR
STATIC GLOBAL pfGBChildProcOrg AS PTR

STATIC FUNCTION TCntAddFldHead(hCntWnd AS PTR, lpFld AS _winFIELDINFO) AS LOGIC STRICT
	//SYSTEM
	RETURN FALSE

STATIC FUNCTION TCntAddFldTail(hCntWnd AS PTR, lpFld AS _winFIELDINFO) AS LOGIC STRICT
	//SYSTEM
	RETURN FALSE

STATIC FUNCTION TCntAddRecHead(hCntWnd AS PTR, lpNew AS _winRECORDCORE) AS LOGIC STRICT
	//SYSTEM
	RETURN FALSE

STATIC FUNCTION TCntAddRecTail(hCntWnd AS PTR, lpNew AS _winRECORDCORE) AS LOGIC STRICT
	//SYSTEM
	RETURN FALSE

STATIC FUNCTION TCntAssociateSet(hWnd AS PTR, hWndAssociate AS PTR) AS PTR STRICT
	//SYSTEM
	RETURN NULL_PTR

STATIC FUNCTION TCntAttribClear(hWnd AS PTR, dwAttrib AS DWORD) AS VOID STRICT
RETURN
STATIC FUNCTION TCntAttribSet(hWnd AS PTR, dwAttrib AS DWORD) AS VOID STRICT
RETURN
STATIC FUNCTION TCntCNCharGet(hCntWnd AS PTR, lParam AS LONGINT) AS DWORD STRICT
	//SYSTEM
	RETURN 0

STATIC FUNCTION TCntCNChildWndGet(hCntWnd AS PTR, lParam AS LONGINT) AS PTR STRICT
	//SYSTEM
	RETURN NULL_PTR

STATIC FUNCTION TCntCNFldGet(hCntWnd AS PTR, lParam AS LONGINT) AS _winFIELDINFO STRICT
	//SYSTEM
	RETURN NULL_PTR //PP-040410
STATIC FUNCTION TCntCNIncExGet(hCntWnd AS PTR, lParam AS LONGINT) AS LONGINT STRICT
	//SYSTEM
	RETURN 0

STATIC FUNCTION TCntCNRecGet(hCntWnd AS PTR, lParam AS LONGINT) AS _winRECORDCORE STRICT
	//SYSTEM
	RETURN NULL_PTR //PP-040410

STATIC FUNCTION TCntCNShiftKeyGet(hCntWnd AS PTR, lParam AS LONGINT) AS LOGIC STRICT
	//SYSTEM
	RETURN FALSE
STATIC FUNCTION TCntColorGet(hWnd AS PTR, iColor AS DWORD) AS DWORD STRICT
	//SYSTEM
	RETURN 0

STATIC FUNCTION TCntColorSet(hWnd AS PTR, iColor AS DWORD, cr AS DWORD) AS DWORD STRICT
	//SYSTEM
	RETURN 0
STATIC FUNCTION TCntCurrentPosExGet(hWnd AS PTR) AS LONGINT STRICT
	//SYSTEM
	RETURN 0

STATIC FUNCTION TCntCurrentPosExSet(hWnd AS PTR, lPos AS LONGINT) AS LONGINT STRICT
	//SYSTEM
	RETURN 0

STATIC FUNCTION TCntCursorSet(hWnd AS PTR, hCursor AS PTR, iArea AS DWORD) AS LOGIC STRICT
	//SYSTEM
	RETURN FALSE

STATIC FUNCTION TCntDeferPaint(hWnd AS PTR) AS VOID STRICT
RETURN
STATIC FUNCTION TCntDeltaExSet(hCntWnd AS PTR, lDelta AS LONGINT) AS VOID STRICT
RETURN
STATIC FUNCTION TCntDeltaPosExGet(hCntWnd AS PTR) AS LONGINT STRICT
	//SYSTEM
	RETURN 0

STATIC FUNCTION TCntDeltaPosExSet(hCntWnd AS PTR, lDeltaPos AS LONGINT) AS VOID STRICT
RETURN
STATIC FUNCTION TCntEndDeferPaint(hWnd AS PTR, bUpdate AS LOGIC) AS VOID STRICT
RETURN
STATIC FUNCTION TCntEndRecEdit(hCntWnd AS PTR, lpRec AS _winRECORDCORE, lpFld AS _winFIELDINFO) AS LOGIC STRICT
	//SYSTEM
	RETURN FALSE

STATIC FUNCTION TCntFldAttrClear(lpFld AS _winFIELDINFO, dwAttrib AS DWORD) AS VOID STRICT
RETURN
STATIC FUNCTION TCntFldAttrSet(lpFld AS _winFIELDINFO, dwAttrib AS DWORD) AS VOID STRICT
RETURN
STATIC FUNCTION TCntFldColorSet(hWnd AS PTR, lpFld AS _winFIELDINFO, iColor AS DWORD, cr AS DWORD) AS DWORD STRICT
	//SYSTEM
	RETURN 0

STATIC FUNCTION TCntFldDataAlnSet(hWnd AS PTR, lpFld AS _winFIELDINFO, dwAlign AS DWORD) AS VOID STRICT
RETURN
STATIC FUNCTION TCntFldDrwProcSet(lpFld AS _winFIELDINFO, lpfnDrawProc AS PTR) AS LOGIC STRICT
	//SYSTEM
	RETURN FALSE

STATIC FUNCTION TCntFldHeadGet(hCntWnd AS PTR) AS _winFIELDINFO STRICT
	//SYSTEM
	RETURN NULL_PTR //PP-040410

STATIC FUNCTION TCntFldTailGet(hCntWnd AS PTR) AS _winFIELDINFO STRICT
	//SYSTEM
	RETURN NULL_PTR //PP-040410

STATIC FUNCTION TCntFldTtlHtSet(hCntWnd AS PTR, nHeight AS INT) AS INT STRICT
	//SYSTEM
	RETURN 0

STATIC FUNCTION TCntFldTtlSepSet(hWnd AS PTR) AS VOID STRICT
RETURN
STATIC FUNCTION TCntFldTtlSet(hWnd AS PTR, lpFld AS _winFIELDINFO, lpszColTitle AS PSZ, wTitleLen AS DWORD) AS LOGIC STRICT
	//SYSTEM
	RETURN FALSE

STATIC FUNCTION TCntFldUserGet(lpFld AS _winFIELDINFO) AS PTR STRICT
	//SYSTEM
	RETURN NULL_PTR

STATIC FUNCTION TCntFldUserSet(lpFld AS _winFIELDINFO, lpUserData AS PTR, wUserBytes AS DWORD) AS LOGIC STRICT
	//SYSTEM
	RETURN FALSE

STATIC FUNCTION TCntFldWidthSet(hCntWnd AS PTR, lpFld AS _winFIELDINFO, nWidth AS DWORD) AS SHORTINT STRICT
	//SYSTEM
	RETURN 0

STATIC FUNCTION TCntFocusExtGet(hCntWnd AS PTR) AS DWORD STRICT
	//SYSTEM
	RETURN 0

STATIC FUNCTION TCntFocusFldGet(hCntWnd AS PTR) AS _winFIELDINFO STRICT
	//SYSTEM
	RETURN NULL_PTR //PP-040410

STATIC FUNCTION TCntFocusFldLock(hCntWnd AS PTR) AS INT STRICT
	//SYSTEM
	RETURN 0

STATIC FUNCTION TCntFocusFldUnlck(hCntWnd AS PTR) AS INT STRICT
	//SYSTEM
	RETURN 0

STATIC FUNCTION TCntFocusMove(hCntWnd AS PTR, wDir AS DWORD) AS LOGIC STRICT
	//SYSTEM
	RETURN FALSE

STATIC FUNCTION TCntFocusOrgGet(hCntWnd AS PTR, bScreen AS LOGIC) AS DWORD STRICT
	//SYSTEM
	RETURN 0

STATIC FUNCTION TCntFocusRecGet(hCntWnd AS PTR) AS _winRECORDCORE STRICT
	//SYSTEM
	RETURN NULL_PTR //PP-040410

STATIC FUNCTION TCntFocusRecLock(hCntWnd AS PTR) AS INT STRICT
	//SYSTEM
	RETURN 0

STATIC FUNCTION TCntFocusRecUnlck(hCntWnd AS PTR) AS INT STRICT
	//SYSTEM
	RETURN 0

STATIC FUNCTION TCntFocusSet(hCntWnd AS PTR, lpRec AS _winRECORDCORE, lpFld AS _winFIELDINFO) AS LOGIC STRICT
	//SYSTEM
	RETURN FALSE

STATIC FUNCTION TCntFontSet(hWnd AS PTR, hFont AS PTR, iFont AS DWORD) AS LOGIC STRICT
	//SYSTEM
	RETURN FALSE

STATIC FUNCTION TCntFreeRecCore(lpRec AS _winRECORDCORE) AS LOGIC STRICT
	//SYSTEM
	RETURN FALSE

STATIC FUNCTION TCntInsFldBefore(hCntWnd AS PTR, lpFld AS _winFIELDINFO, lpNew AS _winFIELDINFO) AS LOGIC STRICT
	//SYSTEM
	RETURN FALSE

STATIC FUNCTION TCntIsFocusCellRO(hCntWnd AS PTR) AS LOGIC STRICT
	//SYSTEM
	RETURN FALSE

STATIC FUNCTION TCntIsRecSelected(hCntWnd AS PTR, lpRec AS _winRECORDCORE) AS LOGIC STRICT
	//SYSTEM
	RETURN FALSE

STATIC FUNCTION TCntKillRecList(hCntWnd AS PTR) AS LOGIC STRICT
	//SYSTEM
	RETURN FALSE

STATIC FUNCTION TCntNewFldInfo() AS _winFIELDINFO STRICT
	//SYSTEM
	RETURN NULL_PTR //PP-040410

STATIC FUNCTION TCntNewRecCore(dwRecSize AS DWORD) AS _winRECORDCORE STRICT
	//SYSTEM
	RETURN NULL_PTR //PP-040410

STATIC FUNCTION TCntNextRec(lpRec AS _winRECORDCORE) AS _winRECORDCORE STRICT
	//SYSTEM
	RETURN NULL_PTR //PP-040410

STATIC FUNCTION TCntNotifyAssoc(hWnd AS PTR, wEvent AS DWORD, wOemCharVal AS DWORD, lpRec AS _winRECORDCORE, lpFld AS _WinFIELDINFO, nInc AS INT,;
	bShiftKey AS LOGIC, bCtrlKey AS LOGIC, lpUserData AS PTR) AS VOID STRICT
RETURN

STATIC FUNCTION TCntRangeExSet(hWnd AS PTR, lMin AS LONGINT, lMax AS LONGINT) AS LOGIC STRICT
	//SYSTEM
	RETURN FALSE

STATIC FUNCTION TCntRangeSet(hWnd AS PTR, iMin AS DWORD, iMax AS DWORD) AS DWORD STRICT
	//SYSTEM
	RETURN 0

STATIC FUNCTION TCntRecAttrClear(lpRec AS _winRECORDCORE, dwAttrib AS DWORD) AS VOID STRICT
RETURN
STATIC FUNCTION TCntRecAttrSet(lpRec AS _winRECORDCORE, dwAttrib AS DWORD) AS VOID STRICT
RETURN
STATIC FUNCTION TCntRecDataSet(lpRec AS _winRECORDCORE, lpData AS PTR) AS LOGIC STRICT
	//SYSTEM
	RETURN FALSE

STATIC FUNCTION TCntRecHeadGet(hCntWnd AS PTR) AS _winRECORDCORE STRICT
	//SYSTEM
	RETURN NULL_PTR //PP-040410

STATIC FUNCTION TCntRecsDispGet(hCntWnd AS PTR) AS INT STRICT
	//SYSTEM
	RETURN 0

STATIC FUNCTION TCntRecTailGet(hCntWnd AS PTR) AS _winRECORDCORE STRICT
	//SYSTEM
	RETURN NULL_PTR //PP-040410

STATIC FUNCTION TCntRecUserGet(lpRec AS _winRECORDCORE) AS PTR STRICT
	//SYSTEM
	RETURN NULL_PTR

STATIC FUNCTION TCntRecUserSet(lpRec AS _winRECORDCORE, lpUserData AS PTR, wUserBytes AS DWORD) AS LOGIC STRICT
	//SYSTEM
	RETURN FALSE

STATIC FUNCTION TCntRemoveFld(hCntWnd AS PTR, lpFld AS _winFIELDINFO) AS _winFIELDINFO STRICT
	//SYSTEM
	RETURN NULL_PTR //PP-040410

STATIC FUNCTION TCntRemoveRec(hCntWnd AS PTR, lpRec AS _winRECORDCORE) AS _winRECORDCORE STRICT
	//SYSTEM
	RETURN NULL_PTR //PP-040410

STATIC FUNCTION TCntRemoveRecHead(hCntWnd AS PTR) AS _winRECORDCORE STRICT
	//SYSTEM
	RETURN NULL_PTR //PP-040410

STATIC FUNCTION TCntRemoveRecTail(hCntWnd AS PTR) AS _winRECORDCORE STRICT
	//SYSTEM
	RETURN NULL_PTR //PP-040410

STATIC FUNCTION TCntRowHtSet(hCntWnd AS PTR, nHeight AS INT, wLineSpace AS DWORD) AS INT STRICT
	//SYSTEM
	RETURN 0

STATIC FUNCTION TCntScrollFldArea(hCntWnd AS PTR, nIncrement AS INT) AS VOID STRICT
RETURN
STATIC FUNCTION TCntScrollRecArea(hCntWnd AS PTR, nIncrement AS INT) AS _winRECORDCORE STRICT
	//SYSTEM
	RETURN NULL_PTR //PP-040410

STATIC FUNCTION TCntScrollRecAreaEx(hCntWnd AS PTR, lIncrement AS LONGINT) AS _winRECORDCORE STRICT
	//SYSTEM
	RETURN NULL_PTR //PP-040410

STATIC FUNCTION TCntSelectRec(hCntWnd AS PTR, lpRec AS _winRECORDCORE) AS LOGIC STRICT
	//SYSTEM
	RETURN FALSE

STATIC FUNCTION TCntSelRecGet(hCntWnd AS PTR) AS _winRECORDCORE STRICT
	//SYSTEM
	RETURN NULL_PTR //PP-040410

STATIC FUNCTION TCntSpltBarCreate(hCntWnd AS PTR, wMode AS DWORD, xCoord AS INT) AS VOID STRICT
RETURN
STATIC FUNCTION TCntSpltBarDelete(hCntWnd AS PTR, wMode AS DWORD, xCoord AS INT) AS VOID STRICT
RETURN
STATIC FUNCTION TCntStyleClear(hWnd AS PTR, dwStyle AS DWORD) AS VOID STRICT
RETURN
STATIC FUNCTION TCntStyleSet(hWnd AS PTR, dwStyle AS DWORD) AS VOID STRICT
RETURN
STATIC FUNCTION TCntTopRecGet(hCntWnd AS PTR) AS _winRECORDCORE STRICT
	//SYSTEM
	RETURN NULL_PTR //PP-040410

STATIC FUNCTION TCntTopRecSet(hCntWnd AS PTR, lpRec AS _winRECORDCORE) AS LOGIC STRICT
	//SYSTEM
	RETURN FALSE

STATIC FUNCTION TCntTtlAlignSet(hWnd AS PTR, dwAlign AS DWORD) AS VOID STRICT
RETURN
STATIC FUNCTION TCntTtlHtSet(hCntWnd AS PTR, nHeight AS INT) AS INT STRICT
	//SYSTEM
	RETURN 0

STATIC FUNCTION TCntTtlSepSet(hWnd AS PTR) AS VOID STRICT
RETURN
STATIC FUNCTION TCntTtlSet(hWnd AS PTR, lpszTitle AS PSZ) AS LOGIC STRICT
	//SYSTEM
	RETURN FALSE

STATIC FUNCTION TCntUnSelectRec(hCntWnd AS PTR, lpRec AS _winRECORDCORE) AS LOGIC STRICT
	//SYSTEM
	RETURN FALSE

STATIC FUNCTION TCntViewSet(hWnd AS PTR, iView AS DWORD) AS DWORD STRICT
	//SYSTEM
	RETURN 0

STATIC FUNCTION TCntVScrollPosExSet(hCntWnd AS PTR, lPosition AS LONGINT) AS VOID STRICT
RETURN
STATIC FUNCTION TCntVScrollPosSet(hCntWnd AS PTR, nPosition AS SHORTINT) AS VOID STRICT
RETURN

FUNCTION __DrawFldData(hWnd AS PTR, strucFieldInfo AS _WinFieldInfo, strucRecordCore AS _WinRecordCore, ;
	ptrData AS PTR, hDC AS PTR, iX AS INT, iY AS INT, dwOptions AS DWORD, ;
	ptrRect AS _WINRECT, pszData AS /*PSZ*/ PTR, dwLength AS DWORD) AS INT /* CALLBACK */
	
	// dcaton 080608
   // Changed parameter 10 from PSZ to PTR.  This function is used as a callback function via a delegate and
   // Marshal.GetFunctionPointerForDelegate( (System.Delegate) ) and the CLR cannot marshal a PSZ since it is a managed structure.
   // Since a PSZ is really just a byte* (as far as native code is concerned) changing the parameter to PTR
   // keeps the CLR happy, and 'pszData' is cast to a PSZ in the call to __DrawCellData.
   // This fixes defect id #350
	
	LOCAL oColumn AS DataColumn
	LOCAL p AS SelfPtr
   // Note lpUserData contains a pointer to a buffer of 4 bytes with the self pointer!
	p := strucFieldInfo:lpUserData
	p := p:ptrSelf
    oColumn := __WCSelfPtr2Object(p)
	IF oColumn != NULL_OBJECT
		oColumn:__DrawCellData(hDC, iX, iY, dwOptions, ptrRect, (PSZ) pszData, dwLength)
		RETURN 0
	ENDIF

	RETURN 1

FUNCTION __LoadContainerDLL()
	LOCAL hDll AS PTR
	LOCAL rsFormat AS ResourceString

	IF glContainerDllLoaded
		RETURN TRUE
	ENDIF

    hDll := LoadLibrary(String2Psz( "CATO3CNT.DLL"))
	IF (hDll == NULL_PTR)
		rsFormat := ResourceString{__WCSLoadLibraryError}
		WCError{#LoadContainerDLL, #DataBrowser, VO_Sprintf(rsFormat:value, "CATO3CNT.DLL"),,,FALSE}:@@Throw()
		RETURN FALSE
	ENDIF
	ghContainerDLL  			:= hDll
	gpfnCntFocusFldUnlck 	:= __GetProcAddress( "CntFocusFldUnlck")
	gpfnCntTopRecGet 			:= __GetProcAddress( "CntTopRecGet")
	gpfnCntFocusRecGet 		:= __GetProcAddress( "CntFocusRecGet")
	gpfnCntSelectRec 			:= __GetProcAddress( "CntSelectRec")
	gpfnCntRecHeadGet 		:= __GetProcAddress( "CntRecHeadGet")
	gpfnCntNextRec 			:= __GetProcAddress( "CntNextRec")
	gpfnCntFocusFldGet 		:= __GetProcAddress( "CntFocusFldGet")
	gpfnCntFocusSet 			:= __GetProcAddress( "CntFocusSet")
	gpfnCntFocusRecLock 		:= __GetProcAddress( "CntFocusRecLock")
	gpfnCntCurrentPosExSet 	:= __GetProcAddress( "CntCurrentPosExSet")
	gpfnCntEndRecEdit 		:= __GetProcAddress( "CntEndRecEdit")
	gpfnCntAddRecHead 		:= __GetProcAddress( "CntAddRecHead")
	gpfnCntRemoveRecTail		:= __GetProcAddress( "CntRemoveRecTail")
	gpfnCntFreeRecCore 		:= __GetProcAddress( "CntFreeRecCore")
	gpfnCntTopRecSet 			:= __GetProcAddress( "CntTopRecSet")
	gpfnCntDeltaExSet 		:= __GetProcAddress( "CntDeltaExSet")
	gpfnCntDeltaPosExSet 	:= __GetProcAddress( "CntDeltaPosExSet")
	gpfnCntVScrollPosExSet 	:= __GetProcAddress( "CntVScrollPosExSet")
	gpfnCntRangeExSet 		:= __GetProcAddress( "CntRangeExSet")
	gpfnCntCurrentPosExGet 	:= __GetProcAddress( "CntCurrentPosExGet")
	gpfnCntRecTailGet 		:= __GetProcAddress( "CntRecTailGet")
	gpfnCntAddRecTail 		:= __GetProcAddress( "CntAddRecTail")
	gpfnCntRemoveRecHead 	:= __GetProcAddress( "CntRemoveRecHead")
	gpfnCntDeltaPosExGet 	:= __GetProcAddress( "CntDeltaPosExGet")
	gpfnCntCNIncExGet 		:= __GetProcAddress( "CntCNIncExGet")
	gpfnCntNewRecCore 		:= __GetProcAddress( "CntNewRecCore")
	gpfnCntRecUserSet 		:= __GetProcAddress( "CntRecUserSet")
	gpfnCntRecAttrSet 		:= __GetProcAddress( "CntRecAttrSet")
	gpfnCntNewFldInfo 		:= __GetProcAddress( "CntNewFldInfo")
	gpfnCntFldUserSet 		:= __GetProcAddress( "CntFldUserSet")
	gpfnCntCNChildWndGet 	:= __GetProcAddress( "CntCNChildWndGet")
	gpfnCntKillRecList 		:= __GetProcAddress( "CntKillRecList")
	gpfnCntSelRecGet 			:= __GetProcAddress( "CntSelRecGet")
	gpfnCntUnSelectRec 		:= __GetProcAddress( "CntUnSelectRec")
	gpfnCntStyleClear 		:= __GetProcAddress( "CntStyleClear")
	gpfnCntIsRecSelected 	:= __GetProcAddress( "CntIsRecSelected")
	gpfnCntRecAttrClear 		:= __GetProcAddress( "CntRecAttrClear")
	gpfnCntStyleSet 			:= __GetProcAddress( "CntStyleSet")
	gpfnCntRecDataSet 		:= __GetProcAddress( "CntRecDataSet")
	gpfnCntRecsDispGet 		:= __GetProcAddress( "CntRecsDispGet")
	gpfnCntFldTtlHtSet 		:= __GetProcAddress( "CntFldTtlHtSet")
	gpfnCntColorSet 			:= __GetProcAddress( "CntColorSet")
	gpfnCntFontSet 			:= __GetProcAddress( "CntFontSet")
	gpfnCntColorGet 			:= __GetProcAddress( "CntColorGet")
	gpfnCntAttribSet 			:= __GetProcAddress( "CntAttribSet")
	gpfnCntAttribClear 		:= __GetProcAddress( "CntAttribClear")
	gpfnCntSpltBarCreate 	:= __GetProcAddress( "CntSpltBarCreate")
	gpfnCntSpltBarDelete 	:= __GetProcAddress( "CntSpltBarDelete")
	gpfnCntEndDeferPaint 	:= __GetProcAddress( "CntEndDeferPaint")
	gpfnCntTtlSet 				:= __GetProcAddress( "CntTtlSet")
	gpfnCntTtlAlignSet 		:= __GetProcAddress( "CntTtlAlignSet")
	gpfnCntTtlHtSet 			:= __GetProcAddress( "CntTtlHtSet")
	gpfnCntTtlSepSet 			:= __GetProcAddress( "CntTtlSepSet")
	gpfnCntCursorSet 			:= __GetProcAddress( "CntCursorSet")
	gpfnCntDeferPaint 		:= __GetProcAddress( "CntDeferPaint")
	gpfnCntFldHeadGet 		:= __GetProcAddress( "CntFldHeadGet")
	gpfnCntAssociateSet 		:= __GetProcAddress( "CntAssociateSet")
	gpfnCntViewSet 			:= __GetProcAddress( "CntViewSet")
	gpfnCntRowHtSet 			:= __GetProcAddress( "CntRowHtSet")
	gpfnCntNotifyAssoc 		:= __GetProcAddress( "CntNotifyAssoc")
	gpfnCntCNRecGet 			:= __GetProcAddress( "CntCNRecGet")
	gpfnCntCNShiftKeyGet 	:= __GetProcAddress( "CntCNShiftKeyGet")
	gpfnCntFocusMove 			:= __GetProcAddress( "CntFocusMove")
	gpfnCntIsFocusCellRO 	:= __GetProcAddress( "CntIsFocusCellRO")
	gpfnCntCNCharGet 			:= __GetProcAddress( "CntCNCharGet")
	gpfnCntCNFldGet 			:= __GetProcAddress( "CntCNFldGet")
	gpfnCntFldTailGet 		:= __GetProcAddress( "CntFldTailGet")
	gpfnCntFocusRecUnlck 	:= __GetProcAddress( "CntFocusRecUnlck")
	gpfnCntScrollRecAreaEx	:= __GetProcAddress( "CntScrollRecAreaEx")
	gpfnCntScrollFldArea 	:= __GetProcAddress( "CntScrollFldArea")
	gpfnCntFldDataAlnSet 	:= __GetProcAddress( "CntFldDataAlnSet")
	gpfnCntFldTtlSepSet 		:= __GetProcAddress( "CntFldTtlSepSet")
	gpfnCntFldTtlSet 			:= __GetProcAddress( "CntFldTtlSet")
	gpfnCntAddFldTail 		:= __GetProcAddress( "CntAddFldTail")
	gpfnCntInsFldBefore 		:= __GetProcAddress( "CntInsFldBefore")
	gpfnCntRemoveFld 			:= __GetProcAddress( "CntRemoveFld")
	gpfnCntFocusExtGet 		:= __GetProcAddress( "CntFocusExtGet")
	gpfnCntFocusOrgGet 		:= __GetProcAddress( "CntFocusOrgGet")
	gpfnCntFocusFldLock 		:= __GetProcAddress( "CntFocusFldLock")
	gpfnCntFldColorSet 		:= __GetProcAddress( "CntFldColorSet")
	gpfnCntFldAttrClear 		:= __GetProcAddress( "CntFldAttrClear")
	gpfnCntFldAttrSet 		:= __GetProcAddress( "CntFldAttrSet")
	gpfnCntFldWidthSet 		:= __GetProcAddress( "CntFldWidthSet")
	gpfnCntFldDrwProcSet 	:= __GetProcAddress( "CntFldDrwProcSet")

	RETURN (glContainerDllLoaded := TRUE)
#ifdef __VULCAN__
   DELEGATE __CellEditProcDelegate( hWnd AS PTR, uMsg AS DWORD, wParam AS DWORD, lParam AS LONGINT ) AS INT
   DELEGATE __DrawFldDataDelegate( hWnd AS PTR, strucFieldInfo AS _WinFieldInfo, strucRecordCore AS _WinRecordCore, ptrData AS PTR, hDC AS PTR, iX AS INT, iY AS INT, dwOptions AS DWORD, ptrRect AS _WINRECT, pszData AS /*PSZ*/ PTR, dwLength AS DWORD ) AS INT
   DELEGATE __WCGBChildProcDelegate( hWnd AS PTR, uMsg AS DWORD, wParam AS DWORD, lParam AS LONGINT ) AS LONGINT
#endif

FUNCTION __WCGBChildProc(hWnd AS PTR, uMsg AS DWORD, wParam AS DWORD, lParam AS LONGINT) AS LONGINT /* WINCALL */
	LOCAL oBrowser AS OBJECT
	LOCAL i AS INT

	IF (uMsg == WM_CHAR)
		oBrowser := __WCGetControlByHandle(GetParent(hWnd))
		IF IsInstanceOf(oBrowser, #DataBrowser)
			IVarPut(oBrowser, #__LastChar, wParam)
		ENDIF
		//PP-040410 This is better handled in control dispatch
		// 	ELSEIF (uMsg == WM_CONTEXTMENU)
		// 		oBrowser := __WCGetControlByHandle(GetParent(hWnd))
		// 		IF (oBrowser:ContextMenu != NULL_OBJECT)
		// 			oBrowser:ContextMenu:ShowAsPopUp(oBrowser)
		// 		ELSEIF (oBrowser:Owner:ContextMenu != NULL_OBJECT)
		// 			oBrowser:Owner:ContextMenu:ShowAsPopUp(oBrowser:Owner)
		// 		ENDIF
	ELSEIF (uMsg == 0x020A) //WM_MOUSEWHEEL
		FOR i:=1 TO 5
			IF (SHORTINT(_CAST, HiWord(wParam)) < 0)
				PostMessage(hwnd, WM_VSCROLL, SB_LINEDOWN, 0)
			ELSE
				PostMessage(hwnd, WM_VSCROLL, SB_LINEUP, 0)
			ENDIF
		NEXT
	ENDIF

	RETURN CallWindowProc(pfGBChildProcOrg, hWnd, uMsg, wParam, lParam)
	
#ifdef __VULCAN__
   DELEGATE __WCGBNotifyProcDelegate( hWnd AS PTR, uMsg AS DWORD, wParam AS DWORD, lParam AS LONGINT ) AS LONGINT
#endif

FUNCTION __WCGBNotifyProc(hWnd AS PTR, uMsg AS DWORD, wParam AS DWORD, lParam AS LONGINT) AS LONGINT /* WINCALL */
	LOCAL oControl AS Control
	LOCAL strucCreateStruct AS _WinCreateStruct
	LOCAL p AS SelfPtr


	IF uMsg == WM_CREATE
		strucCreateStruct := PTR(_CAST, lParam)
		p := strucCreateStruct:lpCreateParams
		oControl := __WCSelfPtr2Object(p)
		SetWindowLong(hWnd, DWL_USER, LONGINT(_CAST, p))
	ELSE
	   p := PTR(_CAST, GetWindowLong(hWnd, DWL_USER))
		oControl := __WCSelfPtr2Object(p)
	ENDIF

	IF oControl != NULL_OBJECT
		RETURN oControl:Dispatch(@@Event{ hWnd, uMsg, wParam, lParam, oControl})
	ENDIF
	RETURN DefWindowProc(hWnd, uMsg, wParam, lParam)

STATIC FUNCTION __WCRegisterGBNotifyWindow(hInst AS PTR) AS LOGIC
	STATIC LOCAL lretVal AS LOGIC
	LOCAL wc IS _WINWNDclass


	IF !lretVal
		wc:style := CS_GLOBALCLASS
#ifdef __VULCAN__
      STATIC LOCAL WCGBNotifyProcDelegate AS __WCGBNotifyProcDelegate
      IF WCGBNotifyProcDelegate == NULL
         WCGBNotifyProcDelegate := __WCGBNotifyProcDelegate{ NULL, @__WCGBNotifyProc() }
      ENDIF
		wc:lpfnWndProc := System.Runtime.InteropServices.Marshal.GetFunctionPointerForDelegate( (System.Delegate) WCGBNotifyProcDelegate )
#else		
		wc:lpfnWndProc := PTR(_CAST, @__WCGBNotifyProc())
#endif		
		wc:hInstance := hInst
		wc:hbrBackground := (COLOR_WINDOW + 1)
        wc:lpszClassName := String2Psz(__WCGBNotifyWindowClass)
		wc:cbWndExtra := 12

		lretVal := (RegisterClass(@wc) != 0)
	ENDIF

	RETURN lretVal


STATIC FUNCTION __GetProcAddress(cProcname AS STRING)	AS	PTR
	LOCAL pAddr AS PTR
	pAddr := GetProcAddress(ghContainerDLL, String2Psz(cProcname))
	IF pAddr == NULL_PTR
		WCError{#LoadContainerDLL, #DataBrowser, "Could not find address of function "+cProcname+" in CATO3CNT.DLL",,,FALSE}:@@Throw()
	ENDIF
	RETURN pAddr  





// dcaton 070724	
// This was incorrectly typed as LOGIC.  It works in VO, but in Vulcan the CLR marshals logics as 1 or 0.  This
// causes the return value of CallWindowProc() to be limited to 0 or 1, which causes all sorts of problems
// when an int is returned.  There is no reason to type this callback as returning a logic, since CallWindowProc()
// really returns INT as documented in MSDN.	
FUNCTION __CellEditProc(hWnd AS PTR, uMsg AS DWORD, wParam AS DWORD, lParam AS LONGINT) AS /* LOGIC */ INT //_WINCALL
	LOCAL oControl AS Control
	LOCAL oOwner AS OBJECT
	LOCAL lpfnDefaultProc AS PTR


	oControl := __WCGetControlByHandle(hWnd)

	IF (oControl != NULL_OBJECT)
		oOwner := oControl:Owner
		IF IsInstanceOf(oOwner, #DataBrowser)
			lpfnDefaultProc := oOwner:ptrControlDefaultProc
			IF oOwner:__EditDispatch(uMsg, wParam, lParam)
				RETURN 1 // TRUE
			ENDIF
			IF (lpfnDefaultProc != NULL_PTR)
//				RETURN LOGIC(_CAST, CallWindowProc(lpfnDefaultProc, hWnd, umsg, wParam, lParam))
				RETURN CallWindowProc(lpfnDefaultProc, hWnd, umsg, wParam, lParam)
			ENDIF
		ENDIF
	ENDIF

	RETURN 1 // TRUE




#region defines
DEFINE GBSSBLEFT := 1
DEFINE GBSSBMIDDLE := 2
DEFINE GBSSBRIGHT := 3
DEFINE ssBlockSelection      := 3
DEFINE ssExtendedSelection := 2
DEFINE ssNoSelection         := 0
DEFINE ssSingleSelection     := 1
DEFINE __WCGBNotifyWindowClass := "GBNotifyContext"
#endregion
