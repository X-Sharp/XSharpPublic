CLASS DataListView INHERIT ListView
	PROTECT oDLVServer AS DataServer
	PROTECT iColumns AS INT
	PROTECT lNoNotifies AS LOGIC
	PROTECT aCache AS ARRAY
	PROTECT iCacheMax AS INT
	PROTECT iCacheStart AS INT
	PROTECT iCacheEnd AS INT
	PROTECT lUseOrder AS LOGIC    

	//PP-030828 Strong typing
	METHOD __AutoLayout() AS VOID STRICT 
	//PP-030828 Strong typing
	LOCAL oDF AS DataField
	LOCAL oLVC AS ListViewColumn
	LOCAL i AS INT
	IF (oDLVServer == NULL_OBJECT)
		RETURN
	ENDIF

	SELF:DeleteAllColumns()
	iColumns := oDLVServer:FCount

	FOR i:= 1 TO iColumns
		oDF := oDLVServer:DataField(i)
		IF (oDF == NULL_OBJECT)
			LOOP
		ENDIF

		oLVC := ListViewColumn{oDF:FieldSpec:Length, oDF:Hyperlabel}
		oLVC:FieldSpec := oDF:FieldSpec
		oLVC:Caption := oDF:FieldSpec:HyperLabel:Caption
		SELF:AddColumn(oLVC)
	NEXT

	iColumns := INT(_CAST, ALen(aColumns))
	RETURN

METHOD __AutoResize() AS VOID STRICT 
	//PP-030828 Strong typing

	//PP-030910 see below
// 	LOCAL oP AS Point
// 	LOCAL oBB AS BoundingBox

	IF SELF:ValidateControl()
		//PP-030910 from S Ebert bug 78, old code below commented
	     WCMoveWindow(SELF, Point{0,0}, oParent:CanvasArea:Size, TRUE)
		/*
		oP := SELF:Origin
		IF (oP:X != 0) .or. (oP:Y != 0)
			SELF:Origin:=Point{0,0}
		ENDIF
		oBB := oParent:CanvasArea
		SELF:Size := Dimension{oBB:Width, oBB:Height}
		*/
	ENDIF
	RETURN

METHOD __CacheHint(oCtrlNotifyEvent AS ControlNotifyEvent) AS VOID STRICT 
	//PP-030828 Strong typing
	LOCAL lpHint AS _winNMLVCACHEHINT
	LOCAL iRecNoSave, i AS INT
	LOCAL iDel AS INT
	LOCAL iStart, iEnd
	LOCAL iFrom, iTo AS INT
//	STATIC LOCAL iCH AS INT

	lpHint := PTR(_CAST, oCtrlNotifyEvent:lParam)

	iFrom := lpHint:iFrom + 1
	iTo 	:= lpHint:iTo + 1
	IF ((iTo - iFrom) > iCacheMax) .OR.;
		((iFrom >= iCacheStart) .AND. (iTo <= iCacheEnd)) .OR.;
		(oDLVServer == NULL_OBJECT)
		RETURN
	ENDIF

	oDLVServer:SuspendNotification()
	iRecNoSave := oDLVServer:RecNo

	IF (iCacheMax - (iTo - iFrom + 1) >= 20)
		iFrom := Max(1, iFrom - 10)
		iTo += 10
	ENDIF

	iStart := 1
	iEnd := (iTo - iFrom) + 1

	IF (iFrom >= iCacheStart) .AND. (iFrom <= iCacheEnd)
		// reuse entries at end of cache
		iDel := (iFrom - iCacheStart)
		FOR i := 1 TO (iDel)
			ADel(aCache, 1)
		NEXT
		SELF:__SetServerPos(iCacheEnd + 1)
		iStart := iCacheEnd - iFrom + 2
	ELSEIF (iTo >= iCacheStart) .AND. (iTo <= iCacheEnd)
		// reuse entries as beginning of cache
		iDel := (iCacheStart - iFrom)
		FOR i := 1 TO iDel
			AIns(aCache, 1)
		NEXT
		SELF:__SetServerPos(iFrom)
		iEnd := iDel
	ELSE
		SELF:__SetServerPos(iFrom)
	ENDIF

	iCacheStart := iFrom
	iCacheEnd := iTo

	FOR i:= iStart TO iEnd
		SELF:__FillCacheItem(i)
		oDLVServer:Skip(1)
		IF oDLVServer:EoF
			oDLVServer:Skip(-1)
			iCacheEnd := SELF:__GetServerPos()
			EXIT
		ENDIF
	NEXT

	oDLVServer:GoTo(iRecNoSave)
	oDLVServer:ResetNotification()

	RETURN

METHOD __FillCacheItem(iIndex AS INT) AS VOID STRICT 
	//PP-030828 Strong typing
	LOCAL j, cCols AS DWORD
	LOCAL symCol AS SYMBOL
	LOCAL oFS AS FieldSpec
	LOCAL sVal AS STRING
   LOCAL oCol AS ListViewColumn
	

	cCols := ALen(aColumns)
	IF IsNil(aCache[iIndex])
		aCache[iIndex] := ArrayCreate(cCols)
	ENDIF
	FOR j := 1 TO cCols
        oCol    := aColumns[j] 
        symCol  := oCol:NameSym
        oFS     := oCol:FieldSpec
		IF (oFS != NULL_OBJECT)
			sVal := oFS:Transform(SELF:FIELDGET(symCol))
		ELSE
			sVal := AsString(SELF:FIELDGET(symCol))
		ENDIF
		aCache[iIndex, j] := sVal
	NEXT
  	RETURN

METHOD __FindItem(oCtrlNotifyEvent AS ControlNotifyEvent) AS INT STRICT 
	//PP-030828 Strong typing
	LOCAL fi AS _winNMLVFINDITEM
	LOCAL iRet := -1 AS INT

	

	IF lUseOrder
		fi := PTR(_CAST, oCtrlNotifyEvent:lParam)
		// self:owner:owner:caption := "searching from "+NTrim(fi.iStart)+" for "+AsString(fi.lvfi._psz)
		oDLVServer:SuspendNotification()
		IF oDLVServer:Seek(AsString(fi:lvfi:_psz), TRUE)
			iRet := SELF:__GetServerPos()-1
		ENDIF
		oDLVServer:ResetNotification()
	ENDIF

	RETURN iRet

METHOD __GetDispInfo(oCtrlNotifyEvent AS ControlNotifyEvent) AS VOID STRICT 
	LOCAL di AS _winLV_DISPINFO
	LOCAL iOrderPos, iCol AS INT
	LOCAL iLen AS DWORD
	LOCAL symCol AS SYMBOL
	LOCAL oFS AS FieldSpec
	LOCAL uVal AS USUAL
	LOCAL sVal AS STRING
	LOCAL iRecNoSave AS INT
    LOCAL oCol AS ListViewColumn
	
	IF (oDLVServer == NULL_OBJECT)
		RETURN
	ENDIF

	di := PTR(_CAST, oCtrlNotifyEvent:lParam)

	IF !LOGIC(_CAST, _AND(di:item:mask, LVIF_TEXT))
		RETURN
	ENDIF

	iOrderPos := di:item:iItem + 1

	IF (iOrderPos >= iCacheStart) .AND. (iOrderPos <= iCacheEnd)
		uVal := aCache[iOrderPos - iCacheStart + 1, di:item:iSubItem+1]
		IF IsString(uVal)
			sVal := uVal
		ELSE
			sVal := ""
		ENDIF
	ELSE
		oDLVServer:SuspendNotification()
		iRecNoSave := oDLVServer:RecNo

		IF (SELF:__SetServerPos(iOrderPos) != 0)
			iCol := di:item:iSubItem + 1
            oCol := aColumns[iCol] 
            symCol  := oCol:NameSym
            oFS     := oCol:FieldSpec
			
			IF (oFS != NULL_OBJECT)
				sVal := oFS:Transform(SELF:FIELDGET(symCol))
			ELSE
				sVal := AsString(SELF:FIELDGET(symCol))
			ENDIF
		ENDIF

		oDLVServer:GoTo(iRecNoSave)
		oDLVServer:ResetNotification()
	ENDIF

	IF !Empty(sVal)
		iLen := SLen(sVal) + 1
		IF (iLen >= DWORD(di:item:cchTextMax))
			sVal :=  Left(sVal, DWORD(di:item:cchTextMax) - 5) + "..."
			iLen :=  SLen(sVal) + 1
		ENDIF
		MemCopy(di:item:pszText, String2Psz(sVal), iLen)
	ENDIF

	RETURN

ACCESS __GetServerCount() AS DWORD STRICT 
	//PP-030828 Strong typing
	IF lUseOrder
		RETURN Send(oDLVServer, #OrderKeyCount)
	ENDIF
	RETURN oDLVServer:RecCount

METHOD __GetServerPos() AS INT STRICT 
	//PP-030828 Strong typing
	LOCAL iRet AS INT

	IF oDLVServer == NULL_OBJECT
		RETURN 0
	ENDIF

	IF lUseOrder
		iRet := Send(oDLVServer, #OrderKeyNo)
	ELSE
		iRet := oDLVServer:RecNo
	ENDIF

	RETURN iRet

METHOD __ItemChanged(oCtrlNotifyEvent AS ControlNotifyEvent) AS VOID STRICT 
	//PP-030828 Strong typing
	LOCAL nmlv AS _winNM_LISTVIEW

	

	nmlv := PTR(_CAST, oCtrlNotifyEvent:lParam)
	IF (_AND(nmlv:uChanged, LVIF_STATE) > 0) .AND. (_AND(nmlv:uNewState, LVIS_SELECTED) > 0)
		lNoNotifies := TRUE
		SELF:__SetServerPos(nmlv:iItem + 1)
		lNoNotifies := FALSE
	ENDIF
	RETURN

METHOD __NotifyChanges(kNotify AS DWORD) AS USUAL STRICT 
	//PP-030828 Strong typing
	
	RETURN NIL

METHOD __RecordChange(lDoSelect := NIL AS USUAL) AS VOID STRICT 
	//PP-030828 Strong typing
	LOCAL iItem AS INT

	Default(@lDoSelect, TRUE)

	IF lDoSelect
		iItem := SELF:__GetServerPos() - 1
		ListView_SetItemState(hwnd, DWORD(iItem), _OR(LVIS_SELECTED,LVIS_FOCUSED), _OR(LVIS_SELECTED,LVIS_FOCUSED))
		ListView_EnsureVisible(hwnd, iItem, 1)
	ENDIF
	RETURN

METHOD __RefreshData() AS VOID STRICT 
	//PP-030828 Strong typing
	LOCAL iOrderPos AS INT
	LOCAL r IS _winRECT

	iOrderPos := SELF:__GetServerPos()

	IF (iOrderPos >= iCacheStart) .AND. (iOrderPos <= iCacheEnd)
		SELF:__FillCacheItem(iOrderPos - iCacheStart + 1)
	ENDIF
	SendMessage(hwnd, LVM_GETITEMRECT, DWORD(_CAST, (iOrderPos - 1)), LONGINT(_CAST, @r))
	InvalidateRect(hwnd, @r, FALSE)
	RETURN

METHOD __RefreshField(uFieldName AS USUAL) AS VOID STRICT 
	//PP-030828 Strong typing
	SELF:__RefreshData()
	RETURN

METHOD __SetServerPos(nOrderPos AS INT, lSuspendNotify := NIL AS USUAL) AS INT STRICT 
	//PP-030828 Strong typing
	LOCAL iRet AS INT

	IF (oDLVServer == NULL_OBJECT)
		RETURN 0
	ENDIF

	Default(@lSuspendNotify, FALSE)
	IF (lSuspendNotify)
		oDLVServer:SuspendNotification()
	ENDIF

	IF lUseOrder
		IF Send(oDLVServer, #OrderKeyGoto, nOrderPos)
			iRet := nOrderPos
		ENDIF
	ELSE
		IF oDLVServer:GoTo(nOrderPos)
			iRet := nOrderPos
		ENDIF
	ENDIF

	IF (lSuspendNotify)
		oDLVServer:ResetNotification()
	ENDIF

	RETURN iRet

METHOD __StatusOK() AS OBJECT STRICT 
	//PP-030828 Strong typing
	

	RETURN NULL_OBJECT

METHOD __Unlink(oDS := NIL AS USUAL) AS Control  STRICT 
	//PP-030828 Strong typing
	

	IF (oDLVServer != NULL_OBJECT)
		oDLVServer:UnRegisterClient(SELF)
		oDLVServer := NULL_OBJECT
	ENDIF

	RETURN SELF

METHOD DeleteAll() 

	

	iCacheStart := -1
	iCacheEnd := -1

	RETURN SUPER:DeleteAll()

METHOD Destroy() 
	

	SELF:__Unlink()

	RETURN SUPER:Destroy()

METHOD FIELDGET(nFieldPos) 
	

	IF (oDLVServer != NULL_OBJECT)
		RETURN oDLVServer:FIELDGET(nFieldPos)
	ENDIF
	RETURN NIL

CONSTRUCTOR(oOwner, xID, oPoint, oDimension, kStyle) 
	LOCAL lUsedAsBrowser AS LOGIC
	LOCAL nStyle			AS LONGINT
   //SE-070427 removed PCount() for future Vulcan compatibility

	// We are used as a browser
	IF IsInstanceOfUsual(oOwner, #datawindow) .AND. xID = NIL
		oOwner 	:= oOwner:__FormWindow
		xID 		:= 99
		oPoint 	:= Point{0, 0}
		oDimension := Dimension{100,100}
		lUsedAsBrowser := TRUE
	ENDIF
	IF (IsNumeric(kStyle))
		nStyle := kStyle
	ENDIF
	nStyle := nStyle | WS_HSCROLL | LVS_SINGLESEL | LVS_REPORT | LVS_SHOWSELALWAYS| LVS_OWNERDATA 
	SUPER(oOwner, xID, oPoint, oDimension, nStyle)

	SELF:SetExLVStyle(_OR(LVS_EX_FULLROWSELECT, LVS_EX_GRIDLINES, LVS_EX_HEADERDRAGDROP), TRUE)

	IF lUsedAsBrowser
		SELF:ControlFont := Font{,9,"MS Sans Serif"}
	ENDIF

	iCacheMax := 100
	iCacheStart := 0
	iCacheEnd := 0

	RETURN 

METHOD Notify(kNotification, uDescription) 

	

	IF lNoNotifies
		IF kNotification == NOTIFYINTENTTOMOVE
			RETURN TRUE
		ELSE
			RETURN NIL
		ENDIF
	ENDIF

	DO CASE
	CASE kNotification == NOTIFYCOMPLETION
		// self:__NotifyChanges(GBNFY_COMPLETION)
		// nOldRecordNum := oDataServer:Recno
	CASE kNotification == NOTIFYINTENTTOMOVE
		// return self:__NotifyChanges(GBNFY_INTENTTOMOVE)
		//self:__refreshdata()
		RETURN TRUE
	CASE kNotification == NOTIFYFILECHANGE
		SELF:Refresh()
		// ASend(aColumn, #__Scatter)
		// self:__NotifyChanges(GBNFY_FILECHANGE)
		// ASend(aColumn, #__Scatter)
		// nOldRecordNum := oDataServer:Recno
	CASE kNotification == NOTIFYFIELDCHANGE
		SELF:__RefreshData()
		// self:__RefreshField(uDescription)
		// self:__NotifyChanges(GBNFY_FIELDCHANGE)
	CASE kNotification == NOTIFYCLOSE
		SELF:__Unlink()

	CASE (kNotification == NOTIFYRECORDCHANGE) .OR.;
		(kNotification == NOTIFYGOBOTTOM) .OR. ;
		(kNotification == NOTIFYGOTOP)
		// ASend(aColumn, #__Scatter)

		// if nOldRecordNum != oDataServer:Recno
		// self:__NotifyChanges(GBNFY_RECORDCHANGE)
		// ASend(aColumn, #__Scatter)
		// else
		// self:__NotifyChanges(GBNFY_FIELDCHANGE)
		// endif
		// nOldRecordNum := oDataServer:Recno
		SELF:__RecordChange()

	CASE (kNotification == NOTIFYDELETE) .OR. (kNotification == NOTIFYAPPEND)
		SELF:Refresh()
		// ASend(aColumn, #__Scatter)
		// self:__NotifyChanges(GBNFY_DODELETE)
		// ASend(aColumn, #__Scatter)
		// nOldRecordNum := oDataServer:Recno
	END CASE

	RETURN NIL

ACCESS Owner 
	

	IF IsInstanceOf(oParent, #__FormFrame)
		RETURN IVarGet(oParent, #DataWindow)
	ENDIF

	RETURN oParent

METHOD Refresh() 
	LOCAL dwItems AS DWORD

	

	dwItems := SELF:__GetServerCount
	SendMessage(hwnd, LVM_SETITEMCOUNT, 0, 1)
	SendMessage(hwnd, LVM_SETITEMCOUNT, dwItems, 1)
	iCacheStart := -1
	iCacheEnd := -1

	IF (dwItems > 0) .AND. (oDLVServer:RecNo > 0)
		oDLVServer:GoTo(oDLVServer:RecNo)
	ENDIF

	InvalidateRect(hwnd, NULL_PTR, TRUE)
	RETURN SELF

ACCESS Server 
	

	RETURN oDLVServer

ASSIGN Server(oNewServer)	
	

	IF (oDLVServer != oNewServer)
		IF (oDLVServer != NULL_OBJECT)
			oDLVServer:UnRegisterClient(SELF)
		ENDIF

		oDLVServer := oNewServer

		IF (ALen(aColumns) == 0)
			SELF:__AutoLayout()
		ENDIF

		oDLVServer:RegisterClient(SELF)
		lUseOrder := IsMethod(oDLVServer, #IndexOrd) .AND. (Send(oDLVServer, #IndexOrd) > 0)
		aCache := ArrayNew(iCacheMax, ALen(aColumns))
		SELF:Refresh()
	ENDIF

	RETURN 

METHOD Use(oNewServer) 
	

	SELF:Server := oNewServer

	RETURN (oDLVServer != NULL_OBJECT)

END CLASS

