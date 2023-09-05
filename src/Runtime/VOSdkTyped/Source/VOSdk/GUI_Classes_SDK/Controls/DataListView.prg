//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

// DataListview is a readonly DataBrowser for Now
using System.Windows.Forms
USING VOSDK := XSharp.VO.SDK
CLASS DataListView INHERIT DataBrowser
    CONSTRUCTOR(oOwner := NULL AS Window, xID:= 1000 AS LONG, oPoint:= NULL AS Point, oDimension := NULL AS Dimension)
        SUPER(oOwner, xID, oPoint, oDimension)
        SELF:SetStandardStyle(GBSREADONLY)
END CLASS

/*
class DataListView inherit ListView implements IDataBrowser
    protect oDLVServer as DataServer
    PROTECT lNoNotifies AS LOGIC
    protect aCache as array
    protect iCacheMax as int
    protect iCacheStart as int
    protect iCacheEnd as int
    protect lUseOrder as logic

    property ControlType as ControlType get ControlType.DataListView


    /// <exclude />
    method OnControlCreated(oC as IVOControl) as void
        var oGrid := (VODataListView) oC
        oGrid:RetrieveVirtualItem	+= __RetrieveVirtualItems
        oGrid:CacheVirtualItems	    += __CacheVirtualItems
        oGrid:SearchForVirtualItem  += __SearchForVirtualItems
        return


    /// <exclude />
    protected method __RetrieveVirtualItems(sender as object, e as RetrieveVirtualItemEventArgs) as void
        // e:Item
        // e:ItemIndex
        local oItem as VOSDK.ListViewItem
        local uValue as usual
        local first as logic
        self:__SetServerPos(e:ItemIndex+1, true)
        oItem := VOSDK.ListViewItem{}
        first := true
        foreach oCol as VOColumnHeader in __ListView:Columns
            local oColumn as ListViewColumn
            local symCol as symbol
            local oFs as FieldSpec
            local sValue as string
            oColumn := oCol:Tag
            symCol := oColumn:NameSym
            uValue := oDLVServer:FIELDGET(symCol)
            oFs    := oDLVServer:FieldSpec(symCol)
            oItem:SetValue(uValue, symCol)

            sValue := oFs:Transform(uValue)
            if first
                oItem:__ListViewItem:Text := sValue
                first := false
            else
                oItem:AddSubItem(sValue)
            endif
            oItem:SetText(sValue, oColumn:NameSym)
        next
        e:Item := (VOListViewItem) oItem:__ListViewItem
        return

    /// <exclude />


    protected method __CacheVirtualItems(sender as object, e as CacheVirtualItemsEventArgs ) as void
        // e:StartIndex
        // e:EndIndex
        local i as int
        local iDel as int
        local iStart, iEnd
        local iRecNoSave as long
        local iFrom, iTo as int


        iFrom := e:StartIndex+1
        iTo 	:= e:EndIndex +1
        if ((iTo - iFrom) > iCacheMax) .or.;
        	((iFrom >= iCacheStart) .and. (iTo <= iCacheEnd)) .or.;
        	(oDLVServer == null_object)
        	return
        endif

        oDLVServer:SuspendNotification()
        iRecNoSave := oDLVServer:RecNo

        if (iCacheMax - (iTo - iFrom + 1) >= 20)
        	iFrom := Max(1, iFrom - 10)
        	iTo += 10
        endif

        iStart := 1
        iEnd := (iTo - iFrom) + 1

        if (iFrom >= iCacheStart) .and. (iFrom <= iCacheEnd)
        	// reuse entries at end of cache
        	iDel := (iFrom - iCacheStart)
        	for i := 1 to (iDel)
        		ADel(aCache, 1)
        	next
        	SELF:__SetServerPos(iCacheEnd + 1, FALSE)
        	iStart := iCacheEnd - iFrom + 2
        elseif (iTo >= iCacheStart) .and. (iTo <= iCacheEnd)
        	// reuse entries as beginning of cache
        	iDel := (iCacheStart - iFrom)
        	for i := 1 to iDel
        		AIns(aCache, 1)
        	next
        	SELF:__SetServerPos(iFrom, FALSE)
        	iEnd := iDel
        else
        	SELF:__SetServerPos(iFrom, FALSE)
        endif

        iCacheStart := iFrom
        iCacheEnd := iTo

        for i:= iStart to iEnd
        	self:__FillCacheItem(i)
        	oDLVServer:Skip(1)
        	if oDLVServer:EoF
        		oDLVServer:Skip(-1)
        		iCacheEnd := self:__GetServerPos()
        		exit
        	endif
        next

        oDLVServer:GoTo(iRecNoSave)
        oDLVServer:ResetNotification()
        return

    /// <exclude />
    protected method __SearchForVirtualItems(sender as object, e as SearchForVirtualItemEventArgs ) as void
        // Todo: Implement __SearchForVirtualItems
        // Direction
        // IncludeSubitemsInSearch
        // Index
        // IsPrefixSearc
        // IsTextSearch
        // StartIndex
        // StartingPoint
        // Text
        return

    /// <exclude />
    property __DataListView as VODataListView get (VODataListView) oCtrl

    /// <exclude />
    method __AutoLayout() as void strict
        //PP-030828 Strong typing
        local oDF as DataField
        local oLVC as ListViewColumn
        local i as int
        if (oDLVServer == null_object)
            return
        endif

        self:DeleteAllColumns()
        VAR iColumns := (LONG) oDLVServer:FCount

        FOR i:= 1 TO iColumns
            oDF := oDLVServer:DataField(i)
            if (oDF == null_object)
                loop
            endif

            oLVC := ListViewColumn{oDF:FieldSpec:Length, oDF:HyperLabel}
            oLVC:FieldSpec := oDF:FieldSpec
            oLVC:Caption := oDF:FieldSpec:HyperLabel:Caption
            SELF:AddColumn(oLVC)
        NEXT

        RETURN

    /// <exclude />
    [Obsolete];
    method __AutoResize() as void strict
        // Handled inside DataForm Class
        return

    /// <exclude />
    property __ColumnCount as long get self:__ListView:Columns:Count

    method __FillCacheItem(iIndex as int) as void strict
        local j, cCols as dword
        local symCol as symbol
        local oFS as FieldSpec
        local sVal as string
        local oCol as ListViewColumn


        cCols := (dword) self:__ColumnCount
        if IsNil(aCache[iIndex])
            aCache[iIndex] := ArrayCreate(cCols)
        endif
        for j := 1 to cCols
            oCol    := self:GetColumn(j)
            symCol  := oCol:NameSym
            oFS     := oCol:FieldSpec
            if (oFS != null_object)
                sVal := oFS:Transform(self:FIELDGET(symCol))
            else
                sVal := AsString(self:FIELDGET(symCol))
            endif
            aCache[iIndex, j] := sVal
        next
        return

        //METHOD __FindItem(oCtrlNotifyEvent AS ControlNotifyEvent) AS INT STRICT
        //	//PP-030828 Strong typing
        //	LOCAL fi AS _winNMLVFINDITEM
        //	LOCAL iRet := -1 AS INT

        //	IF lUseOrder
        //		fi := PTR(_CAST, oCtrlNotifyEvent:lParam)
        //		// self:owner:owner:caption := "searching from "+NTrim(fi.iStart)+" for "+AsString(fi.lvfi._psz)
        //		oDLVServer:SuspendNotification()
        //		IF oDLVServer:Seek(AsString(fi:lvfi:_psz), TRUE)
        //			iRet := SELF:__GetServerPos()-1
        //		ENDIF
        //		oDLVServer:ResetNotification()
        //	ENDIF

        //	RETURN iRet

    /// <exclude />
    access __GetServerCount() as long strict
        //PP-030828 Strong typing
        if lUseOrder
            return Send(oDLVServer, #OrderKeyCount)
        endif
        return oDLVServer:RecCount

    /// <exclude />
    method __GetServerPos() as int strict
        //PP-030828 Strong typing
        local iRet as int

        if oDLVServer == null_object
            return 0
        endif

        if lUseOrder
            iRet := Send(oDLVServer, #OrderKeyNo)
        else
            iRet := oDLVServer:RecNo
        endif

        return iRet
    /// <exclude />
    method __NotifyChanges(kNotify as dword) as usual strict

        return nil

    /// <exclude />
    method __RecordChange(lDoSelect := nil as usual) as void strict
        local oLvItem		as VOListViewItem
        local iItem as int
        DEFAULT lDoSelect TO  true

        if lDoSelect
            iItem := self:__GetServerPos() - 1
            oLvItem := __ListView:Items[iItem]
            oLvItem:Selected := true
            oLvItem:Focused := true
            __ListView:EnsureVisible(iItem)
        endif
        return

    method __RefreshData() as void strict
        LOCAL iPos AS INT
        LOCAL iItem AS INT

        iPos := SELF:__GetServerPos()

        IF (iPos >= iCacheStart) .AND. (iPos <= iCacheEnd)
        	SELF:__FillCacheItem(iPos - iCacheStart + 1)
        ENDIF

        iItem := iPos - 1
        SELF:__ListView:RedrawItems(iItem, iItem, FALSE)

        return

    /// <exclude />
    method __RefreshField(uFieldName as usual) as void strict
        self:__RefreshData()
        return

    /// <exclude />
    METHOD __SetServerPos(nOrderPos AS INT, lSuspendNotify AS LOGIC) AS INT STRICT
        local iRet as int

        if (oDLVServer == null_object)
            return 0
        endif

        IF (lSuspendNotify)
            oDLVServer:SuspendNotification()
        endif

        if lUseOrder
            if Send(oDLVServer, #OrderKeyGoto, nOrderPos)
                iRet := nOrderPos
            endif
        else
            if oDLVServer:GoTo(nOrderPos)
                iRet := nOrderPos
            endif
        endif

        if (lSuspendNotify)
            oDLVServer:ResetNotification()
        endif

        return iRet

    /// <exclude />
    method __StatusOK() as object strict
        return null_object

    /// <exclude />
    method __Unlink(oDS := nil as usual) as VOSDK.Control  strict
        if (oDLVServer != null_object)
            oDLVServer:UnRegisterClient(self)
            oDLVServer := null_object
        endif

        return self
    /// <include file="Gui.xml" path="doc/DataListView.DeleteAll/*" />
    method DeleteAll() as logic

        iCacheStart := -1
        iCacheEnd := -1

        return super:DeleteAll()

    /// <include file="Gui.xml" path="doc/DataListView.Destroy/*" />
    method Destroy() as usual clipper
        self:__Unlink()
        return super:Destroy()

    /// <include file="Gui.xml" path="doc/DataListView.FieldGet/*" />
    method FieldGet(nFieldPos as usual) as usual


        if (oDLVServer != null_object)
            return oDLVServer:FIELDGET(nFieldPos)
        endif
        return nil

    /// <include file="Gui.xml" path="doc/DataListView.ctor/*" />
    constructor(oOwner , xID , oPoint , oDimension , kStyle )
        local lUsedAsBrowser as logic
        local nStyle			as longint

        // We are used as a browser
        if oOwner is Datawindow .and. xID == nil
            //oOwner 	:= ((DataWindow)oOwner):__FormWindow
            xID 	:= 99
            oPoint 	:= Point{0, 0}
            oDimension := Dimension{100,100}
            lUsedAsBrowser := true
        endif
        if IsNumeric(kStyle)
            nStyle := kStyle
        endif
        nStyle := (long) (nStyle| WS_HSCROLL | LVS_SINGLESEL | LVS_REPORT | LVS_SHOWSELALWAYS| LVS_OWNERDATA )
        super(oOwner, xID, oPoint, oDimension, nStyle)

        self:SetExLVStyle(_or(LVS_EX_FULLROWSELECT, LVS_EX_GRIDLINES, LVS_EX_HEADERDRAGDROP), true)

        if lUsedAsBrowser
            self:ControlFont := Font{,9,"MS Sans Serif"}
        endif

        iCacheMax := 100
        iCacheStart := 0
        iCacheEnd := 0
        SELF:__ListView:VirtualMode := TRUE
        SELF:FullRowSelect := TRUE

        return

    /// <include file="Gui.xml" path="doc/DataListView.Notify/*" />
    method Notify(kNotification, uDescription)

        if lNoNotifies
            if kNotification == NOTIFYINTENTTOMOVE
                return true
            else
                return nil
            endif
        endif

        do case
        case kNotification == NOTIFYCOMPLETION
            // self:__NotifyChanges(GBNFY_COMPLETION)
            // nOldRecordNum := oDataServer:Recno
            nop

        case kNotification == NOTIFYINTENTTOMOVE
            // return self:__NotifyChanges(GBNFY_INTENTTOMOVE)
            //self:__refreshdata()
            return true
        case kNotification == NOTIFYFILECHANGE
            self:Refresh()
            // ASend(aColumn, #__Scatter)
            // self:__NotifyChanges(GBNFY_FILECHANGE)
            // ASend(aColumn, #__Scatter)
            // nOldRecordNum := oDataServer:Recno
        case kNotification == NOTIFYFIELDCHANGE
            self:__RefreshData()
            // self:__RefreshField(uDescription)
            // self:__NotifyChanges(GBNFY_FIELDCHANGE)
        case kNotification == NOTIFYCLOSE
            self:__Unlink()

        case (kNotification == NOTIFYRECORDCHANGE) .or.;
                (kNotification == NOTIFYGOBOTTOM) .or. ;
                (kNotification == NOTIFYGOTOP)
            // ASend(aColumn, #__Scatter)

            // if nOldRecordNum != oDataServer:Recno
            // self:__NotifyChanges(GBNFY_RECORDCHANGE)
            // ASend(aColumn, #__Scatter)
            // else
            // self:__NotifyChanges(GBNFY_FIELDCHANGE)
            // endif
            // nOldRecordNum := oDataServer:Recno
            self:__RecordChange()

        case (kNotification == NOTIFYDELETE) .or. (kNotification == NOTIFYAPPEND)
            self:Refresh()
            // ASend(aColumn, #__Scatter)
            // self:__NotifyChanges(GBNFY_DODELETE)
            // ASend(aColumn, #__Scatter)
            // nOldRecordNum := oDataServer:Recno
        end case

        return nil


    /// <include file="Gui.xml" path="doc/DataListView.Refresh/*" />
    method Refresh() as void strict
        local dwItems as long
        dwItems := self:__GetServerCount
        self:__ListView:VirtualListSize := dwItems
        if (dwItems > 0) .and. (oDLVServer:RecNo > 0)
            oDLVServer:GoTo(oDLVServer:RecNo)
        endif
        self:__ListView:RedrawItems(0, dwItems-1, true)

        return

    /// <include file="Gui.xml" path="doc/DataListView.Server/*" />
    property Server as DataServer
        get
            return oDLVServer
        end get
        set


            if (oDLVServer != value)
                if (oDLVServer != null_object)
                    oDLVServer:UnRegisterClient(self)
                endif

                oDLVServer := value

                if (__ListView:Columns:Count == 0)
                    self:__AutoLayout()
                endif

                oDLVServer:RegisterClient(self)
                lUseOrder := IsMethod(oDLVServer, #IndexOrd) .and. (Send(oDLVServer, #IndexOrd) > 0)
                aCache := ArrayNew(iCacheMax, __ListView:Columns:Count)
                self:Refresh()
            endif

        end set
    end property
    /// <include file="Gui.xml" path="doc/DataListView.Use/*" />

    method GetSelectedItem() as ListViewItem
        return null_object

    method Use(oNewServer as DataServer) as logic
        self:Server := oNewServer
        RETURN (oDLVServer != NULL_OBJECT)


end class
*/
