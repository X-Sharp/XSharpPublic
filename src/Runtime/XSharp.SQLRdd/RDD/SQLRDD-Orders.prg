//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using XSharp.RDD.Enums
using XSharp.RDD.Support
using System.IO
using System.Collections.Generic
using System.Data
using System.Text
using System.Diagnostics
using System.Reflection
using System.Data.Common
using XSharp.RDD.SqlRDD.Providers
begin namespace XSharp.RDD.SqlRDD


partial class SQLRDD
    internal _currentOrder   as SqlDbOrder

    internal property CurrentOrder   as SqlDbOrder get _currentOrder set _currentOrder := value


    internal method FindOrderBag(bagName as string) as SqlDbOrderBag
        local result := null as SqlDbOrderBag
        bagName := System.IO.Path.GetFileNameWithoutExtension(bagName)
        foreach var bag in self:OrderBagList
            if String.Compare(bag:FileName, bagName, true) == 0
                result := bag
                exit
            endif
        next
        return result
    end method


    internal method FindOrder(orderInfo as DbOrderInfo) as SqlDbOrder
        local selectedBag := null as SqlDbOrderBag
        local order as SqlDbOrder
        var bagName   := orderInfo:BagName
        if !String.IsNullOrEmpty(bagName)
            selectedBag := self:FindOrderBag(bagName)
        endif
        order := null
        // A tag number can arrive as any integer type, not just LONG - VO code commonly passes
        // it as DWORD (e.g. from IndexCount()/DBOI_ORDERCOUNT, both DWORD-typed), and ADS/DBF
        // accepts that without complaint. Checking only "is long" made SetOrder(<a DWORD>)
        // silently fail here (falls through both branches, order stays null) even though the
        // tag genuinely exists - a real ADS-compatibility gap, not a missing/misconfigured tag.
        var lHasNumericOrder := false
        var iOrder := 0
        if orderInfo:Order != null
            try
                iOrder := Convert.ToInt32(orderInfo:Order)
                lHasNumericOrder := true
            catch
                lHasNumericOrder := false
            end try
        endif
        if lHasNumericOrder
            if selectedBag != null
                order := selectedBag:FindTag(iOrder)
            else
                foreach var bag in OrderBagList
                    order := bag:FindTag(iOrder)
                    if order != null
                        exit
                    endif
                    iOrder -= bag:Tags:Count
                next
            endif
        elseif orderInfo:Order is string var strOrder
            if selectedBag != null
                order := selectedBag:FindTagByName(strOrder)
            else
                foreach var bag in OrderBagList
                    order :=  bag:FindTagByName(strOrder)
                    if order != null
                        exit
                    endif
                next
            endif
        endif
        return order
    end method

	/// <summary>Create a new index or tag.</summary>
	/// <param name="info">An object containing information for order creation.</param>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
    override method OrderCreate(orderInfo as DbOrderCreateInfo ) as logic
        local result as logic
        if self:_tableMode == TableMode.Table
            var bagName     := orderInfo:BagName
            var tagName     := (string) orderInfo:Order
            if String.IsNullOrEmpty(bagName)
                bagName := System.IO.Path.GetFileNameWithoutExtension(SELF:_FileName)
            endif
            // Find orderbag or create one
            local oBag := null as SqlDbOrderBag
            foreach var bag in self:OrderBagList
                if String.Compare(bag:FileName, bagName, true) == 0
                    oBag := bag
                    exit
                endif
            next
            if oBag == null
                oBag := SqlDbOrderBag{SELF, bagName}
                self:OrderBagList:Add(oBag)
            endif
            var oTag := SqlDbOrder{SELF, tagName, orderInfo:Expression, oBag}
            if orderInfo:OrdCondInfo != null
                if !String.IsNullOrEmpty(orderInfo:OrdCondInfo:ForExpression)
                    oTag:Condition := orderInfo:OrdCondInfo:ForExpression
                endif
            endif
            // Now create the index on the server
            self:_builder:CreateIndex(oTag)

            oBag:Tags:Add(oTag)
            oBag:Save()
            CurrentOrder := oTag
            CurrentOrder:ClearScopes()
            self:_CloseCursor()
            self:Connection:MetadataProvider:CreateIndex(SELF:_cTable, orderInfo)
            return TRUE
        else
            self:_dbfError(Subcodes.EDB_CREATEINDEX, Gencode.EG_SYNTAX, "OrdCreate", "Cannot create an index on a result set from a SELECT statement")
            result := FALSE
        endif
        return result
    end method

	/// <summary>Rebuild all indexes for the current Workarea.</summary>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
    override method OrderListRebuild() as logic
        local result as logic
        if self:_tableMode == TableMode.Table
            result := true
        else
            // The _creatingIndex flag is used to make sure that string fields are returned untrimmed
            SELF:_creatingIndex := TRUE
            result := super:OrderListRebuild()
            SELF:_creatingIndex := FALSE
        endif
        return result

	/// <summary>Delete an index or tag.</summary>
	/// <param name="info">An object containing information about the order to remove.</param>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
    override method OrderDestroy(orderInfo AS DbOrderInfo ) as logic
        local result := false as logic
        if self:_tableMode == TableMode.Table
            // todo: Rebuild Orders in tablemode, query mode uses DBFVFP driver for indices
            var order := self:FindOrder(orderInfo)
            result := false
            if order != null
                // Now delete the index from the server
                result := self:_builder:DropIndex(order)
            endif

        else
            self:_dbfError(Subcodes.EDB_ORDDESTROY, Gencode.EG_SYNTAX, "OrderDestroy", "Cannot destroy an index on a result set from a SELECT statement")
        endif
        return result
    end method

    /// <summary>The column metadata of the underlying table, used to determine the
    /// declared (maximum) width of the columns that make up an index key expression.</summary>
    internal property TableColumns as IList<SqlDbColumnDef> get _oTd:Columns

    /// <summary>Whether string field values returned by GetValue() are right-trimmed
    /// (TRUE) or padded to the declared column width (FALSE). Exposed so that
    /// SqlDbOrder can temporarily disable trimming while probing the maximum
    /// length of an index key expression.</summary>
    internal property TrimValues as logic get _trimValues set _trimValues := value


	/// <summary>Set focus to another index in the list open indexes for the current Workarea.</summary>
	/// <param name="info">An object containing information about the order to select.</param>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
    override method OrderListFocus(orderInfo as DbOrderInfo) as logic
        local result := false as logic
        if self:_tableMode == TableMode.Table
            var currentRecord := SELF:RecNo
            // Flush any pending field changes on the CURRENTLY loaded row/order BEFORE
            // possibly tearing down the cursor further down. This must run even when the
            // requested order turns out to already be the active one (see the short-circuits
            // below) - GoCold() is cheap when nothing is dirty, so it is always safe to call
            // first. Skipping it in the "nothing to do" case would silently lose pending writes:
            // SetOrder()/RestDB() around a write is a standard "write outside the active
            // index/order" pattern used throughout the app, and it must keep flushing even when
            // the order doesn't actually change.
            SELF:GoCold()
            if (orderInfo:Order is long var nOrder0 .and. nOrder0 == 0) .or. ;
               (orderInfo:Order is string var cOrder0 .and. String.IsNullOrEmpty(cOrder0))
                // Order 0 (or an empty order name) means: switch back to the natural/physical
                // record order, i.e. no order at all. There is no "tag zero" to look up, so
                // this must always succeed - matching VO, which returns TRUE for SetOrder(0).
                if !(SELF:CurrentOrder == null .and. self:_hasData)
                    // Only reset the cursor when the order is actually changing, or when this
                    // is the table's first activation (no data loaded yet - _hasData false).
                    // _CloseCursor() forces the GoTo() below to pay for a full requery/recount
                    // via _ForceOpen(). Application code commonly calls SetOrder(0) defensively
                    // ("just in case") without checking the current order first - harmless under
                    // DBF, expensive here, and skipping the reset when nothing actually changes
                    // is safe: GoTo()/CalculateKeyLength() below still run unconditionally (see
                    // comment further down) so nothing is silently left stale.
                    self:_CloseCursor()
                endif
                SELF:CurrentOrder := null
                self:GoTo(currentRecord)
                result := true
            else
                var newOrder := self:FindOrder(orderInfo)
                if !(newOrder != null .and. newOrder == SELF:CurrentOrder .and. self:_hasData)
                    self:_CloseCursor()
                endif
                SELF:CurrentOrder := newOrder
                result := CurrentOrder != null
                IF result
                    SELF:CurrentOrder:ClearCache()
                    // Position on a record BEFORE calculating the key length: CalculateKeyLength()
                    // evaluates the key expression against the current record, which requires a
                    // loaded row. _CloseCursor() above cleared it, so without this GoTo there would
                    // be nothing to evaluate.
                    self:GoTo(currentRecord)
                    SELF:CurrentOrder:CalculateKeyLength()
                ENDIF
            endif
        else
            if orderInfo:Order != null
                if orderInfo:Order is long var nOrder .and. nOrder == 0
                    result := true
                elseif orderInfo:Order is string var cOrder .and. String.IsNullOrEmpty(cOrder)
                    result := true
                else
                    self:_dbfError(Subcodes.EDB_SETORDER, Gencode.EG_SYNTAX, "OrderListFocus", "Cannot select an index on a result set from a SELECT statement")
                    result := FALSE
                endif
            endif
        endif
        return result
    end method

	/// <summary>Open an index file and add to the list of open indexes for the current Workarea.</summary>
	/// <param name="info">An object containing information about the orderlist (file)  to add.</param>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
    override method OrderListAdd( orderInfo AS DbOrderInfo) as logic
        local result := false as logic
        if self:_tableMode == TableMode.Table
            if !String.IsNullOrEmpty(orderInfo:BagName)
                var bag := self:FindOrderBag(orderInfo:BagName)
                if bag != null
                    OrderBagList:Remove(bag)
                    bag:Close()
                endif
                if System.IO.File.Exists(orderInfo:BagName)
                    bag := SqlDbOrderBag{SELF, orderInfo:BagName}
                    if bag:Load()
                        SELF:OrderBagList:Add(bag)
                        result := true
                    endif
                endif
            endif
        else
            result := super:OrderListAdd(orderInfo)
        endif
        return result
    end method

	/// <summary>Close an index file and remove it from the list of open indexes for the current Workarea.</summary>
	/// <param name="info"></param>
	/// <param name="info">An object containing information about the orderlist (file) to delete.</param>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
    override method OrderListDelete( orderInfo AS DbOrderInfo) as logic
        local result := false as logic
        LOCAL oStruct := NULL AS SqlDbOrderBag
        if self:_tableMode == TableMode.Table
            if orderInfo:AllTags
                FOREACH oBag AS SqlDbOrderBag IN SELF:OrderBagList
                    IF ! oBag:ProductionIndex
                        oBag:Close()
                    ELSE
                        oStruct := oBag
                    ENDIF
                NEXT
                SELF:OrderBagList:Clear()
                IF oStruct != NULL
                    SELF:OrderBagList:Add(oStruct)
                ENDIF
            else
                IF ! String.IsNullOrEmpty(orderInfo:BagName)
                    VAR oBag := SELF:FindOrderBag(orderInfo:BagName)
                    IF oBag != NULL
                        oBag:Close()
                    ENDIF
                ELSE
                    var oTag := SELF:FindOrder(orderInfo)
                    IF oTag != NULL
                        VAR oBag := oTag:OrderBag
                        result :=  oBag:Close()
                    ENDIF
                endif
            endif
        else
            result := super:OrderListDelete(orderInfo)
        endif
        return result
    end method

	/// <summary>Retrieve information about an index.</summary>
	/// <param name="info">An object containing information about the order to retrieve the info for.</param>
	/// <param name="nOrdinal">Specifies the type of information to retrieve.</param>
    /// <returns>The requested value for the specified index.</returns>
    override method OrderInfo(nOrdinal as dword , info as DbOrderInfo ) as object
        local isOk := true as logic
        local oBag := null as SqlDbOrderBag
        if self:_tableMode != TableMode.Table
            return super:OrderInfo(nOrdinal, info)
        endif
        local workOrder as SqlDbOrder
        if !info:IsEmpty
            workOrder := SELF:FindOrder(info)
        else
            workOrder := self:CurrentOrder
        endif
        if workOrder != null
            oBag := workOrder:OrderBag
        endif
        begin switch nOrdinal
        case DBOI_DEFBAGEXT
            info:Result := SqlDbOrderBag.BAG_EXTENSION
        case DBOI_CONDITION
            if workOrder != null
                info:Result := workOrder:Condition
            else
                info:Result := ""
            endif
        case DBOI_EXPRESSION
            if workOrder != null
                info:Result := workOrder:Expression
            else
                info:Result := ""
            endif
        case DBOI_ORDERCOUNT
            if oBag == null
                var nCount := 0
                foreach var bag in Self:OrderBagList
                    nCount += bag:Tags:Count
                next
                info:Result := nCount
            else
                info:Result := oBag:Tags:Count
            endif
        case DBOI_FULLPATH
            if workOrder != null
                info:Result := workOrder:OrderBag:LogicalName
            else
                info:Result := String.Empty
            endif
        case DBOI_BAGCOUNT
            info:Result := self:OrderBagList:Count
        case DBOI_BAGNAME
            if OrderBagList:Count > 0
                if info:Order is long var nOrder
                    if nOrder >= 1 .and. nOrder <= OrderBagList:Count
                        var bag := self:OrderBagList[nOrder-1]
                        info:Result := bag:LogicalName
                    else
                        info:Result := ""
                    endif
                elseif workOrder != null
                    info:Result := workOrder:OrderBag:LogicalName
                else
                    info:Result :=String.Empty
                endif
            else
                info:Result :=String.Empty
            endif

        case DBOI_NAME
            if workOrder != null
                info:Result := workOrder:Name
            else
                info:Result := String.Empty
            endif
        case DBOI_ISDESC
            if workOrder != null
                var oldValue  := workOrder:Descending
                if info:Result is logic var descend
                    workOrder:Descending := descend
                endif
                info:Result := oldValue
            else
                info:Result := false
            endif
        case DBOI_ISCOND
            if workOrder != null
                info:Result := workOrder:Conditional
            else
                info:Result := false
            endif
        case DBOI_KEYSIZE
            if workOrder != null
                info:Result := workOrder:KeyLength
            else
                info:Result := 0
            endif
        case DBOI_KEYDEC
            info:Result := 0
        case DBOI_HPLOCKING
            info:Result := false
        case DBOI_UNIQUE
            if workOrder != null
                info:Result := workOrder:Unique
            else
                info:Result := false
            endif
        case DBOI_SETCODEBLOCK
            if workOrder != null
                info:Result := workOrder:KeyCodeBlock
            endif
        case DBOI_KEYVAL
            if workOrder != null
                isOk := true
                try
                    info:Result := workOrder:KeyValue
                catch ex as Exception
                    isOk := false
                    self:_dbfError(ex, Subcodes.EDB_EXPRESSION, Gencode.EG_SYNTAX, "DBFCDX.OrderInfo")
                end try
                if !isOk
                    info:Result := DBNull.Value
                endif
            else
                info:Result := DBNull.Value
            endif
        case DBOI_SCOPETOPCLEAR
        case DBOI_SCOPEBOTTOMCLEAR
            if workOrder != null
                var alreadyClear := (nOrdinal == DBOI_SCOPETOPCLEAR    .and. workOrder:TopScope    == null) .or. ;
                                     (nOrdinal == DBOI_SCOPEBOTTOMCLEAR .and. workOrder:BottomScope == null)
                if !alreadyClear
                    // Only reset the cursor when there was actually a scope to clear - re-clearing
                    // an already-clear scope (a common defensive pattern) would otherwise pay for a
                    // cursor reset for nothing under SqlRDD, unlike DBF where this is free.
                    workOrder:SetOrderScope(info:Result, (DbOrder_Info) nOrdinal)
                    self:_CloseCursor()
                endif
            endif
            info:Result := null
        case DBOI_SCOPETOP
        case DBOI_SCOPEBOTTOM
            if workOrder != null
                local oldValue as object
                if nOrdinal == DBOI_SCOPETOP
                    oldValue := workOrder:TopScope
                elseif nOrdinal == DBOI_SCOPEBOTTOM
                    oldValue := workOrder:BottomScope
                else
                    oldValue := DBNull.Value
                endif
                if info:Result != null .and. !Object.Equals(oldValue, info:Result)
                    // Only touch the cursor when the scope value actually changes - re-applying
                    // the same scope value (a common defensive pattern, e.g. re-scoping on every
                    // keystroke when the underlying value hasn't changed) would otherwise pay for
                    // a cursor reset for nothing under SqlRDD, unlike DBF where this is free.
                    workOrder:SetOrderScope(info:Result, (DbOrder_Info) nOrdinal)
                    self:_CloseCursor()
                endif
                info:Result := oldValue
            else
                info:Result := DBNull.Value
            endif
        case DBOI_NUMBER
            if workOrder != null
                var i := 0
                foreach var tag in oBag:Tags
                    i++
                    if tag == workOrder
                        info:Result := i
                        exit
                    endif
                next
            else
                info:Result := 0
            endif
        case DBOI_KEYCOUNT
            self:_ForceOpen()
            info:Result := self:OrderKeyCount
        case DBOI_POSITION
            // OrdKeyNo()/DBOI_POSITION reports the record's position within the current order.
            // When the cursor sits on a record outside the order (see GoTo()/_outsideOrder),
            // RowNumber/_currentPageNo just reflect the ad-hoc single-row buffer we loaded for
            // it, not a real position - matching DBF, that must report 0, not a bogus row number.
            if self:_outsideOrder
                info:Result := 0
            else
                info:Result := self:RowNumber + (self:_currentPageNo-1) * self:_oTd:PageSize
            endif
        case DBOI_RECNO
            // our position is the row number in the local cursor
            info:Result := self:RowNumber + (self:_currentPageNo-1) * self:_oTd:PageSize
        otherwise
            super:OrderInfo(nOrdinal, info)
        end switch
        return info:Result
    end method

   /// <summary>Perform a seek operation on the current selected index for the current Workarea.</summary>
    /// <param name="info">An object containing containing the necessary seek information.</param>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
    /// <remarks>The result of the actual seek operation is stored in the Found property of the RDD and the EOF property.</remarks>
    /// <remarks>
    /// When the area is in Tablemode, and no data has been read before, then this will trigger fetching the data from the database
    /// </remarks>
    override method Seek(seekInfo as DbSeekInfo) as logic
        local oKey as object
        // change behavior when all rows are read.
        // In that case we can search in the local buffer
        SELF:_Found := FALSE
        SELF:_SetEOF(FALSE)
        oKey := seekInfo:Value
        if oKey == null         // Seek NIL
            if seekInfo:Last
                return self:GoBottom()
            else
                return self:GoTop()
            endif
        endif
        if self:CurrentOrder == null
            self:_dbfError(Subcodes.ERDD_DATATYPE, Gencode.EG_NOORDER, "SQLRDD:Seek","No current Order" )
            return false
        endif
        var cSeekWhere := CurrentOrder:SeekExpression(seekInfo )

        SELF:_ClearTable()
        SELF:_currentPageNo := 1
        // Fetch a normal, full-size page here - NOT a single-row buffer. _FetchPage()'s
        // paging math ((CurrentPage-1) * PageSize) assumes every page, including this first
        // one, holds a full PageSize worth of rows; a caller that finds a match and then
        // walks forward with Skip() past this buffer (the common "seek to the first record
        // of a key, then Skip() while the key still matches" idiom) would otherwise jump
        // straight to absolute offset PageSize on the next fetch instead of to row 2,
        // silently skipping every other row that shares this seek's key.
        self:_OpenTable(cSeekWhere)

        // _OpenTable() can fail (e.g. the underlying SELECT errors out) and leave DataTable
        // null instead of an empty table - treat that the same as "no rows found" instead of
        // crashing on DataTable:Rows below, same fix as GoTo()/_ClearTable() already got.
        IF (SELF:DataTable == null .or. SELF:DataTable:Rows:Count = 0) .and. !seekInfo.SoftSeek
            SELF:GoTo(0)
            SELF:_Found := false
            SELF:_SetEOF(true)
            SELF:_SetBOF(false)
            return false
        ENDIF

        var nRec   := SELF:RecNo
        var lFound := SELF:GoTo(nRec)

        if lFound .and.;
            !SELF:_FilterInfo:Active .and.;
            !seekInfo.Last
            SELF:_SetEOF(false)
            SELF:_SetBOF(false)
            self:Found := True
            return true
        endif

        var uSeek       := seekInfo:Value
        var cbFilter    := SELF:_FilterInfo:FilterBlock
        var oDBOI       := DbOrderInfo{}
        var lData       := false
        WHILE(!SELF:EoF .AND. !SELF:BoF)
            if !SELF:_FilterInfo:Active .or. SELF:EvalFilter(cbFilter)
                if self:KeyCompare(self:OrderInfo(DBOI_KEYVAL,oDBOI),uSeek) = 0
                    lData   := true
                    nRec    := self:RecNo
                    if !seekInfo:Last
                        exit
                    endif
                elseif seekInfo.SoftSeek .and. !lData
                    nRec := self:RecNo
                    exit
                else
                    exit
                endif
            endif
            self:Skip(1)
        enddo
        self:GoTo(nRec)

        IF lFound
            SELF:_SetEOF(FALSE)
            SELF:_SetBOF(FALSE)
            self:Found  := (self:KeyCompare(self:OrderInfo(DBOI_KEYVAL,oDBOI),uSeek) = 0)
        ELSE
            SELF:GoTo(0)
            SELF:_SetEOF(TRUE)
            SELF:_SetBOF(FALSE)
            self:Found := false
        ENDIF

        return self:Found
    end method

    PRIVATE METHOD KeyCompare(oRecKey as OBJECT, oKey as OBJECT) AS LONG
        LOCAL result  AS INT64
        IF oRecKey IS NULL
            result := -1
        ELSEIF oRecKey IS STRING var strRecKey
            result := RuntimeState.StringCompare(strRecKey, oKey:ToString())
        ELSEIF oRecKey IS LONG var nRecKey
            result := nRecKey - (LONG) oKey
        ELSEIF oRecKey IS INT64 var i64RecKey
            result := i64RecKey - (INT64) oKey
        ELSEIF oRecKey IS IFloat var fRecKey
            result := (INT64) (fRecKey:Value - ((IFloat) oKey):Value)
        ELSEIF oRecKey IS IDate var dRecKey
            result := SELF:_DateCompare(dRecKey, (IDate) oKey)
         ELSEIF oRecKey IS DateTime var dtRecKey
            result := SELF:_DateTimeCompare(dtRecKey, (DateTime) oKey)
        ELSE
            result := 0
        ENDIF
        RETURN (LONG) result
    END METHOD
    PRIVATE METHOD _DateCompare(d1 as IDate, d2 as IDate) AS LONG
        LOCAL result AS LONG
        IF d1:Year != d2:Year
            result := d1:Year - d2:Year
        ELSEIF d1:Month != d2:Month
            result := d1:Month - d2:Month
        ELSE
            result := d1:Day - d2:Day
        ENDIF
        RETURN result
    END METHOD
    PRIVATE METHOD _DateTimeCompare(d1 as DateTime, d2 as DateTime) AS LONG
        return d1:CompareTo(d2)
    END METHOD

END CLASS
END NAMESPACE // XSharp.SQLRdd
