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


partial class SQLRDD inherit DBFVFP
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
        if orderInfo:Order is long var iOrder
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
            self:_CloseCursor()
            self:Connection:MetadataProvider:CreateIndex(SELF:_cTable, orderInfo)
            return TRUE
        else
            // The _creatingIndex flag is used to make sure that string fields are returned untrimmed
            self:_creatingIndex := TRUE
            result := super:OrderCreate(orderInfo)
            self:_creatingIndex := FALSE
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
        local result as logic
        if self:_tableMode == TableMode.Table
            // todo: Rebuild Orders in tablemode, query mode uses DBFVFP driver for indices
            var order := self:FindOrder(orderInfo)
            result := false
            if order != null
                // Now delete the index from the server
                result := self:_builder:DropIndex(order)
            endif

        else
            result := super:OrderDestroy(orderInfo)
        endif
        return result
    end method

	/// <summary>Set focus to another index in the list open indexes for the current Workarea.</summary>
	/// <param name="info">An object containing information about the order to select.</param>
    /// <returns><include file="CoreComments.xml" path="Comments/TrueOrFalse/*" /></returns>
    override method OrderListFocus(orderInfo as DbOrderInfo) as logic
        local result as logic
        if self:_tableMode == TableMode.Table
            self:_CloseCursor()
            var currentRecord := SELF:RecNo
            SELF:CurrentOrder := self:FindOrder(orderInfo)
            result := CurrentOrder != null
            if result .and. SELF:_recnoColumNo > -1
                self:GoTo(currentRecord)
            endif
        else
            result := super:OrderListFocus(orderInfo)
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
                info:Result := workOrder:OrderBag:FullPath
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
                    info:Result := self:EvalBlock(workOrder:KeyCodeBlock)
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
                workOrder:SetOrderScope(info:Result, (DbOrder_Info) nOrdinal)
                self:_CloseCursor()
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
                if info:Result != null
                    workOrder:SetOrderScope(info:Result, (DbOrder_Info) nOrdinal)
                endif
                info:Result := oldValue
                self:_CloseCursor()
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
            info:Result := self:RowCount
        case DBOI_POSITION
        case DBOI_RECNO
            // our position is the row number in the local cursor
            info:Result := self:RowNumber
        otherwise
            super:OrderInfo(nOrdinal, info)
        end switch
        return info:Result
    end method

END CLASS
END NAMESPACE // XSharp.SQLRdd
