// SQLRDD___Orders.prg
// Created by    : robert
// Creation Date : 2/8/2024 1:11:48 PM
// Created for   :
// WorkStation   : LEDA


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
    protect _currentOrder   as SqlDbOrder

    internal property CurrentOrder   as SqlDbOrder get _currentOrder set _currentOrder := value

    /// <inheritdoc />
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
            foreach var bag in self:IndexList
                if String.Compare(bag:FileName, bagName, true) == 0
                    oBag := bag
                    exit
                endif
            next
            if oBag == null
                oBag := SqlDbOrderBag{bagName, SELF}
                self:IndexList:Add(oBag)
            endif
            var oTag := SqlDbOrder{SELF, tagName, orderInfo:Expression, oBag}
            if orderInfo:OrdCondInfo != null
                if !String.IsNullOrEmpty(orderInfo:OrdCondInfo:ForExpression)
                    oTag:Condition := orderInfo:OrdCondInfo:ForExpression
                endif
            endif
            oBag:Tags:Add(oTag)
            CurrentOrder := oTag
            self:_hasData := false
            return TRUE
        else
            // The _creatingIndex flag is used to make sure that string fields are returned untrimmed
            self:_creatingIndex := TRUE
            result := super:OrderCreate(orderInfo)
            self:_creatingIndex := FALSE
        endif
        return result

    /// <inheritdoc />
    override method OrderListRebuild() as logic
        local result as logic
        if self:_tableMode == TableMode.Table
            // todo: Rebuild Orders in tablemode, query mode uses DBFVFP driver for indices
            result := false
            // todo: Create Order in tablemode, query mode uses DBFVFP driver for indices
            SELF:_dbfError( Subcodes.ERDD_CREATE_ORDER, Gencode.EG_CREATE)
        else
            // The _creatingIndex flag is used to make sure that string fields are returned untrimmed
            SELF:_creatingIndex := TRUE
            result := super:OrderListRebuild()
            SELF:_creatingIndex := FALSE
        endif
        return result

    override method OrderDestroy(orderInfo AS DbOrderInfo ) as logic
        local result as logic
        if self:_tableMode == TableMode.Table
            // todo: Rebuild Orders in tablemode, query mode uses DBFVFP driver for indices
            result := false
        else
            result := super:OrderDestroy(orderInfo)
        endif
        return result

    override method OrderListFocus(orderInfo as DbOrderInfo) as logic
        local result as logic
        if self:_tableMode == TableMode.Table
            self:_hasData := false
            result := self:_obuilder:OrderListFocus(orderInfo)
        else
            result := super:OrderListFocus(orderInfo)
        endif
        return result


    override method OrderListAdd( orderInfo AS DbOrderInfo) as logic
        local result as logic
        if self:_tableMode == TableMode.Table
            // todo: Add Order in TableMode
            result := false
        else
            result := super:OrderListAdd(orderInfo)
        endif
        return result
    override method OrderListDelete( orderInfo AS DbOrderInfo) as logic
        local result as logic
        if self:_tableMode == TableMode.Table
            // todo: Delete Order in TableMode
            result := false
        else
            result := super:OrderListDelete(orderInfo)
        endif
        return result

    override method OrderInfo(nOrdinal as dword , info as DbOrderInfo ) as object
        local isOk := true as logic
        local oBag := null as SqlDbOrderBag
        var hasBagName := ! String.IsNullOrEmpty(info:BagName)
        if self:_tableMode != TableMode.Table
            return super:OrderInfo(nOrdinal, info)
        endif
        local workOrder as SqlDbOrder
        if !info:IsEmpty
            workOrder := self:_obuilder:FindOrder(info)
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
                if hasBagName
                    info:Result := 0
                else
                    info:Result := self:IndexList:Count
                endif
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
            info:Result := self:IndexList:Count
        case DBOI_BAGNAME
            if IndexList:Count > 0
                if info:Order is long var nOrder
                    if nOrder >= 1 .and. nOrder <= IndexList:Count
                        var bag := self:IndexList[nOrder-1]
                        info:Result := bag:LogicalName
                    else
                        info:Result := ""
                    endif
                elseif workOrder != null
                    info:Result := workOrder:Name
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
                self:_hasData := false
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
                self:_hasData := false
            else
                info:Result := DBNull.Value
            endif
        otherwise
            super:OrderInfo(nOrdinal, info)
        end switch
        return info:Result
END CLASS
END NAMESPACE // XSharp.SQLRdd
