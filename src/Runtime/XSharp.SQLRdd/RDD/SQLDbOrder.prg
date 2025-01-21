//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


using System
using System.Collections.Generic
using System.Text
using XSharp.RDD.Support
using XSharp.RDD.Enums
using XSharp.RDD.SqlRDD.Providers

begin namespace XSharp.RDD.SqlRDD

/// <summary>
/// The Order class.
/// </summary>
internal class SqlDbOrder inherit SqlDbObject

#include "..\Taginfo.xh"

    private _KeyCodeBlock as ICodeblock
    private _ForCodeBlock as ICodeblock
    private _Expression   as SqlDbExpression
    private _KeyCache     as IDictionary<object,Int>
    property KeyCodeBlock as ICodeblock get _KeyCodeBlock
    property ForCodeBlock as ICodeblock get _ForCodeBlock
    property OrderBag     as SqlDbOrderBag auto
    property RDD    	  as SQLRDD   auto
    property SQLKey		  as string   get _Expression:SQLKey
    property cbExpr		  as object   auto
    property uTopScope	  as object   auto
    property uBotScope	  as object   auto
    property Descending   as logic    auto
    property KeyLength	  as int      get _Expression:KeyLength
    property HasFunctions as logic    get _Expression:HasFunctions
    property Conditional  as logic    get !String.IsNullOrEmpty(Condition)
    property TopScope     as object   auto
    property BottomScope  as object   auto
    property HasScopes    as logic    get TopScope != null .or. BottomScope != null
    property SqlWhere     as string   auto
    property Segments	  as IList<SqlDbSegment>   get _Expression:Segments
    property ColumnList	  as IList<string>    get _Expression:ColumnList
    property OrderList	  as IList<string>    get _Expression:OrderList
    property OrderListStr as string get _Expression:OrderListStr
    property Provider     as ISqlDbProvider get RDD:Provider
    property Connection   as SqlDbConnection get RDD:Connection
    property FileName     as string get self:OrderBag:FileName+"_"+self:Name

    property KeyCache     as IDictionary<object,Int>
        get
            if _KeyCache == null
                _KeyCache := Dictionary<object,Int>{}
            endif
            return _KeyCache
        end get
    end property

    constructor(oRDD as SQLRDD, cName as string, cIndexExpr as string, oBag as SqlDbOrderBag)
        super(cName)
        self:RDD            := oRDD
        self:Expression     := cIndexExpr
        self:OrderBag       := oBag
        SELF:_Expression    := SqlDbExpression{self,cIndexExpr}
        self:_KeyCodeBlock  := self:RDD:Compile(cIndexExpr)
        self:ClearScopes()
        return
    end constructor

    method ClearScopes() as void
        self:TopScope      := null
        self:BottomScope   := null
        self:ClearCache()
        return
    end method
    METHOD ClearCache() as void
        SELF:_KeyCache      := null
    end method

    PROPERTY KeyValue as OBJECT GET SELF:RDD:EvalBlock(SELF:_KeyCodeBlock)

    method SetCondition(cForExpr as string) as logic
        if ! String.IsNullOrEmpty(cForExpr)
            var oExp := SqlDbExpression{self,cForExpr}
            self:Condition  := cForExpr
            self:SqlWhere   := oExp:SQLKey
            self:_ForCodeBlock := self:RDD:Compile(cForExpr)

            return true
        endif
        return false
    end method

    method VerifyKeyType(oExpr as OBJECT) AS OBJECT
        var conn := SELF:RDD:Connection
        var tmp := conn:UseNulls
        conn:UseNulls := FALSE
        var keyValue := SELF:KeyValue
        conn:UseNulls := tmp
        SWITCH keyValue
        CASE strValue as String
            IF oExpr is String
                RETURN oExpr
            ELSE
                RETURN oExpr:ToString()
            ENDIF
        CASE intValue as LONG
            IF oExpr is LONG
                RETURN oExpr
            ELSEIF oExpr is INT64 var i64Value
                return Convert.ToInt32(i64Value)
            ELSEIF oExpr is IFloat var flValue
                return Convert.ToInt32(flValue:Value)
            ENDIF
        CASE i64Value as INT64
            IF oExpr is INT64
                RETURN oExpr
            ELSEIF oExpr is INT var iValue
                return Convert.ToInt64(iValue)
            ELSEIF oExpr is IFloat var flValue
                return Convert.ToInt64(flValue:Value)
            ENDIF
        CASE dValue as IDate
            IF oExpr is IDate
                RETURN oExpr
            ELSEIF oExpr is DateTime var dtValue
                RETURN DbDate{dtValue:Year,dtValue:Month,dtValue:Day}
            ENDIF
        CASE dtValue as DateTime
            IF oExpr is DateTime
                RETURN oExpr
            ELSEIF oExpr is IDate var dValue
                RETURN DateTime{dValue:Year,dValue:Month,dValue:Day}
            ENDIF
        CASE flValue as IFloat
            IF oExpr is IFloat
                RETURN oExpr
            ELSEIF oExpr is LONG var lValue
                RETURN DbFloat{lValue,10,0}
            ELSEIF oExpr is INT64 var iValue
                RETURN DbFloat{iValue,10,0}
            ENDIF
        OTHERWISE
            NOP
        END SWITCH
        SELF:RDD:_dbfError( Subcodes.ERDD_VAR_TYPE, Gencode.EG_DATATYPE,SELF:FileName)
        RETURN NULL

    method SetOrderScope( oValue as object, nInfo as DbOrder_Info) as void
        switch nInfo
        case DbOrder_Info.DBOI_SCOPETOP
            self:TopScope := oValue
        case DbOrder_Info.DBOI_SCOPEBOTTOM
            self:BottomScope := oValue
        case DbOrder_Info.DBOI_SCOPETOPCLEAR
            self:TopScope := null
        case DbOrder_Info.DBOI_SCOPEBOTTOMCLEAR
            self:BottomScope := null
        end switch
        return
    end method

    method GetScopeClause() as string
        if ! self:HasScopes
            return ""
        endif
        var seekInfo       := DbSeekInfo{}
        if SELF:BottomScope != NULL .and. SELF:TopScope != NULL .and. self:BottomScope:ToString() == self:TopScope:ToString()
            // single scope
            seekInfo:Value     := self:TopScope
            seekInfo:SoftSeek  := false
            return self:SeekExpression(seekInfo)
        else
            local cFirst := null, cLast := null as string
            if self:TopScope != null
                seekInfo:Value    := self:TopScope
                seekInfo:SoftSeek := true
                cFirst := self:SeekExpression(seekInfo)
            endif
            if self:BottomScope != null
                seekInfo:Value    := self:BottomScope
                seekInfo:SoftSeek := true
                seekInfo:Last     := true
                cLast := self:SeekExpression(seekInfo)
            endif
            if String.IsNullOrEmpty(cFirst)
                return cLast
            endif
            if String.IsNullOrEmpty(cLast)
                return cFirst
            endif
            return cFirst + SqlDbProvider.AndClause+cLast
        endif
    end method

    method SeekExpression(seekInfo as DbSeekInfo) as string
        local cComp as string
        local cWhereClause as string
        if seekInfo:Last
            cComp := iif( seekInfo:SoftSeek, " <= ", " = " )
        else
            cComp := iif( seekInfo:SoftSeek, " >= ", " = " )
        endif
        if seekInfo:Value is string var strValue
            var strLen := strValue:Length
            if strLen < self:KeyLength
                if (! seekInfo:SoftSeek)
                    cComp := " like "
                    strValue += "%"
                    cWhereClause := self:SQLKey
                else
                    cWhereClause := Provider:GetFunction("SUBSTR(%1%,%2%,%3%)")
                    cWhereClause := cWhereClause:Replace("%1%", self:SQLKey)
                    cWhereClause := cWhereClause:Replace("%2%","1")
                    cWhereClause := cWhereClause:Replace("%3%", strLen:ToString())
                endif
            else
                cWhereClause := self:SQLKey
            endif
            cWhereClause += cComp + Functions.XsValueToSqlValue(strValue)
        else
            cWhereClause := self:SQLKey+cComp+Functions.XsValueToSqlValue(seekInfo:Value)
        endif
        return cWhereClause
    end method

end class
end namespace // XSharp.RDD.SqlRDD
