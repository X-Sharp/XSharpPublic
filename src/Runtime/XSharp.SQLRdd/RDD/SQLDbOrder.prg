//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


using System
using System.Collections.Generic
using System.Linq
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

    property DbExpression   as SqlDbExpression auto get private set
    private _KeyCache     as IDictionary<object,dword>
    property KeyCodeBlock as ICodeblock auto get private set
    property ForCodeBlock as ICodeblock auto get private set
    property OrderBag     as SqlDbOrderBag auto
    property RDD    	  as SQLRDD   auto get private set
    property SQLKey		  as string   get DbExpression:SQLKey
    property cbExpr		  as object   auto get private set
    property uTopScope	  as object   auto get private set
    property uBotScope	  as object   auto get private set
    property Descending   as logic    auto get protected internal set
    property KeyLength	  as int      auto get private set
    property HasFunctions as logic    get DbExpression:HasFunctions
    property Conditional  as logic    get !String.IsNullOrEmpty(Condition)
    property TopScope     as object   auto get private set
    property BottomScope  as object   auto get private set
    property HasScopes    as logic    get TopScope != null .or. BottomScope != null
    property SqlWhere     as string   auto get private set
    property Segments	  as IList<SqlDbSegment>   get DbExpression:Segments
    property ColumnList	  as IList<string>    get DbExpression:ColumnList
    property OrderList	  as IList<string>    get DbExpression:OrderList
    property OrderListStr as string get DbExpression:OrderListStr
    property Provider     as ISqlDbProvider get RDD:Provider
    property Connection   as SqlDbConnection get RDD:Connection
    property FileName     as string get self:OrderBag:FileName+"_"+self:Name

    property KeyCache     as IDictionary<object,dword>
        get
            if _KeyCache == null
                _KeyCache := Dictionary<object,dword>{}
            endif
            return _KeyCache
        end get
    end property

    constructor(oRDD as SQLRDD, cName as string, cIndexExpr as string, oBag as SqlDbOrderBag)
        super(cName)
        self:RDD            := oRDD
        self:Expression     := cIndexExpr
        self:OrderBag       := oBag
        SELF:DbExpression    := SqlDbExpression{self,cIndexExpr}
        self:KeyCodeBlock  := self:RDD:Compile(cIndexExpr)
        self:ClearScopes()
        return
    end constructor

    /// <summary>Calculate the (maximum) length of the key value.</summary>
    /// <remarks>
    /// We evaluate the actual key expression (KeyCodeBlock) against the current record, with
    /// trimming of trailing spaces temporarily switched off. This gives the true maximum
    /// length regardless of what function(s) the expression applies (UPPER, LEFT, DTOS, DTOC,
    /// concatenation, ...), because it runs the real expression instead of guessing from
    /// column metadata. Disabling trimming matters because GetValue() would otherwise strip
    /// trailing spaces from the underlying field(s) before the expression sees them, which
    /// would understate the length whenever the current record's value happens to be shorter
    /// than the column's declared width.
    /// If no record is available to evaluate (e.g. an empty table) we fall back to summing the
    /// declared column widths - not exact for length-changing functions like LEFT()/DTOS(), but
    /// a safe (never too small) approximation.
    /// </remarks>
    method CalculateKeyLength() as void
        var lOldTrim := self:RDD:TrimValues
        self:RDD:TrimValues := false
        local result as object
        try
            result := self:RDD:EvalBlock(self:KeyCodeBlock)
        finally
            self:RDD:TrimValues := lOldTrim
        end try
        if result is string var strValue
            self:KeyLength := strValue:Length
        else
            self:KeyLength := self:CalculateKeyLengthFromMetadata()
        endif
        return
    end method

    private method CalculateKeyLengthFromMetadata() as int
        local nLen := 0 as int
        foreach var cCol in self:DbExpression:ColumnList
            var cName  := cCol:Trim(<char>{'[',']','"','`'})
            var oCol   := self:RDD:TableColumns:FirstOrDefault({ c => String.Compare(c:Name, cName, true) == 0 })
            if oCol != null
                nLen += (int) oCol:Length
            endif
        next
        if nLen == 0
            // Could not determine the width from metadata either - fall back to a value that
            // is always treated as "the search value may be shorter than the key".
            nLen := Int32.MaxValue
        endif
        return nLen
    end method

    method ClearScopes() as void
        self:TopScope      := null
        self:BottomScope   := null
        self:ClearCache()
        return
    end method
    
    METHOD ClearCache() as void
        SELF:_KeyCache      := null
    end method

    PROPERTY KeyValue as OBJECT GET SELF:RDD:EvalBlock(SELF:KeyCodeBlock)

    method SetCondition(cForExpr as string) as logic
        if ! String.IsNullOrEmpty(cForExpr)
            var oExp := SqlDbExpression{self,cForExpr}
            self:Condition  := cForExpr
            self:SqlWhere   := oExp:SQLKey
            self:ForCodeBlock := self:RDD:Compile(cForExpr)

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
                cFirst := self:RangeExpression(self:TopScope, false)
            endif
            if self:BottomScope != null
                // The bottom scope is an upper bound: keys up to (and starting with) the
                // bottom value. Building it like a soft seek used to produce a second lower
                // bound instead, so a range scope selected everything from the bottom value on.
                cLast := self:RangeExpression(self:BottomScope, true)
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
        if seekInfo:SoftSeek
            return self:RangeExpression(seekInfo:Value, false)
        endif
        return self:KeyCondition(seekInfo:Value, false, " = ")
    end method

    /// <summary>
    /// Condition for all keys on one side of oValue in the order's direction (a key that
    /// starts with oValue counts as matching). The lower bound (lUpper FALSE) is what a soft
    /// seek and a top scope need, the upper bound is what a bottom scope needs.
    /// </summary>
    method RangeExpression(oValue as object, lUpper as logic) as string
        // For a descending order the "lower" end in order sense holds the larger keys.
        local cComp := iif(lUpper != self:Descending, " <= ", " >= ") as string
        return self:KeyCondition(oValue, true, cComp)
    end method

    private method KeyCondition(oValue as object, lSoftSeek as logic, cComp as string) as string
        local cWhereClause as string
        if oValue is string var strValue
            var strLen := strValue:Length
            // Also for a value of the full key length: e.g. a relation seek on REF_NR+DTOS(DATUM)
            // as [REF_NR] = ... AND <date part> = ... is an index seek, the concatenation is not.
            var cColumnCondition := SELF:BuildColumnAwareCondition(strValue, lSoftSeek, cComp)
            if cColumnCondition != null
                return cColumnCondition
            endif
            if strLen < self:KeyLength
                // Fallback for keys we can't map onto individual columns (functions in the key
                // expression, unknown column metadata, ...): the original, always-correct
                // condition against the fully concatenated SQLKey expression. SQL Server cannot
                // use a normal index to seek into this - it has to compute the concatenation for
                // every row - so prefer the column-aware path above whenever possible.
                if (! lSoftSeek)
                    return self:SQLKey + " like " + Provider:LikePattern(strValue)
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
            cWhereClause := self:SQLKey+cComp+Functions.XsValueToSqlValue(oValue)
        endif
        return cWhereClause
    end method

    /// <summary>
    /// Build a seek/scope condition against the individual columns that make up a plain
    /// concatenated key (e.g. ALORT+NAMEUMLAUT), instead of against the fully concatenated
    /// SQLKey expression.
    /// </summary>
    /// <remarks>
    /// A condition like `[ALORT]+[NAMEUMLAUT] LIKE 'X%'` cannot use a normal index on
    /// (ALORT, NAMEUMLAUT, ...) - SQL Server has to evaluate the concatenation for every row.
    /// A value that covers one or more leading columns exactly can instead be expressed as
    /// `[ALORT] = 'X' AND [NAMEUMLAUT] LIKE 'Y%'`, which is a normal composite-index seek.
    /// Returns NULL when the key has functions in it, or column metadata/widths can't be
    /// determined - callers must fall back to the concatenation-based condition in that case.
    /// </remarks>
    private method BuildColumnAwareCondition(strValue as string, lSoftSeek as logic, cRangeComp as string) as string
        // The parts of a key that concatenates columns and DTOS(column); a DTOS() part is
        // compared as its yyyymmdd text, which is what the key holds at that position.
        var terms := self:DbExpression:KeyTerms
        if terms == null .or. terms:Count == 0
            return null
        endif
        var columns := List<string>{}
        var widths  := List<int>{}
        foreach var oTerm in terms
            if oTerm:IsDate
                widths:Add(8)
            else
                var cName := oTerm:Column:Trim(<char>{'[',']','"','`'})
                var oCol := self:RDD:TableColumns:FirstOrDefault({ c => String.Compare(c:Name, cName, true) == 0 })
                if oCol == null
                    return null
                endif
                widths:Add((int) oCol:Length)
            endif
            columns:Add(oTerm:SqlExpr)
        next

        // Split the value over the key columns: every column the value fully covers gets its
        // full-width part, the column where the value ends (if any) gets the remaining prefix.
        var parts    := List<string>{}
        var cRemaining := strValue
        for var i := 0 upto columns:Count-1
            if cRemaining:Length == 0
                exit
            endif
            var nWidth := widths[i]
            if cRemaining:Length >= nWidth
                parts:Add(cRemaining:Substring(0, nWidth))
                cRemaining := cRemaining:Substring(nWidth)
            else
                parts:Add(cRemaining)
                cRemaining := ""
            endif
        next
        if parts:Count == 0 .or. cRemaining:Length > 0
            // longer than the key: leave it to the condition on the whole key text
            return null
        endif

        var sb := StringBuilder{}
        if !lSoftSeek
            // Pin down every fully covered column with equality, and match the partial one
            // (if any) as a prefix - the same logic the concatenation fallback uses, just scoped
            // to the individual columns so the index can still be used.
            for var i := 0 upto parts:Count-1
                if sb:Length > 0
                    sb:Append(SqlDbProvider.AndClause)
                endif
                if parts[i]:Length >= widths[i]
                    sb:Append(columns[i] + " = " + Functions.XsValueToSqlValue(parts[i]))
                else
                    sb:Append(columns[i] + " like " + Provider:LikePattern(parts[i]))
                endif
            next
        else
            // A range on a concatenated key is a lexicographic comparison of the columns:
            //   (c1 > v1) OR (c1 = v1 AND c2 > v2) OR ... OR (c1 = v1 AND ... AND cn >= vn)
            // Pinning the leading columns down with equality instead would confine the range
            // to the rows that share the leading column values, so e.g. a soft seek past the
            // last key of such a group would find nothing instead of the next group.
            var cStrictComp := cRangeComp:Replace("=", "")
            for var i := 0 upto parts:Count-1
                if i > 0
                    sb:Append(" OR ")
                endif
                sb:Append("(")
                for var j := 0 upto i-1
                    sb:Append(columns[j] + " = " + Functions.XsValueToSqlValue(parts[j]))
                    sb:Append(SqlDbProvider.AndClause)
                next
                var cColExpr := columns[i]
                if parts[i]:Length < widths[i]
                    // Compare only the prefix of the column that the value actually covers.
                    var cSubstr := Provider:GetFunction("SUBSTR(%1%,%2%,%3%)")
                    cColExpr := cSubstr:Replace("%1%", cColExpr):Replace("%2%","1"):Replace("%3%", parts[i]:Length:ToString())
                endif
                var cComp := iif(i == parts:Count-1, cRangeComp, cStrictComp)
                sb:Append(cColExpr + cComp + Functions.XsValueToSqlValue(parts[i]))
                sb:Append(")")
            next
            if parts:Count > 1
                sb:Insert(0, "(")
                sb:Append(")")
            endif
        endif
        return sb:ToString()
    end method

end class
end namespace // XSharp.RDD.SqlRDD
