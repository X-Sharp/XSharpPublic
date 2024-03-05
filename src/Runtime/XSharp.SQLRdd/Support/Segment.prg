// Segment.prg
// Created by    : robert
// Creation Date : 2/15/2023 6:36:28 PM
// Created for   :
// WorkStation   : NYX


using System
using System.Collections.Generic
using System.Text
using System.Diagnostics

begin namespace XSharp.RDD.SqlRDD

/// <summary>
/// The Segment class.
/// </summary>
[DebuggerDisplay("{Key}")];
internal class SqlDbSegment inherit SqlDbObject
    property Key		    as string auto
    property Key1	        as string auto
    property Expression	    as SqlDbExpression  auto
    property Descending     as logic  auto
    property KeyLen 	    as long  auto
    property HasFunctions   as logic  auto
    property SQLKey	        as string  auto
    property OrderList	    as IList<string> auto
    property ColumnList	    as IList<string> auto
    property Widths		    as IList<long> auto

    constructor(oExpr as SqlDbExpression, cKey as string, lDesc as logic)
        super(cKey)
        self:Expression := oExpr
        self:Key        := cKey
        self:Descending := lDesc
        if lDesc
            self:Key1   := "DESCEND("+cKey+")"
        else
            self:Key1   := cKey
        endif
        return
    method CalculateKeyLength as void
        return
end class
end namespace // XSharp.RDD.SqlRDD
