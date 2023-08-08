// Segment.prg
// Created by    : robert
// Creation Date : 2/15/2023 6:36:28 PM
// Created for   :
// WorkStation   : NYX


USING System
USING System.Collections.Generic
USING System.Text
USING System.Diagnostics

BEGIN NAMESPACE XSharp.RDD.SqlRDD

/// <summary>
/// The Segment class.
/// </summary>
[DebuggerDisplay("{Key}")];
CLASS SqlDbSegment INHERIT SqlDbObject
    PROPERTY Key		    AS STRING AUTO
    PROPERTY Key1	        AS STRING AUTO
    PROPERTY Expression	    AS SqlDbExpression  AUTO
    PROPERTY Descending     AS LOGIC  AUTO
    PROPERTY KeyLen 	    AS LONG  AUTO
    PROPERTY HasFunctions   AS LOGIC  AUTO
    PROPERTY SQLKey	        AS STRING  AUTO
    PROPERTY OrderList	    AS IList<STRING> AUTO
    PROPERTY ColumnList	    AS IList<STRING> AUTO
    PROPERTY Widths		    AS IList<LONG> AUTO

    CONSTRUCTOR(oExpr as SqlDbExpression, cKey AS STRING, lDesc AS LOGIC)
        SUPER(cKey)
        SELF:Expression := oExpr
        SELF:Key        := cKey
        SELF:Descending := lDesc
        IF lDesc
            SELF:Key1   := "DESCEND("+cKey+")"
        ELSE
            SELF:Key1   := cKey
        ENDIF
        RETURN
    METHOD CalculateKeyLength AS VOID
        RETURN
END CLASS
END NAMESPACE // XSharp.RDD.SqlRDD
