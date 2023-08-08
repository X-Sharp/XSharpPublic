//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


USING System
USING System.Collections.Generic
USING System.Text

BEGIN NAMESPACE XSharp.RDD.SqlRDD

/// <summary>
/// The Order class.
/// </summary>
CLASS SqlDbOrder INHERIT SqlDbObject
    PROPERTY Bag		AS OrderBag AUTO
    PROPERTY WorkArea	AS SQLRDD   AUTO
    PROPERTY KeyExp     AS STRING   AUTO
    PROPERTY AdoKey		AS STRING   AUTO
    PROPERTY cbExpr		AS OBJECT   AUTO
    PROPERTY uTopScope	AS OBJECT   AUTO
    PROPERTY uBotScope	AS OBJECT   AUTO
    PROPERTY Descending AS LOGIC    AUTO
    PROPERTY Unique    	AS LOGIC    AUTO
    PROPERTY KeyLength	AS DWORD    AUTO
    PROPERTY HasFunctions AS LOGIC  AUTO
    PROPERTY Segments	AS IList<SqlDbSegment>   AUTO
    PROPERTY ColumnList	AS IList<STRING>    AUTO
    PROPERTY OrderList	AS IList<STRING>    AUTO

    CONSTRUCTOR(oRDD AS SQLRDD, cName AS STRING, cIndexExpr AS STRING, oBag AS OrderBag)
        SUPER(cName)
        SELF:WorkArea       := oRDD
        SELF:KeyExp         := cIndexExpr
        SELF:Bag            := oBag
        VAR oExp            := SqlDbExpression{SELF,cIndexExpr}
        SELF:HasFunctions 	:= oExp:HasFunctions
        SELF:AdoKey			:= oExp:SQLKey
        SELF:OrderList 	    := oExp:OrderList
        SELF:ColumnList	    := oExp:ColumnList
        SELF:Segments		:= oExp:Segments

        RETURN

END CLASS
END NAMESPACE // XSharp.RDD.SqlRDD
