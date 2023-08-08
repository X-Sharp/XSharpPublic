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
/// The OrderBag class.
/// </summary>
CLASS OrderBag INHERIT List<SqlDbOrder>
    PROPERTY FileName           AS STRING AUTO
    PROPERTY ProductionIndex    AS LOGIC AUTO
    PROPERTY WorkArea	        AS SQLRDD AUTO

    CONSTRUCTOR(cName AS STRING, oArea AS SQLRDD)
        SUPER()
        SELF:FileName := cName
        SELF:WorkArea := oArea
        RETURN
    NEW METHOD Add(oOrder AS SqlDbOrder) AS VOID
        SUPER:Add(oOrder)
        //SELF:WorkArea:Orders:Add(oOrder)
        SELF:Save()
        RETURN
    METHOD Close AS LOGIC
        SELF:Clear()
        RETURN TRUE
    METHOD Load(aTags AS IList<STRING>) AS VOID
        RETURN
    METHOD Remove(nTagPos AS LONG)  AS OBJECT
        RETURN NULL
    METHOD Remove(cTagName AS STRING)  AS OBJECT
        RETURN NULL
    METHOD Save()  AS VOID
        RETURN
END CLASS
END NAMESPACE // XSharp.RDD.SqlRDD
DEFINE strTags := "Tags"
