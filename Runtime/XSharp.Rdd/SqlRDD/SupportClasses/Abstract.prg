//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


USING System
USING System.Collections.Generic
USING System.Text
USING System.Diagnostics

BEGIN NAMESPACE XSharp.RDD.SqlRDD

/// <summary>
/// The Abstract class.
/// </summary>
[DebuggerDisplay("{Name,nq}")];
CLASS SqlDbObject
#ifdef DEBUG
    STATIC PRIVATE nId := 0 AS LONG
    PROPERTY Id	  AS LONG AUTO GET PRIVATE SET
#endif
    PROPERTY Name AS STRING AUTO GET PRIVATE SET

    CONSTRUCTOR()
#ifdef DEBUG
        SELF:Id := ++nId
#endif
        RETURN
    CONSTRUCTOR(cName AS STRING)
        SELF()
        SELF:Name := cName
        RETURN

    OVERRIDE METHOD ToString() AS STRING
        RETURN SELF:Name

    INTERNAL METHOD SetName(cName as STRING) AS VOID
        SELF:Name := cName
        RETURN

END CLASS
END NAMESPACE // XSharp.RDD.SqlRDD
