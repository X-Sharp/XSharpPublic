//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


USING System
USING System.Collections.Generic
USING System.Text
using System.Diagnostics

BEGIN NAMESPACE XSharp.RDD.SqlRDD

/// <summary>
/// The Token class.
/// </summary>
[DebuggerDisplay("{Type} {Name,nq}")];
CLASS SqlDbToken
    PROPERTY Type       AS TokenType    AUTO GET INTERNAL SET
    PROPERTY Name       AS STRING       AUTO GET INTERNAL SET
    PROPERTY SQLName    AS STRING       AUTO GET SET
    PROPERTY PCount     AS LONG         AUTO GET SET
    PROPERTY Args       AS IList<OBJECT> AUTO GET SET
    PROPERTY Descend    AS LOGIC        AUTO GET SET
    PROPERTY NewSeg     AS LOGIC        AUTO GET SET

    CONSTRUCTOR(tokenType AS TokenType, name AS STRING)
        SELF:Type   := tokenType
        SELF:Name   := name
        SELF:SQLName := name
        SELF:Args  := List<Object>{}
        RETURN

    METHOD Func2String() AS STRING
        LOCAL i 		AS INT
        LOCAL sb        AS StringBuilder
        ~"ONLYEARLY+"
        sb := StringBuilder{SELF:SQLName}
        i := 1
        FOREACH var arg in Args
            sb:Replace("%"+i:ToString()+"%", arg:ToString())
            i++
        NEXT
        RETURN sb:ToString()

END CLASS
END NAMESPACE // XSharp.RDD.SqlRDD
