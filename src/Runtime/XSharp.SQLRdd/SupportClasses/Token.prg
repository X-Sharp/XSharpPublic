//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


using System
using System.Collections.Generic
using System.Text
using System.Diagnostics

begin namespace XSharp.RDD.SqlRDD

/// <summary>
/// The Token class.
/// </summary>
[DebuggerDisplay("{Type} {Name,nq}")];
class SqlDbToken
    property Type       as TokenType    auto get internal set
    property Name       as string       auto get internal set
    property SQLName    as string       auto get set
    property PCount     as long         auto get set
    property Args       as IList<object> auto get set
    property Descend    as logic        auto get set
    property NewSeg     as logic        auto get set

    constructor(tokenType as TokenType, name as string)
        self:Type   := tokenType
        self:Name   := name
        self:SQLName := name
        self:Args  := List<object>{}
        return

    method Func2String() as string
        local i 		as int
        local sb        as StringBuilder
        ~"ONLYEARLY+"
        sb := StringBuilder{self:SQLName}
        i := 1
        foreach var arg in Args
            sb:Replace("%"+i:ToString()+"%", arg:ToString())
            i++
        next
        return sb:ToString()

end class
end namespace // XSharp.RDD.SqlRDD
