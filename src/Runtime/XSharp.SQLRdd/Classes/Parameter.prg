//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System
using System.Collections.Generic
using System.Text
using System.Data.Common
using System.Data

begin namespace XSharp.RDD.SqlRDD

/// <summary>
/// The SqlDbParameter class.
/// </summary>
class SqlDbParameter inherit SqlDbObject
    private oValue as object
    /// <summary>Ordinal position of the parameter</summary>
    property Ordinal        as long auto
    /// <summary>Value of the parameter</summary>
    property Value          as object
        get
            if DbParameter != null
                return DbParameter.Value
            else
                return oValue
            endif
        end get
        set
            if DbParameter != null
                DbParameter.Value := value
            else
                oValue := value
            endif
        end set
    end property
    /// <summary>Direction (Input, Output, InputOutput) </summary>
    property Direction      as ParameterDirection auto
    /// <summary>DbParameter object generated to pass the value to the Ado.Net dataprovider</summary>
    property DbParameter    as DbParameter auto

    constructor(nOrdinal as long, oValue as object)
        super(i"p{nOrdinal}")
        self:Ordinal    := nOrdinal
        self:Value      := oValue
        self:Direction  := ParameterDirection.Input
        return
    constructor(cName as string, oValue as object)
        super(cName)
        self:Ordinal    := -1
        self:Value      := oValue
        self:Direction  := ParameterDirection.Input
        return

end class
end namespace
