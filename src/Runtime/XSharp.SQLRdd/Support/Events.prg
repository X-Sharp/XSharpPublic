//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System
using System.Collections.Generic
using System.Text

begin namespace XSharp.RDD.SqlRDD

/// <summary>
/// The SqlRddEvent class. Gets sent by the RDD to the App
/// </summary>
class SqlRddEventArgs
    /// <summary>
    /// Reason for the event
    /// </summary>
    property Reason as SqlRDDEventReason auto
    /// <summary>
    /// Table/Index/Tag for which the event is happening (if any)
    /// </summary>
    property Name  as string auto
    /// <summary>
    /// Default value calculated by the RDD
    /// </summary>
    property Value  as object auto
    internal constructor( nReason as SqlRDDEventReason, cName as string, oValue as object)
        self:Reason := nReason
        self:Value  := oValue
        self:Name   := cName
        return
    /// <summary>
    /// Return the value as a string or null when the value is not a string
    /// </summary>
    property StringValue as string
        get
            if self:Value is string var strValue
                return strValue
            endif
            return null
        end get
    end property
    /// <summary>
    /// Return the value as an integer or 0 when the value is not numeric
    /// </summary>
    property IntValue as long
        get
            if self:Value is long var intValue
                return intValue
            endif
            return 0
        end get
    end property
    /// <summary>
    /// Return the value as a list of strings or null when the value is not a list of strings
    /// </summary>
    property ListValue as List<string>
        get
            if self:Value is List<string> var listValue
                return listValue
            endif
            return null
        end get
    end property

end class
end namespace // XSharp.RDD.SqlRDD.SupportClasses
