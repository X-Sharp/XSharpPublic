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
/// The SqlDbEventObject class. This objectc is responsible for raising events
/// </summary>
class SqlDbEventObject inherit SqlDbHandleObject
    /// <summary>
    /// Create a new instance of the SqlDbEventObject class
    /// </summary>
    /// <param name="cName">Name for the object</param>
    constructor(cName as string)
        super(cName)
        return

    /// <summary>
    /// Event handler for the SqlRDD events
    /// </summary>
    public event CallBack as SqlRDDEventHandler
    internal method RaiseEvent(oObject as SqlDbObject, nEvent as SqlRDDEventReason, cTable as string, oValue as object) as object
        if String.IsNullOrEmpty(cTable)
            cTable := oObject:Name
        endif
        var oArgs := SqlRddEventArgs{ nEvent, cTable, oValue}
        if @@CallBack != null
            var result := @@CallBack ( oObject, oArgs )
            return result
        endif
        return oArgs:Value
    internal method RaiseStringEvent(oObject as SqlDbObject, nEvent as SqlRDDEventReason, cTable as string, oValue as string) as string
        var result := RaiseEvent(oObject, nEvent, cTable, oValue)
        if result is string var strValue
            return strValue
        endif
        return oValue
    internal method RaiseIntEvent(oObject as SqlDbObject, nEvent as SqlRDDEventReason, cTable as string, oValue as int) as int
        var result := RaiseEvent(oObject, nEvent, cTable, oValue)
        if result is int var intValue
            return intValue
        endif
        return oValue
    internal method RaiseListEvent(oObject as SqlDbObject, nEvent as SqlRDDEventReason, cTable as string, oValue as IList<string>) as IList<string>
        var result := RaiseEvent(oObject, nEvent, cTable, oValue)
        if result is IList<string> var listValue
            return listValue
        endif
        return oValue
    internal method RaiseLogicEvent(oObject as SqlDbObject, nEvent as SqlRDDEventReason, cTable as string, oValue as logic) as logic
        var result := RaiseEvent(oObject, nEvent, cTable, oValue)
        if result is logic var logValue
            return logValue
        endif
        return oValue

end class
end namespace
