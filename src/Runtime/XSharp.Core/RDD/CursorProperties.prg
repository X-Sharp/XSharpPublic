USING System.Collections.Generic
USING System.Reflection
/// <summary>Enum that matches the various FoxPro cursor properties, used in CursorGetProp() and CursorSetProp()</summary>
/// <seealso cref="O:XSharp.VFP.Functions.CursorGetProp" />
/// <seealso cref="O:XSharp.VFP.Functions.CursorSetProp" />

enum XSharp.RDD.CursorProperty
    member ADOBookmark
    member ADOCodePage
    member ADORecordset
    member AllowSimultaneousFetch
    member AutoIncError
    member BatchUpdateCount
    member Buffering
    member CompareMemo
    member ConnectHandle
    member ConnectName
    member Database
    member FetchAsNeeded
    member FetchIsComplete
    member FetchMemo
    member FetchSize
    member KeyFieldList
    member MapBinary
    member MapVarchar
    member MaxRecords
    member ParameterList
    member Prepared
    member RecordsFetched
    member Refresh
    member SendUpdates
    member SourceName
    member SourceType
    member SQL
    member Tables
    member UpdatableFieldList
    member UpdateNameList
    member UpdateType
    member UseMemoSize
    member WhereType
END ENUM

INTERNAL GLOBAL cursorProperties AS Dictionary<STRING, LONG>

FUNCTION GetCursorProperty(propertyName as STRING) AS LONG
    IF cursorProperties == NULL
        cursorProperties := Dictionary<STRING, LONG>{StringComparer.OrdinalIgnoreCase}
        var values := System.Enum.GetValues(typeof(XSharp.RDD.CursorProperty))
        FOREACH var enumvalue in values
            var name := System.Enum.GetName(typeof(XSharp.RDD.CursorProperty), enumvalue)
            cursorProperties:Add(name, (LONG) enumvalue)
        NEXT
    ENDIF
    if cursorProperties:TryGetValue(propertyName, out var prop)
        return prop
    ENDIF
    RETURN -1


