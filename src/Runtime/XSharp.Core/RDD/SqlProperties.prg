USING System.Collections.Generic
/// <summary>Enum that matches the various FoxPro Sql properties, used in SqlGetProp() and SqlSetProp()</summary>
/// <seealso cref="O:XSharp.VFP.Functions.SqlGetProp" />
/// <seealso cref="O:XSharp.VFP.Functions.SqlSetProp" />

ENUM XSharp.RDD.SQLProperty
    /// <include file="XSharp.CoreDocs.xml" path="doc/SQLProperty.Asynchronous/*" />
    member Asynchronous
    /// <include file="XSharp.CoreDocs.xml" path="doc/SQLProperty.BatchMode/*" />
    member BatchMode
    /// <include file="XSharp.CoreDocs.xml" path="doc/SQLProperty.ConnectBusy/*" />
    member ConnectBusy
    /// <include file="XSharp.CoreDocs.xml" path="doc/SQLProperty.ConnectString/*" />
    member ConnectString
    /// <include file="XSharp.CoreDocs.xml" path="doc/SQLProperty.ConnectTimeOut/*" />
    member ConnectTimeOut
    /// <include file="XSharp.CoreDocs.xml" path="doc/SQLProperty.DataSource/*" />
    member DataSource
    /// <include file="XSharp.CoreDocs.xml" path="doc/SQLProperty.DisconnectRollback/*" />
    MEMBER DisconnectRollback
    /// <include file="XSharp.CoreDocs.xml" path="doc/SQLProperty.DispLogin/*" />
    member DispLogin
    /// <include file="XSharp.CoreDocs.xml" path="doc/SQLProperty.DispWarnings/*" />
    member DispWarnings
    /// <include file="XSharp.CoreDocs.xml" path="doc/SQLProperty.IdleTimeout/*" />
    member IdleTimeout
    /// <include file="XSharp.CoreDocs.xml" path="doc/SQLProperty.NativeCommand/*" />
    member NativeCommand
    /// <include file="XSharp.CoreDocs.xml" path="doc/SQLProperty.ODBChdbc/*" />
    member ODBChdbc
    /// <include file="XSharp.CoreDocs.xml" path="doc/SQLProperty.ODBChstmt/*" />
    member ODBChstmt
    /// <include file="XSharp.CoreDocs.xml" path="doc/SQLProperty.PacketSize/*" />
    member PacketSize
    /// <include file="XSharp.CoreDocs.xml" path="doc/SQLProperty.Password/*" />
    member Password
    /// <include file="XSharp.CoreDocs.xml" path="doc/SQLProperty.QueryTimeOut/*" />
    member QueryTimeOut
    /// <include file="XSharp.CoreDocs.xml" path="doc/SQLProperty.Shared/*" />
    member Shared
    /// <include file="XSharp.CoreDocs.xml" path="doc/SQLProperty.Transactions/*" />
    member Transactions
    /// <include file="XSharp.CoreDocs.xml" path="doc/SQLProperty.UserId/*" />
    member UserId
    /// <include file="XSharp.CoreDocs.xml" path="doc/SQLProperty.WaitTime/*" />
    member WaitTime

END ENUM

INTERNAL GLOBAL sqlProperties AS Dictionary<STRING, LONG>

FUNCTION GetSQLProperty(propertyName as STRING) AS LONG
    IF sqlProperties == NULL
        sqlProperties := Dictionary<STRING, LONG>{StringComparer.OrdinalIgnoreCase}
        var values := System.Enum.GetValues(typeof(XSharp.RDD.SQLProperty))
        FOREACH var enumvalue in values
            var name := System.Enum.GetName(typeof(XSharp.RDD.SQLProperty), enumvalue)
            sqlProperties:Add(name, (LONG) enumvalue)
        NEXT
    ENDIF
    if sqlProperties:TryGetValue(propertyName, out var result)
        return result
    ENDIF
    RETURN -1
