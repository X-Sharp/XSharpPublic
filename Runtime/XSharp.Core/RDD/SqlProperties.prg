USING System.Collections.Generic
/// <summary>Enum that matches the various FoxPro Sql properties, used in SqlGetProp() and SqlSetProp()</summary>
/// <seealso "O:XSharp.VFP.Functions.SqlGetProp" />
/// <seealso "O:XSharp.VFP.Functions.SqlSetProp" />

ENUM XSharp.RDD.SQLProperty
    /// <summary>Specifies whether result sets are returned synchronously.</summary>
    MEMBER Asynchronous       
    /// <summary>Specifies whether SQLEXEC( ) returns result sets all at once (True (.T.), the default), or individually with SQLMORERESULTS( ) (False (.F.)).</summary>
    MEMBER BatchMode         
    /// <summary>Contains True (.T.) if a shared connection is busy; otherwise contains False (.F.).</summary>
    MEMBER ConnectBusy       
    /// <summary>The login connection string.</summary>
    MEMBER ConnectString     
    /// <summary>Specifies the time to wait (in seconds) before returning a connection time-out error. If you specify 0, the wait is indefinite and a time-out error is never returned. ConnectTimeOut can be 0 to 600. The default is 15.</summary>
    MEMBER ConnectTimeOut    
    /// <summary>The name of the data source as defined in the ODBC.INI file.</summary>
    MEMBER DataSource        
    /// <summary>Specifies if a pending transaction is committed or rolled back when SQLDISCONNECT( ) is called for the last connection handle.</summary>
    MEMBER DisconnectRollback
    /// <summary>Contains a numeric value that determines when the ODBC Login dialog box is displayed.</summary>
    MEMBER DispLogin         
    /// <summary>Specifies if error messages are displayed (True (.T.)) or are not displayed (False (.F.), the default).</summary>
    MEMBER DispWarnings      
    /// <summary>The idle timeout interval in minutes. Active connections are deactivated after the specified time interval. The default value is 0 (wait indefinitely).</summary>
    MEMBER IdleTimeout       
    /// <summary>The native commandtext in the SQLStatement object.</summary>
    MEMBER NativeCommand     
    /// <summary>The internal ODBC connection, which may be used by external code to call ODBC.</summary>
    /// <remarks>In X# this does not return the connection handle but the DbConnection object !</remarks>
    MEMBER ODBChdbc          
    /// <summary>The internal ODBC statement , which may be used by external code to call ODBC.</summary>
    /// <remarks>In X# this does not return the statement handle but the DbCommand object !</remarks>
    MEMBER ODBChstmt         
    /// <summary>The size of the network packet used by the connection. Adjusting this value can improve performance. The default value is 4096 bytes (4K).</summary>
    MEMBER PacketSize        
    /// <summary>The connection password.</summary>
    MEMBER Password          
    /// <summary>Specifies the time to wait (in seconds) before returning a general time-out error. If you specify 0 (the default), the wait is indefinite and a time-out error is never returned. QueryTimeOut can be 0 to 600.</summary>
    MEMBER QueryTimeOut      
    /// <summary>Specifies whether the underlying connection is a shared connection (True (.T.)), or not (False (.F.)).</summary>
    MEMBER Shared            
    /// <summary>Contains a numeric value that determines how the connection manages transactions on the remote table.</summary>
    MEMBER Transactions      
    /// <summary>The user identification.</summary>
    MEMBER UserId            
    /// <summary>The amount of time in milliseconds that elapses before Visual FoxPro checks if the SQL statement has completed executing. The default is 100 milliseconds.</summary>
    MEMBER WaitTime          
    
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
    IF sqlProperties:ContainsKey(propertyName)
        return sqlProperties[propertyName]
    ENDIF
    RETURN -1
