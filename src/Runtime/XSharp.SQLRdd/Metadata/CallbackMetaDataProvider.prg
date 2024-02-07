// CallbackMetaDataProvider.prg
// Created by    : robert
// Creation Date : 1/31/2024 11:54:13 AM
// Created for   :
// WorkStation   : LEDA


USING System
USING System.Collections.Generic
USING System.Text

BEGIN NAMESPACE XSharp.RDD.SqlRDD.Providers

/// <summary>
/// The CallbackMetaDataProvider class. Provides metadata for the RDD from a callback function.
/// </summary>
CLASS CallbackMetaDataProvider
    protect _connection AS SqlDbConnection
    CONSTRUCTOR(conn as SqlDbConnection)
        self:_connection := conn
        RETURN

END CLASS
END NAMESPACE // XSharp.SQLRdd.Metadata
