// DatabaseMetadataProvider.prg
// Created by    : robert
// Creation Date : 1/31/2024 11:54:42 AM
// Created for   :
// WorkStation   : LEDA


USING System
USING System.Collections.Generic
USING System.Text

BEGIN NAMESPACE XSharp.RDD.SqlRDD.Providers

	/// <summary>
    /// The DatabaseMetadataProvider class. Reads Metadata from tables in the database.
    /// </summary>
    /// <remarks>
    /// It uses the following tables (when they exist)
    /// - xs_tableInfo
    /// - xs_indexInfo
    /// - xs_defaults
    /// </remarks>
	CLASS DatabaseMetadataProvider
        protect _connection AS SqlDbConnection
    CONSTRUCTOR(conn as SqlDbConnection)
         self:_connection := conn
         RETURN

	END CLASS
END NAMESPACE // XSharp.SQLRdd.Metadata
