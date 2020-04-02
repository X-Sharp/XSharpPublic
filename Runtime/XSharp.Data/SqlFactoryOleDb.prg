//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Data
USING System.Data.SqlClient
USING System.Data.Common
USING System.Reflection
USING System.Text
USING System.Runtime.InteropServices

/// <summary>This is the class that implements a Factory to access data through the Ado.Net Microsoft SQL Server classes.</summary>


CLASS XSharp.Data.OleDbFactory INHERIT XSharp.Data.AbstractSqlFactory

    /// <inheritdoc />
    PROPERTY QuoteChar AS STRING GET chr(34)
    PROPERTY Name      AS STRING GET "OleDbFactory"
    
    CONSTRUCTOR
        SUPER()
        oInstance := System.Data.OleDb.OleDbFactory.Instance


    /// <inheritdoc />
    METHOD GetName(oConn AS DbConnection) AS STRING
        RETURN "OleDb"

    /// <inheritdoc />
    METHOD DriverConnect(hWindow AS OBJECT, uCompletion AS OBJECT, cConnectionString AS OBJECT) AS STRING

        RETURN ""

END CLASS

