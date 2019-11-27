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

CLASS SqlServerFactory INHERIT AbstractSqlFactory

    PROPERTY QuoteChar AS STRING GET chr(34)

    CONSTRUCTOR
        SUPER()
        oInstance := System.Data.SqlClient.SqlClientFactory.Instance


    METHOD GetName(oConn AS DbConnection) AS STRING
        RETURN "SQL"

    METHOD DriverConnect(hWindow AS USUAL, uCompletion AS USUAL, cConnectionString AS USUAL) AS STRING
        LOCAL oODBC AS OdbcFactory
        LOCAL cResult AS STRING
        oODBC := OdbcFactory{}
        cConnectionString := "Driver=SQL Server"
        cResult := SUPER:DriverConnect(hWindow, SQL_DRIVER_COMPLETE, cConnectionString)
        IF String.IsNullOrEmpty(cResult)
            RETURN cResult
        ENDIF
        VAR oBuilder := oODBC:CreateConnectionStringBuilder()
        oBuilder:ConnectionString := cResult
        oBuilder:Remove("Driver")
        RETURN oBuilder:ToString()

END CLASS

