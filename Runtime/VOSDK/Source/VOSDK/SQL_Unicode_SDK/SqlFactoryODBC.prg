//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System.Data
USING System.Data.Odbc
USING System.Data.Common
USING System.Reflection
USING System.Text
USING System.Runtime.InteropServices

PROCEDURE InitDefaultProvider INIT3
    SetSqlFactory(OdbcFactory{})



CLASS OdbcFactory INHERIT AbstractSqlFactory

    PROPERTY QuoteChar AS STRING GET ""

    CONSTRUCTOR
        SUPER()
        oInstance := System.Data.Odbc.OdbcFactory.Instance

    METHOD GetName(oConn AS DbConnection) AS STRING
        RETURN "ODBC"


    METHOD EnhanceException(oEx AS SYstem.Exception)  AS SYstem.Exception
        RETURN oEx

    METHOD HandleSpecialValue(oValue AS OBJECT, oFS AS FieldSpec, lDateTimeAsDate AS LOGIC) AS USUAL
        RETURN oValue

    METHOD TranslateStatement(cStatement AS STRING) AS STRING
        RETURN cStatement


    METHOD AfterOpen(oDataReader AS DbDataReader) AS DbDataReader
        RETURN oDataReader

END CLASS

