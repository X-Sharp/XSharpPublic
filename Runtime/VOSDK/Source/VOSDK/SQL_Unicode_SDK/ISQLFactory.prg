//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Data.Common
USING System.Data
INTERFACE ISqlFactory

    PROPERTY QuoteChar AS STRING GET
    PROPERTY CanCreateDataSourceEnumerator AS LOGIC GET

    METHOD GetName(oConn AS DbConnection) AS STRING  

    METHOD CreateConnection AS DbConnection
    METHOD CreateCommand                        AS DbCommand
    METHOD CreateConnectionStringBuilder    AS DbConnectionStringBuilder
    METHOD CreateParameter                  AS DbParameter
    METHOD CreateDataAdapter                AS DbDataAdapter
    METHOD CreateDataSourceEnumerator() AS DbDataSourceEnumerator
    
    METHOD BeforeConnect(cString AS STRING, cUser AS STRING, cPassword AS STRING) AS STRING
    METHOD AfterConnect(oConnection AS DbConnection) AS VOID
       
    METHOD BeforeDisConnect(oConnection AS DbConnection) AS VOID
    METHOD AfterDisConnect(oConnection AS DbConnection) AS VOID

    METHOD BeforeRollBack(oConnection AS DbTransaction) AS VOID
    METHOD AfterRollBack(oConnection AS DbTransaction) AS VOID

    METHOD BeforeCommit(oConnection AS DbTransaction) AS VOID
    METHOD AfterCommit(oConnection AS DbTransaction) AS VOID

    METHOD DriverConnect(hWindow AS USUAL, nCompletion AS USUAL, cConnectionString AS USUAL) AS STRING
    METHOD EnhanceException(oEx AS SYstem.Exception)  AS SYstem.Exception
    METHOD HandleSpecialValue(oValue AS OBJECT,oFS AS FieldSpec, lDateTimeAsDate AS LOGIC) AS USUAL
    METHOD TranslateStatement(cStatement AS STRING) AS STRING
        
    METHOD AfterOpen(oDataReader AS DbDataReader) AS DbDataReader

    METHOD DotNetType2VOType(oSchema AS DataTable, oColumn AS DataColumn, cFieldName AS STRING, oFS REF FieldSpec) AS ARRAY

END INTERFACE    


STATIC GLOBAL oDefaultFactory AS ISqlFactory


FUNCTION SetSqlFactory(oFactory AS ISqlFactory) AS ISqlFactory
    LOCAL oOld AS ISqlFactory
    oOld := oDefaultFactory
    oDefaultFactory := oFactory
    RETURN oOld

FUNCTION GetSqlFactory() AS ISqlFactory
    LOCAL oOld AS ISqlFactory
    oOld := oDefaultFactory
    RETURN oOld

