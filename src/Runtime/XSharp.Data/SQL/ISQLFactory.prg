//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Data.Common
USING System.Data
USING XSharp.Data


/// <include file="XSharp.Data.Docs.xml" path="doc/ISqlFactory/*" />
INTERFACE XSharp.Data.ISqlFactory

    /// <include file="XSharp.Data.Docs.xml" path="doc/ISqlFactory.QuoteChar/*" />
    PROPERTY QuoteChar AS STRING GET
    /// <include file="XSharp.Data.Docs.xml" path="doc/ISqlFactory.Name/*" />
    PROPERTY Name      AS STRING GET

    /// <include file="XSharp.Data.Docs.xml" path="doc/ISqlFactory.CanCreateDataSourceEnumerator/*" />
    PROPERTY CanCreateDataSourceEnumerator AS LOGIC GET
    /// <include file="XSharp.Data.Docs.xml" path="doc/ISqlFactory.ParameterPrefix/*" />
    PROPERTY ParameterPrefix AS CHAR GET

    /// <include file="XSharp.Data.Docs.xml" path="doc/ISqlFactory.ParameterNameInQuery/*" />
    PROPERTY ParameterNameInQuery AS LOGIC GET

    /// <include file="XSharp.Data.Docs.xml" path="doc/ISqlFactory.GetName/*" />
    METHOD GetName(oConn AS DbConnection) AS STRING  

    /// <include file="XSharp.Data.Docs.xml" path="doc/ISqlFactory.CreateConnection/*" />
    METHOD CreateConnection AS DbConnection

    /// <include file="XSharp.Data.Docs.xml" path="doc/ISqlFactory.CreateCommand/*" />
    METHOD CreateCommand                        AS DbCommand

    /// <include file="XSharp.Data.Docs.xml" path="doc/ISqlFactory.CreateCommandBuilder/*" />
    METHOD CreateCommandBuilder    AS DbCommandBuilder  

    /// <include file="XSharp.Data.Docs.xml" path="doc/ISqlFactory.CreateConnectionStringBuilder/*" />
    METHOD CreateConnectionStringBuilder    AS DbConnectionStringBuilder

    /// <include file="XSharp.Data.Docs.xml" path="doc/ISqlFactory.CreateParameter/*" />
    METHOD CreateParameter                  AS DbParameter

    /// <include file="XSharp.Data.Docs.xml" path="doc/ISqlFactory.CreateDataAdapter/*" />
    METHOD CreateDataAdapter                AS DbDataAdapter

    /// <include file="XSharp.Data.Docs.xml" path="doc/ISqlFactory.CreateDataSourceEnumerator/*" />
    METHOD CreateDataSourceEnumerator() AS DbDataSourceEnumerator



    /// <include file="XSharp.Data.Docs.xml" path="doc/ISqlFactory.BeforeConnect/*" />
    METHOD BeforeConnect(cString AS STRING, cUser AS STRING, cPassword AS STRING) AS STRING

    /// <include file="XSharp.Data.Docs.xml" path="doc/ISqlFactory.AfterConnect/*" />
    METHOD AfterConnect(oConnection AS DbConnection) AS VOID

    /// <include file="XSharp.Data.Docs.xml" path="doc/ISqlFactory.BeforeDisConnect/*" />
    METHOD BeforeDisConnect(oConnection AS DbConnection) AS VOID

    /// <include file="XSharp.Data.Docs.xml" path="doc/ISqlFactory.AfterDisConnect/*" />
    METHOD AfterDisConnect(oConnection AS DbConnection) AS VOID

    /// <include file="XSharp.Data.Docs.xml" path="doc/ISqlFactory.BeforeRollBack/*" />
    METHOD BeforeRollBack(oTransaction AS DbTransaction) AS VOID

    /// <include file="XSharp.Data.Docs.xml" path="doc/ISqlFactory.AfterRollBack/*" />
    METHOD AfterRollBack(oTransaction AS DbTransaction) AS VOID

    /// <include file="XSharp.Data.Docs.xml" path="doc/ISqlFactory.BeforeCommit/*" />
    METHOD BeforeCommit(oTransaction AS DbTransaction) AS VOID

    /// <include file="XSharp.Data.Docs.xml" path="doc/ISqlFactory.AfterCommit/*" />
    METHOD AfterCommit(oTransaction AS DbTransaction) AS VOID

    /// <include file="XSharp.Data.Docs.xml" path="doc/ISqlFactory.DriverConnect/*" />
    METHOD DriverConnect(hWindow AS IntPtr, nCompletion AS OBJECT, cConnectionString AS OBJECT) AS STRING

    /// <include file="XSharp.Data.Docs.xml" path="doc/ISqlFactory.EnhanceException/*" />
    METHOD EnhanceException(oEx AS System.Exception)  AS System.Exception

    /// <include file="XSharp.Data.Docs.xml" path="doc/ISqlFactory.HandleSpecialValue/*" />
    METHOD HandleSpecialValue(oValue AS OBJECT,oFS AS OBJECT, lDateTimeAsDate AS LOGIC) AS OBJECT

    /// <include file="XSharp.Data.Docs.xml" path="doc/ISqlFactory.TranslateStatement/*" />
    METHOD TranslateStatement(cStatement AS STRING) AS STRING
        
    /// <include file="XSharp.Data.Docs.xml" path="doc/ISqlFactory.AfterOpen/*" />
    METHOD AfterOpen(oDataReader AS DbDataReader) AS DbDataReader

    /// <include file="XSharp.Data.Docs.xml" path="doc/ISqlFactory.GetMetaDataColumnValues/*" />
    METHOD GetMetaDataColumnValues(oRow AS DataRow) AS OBJECT[]
    
    /// <include file="XSharp.Data.Docs.xml" path="doc/ISqlFactory.GetMetaDataTableValues/*" />
    METHOD GetMetaDataTableValues(oRow AS DataRow) AS OBJECT[]

END INTERFACE    


/// <include file="XSharp.Data.Docs.xml" path="doc/SetSqlFactory/*" />
FUNCTION SetSqlFactory(oFactory AS ISqlFactory) AS ISqlFactory
    LOCAL oOld AS ISqlFactory
    oOld := XSharp.Data.AbstractSqlFactory.DefaultFactory
    XSharp.Data.AbstractSqlFactory.DefaultFactory := oFactory
    RETURN oOld

/// <include file="XSharp.Data.Docs.xml" path="doc/GetSqlFactory/*" />
FUNCTION GetSqlFactory() AS ISqlFactory STRICT
    LOCAL oOld AS ISqlFactory
    oOld := XSharp.Data.AbstractSqlFactory.DefaultFactory
    RETURN oOld

