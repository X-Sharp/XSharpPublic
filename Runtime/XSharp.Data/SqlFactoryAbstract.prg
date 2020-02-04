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




    /// <summary>This is the base class for most ISqlFactory implementations.</summary>
    /// <remarks>This class creates connections, commands etc by calling the appropriate methods on a protected DbProvider instance.</remarks>

ABSTRACT CLASS XSharp.Data.AbstractSqlFactory IMPLEMENTS XSharp.Data.ISqlFactory
    #region Static
    STATIC PROPERTY DefaultFactory AS ISqlFactory AUTO
    STATIC CONSTRUCTOR
        DefaultFactory := OdbcFactory{}
    #endregion

    PROTECTED oInstance AS System.Data.Common.DbProviderFactory

    PROPERTY QuoteChar AS STRING GET ""

    PROPERTY CanCreateDataSourceEnumerator AS LOGIC GET oInstance:CanCreateDataSourceEnumerator


    CONSTRUCTOR
        oInstance := NULL

    /// <inheritdoc />
    METHOD CreateConnection AS DbConnection
        RETURN oInstance:CreateConnection()
        
    /// <inheritdoc />
    METHOD CreateCommand    AS DbCommand
        RETURN oInstance:CreateCommand()

    /// <inheritdoc />
    METHOD CreateCommandBuilder    AS DbCommandBuilder
        RETURN oInstance:CreateCommandBuilder()

    /// <inheritdoc />
    METHOD CreateParameter  AS DbParameter
        RETURN oInstance:CreateParameter()

    /// <inheritdoc />
    METHOD CreateDataAdapter    AS DbDataAdapter
        RETURN oInstance:CreateDataAdapter()

    /// <inheritdoc />
    METHOD CreateConnectionStringBuilder AS DbConnectionStringBuilder
        RETURN oInstance:CreateConnectionStringBuilder()

    /// <inheritdoc />
    METHOD CreateDataSourceEnumerator() AS DbDataSourceEnumerator
        RETURN oInstance:CreateDataSourceEnumerator()

    /// <inheritdoc />
    METHOD AfterConnect(oConnection AS DbConnection) AS VOID
        RETURN

    /// <inheritdoc />
    METHOD BeforeConnect(cString AS STRING, cUser AS STRING, cPassword AS STRING) AS STRING
	    IF !cString:Contains("=") .AND. ! String.IsNullOrEmpty(cString)
			cString := "DSN="+cString+";" 
		ENDIF
		IF !String.IsNullOrEmpty(cUser)
			cString += "UID="+cUser+";"
		ENDIF
		IF !String.IsNullOrEmpty(cPassword)
			cString += "PWD="+cPassword+";"
		ENDIF
        RETURN cString
        
    /// <inheritdoc />
    METHOD BeforeDisConnect(oConnection AS DbConnection) AS VOID
        RETURN

    /// <inheritdoc />
    METHOD AfterDisConnect(oConnection AS DbConnection) AS VOID
        RETURN

    /// <inheritdoc />
    METHOD BeforeRollBack(oConnection AS DbTransaction) AS VOID
        RETURN

    /// <inheritdoc />
    METHOD AfterRollBack(oConnection AS DbTransaction) AS VOID
        RETURN

    /// <inheritdoc />
    METHOD BeforeCommit(oConnection AS DbTransaction) AS VOID
        RETURN

    /// <inheritdoc />
    METHOD AfterCommit(oConnection AS DbTransaction) AS VOID
        RETURN

    /// <inheritdoc />
    METHOD GetName(oConn AS DbConnection) AS STRING
        RETURN "ODBC"

    /// <inheritdoc />
    METHOD DriverConnect(hWindow AS OBJECT, uCompletion AS OBJECT, cConnectionString AS OBJECT) AS STRING
        RETURN ""

    /// <inheritdoc />
    METHOD EnhanceException(oEx AS System.Exception)  AS System.Exception
        RETURN oEx

    /// <inheritdoc />
    METHOD HandleSpecialValue(oValue AS OBJECT, oFS AS OBJECT, lDateTimeAsDate AS LOGIC) AS OBJECT
        RETURN oValue

    /// <inheritdoc />
    METHOD TranslateStatement(cStatement AS STRING) AS STRING
        RETURN cStatement

    /// <inheritdoc />
    METHOD AfterOpen(oDataReader AS DbDataReader) AS DbDataReader
        RETURN oDataReader

 


END CLASS

