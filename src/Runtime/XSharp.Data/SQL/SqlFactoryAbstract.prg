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

    /// <inheritdoc />
    VIRTUAL PROPERTY QuoteChar AS STRING GET ""
    /// <inheritdoc />
    VIRTUAL PROPERTY Name      AS STRING GET "AbstractFactory"

    /// <inheritdoc />
    VIRTUAL PROPERTY ParameterPrefix AS CHAR GET c"?"
    /// <inheritdoc />
    VIRTUAL PROPERTY ParameterNameInQuery AS LOGIC GET FALSE

    /// <inheritdoc />
    VIRTUAL PROPERTY CanCreateDataSourceEnumerator AS LOGIC GET oInstance:CanCreateDataSourceEnumerator




    CONSTRUCTOR
        oInstance := NULL

    /// <inheritdoc />
    VIRTUAL METHOD CreateConnection AS DbConnection
        RETURN oInstance:CreateConnection()

    /// <inheritdoc />
    VIRTUAL METHOD CreateCommand    AS DbCommand
        RETURN oInstance:CreateCommand()

    /// <inheritdoc />
    VIRTUAL METHOD CreateCommandBuilder    AS DbCommandBuilder
        RETURN oInstance:CreateCommandBuilder()

    /// <inheritdoc />
    VIRTUAL METHOD CreateParameter  AS DbParameter
        RETURN oInstance:CreateParameter()

    /// <inheritdoc />
    VIRTUAL METHOD CreateDataAdapter    AS DbDataAdapter
        RETURN oInstance:CreateDataAdapter()

    /// <inheritdoc />
    VIRTUAL METHOD CreateConnectionStringBuilder AS DbConnectionStringBuilder
        RETURN oInstance:CreateConnectionStringBuilder()

    /// <inheritdoc />
    VIRTUAL METHOD CreateDataSourceEnumerator() AS DbDataSourceEnumerator
        RETURN oInstance:CreateDataSourceEnumerator()

    /// <inheritdoc />
    VIRTUAL METHOD AfterConnect(oConnection AS DbConnection) AS VOID
        RETURN

    /// <inheritdoc />
    VIRTUAL METHOD BeforeConnect(cString AS STRING, cUser AS STRING, cPassword AS STRING) AS STRING
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
    VIRTUAL METHOD BeforeDisConnect(oConnection AS DbConnection) AS VOID
        RETURN

    /// <inheritdoc />
    VIRTUAL METHOD AfterDisConnect(oConnection AS DbConnection) AS VOID
        RETURN

    /// <inheritdoc />
    VIRTUAL METHOD BeforeRollBack(oConnection AS DbTransaction) AS VOID
        RETURN

    /// <inheritdoc />
    VIRTUAL METHOD AfterRollBack(oConnection AS DbTransaction) AS VOID
        RETURN

    /// <inheritdoc />
    VIRTUAL METHOD BeforeCommit(oConnection AS DbTransaction) AS VOID
        RETURN

    /// <inheritdoc />
    VIRTUAL METHOD AfterCommit(oConnection AS DbTransaction) AS VOID
        RETURN

    /// <inheritdoc />
    VIRTUAL METHOD GetName(oConn AS DbConnection) AS STRING
        RETURN oConn:GetType():FullName

    /// <inheritdoc />
    VIRTUAL METHOD DriverConnect(hWindow AS IntPtr, uCompletion AS OBJECT, cConnectionString AS OBJECT) AS STRING
        RETURN ""

    /// <inheritdoc />
    VIRTUAL METHOD EnhanceException(oEx AS System.Exception)  AS System.Exception
        RETURN oEx

    /// <inheritdoc />
    VIRTUAL METHOD HandleSpecialValue(oValue AS OBJECT, oFS AS OBJECT, lDateTimeAsDate AS LOGIC) AS OBJECT
        RETURN oValue

    /// <inheritdoc />
    VIRTUAL METHOD TranslateStatement(cStatement AS STRING) AS STRING
        RETURN cStatement

    /// <inheritdoc />
    VIRTUAL METHOD AfterOpen(oDataReader AS DbDataReader) AS DbDataReader
        RETURN oDataReader

    /// <inheritdoc />
    ABSTRACT METHOD GetMetaDataColumnValues(oRow AS DataRow) AS OBJECT[]

    /// <inheritdoc />
    ABSTRACT METHOD GetMetaDataTableValues(oRow AS DataRow) AS OBJECT[]



END CLASS

