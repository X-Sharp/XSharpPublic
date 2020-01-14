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

    METHOD CreateConnection AS DbConnection
        RETURN oInstance:CreateConnection()
        
    METHOD CreateCommand    AS DbCommand
        RETURN oInstance:CreateCommand()

    METHOD CreateCommandBuilder    AS DbCommandBuilder
        RETURN oInstance:CreateCommandBuilder()

    METHOD CreateParameter  AS DbParameter
        RETURN oInstance:CreateParameter()
    METHOD CreateDataAdapter    AS DbDataAdapter
        RETURN oInstance:CreateDataAdapter()
    METHOD CreateConnectionStringBuilder AS DbConnectionStringBuilder
        RETURN oInstance:CreateConnectionStringBuilder()

    METHOD CreateDataSourceEnumerator() AS DbDataSourceEnumerator
        RETURN oInstance:CreateDataSourceEnumerator()
    METHOD AfterConnect(oConnection AS DbConnection) AS VOID
        RETURN


    METHOD BeforeConnect(cString AS STRING, cUser AS STRING, cPassword AS STRING) AS STRING
	    IF !cString:Contains("=") .AND. ! STRING.IsNullOrEmpty(cString)
			cString := "DSN="+cString+";" 
		ENDIF
		IF !STRING.IsNullOrEmpty(cUser)
			cString += "UID="+cUser+";"
		ENDIF
		IF !STRING.IsNullOrEmpty(cPassword)
			cString += "PWD="+cPassword+";"
		ENDIF
        RETURN cString
        
    METHOD BeforeDisConnect(oConnection AS DbConnection) AS VOID
        RETURN

    METHOD AfterDisConnect(oConnection AS DbConnection) AS VOID
        RETURN

    METHOD BeforeRollBack(oConnection AS DbTransaction) AS VOID
        RETURN

    METHOD AfterRollBack(oConnection AS DbTransaction) AS VOID
        RETURN

    METHOD BeforeCommit(oConnection AS DbTransaction) AS VOID
        RETURN

    METHOD AfterCommit(oConnection AS DbTransaction) AS VOID
        RETURN

    METHOD GetName(oConn AS DbConnection) AS STRING
        RETURN "ODBC"

    METHOD DriverConnect(hWindow AS object, uCompletion AS object, cConnectionString AS object) AS STRING
        RETURN ""

    METHOD EnhanceException(oEx AS SYstem.Exception)  AS SYstem.Exception
        RETURN oEx

    METHOD HandleSpecialValue(oValue AS OBJECT, oFS AS OBJECT, lDateTimeAsDate AS LOGIC) AS OBJECT
        RETURN oValue

    METHOD TranslateStatement(cStatement AS STRING) AS STRING
        RETURN cStatement


    METHOD AfterOpen(oDataReader AS DbDataReader) AS DbDataReader
        RETURN oDataReader

 


END CLASS

