//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Collections.Generic 
USING System
USING XSharp.VFP
USING XSharp.Data

/// <include file="VFPDocs.xml" path="Runtimefunctions/sqlconnect/*" />
/// <seealso cref="M:XSharp.VFP.Functions.SqlDisconnect(System.Int32)">SqlDisconnect()</seealso>
/// <seealso cref="M:XSharp.VFP.Functions.SqlStringConnect(XSharp.__Usual,XSharp.__Usual)">SqlStringConnect()</seealso>
/// <seealso cref="M:XSharp.VFP.Functions.SqlGetProp(System.Int32,System.String)">SqlGetProp()</seealso>
/// <seealso cref="M:XSharp.VFP.Functions.SqlSetProp(System.Int32,System.String,XSharp.__Usual)">SqlSetProp()</seealso>
/// <seealso cref="M:XSharp.VFP.Functions.SqlExec(System.Int32,XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)">SqlExec()</seealso>

FUNCTION SqlConnect(uSource AS USUAL, cUserID := NIL AS USUAL, cPassword := NIL AS USUAL, lShared := NIL AS USUAL) AS LONG
    // uSource may be either a DSN or a nStatementHandle
    LOCAL nHandle := -1 AS LONG
    LOCAL cSource AS STRING
    LOCAL oStmt AS XSharp.VFP.SQLStatement
    IF IsNumeric(uSource)
        IF ! IsNil(cUserID) .OR. ! IsNil(cPassword) .OR. ! IsNil(lShared)
           THROW Error{"Number or type of parameters is not correct"}
        ENDIF
        nHandle     := uSource
        oStmt := SQLSupport.FindStatement(nHandle)
        IF oStmt == NULL_OBJECT
            THROW Error{"Statement Handle ("+nHandle:ToString()+") is invalid"}
        ENDIF
        // This should open a connection automatically since the previous connection as already validated
        IF ! oStmt:Connection:Shared
            THROW Error{"Statement Handle ("+nHandle:ToString()+") does not have a shared connection"}
        ENDIF
        oStmt := XSharp.VFP.SQLStatement{oStmt:Connection}
        nHandle := SQLSupport.AddStatement(oStmt)

    ELSEIF IsString(uSource)
        // if one or more parameters are missing then the connect dialog is shown
        cSource := uSource
        IF ! IsString(cUserID) ; cUserID := "" ; ENDIF
        IF ! IsString(cPassword) ; cPassword := "" ; ENDIF
        IF ! IsLogic(lShared) ; lShared := TRUE ; ENDIF
        oStmt := XSharp.VFP.SQLStatement{cSource, cUserID, cPassword, lShared}
        nHandle := SQLSupport.AddStatement(oStmt)
    ENDIF
    RETURN nHandle
    

/// <include file="VFPDocs.xml" path="Runtimefunctions/sqlstringconnect/*" />
/// <seealso cref="M:XSharp.VFP.Functions.SqlDisconnect(System.Int32)">SqlDisconnect()</seealso>
/// <seealso cref="M:XSharp.VFP.Functions.SqlConnect(XSharp.__Usual,XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)">SqlConnect()</seealso>
/// <seealso cref="M:XSharp.VFP.Functions.SqlExec(System.Int32,XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)">SqlExec()</seealso>
/// <seealso cref="M:XSharp.VFP.Functions.SqlGetProp(System.Int32,System.String)">SqlGetProp()</seealso>
/// <seealso cref="M:XSharp.VFP.Functions.SqlSetProp(System.Int32,System.String,XSharp.__Usual)">SqlSetProp()</seealso>

FUNCTION SqlStringConnect( uSharedOrConnectString , lSharable) AS LONG
    //  uSharedOrConnectString may be either lShared or a connection string
    // in FoxPro when passing lShared as TRUE then the "Select Connection or Data Source Dialog Box"  is shown
    // This is the normal 'DriverConnect' dialog for ODBC drivers
    LOCAL nHandle := -1 AS LONG
    LOCAL oStmt AS XSharp.VFP.SQLStatement
    IF IsLogic(uSharedOrConnectString)
        // show connection string dialog
       oStmt := XSharp.VFP.SQLStatement{"", uSharedOrConnectString}
    ELSEIF IsString(uSharedOrConnectString)
       IF ! IsLogic(lSharable) ; lSharable := TRUE ; ENDIF
       oStmt := XSharp.VFP.SQLStatement{uSharedOrConnectString,lSharable}
    ELSE
       THROW Error{"Number or type of parameters is not correct"}
    ENDIF
    IF oStmt:Connected
        nHandle := SQLSupport.AddStatement(oStmt)
    ENDIF
    RETURN nHandle
  
    



/// <include file="VFPDocs.xml" path="Runtimefunctions/sqlcancel/*" />
/// <seealso cref="M:XSharp.VFP.Functions.SqlConnect(XSharp.__Usual,XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)">SqlConnect()</seealso>
/// <seealso cref="M:XSharp.VFP.Functions.SqlExec(System.Int32,XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)">SqlExec()</seealso>
/// <seealso cref="M:XSharp.VFP.Functions.SqlSetProp(System.Int32,System.String,XSharp.__Usual)">SqlSetProp()</seealso>
FUNCTION SqlCancel( nStatementHandle AS LONG) AS LONG
    VAR oStmt := GetStatement(nStatementHandle)    
    IF oStmt != NULL
        oStmt:Cancel()
        RETURN 1
    ENDIF
    RETURN -1


/// <include file="VFPDocs.xml" path="Runtimefunctions/sqldisconnect/*" />
/// <seealso cref="M:XSharp.VFP.Functions.SqlStringConnect(XSharp.__Usual,XSharp.__Usual)">SqlStringConnect()</seealso>
/// <seealso cref="M:XSharp.VFP.Functions.SqlConnect(XSharp.__Usual,XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)">SqlConnect()</seealso>
FUNCTION SqlDisconnect( nStatementHandle AS LONG) AS LONG
    IF nStatementHandle > 0
        VAR oStmt := GetStatement(nStatementHandle)    
        IF oStmt != NULL
            SQLSupport.RemoveStatement(nStatementHandle)
            oStmt:DisConnect()
            RETURN 1
        ENDIF
    ELSEIF nStatementHandle == 0
        FOREACH oStmt AS SQLStatement IN SQLSupport.GetStatements()
            SQLSupport.RemoveStatement(oStmt:Handle)
            oStmt:DisConnect()
        NEXT
        RETURN 1
    ENDIF
    RETURN -1

/// <include file="VFPDocs.xml" path="Runtimefunctions/sqlexec/*" />
/// <seealso cref="M:XSharp.VFP.Functions.SqlPrepare(System.Int32,System.String,XSharp.__Usual)">SqlPrepare()</seealso>
/// <seealso cref="M:XSharp.VFP.Functions.SqlConnect(XSharp.__Usual,XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)">SqlConnect()</seealso>
FUNCTION SqlExec( nStatementHandle AS LONG, cSQLCommand := NIL AS USUAL, cCursorName := NIL AS USUAL, aCountInfo := NIL AS USUAL) AS LONG
    LOCAL aInfo AS ARRAY
    LOCAL prepared := FALSE AS LOGIC
    VAR oStmt := GetStatement(nStatementHandle)    
    IF oStmt != NULL
        IF ! IsString(cCursorName)
            cCursorName := "SQLRESULT"
        ENDIF
        IF !IsString(cSQLCommand)
            IF ! oStmt:Prepared .AND. ! oStmt:Asynchronous
                THROW Error{"SQL Statement parameter is required for non prepared SQL Statements"}
            ENDIF
            prepared := TRUE
        ENDIF
        LOCAL result AS LONG
        aInfo := {}
        IF prepared
            result := oStmt:Execute(aInfo)
        ELSE
            result := oStmt:Execute(cSQLCommand, cCursorName,aInfo)
        ENDIF
        CopyResults(aCountInfo, aInfo)
        RETURN result
    ENDIF
    RETURN 0
    

/// <summary>-- todo --</summary>
/// <include file="VFPDocs.xml" path="Runtimefunctions/sqlidledisconnect/*" />

FUNCTION SqlIdleDisconnect( nStatementHandle AS LONG) AS LONG
    GetStatement(nStatementHandle)    
    THROW NotImplementedException{}
    // RETURN 0

/// <include file="VFPDocs.xml" path="Runtimefunctions/sqlmoreresults/*" />
/// <seealso cref="M:XSharp.VFP.Functions.SqlConnect(XSharp.__Usual,XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)">SqlConnect()</seealso>
/// <seealso cref="M:XSharp.VFP.Functions.SqlExec(System.Int32,XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)">SqlExec()</seealso>
/// <seealso cref="M:XSharp.VFP.Functions.SqlGetProp(System.Int32,System.String)">SqlGetProp()</seealso>
/// <seealso cref="M:XSharp.VFP.Functions.SqlSetProp(System.Int32,System.String,XSharp.__Usual)">SqlSetProp()</seealso>
FUNCTION SqlMoreResults( nStatementHandle AS LONG, cCursorName := NIL AS USUAL , aCountInfo := NIL AS USUAL) AS LONG
    VAR oStmt := GetStatement(nStatementHandle)    
    IF oStmt != NULL
        IF ! IsString(cCursorName)
            cCursorName := ""       // Empty so the previous name is kept
        ENDIF
        LOCAL aInfo := {} AS ARRAY
        VAR result := oStmt:MoreResults(cCursorName, aInfo)
        CopyResults(aCountInfo, aInfo)
        RETURN result
    ENDIF
    RETURN 0
        

/// <include file="VFPDocs.xml" path="Runtimefunctions/sqlprepare/*" />
/// <seealso cref="M:XSharp.VFP.Functions.SqlExec(System.Int32,XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)">SqlExec()</seealso>
/// <seealso cref="M:XSharp.VFP.Functions.SqlConnect(XSharp.__Usual,XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)">SqlConnect()</seealso>
FUNCTION SqlPrepare( nStatementHandle AS LONG, cSQLCommand AS STRING, cCursorName := NIL AS USUAL) AS LONG
    VAR oStmt := GetStatement(nStatementHandle)    
    IF oStmt != NULL
        IF ! IsString(cSQLCommand)
            THROW Error{"Number or type of parameters is not correct"}
        ENDIF
        IF ! IsString(cCursorName)
            cCursorName := "SQLRESULT"
        ENDIF
        VAR result := oStmt:Prepare(cSQLCommand, cCursorName)
        RETURN result
    ENDIF
    RETURN 0


INTERNAL FUNCTION GetStatement(nStatementHandle AS LONG) AS XSharp.VFP.SQLStatement
    RETURN SQLSupport.FindStatement(nStatementHandle)



/// <include file="VFPDocs.xml" path="Runtimefunctions/sqlsetfactory/*" />

FUNCTION SqlSetFactory(uFactory := NIL AS USUAL) AS ISqlFactory
    LOCAL oResult := SQLSupport.Factory
    IF IsString(uFactory)
        LOCAL cProvider AS STRING
        LOCAL oFactory  := NULL_OBJECT AS ISqlFactory
        cProvider := (STRING) uFactory
        cProvider := cProvider:ToLower()
        IF cProvider:StartsWith("odbc")
            oFactory := XSharp.Data.OdbcFactory{}
        ELSEIF cProvider:StartsWith("oledb")
            oFactory := XSharp.Data.OleDbFactory{}
        ELSEIF cProvider:StartsWith("sql")
           oFactory := XSharp.Data.SqlServerFactory{}
        ENDIF
        IF oFactory != NULL_OBJECT
            SQLSupport.Factory := (ISqlFactory) oFactory
        ENDIF
    ELSEIF IsObject(uFactory)
        LOCAL oFactory AS OBJECT
        oFactory := uFactory
        IF oFactory IS ISqlFactory VAR oDataFactory
            SQLSupport.Factory := oDataFactory
        ENDIF
    ENDIF
    RETURN oResult


STATIC FUNCTION CopyResults(aCountInfo AS USUAL, aInfo AS ARRAY) AS VOID
    IF IsArray(aCountInfo)
        ASize(aCountInfo, ALen(aInfo))
        IF ALen(aInfo) > 0
            ACopy(aInfo, aCountInfo)
        ENDIF
    ENDIF


#region Transaction Support
/// <include file="VFPDocs.xml" path="Runtimefunctions/sqlcommit/*" />
/// <seealso cref="M:XSharp.VFP.Functions.SqlConnect(XSharp.__Usual,XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)">SqlConnect()</seealso>
/// <seealso cref="M:XSharp.VFP.Functions.SqlSetProp(System.Int32,System.String,XSharp.__Usual)">SqlSetProp()</seealso>
/// <seealso cref="M:XSharp.VFP.Functions.SqlRollBack(System.Int32)">SqlRollBack()</seealso>
FUNCTION SqlCommit( nStatementHandle AS LONG) AS LONG
    VAR oStmt := GetStatement(nStatementHandle)
    IF oStmt != NULL .AND. oStmt:Commit()
       RETURN 1
    ENDIF    
    RETURN 0

/// <include file="VFPDocs.xml" path="Runtimefunctions/sqlrollback/*" />
/// <seealso cref="M:XSharp.VFP.Functions.SqlConnect(XSharp.__Usual,XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)">SqlConnect()</seealso>
/// <seealso cref="M:XSharp.VFP.Functions.SqlSetProp(System.Int32,System.String,XSharp.__Usual)">SqlSetProp()</seealso>
/// <seealso cref="M:XSharp.VFP.Functions.SqlCommit(System.Int32)">SqlCommit()</seealso>
FUNCTION SqlRollBack( nStatementHandle AS LONG) AS LONG
    VAR oStmt := GetStatement(nStatementHandle)    
    IF oStmt != NULL .AND. oStmt:Rollback()
       RETURN 1
    ENDIF    
    RETURN 0

#endregion

#region Properties

/// <include file="VFPDocs.xml" path="Runtimefunctions/sqlgetprop/*" />
/// <seealso cref="M:XSharp.VFP.Functions.SqlConnect(XSharp.__Usual,XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)">SqlConnect()</seealso>
/// <seealso cref="M:XSharp.VFP.Functions.SqlSetProp(System.Int32,System.String,XSharp.__Usual)">SqlSetProp()</seealso>

FUNCTION SqlGetProp( nStatementHandle AS LONG, cSetting AS STRING ) AS USUAL
    GetStatement(nStatementHandle)    
    RETURN SQLSupport.GetSetProperty(nStatementHandle, cSetting,NULL)


/// <include file="VFPDocs.xml" path="Runtimefunctions/sqlsetprop/*" />
/// <seealso cref="M:XSharp.VFP.Functions.SqlConnect(XSharp.__Usual,XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)">SqlConnect()</seealso>
/// <seealso cref="M:XSharp.VFP.Functions.SqlGetProp(System.Int32,System.String)">SqlGetProp()</seealso>
FUNCTION SqlSetProp( nStatementHandle AS LONG, cSetting AS STRING, eExpression AS USUAL) AS LONG
    GetStatement(nStatementHandle)    
    RETURN (INT) SQLSupport.GetSetProperty(nStatementHandle, cSetting,eExpression)

#endregion



#region MetaData

/// <include file="VFPDocs.xml" path="Runtimefunctions/sqltables/*" />
/// <seealso cref="M:XSharp.VFP.Functions.SqlColumns(System.Int32,XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)">SqlColumns()</seealso>
/// <seealso cref="M:XSharp.VFP.Functions.SqlConnect(XSharp.__Usual,XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)">SqlConnect()</seealso>
/// <seealso cref="M:XSharp.VFP.Functions.SqlGetProp(System.Int32,System.String)">SqlGetProp()</seealso>
/// <seealso cref="M:XSharp.VFP.Functions.SqlSetProp(System.Int32,System.String,XSharp.__Usual)">SqlSetProp()</seealso>

FUNCTION SqlTables( nStatementHandle AS LONG , cTableTypes:= NIL AS USUAL , cCursorName := NIL AS USUAL) AS LONG
    VAR oStmt := GetStatement(nStatementHandle)    
    IF oStmt != NULL
       IF ! IsString(cTableTypes)
           cTableTypes := "" 
       ENDIF
       IF ! IsString(cCursorName)
            cCursorName := "SQLRESULT"
        ENDIF        
        oStmt:GetTables(cTableTypes, cCursorName)
        RETURN 1
    ENDIF
    RETURN -1

/// <include file="VFPDocs.xml" path="Runtimefunctions/sqlcolumns/*" />
/// <seealso cref="M:XSharp.VFP.Functions.SqlConnect(XSharp.__Usual,XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)">SqlConnect()</seealso>
/// <seealso cref="M:XSharp.VFP.Functions.SqlGetProp(System.Int32,System.String)">SqlGetProp()</seealso>
/// <seealso cref="M:XSharp.VFP.Functions.SqlSetProp(System.Int32,System.String,XSharp.__Usual)">SqlSetProp()</seealso>
/// <seealso cref="M:XSharp.VFP.Functions.SqlTables(System.Int32,XSharp.__Usual,XSharp.__Usual)">SqlTables()</seealso>
FUNCTION SqlColumns( nStatementHandle AS LONG, cTableName := NIL AS USUAL, cType:= NIL AS USUAL, cCursorName:= NIL AS USUAL) AS USUAL
    VAR oStmt := GetStatement(nStatementHandle)    
    IF oStmt != NULL
       IF ! IsString(cTableName)
           cTableName := ""
       ENDIF
       IF ! IsString(cType)
           cType := "FOXPRO"
       ENDIF
       IF Upper(cType) != "FOXPRO"  .AND. Upper(cType) != "NATIVE"
            THROW Error{"Incorrect value for cType. Expected 'FOXPRO' or 'NATIVE'. Default is 'FOXPRO'"}
       ENDIF
       IF ! IsString(cCursorName)
            cCursorName := "SQLRESULT"
       ENDIF        
       oStmt:GetColumns(cTableName, cType, cCursorName)
       IF Used(cCursorName) .AND. RecCount(cCursorName) > 0
           RETURN 1
       ELSE
            RETURN IIF(Upper(cType) != "FOXPRO" ,TRUE, FALSE)
       ENDIF
    ENDIF
    RETURN 0



/// <include file="VFPDocs.xml" path="Runtimefunctions/asqlhandles/*" />
/// <seealso cref="M:XSharp.VFP.Functions.SqlConnect(XSharp.__Usual,XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)">SqlConnect()</seealso>
/// <seealso cref="M:XSharp.VFP.Functions.SqlGetProp(System.Int32,System.String)">SqlGetProp()</seealso>
/// <seealso cref="M:XSharp.VFP.Functions.SqlSetProp(System.Int32,System.String,XSharp.__Usual)">SqlSetProp()</seealso>
FUNCTION ASqlHandles (ArrayName AS ARRAY, nStatementHandle := NIL AS USUAL) AS DWORD
    LOCAL aResult AS ARRAY
    IF IsNumeric(nStatementHandle)
        VAR oStmt := GetStatement(nStatementHandle)
        VAR oConn := oStmt:Connection
        aResult := {}
        FOREACH VAR oStmt2 IN oConn:Statements
            AAdd(aResult, oStmt2:Handle)
        NEXT
    ELSE
        aResult := {}
        FOREACH oStmt AS SQLStatement IN SQLSupport.GetStatements()
            AAdd(aResult, oStmt:Handle)
        NEXT
    ENDIF
    IF ALen(aResult) > 0
        ASize(ArrayName, ALen(aResult))
        ACopy(aResult, ArrayName)
    ENDIF
    RETURN ALen(aResult)
    
    


#endregion




