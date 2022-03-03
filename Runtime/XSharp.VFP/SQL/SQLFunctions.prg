//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Collections.Generic
USING System
USING System.Text
USING XSharp.VFP
USING XSharp.Data
USING XSharp.RDD
USING XSharp.Internal

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sqlconnect/*" />
/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sqlconnectoverload/*" />
FUNCTION SqlConnect(nStatementHandle AS LONG) AS LONG
    LOCAL oStmt AS XSharp.VFP.SQLStatement
    oStmt := SQLSupport.FindStatement(nStatementHandle)
    IF oStmt == NULL_OBJECT
        THROW Error{__VfpStr(VFPErrors.STATEMENT_HANDLE_INVALID, nStatementHandle)}
    ENDIF
    // This should open a connection automatically since the previous connection as already validated
    IF ! oStmt:Connection:Shared
        THROW Error{__VfpStr(VFPErrors.STATEMENT_HANDLE_NOTSHARED, nStatementHandle)}
    ENDIF
    oStmt := XSharp.VFP.SQLStatement{oStmt:Connection}
    nStatementHandle := SQLSupport.AddStatement(oStmt)

    RETURN nStatementHandle


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sqlconnect/*" />
FUNCTION SqlConnect(cDataSourceName AS STRING, cUserID := NIL AS USUAL, cPassword := NIL AS USUAL, lShared := NIL AS USUAL) AS LONG
    LOCAL nHandle := -1 AS LONG
    LOCAL cSource AS STRING
    LOCAL oStmt AS XSharp.VFP.SQLStatement
    // if one or more parameters are missing then the connect dialog is shown
    cSource := cDataSourceName
    IF ! IsString(cUserID) ; cUserID := "" ; ENDIF
    IF ! IsString(cPassword) ; cPassword := "" ; ENDIF
    IF ! IsLogic(lShared) ; lShared := FALSE ; ENDIF
    oStmt := XSharp.VFP.SQLStatement{cSource, cUserID, cPassword, lShared}
    nHandle := SQLSupport.AddStatement(oStmt)
    RETURN nHandle



/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sqlstringconnect/*" />
/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sqlstringconnectoverload/*" />
FUNCTION SqlStringConnect( lShared AS LOGIC) AS LONG
    RETURN SqlStringConnect("", lShared)


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sqlstringconnect/*" />
FUNCTION SqlStringConnect( cConnectString AS STRING, lShared AS LOGIC) AS LONG
    LOCAL nHandle := -1 AS LONG
    LOCAL oStmt AS XSharp.VFP.SQLStatement
    oStmt := XSharp.VFP.SQLStatement{cConnectString,lShared}
    IF oStmt:Connected
        nHandle := SQLSupport.AddStatement(oStmt)
    ENDIF
    RETURN nHandle

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sqlstringconnect/*" />
FUNCTION SqlStringConnect( cConnectString AS STRING) AS LONG
    RETURN SqlStringConnect(cConnectString, FALSE)



/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sqlcancel/*" />
FUNCTION SqlCancel( nStatementHandle AS LONG) AS LONG
    VAR oStmt := GetStatement(nStatementHandle)
    IF oStmt != NULL
        oStmt:Cancel()
        RETURN 1
    ENDIF
    RETURN -1


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sqldisconnect/*" />
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

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sqlexec/*" />
/// <seealso cref="NeedsAccessToLocalsAttribute" />
[NeedsAccessToLocals(TRUE)]; 
FUNCTION SqlExec( nStatementHandle AS LONG, cSQLCommand := "" AS STRING, cCursorName := "SQLRESULT" AS STRING, aCountInfo := NULL_ARRAY  AS ARRAY) AS LONG
    LOCAL aInfo AS ARRAY
    LOCAL prepared := FALSE AS LOGIC
    VAR oStmt := GetStatement(nStatementHandle)
    IF oStmt != NULL
        IF String.IsNullOrEmpty(cSQLCommand)
            IF ! oStmt:Prepared .AND. ! oStmt:Asynchronous
                VAR cMessage := __VfpStr(VFPErrors.COMMAND_PARAMETER_REQUIRED)
                THROW Error{cMessage}
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
        IF aCountInfo != NULL_ARRAY
            CopyResults(aCountInfo, aInfo)
        ENDIF
        // FoxPro returns current area is the first result cursor area and current Recno is top
        // take care when there isn´t query result
        if result >0 and Used(cCursorName)
           if !DbSelectArea(cCursorName)
              result:= -1
           endif
           if !DbGoTop()
              result:= -1
           endif
        endif
        RETURN result
    ENDIF
    RETURN 0



/// <summary>-- todo --</summary>
/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sqlidledisconnect/*" />
FUNCTION SqlIdleDisconnect( nStatementHandle AS LONG) AS LONG
    GetStatement(nStatementHandle)
    THROW NotImplementedException{}
    // RETURN 0

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sqlmoreresults/*" />
FUNCTION SqlMoreResults( nStatementHandle AS LONG, cCursorName := NIL AS USUAL , aCountInfo := NIL AS USUAL) AS LONG
    VAR oStmt := GetStatement(nStatementHandle)
    IF oStmt != NULL
        IF ! IsString(cCursorName)
            cCursorName := ""       // Empty so the previous name is kept
        ENDIF
        LOCAL aInfo := {} AS ARRAY
        VAR result := oStmt:MoreResults(cCursorName, aInfo)
        IF IsArray(aCountInfo)
            CopyResults(aCountInfo, aInfo)
        ENDIF
        RETURN result
    ENDIF
    RETURN 0


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sqlprepare/*" />
/// <seealso cref="NeedsAccessToLocalsAttribute" />
[NeedsAccessToLocals(TRUE)];
FUNCTION SqlPrepare( nStatementHandle AS LONG, cSQLCommand AS STRING, cCursorName := "SQLRESULT" AS STRING) AS LONG
    VAR oStmt := GetStatement(nStatementHandle)
    IF oStmt != NULL
        IF String.IsNullOrEmpty(cSQLCommand)
            VAR cMessage := __VfpStr(VFPErrors.COMMAND_PARAMETER_REQUIRED)
            THROW Error{cMessage}
        ENDIF
        VAR result := oStmt:Prepare(cSQLCommand, cCursorName)
        RETURN result
    ENDIF
    RETURN 0


INTERNAL FUNCTION GetStatement(nStatementHandle AS LONG) AS XSharp.VFP.SQLStatement
    RETURN SQLSupport.FindStatement(nStatementHandle)



/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sqlsetfactory/*" />

FUNCTION SqlSetFactory() AS ISqlFactory
    RETURN SQLSupport.Factory

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sqlsetfactory/*" />
FUNCTION SqlSetFactory(cFactory AS STRING ) AS ISqlFactory
    LOCAL oResult := SQLSupport.Factory AS ISqlFactory
    LOCAL oFactory  := NULL_OBJECT AS ISqlFactory
    cFactory := cFactory:ToLower()
    IF cFactory:StartsWith("odbc")
        oFactory := XSharp.Data.OdbcFactory{}
    ELSEIF cFactory:StartsWith("oledb")
        oFactory := XSharp.Data.OleDbFactory{}
    ELSEIF cFactory:StartsWith("sql")
        oFactory := XSharp.Data.SqlServerFactory{}
    ENDIF
    IF oFactory != NULL_OBJECT
        SQLSupport.Factory := (ISqlFactory) oFactory
    ENDIF
    RETURN oResult



/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sqlsetfactory/*" />
FUNCTION SqlSetFactory(oFactory AS ISqlFactory) AS ISqlFactory
    LOCAL oResult := SQLSupport.Factory AS ISqlFactory
    IF oFactory != NULL
        SQLSupport.Factory := oFactory
    ENDIF
    RETURN oResult

INTERNAL FUNCTION CopyResults(aCountInfo AS ARRAY, aInfo AS ARRAY) AS VOID
    ASize(aCountInfo, ALen(aInfo))
    IF ALen(aInfo) > 0
        ACopy(aInfo, aCountInfo)
    ENDIF


#region Transaction Support
/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sqlcommit/*" />
FUNCTION SqlCommit( nStatementHandle AS LONG) AS LONG
    VAR oStmt := GetStatement(nStatementHandle)
    IF oStmt != NULL .AND. oStmt:Commit()
       RETURN 1
    ENDIF
    RETURN 0

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sqlrollback/*" />
FUNCTION SqlRollBack( nStatementHandle AS LONG) AS LONG
    VAR oStmt := GetStatement(nStatementHandle)
    IF oStmt != NULL .AND. oStmt:Rollback()
       RETURN 1
    ENDIF
    RETURN 0

#endregion

#region Properties

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sqlgetprop/*" />
/// <seealso cref="SQLProperty" />
FUNCTION SqlGetProp( nStatementHandle AS LONG, cSetting AS STRING ) AS USUAL
    RETURN SQLSupport.GetSetProperty(nStatementHandle, cSetting,NULL)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sqlgetprop/*" />
/// <param name="nSetting">Specifies the setting. For a list of the settings you can specify see the <see cref='SQLProperty' >SQLProperty Enum</see>.</param>
/// <seealso cref="SQLProperty" />
FUNCTION SqlGetProp( nStatementHandle AS LONG, nSetting AS LONG ) AS USUAL
    var cSetting := System.Enum.GetName(typeof(SQLProperty), nSetting)
    RETURN SQLSupport.GetSetProperty(nStatementHandle, cSetting,NULL)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sqlsetprop/*" />
/// <seealso cref="SQLProperty" />
FUNCTION SqlSetProp( nStatementHandle AS LONG, cSetting AS STRING, eExpression AS USUAL) AS LONG
    RETURN (INT) SQLSupport.GetSetProperty(nStatementHandle, cSetting,eExpression)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sqlsetprop/*" />
/// <param name="nSetting">Specifies the setting. For a list of the settings you can specify see the <see cref='SQLProperty'>SQLProperty Enum</see>.</param>
/// <seealso cref="SQLProperty" />
FUNCTION SqlSetProp( nStatementHandle AS LONG, nSetting AS LONG, eExpression AS USUAL) AS LONG
    var cSetting := System.Enum.GetName(typeof(SQLProperty), nSetting)
    RETURN (INT) SQLSupport.GetSetProperty(nStatementHandle, cSetting,eExpression)

#endregion



#region MetaData

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sqltables/*" />

FUNCTION SqlTables( nStatementHandle AS LONG , cTableTypes:= "" AS STRING, cCursorName := "SQLRESULT" AS STRING) AS LONG
    VAR oStmt := GetStatement(nStatementHandle)
    IF oStmt != NULL
        oStmt:GetTables(cTableTypes, cCursorName)
        RETURN 1
    ENDIF
    RETURN -1

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sqlcolumns/*" />
FUNCTION SqlColumns( nStatementHandle AS LONG, cTableName := "" AS STRING, cType:= "FOXPRO" AS STRING, cCursorName:= "SQLRESULT" AS STRING) AS USUAL
    VAR oStmt := GetStatement(nStatementHandle)
    IF oStmt != NULL
       cType := cType:Trim():ToUpper()
       IF cType != "FOXPRO"  .AND. cType != "NATIVE"
            VAR cMessage := __VfpStr(VFPErrors.SQLCOLUMNS_CTYPE)
            THROW Error{cMessage}
       ENDIF
       oStmt:GetColumns(cTableName, cType, cCursorName)
       IF Used(cCursorName) .AND. RecCount(cCursorName) > 0
           RETURN 1
       ELSE
            RETURN IIF(Upper(cType) != "FOXPRO" ,TRUE, FALSE)
       ENDIF
    ENDIF
    RETURN 0



/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/asqlhandles/*" />
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


FUNCTION SqlParameters( nStatementHandle AS LONG, oParams AS OBJECT) AS LONG
    VAR oStmt := GetStatement(nStatementHandle)
    IF oStmt != NULL
        oStmt:ParamsObject := oParams
    ENDIF
    RETURN 0



#endregion






