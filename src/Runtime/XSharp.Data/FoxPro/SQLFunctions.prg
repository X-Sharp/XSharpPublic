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


INTERNAL STATIC CLASS SqlFunctions



STATIC METHOD SqlConnect(nStatementHandle AS LONG) AS LONG
    LOCAL oStmt AS XSharp.VFP.SQLStatement
    oStmt := SQLSupport.FindStatement(nStatementHandle)
    IF oStmt == NULL_OBJECT
        THROW Error{oVFPErrorMessage(VFPErrors.STATEMENT_HANDLE_INVALID, nStatementHandle)}
    ENDIF
    // This should open a connection automatically since the previous connection as already validated
    IF ! oStmt:Connection:Shared
        THROW Error{oVFPErrorMessage(VFPErrors.STATEMENT_HANDLE_NOTSHARED, nStatementHandle)}
    ENDIF
    oStmt := XSharp.VFP.SQLStatement{oStmt:Connection}
    nStatementHandle := SQLSupport.AddStatement(oStmt)

    RETURN nStatementHandle


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sqlconnect/*" />
STATIC METHOD SqlConnect(cDataSourceName AS STRING, cUserID := NIL AS USUAL, cPassword := NIL AS USUAL, lShared := NIL AS USUAL) AS LONG
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



STATIC METHOD SqlStringConnect( cConnectString AS STRING, lShared AS LOGIC) AS LONG
    LOCAL nHandle := -1 AS LONG
    LOCAL oStmt AS XSharp.VFP.SQLStatement
    oStmt := XSharp.VFP.SQLStatement{cConnectString,lShared}
    IF oStmt:Connected
        nHandle := SQLSupport.AddStatement(oStmt)
    ENDIF
    RETURN nHandle


STATIC METHOD SqlCancel( nStatementHandle AS LONG) AS LONG
    VAR oStmt := GetStatement(nStatementHandle)
    IF oStmt != NULL
        oStmt:Cancel()
        RETURN 1
    ENDIF
    RETURN -1


STATIC METHOD SqlDisconnect( nStatementHandle AS LONG) AS LONG
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
STATIC METHOD SqlExec( nStatementHandle AS LONG, cSQLCommand := "" AS STRING, cCursorName := "SQLRESULT" AS STRING, aCountInfo := NULL_ARRAY  AS ARRAY) AS LONG
    LOCAL aInfo AS ARRAY
    LOCAL prepared := FALSE AS LOGIC
    VAR oStmt := GetStatement(nStatementHandle)
    IF oStmt != NULL
        IF String.IsNullOrEmpty(cSQLCommand)
            IF ! oStmt:Prepared .AND. ! oStmt:Asynchronous
                VAR cMessage := oVFPErrorMessage(VFPErrors.COMMAND_PARAMETER_REQUIRED)
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
        if result >0 .and. Used(cCursorName)
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



STATIC METHOD SqlIdleDisconnect( nStatementHandle AS LONG) AS LONG
    GetStatement(nStatementHandle)
    THROW NotImplementedException{}

STATIC METHOD SqlMoreResults( nStatementHandle AS LONG, cCursorName := NIL AS USUAL , aCountInfo := NIL AS USUAL) AS LONG
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


STATIC METHOD SqlPrepare( nStatementHandle AS LONG, cSQLCommand AS STRING, cCursorName := "SQLRESULT" AS STRING) AS LONG
    VAR oStmt := GetStatement(nStatementHandle)
    IF oStmt != NULL
        IF String.IsNullOrEmpty(cSQLCommand)
            VAR cMessage := oVFPErrorMessage(VFPErrors.COMMAND_PARAMETER_REQUIRED)
            THROW Error{cMessage}
        ENDIF
        VAR result := oStmt:Prepare(cSQLCommand, cCursorName)
        RETURN result
    ENDIF
    RETURN 0


STATIC METHOD GetStatement(nStatementHandle AS LONG) AS XSharp.VFP.SQLStatement
    RETURN SQLSupport.FindStatement(nStatementHandle)


STATIC METHOD CopyResults(aCountInfo AS ARRAY, aInfo AS ARRAY) AS VOID
    ASize(aCountInfo, ALen(aInfo))
    IF ALen(aInfo) > 0
        ACopy(aInfo, aCountInfo)
    ENDIF


#region Transaction Support
/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sqlcommit/*" />
STATIC METHOD SqlCommit( nStatementHandle AS LONG) AS LONG
    VAR oStmt := GetStatement(nStatementHandle)
    IF oStmt != NULL .AND. oStmt:Commit()
       RETURN 1
    ENDIF
    RETURN 0

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sqlrollback/*" />
STATIC METHOD SqlRollBack( nStatementHandle AS LONG) AS LONG
    VAR oStmt := GetStatement(nStatementHandle)
    IF oStmt != NULL .AND. oStmt:Rollback()
       RETURN 1
    ENDIF
    RETURN 0

#endregion

#region Properties

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sqlgetprop/*" />
/// <seealso cref="SQLProperty" />
STATIC METHOD  SqlGetProp( nStatementHandle AS LONG, cSetting AS STRING ) AS USUAL
    RETURN SQLSupport.GetSetProperty(nStatementHandle, cSetting,NULL)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sqlgetprop/*" />
/// <param name="nSetting">Specifies the setting. For a list of the settings you can specify see the <see cref='SQLProperty' >SQLProperty Enum</see>.</param>
/// <seealso cref="SQLProperty" />
STATIC METHOD  SqlGetProp( nStatementHandle AS LONG, nSetting AS LONG ) AS USUAL
    var cSetting := System.Enum.GetName(typeof(SQLProperty), nSetting)
    RETURN SQLSupport.GetSetProperty(nStatementHandle, cSetting,NULL)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sqlsetprop/*" />
/// <seealso cref="SQLProperty" />
STATIC METHOD  SqlSetProp( nStatementHandle AS LONG, cSetting AS STRING, eExpression AS USUAL) AS LONG
    RETURN (INT) SQLSupport.GetSetProperty(nStatementHandle, cSetting,eExpression)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sqlsetprop/*" />
/// <param name="nSetting">Specifies the setting. For a list of the settings you can specify see the <see cref='SQLProperty'>SQLProperty Enum</see>.</param>
/// <seealso cref="SQLProperty" />
STATIC METHOD  SqlSetProp( nStatementHandle AS LONG, nSetting AS LONG, eExpression AS USUAL) AS LONG
    var cSetting := System.Enum.GetName(typeof(SQLProperty), nSetting)
    RETURN (INT) SQLSupport.GetSetProperty(nStatementHandle, cSetting,eExpression)

#endregion



#region MetaData


STATIC METHOD SqlTables( nStatementHandle AS LONG , cTableTypes AS STRING, cCursorName AS STRING) AS LONG
    VAR oStmt := GetStatement(nStatementHandle)
    IF oStmt != NULL
        oStmt:GetTables(cTableTypes, cCursorName)
        RETURN 1
    ENDIF
    RETURN -1

STATIC METHOD SqlColumns( nStatementHandle AS LONG, cTableName AS STRING, cType AS STRING, cCursorName AS STRING) AS USUAL
    VAR oStmt := GetStatement(nStatementHandle)
    IF oStmt != NULL
       cType := cType:Trim():ToUpper()
       IF cType != "FOXPRO"  .AND. cType != "NATIVE"
            VAR cMessage := oVFPErrorMessage(VFPErrors.SQLCOLUMNS_CTYPE)
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

STATIC METHOD SqlParameters( nStatementHandle AS LONG, oParams AS OBJECT) AS LONG
    VAR oStmt := GetStatement(nStatementHandle)
    IF oStmt != NULL
        oStmt:ParamsObject := oParams
    ENDIF
    RETURN 0

END CLASS

#endregion






