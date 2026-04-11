//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Collections.Generic
USING System
USING XSharp.VFP
USING XSharp.Data
USING XSharp.RDD
USING XSharp.Internal

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sqlconnect/*" />
/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sqlconnectoverload/*" />
[FoxProFunction("SQLCONNECT", FoxFunctionCategory.SQL, FoxEngine.SQL, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION SqlConnect(nStatementHandle AS LONG) AS LONG
   RETURN SqlFunctions.SqlConnect(nStatementHandle)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sqlconnect/*" />
FUNCTION SqlConnect(cDataSourceName AS STRING, cUserID := NIL AS USUAL, cPassword := NIL AS USUAL, lShared := NIL AS USUAL) AS LONG
    RETURN SqlFunctions.SqlConnect(cDataSourceName, cUserID, cPassword, lShared)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sqlstringconnect/*" />
/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sqlstringconnectoverload/*" />
[FoxProFunction("SQLSTRINGCONNECT", FoxFunctionCategory.SQL, FoxEngine.SQL, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION SqlStringConnect( lShared AS LOGIC) AS LONG
    RETURN SqlFunctions.SqlStringConnect("", lShared)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sqlstringconnect/*" />
FUNCTION SqlStringConnect( cConnectString AS STRING, lShared AS LOGIC) AS LONG
    RETURN SqlFunctions.SqlStringConnect(cConnectString, lShared)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sqlstringconnect/*" />
FUNCTION SqlStringConnect( cConnectString AS STRING) AS LONG
    RETURN SqlFunctions.SqlStringConnect(cConnectString, FALSE)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sqlcancel/*" />
[FoxProFunction("SQLCANCEL", FoxFunctionCategory.SQL, FoxEngine.SQL, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION SqlCancel( nStatementHandle AS LONG) AS LONG
    RETURN SqlFunctions.SqlCancel(nStatementHandle)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sqldisconnect/*" />
[FoxProFunction("SQLDISCONNECT", FoxFunctionCategory.SQL, FoxEngine.SQL, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION SqlDisconnect( nStatementHandle AS LONG) AS LONG
    RETURN SqlFunctions.SqlDisconnect( nStatementHandle)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sqlexec/*" />
/// <seealso cref="NeedsAccessToLocalsAttribute" />
[NeedsAccessToLocals(TRUE)];
[FoxProFunction("SQLEXEC", FoxFunctionCategory.SQL, FoxEngine.SQL, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION SqlExec( nStatementHandle AS LONG, cSQLCommand := "" AS STRING, cCursorName := "SQLRESULT" AS STRING, aCountInfo := NULL_ARRAY  AS ARRAY) AS LONG
    RETURN SqlFunctions.SqlExec(nStatementHandle, cSQLCommand, cCursorName, aCountInfo)

/// <summary>-- todo --</summary>
/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sqlidledisconnect/*" />
[FoxProFunction("SQLIDLEDISCONNECT", FoxFunctionCategory.SQL, FoxEngine.SQL, FoxFunctionStatus.Full, FoxCriticality.Low)];
FUNCTION SqlIdleDisconnect( nStatementHandle AS LONG) AS LONG
    return SqlFunctions.SqlIdleDisconnect ( nStatementHandle)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sqlmoreresults/*" />
[FoxProFunction("SQLMORERESULTS", FoxFunctionCategory.SQL, FoxEngine.SQL, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION SqlMoreResults( nStatementHandle AS LONG, cCursorName := NIL AS USUAL , aCountInfo := NIL AS USUAL) AS LONG
    return SqlFunctions.SqlMoreResults(nStatementHandle, cCursorName, aCountInfo)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sqlprepare/*" />
/// <seealso cref="NeedsAccessToLocalsAttribute" />
[NeedsAccessToLocals(TRUE)];
[FoxProFunction("SQLPREPARE", FoxFunctionCategory.SQL, FoxEngine.SQL, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION SqlPrepare( nStatementHandle AS LONG, cSQLCommand AS STRING, cCursorName := "SQLRESULT" AS STRING) AS LONG
   return SqlFunctions.SqlPrepare ( nStatementHandle, cSQLCommand, cCursorName)

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

#region Transaction Support
/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sqlcommit/*" />
[FoxProFunction("SQLCOMMIT", FoxFunctionCategory.SQL, FoxEngine.SQL, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION SqlCommit( nStatementHandle AS LONG) AS LONG
    RETURN SqlFunctions.SqlCommit(nStatementHandle)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sqlrollback/*" />
[FoxProFunction("SQLROLLBACK", FoxFunctionCategory.SQL, FoxEngine.SQL, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION SqlRollBack( nStatementHandle AS LONG) AS LONG
    RETURN SqlFunctions.SqlRollBack(nStatementHandle )

#endregion

#region Properties

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sqlgetprop/*" />
/// <seealso cref="SQLProperty" />
[FoxProFunction("SQLGETPROP", FoxFunctionCategory.SQL, FoxEngine.SQL, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION SqlGetProp( nStatementHandle AS LONG, cSetting AS STRING ) AS USUAL
    RETURN SQLSupport.GetSetProperty(nStatementHandle, cSetting,NULL)

/// <include file="XSharp.VFP.Docs.xml" path="doc/SqlGetProp/*" />
FUNCTION SqlGetProp( nStatementHandle AS LONG, nSetting AS LONG ) AS USUAL
    var cSetting := System.Enum.GetName(typeof(SQLProperty), nSetting)
    RETURN SQLSupport.GetSetProperty(nStatementHandle, cSetting,NULL)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sqlsetprop/*" />
/// <seealso cref="SQLProperty" />
[FoxProFunction("SQLSETPROP", FoxFunctionCategory.SQL, FoxEngine.SQL, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION SqlSetProp( nStatementHandle AS LONG, cSetting AS STRING, eExpression AS USUAL) AS LONG
    RETURN (INT) SQLSupport.GetSetProperty(nStatementHandle, cSetting,eExpression)

/// <include file="XSharp.VFP.Docs.xml" path="doc/SqlSetProp/*" />
FUNCTION SqlSetProp( nStatementHandle AS LONG, nSetting AS LONG, eExpression AS USUAL) AS LONG
    var cSetting := System.Enum.GetName(typeof(SQLProperty), nSetting)
    RETURN (INT) SQLSupport.GetSetProperty(nStatementHandle, cSetting,eExpression)

#endregion

#region MetaData

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sqltables/*" />
[FoxProFunction("SQLTABLES", FoxFunctionCategory.SQL, FoxEngine.SQL, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION SqlTables( nStatementHandle AS LONG , cTableTypes:= "" AS STRING, cCursorName := "SQLRESULT" AS STRING) AS LONG
    RETURN SqlFunctions.SqlTables(nStatementHandle , cTableTypes, cCursorName)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/sqlcolumns/*" />
[FoxProFunction("SQLCOLUMNS", FoxFunctionCategory.SQL, FoxEngine.SQL, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION SqlColumns( nStatementHandle AS LONG, cTableName := "" AS STRING, cType:= "FOXPRO" AS STRING, cCursorName:= "SQLRESULT" AS STRING) AS USUAL
    RETURN SqlFunctions.SqlColumns( nStatementHandle, cTableName, cType, cCursorName)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/asqlhandles/*" />
[FoxArrayInputParameter(1)];
[FoxProFunction("ASQLHANDLES", FoxFunctionCategory.Array, FoxEngine.SQL, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION ASqlHandles ( ArrayName AS USUAL, nStatementHandle := NIL AS USUAL) AS DWORD
    LOCAL aResult AS ARRAY
    LOCAL aFoxArray AS __FoxArray
    IF ArrayName IS __FoxArray var aFox
        aFoxArray := aFox
    ELSEIF IsNil(ArrayName)
        aFoxArray := __FoxArray{}
    ELSE
        var cMessage := __VfpStr(VFPErrors.VFP_VARIABLE_NOT_ARRAY, nameof(ArrayName))
        THROW ArgumentException{cMessage}
    ENDIF
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
        ASize(aFoxArray, ALen(aResult))
        ACopy(aResult, aFoxArray)
    ENDIF
    RETURN ALen(aFoxArray)
/// <exclude />
FUNCTION SqlParameters( nStatementHandle AS LONG, oParams AS OBJECT) AS LONG
    RETURN SqlFunctions.SqlParameters(nStatementHandle, oParams)

#endregion

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/clearresultset/*" />
[FoxProFunction("CLEARRESULTSET", FoxFunctionCategory.SQL, FoxEngine.SQL, FoxFunctionStatus.Stub, FoxCriticality.Medium)];
FUNCTION ClearResultSet() AS LOGIC
    THROW NotImplementedException{}
