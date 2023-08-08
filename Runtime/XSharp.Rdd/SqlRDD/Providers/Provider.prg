//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


USING System
USING System.Reflection
USING System.Collections.Generic
USING System.Text
USING System.Data.Common
BEGIN NAMESPACE XSharp.RDD.SqlRDD

/// <summary>
/// The Provider class.
/// </summary>
ABSTRACT CLASS SqlDbProvider INHERIT SqlDbObject

#region Static fields and methods
    STATIC _ProviderClasses as IDictionary<String, System.Type>
    STATIC _ProviderObjects as IDictionary<String, SqlDbProvider>
    STATIC PROPERTY Current AS SqlDbProvider AUTO

    STATIC CONSTRUCTOR()
        Current := NULL
        _ProviderClasses := Dictionary<string,  System.Type>{StringComparer.OrdinalIgnoreCase}
        _ProviderObjects := Dictionary<string, SqlDbProvider>{StringComparer.OrdinalIgnoreCase}
        RegisterProvider("SQLSERVER",typeof(XSharp.RDD.SqlRDD.Providers.SqlServer))
        RegisterProvider("ODBC", typeof(XSharp.RDD.SqlRDD.Providers.ODBC))
        RegisterProvider("OLEDB",typeof(XSharp.RDD.SqlRDD.Providers.OleDb ))
        RegisterProvider("MYSQL",typeof(XSharp.RDD.SqlRDD.Providers.MySql ))
        RegisterProvider("ADS",typeof(XSharp.RDD.SqlRDD.Providers.Advantage ))
        RegisterProvider("Advantage",typeof(XSharp.RDD.SqlRDD.Providers.Advantage ))
        RegisterProvider("Oracle",typeof(XSharp.RDD.SqlRDD.Providers.Oracle ))
        SetDefaultProvider("ODBC")

    STATIC METHOD RegisterProvider(Name as STRING, ClassName as System.Type) AS LOGIC
        _ProviderClasses[Name] := ClassName
        RETURN TRUE

    STATIC METHOD UnRegisterProvider(Name as STRING) AS LOGIC
        if (_ProviderClasses:ContainsKey(Name))
            _ProviderClasses:Remove(Name)
            RETURN TRUE
        ENDIF
        RETURN FALSE

    STATIC METHOD SetDefaultProvider(provider as SqlDbProvider) AS SqlDbProvider
        var old := Current
        Current := provider
        return old

    STATIC METHOD SetDefaultProvider(name as String) AS SqlDbProvider
        var old := Current
        Current := GetProvider(name)
        return old

    STATIC PROTECTED METHOD _FindType(cName as STRING) AS System.Type
        var aAssemblies := AppDomain.CurrentDomain:GetAssemblies()
        foreach asm as Assembly in aAssemblies
            FOREACH t as System.Type in asm:DefinedTypes
                if String.Compare(t:FullName, cName ,true) == 0
                    RETURN t
                endif
            next
        next
        RETURN NULL
    STATIC PROTECTED METHOD _LoadFactoryFromDllAndType(DllName as STRING, TypeName as STRING) AS DbProviderFactory
        local assembly := NULL as Assembly
        local type     := NULL as System.Type
        foreach var asm in AppDomain.CurrentDomain.GetAssemblies()
            type := asm:GetType(TypeName, false)
            if type != null
                exit
            endif
        next
        if type == null .and. System.IO.File.Exists(DllName)
            var fileInfo := System.IO.FileInfo{DllName}
                var asm := System.Reflection.Assembly.LoadFile(fileInfo:FullName)
            type := asm:GetType(TypeName)
        ENDIF
        if type != null
            local oFld := type:GetField("Instance") as FieldInfo
            if oFld != NULL
                var oFact := (DbProviderFactory) oFld:GetValue(null)
                RETURN oFact
            ENDIF
        endif
        RETURN NULL
    STATIC METHOD GetProvider(Name as STRING) AS SqlDbProvider
        if _ProviderObjects:ContainsKey(Name)
            RETURN _ProviderObjects[Name]
        endif
        if ! _ProviderClasses:ContainsKey(Name)
            RETURN NULL
        ENDIF
        var type := _ProviderClasses[Name]
        TRY
            if typeof(SqlDbProvider):IsAssignableFrom(type)
                var oProv := (SqlDbProvider) Activator.CreateInstance(type)
                if (oProv:Factory != null)
                    _ProviderObjects:Add(Name, oProv)
                    return oProv
                endif
            endif
        CATCH e as Exception
        END TRY
        RETURN NULL

    #endregion
    PRIVATE _Factory as DbProviderFactory

    PROPERTY Factory AS DbProviderFactory
        GET
            IF _Factory == NULL
                _Factory := _LoadFactoryFromDllAndType(DllName, TypeName)
                IF _Factory == NULL
                    Throw Exception{i"Could not load DbProviderFactory {TypeName} from DLL {DllName}"}
                ENDIF
            ENDIF
            RETURN _Factory
        END GET
    END PROPERTY
    ABSTRACT PROPERTY DllName AS STRING GET
    ABSTRACT PROPERTY TypeName AS STRING GET
    ABSTRACT METHOD GetFunctions() AS Dictionary<String, String>
    METHOD GetFunction(cFunction as STRING) AS STRING
        var funcs := GetFunctions()
        if funcs:ContainsKey(cFunction)
            return funcs[cFunction]
        endif
        return cFunction
    CONSTRUCTOR(cName as STRING)
        SUPER(cName)
        var cmdBuilder := SELF:CreateCommandBuilder()
        SELF:QuotePrefix := cmdBuilder:QuotePrefix
        SELF:QuoteSuffix := cmdBuilder:QuoteSuffix


#region Methods and Properties from the factory
    PROPERTY CanCreateDataSourceEnumerator as LOGIC GET Factory:CanCreateDataSourceEnumerator

    METHOD CreateCommand() as DbCommand
        RETURN Factory:CreateCommand()

    METHOD CreateCommandBuilder() as DbCommandBuilder
        RETURN Factory:CreateCommandBuilder()

    METHOD CreateConnection() as DbConnection
        RETURN Factory:CreateConnection()

    METHOD CreateConnectionStringBuilder() as DbConnectionStringBuilder
        RETURN Factory:CreateConnectionStringBuilder()

    METHOD CreateParameter() as DbParameter
        RETURN Factory:CreateParameter()

    METHOD CreateDataSourceEnumerator() as DbDataSourceEnumerator
        IF Factory:CanCreateDataSourceEnumerator
            RETURN Factory:CreateDataSourceEnumerator()
        ENDIF
        RETURN NULL

#endregion
#region Virtual Properties with statements etc
    VIRTUAL PROPERTY CreateTableStatement   AS STRING => "create table "+TableNameMacro+" ( "+FieldDefinitionListMacro+" )"
    VIRTUAL PROPERTY CreateIndexStatement   AS STRING => "create "+UniqueMacro+" index "+TableNameMacro+"_"+IndexNameMacro+" on "+TableNameMacro+"( "+FieldListMacro+" )"
    VIRTUAL PROPERTY DropTableStatement     AS STRING => "drop table "+TableNameMacro
    VIRTUAL PROPERTY DropIndexStatement     AS STRING => "drop index "+TableNameMacro+"_"+IndexNameMacro
    VIRTUAL PROPERTY DeleteAllRowsStatement AS STRING => "delete from "+TableNameMacro
    VIRTUAL PROPERTY SelectTopStatement     AS STRING => "select top "+TopCountMacro+" "+ColumnsMacro+" from "+FromClauseMacro
    VIRTUAL PROPERTY InsertStatement        AS STRING => "insert into "+TableNameMacro+" ( "+ColumnsMacro+") values ( "+ValuesMacro+" )"
    VIRTUAL PROPERTY OrderByClause          AS STRING => " order by "+ColumnsMacro+" "
    VIRTUAL PROPERTY WhereClause            AS STRING => " where "
    VIRTUAL PROPERTY AndClause              AS STRING => " and "
    VIRTUAL PROPERTY OrClause               AS STRING => " or "
    VIRTUAL PROPERTY MaxRows                AS INT    => 1000
    VIRTUAL PROPERTY QuotePrefix            AS STRING AUTO := "["
    VIRTUAL PROPERTY QuoteSuffix            AS STRING AUTO := "]"
#endregion

#region Constants
    PUBLIC CONST FieldListMacro  := "%FL%" AS STRING
    PUBLIC CONST IndexNameMacro  := "%I%" AS STRING
    PUBLIC CONST TopCountMacro   := "%N%" AS STRING
    PUBLIC CONST TableNameMacro  := "%T%" AS STRING
    PUBLIC CONST UniqueMacro     := "%U%" AS STRING
    PUBLIC CONST FieldDefinitionListMacro := "%FDL%" AS STRING
    PUBLIC CONST FromClauseMacro := "%FROM%" AS STRING
    PUBLIC CONST ColumnsMacro    := "%C%" AS STRING
    PUBLIC CONST ValuesMacro     := "%V%" AS STRING

    PUBLIC CONST ConnectionDelimiter := "::" AS STRING
    PUBLIC CONST IndexExt            := "SQX" AS STRING
    PUBLIC CONST TableExt            := "SQF" AS STRING
    PUBLIC CONST ConfigFile          := "confix.sql" AS STRING


#endregion

END CLASS


END NAMESPACE // XSharp.RDD.SqlRDD
