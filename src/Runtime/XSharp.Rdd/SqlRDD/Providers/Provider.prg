﻿//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


using System
using System.Reflection
using System.Collections.Generic
using System.Text
using System.Data.Common
begin namespace XSharp.RDD.SqlRDD

/// <summary>
/// The Provider class.
/// </summary>
abstract class SqlDbProvider inherit SqlDbObject

    internal const DefaultQuotePrefix       := """" as string
    internal const DefaultQuoteSuffix       := """" as string

#region Static fields and methods
    static _ProviderClasses as IDictionary<string, System.Type>
    static _ProviderObjects as IDictionary<string, SqlDbProvider>
    static property Current as SqlDbProvider auto

    static constructor()
        Current := null
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

    static method RegisterProvider(Name as string, ClassName as System.Type) as logic
        _ProviderClasses[Name] := ClassName
        return true

    static method UnRegisterProvider(Name as string) as logic
        if (_ProviderClasses:ContainsKey(Name))
            _ProviderClasses:Remove(Name)
            return true
        endif
        return false

    static method SetDefaultProvider(provider as SqlDbProvider) as SqlDbProvider
        var old := Current
        Current := provider
        return old

    static method SetDefaultProvider(name as string) as SqlDbProvider
        var old := Current
        Current := GetProvider(name)
        return old

    static protected method _FindType(cName as string) as System.Type
        var aAssemblies := AppDomain.CurrentDomain:GetAssemblies()
        foreach asm as Assembly in aAssemblies
            foreach t as System.Type in asm:DefinedTypes
                if String.Compare(t:FullName, cName ,true) == 0
                    return t
                endif
            next
        next
        return null
    static protected method _LoadFactoryFromDllAndType(DllName as string, TypeName as string) as DbProviderFactory
        local assembly := null as Assembly
        local type     := null as System.Type
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
        endif
        if type != null
            local oFld := type:GetField("Instance") as FieldInfo
            if oFld != null
                var oFact := (DbProviderFactory) oFld:GetValue(null)
                return oFact
            endif
        endif
        return null
    static method GetProvider(Name as string) as SqlDbProvider
        if _ProviderObjects:ContainsKey(Name)
            return _ProviderObjects[Name]
        endif
        if ! _ProviderClasses:ContainsKey(Name)
            return null
        endif
        var type := _ProviderClasses[Name]
        try
            if typeof(SqlDbProvider):IsAssignableFrom(type)
                var oProv := (SqlDbProvider) Activator.CreateInstance(type)
                if (oProv:Factory != null)
                    _ProviderObjects:Add(Name, oProv)
                    return oProv
                endif
            endif
        catch e as Exception
            nop
        end try
        return null

#endregion
    private _Factory as DbProviderFactory

    property Factory as DbProviderFactory
        get
            if _Factory == null
                _Factory := _LoadFactoryFromDllAndType(DllName, TypeName)
                if _Factory == null
                    throw Exception{i"Could not load DbProviderFactory {TypeName} from DLL {DllName}"}
                endif
            endif
            return _Factory
        end get
    end property
    abstract property DllName as string get
    abstract property TypeName as string get
    abstract method GetFunctions() as Dictionary<string, string>
    method GetFunction(cFunction as string) as string
        var funcs := GetFunctions()
        if funcs:ContainsKey(cFunction)
            return funcs[cFunction]
        endif
        return cFunction
    constructor(cName as string)
        super(cName)
        var cmdBuilder := self:CreateCommandBuilder()
        self:QuotePrefix := cmdBuilder:QuotePrefix
        self:QuoteSuffix := cmdBuilder:QuoteSuffix


#region Methods and Properties from the factory
    property CanCreateDataSourceEnumerator as logic get Factory:CanCreateDataSourceEnumerator

    method CreateCommand() as DbCommand
        return Factory:CreateCommand()

    method CreateCommandBuilder() as DbCommandBuilder
        return Factory:CreateCommandBuilder()

    method CreateConnection() as DbConnection
        return Factory:CreateConnection()

    method CreateConnectionStringBuilder() as DbConnectionStringBuilder
        return Factory:CreateConnectionStringBuilder()

    method CreateParameter() as DbParameter
        return Factory:CreateParameter()

    method CreateDataSourceEnumerator() as DbDataSourceEnumerator
        if Factory:CanCreateDataSourceEnumerator
            return Factory:CreateDataSourceEnumerator()
        endif
        return null

#endregion
#region Virtual Properties with statements etc
    virtual property CreateTableStatement   as string => "create table "+TableNameMacro+" ( "+FieldDefinitionListMacro+" )"
    virtual property CreateIndexStatement   as string => "create "+UniqueMacro+" index "+TableNameMacro+"_"+IndexNameMacro+" on "+TableNameMacro+"( "+FieldListMacro+" )"
    virtual property DropTableStatement     as string => "drop table "+TableNameMacro
    virtual property DropIndexStatement     as string => "drop index "+TableNameMacro+"_"+IndexNameMacro
    virtual property DeleteAllRowsStatement as string => "delete from "+TableNameMacro
    virtual property SelectTopStatement     as string => "select top "+TopCountMacro+" "+ColumnsMacro+" from "+FromClauseMacro
    virtual property InsertStatement        as string => "insert into "+TableNameMacro+" ( "+ColumnsMacro+") values ( "+ValuesMacro+" )"
    virtual property OrderByClause          as string => " order by "+ColumnsMacro+" "
    virtual property MaxRows                as int    => 1000
    virtual property QuotePrefix            as string auto := "["
    virtual property QuoteSuffix            as string auto := "]"

#endregion

#region Constants
    public const SelectClause    := "select " as string
    public const FromClause      := " from "  as string
    public const WhereClause     := " where " as string
    public const AndClause       := " and " as string
    public const OrClause        := " or "  as string

    public const FieldListMacro  := "%FL%" as string
    public const IndexNameMacro  := "%I%" as string
    public const TopCountMacro   := "%N%" as string
    public const TableNameMacro  := "%T%" as string
    public const UniqueMacro     := "%U%" as string
    public const FieldDefinitionListMacro := "%FDL%" as string
    public const FromClauseMacro := "%FROM%" as string
    public const ColumnsMacro    := "%C%" as string
    public const ValuesMacro     := "%V%" as string

    public const ConnectionDelimiter := "::" as string
    public const IndexExt            := "SQX" as string
    public const TableExt            := "SQF" as string
    public const ConfigFile          := "confix.sql" as string


#endregion

end class


end namespace // XSharp.RDD.SqlRDD
