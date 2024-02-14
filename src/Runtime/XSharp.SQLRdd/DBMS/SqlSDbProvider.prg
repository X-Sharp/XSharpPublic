//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


using System
using System.Reflection
using System.Collections.Generic
using System.Text
using System.Data.Common
using XSharp.RDD.Enums
using XSharp.RDD.Support
begin namespace XSharp.RDD.SqlRDD.Providers

/// <summary>
/// The Provider class.
/// </summary>
abstract class SqlDbProvider inherit SqlDbObject

#region Static fields and methods
    static _ProviderClasses as IDictionary<string, System.Type>
    static _ProviderObjects as IDictionary<string, SqlDbProvider>
    static _lastException as Exception
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
    end constructor

    static method RegisterProvider(Name as string, ClassName as System.Type) as logic
        _ProviderClasses[Name] := ClassName
        return true
    end method

    static method UnRegisterProvider(Name as string) as logic
        if (_ProviderClasses:ContainsKey(Name))
            _ProviderClasses:Remove(Name)
            return true
        endif
        return false
    end method

    static method SetDefaultProvider(provider as SqlDbProvider) as SqlDbProvider
        var old := Current
        Current := provider
        return old
    end method

    static method SetDefaultProvider(name as string) as SqlDbProvider
        var old := Current
        Current := GetProvider(name)
        return old
    end method

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
    end method

    static protected method _LoadFactoryFromDllAndType(DllName as string, TypeName as string) as DbProviderFactory
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
    end method

        static method GetProvider(Name as string) as SqlDbProvider
        if _ProviderObjects:TryGetValue(Name, out var result)
            return result
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
            _lastException := e
            nop
        end try
        return null
    end method

#endregion
    protected _Factory as DbProviderFactory
    protected _cmdBuilder as DbCommandBuilder

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
    /// <summary>
    /// Name of the DLL that contains the DbProviderFactory
    /// </summary>
    abstract property DllName as string get
    /// <summary>
    /// Type name of the DbProviderFactory
    /// </summary>
    abstract property TypeName as string get
    /// <summary>
    /// Return a list of function translations for this provider
    /// </summary>
    /// <returns>Dictionary with XBase functions mapped to SQL functions</returns>
    abstract method GetFunctions() as Dictionary<string, string>
    method GetFunction(cFunction as string) as string
        var funcs := GetFunctions()
        if funcs:TryGetValue(cFunction, out var oFunc)
            return oFunc
        endif
        return cFunction
    end method

    /// <summary>
    /// Create a new SqlDbProvider object
    /// </summary>
    /// <param name="cName">Name of the object</param>
    constructor(cName as string)
        super(cName)
        _cmdBuilder := self:CreateCommandBuilder()
    end constructor


#region Methods and Properties from the factory
    property CanCreateDataSourceEnumerator as logic get Factory:CanCreateDataSourceEnumerator

    /// <summary>
    /// Surround an identifier with the correct quotes for the provider
    /// </summary>
    /// <param name="cId">Identifier</param>
    /// <returns>Quoted Identifier</returns>
    /// <example>
    /// <code>
    /// var cId := "MyTable"
    /// var cQuoted := provider:QuoteIdentifier(cId)
    /// // cQuoted will be "[MyTable]" for SQLServer
    /// </code>
    /// </example>
    method QuoteIdentifier(cId as string) as string
        return _cmdBuilder:QuoteIdentifier(cId)
    end method
    /// <summary>
    /// Create a provider specific DbCommand Object
    /// </summary>
    /// <returns>A new DbCommand object</returns>
    virtual method CreateCommand() as DbCommand
        return Factory:CreateCommand()
    end method
    /// <summary>
    /// Create a provider specific DbCommandBuilder Object
    /// </summary>
    /// <returns>A new DbCommandBuilder object</returns>
    virtual method CreateCommandBuilder() as DbCommandBuilder
        return Factory:CreateCommandBuilder()
    end method

    /// <summary>
    /// Create a provider specific DbConnection Object
    /// </summary>
    /// <returns>A new DbConnection object</returns>
    virtual method CreateConnection() as DbConnection
        return Factory:CreateConnection()
    end method

    /// <summary>
    /// Create a provider specific DbConnectionStringBuilder Object
    /// </summary>
    /// <returns>A new DbConnectionStringBuilder object</returns>
    virtual method CreateConnectionStringBuilder() as DbConnectionStringBuilder
        return Factory:CreateConnectionStringBuilder()
    end method

    /// <summary>
    /// Create a provider specific DbParameter Object
    /// </summary>
    /// <returns>A new DbParameter object</returns>
    virtual method CreateParameter() as DbParameter
        return Factory:CreateParameter()
    end method

    /// <summary>
    /// Create a provider specific DbDataSourceEnumerator Object
    /// </summary>
    /// <returns>A new DbDataSourceEnumerator object</returns>

    virtual method CreateDataSourceEnumerator() as DbDataSourceEnumerator
        if Factory:CanCreateDataSourceEnumerator
            return Factory:CreateDataSourceEnumerator()
        endif
        return null
    end method

    #endregion

#region Virtual Properties with statements etc
    /// <summary>
    /// Syntax for a CREATE TABLE statement.
    /// </summary>
    virtual property CreateTableStatement   as string => "create table "+TableNameMacro+" ( "+FieldDefinitionListMacro+" )"
    /// <summary>
    /// Syntax for a CREATE INDEX statement.
    /// </summary>
    virtual property CreateIndexStatement   as string => "create "+UniqueMacro+" index "+TableNameMacro+"_"+IndexNameMacro+" on "+TableNameMacro+"( "+FieldListMacro+" )"
    /// <summary>
    /// Syntax for a DROP TABLE statement.
    /// </summary>
    virtual property DropTableStatement     as string => "drop table if exists "+TableNameMacro
    /// <summary>
    /// Syntax for a DROP INDEX statement.
    /// </summary>
    virtual property DropIndexStatement     as string => "drop index "+TableNameMacro+"."+IndexNameMacro
    /// <summary>
    /// Syntax for the statement to delete all rows from a table (ZAP)
    /// </summary>
    virtual property DeleteAllRowsStatement as string => "delete from "+TableNameMacro
    /// <summary>
    /// Syntax for the statement to select a limited number of rows from the table
    /// </summary>
    virtual property SelectTopStatement     as string => "select top "+TopCountMacro+" "+ColumnsMacro+" from "+TableNameMacro
    /// <summary>
    /// Syntax for the INSERT statement
    /// </summary>
    virtual property InsertStatement        as string => "insert into "+TableNameMacro+" ( "+ColumnsMacro+") values ( "+ValuesMacro+" )"
    /// <summary>
    /// Syntax for the UPDATE statement
    /// </summary>
    virtual property UpdateStatement        as string => "update "+TableNameMacro+" set "+ColumnsMacro+" where "+WhereMacro
    /// <summary>
    /// Syntax for the DELETE statement
    /// </summary>
    virtual property DeleteStatement        as string => "delete from "+TableNameMacro+" where "+WhereMacro
    /// <summary>
    /// Syntax for the ORDER BY clause
    /// </summary>
    virtual property OrderByClause          as string => " order by "+ColumnsMacro+" "
    /// <summary>
    /// default value for MaxRows
    /// </summary>
    virtual property MaxRows                as int    => 1000
    /// <summary>
    /// Syntax for the statement to retrieve the identity value of the last inserted row
    /// </summary>
    virtual property GetIdentity            as string => ""
    /// <summary>
    /// Syntax for the statement to retrieve the number of rows updated by the last statement
    /// </summary>
    virtual property GetRowCount            as string => ""

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
    public const ColumnsMacro    := "%C%" as string
    public const ValuesMacro     := "%V%" as string
    public const WhereMacro      := "%W%" as string

    public const ConnectionDelimiter := "::" as string


#endregion
    /// <summary>
    /// Create the column definition (in SQL syntax) from a RDDFieldInfo object
    /// </summary>
    /// <param name="oInfo">Object that contains the field definition</param>
    /// <returns>a string in SQL syntax</returns>
    /// <remarks>
    /// This is used when creating a SQL table with DbCreate()
    /// </remarks>
    virtual method GetSqlColumnInfo(oInfo as RddFieldInfo) as string
        local sResult := "" as string
        var name := i"{QuoteIdentifier(oInfo.ColumnName)}"
        switch oInfo:FieldType
        case DbFieldType.Character
        case DbFieldType.VarChar
            sResult := i"{name} varchar ({oInfo.Length}) default ''"
        case DbFieldType.Date
            sResult := i"{name} date"
        case DbFieldType.Logic
            sResult := i"{name} bit default 0"
        case DbFieldType.Memo
            sResult := i"{name} varchar (max) default ''"
        case DbFieldType.Number
            sResult := i"{name} Numeric ({oInfo.Length}, {oInfo.Decimals}) default 0"
        case DbFieldType.Currency
            sResult := i"{name} decimal default 0"
        case DbFieldType.DateTime
            sResult := i"{name} datetime "
        case DbFieldType.Double
        case DbFieldType.Float
            sResult := i"{name} float default 0"
        case DbFieldType.Integer
            sResult := i"{name} int "
            sResult += "default 0"
        case DbFieldType.Blob
        case DbFieldType.General
        case DbFieldType.Picture
        case DbFieldType.VarBinary
            sResult := i"{name} binary "
        case DbFieldType.NullFlags
            sResult := ""
        end switch
        if !String.IsNullOrEmpty(sResult)
            if oInfo:Flags:HasFlag(DBFFieldFlags.Nullable)
                sResult += " null "
            endif
        endif
        return sResult
    end method

end class


end namespace // XSharp.RDD.SqlRDD
