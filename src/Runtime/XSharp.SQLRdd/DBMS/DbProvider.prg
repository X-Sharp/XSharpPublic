//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


using System
using System.Reflection
using System.Collections.Concurrent
using System.Collections.Generic
using System.Text
using System.Data.Common
using XSharp.RDD.Enums
using XSharp.RDD.Support
begin namespace XSharp.RDD.SqlRDD.Providers

/// <summary>
/// The SqlDbProvider class. Abstract base class for other providers.
/// </summary>
abstract class SqlDbProvider inherit SqlDbObject implements ISqlDbProvider

#region Static fields and methods
    private static _ProviderClasses as ConcurrentDictionary<string, System.Type>
    private static _ProviderObjects as ConcurrentDictionary<string, ISqlDbProvider>
    private static _lastException as Exception
    private static lockObj := object{} as object

    /// <summary>
    /// The defaut SqlDbprovider
    /// </summary>
    static property Current as ISqlDbProvider auto

    static constructor()
        Current := null
        _ProviderClasses := ConcurrentDictionary<string,  System.Type>{StringComparer.OrdinalIgnoreCase}
        _ProviderObjects := ConcurrentDictionary<string, ISqlDbProvider>{StringComparer.OrdinalIgnoreCase}
        RegisterProvider("SQLSERVER",typeof(XSharp.RDD.SqlRDD.Providers.SqlDbProviderSqlServer))
        RegisterProvider("ODBC", typeof(XSharp.RDD.SqlRDD.Providers.SqlDbProviderODBC))
        RegisterProvider("OLEDB",typeof(XSharp.RDD.SqlRDD.Providers.SqlDbProviderOleDb))
        RegisterProvider("MYSQL",typeof(XSharp.RDD.SqlRDD.Providers.SqlDbProviderMySql))
        RegisterProvider("ADS",typeof(XSharp.RDD.SqlRDD.Providers.SqlDbProviderAdvantage))
        RegisterProvider("ADVANTAGE",typeof(XSharp.RDD.SqlRDD.Providers.SqlDbProviderAdvantage))
        RegisterProvider("ORACLE",typeof(XSharp.RDD.SqlRDD.Providers.SqlDbProviderOracle ))
        RegisterProvider("POSTGRESQL",typeof(XSharp.RDD.SqlRDD.Providers.SqlDbProviderPostgreSQL ))
        RegisterProvider("SQLITE",typeof(XSharp.RDD.SqlRDD.Providers.SqlDbProviderSQLite ))
        SetDefaultProvider("ODBC")
    end constructor


    /// <summary>
    /// Register a provider in the table of providers
    /// </summary>
    /// <param name="Name">Unique name</param>
    /// <param name="ClassName">Dotnet type of the class that implements the provider</param>
    /// <returns>TRUE when succesfully registered</returns>
    static method RegisterProvider(Name as string, ClassName as System.Type) as logic
        if _ProviderClasses:ContainsKey(Name)
            _ProviderClasses:TryRemove(Name, out var _)
            return _ProviderClasses:TryAdd(Name, ClassName)
        endif
        return _ProviderClasses:TryAdd(Name, ClassName)
    end method


    /// <summary>
    /// UnRegister a provider by name
    /// </summary>
    /// <param name="name">Name that was previously registered</param>
    /// <returns>TRUE when succesfully removed</returns>
    static method UnRegisterProvider(Name as string) as logic
        return _ProviderClasses:TryRemove(Name, out var _)
    end method

    /// <summary>
    /// Set the default provider
    /// </summary>
    /// <param name="provider">Provider object</param>
    /// <returns>previous default provider</returns>
    static method SetDefaultProvider(provider as ISqlDbProvider) as ISqlDbProvider
        var old := Current
        Current := provider
        return old
    end method

    /// <summary>
    /// Set the default provider by name
    /// </summary>
    /// <param name="name">Name that was previously registered</param>
    /// <returns>previous default provider</returns>
    static method SetDefaultProvider(name as string) as ISqlDbProvider
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

    /// <summary>
    /// Get the provider object for a provide name
    /// </summary>
    /// <param name="name">Name that was previously registered</param>
    /// <returns>The provider object or NULL when there was an error</returns>
    /// <remarks>
    /// When the provider object is not yet created, it will be created from the registered type name and stored in a dictionary
    /// </remarks>
    static method GetProvider(Name as string) as ISqlDbProvider
        if _ProviderObjects:TryGetValue(Name, out var result)
            return result
        endif
        if ! _ProviderClasses:TryGetValue(Name, out var type)
            return null
        endif
        try
            if typeof(ISqlDbProvider):IsAssignableFrom(type)
                var oProv := (ISqlDbProvider) Activator.CreateInstance(type)
                if (oProv:Factory != null)
                    _ProviderObjects:TryAdd(Name, oProv)
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
    /// <summary>
    /// The DbProviderFactory for this provider
    /// </summary>
    /// <remarks>
    /// This property is lazy loaded. The first time it is accessed the factory is loaded from the DLL and type name
    /// </remarks>
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
    /// <inheritdoc/>
    abstract property DllName as string get
    /// <inheritdoc/>
    abstract property TypeName as string get
    /// <summary>
    /// Get a dictionary with the functions that are supported by this provider,
    /// </summary>
    /// <remarks>
    /// The first string is the Xbase function name
    /// The second string is the SQL function name
    /// </remarks>
    /// <returns>The dictionary</returns>
    abstract method GetFunctions() as Dictionary<string, string>
    /// <inheritdoc/>
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

    /// <inheritdoc/>
    virtual method QuoteIdentifier(cId as string) as string
        return _cmdBuilder:QuoteIdentifier(self:CaseSync(cId))
    end method
    /// <inheritdoc/>
    virtual method CreateCommand() as DbCommand
        return Factory:CreateCommand()
    end method
        /// <inheritdoc/>

    virtual method CreateCommandBuilder() as DbCommandBuilder
        return Factory:CreateCommandBuilder()
    end method

    /// <inheritdoc/>
    virtual method CreateConnection() as DbConnection
        return Factory:CreateConnection()
    end method

    /// <inheritdoc/>
    virtual method CreateConnectionStringBuilder() as DbConnectionStringBuilder
        return Factory:CreateConnectionStringBuilder()
    end method

    /// <inheritdoc/>
    virtual method CreateParameter() as DbParameter
        return Factory:CreateParameter()
    end method

    /// <inheritdoc/>
    virtual method CreateDataSourceEnumerator() as DbDataSourceEnumerator
        if Factory:CanCreateDataSourceEnumerator
            return Factory:CreateDataSourceEnumerator()
        endif
        return null
    end method

    #endregion

#region Virtual Properties with statements etc
    /// <inheritdoc/>
    /// <remarks>
    /// The default implementation returns <code> "create table "+TableNameMacro+" ( "+FieldDefinitionListMacro+" )" </code>
    /// </remarks>
    virtual property CreateTableStatement   as string => "create table "+TableNameMacro+" ( "+FieldDefinitionListMacro+" )"
    /// <inheritdoc/>
    /// <remarks>
    /// The default implementation returns <code> "create "+UniqueMacro+" index "+TableNameMacro+"_"+IndexNameMacro+" on "+TableNameMacro+"( "+FieldListMacro+" )" </code>
    /// </remarks>
    virtual property CreateIndexStatement   as string => "create "+UniqueMacro+" index "+IndexNameMacro+" on "+TableNameMacro+"( "+FieldListMacro+" )"
    /// <inheritdoc/>
    /// <remarks>
    /// The default implementation returns <code> "drop table if exists "+TableNameMacro </code>
    /// </remarks>
    virtual property DropTableStatement     as string => "drop table if exists "+TableNameMacro
    /// <inheritdoc/>
    /// <remarks>
    /// The default implementation returns <code>"drop index if exists "+IndexNameMacro+" on "+TableNameMacro</code>
    /// </remarks>
    virtual property DropIndexStatement     as string => "drop index if exists "+IndexNameMacro+" on "+TableNameMacro
    /// <inheritdoc/>
    /// <remarks>
    /// The default implementation returns <code> "delete from "+TableNameMacro </code>
    /// </remarks>
    virtual property DeleteAllRowsStatement as string => "delete from "+TableNameMacro
    /// <inheritdoc/>
    /// <remarks>
    /// The default implementation returns <code> "select top "+TopCountMacro+" "+ColumnsMacro+" from "+TableNameMacro </code>
    /// </remarks>
    virtual property SelectTopStatement     as string => "select top "+TopCountMacro+" "+ColumnsMacro+" from "+TableNameMacro
    /// <inheritdoc/>
    /// <remarks>
    /// The default implementation returns <code> "insert into "+TableNameMacro+" ( "+ColumnsMacro+") values ( "+ValuesMacro+" )" </code>
    /// </remarks>
    virtual property InsertStatement        as string => "insert into "+TableNameMacro+" ( "+ColumnsMacro+") values ( "+ValuesMacro+" )"
    /// <inheritdoc/>
    /// <remarks>
    /// The default implementation returns <code> "update "+TableNameMacro+" set "+ColumnsMacro+" where "+WhereMacro </code>
    /// </remarks>
    virtual property UpdateStatement        as string => "update "+TableNameMacro+" set "+ColumnsMacro+" where "+WhereMacro
    /// <inheritdoc/>
    /// <remarks>
    /// The default implementation returns <code> "delete from "+TableNameMacro+" where "+WhereMacro </code>
    /// </remarks>
    virtual property DeleteStatement        as string => "delete from "+TableNameMacro+" where "+WhereMacro
    /// <inheritdoc/>
    /// <remarks>
    /// The default implementation returns <code> order by "+ColumnsMacro+" " </code>
    /// </remarks>
    virtual property OrderByClause          as string => " order by "+ColumnsMacro+" "
    /// <inheritdoc/>
    /// <remarks>
    /// The default implementation returns an empty string
    /// </remarks>
    virtual property GetIdentity            as string => ""

    /// <inheritdoc/>
    /// <remarks>
    /// The default implementation returns an empty string
    /// </remarks>
    virtual property GetRowCount            as string => ""

    /// <inheritdoc />
    /// <remarks>
    /// The default implementation returns the value "true"
    /// </remarks>
    virtual property TrueLiteral            as string => "true"
    /// <inheritdoc />
    /// <remarks>
    /// The default implementation returns the value "false"
    /// </remarks>
    virtual property FalseLiteral            as string => "false"


#endregion

    #region Constants
    /// <summary>Literal that can be used in SQL statements to indicate a SELECT clause</summary>
    public const SelectClause    := "select " as string
    /// <summary>Literal that can be used in SQL statements to indicate a FROM clause</summary>
    public const FromClause      := " from "  as string
    /// <summary>Literal that can be used in SQL statements to indicate a WHERE clause</summary>
    public const WhereClause     := " where " as string
    /// <summary>Literal that can be used in SQL statements to indicate a AND operator</summary>
    public const AndClause       := " and " as string
    /// <summary>Literal that can be used in SQL statements to indicate a OR operator</summary>
    public const OrClause        := " or "  as string
    /// <summary>Literal that can be used in SQL statements to indicate that a column may contain null values</summary>
    public const NullClause      := " null "  as string
    /// <summary>Literal that can be used in SQL statements to indicate that a column should not contain null values</summary>
    public const NotNullClause   := " not null "  as string

    /// <summary>Literal that can be used in SQL statements to indicate a field list</summary>
    public const FieldListMacro  := "%FL%" as string
    /// <summary>Literal that can be used in SQL statements to indicate an index name</summary>
    public const IndexNameMacro  := "%I%" as string
    /// <summary>Literal that can be used in SQL statements to indicate the top count</summary>
    public const TopCountMacro   := "%N%" as string
    /// <summary>Literal that can be used in SQL statements to indicate the table name</summary>
    public const TableNameMacro  := "%T%" as string
    /// <summary>Literal that can be used in SQL statements to indicate the unique keyword</summary>
    public const UniqueMacro     := "%U%" as string
    /// <summary>Literal that can be used in SQL statements to indicate a fielddefinition list</summary>
    public const FieldDefinitionListMacro := "%FDL%" as string
    /// <summary>Literal that can be used in SQL statements to indicate a list of columns</summary>
    public const ColumnsMacro    := "%C%" as string
    /// <summary>Literal that can be used in SQL statements to indicate a list of values</summary>
    public const ValuesMacro     := "%V%" as string
    /// <summary>Literal that can be used in SQL statements to indicate a WHERE clause</summary>
    public const WhereMacro      := "%W%" as string
    /// <summary>Connection Delimiter (::) </summary>
    public const ConnectionDelimiter := "::" as string


#endregion
    /// <inheritdoc/>
    virtual method GetSqlColumnInfo(oInfo as RddFieldInfo, oConn as SqlDbConnection) as string
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
        if !String.IsNullOrEmpty(sResult) .and. oConn:UseNulls
            if oInfo:Flags:HasFlag(DBFFieldFlags.Nullable)
                sResult += " null "
            else
                sResult += " not null "
            endif
        endif
        return sResult
    end method

    /// <inheritdoc />
    /// <remarks>The default implementation returns the identifier in lowercase</remarks>
    virtual method CaseSync(cIdentifier as string) as string
        return cIdentifier:ToLower()
    end method
end class


end namespace // XSharp.RDD.SqlRDD
