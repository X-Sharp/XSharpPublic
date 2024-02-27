//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

// This is a contribution from Andreas Trogmann - Riedmann GmbH - srl.


using System
using System.Collections.Generic
using XSharp.RDD.SqlRDD
using System.Data.Common
using System.Reflection
using XSharp.RDD.Enums
using XSharp.RDD.Support
begin namespace XSharp.RDD.SqlRDD.Providers

/// <summary>
/// The SqlDbProvider for MySql
/// </summary>
/// <remarks>
/// This class depends on the DLL MySql.Data.dll
/// </remarks>

class SqlDbProviderPostgresSql inherit SqlDbProvider
    /// <inheritdoc />
    override property DllName                as string => "Npgsql.dll"
    /// <inheritdoc />
    override property TypeName               as string => "Npgsql.NpgsqlFactory"
    /// <inheritdoc />
    override property GetIdentity            as string => "select LAST_INSERT_ID()"
    /// <inheritdoc />
    override property GetRowCount            as string => "select FOUND_ROWS( )"
    /// <inheritdoc />
    override property SelectTopStatement     as string => "select "+ColumnsMacro+" from "+TableNameMacro+" limit "+TopCountMacro
    private static lockObj := object{} as object
    constructor()
        super("PostgreSql")
        return
    end constructor

    private static aFuncs := null as Dictionary<string, string>
    override method GetFunctions() as Dictionary<string, string>
        if aFuncs == null
            begin lock lockObj
                if aFuncs == null
                    aFuncs := Dictionary<string, string>{StringComparer.OrdinalIgnoreCase} {;
                        {"STR(%1%,%2%)"				,"LPAD(%1%::text,%2%)"},;
                        {"LEFT(%1%,%2%)"			,"LEFT(%1%,%2%)"},;
                        {"LOWER(%1%)"			    ,"LOWER(%1%)"},;
                        {"UPPER(%1%)"			    ,"UPPER(%1%)"},;
                        {"DTOS(%1%)"				,"TO_CHAR(CURRENT_DATE, 'YYYYMMDD')"},;
                        {"DAY(%1%)"					,"EXTRACT('DAY' FROM %1%)"},;
                        {"MONTH(%1%)"				,"EXTRACT('MONTH' FROM %1%)"},;
                        {"YEAR(%1%)"				,"EXTRACT('YEAR' FROM %1%)"},;
                        {"TODAY()"					,"CURRENT_DATE"},;
                        {"CHR(%1%)"					,"CHR(%1%)"},;
                        {"LEN(%1%)"					,"LENGTH(%1%)"},;
                        {"REPL(%1%,%2%)"			,"REPEAT(%1%,%2%)"},;
                        {"ASC(%1%)"					,"ASCII(%1%)"},;
                        {"TRIM(%1%)"				,"RTRIM(%1%)"},;
                        {"ALLTRIM(%1%)"				,"TRIM(%1%)"},;
                        {"+"						,"||"}}

                endif
            end lock
        endif
        return aFuncs

    override method GetSqlColumnInfo(oInfo as RddFieldInfo, oConn as SqlDbConnection) as string
        local sResult as string
        switch oInfo:FieldType
        case DbFieldType.Character
        case DbFieldType.VarChar
            sResult := i"{QuoteIdentifier(oInfo.ColumnName)} nvarchar ({oInfo.Length}) default ''"
            if oConn:UseNulls
                if oInfo:Flags:HasFlag(DBFFieldFlags.Nullable)
                    sResult += NullClause
                else
                    sResult += NotNullClause
                endif
            endif
        case DbFieldType.Logic
            sResult := i"{QuoteIdentifier(oInfo.ColumnName)} int default 0"
        case DbFieldType.Integer
            sResult := i"{QuoteIdentifier(oInfo.ColumnName)} int "
            if oInfo:Flags:HasFlag(DBFFieldFlags.AutoIncrement)
                sResult := i"{QuoteIdentifier(oInfo.ColumnName)} serial4 "
            else
                sResult += " default 0"
            endif
        case DbFieldType.Memo
            sResult := i"{QuoteIdentifier(oInfo.ColumnName)} TEXT "
        case DbFieldType.Blob
        case DbFieldType.General
        case DbFieldType.Picture
        case DbFieldType.VarBinary
            sResult := i"{QuoteIdentifier(oInfo.ColumnName)} BLOB "

        otherwise
            sResult := super:GetSqlColumnInfo(oInfo, oConn)
        end switch
        return sResult
end class
end namespace // XSharp.RDD.SqlRDD.SupportClasses
