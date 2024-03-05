//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


using System
using System.Collections.Generic
using System.Text
using XSharp.RDD.SqlRDD
using System.Data.Common
using XSharp.RDD.Enums
using XSharp.RDD.Support
begin namespace XSharp.RDD.SqlRDD.Providers

/// <summary>
/// The SqlDbProvider for OLEDB
/// </summary>
/// <remarks>
/// This class depends on the DLL System.Data.dll
/// </remarks>
class SqlDbProviderOleDb inherit SqlDbProvider
    /// <inheritdoc />
    override property DllName as string => "System.Data.dll"
    /// <inheritdoc />
    override property TypeName as string => "System.Data.OleDb.OleDbFactory"
    private static lockObj := object{} as object

    constructor()
        super("OleDb")
        return
    end constructor

    private static aFuncs := null as Dictionary<string, string>
    /// <inheritdoc />
    override method GetFunctions() as Dictionary<string, string>
        if aFuncs == null
            begin lock lockObj
                if aFuncs == null
                    aFuncs := Dictionary<string, string>{StringComparer.OrdinalIgnoreCase} {;
                        {"STR(%1%,%2%,%3%)"			,"{fn RIGHT({fn SPACE(%2%)}+{fn CONVERT({fn ROUND(%1%,%3%)},SQL_VARCHAR)},%2%)}"},;
                        {"STR(%1%,%2%)"				,"{fn RIGHT({fn SPACE(%2%)}+{fn CONVERT({fn ROUND(%1%,0)},SQL_VARCHAR)},%2%)}"},;
                        {"SUBSTR(%1%,%2%,%3%)"		,"{fn SUBSTRING(%1%,%2%,%3%)}"},;
                        {"DTOS(%1%)"				,"{fn CONVERT({fn YEAR(%1%)}, SQL_VARCHAR)}+{fn RIGHT('0'+ {fn CONVERT({fn MONTH(%1%)},SQL_VARCHAR)},2)}+{fn RIGHT('0'+ {fn CONVERT({fn DAYOFMONTH(%1%)},SQL_VARCHAR)},2)}"},;
                        {"DAY(%1%)"					,"{fn DAYOFMONTH(%1%)}"},;
                        {"MONTH(%1%)"				,"{fn MONTH(%1%)}"},;
                        {"YEAR(%1%)"				,"{fn YEAR(%1%)}"},;
                        {"UPPER(%1%)"				,"{fn UCASE(%1%)}"},;
                        {"LOWER(%1%)"				,"{fn LCASE(%1%)}"},;
                        {"LEN(%1%)"					,"{fn LENGTH(%1%)}"},;
                        {"CHR(%1%)"					,"{fn CHAR(%1%)}"},;
                        {"ASC(%1%)"					,"{fn ASCII(%1%)}"},;
                        {"TODAY()"					,"{fn CURDATE()}"},;
                        {"REPL(%1%,%2%)"			,"{fn REPEAT(%1%,%2%)}"},;
                        {"TRIM(%1%)"				,"{fn RTRIM(%1%)}"},;
                        {"ALLTRIM(%1%)"				,"{fn LTRIM( {fn RTRIM(%1%) } )}"},;
                        {"LEFT(%1%,%2%)"			,"{fn LEFT(%1%,%2%)}"},;
                        {"RIGHT(%1%)"				,"{fn RIGHT(%1%)}"},;
                        {"+"						,"+"}}
                endif
            end lock
        endif

        return aFuncs
    end method


    /// <inheritdoc />
    override method CreateCommandBuilder() as DbCommandBuilder
        var cmdBuilder := super:CreateCommandBuilder()
        cmdBuilder:QuotePrefix := """"
        cmdBuilder:QuoteSuffix := """"
        return cmdBuilder
    end method
end class
end namespace // XSharp.RDD.SqlRDD.SupportClasses
