//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING System.Collections.Generic
USING System.Text
USING XSharp.RDD.SqlRDD
USING System.Data.Common

BEGIN NAMESPACE XSharp.RDD.SqlRDD.Providers

/// <summary>
/// The ODBC provider class.
/// </summary>
CLASS ODBC INHERIT SqlDbProvider
    override property DllName as string => "System.Data.dll"
    override property TypeName as string => "System.Data.Odbc.OdbcFactory"

    CONSTRUCTOR()
        SUPER("ODBC")
        RETURN

    private static aFuncs := NULL as Dictionary<String, String>
    OVERRIDE METHOD GetFunctions() AS Dictionary<String, String>
        if aFuncs == NULL
            aFuncs := Dictionary<String, String>{StringComparer.OrdinalIgnoreCase} {;
                {"STR(%1%,%2%,%3%)"			,"{fn RIGHT({fn SPACE(%2%)}+{fn CONVERT({fn ROUND(%1%,%3%)},SQL_VARCHAR)},%2%)}"},;
                {"STR(%1%,%2%)"				,"{fn RIGHT({fn SPACE(%2%)}+{fn CONVERT({fn ROUND(%1%,0)},SQL_VARCHAR)},%2%)}"},;
                {"SUBSTR(%1%,%2%,%3%)"		,"{fn SUBSTRING(%1%,%2%,%3%)}"},;
                {"DTOS(%1%)"				,"{fn CONVERT({fn YEAR(%1%)}, SQL_VARCHAR)}+{fn RIGHT('0'+ {fn CONVERT({fn MONTH(%1%)},SQL_VARCHAR)},2)}+{fn RIGHT('0'+ {fn CONVERT({fn DAYOFMONTH(%1%)},SQL_VARCHAR)},2)}"},;
                {"DAY(%1%)"					,"{fn DAYOFMONTH(%1%)}"},;
                {"MONTH(%1%)"				,"{fn MONTH(%1%)}"},;
                {"YEAR(%1%)"				,"{fn YEAR(%1%)}"},;
                {"UPPER(%1%)"				,"{fn UCASE(%1%)}"},;
                {"LOWER(%1%)"				,"{fn LCASE(%1%)}"},;
                {"LEFT(%1%,%2%)"			,"{fn LEFT(%1%,%2%)}"},;
                {"LEN(%1%)"					,"{fn LENGTH(%1%)}"},;
                {"CHR(%1%)"					,"{fn CHAR(%1%)}"},;
                {"ASC(%1%)"					,"{fn ASCII(%1%)}"},;
                {"TODAY()"					,"{fn CURDATE()}"},;
                {"REPL(%1%,%2%)"			,"{fn REPEAT(%1%,%2%)}"},;
                {"TRIM(%1%)"				,"{fn RTRIM(%1%)}"},;
                {"ALLTRIM(%1%)"				,"{fn LTRIM( {fn RTRIM(%1%) } )}"},;
                {"RIGHT(%1%)"				,"{fn RIGHT(%1%)}"},;
                {"+"						,"+"}}
        endif
        RETURN aFuncs

END CLASS
END NAMESPACE // XSharp.RDD.SqlRDD.SupportClasses
