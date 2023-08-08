//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


USING System
USING System.Collections.Generic
USING XSharp.RDD.SqlRDD
USING System.Data.Common
using System.Reflection

BEGIN NAMESPACE XSharp.RDD.SqlRDD.Providers

/// <summary>
/// The Oracle provider class.
/// </summary>
CLASS MySql INHERIT SqlDbProvider
    override property DllName as string => "MySql.Data.dll"
    override property TypeName as string => "MySql.Data.MySqlClient.MySqlClientFactory"


    CONSTRUCTOR()
        SUPER("MySql")
        RETURN
    private static aFuncs := NULL as Dictionary<String, String>
    OVERRIDE METHOD GetFunctions() AS Dictionary<String, String>
        if aFuncs == NULL
            aFuncs := Dictionary<String, String>{StringComparer.OrdinalIgnoreCase} {;
                {"LEFT(%1%,%2%)"			,"LEFT(%1%,%2%)"},;
                {"LOWER(%1%)"			    ,"LOWER(%1%)"},;
                {"UPPER(%1%)"			    ,"UPPER(%1%)"},;
                {"DTOS(%1%)"				,"DATE_FORMAT(%1%,'%Y%m%d')"},;
                {"DAY(%1%)"					,"DAY(%1%)"},;
                {"MONTH(%1%)"				,"MONTH(%1%)"},;
                {"YEAR(%1%)"				,"YEAR(%1%)"},;
                {"TODAY()"					,"SYSDATE()"},;
                {"CHR(%1%)"					,"CHAR(%1%)"},;
                {"LEN(%1%)"					,"LENGTH(%1%)"},;
                {"REPL(%1%,%2%)"			,"REPEAT(%1%,%2%)"},;
                {"ASC(%1%)"					,"ASCII(%1%)"},;
                {"TRIM(%1%)"				,"RTRIM(%1%)"},;
                {"ALLTRIM(%1%)"				,"TRIM(%1%)"},;
                {"+"						,"||"}}
        endif
        return aFuncs

    OVERRIDE PROPERTY SelectTopStatement     AS STRING => "select "+ColumnsMacro+" from "+FromClauseMacro+" limit "+TopCountMacro

END CLASS
END NAMESPACE // XSharp.RDD.SqlRDD.SupportClasses
