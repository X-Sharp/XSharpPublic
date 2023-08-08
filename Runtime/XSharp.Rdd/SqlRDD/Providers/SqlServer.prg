//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


USING System
USING System.Collections.Generic
USING System.Text
USING XSharp.RDD.SqlRDD
using System.Data.Common

BEGIN NAMESPACE XSharp.RDD.SqlRDD.Providers
    
/// <summary>
/// The SqlServer provider class.
/// </summary>
CLASS SqlServer INHERIT SqlDbProvider
    override property DllName as string => "System.Data.dll"
    override property TypeName as string => "System.Data.SqlClient.SqlClientFactory"
        
    CONSTRUCTOR() STRICT
        SUPER("SqlServer")
        RETURN
    private static aFuncs := NULL as Dictionary<String, String>
    OVERRIDE METHOD GetFunctions() AS Dictionary<String, String>
        if aFuncs == NULL
            aFuncs := Dictionary<String, String>{StringComparer.OrdinalIgnoreCase} {;
                {"STR(%1%,%2%,%3%)"			,"STR(%1%,%2%,%3%)"},;
                {"STR(%1%,%2%)"				,"STR(%1%,%2%,0)"},;
                {"SUBSTR(%1%,%2%,%3%)"		,"SUBSTRING(%1%,%,%3%)"},;
                {"DTOS(%1%)"				,"CONVERT(char(8), %1%,11)"},;
                {"IIF(%1%,%2%,%3%)"			,"CASE WHEN %1% THEN %2% ELSE %3% END"},;
                {"TODAY()"					,"GETDATE()"},;
                {"CHR(%1%)"					,"CHAR(%1%)"},;
                {"REPL(%1%,%2%)"			,"REPLICATE(%1%,%2%)"},;
                {"ASC(%1%)"					,"ASCII(%1%)"},;
                {"TRIM(%1%)"				,"RTRIM(%1%)"},;
                {"ALLTRIM(%1%)"				,"TRIM(%1%)"},;
                {"+"						,"+"}}
        endif
        return aFuncs
END CLASS
END NAMESPACE // XSharp.RDD.SqlRDD.SupportClasses
