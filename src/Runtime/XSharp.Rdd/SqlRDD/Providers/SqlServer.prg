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

begin namespace XSharp.RDD.SqlRDD.Providers

/// <summary>
/// The SqlServer provider class.
/// </summary>
class SqlServer inherit SqlDbProvider
    override property DllName as string => "System.Data.dll"
    override property TypeName as string => "System.Data.SqlClient.SqlClientFactory"

    constructor() strict
        super("SqlServer")
        return
    private static aFuncs := null as Dictionary<string, string>
    override method GetFunctions() as Dictionary<string, string>
        if aFuncs == null
            aFuncs := Dictionary<string, string>{StringComparer.OrdinalIgnoreCase} {;
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
end class
end namespace // XSharp.RDD.SqlRDD.SupportClasses
