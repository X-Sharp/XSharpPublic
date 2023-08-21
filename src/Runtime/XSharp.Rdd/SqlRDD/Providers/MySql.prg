//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


using System
using System.Collections.Generic
using XSharp.RDD.SqlRDD
using System.Data.Common
using System.Reflection

begin namespace XSharp.RDD.SqlRDD.Providers

/// <summary>
/// The Oracle provider class.
/// </summary>
class MySql inherit SqlDbProvider
    override property DllName as string => "MySql.Data.dll"
    override property TypeName as string => "MySql.Data.MySqlClient.MySqlClientFactory"


    constructor()
        super("MySql")
        return
    private static aFuncs := null as Dictionary<string, string>
    override method GetFunctions() as Dictionary<string, string>
        if aFuncs == null
            aFuncs := Dictionary<string, string>{StringComparer.OrdinalIgnoreCase} {;
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

    override property SelectTopStatement     as string => "select "+ColumnsMacro+" from "+FromClauseMacro+" limit "+TopCountMacro

end class
end namespace // XSharp.RDD.SqlRDD.SupportClasses
