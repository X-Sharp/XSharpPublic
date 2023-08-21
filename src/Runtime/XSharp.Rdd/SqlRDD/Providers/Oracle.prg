﻿//
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
class Oracle inherit SqlDbProvider
    override property DllName as string => "System.Data.OracleClient.dll"
    override property TypeName as string => "System.Data.OracleClient.OracleClientFactory"

    constructor()
        super("Oracle")
        return
    private static aFuncs as Dictionary<string, string>
    override method GetFunctions() as Dictionary<string, string>
        if aFuncs == null
            aFuncs := Dictionary<string, string>{StringComparer.OrdinalIgnoreCase} {;
                {"LEFT(%1%,%2%)"			,"SUBSTR(%1%,1,%2%)"},;
                {"DTOS(%1%)"				,"TO_CHAR(%1%,'YYYYMMDD')"},;
                {"DAY(%1%)"					,"TO_NUM(TO_CHAR(%1%,'DD'))"},;
                {"MONTH(%1%)"				,"TO_NUM(TO_CHAR(%1%,'MM'))"},;
                {"YEAR(%1%)"				,"TO_NUM(TO_CHAR(%1%,'YYYY'))"},;
                {"TODAY()"					,"SYSDATE "},;
                {"CHR(%1%)"					,"CHAR(%1%)"},;
                {"LEN(%1%)"					,"LENGTH(%1%)"},;
                {"REPL(%1%,%2%)"			,"REPLICATE(%1%,%2%)"},;
                {"ASC(%1%)"					,"ASCII(%1%)"},;
                {"TRIM(%1%)"				,"RTRIM(%1%)"},;
                {"ALLTRIM(%1%)"				,"LTRIM(RTRIM(%1%))"},;
                {"+"						,"||"}}
        endif
        return aFuncs

    override property SelectTopStatement     as string => "select * from (select "+ColumnsMacro+" from "+FromClauseMacro+" ) where RowNum <= "+TopCountMacro

end class
end namespace // XSharp.RDD.SqlRDD.SupportClasses
