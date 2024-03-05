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
using XSharp.RDD.Enums
using XSharp.RDD.Support
begin namespace XSharp.RDD.SqlRDD.Providers

/// <summary>
/// The SqlDbProvider for Oracle, using the System.Data.OracleClient namespace.
/// </summary>
/// <remarks>
/// This class depends on the DLL System.Data.OracleClient.dll
/// </remarks>
class SqlDbProviderOracle inherit SqlDbProvider
    /// <inheritdoc />
    override property DllName as string => "System.Data.OracleClient.dll"
    /// <inheritdoc />
    override property TypeName as string => "System.Data.OracleClient.OracleClientFactory"
    private static lockObj := object{} as object

    /// <inheritdoc />
    override property GetIdentity            as string => "select LAST_INSERT_ID()"
    /// <inheritdoc />
    override property GetRowCount            as string => "select SQL%ROWCOUNT"
    /// <inheritdoc />
    override property SelectTopStatement     as string => "select "+ColumnsMacro+" from "+TableNameMacro+" top "+TopCountMacro


    constructor()
        super("Oracle")
        return
    end constructor

    private static aFuncs as Dictionary<string, string>
    override method GetFunctions() as Dictionary<string, string>
        if aFuncs == null
            begin lock lockObj
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
            end lock
        endif
        return aFuncs

    override method GetSqlColumnInfo(oInfo as RddFieldInfo, oConn as SqlDbConnection) as string
        local sResult as string
        switch oInfo:FieldType
        case DbFieldType.Character
        case DbFieldType.VarChar
            sResult := i"{QuoteIdentifier(oInfo.ColumnName)} NVARCHAR ({oInfo.Length}) default ''"
            if oConn:UseNulls
                if oInfo:Flags:HasFlag(DBFFieldFlags.Nullable)
                    sResult += NullClause
                else
                    sResult += NotNullClause
                endif
            endif
        case DbFieldType.DateTime
            sResult := i"{QuoteIdentifier(oInfo.ColumnName)} TIMESTAMP "

        case DbFieldType.Double
        case DbFieldType.Float
            sResult := i"{QuoteIdentifier(oInfo.ColumnName)} FLOAT ({oInfo.Decimals}) DEFAULT 0"

        case DbFieldType.Number
            sResult := i"{QuoteIdentifier(oInfo.ColumnName)} NUMBER ({oInfo.Length}, {oInfo.Decimals}) DEFAULT 0"
        case DbFieldType.Integer
            sResult := i"{QuoteIdentifier(oInfo.ColumnName)} NUMBER (10,0) "
            if oInfo:Flags:HasFlag(DBFFieldFlags.AutoIncrement)
                sResult += " GENERATED ALWAYS AS IDENTITY "
            else
                sResult += "default 0"
            endif
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
