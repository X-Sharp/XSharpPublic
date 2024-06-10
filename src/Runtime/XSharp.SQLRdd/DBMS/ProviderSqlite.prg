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
/// The SqlDbProvider for ODBC
/// </summary>
/// <remarks>
/// This class depends on the DLL System.Data.dll
/// </remarks>

class SqlDbProviderSQLite inherit SqlDbProvider
   /// <inheritdoc />
   override property DllName as string => "System.Data.Sqlite.dll"
   /// <inheritdoc />
   override property TypeName as string => "System.Data.SQLite.SQLiteFactory"
   /// <inheritdoc />
   override property CreateIndexStatement   as string => "create "+UniqueMacro+" index "+IndexNameMacro+" on "+TableNameMacro+"( "+FieldListMacro+" )"
   /// <inheritdoc />
   override property DropIndexStatement     as string => "drop index if exists "+IndexNameMacro

   /// <inheritdoc />
   override property SelectTopStatement     as string => "select "+ColumnsMacro+" from "+TableNameMacro+" limit "+TopCountMacro

   /// <inheritdoc />
   override property GetIdentity as string => "SELECT last_insert_rowid()"
   /// <inheritdoc />
   override property GetRowCount as string => "SELECT changes()"

   private static lockObj := object{} as object
   constructor()
      super("SQLite")
      return
   end constructor

   private static aFuncs := null as Dictionary<string, string>
   /// <inheritdoc />
     override method GetFunctions() as Dictionary<string, string>
      if aFuncs == null
          begin lock lockObj
              if aFuncs == null
                  aFuncs := Dictionary<string, string>{StringComparer.OrdinalIgnoreCase} {;
                      {"LEFT(%1%,%2%)"			,"SUBSTR(%1%,1,%2%)"},;
                      {"LOWER(%1%)"			   ,"LOWER(%1%)"},;
                      {"UPPER(%1%)"			   ,"UPPER(%1%)"},;
                      {"DTOS(%1%)"				,"strftime('%Y%m%d',%1%)"},;
                      {"DAY(%1%)"				,"cast(strftime('%d',%1) as int)"},;
                      {"MONTH(%1%)"				,"cast(strftime('%m',%1) as int)"},;
                      {"YEAR(%1%)"				,"cast(strftime('%Y',%1) as int)"},;
                      {"TODAY()"					,"CURRENT_DATE"},;
                      {"CHR(%1%)"				,"CHAR(%1%)"},;
                      {"LEN(%1%)"				,"LENGTH(%1%)"},;
                      {"REPL(%1%,%2%)"			,"FORMAT('%.*c',%2%,%1%)"},;
                      {"ASC(%1%)"				,"ASCII(%1%)"},;
                      {"TRIM(%1%)"				,"RTRIM(%1%)"},;
                      {"ALLTRIM(%1%)"			,"TRIM(%1%)"},;
                      {"+"						   ,"||"}}
              endif
          end lock
      endif
      return aFuncs
   /// <inheritdoc />

   override method CreateCommandBuilder() as DbCommandBuilder
      var cmdBuilder := super:CreateCommandBuilder()
      return cmdBuilder

   override method CaseSync(cIdentifier as string) as string
      return cIdentifier:ToLower()

   override method GetSqlColumnInfo(oInfo as RddFieldInfo, oConn as SqlDbConnection) as string
      local sResult as string
      local name as string
      name := SELF:QuoteIdentifier(oInfo:ColumnName)
      switch oInfo:FieldType
      case DbFieldType.Character
      case DbFieldType.VarChar
         sResult := i"{name} nvarchar ({oInfo:Length}) default ''"
      case DbFieldType.Integer
         sResult := i"{name} integer "
         if oConn:UseNulls
            if oInfo:Flags:HasFlag(DBFFieldFlags.AutoIncrement)
               sResult += " primary key AUTOINCREMENT "
            else
               sResult += " default 0"
            endif
         endif
      case DbFieldType.Date
         sResult := i"{name} date"
      case DbFieldType.DateTime
         sResult := i"{name} datetime"

      case DbFieldType.Number when oInfo:Decimals == 0
         sResult := i"{name} int default 0"
      otherwise
         sResult := super:GetSqlColumnInfo(oInfo, oConn)
      end switch
      return sResult
   end method
end class
end namespace // XSharp.RDD.SqlRDD.SupportClasses
