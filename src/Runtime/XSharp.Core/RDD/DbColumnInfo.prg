//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING System.Collections.Generic
USING System.Text
USING System.Diagnostics
USING XSharp.RDD.Support
USING XSharp.RDD.Enums


/// <summary>This class describes extended information for a field in a workarea, for fields that come from a SQL backend.</summary>
[DebuggerDisplay("{ColumnName,nq} #{Ordinal} ({FieldTypeStr,nq} {Length} {Decimals}), ")];
CLASS XSharp.RDD.DbColumnInfo INHERIT RddFieldInfo
    /// <summary>DotNet datatype of the column</summary>
    PROPERTY DotNetType     AS System.Type AUTO
    /// <summary>Numeric Scale</summary>
    PROPERTY NumericScale   AS LONG AUTO
    /// <summary>Numeric Precision</summary>
    PROPERTY NumericPrecision AS LONG AUTO
    /// <summary>Numeric Precision</summary>
    PROPERTY ReadOnly AS LOGIC AUTO
    /// <summary>Initializes a new instance of the DbColumnInfo class</summary>
    CONSTRUCTOR(sName AS STRING, sType AS STRING, nLength AS LONG, nDecimals AS LONG, nOffSet := -1 AS LONG)
        SUPER(sName, sType, nLength, nDecimals)
        SELF:ColumnName := sName
        SELF:Caption    := SELF:ColumnName
        SELF:CalculateColumnType()

    /// <summary>Initializes a new instance of the DbColumnInfo class</summary>
    CONSTRUCTOR(oInfo AS RddFieldInfo)
        SUPER(oInfo)
        SELF:CalculateColumnType()
        RETURN

    /// <summary>Calculate the column type from the FieldType</summary>
    METHOD CalculateColumnType() AS VOID
        SWITCH SELF:FieldType
        CASE DbFieldType.Character
            SELF:DotNetType := typeof(STRING)
        CASE DbFieldType.Date
            SELF:DotNetType := typeof(System.DateTime)
        CASE DbFieldType.Number
            IF SELF:Decimals > 0
                SELF:DotNetType := typeof(REAL8)
            ELSE
                SELF:DotNetType := typeof(LONG)
            ENDIF
        CASE DbFieldType.Logic
            SELF:DotNetType := typeof(LOGIC)
        CASE DbFieldType.Memo
            SELF:DotNetType := typeof(STRING)
        CASE DbFieldType.Blob
            SELF:DotNetType := typeof(BYTE[])
        CASE DbFieldType.Currency
            SELF:DotNetType := typeof(DECIMAL)
        CASE DbFieldType.Double
            SELF:DotNetType := typeof(REAL8)
        CASE DbFieldType.DateTime
            SELF:DotNetType := typeof(System.DateTime)
        CASE DbFieldType.Float
            SELF:DotNetType := typeof(REAL8)
        CASE DbFieldType.General
            SELF:DotNetType := typeof(BYTE[])
        CASE DbFieldType.Integer
            SELF:DotNetType := typeof(LONG)
        CASE DbFieldType.Picture
            SELF:DotNetType := typeof(BYTE[])
        CASE DbFieldType.VarBinary
            SELF:DotNetType := typeof(BYTE[])
        CASE DbFieldType.VarChar
            SELF:DotNetType := typeof(STRING)
        END SWITCH
        RETURN

 END CLASS
