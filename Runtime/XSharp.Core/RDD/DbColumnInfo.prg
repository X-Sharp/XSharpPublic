// DbColumnInfo.prg
// Created by    : robert
// Creation Date : 4/7/2020 2:02:47 PM
// Created for   : 
// WorkStation   : ARTEMIS


USING System
USING System.Collections.Generic
USING System.Text
USING System.Diagnostics
USING XSharp.RDD.Support
USING XSharp.RDD.Enums

[DebuggerDisplay("{ColumnName,nq} ({FieldTypeStr,nq} {Length} {Decimals})")];
CLASS XSharp.RDD.DbColumnInfo INHERIT RddFieldInfo
    PROPERTY ColumnName     AS STRING AUTO
    PROPERTY DotNetType     AS System.Type AUTO
    PROPERTY NumericScale   AS LONG AUTO
    PROPERTY NumericPrecision AS LONG AUTO
    PROPERTY Description    AS STRING AUTO
    PROPERTY Ordinal        AS LONG AUTO
    
    CONSTRUCTOR(sName AS STRING, sType AS STRING, nLength AS LONG, nDecimals AS LONG, nOffSet := -1 AS LONG)
        SUPER(sName, sType, nLength, nDecimals)
        SELF:ColumnName := sName
        SELF:CalculateColumnType()

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
