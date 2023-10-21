//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.IO
USING XSharp.RDD.Enums
USING System.Collections.Generic
USING System.Linq
USING XSharp.RDD
USING System.Data
USING System.Reflection

BEGIN NAMESPACE XSharp
/// <exclude />
STATIC CLASS SQLHelpers

    STATIC PRIVATE INITONLY aFieldNames AS System.Collections.Generic.Dictionary<STRING, STRING>
    STATIC CONSTRUCTOR
        aFieldNames   := System.Collections.Generic.Dictionary<STRING, STRING>{}

    STATIC METHOD GetColumnInfo (aFieldNames as IList<STRING>, columnName as STRING, oType as System.Type, nSize as LONG, ;
            nScale as LONG, nPrec as LONG, IsAutoInc as LOGIC, lAllowNull as LOGIC, lReadOnly as LOGIC,;
            nOrdinal as LONG, lLongNamesAllowed AS LOGIC) AS DbColumnInfo
        LOCAL TC AS TypeCode
        LOCAL nLen as LONG
        LOCAL nDec as LONG
        LOCAL result as DbColumnInfo
        LOCAL cType as STRING
        TC      := Type.GetTypeCode(oType)
        nDec    := 0
        SWITCH TC
        CASE TypeCode.String
            cType   := "C"
            nLen    := nSize
            // Automatically Convert Long Strings to Memos
            IF nLen  > 255 .OR. nLen < 0
                nLen    := 10
                cType   := "M"
            ENDIF
            result := DbColumnInfo{columnName,cType,nLen ,0 }

        CASE TypeCode.Boolean
            result := DbColumnInfo{columnName,"L",1 ,0 }

        CASE TypeCode.Decimal
            result := DbColumnInfo{columnName,"Y",16 ,4 }
            result:NumericScale     := 16
            result:NumericPrecision := 4

        CASE TypeCode.Double
        CASE TypeCode.Single
            nDec := 1
            nLen := 10
            IF nScale == 255
                nScale := 2
            ENDIF
            nDec := nScale
            nLen := nPrec
            IF nLen == 0
                // I have seen a case where nDec == 31 and nLen == 0
                // Fix this to something usefull
                nDec := 2
                nLen := 10
            ELSEIF nDec == 127
                //IF nLen == 38 .AND. oProviderType = ProviderType.Oracle // Standardvalue for calculated fields in oracle-queries, cutting decimals leads to wrong results in that case
                //	nDec := 10
                //ELSE
                nDec := 0 // Overflow abfangen
                //ENDIF
            ENDIF
            result := DbColumnInfo{columnName,"N",nLen ,nDec }
            result:NumericScale     := nScale
            result:NumericPrecision := nPrec

        CASE TypeCode.Int32		// -2147483647 - 2147483648 (2^31)
            result := DbColumnInfo{columnName,"I",4 ,0}
            IF IsAutoInc
                result:Flags |= DBFFieldFlags.AutoIncrement
            ENDIF

        CASE TypeCode.Int64		// - 9223372036854775807 - 9223372036854775808 (2^63)
            result := DbColumnInfo{columnName,"N",21 ,0}

        CASE TypeCode.Int16	// -32767 - 32768 (2^15)
            result := DbColumnInfo{columnName,"N",6 ,0}

        CASE TypeCode.Byte
            result := DbColumnInfo{columnName,"N",4 ,0}

        CASE TypeCode.SByte	// 0 - 255 	(2^8)
            result := DbColumnInfo{columnName,"N",3 ,0}

        CASE TypeCode.UInt16	// 0 - 65535 (2^16)
            result := DbColumnInfo{columnName,"N",5 ,0}

        CASE TypeCode.UInt32		// 0 - 4294836225 (2^32)
            result := DbColumnInfo{columnName,"N",10 ,0}

        CASE TypeCode.UInt64	// 0 - 18445618199572250625 (2^64)
            nLen := 20
            result := DbColumnInfo{columnName,"N",nLen ,0}

        CASE TypeCode.DateTime
            nLen 	:= 8
            IF nPrec <= 10
                cType   := "D"
            ELSE
                cType   := "T"
            ENDIF
            result  := DbColumnInfo{columnName,cType,nLen ,0}

        CASE TypeCode.Object
            IF oType == typeof(BYTE[])
                cType   := "P"
                nLen 	:= 4
                result  := DbColumnInfo{columnName,cType,nLen ,0}

            ELSE
                LOCAL lIsDate := FALSE AS LOGIC
                LOCAL oMems AS MethodInfo[]
                LOCAL lFound := FALSE AS LOGIC
                // check to see if the datatype has a dbType
                oMems := oType:GetMethods(BindingFlags.Public|BindingFlags.Static)
                FOREACH oMem AS MethodInfo IN oMems
                    IF oMem:ReturnType == TypeOf(System.DateTime)  .AND. String.Compare(oMem:Name, "op_Explicit", StringComparison.OrdinalIgnoreCase) == 0
                        lIsDate := TRUE
                        lFound  := TRUE
                        EXIT
                    ENDIF
                NEXT
                IF ! lFound
                    LOCAL cTypeName AS STRING
                    cTypeName := oType:Name:ToUpperInvariant()
                    lIsDate     := cTypeName:Contains("DATE")
                ENDIF
                IF lIsDate
                    cType   := "D"
                    nLen 	:= 8
                ELSE
                    cType 	:= "C"
                    nLen 	:= 10
                ENDIF
                result := DbColumnInfo{columnName,cType,nLen ,0}
            ENDIF
        OTHERWISE
            cType := "C"
            nLen 	:= nSize
            IF nLen <= 0
                cType := "M"
                nLen  := 4
            ENDIF
            result := DbColumnInfo{columnName,cType,nLen ,0}

        END SWITCH
        IF lAllowNull
            result:Flags |= DBFFieldFlags.Nullable
        ENDIF
        VAR cFldName        := SQLHelpers.CleanupColumnName(columnName)
        result:ColumnName   := columnName
        result:Name         := MakeFieldNameUnique(cFldName, aFieldNames, lLongNamesAllowed)
        result:Alias        := result:Name
        result:DotNetType   := oType
        result:ReadOnly     := lReadOnly
        result:Ordinal      := nOrdinal
        RETURN result


    STATIC METHOD GetColumnInfoFromSchemaRow(oColumn AS DataColumn, aFieldNames AS IList<STRING>, lLongNamesAllowed := FALSE AS LOGIC) AS DbColumnInfo
        RETURN GetColumnInfo(aFieldNames, oColumn:ColumnName, oColumn:DataType, oColumn:MaxLength, -1,-1, ;
            oColumn:AutoIncrement, oColumn:AllowDBNull, oColumn:ReadOnly, oColumn:Ordinal, lLongNamesAllowed)


    INTERNAL CONST COLUMNNAME := "ColumnName" AS STRING
    INTERNAL CONST DATATYPE := "DataType" as STRING
    INTERNAL CONST NUMERICSCALE := "NumericScale" as STRING
    INTERNAL CONST NUMERICPRECISION := "NumericPrecision" as STRING
    INTERNAL CONST COLUMNORDINAL := "ColumnOrdinal" as STRING
    INTERNAL CONST COLUMNSIZE := "ColumnSize" as STRING
    INTERNAL CONST ISREADONLY := "IsReadOnly" as STRING
    INTERNAL CONST ISAUTOINCREMENT := "IsAutoIncrement" AS STRING
    INTERNAL CONST ALLOWDBNULL := "AllowDBNull" AS STRING
    INTERNAL STATIC METHOD SafeConvert<T>(oValue as OBJECT, oDefault AS T) AS T
        IF oValue IS T var tValue
            RETURN tValue
        ENDIF
        RETURN oDefault

    STATIC METHOD GetColumnInfoFromSchemaRow(schemaRow AS DataRow, aFieldNames AS IList<STRING>, lLongNamesAllowed := FALSE AS LOGIC) AS DbColumnInfo
        VAR columnName  := schemaRow[COLUMNNAME]:ToString( )
        VAR oType       := (Type) schemaRow[DATATYPE]
        VAR nSize       := SafeConvert(schemaRow[COLUMNSIZE], 0)
        VAR nScale      := SafeConvert(schemaRow:Item[NUMERICSCALE], (short) 0)
        VAR nPrec       := SafeConvert(schemaRow:Item[NUMERICPRECISION], (short) 0)
        VAR lAuto       := SafeConvert(schemaRow[ISAUTOINCREMENT], FALSE)
        VAR lNull       := SafeConvert(schemaRow[ALLOWDBNULL], FALSE)
        VAR lRO         := SafeConvert(schemaRow[ISREADONLY], FALSE)
        var nOrd        := SafeConvert(schemaRow[COLUMNORDINAL], 0)
        RETURN GetColumnInfo(aFieldNames, columnName, oType, nSize ,nScale, nPrec, lAuto, lNull, lRO, nOrd, lLongNamesAllowed)


    STATIC METHOD  CleanupColumnName( cColumnName AS  STRING ) AS STRING
        LOCAL sb AS System.Text.StringBuilder
        LOCAL lLastWasOk AS LOGIC
        LOCAL cResult AS STRING
        LOCAL cWork		AS STRING
        if aFieldNames:TryGetValue(cColumnName, out var name)
            RETURN name
        ENDIF
        // return only allowed characters
        sb  := System.Text.StringBuilder{}
        // When the column is an expresion like CONCAT( foo, bar)
        // Then remove the function name and the last closing param
        // when there is more than one function we remove all functions
        cWork := cColumnName
        DO WHILE cWork:Contains("(") .AND. cWork:Contains(")") .AND. cWork:IndexOf("(") < cWork:IndexOf(")")
            cWork := cWork:Substring(cWork:IndexOf('(')+1)
            cWork := cWork:Substring(0, cWork:LastIndexOf(')'))
        ENDDO
        // Remove the paramter delimiters
        cWork := cWork:Replace(",", "")
        lLastWasOk := FALSE
        FOREACH IMPLIED cChar IN cWork
            IF Char.IsLetterOrDigit(cChar) .or. cChar == c'_'
                sb:Append(cChar)
                lLastWasOk := TRUE
            ELSEIF lLastWasOk
                sb:Append('_')
                lLastWasOk := FALSE
            ENDIF
        NEXT
        IF sb:Length == 0
            // Something like "Count(*)" was passed in
            cWork := cColumnName
            cWork := cWork:Replace("(", "")
            cWork := cWork:Replace(")", "")
            FOREACH IMPLIED cChar IN cWork
                IF Char.IsLetterOrDigit(cChar) .or. cChar == c'_'
                    sb:Append(cChar)
                    lLastWasOk := TRUE
                ELSEIF lLastWasOk
                    sb:Append('_')
                    lLastWasOk := FALSE
                ENDIF
            NEXT
        ENDIF
        IF sb:Length > 1
            DO WHILE sb[sb:Length-1] == c'_'
                sb:Remove(sb:Length-1,1)
            ENDDO
        ENDIF
        IF sb:Length == 0
            cResult := "EXPR"
        ELSE
            cResult := sb:ToString()
        ENDIF
        aFieldNames:Add(cColumnName, cResult)
        RETURN cResult

    STATIC METHOD MakeFieldNameUnique(cName AS STRING, aFldNames AS IList<STRING>,lLongNamesAllowed AS LOGIC ) AS STRING
        LOCAL dwPos, dwFld AS LONG
        LOCAL cNewname		 AS STRING
        IF String.IsNullOrEmpty(cName)
            dwFld := 0
            dwPos := 1
            DO WHILE dwPos >= 0
                ++dwFld
                cName := "FLD"+dwFld:ToString():PadLeft(3,c'0')
                dwPos := aFldNames:IndexOf(cName:ToUpper())
            ENDDO
        ELSE
            // remove column prefixes
            dwPos := cName:IndexOf(".")+1
            IF dwPos > 0
                cName := cName:Substring(dwPos)
            ENDIF
            // remove embedded spaces
            cName 	:= cName:Replace(" ", "_"):ToUpper()
            IF ! lLongNamesAllowed
                cNewname := Left(cName,10)
            ELSE
                cNewname := cName
            ENDIF
            dwFld 	:= 1
            DO WHILE aFldNames:IndexOf(cNewname) >= 0
                ++dwFld
                VAR tmp := dwFld:ToString()
                VAR len := tmp:Length
                IF cName:Length + len <= 10
                    cNewname := cName + tmp
                ELSE
                    cNewname := cName:Substring(0, 10 - tmp:Length)+tmp
                ENDIF
            ENDDO
            cName 	:= cNewname
        ENDIF
        aFldNames:Add(cName)
        RETURN cName

    PUBLIC STATIC METHOD ReturnsRows(cCommand AS STRING) AS LOGIC
        LOCAL aParts := cCommand:Split(" ()":ToCharArray()) AS STRING[]
        IF aParts:Length > 0
            local cWord := aParts[0]:ToLower() as string
            SWITCH cWord
            CASE "select"
            CASE "execute"
                RETURN TRUE
                // dml
            CASE "insert"
            CASE "delete"
            CASE "update"
                // ddl
            CASE "create"
            CASE "drop"
            CASE "alter"
                RETURN FALSE
            OTHERWISE
                RETURN false  //?
            END SWITCH
        ENDIF
        RETURN FALSE
END CLASS
END NAMESPACE
