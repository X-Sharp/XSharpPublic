//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System.Collections.Generic
using System.Text
USING XSharp.RDD.Support
USING XSharp.RDD.Enums
BEGIN NAMESPACE XSharp.RDD
    /// <summary>TSV RDD. For reading and writing tab delimited files.</summary>
    CLASS TSV INHERIT CSV
        CONSTRUCTOR
            SUPER()
            SELF:_Separator := Chr(9)
    END CLASS

    /// <summary>CSV RDD. For reading and writing semicolon delimited files.</summary>
    CLASS CSV INHERIT DELIM
        CONSTRUCTOR
            SUPER()
            SELF:_Separator := ";"

    METHOD Create(info AS DbOpenInfo) AS LOGIC
        local lOk as LOGIC
        LOCAL oSb as StringBuilder
        lOk := SUPER:Create(info)
        IF lOk
            // Write field headers to CSV file.
            oSb := StringBuilder{}
            FOREACH var fld in SELF:_Fields
                if String.IsNullOrEmpty(fld:Alias)
                    oSb:Append(fld:Name)
                else
                    oSb:Append(fld:Alias)
                endif
                oSb:Append(SELF:_Separator)
            NEXT
            var result := oSb:ToString()
            result := result:Substring(0, result:Length-1) +SELF:_RecordSeparator
            SELF:_WriteString(result)
        ENDIF
        RETURN lOk
    END CLASS


/// <summary>DELIM RDD. For reading and writing delimited files.</summary>
    CLASS DELIM INHERIT TEXTRDD
        CONSTRUCTOR
            SUPER()

        VIRTUAL PROPERTY Driver AS STRING GET "DELIM" 


        PROTECTED METHOD _WriteRecord() AS LOGIC STRICT
            LOCAL oSb as StringBuilder
            LOCAL nIndex as LONG
            LOCAL hasDelimiter as LOGIC
            oSb := StringBuilder{}
            nIndex := 0
            hasDelimiter := ! String.IsNullOrEmpty(SELF:_Delimiter)
            FOREACH var oField in SELF:_Fields
                var oValue := SELF:_fieldData[nIndex]
                var sValue := SELF:_GetFieldValue(oField, oValue)
                if nIndex > 0
                    oSb:Append(SELF:_Separator)
                endif
                SWITCH oField:FieldType
                CASE DbFieldType.Character   // C
                CASE DbFieldType.VarChar     // 'V'
                CASE DbFieldType.VarBinary   // 'Q'
                    if hasDelimiter
                        oSb:Append(_Delimiter)
                    endif
                    oSb:Append(sValue:Replace(_Separator,""))
                    if hasDelimiter
                        oSb:Append(_Delimiter)
                    endif
                CASE DbFieldType.Number         // 'N'
                CASE DbFieldType.Float          // 'F'
                CASE DbFieldType.Double         // 'B'
                CASE DbFieldType.Currency		// 'Y'
                CASE DbFieldType.Integer        // 'I'
                    oSb:Append(sValue:Trim())
                Otherwise
                    oSb:Append(sValue:Replace(_Separator,""))
                END SWITCH
                nIndex += 1
            NEXT
            oSb:Append(SELF:_RecordSeparator)
            SELF:_WriteString(oSb:ToString())
            RETURN TRUE

        PROTECTED METHOD _GetLastRec AS LONG
            LOCAL dwPos     AS DWORD
            LOCAL nCount := 0 AS LONG
            dwPos := FTell(SELF:_hFile)
            FSeek3(SELF:_hFile, 0, FS_SET)
            DO WHILE ! FEof(SELF:_hFile)
                nCount++
                FReadLine(SELF:_hFile, 4096)
            ENDDO
            FSeek3(SELF:_hFile, (LONG) dwPos, FS_SET)
            RETURN nCount

    END CLASS
END NAMESPACE

