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
/// <summary>SDF RDD. For reading and writing text files.</summary>
    CLASS SDF INHERIT TEXTRDD  

        VIRTUAL PROPERTY Driver AS STRING GET "SDF"


        PROTECTED METHOD _WriteRecord() AS LOGIC STRICT
            LOCAL oSb as StringBuilder
            LOCAL nIndex as LONG
            oSb := StringBuilder{}
            nIndex := 0
            FOREACH var oField in SELF:_Fields
                var oValue := SELF:_fieldData[nIndex]
                LOCAL sValue as STRING
                sValue := SELF:_GetFieldValue(oField, oValue)
                SWITCH oField:FieldType
                CASE DbFieldType.Number         // 'N'  
                CASE DbFieldType.Float          // 'F'
                CASE DbFieldType.Double         // 'B'
                CASE DbFieldType.Currency		// 'Y'
                CASE DbFieldType.Integer        // 'I'
                    sValue := sValue:PadLeft(oField:Length,' ')
                OTHERWISE
                    sValue := sValue:PadRight(oField:Length,' ')
                END SWITCH
                oSb:Append(sValue)
                nIndex += 1
            NEXT
            oSb:Append(SELF:_RecordSeparator)
            SELF:_WriteString(oSb:ToString())
            RETURN TRUE

       PROTECTED METHOD _GetLastRec AS LONG
            LOCAL dwPos AS DWORD
            LOCAL dwLen AS LONG
            LOCAL nCount AS LONG
            dwPos := FTell(SELF:_hFile)
            dwLen := FSeek3(SELF:_hFile, 0, FS_END)
            nCount := dwLen / SELF:_RecordLength
            FSeek3(SELF:_hFile, (LONG) dwPos, FS_SET)
            RETURN nCount


    END CLASS
END NAMESPACE
