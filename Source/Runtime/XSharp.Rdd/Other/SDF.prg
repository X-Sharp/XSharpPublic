//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.Collections.Generic
USING System.Text
USING XSharp.RDD.Support
USING XSharp.RDD.Enums
USING System.Diagnostics

BEGIN NAMESPACE XSharp.RDD
    /// <summary>SDF RDD. For reading and writing text files.</summary>

    [DebuggerDisplay("SDF ({Alias,nq})")];
    CLASS SDF INHERIT TEXTRDD
        PROTECT buffer AS BYTE[]
        /// <inheritdoc />
        OVERRIDE PROPERTY Driver AS STRING GET nameof(SDF)


        PROTECTED OVERRIDE METHOD _readRecord() AS LOGIC STRICT
            IF _BufferValid
                RETURN TRUE
            ENDIF
            IF SELF:EoF
                RETURN FALSE
            ENDIF
            IF FRead3(SELF:_hFile, SELF:buffer, (DWORD) SELF:buffer:Length) == (DWORD) SELF:buffer:Length
                VAR cLine := SELF:_Encoding:GetString(SELF:buffer, 0, SELF:_RecordLength)
                VAR nOffSet  := 0
                VAR nIndex   := 0
                FOREACH VAR fld IN SELF:_Fields
                    VAR sValue := cLine:Substring(nOffSet, fld:Length)
                    VAR oValue := SELF:_getFieldValue(_Fields[nIndex], sValue )
                    SELF:_fieldData [nIndex] := oValue
                    nIndex += 1
                    nOffSet += fld:Length
                NEXT
                _BufferValid := TRUE
                RETURN TRUE
            ENDIF
            SELF:EoF := TRUE
            RETURN FALSE



        PROTECTED OVERRIDE METHOD _writeRecord() AS LOGIC STRICT
            LOCAL oSb AS StringBuilder
            LOCAL nIndex AS LONG
            oSb := StringBuilder{}
            nIndex := 0
            FOREACH VAR oField IN SELF:_Fields
                VAR oValue := SELF:_fieldData[nIndex]
                VAR sValue := SELF:_getFieldString(oField, oValue)
                SWITCH oField:FieldType
                    CASE DbFieldType.Number         // 'N'
                    CASE DbFieldType.Float          // 'F'
                        IF sValue:Length != oField:Length
                            sValue := sValue:PadLeft(oField:Length,' ')
                        ENDIF
                    CASE DbFieldType.Double         // 'B'
                    CASE DbFieldType.Currency		// 'Y'
                    CASE DbFieldType.Integer        // 'I'
                        IF sValue:Length != oField:Length
                            sValue := sValue:PadLeft(oField:Length,' ')
                        ENDIF
                    OTHERWISE
                        IF sValue:Length != oField:Length
                            sValue := sValue:PadRight(oField:Length,' ')
                        ENDIF
                END SWITCH
                oSb:Append(sValue)
                nIndex += 1
            NEXT
            oSb:Append(SELF:_RecordSeparator)
            SELF:_WriteString(oSb:ToString())
            RETURN TRUE

        PROTECTED OVERRIDE METHOD _getLastRec AS LONG
            LOCAL dwPos AS DWORD
            LOCAL dwLen AS LONG
            LOCAL nCount AS LONG
            dwPos := FTell(SELF:_hFile)
            dwLen := FSeek3(SELF:_hFile, 0, FS_END)
            nCount := dwLen / (SELF:_RecordLength+SELF:_RecordSeparator:Length)
            FSeek3(SELF:_hFile, (LONG) dwPos, FS_SET)
            RETURN nCount

        /// <inheritdoc />
        OVERRIDE METHOD GoTop() AS LOGIC
            IF SUPER:GoTop()
                SELF:buffer := BYTE[]{SELF:_RecordLength+SELF:_RecordSeparator:Length}
                RETURN TRUE
            ENDIF
            RETURN FALSE


    END CLASS
END NAMESPACE
