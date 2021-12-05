//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System.Collections.Generic
using System.Text
USING XSharp.RDD.Support
USING XSharp.RDD.Enums
USING System.Diagnostics

BEGIN NAMESPACE XSharp.RDD
    /// <summary>TSV RDD. For reading and writing tab delimited files.</summary>
    [DebuggerDisplay("TSV ({Alias,nq})")];
    CLASS TSV INHERIT CSV
        /// <inheritdoc/>
        OVERRIDE PROPERTY Driver AS STRING GET nameof(TSV)
        CONSTRUCTOR
            SUPER()
            SELF:FieldSeparator := Chr(9)
            SELF:_lHasHeader := TRUE
    END CLASS

    /// <summary>CSV RDD. For reading and writing semicolon delimited files.</summary>
    [DebuggerDisplay("CSV ({Alias,nq})")];
    CLASS CSV INHERIT DELIM
        /// <inheritdoc/>
        OVERRIDE PROPERTY Driver AS STRING GET nameof(CSV)

        CONSTRUCTOR
            SUPER()
            SELF:FieldSeparator := ";"
            SELF:_lHasHeader := TRUE
    END CLASS


/// <summary>DELIM RDD. For reading and writing delimited files.</summary>
    [DebuggerDisplay("DELIM ({Alias,nq})")];
    CLASS DELIM INHERIT TEXTRDD
        PROTECT _oSb as StringBuilder
        PROTECT _lHasHeader as LOGIC
        CONSTRUCTOR
            SUPER()
            _oSb := StringBuilder{}
            SELF:FieldSeparator  := RuntimeState.FieldDelimiter
            SELF:StringDelimiter := RuntimeState.StringDelimiter
            SELF:_lHasHeader := FALSE
        /// <inheritdoc/>
        OVERRIDE PROPERTY Driver AS STRING GET nameof(DELIM)

        PROTECTED OVERRIDE METHOD _readRecord() AS LOGIC STRICT
            IF _BufferValid .OR. SELF:EoF
                return true
            endif
            var cLine    := FReadLine(SELF:_hFile, 4096)
            var inString := FALSE
            VAR cFieldDelim     := SELF:FieldSeparator[0]
            VAR cStringDelim    := SELF:StringDelimiter[0]
            var nIndex   := 0
            var newField := FALSE
            local oValue as OBJECT
            _oSb:Clear()
            // now parse the line
            foreach var c in cLine
                IF c == cStringDelim
                    inString := ! inString
                ELSEIF c == cFieldDelim .AND. ! inString
                    newField := TRUE
                else
                    _oSb:Append(c)
                endif
                if newField
                    oValue := SELF:_getFieldValue(_Fields[nIndex], _oSb:ToString())
                    SELF:_fieldData [nIndex] := oValue
                    newField := FALSE
                    _oSb:Clear()
                    nIndex += 1
                endif
            next
            oValue := SELF:_getFieldValue(_Fields[nIndex], _oSb:ToString())
            SELF:_fieldData [nIndex] := oValue
            nIndex += 1
            if nIndex < SELF:_Fields:Length
                SELF:EoF := TRUE
                RETURN FALSE
            ENDIF
            _BufferValid := TRUE
            RETURN TRUE

        PROTECTED OVERRIDE METHOD _writeRecord() AS LOGIC STRICT
            LOCAL nIndex as LONG
            LOCAL hasDelimiter as LOGIC
            _oSb:Clear()
            nIndex := 0
            VAR stringDelimiter := SELF:StringDelimiter
            hasDelimiter := ! String.IsNullOrEmpty(stringDelimiter)
            FOREACH var oField in SELF:_Fields
                var oValue := SELF:_fieldData[nIndex]
                VAR sValue := SELF:_getFieldString(oField, oValue)
                IF nIndex > 0
                    _oSb:Append(SELF:FieldSeparator)
                ENDIF
                SWITCH oField:FieldType
                CASE DbFieldType.Character   // C
                CASE DbFieldType.VarChar     // 'V'
                CASE DbFieldType.VarBinary   // 'Q'
                    IF hasDelimiter
                        _oSb:Append(stringDelimiter)
                    endif
                    if stringDelimiter:Length > 0
                        _oSb:Append(sValue:Replace(stringDelimiter,""))
                    else
                        _oSb:Append(sValue)
                    endif
                     IF hasDelimiter
                        _oSb:Append(stringDelimiter)
                    ENDIF
                CASE DbFieldType.Number         // 'N'
                CASE DbFieldType.Float          // 'F'
                CASE DbFieldType.Double         // 'B'
                CASE DbFieldType.Currency		// 'Y'
                CASE DbFieldType.Integer        // 'I'
                    _oSb:Append(sValue:Trim())
                OTHERWISE
                    _oSb:Append(sValue:Replace(stringDelimiter,""))
                END SWITCH
                nIndex += 1
            NEXT
            _oSb:Append(SELF:_RecordSeparator)
            SELF:_WriteString(_oSb:ToString())
            RETURN TRUE

        PROTECTED OVERRIDE METHOD _getLastRec AS LONG
            LOCAL dwPos     AS DWORD
            LOCAL nCount := 0 AS LONG
            LOCAL buffer    AS BYTE[]
            dwPos := FTell(SELF:_hFile)
            FSeek3(SELF:_hFile, 0, FS_SET)
            buffer := BYTE[]{4096}
            VAR afterCR := FALSE
            DO WHILE ! FEof(SELF:_hFile)
                VAR numRead := FRead3(SELF:_hFile, buffer, 4096)
                FOR VAR i := 0 TO numRead-1
                    SWITCH buffer[i]
                    CASE 13 // CR
                       nCount++
                       afterCR := TRUE
                    CASE 10 // LF
                        IF ! afterCR
                            nCount++
                        ENDIF
                        afterCR := FALSE
                    OTHERWISE
                        afterCR := FALSE
                    END SWITCH
                NEXT
            ENDDO
            FSeek3(SELF:_hFile, (LONG) dwPos, FS_SET)
            // Suppress last line when it has a length of 0
            IF SELF:_lHasHeader
                nCount --
            ENDIF
            RETURN nCount

    /// <inheritdoc/>
    OVERRIDE METHOD Create(info AS DbOpenInfo) AS LOGIC
        local lOk as LOGIC
        lOk := SUPER:Create(info)
        IF lOk .and. _lHasHeader
            // Write field headers to CSV file.
            _oSb:Clear()
            FOREACH var fld in SELF:_Fields
                if String.IsNullOrEmpty(fld:Alias)
                    _oSb:Append(fld:Name:ToLower())
                else
                    _oSb:Append(fld:Alias:ToLower())
                endif
                _oSb:Append(SELF:_Separator)
            NEXT
            var result := _oSb:ToString()
            result := result:Substring(0, result:Length-1) +SELF:_RecordSeparator
            SELF:_WriteString(result)
        ENDIF
        RETURN lOk

    /// <inheritdoc/>
    OVERRIDE METHOD GoTop() AS LOGIC
        local lOk as LOGIC
        lOk := SUPER:GoTop()
        IF lOk .and. SELF:_lHasHeader
            FReadLine(SELF:_hFile, 4096)
        ENDIF
        RETURN lOk
    END CLASS
END NAMESPACE

