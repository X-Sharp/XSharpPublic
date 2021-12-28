//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING XSharp.RDD.Support
USING System.Text
USING System.Collections.Generic
USING System.Globalization
USING XSharp.RDD.Enums

BEGIN NAMESPACE XSharp.RDD
    /// <summary>Abstract TextRDD. For reading and writing delimited files and SDF files.</summary>
    ABSTRACT CLASS TEXTRDD INHERIT Workarea
        PROTECT INTERNAL _Encoding      AS Encoding
        PROTECT _Hot            AS LOGIC
        PROTECT _Ansi           AS LOGIC
        PROTECT _BufferValid    AS LOGIC
        PROTECT _Reccount       AS LONG
        PROTECT _Recno          AS LONG
        PROTECT _fieldData      AS OBJECT[]
        PROTECT _numformat      AS NumberFormatInfo
        PROTECT _Buffer         AS BYTE[]
        PROTECT _Writing        as LOGIC
        PROTECT _RecordSeparator AS STRING
        INTERNAL PROPERTY FieldSeparator    AS STRING GET _Separator SET _Separator := VALUE
        INTERNAL PROPERTY StringDelimiter   AS STRING GET _Delimiter SET _Delimiter := VALUE
        INTERNAL PROPERTY RecordSeparator   AS STRING GET _RecordSeparator SET _RecordSeparator := VALUE

        STATIC PRIVATE  culture := CultureInfo.InvariantCulture AS CultureInfo
        PROTECT PROPERTY IsOpen AS LOGIC GET SELF:_hFile != F_ERROR
        INTERNAL _OpenInfo		AS DbOpenInfo // current dbOpenInfo structure in OPEN/CREATE method

        #region abstract methods, must be implemented in subclass
        ABSTRACT PROTECTED METHOD _getLastRec()  AS LONG
        ABSTRACT PROTECTED METHOD _writeRecord() AS LOGIC
        ABSTRACT PROTECTED METHOD _readRecord() AS LOGIC
        #endregion

        #region worker methods
        PROTECTED METHOD _prepareFields AS VOID
            SELF:_fieldData     := OBJECT[]{SELF:_Fields:Length}
            local nSize := 0 AS LONG
            foreach var fld in SELF:_Fields
                nSize += fld:Length
            next
            SELF:_RecordLength := nSize


        PROTECTED METHOD _getFieldValue(oField AS RddFieldInfo, sValue AS STRING) AS OBJECT
            SWITCH oField:FieldType
                CASE DbFieldType.Character   // C
                CASE DbFieldType.VarChar     // 'V'
                CASE DbFieldType.VarBinary   // 'Q'
                    return sValue
                CASE DbFieldType.Date
                    if sValue:Length >= 8
                        var year  := Convert.ToInt32(sValue:Substring(0,4))
                        var month := Convert.ToInt32(sValue:Substring(4,2))
                        var day   := Convert.ToInt32(sValue:Substring(6,2))
                        return DbDate{year,month,day}
                    endif
                    SELF:_txtError(Subcodes.ERDD_DATATYPE, EG_DATATYPE)
                CASE DbFieldType.DateTime      // 'T'
                    local oDt as DateTime
                    IF DateTime.TryParse(sValue, OUT oDt)
                        return oDt
                    ENDIF
                    SELF:_txtError(Subcodes.ERDD_DATATYPE, EG_DATATYPE)
                CASE DbFieldType.Logic
                    return sValue[0] == 'T' .or. sValue[0] == 'Y'

                CASE DbFieldType.Number         // 'N'
                CASE DbFieldType.Float          // 'F'
                CASE DbFieldType.Double         // 'B'
                CASE DbFieldType.Currency		// 'Y'
                CASE DbFieldType.Integer        // 'I'
                    var result := _Val(sValue)
                    if result is REAL8 VAR r8
                        return DbFloat{r8, oField:Length, oField:Decimals}
                    elseif result != null
                        return result
                    endif
                    SELF:_txtError(Subcodes.ERDD_DATATYPE, EG_DATATYPE)
                Otherwise
                    // Skip column contents
                    NOP
            END SWITCH
            return NULL


        PROTECTED METHOD _getFieldString(oField AS RddFieldInfo, oValue AS OBJECT) AS STRING
            LOCAL sValue := "" as STRING
            SWITCH oField:FieldType
                CASE DbFieldType.Character   // C
                CASE DbFieldType.VarChar     // 'V'
                CASE DbFieldType.VarBinary   // 'Q'
                    sValue := oValue:ToString():TrimEnd()
                CASE DbFieldType.Date
                    if oValue IS IDate var oDate
                        VAR oDt := DateTime{oDate:Year, oDate:Month, oDate:Day}
                        sValue := DtToS(oDt)

                    elseif oValue is DateTime var oDt
                        sValue := DtToS(oDt)
                    else
                        SELF:_txtError(Subcodes.ERDD_DATATYPE, EG_DATATYPE)
                    endif
                CASE DbFieldType.DateTime      // 'T'
                    local oDt  as DateTime
                    if oValue IS IDate var oDate
                        if oDate:IsEmpty
                            oDt := DateTime.MinValue
                        ELSE
                            oDt := DateTime{oDate:Year, oDate:Month, oDate:Day}
                        ENDIF
                    elseif oValue is DateTime var oDt2
                        oDt := oDt2
                    else
                        oDt := DateTime.MinValue
                        SELF:_txtError(Subcodes.ERDD_DATATYPE, EG_DATATYPE)
                    endif
                    sValue := DtToS(oDt)
                CASE DbFieldType.Logic

                    IF oValue is LOGIC VAR logValue
                        sValue := iif(logValue,"T","F")
                    ELSE
                        SELF:_txtError(Subcodes.ERDD_DATATYPE, EG_DATATYPE)
                    ENDIF
                CASE DbFieldType.Number         // 'N'
                CASE DbFieldType.Float          // 'F'
                CASE DbFieldType.Double         // 'B'
                CASE DbFieldType.Currency		// 'Y'
                CASE DbFieldType.Integer        // 'I'
                    _numformat:NumberDecimalDigits :=   oField:Decimals
                    IF oValue is IFloat VAR flValue
                        sValue := flValue:Value:ToString("F", _numformat)
                    ELSEIF oValue is LONG VAR longValue
                        sValue := longValue:ToString("F", _numformat)
                    ELSEIF oValue is REAL8 VAR r8Value
                        sValue :=  r8Value:ToString("F", _numformat)
                    ELSE
                        SELF:_txtError(Subcodes.ERDD_DATATYPE, EG_DATATYPE)
                    ENDIF
                    IF sValue:Length > oField:Length
                        sValue := STRING{'*', oField:Length}
                    ENDIF
                Otherwise
                    // Skip column contents
                    NOP
            END SWITCH
            RETURN sValue

        Protected Method _WriteString(strValue as STRING) AS LOGIC
            if strValue:Length > SELF:_Buffer:Length
                SELF:_Buffer := Byte[]{strValue:Length }
            ENDIF
            SELF:_Encoding:GetBytes(strValue, 0, strValue:Length, _Buffer, 0)
            RETURN FWrite3(SELF:_hFile, _Buffer, (DWORD) strValue:Length) == (DWORD) strValue:Length

        #endregion
        /// <inheritdoc />
        CONSTRUCTOR
            SUPER()
            SELF:_hFile          := IntPtr.Zero
            SELF:_TransRec 		 := TRUE
            SELF:_RecordLength 	 := 0
            SELF:StringDelimiter := e"\""
            SELF:FieldSeparator  := ","
            SELF:RecordSeparator := RuntimeState.Eol
            SELF:_fieldData      := OBJECT[]{0}
            SELF:_numformat := (NumberFormatInfo) culture:NumberFormat:Clone()
            SELF:_numformat:NumberDecimalSeparator := "."
            SELF:_Buffer        := Byte[]{512}
            SELF:_Writing       := FALSE
            SELF:_Separator     := ","


            /// <inheritdoc />
        OVERRIDE METHOD GoBottom() AS LOGIC
            RETURN FALSE


            /// <inheritdoc />
        OVERRIDE METHOD GoTop() AS LOGIC
            IF SELF:IsOpen
                BEGIN LOCK SELF
                    FSeek3(SELF:_hFile, 0, FS_SET)
                    SELF:_Top := TRUE
                    SELF:_Bottom := FALSE
                    SELF:_BufferValid := FALSE
                    SELF:_Recno := 1
                END LOCK
                RETURN TRUE
            ENDIF
            RETURN FALSE


            /// <inheritdoc />
        OVERRIDE METHOD Skip(nToSkip AS INT) AS LOGIC
            IF SELF:_Recno <= SELF:_Reccount
                SELF:_Recno += 1
                SELF:_BufferValid := FALSE
                RETURN SELF:_readRecord()
            ELSE
                SELF:_Recno += 1
                SELF:EoF := TRUE
                RETURN FALSE
            ENDIF


            /// <inheritdoc />
        OVERRIDE METHOD Append(lReleaseLock AS LOGIC) AS LOGIC
            LOCAL isOK as LOGIC
            isOK := SELF:GoCold()
            IF isOK
                SELF:_fieldData := OBJECT[]{SELF:_Fields:Length}
            ENDIF
            RETURN isOK



        OVERRIDE METHOD Close() 			AS LOGIC
            LOCAL isOk := FALSE AS LOGIC
            IF SELF:IsOpen
                isOk := SELF:GoCold()
                IF SELF:_Writing .and.  RuntimeState.Eof
                    FWrite(SELF:_hFile,_chr(26),1)
                ENDIF
                IF isOk
                    IF !SELF:_ReadOnly
                        SELF:Flush()
                    ENDIF
                    //
                    TRY
                        isOk := FClose( SELF:_hFile )

                        isOk := SUPER:Close() .AND. isOk
                    CATCH ex AS Exception
                        isOk := FALSE
                        SELF:_txtError(ex, Subcodes.ERDD_CLOSE_FILE,Gencode.EG_CLOSE,  "TEXTRDD.Close")

                    END TRY
                    SELF:_hFile := F_ERROR
                ENDIF
            ENDIF
            RETURN isOk


        PRIVATE METHOD _DetermineCodePage() AS LOGIC
            LOCAL codePage AS LONG
            IF XSharp.RuntimeState.Ansi
                SELF:_Ansi := TRUE
                codePage := XSharp.RuntimeState.WinCodePage
            ELSE
                SELF:_Ansi := FALSE
                codePage := XSharp.RuntimeState.DosCodePage
            ENDIF
            SELF:_Encoding := System.Text.Encoding.GetEncoding( codePage )
            return TRUE

        /// <inheritdoc />
        OVERRIDE METHOD Create(info AS DbOpenInfo) AS LOGIC
            LOCAL isOK AS LOGIC
            isOK := FALSE
            IF SELF:_Fields:Length == 0
                RETURN FALSE
            ENDIF
            SELF:_OpenInfo := info
            //
            SELF:_Hot := FALSE
            SELF:_FileName := SELF:_OpenInfo:FullName
            SELF:_Alias := SELF:_OpenInfo:Alias
            SELF:_Shared := SELF:_OpenInfo:Shared
            SELF:_ReadOnly := SELF:_OpenInfo:ReadOnly

            //
            SELF:_hFile    := FCreate2( SELF:_FileName, FO_EXCLUSIVE)
            IF SELF:IsOpen
                SELF:_FileName := FGetFileName(SELF:_hFile)
                isOK := SELF:_DetermineCodePage()
                SELF:_prepareFields()
            ELSE
                isOK := FALSE
                LOCAL ex := FException() AS Exception
                SELF:_txtError( ex, ERDD.CREATE_FILE, XSharp.Gencode.EG_CREATE )
            ENDIF
            SELF:_Writing  := TRUE
            RETURN isOK

            /// <inheritdoc />
        OVERRIDE METHOD Open(info AS DbOpenInfo) AS LOGIC
            LOCAL isOK AS LOGIC
            //
            isOK := FALSE
            SELF:_OpenInfo := info
            SELF:_Hot := FALSE
            SELF:_FileName := SELF:_OpenInfo:FullName
            SELF:_Alias := SELF:_OpenInfo:Alias
            SELF:_Shared := SELF:_OpenInfo:Shared
            SELF:_ReadOnly := SELF:_OpenInfo:ReadOnly
            SELF:_hFile    := FOpen(SELF:_FileName, SELF:_OpenInfo:FileMode)
            IF SELF:IsOpen
                SELF:_FileName := FGetFileName(SELF:_hFile)
                isOK := SELF:_DetermineCodePage()
                IF FSize(SELF:_hFile) < Int32.MaxValue
                    FConvertToMemoryStream(SELF:_hFile)
                ENDIF
                SELF:_prepareFields()
                SELF:_Reccount := SELF:_getLastRec()
                SELF:GoTop()
            ELSE
                // Error or just FALSE ?
                isOK := FALSE
                LOCAL ex := FException() AS Exception
                SELF:_txtError( ex, ERDD.OPEN_FILE, XSharp.Gencode.EG_OPEN )
            ENDIF
            RETURN isOK

            /// <inheritdoc />
        OVERRIDE METHOD Flush() 			AS LOGIC
            RETURN FFlush(SELF:_hFile)

            /// <inheritdoc />
        OVERRIDE METHOD GoCold()			AS LOGIC
            IF SELF:_Hot
                SELF:_writeRecord()
                SELF:_Hot := FALSE
            ENDIF
            RETURN TRUE

            /// <inheritdoc />
        OVERRIDE METHOD GetValue(nFldPos AS INT) AS OBJECT
            // Subclass fills the _fieldData list
            IF SELF:_readRecord()
                if nFldPos <= _fieldData:Length
                    return _fieldData[nFldPos -1]
                endif
            endif
            RETURN NULL

            /// <inheritdoc />
        OVERRIDE METHOD PutValue(nFldPos AS INT, oValue AS OBJECT) AS LOGIC
            // Subclass persists the _fieldData list
            IF nFldPos <= _fieldData:Length
                SELF:_fieldData[nFldPos-1] := oValue
                SELF:_Hot := TRUE
            ENDIF
            RETURN TRUE

            /// <inheritdoc />
        OVERRIDE METHOD Info(nOrdinal AS INT, oNewValue AS OBJECT) AS OBJECT
            SWITCH nOrdinal
                CASE DBI_ISDBF
                    return FALSE
                CASE DBI_GETHEADERSIZE
                    return 0
                case DBI_FILEHANDLE
                    return SELF:_hFile
                CASE DBI_FULLPATH
                    return SELF:_FileName
                CASE DBI_SETDELIMITER                   // Field delimiter
                    VAR result := SELF:FieldSeparator
                    IF oNewValue IS STRING
                        SELF:FieldSeparator := (STRING) oNewValue
                    ENDIF
                    RETURN result
                case DBI_SEPARATOR
                    VAR result := SELF:RecordSeparator
                    IF oNewValue IS STRING
                        SELF:RecordSeparator := (STRING) oNewValue
                    ENDIF
                    RETURN result

                case DBI_GETDELIMITER
                    return SELF:_Delimiter
                CASE DBI_CANPUTREC
                    RETURN FALSE
            END SWITCH
            return SUPER:Info(nOrdinal, oNewValue)

            // Properties

            /// <inheritdoc />
        OVERRIDE PROPERTY Deleted 	AS LOGIC GET 	FALSE
        /// <inheritdoc />
        OVERRIDE PROPERTY RecCount	AS LONG GET _Reccount
        /// <inheritdoc />
        OVERRIDE PROPERTY RecId		AS OBJECT GET  _Recno
        /// <inheritdoc />
        OVERRIDE PROPERTY RecNo		AS LONG 	GET _Recno
        /// <inheritdoc />
        OVERRIDE PROPERTY Driver AS STRING GET "TEXTRDD"


        INTERNAL METHOD _txtError(ex AS Exception, iSubCode AS DWORD, iGenCode AS DWORD) AS VOID
            SELF:_txtError(ex, iSubCode, iGenCode, String.Empty, ex?:Message, XSharp.Severity.ES_ERROR)

        INTERNAL METHOD _txtError(iSubCode AS DWORD, iGenCode AS DWORD) AS VOID
            SELF:_txtError(NULL, iSubCode, iGenCode, String.Empty, String.Empty, XSharp.Severity.ES_ERROR)

        INTERNAL METHOD _txtError(ex AS Exception,iSubCode AS DWORD, iGenCode AS DWORD, iSeverity AS DWORD) AS VOID
            SELF:_txtError(ex, iSubCode, iGenCode, String.Empty, String.Empty, iSeverity)

        INTERNAL METHOD _txtError(iSubCode AS DWORD, iGenCode AS DWORD, iSeverity AS DWORD) AS VOID
            SELF:_txtError(NULL, iSubCode, iGenCode, String.Empty, String.Empty, iSeverity)

        INTERNAL METHOD _txtError(iSubCode AS DWORD, iGenCode AS DWORD, strFunction AS STRING) AS VOID
            SELF:_txtError(NULL, iSubCode, iGenCode, strFunction, String.Empty, XSharp.Severity.ES_ERROR)

        INTERNAL METHOD _txtError(ex AS Exception, iSubCode AS DWORD, iGenCode AS DWORD, strFunction AS STRING) AS VOID
            SELF:_txtError(ex, iSubCode, iGenCode, strFunction, String.Empty, XSharp.Severity.ES_ERROR)

        INTERNAL METHOD _txtError(iSubCode AS DWORD, iGenCode AS DWORD, strFunction AS STRING, strMessage AS STRING) AS VOID
            SELF:_txtError(NULL, iSubCode, iGenCode, strFunction,strMessage, XSharp.Severity.ES_ERROR)

        INTERNAL METHOD _txtError(ex AS Exception, iGenCode AS DWORD, iSubCode AS DWORD, strFunction AS STRING, strMessage AS STRING, iSeverity AS DWORD) AS VOID
            LOCAL oError AS RddError
            //
            IF ex != NULL
                oError := RddError{ex,iGenCode, iSubCode}
            ELSE
                oError := RddError{iGenCode, iSubCode}
            ENDIF
            oError:SubSystem := SELF:Driver
            oError:Severity := iSeverity
            oError:FuncSym  := IIF(strFunction == NULL, "", strFunction) // code in the SDK expects all string properties to be non-NULL
            oError:FileName := SELF:_FileName
            IF String.IsNullOrEmpty(strMessage)  .AND. ex != NULL
                strMessage := ex:Message
            ENDIF
            oError:Description := IIF(strMessage == NULL , "", strMessage)
            RuntimeState.LastRddError := oError
            //
            THROW oError




    END CLASS
END NAMESPACE

