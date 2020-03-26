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
    PROTECT _RecordSeparator as STRING
    
    STATIC PRIVATE  culture := CultureInfo.InvariantCulture AS CultureInfo
    PROTECT PROPERTY IsOpen AS LOGIC GET SELF:_hFile != F_ERROR
	INTERNAL _OpenInfo		AS DbOpenInfo // current dbOpenInfo structure in OPEN/CREATE method

    ABSTRACT PROTECTED METHOD _GetLastRec()  as LONG
    ABSTRACT PROTECTED METHOD _WriteRecord() AS LOGIC


	CONSTRUCTOR
		SUPER()                     
		SELF:_hFile         := IntPtr.Zero
		SELF:_TransRec 		:= TRUE
		SELF:_RecordLength 	:= 0
		SELF:_Delimiter		:= e"\""
		SELF:_Separator		:= ","
        SELF:_fieldData     := OBJECT[]{0}
    	SELF:_numformat := (NumberFormatInfo) culture:NumberFormat:Clone()
	    SELF:_numformat:NumberDecimalSeparator := "."
        SELF:_Buffer        := Byte[]{512}
        SELF:_Writing       := FALSE
        SELF:_Separator     := ","
        SELF:_RecordSeparator := CRLF

METHOD GoBottom() AS LOGIC
    IF SELF:IsOpen
		BEGIN LOCK SELF
		END LOCK
        RETURN TRUE
	ENDIF
    RETURN FALSE


METHOD GoTop() AS LOGIC
    IF SELF:IsOpen
		BEGIN LOCK SELF
			FSeek3(SELF:_hFile, 0, FS_SET)
			SELF:_Top := TRUE
			SELF:_Bottom := FALSE
			SELF:_BufferValid := FALSE
		END LOCK
        RETURN TRUE
	ENDIF
    RETURN FALSE
    
//	METHOD GoBottom() AS LOGIC   
/// <inheritdoc />
METHOD GoTo(nRec AS LONG) AS LOGIC
	THROW NotImplementedException{}
/// <inheritdoc />
METHOD GoToId(oRec AS OBJECT) AS LOGIC
	THROW NotImplementedException{}

METHOD Skip(nToSkip AS INT) AS LOGIC
	THROW NotImplementedException{}

METHOD SkipRaw(nToSkip AS INT) AS LOGIC 
	THROW NotImplementedException{}


METHOD AddField(info AS RddFieldInfo) AS LOGIC
	RETURN SUPER:AddField(info)

METHOD Append(lReleaseLock AS LOGIC) AS LOGIC
	LOCAL isOK as LOGIC
    isOK := SELF:GoCold()
    IF isOK
        SELF:_fieldData := OBJECT[]{SELF:_Fields:Length}
    ENDIF
    RETURN isOK

 
PROTECTED METHOD _GetFieldValue(oField as RddFieldInfo, oValue as OBJECT) AS STRING
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
            local oDt as DateTime
            if oValue IS IDate var oDate
                oDt := DateTime{oDate:Year, oDate:Month, oDate:Day}
            elseif oValue is DateTime var oDt2
                oDt := oDt2
            else
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
    return sValue

    Protected Method _WriteString(strValue as STRING) AS LOGIC
        if strValue:Length > SELF:_Buffer:Length
            SELF:_Buffer := Byte[]{strValue:Length }
        ENDIF
        SELF:_Encoding:GetBytes(strValue, 0, strValue:Length, _Buffer, 0)
        RETURN FWrite3(SELF:_hFile, _Buffer, (DWORD) strValue:Length) == (DWORD) strValue:Length
       


METHOD Close() 			AS LOGIC  
	LOCAL isOk := FALSE AS LOGIC
	IF SELF:IsOpen
		isOk := SELF:GoCold()
        IF SELF:_Writing
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
METHOD Create(info AS DbOpenInfo) AS LOGIC  
	LOCAL isOK AS LOGIC
	isOK := FALSE
	IF SELF:_Fields:Length == 0 
		RETURN FALSE
	ENDIF
	SELF:_OpenInfo := info
	SELF:_OpenInfo:FileName := System.IO.Path.ChangeExtension( SELF:_OpenInfo:FileName, SELF:_OpenInfo:Extension )
    //
	SELF:_Hot := FALSE
	SELF:_FileName := SELF:_OpenInfo:FileName
	SELF:_Alias := SELF:_OpenInfo:Alias
	SELF:_Shared := SELF:_OpenInfo:Shared
	SELF:_ReadOnly := SELF:_OpenInfo:ReadOnly
    //
	SELF:_hFile    := FCreate2( SELF:_FileName, FO_EXCLUSIVE)
   	IF SELF:IsOpen
        isOK := SELF:_DetermineCodePage()
    ELSE
		isOK := FALSE
		LOCAL ex := FException() AS Exception
		SELF:_txtError( ex, ERDD.CREATE_FILE, XSharp.Gencode.EG_CREATE )
    ENDIF
    SELF:_Writing  := TRUE
RETURN isOK

/// <inheritdoc />
METHOD Open(info AS DbOpenInfo) AS LOGIC
	LOCAL isOK AS LOGIC
    //
	isOK := FALSE
	SELF:_OpenInfo := info
    // We do not assume an extension, should we do that ?

	SELF:_Hot := FALSE
	SELF:_FileName := SELF:_OpenInfo:FileName
	IF File(SELF:_FileName)
		SELF:_FileName := FPathName()
		SELF:_OpenInfo:FileName := SELF:_FileName
	ENDIF
	SELF:_Alias := SELF:_OpenInfo:Alias
	SELF:_Shared := SELF:_OpenInfo:Shared
	SELF:_ReadOnly := SELF:_OpenInfo:ReadOnly
	SELF:_hFile    := FOpen(SELF:_FileName, SELF:_OpenInfo:FileMode)
	IF SELF:IsOpen
        isOK := SELF:_DetermineCodePage()
        SELF:_Reccount := SELF:_GetLastRec()
	ELSE
        // Error or just FALSE ?
		isOK := FALSE
		LOCAL ex := FException() AS Exception
		SELF:_txtError( ex, ERDD.OPEN_FILE, XSharp.Gencode.EG_OPEN )
	ENDIF
    RETURN isOK
	
    //
	// Filtering and Scoping 
//	METHOD ClearFilter() 	AS LOGIC
//	METHOD ClearScope() 	AS LOGIC 
//	METHOD Continue()		AS LOGIC     
//	METHOD GetScope()		AS DbScopeInfo 
//	METHOD SetFilter(info AS DbFilterInfo) AS LOGIC 
//	METHOD SetScope(info AS DbScopeInfo) AS LOGIC
	// Fields
//	METHOD CreateFields(aFields AS DbField[]) AS LOGIC
//	METHOD FieldIndex(fieldName AS STRING) AS LONG 
//  METHOD FieldInfo(nFldPos AS LONG, nOrdinal AS LONG, oNewValue AS OBJECT) AS OBJECT
//	METHOD FieldName(nFldPos AS LONG) AS STRING 

// METHOD GetValueFile(nFldPos AS INT, fileName AS STRING) AS LOGIC

//	METHOD GetValueLength(nFldPos AS INT) AS INT
/// <inheritdoc />
METHOD Flush() 			AS LOGIC
    RETURN FFlush(SELF:_hFile)

/// <inheritdoc />


METHOD GoCold()			AS LOGIC
	IF SELF:_Hot
        SELF:_WriteRecord()
		SELF:_Hot := FALSE
    ENDIF
    RETURN TRUE

/// <inheritdoc />
METHOD GetValue(nFldPos AS INT) AS OBJECT
    // Subclass fills the _fieldData list
	if nFldPos <= _fieldData:Length
        return _fieldData[nFldPos -1]
    endif
    RETURN NULL

/// <inheritdoc />
METHOD PutValue(nFldPos AS INT, oValue AS OBJECT) AS LOGIC
    // Subclass persists the _fieldData list
    IF nFldPos <= _fieldData:Length
        SELF:_fieldData[nFldPos-1] := oValue
        SELF:_Hot := TRUE
    ENDIF
    RETURN TRUE

METHOD Info(nOrdinal AS INT, oNewValue AS OBJECT) AS OBJECT
	SWITCH nOrdinal
    CASE DBI_ISDBF
        return FALSE
    CASE DBI_GETHEADERSIZE
        return 0
    case DBI_FILEHANDLE
        return SELF:_hFile
    CASE DBI_FULLPATH
        return SELF:_FileName
    case DBI_SETDELIMITER
        var result := SELF:_Delimiter
        IF oNewValue IS STRING
            SELF:_Delimiter := (STRING) oNewValue
        ENDIF
        RETURN result
    case DBI_SEPARATOR
        var result := SELF:_Separator
        IF oNewValue IS STRING
            SELF:_Separator := (STRING) oNewValue
        ENDIF
        RETURN result
        
    case DBI_GETDELIMITER
        return SELF:_Delimiter
    CASE DBI_CANPUTREC
        RETURN FALSE
    END SWITCH
    return SUPER:Info(nOrdinal, oNewValue)

	// Properties

	PROPERTY Deleted 	AS LOGIC GET 	FALSE
	PROPERTY RecCount	AS LONG GET _Reccount
	PROPERTY RecId		AS OBJECT GET  _Recno
	PROPERTY RecNo		AS LONG 	GET _Recno
VIRTUAL PROPERTY Driver AS STRING GET "TEXTRDD" 


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

