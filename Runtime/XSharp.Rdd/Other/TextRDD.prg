//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING XSharp.RDD.Support
USING System.Text
BEGIN NAMESPACE XSharp.RDD
/// <summary>DELIM RDD. For reading and writing delimited files.</summary>
ABSTRACT CLASS TEXTRDD INHERIT Workarea
    PROTECT INTERNAL _Encoding      AS Encoding
	PROTECT _Hot            AS LOGIC
    PROTECT _Ansi           AS LOGIC
    PROTECT _BufferValid    AS LOGIC
    PROTECT _Reccount       AS LONG
    PROTECT _Recno          AS LONG
    PROTECT PROPERTY IsOpen AS LOGIC GET SELF:_hFile != F_ERROR
	INTERNAL _OpenInfo		AS DbOpenInfo // current dbOpenInfo structure in OPEN/CREATE method

	CONSTRUCTOR
		SUPER()                     
		SELF:_hFile         := IntPtr.Zero
		SELF:_TransRec 		:= TRUE
		SELF:_RecordLength 	:= 0
		SELF:_Delimiter		:= e"\""
		SELF:_Separator		:= ","    

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
	THROW NotImplementedException{}



METHOD Append(lReleaseLock AS LOGIC) AS LOGIC
	THROW NotImplementedException{}

 

METHOD Close() 			AS LOGIC  
	LOCAL isOk := FALSE AS LOGIC
	IF SELF:IsOpen
		isOk := SELF:GoCold()
    //
		IF isOk 
			SELF:UnLock(0)
        //
			IF !SELF:_ReadOnly
				SELF:Flush()
			ENDIF
        //
			TRY
				isOk := FClose( SELF:_hFile )
				
				isOk := SUPER:Close() .AND. isOk
			CATCH ex AS Exception
				isOk := FALSE
				SELF:_txtError(ex, SubCodes.ERDD_CLOSE_FILE,GenCode.EG_CLOSE,  "TEXTRDD.Close") 
				
			END TRY
			SELF:_hFile := F_ERROR
		ENDIF
	ENDIF
RETURN isOk
/// <inheritdoc />
METHOD Create(info AS DbOpenInfo) AS LOGIC  
	THROW NotImplementedException{}
/// <inheritdoc />
METHOD Open(info AS DbOpenInfo) AS LOGIC
	LOCAL isOK AS LOGIC
    //
	isOk := FALSE
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
	LOCAL codePage AS LONG
	IF SELF:IsOpen
        IF XSharp.RuntimeState.Ansi
			SELF:_Ansi := TRUE
			codePage := XSharp.RuntimeState.WinCodePage
		ELSE
			SELF:_Ansi := FALSE
			codePage := XSharp.RuntimeState.DosCodePage
		ENDIF        
		SELF:_Encoding := System.Text.Encoding.GetEncoding( codePage ) 
        isOk := TRUE
        SELF:_Reccount := SELF:_GetLastRec()
	ELSE
        // Error or just FALSE ?
		isOK := FALSE
		LOCAL ex := FException() AS Exception
		SELF:_txtError( ex, ERDD.OPEN_FILE, XSharp.Gencode.EG_OPEN )
	ENDIF
    RETURN isOk
	
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
/// <inheritdoc />
METHOD GetValue(nFldPos AS INT) AS OBJECT
	THROW NotImplementedException{}
// METHOD GetValueFile(nFldPos AS INT, fileName AS STRING) AS LOGIC

//	METHOD GetValueLength(nFldPos AS INT) AS INT
/// <inheritdoc />
METHOD Flush() 			AS LOGIC
	THROW NotImplementedException{}
/// <inheritdoc />
METHOD GoCold()			AS LOGIC
	THROW NotImplementedException{}
/// <inheritdoc />

METHOD PutValue(nFldPos AS INT, oValue AS OBJECT) AS LOGIC
	THROW NotImplementedException{}

METHOD Info(nOrdinal AS INT, oNewValue AS OBJECT) AS OBJECT
	THROW NotImplementedException{}

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
	RuntimeState.LastRDDError := oError
    //
	THROW oError

    ABSTRACT METHOD _GetLastRec as LONG



END CLASS
END NAMESPACE

