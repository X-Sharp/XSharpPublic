//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


USING System.Data.Common
using System.Data

PARTIAL CLASS SqlSelect INHERIT DataServer
	PROPERTY AppendData AS ARRAY GET {}
		
	PROPERTY AppendFlag AS LOGIC GET SELF:lAppendFlag
	
	ACCESS BoF 
		IF SELF:lAppendFlag
			RETURN FALSE
		ENDIF
		RETURN SELF:lBof
	
	ACCESS Connection 
		RETURN SELF:oConn
	
	ACCESS CursorName AS STRING
		RETURN ""
	
	ASSIGN CursorName( cCursorName AS STRING)
		RETURN
	
	
	ACCESS DBStruct 
		LOCAL aStruct       AS ARRAY
		LOCAL nIndex        AS DWORD
		LOCAL oCol			  AS SQLColumn
		LOCAL oFs			  AS FieldSpec
		//RvdH 050413 Centralize opening of cursor
		IF ! SELF:__ForceOpen()
			RETURN {}
		ENDIF
		
		// create an array for the dbstructs
		aStruct := ArrayNew( SELF:nNumCols )
		
		// fill it...
		FOR nIndex := 1 TO SELF:nNumCols
			oCol := SELF:aSQLColumns[nIndex]
			oFs := oCol:FieldSpec
			aStruct[nIndex] :=    ;
			{  oCol:ColName,   ;
			oFs:ValType,    ;
			oFs:Length,     ;
			oFs:Decimals    ;
			}
		NEXT
		
		RETURN aStruct
	
	ACCESS Deleted AS LOGIC
		RETURN FALSE
	
	ACCESS EoF 
		SELF:__ForceOpen()
		IF SELF:lAppendFlag
			RETURN FALSE
		ENDIF
		RETURN SELF:lEof
	
	ACCESS ErrInfo
		RETURN SELF:oStmt:ErrInfo
	
	ACCESS FCount 
		LOCAL nRet  AS DWORD
		IF ! SELF:__ForceOpen()
			nRet := 0
		ELSE
			nRet := (DWORD) SELF:nNumCols 
		ENDIF
		RETURN nRet
	
	ACCESS FOUND () AS LOGIC
		RETURN !SELF:EoF
	
	ACCESS HyperLabel 
		SELF:__ForceOpen()
		RETURN SUPER:HyperLabel
	
	ACCESS IndexColumns 
		RETURN aIndexCol
	
	ACCESS LastRec AS LONG
		IF ! SELF:__ForceOpen()
			RETURN 0
		ENDIF
		IF SELF:oTable != NULL
			RETURN SELF:oTable:Rows:Count
		ENDIF
		RETURN 0
	
	PROPERTY lRowModified AS LOGIC GET oCurrentRow != NULL .and. oCurrentRow:RowState != DataRowState.Unchanged

	ACCESS Modified AS LOGIC
		RETURN lRowModified
	
	ACCESS MoreResults AS LOGIC
		RETURN !SELF:lEof
	
	ACCESS NativeSQL AS STRING
		RETURN SELF:oStmt:NativeSQL
	
	PROPERTY NullAsBlank AS LOGIC ;
		GET SELF:lNullAsBlank    ;
		SET SELF:lNullAsBlank := Value
	
	ACCESS NumCols AS LONG
		RETURN nNumCols
	
	ACCESS NumParameters AS LONG
		RETURN SELF:oStmt:NumParameters
	
	ACCESS NumResultColumns AS DWORD
		RETURN SELF:FCount
	
	ACCESS NumSuccessfulRows AS LONG
		RETURN SELF:nRowCount
	
	ACCESS PrepFlag AS LOGIC
		RETURN SELF:oStmt:PrepFlag
	
	ACCESS RecCount 
		RETURN SELF:LASTREC
	
	ACCESS RecNo
		IF ! SELF:__ForceOpen()
			RETURN 0
		ENDIF
		RETURN SELF:nCurrentRow+1
	
	ACCESS SQLColumns AS SQLColumn[]
		RETURN SELF:aSQLColumns
	
	PROPERTY SqlData AS ARRAY GET {}
	
	ACCESS SQLString AS STRING
		RETURN SELF:oStmt:SQLString
	
	ASSIGN SQLString( uVal  AS STRING)
		IF SELF:oStmt:PrepFlag
			IF !SELF:Close()
				RETURN 
			ENDIF
		ENDIF
		
		SELF:oStmt:SQLString := uVal
		
		//  Have a statement?
		IF !String.IsNullOrEmpty(oStmt:SQLString)
			SELF:__FindTableName()
		ENDIF
		RETURN 
	
	ACCESS Statement AS SqlStatement
		RETURN SELF:oStmt
	
	ACCESS StatementHandle
		RETURN SELF:oStmt:StatementHandle
		
	ACCESS Status AS USUAL
		RETURN SELF:oStmt:Status
	
	ACCESS TableName AS STRING
		RETURN SELF:cTableName
	
	ACCESS TimeStampAsDate AS LOGIC
		RETURN SELF:lTimeStampAsDate
	
	ASSIGN TimeStampAsDate	( lNew AS LOGIC)
		SELF:lTimeStampAsDate := lNew
	
	ACCESS Used AS LOGIC
		RETURN SELF:lCsrOpenFlag
	
	
END CLASS
