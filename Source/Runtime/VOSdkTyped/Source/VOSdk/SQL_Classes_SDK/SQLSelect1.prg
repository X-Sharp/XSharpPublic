//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


USING System.Data.Common
USING System.Data
USING System.Collections.Generic
USING System.Reflection
USING System.Diagnostics


/// <include file="Sql.xml" path="doc/SQLSelect/*" />
[XSharp.Internal.TypesChanged];
[DebuggerDisplay( "SqlSelect {TableName,nq}" )] ;
PARTIAL CLASS SQLSelect INHERIT DataServer
    PROTECT oStmt           AS SQLStatement   // Statement object
    PROTECT oConn           AS SQLConnection
    PROTECT cCursor         AS STRING		// No longer used
    PROTECT lCsrOpenFlag 	AS LOGIC		// Is the server Open
    PROTECT lFetchFlag      AS LOGIC        // First Fetch has been performed
    PROTECT lBof            AS LOGIC        // Are we at BOF
    PROTECT lEof            AS LOGIC        // Are we at EOF


    PROTECT nNumCols		AS LONG			// # Of Columns in resultset
    PROTECT lAppendFlag     AS LOGIC        // Are we on a new record
    PROTECT aIndexCol       AS List<INT>
    PROTECT nRowCount		AS LONG			// Number of rows
    PROTECT cTableName      AS STRING
    PROTECT nNotifyCount    AS INT
    PROTECT lSuppressNotify AS LOGIC
    PROTECT aSQLColumns       AS SQLColumn[]    // Array of SqlColumn objects
    PROTECT aColumnAttributes AS SQLColumnAttributes[]   // Array of SQLColumnAttributes Objects.
    PROTECT lNullAsBlank	  AS LOGIC		// Return Null values as Blank
    PROTECT lTimeStampAsDate  AS LOGIC		// Should datetimes be handled as data ?
    PROTECT aLastArgs         AS ARRAY


    protect lReadOnly        as logic       // is the result set readonly
    protect lBatchUpdates    as logic       // does the result set support batch updates


    // Dummy Properties
    PROTECT nLastRecNum     AS LONG
    PROTECT nRecNum         AS LONG
    PROTECT lLastRecFound   AS LOGIC
    PROTECT lDeleteFlag     AS LOGIC


    // New .Net Properties
    PROTECT oNetConn		AS IDbConnection // Connection
    PROTECT oTable			AS DataTable	// Contents of resultset
    PROTECT oSchema			AS DataTable	// Schema definition of resultset
    PROTECT oAdapter        AS DbDataAdapter  // Data Adapter for Insert/Update/Delete
    PROTECT oCurrentRow		AS DataRow		// Current data row in result set. NULL when at EOF


    PROTECT oFieldHash      AS System.Collections.Hashtable // Hashtable of Strings and Symbols and column positions
    PROTECT nCurrentRow		AS LONG			// Current row number, 0 based
    PROTECT nSuspendNot     AS LONG			// Is used to keep track of the current notification state
    PROTECT lErrorFlag      AS LOGIC		// Was there an error with the last operation
    PROTECT lChanges        AS LOGIC




    #region Internal Methods


    method __CheckReadOnly as void
        if self:lReadOnly
            var error := Error{"Write not allowed"}
            error:Gencode := EG_READONLY
            error:FuncSym := ProcName(2)
            throw error
        endif

 /// <exclude />
    METHOD __getDataTable() AS DataTable
        RETURN SELF:oTable


 /// <exclude />
    METHOD __AllocStmt() AS LOGIC STRICT
        IF SELF:oNetConn:State != ConnectionState.Open
            SELF:oNetConn:Open()
        ENDIF
        RETURN oStmt:__AllocStmt()


 /// <exclude />
    METHOD __CheckEOF AS VOID
        IF nRowCount == 0
            SELF:lBof := TRUE
            SELF:lEof := TRUE
        ELSE
            SELF:lBof := FALSE
            SELF:lEof := FALSE
            IF nCurrentRow < 0
                SELF:lBof := TRUE
                nCurrentRow := 0
            ELSEIF nCurrentRow >= nRowCount
                SELF:lEof := TRUE
                nCurrentRow := nRowCount
            ENDIF
            IF nCurrentRow < nRowCount
                oCurrentRow := oTable:Rows[nCurrentRow]
            ELSE
                oCurrentRow := NULL
            ENDIF
        ENDIF
        RETURN


 /// <exclude />
    [Obsolete];
    METHOD __CopySQLError( symMethod AS SYMBOL, oErrInfo AS SQLErrorInfo) AS VOID STRICT
        RETURN


 /// <exclude />
    METHOD __ForceOpen AS LOGIC STRICT
        TRY
            IF ! SELF:lCsrOpenFlag
                SELF:Execute(SELF:aLastArgs)
            ENDIF
        CATCH AS Exception
            lCsrOpenFlag := FALSE
        END TRY
        RETURN lCsrOpenFlag


 /// <exclude />
    METHOD __FreeStmt( fOption AS WORD ) AS LOGIC STRICT
        RETURN oStmt:__FreeStmt( fOption )


 /// <exclude />
    METHOD __GetColIndex( uFieldID AS USUAL, lAutoFetch AS LOGIC) AS DWORD STRICT
        LOCAL nPos AS DWORD
        LOCAL lOk  AS LOGIC
        nPos := 0
        IF lAutoFetch
            lOk := SELF:__ForceOpen()
        ELSE
            lOk := TRUE
        ENDIF
        IF lOk
            IF IsNumeric( uFieldID )
                IF uFieldID > 0 .AND. uFieldID <= nNumCols
                    nPos := uFieldID
                ENDIF
            ELSEIF IsSymbol( uFieldID )
                LOCAL cField AS STRING
                cField := uFieldID
                IF (oFieldHash:ContainsKey(cField))
                    nPos := (DWORD) oFieldHash[cField]
                ENDIF
            ELSEIF IsString( uFieldID )
                LOCAL cField AS STRING
                LOCAL nDot AS DWORD
                cField := Upper(uFieldID)
                nDot := At(".", cField)
                IF nDot > 0
                    cField := SubStr2(cField, nDot+1)
                ENDIF
                IF (oFieldHash:ContainsKey(cField))
                    nPos := (DWORD) oFieldHash[cField]
                ENDIF


            ENDIF
        ENDIF
        RETURN nPos


 /// <exclude />
    METHOD __PrepareForRecordMovement AS LOGIC STRICT
        LOCAL lOk AS LOGIC
        lOk := SELF:__ForceOpen()
        IF lOk .AND. SELF:lEof .AND. SELF:lBof
            lOk := FALSE
        ENDIF
        IF lOk .AND. ! SELF:__Notify( NOTIFYINTENTTOMOVE )
            lOk := FALSE
        ENDIF
        IF lOk .AND. ! SELF:__GoCold(FALSE)
            lOk := FALSE
        ENDIF
        RETURN lOk




    #endregion


 /// <exclude />
    METHOD __GetFieldName( uFieldPosition AS USUAL) AS STRING STRICT
        LOCAL nIndex AS DWORD
        LOCAL cRet   AS STRING


        nIndex := SELF:__GetColIndex( uFieldPosition, FALSE )


        IF nIndex = 0 .OR. nIndex > nNumCols
            oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADFLD ), #FieldName )
        ELSE
            oStmt:ErrInfo:ErrorFlag := FALSE
            cRet := ((SQLColumn)aSQLColumns[nIndex]):ColName
        ENDIF


        RETURN cRet


    METHOD __CreateDataAdapter AS VOID

        self:oAdapter := self:oConn:Factory:CreateDataAdapter()
        SELF:oAdapter:MissingSchemaAction := MissingSchemaAction.AddWithKey
        self:oAdapter:SelectCommand := self:oStmt:StatementHandle
        var builder := self:oConn:Factory:CreateCommandBuilder()
        builder:DataAdapter := self:oAdapter
        self:oAdapter:InsertCommand := builder:GetInsertCommand()
        self:oAdapter:UpdateCommand := builder:GetUpdateCommand()
        self:oAdapter:DeleteCommand := builder:GetDeleteCommand()



    METHOD ShowSQLError(strMessage as STRING, symMethod as SYMBOL, e := null as Exception) AS VOID
        oStmt:__GenerateSQLError( strMessage, symMethod, e)
        SELF:Error(SELF:ErrInfo, symMethod)
        RETURN

 /// <exclude />
    METHOD __GoCold(lUpdateBatch AS LOGIC) AS LOGIC STRICT
        local lOk := FALSE as logic
        local lWriteBatch as logic
        lWriteBatch := lUpdateBatch .or. ! self:lBatchUpdates
        if self:lChanges .and. lWriteBatch
            local oChanges as DataTable

            oChanges := self:oTable:GetChanges()
            if oChanges != null .and. oChanges:Rows:Count > 0
                if self:oAdapter == null_object
                    SELF:__CreateDataAdapter()
                endif
                TRY
                    self:oAdapter:Update(self:oTable)
                    SELF:oTable:AcceptChanges()
                    SELF:lChanges := FALSE
                    lOk := TRUE
                CATCH e as Exception
                    oStmt:__GenerateSQLError( e:Message, #__GoCold )
                    lOk := FALSE
                END TRY
            ENDIF

        ELSE
            lOk := TRUE
        ENDIF
        RETURN lOk


 /// <exclude />
    METHOD __InitColumnDesc() AS LOGIC STRICT
        LOCAL oDF           AS DataField		// Datafield
        LOCAL oHL           AS HyperLabel		// Hyperlabel
        LOCAL aNames        AS ARRAY
        LOCAL cFldName      AS STRING
        LOCAL oCol          AS SQLColumn
        LOCAL i             AS DWORD
        LOCAL oColumns		AS DataColumnCollection
        LOCAL oFS           AS FieldSpec		// Fieldspec
        LOCAL oRow          AS DataRow
        LOCAL ihelp         AS SHORT
        oColumns            := oTable:Columns
        // Do not re-read the table definition with the # of columns has not changed
        IF TRUE // SELF:nNumCols != oColumns:Count
            SELF:nNumCols       := oColumns:Count
            SELF:wFieldCount    := (DWORD) nNumCols
            SELF:aDataFields    := ArrayNew(nNumCols )
            SELF:aSQLColumns    := SQLColumn[]{nNumCols }
            SELF:aColumnAttributes := SQLColumnAttributes[]{nNumCols }
            SELF:oFieldHash	    := System.Collections.Hashtable{}
            // aNames is used by FieldNameCheck to guarantee unicity of field names
            aNames := {}
            FOREACH oColumn AS DataColumn IN oColumns
                i			:= (DWORD) (oColumn:Ordinal +1)
                cFldName	:= oColumn:ColumnName
                // Remove unwanted characters
                cFldName    := SqlFunctions.CleanupColumnName(cFldName)
                cFldName	:= SqlFunctions.FieldNameCheck(cFldName, aNames)
                oFS         := DotNetType2VOType(oSchema, oColumn,cFldName)
                oHL    		:= oFS:HyperLabel
                oDF    		:= DataField{ oHL, oFS }
                oFieldHash:Add(Upper(oDF:Name), i)
                TRY
                    IF oSchema != NULL
                        //LOCAL oT AS System.Type
                        //LOCAL iScale AS SHORT
                        oRow := oSchema:Rows[oColumn:Ordinal]
                        //oT := (System.Type) oRow["DataType"]
                        IF Convert.IsDBNull(oRow["NumericScale"])
                            ihelp := 0
                        ELSE
                            ihelp := Convert.ToInt16(oRow["NumericScale"])
                        ENDIF
                        oCol := SQLColumn{oHL, oFS, (System.Type) oRow["DataType"], ihelp,  oColumn:AllowDBNull, oColumn:Ordinal, cFldName, cFldName}
                        //oCol := SqlColumn{oHL, oFS, oT, iScale,  oColumn:AllowDBNull, oColumn:Ordinal, oColumn:ColumnName, oColumn:ColumnName}
                    ELSE
                        oCol := SQLColumn{oHL, oFS, oColumn:DataType, 0,  oColumn:AllowDBNull, oColumn:Ordinal, oColumn:ColumnName, cFldName}
                    ENDIF
                    SELF:aSQLColumns[i] := oCol
                    SELF:aDataFields[i] := oDF
                CATCH e AS Exception
                    oStmt:__GenerateSQLError( "Error reading structure: "+e:Message, #__InitColumnDesc )
                END TRY
            NEXT
        ENDIF
        RETURN TRUE


 /// <exclude />
    METHOD __Notify(kNot AS LONG ,uDesc := NIL AS USUAL) AS LOGIC
        LOCAL lOk 	AS USUAL
        LOCAL oError AS Error
        lOk := SELF:Notify(kNot,uDesc)
        IF kNot == NOTIFYINTENTTOMOVE .AND. ! lOk
            oError := SqlFunctions.CreateError(0, "IntentToMove returned FALSE")
            oError:FuncSym	:= #Notify
            SELF:Error(oError, #Notify)
        ENDIF
        RETURN lOk


 /// <exclude />
    METHOD __Open(Table AS DataTable, Schema AS DataTable) AS LOGIC
        LOCAL cName AS STRING
        LOCAL lRet AS LOGIC
        lRet := false
        oTable      := Table
        oSchema     := Schema
        nRowCount	:= oTable:Rows:Count
        nCurrentRow := 0
        SELF:__CheckEOF()
        IF oSchema != NULL_OBJECT .AND. oSchema:Rows:Count > 0
            LOCAL oRow AS DataRow
            oRow := oSchema:Rows[0]
            cName := oRow:Item["BaseTableName"]:ToString()
        ELSE
            cName := oTable:TableName
        ENDIF


        IF String.IsNullOrEmpty(cName)
            LOCAL nPos AS DWORD
            LOCAL cStmt AS STRING
            cStmt := oStmt:SQLString
            nPos := AtC(" FROM ", cStmt)
            IF nPos > 0
                cName := AllTrim(SubStr2(cStmt, nPos+6))
            ELSE
                cName := oStmt:SQLString
            ENDIF
        ENDIF
        // Limit name for Hyperlabel to Table Name(s), so get rid of Select and Where/Order By clause
        oHyperLabel  	:= HyperLabel{ String2Symbol(cName),cName }
        // Build the list of DataField objects, the Field array and the
        // DbStruct array


        SELF:lCsrOpenFlag := TRUE
        SELF:lBof         := FALSE
        SELF:lEof         := FALSE
        SELF:lFetchFlag   := FALSE


        lRet := SELF:__InitColumnDesc()
        SELF:__CheckEOF()
        IF SELF:nNumCols = 0
            lRet := FALSE
        ENDIF
        RETURN lRet




    STATIC METHOD FindTableName( cStmt AS STRING) AS STRING
        LOCAL cTable:="" AS STRING
        LOCAL nPos   AS DWORD
        IF cStmt != NULL_STRING
            nPos := AtC( "select ", cStmt )
            IF nPos > 0
                cTable := LTrim( SubStr2( cStmt, nPos + 7 ) )
            ENDIF


            nPos := AtC( " from ", cStmt )
            cTable := LTrim( SubStr2( cStmt, nPos + 6 ) )
            nPos := At2( " ", cTable )
            IF nPos >= 2
                cTable := SubStr3( cTable, 1, nPos - 1 )
            ENDIF
        ENDIF
        RETURN cTable




 /// <exclude />
    METHOD __FindTableName() AS VOID STRICT
        LOCAL cClass AS STRING
        LOCAL cStmt	 AS STRING
        // RvdH 050413 Optimized by using local strings


        cStmt := SELF:oStmt:SQLString
        IF cStmt != NULL_STRING
            cTableName := FindTableName(cStmt)
            // tell the DataServer the name...
            cClass := Symbol2String( ClassName( SELF ) )
            oHyperLabel := HyperLabel{ cTableName, cTableName,                                   ;
            cClass + ": "+cTableName, ;
            cClass + "_"+cTableName }
        ENDIF
        RETURN




 /// <exclude />
    METHOD __RecCount() AS LONGINT STRICT
        RETURN SELF:nRowCount




 /// <exclude />
    METHOD __SetRecordFlags( lBofNew AS USUAL, lEofNew AS USUAL) AS VOID STRICT
        IF IsLogic( lBofNew )
            SELF:lBof := lBofNew
        ENDIF


        IF IsLogic( lEofNew )


            SELF:lEof := lEofNew
        ENDIF


        RETURN


    #region Dummy Methods


/// <exclude />
    [Obsolete];
    METHOD __BuildUpdateStmt AS STRING STRICT
        RETURN ""


 /// <exclude />
    [Obsolete];
    METHOD  __CopyDataBuffer( aSQLSource AS ARRAY, aSQLTarget AS ARRAY) AS ARRAY STRICT
        RETURN aSQLTarget


 /// <exclude />
    [Obsolete];
    METHOD __FigureScrollUpdateType() AS DWORD STRICT
        RETURN 0


 /// <exclude />
    [Obsolete];
    METHOD __GetCursorName () AS LOGIC STRICT
        RETURN TRUE


/// <exclude />
    [Obsolete];
    METHOD __GetLongData( nODBCType AS SHORTINT, nIndex AS DWORD ) AS LOGIC STRICT
        RETURN TRUE


 /// <exclude />
    [Obsolete];
    METHOD __GetUpdateKey( lAppend AS LOGIC) AS STRING STRICT
        RETURN ""


 /// <exclude />
    [Obsolete];
    METHOD __GetUpdateStmt( nIndex AS DWORD, lAppend AS LOGIC) AS SQLStatement STRICT
        RETURN NULL


 /// <exclude />
    [Obsolete];
    METHOD __GetUpdateVal( lAppend AS LOGIC) AS STRING STRICT
        RETURN ""


 /// <exclude />
    [Obsolete];
    METHOD __InitColValue( nIndex AS DWORD ) AS LOGIC STRICT
        RETURN TRUE


 /// <exclude />
    [Obsolete];
    METHOD __MemFree() AS VOID STRICT
        RETURN


 /// <exclude />
    [Obsolete];
    METHOD  __PrepareStmtOptions( oStatement AS SQLStatement ) AS VOID STRICT
        RETURN


 /// <exclude />
    [Obsolete];
    METHOD __PutLongData( cValue AS STRING, nODBCType AS SHORTINT, nIndex AS DWORD, lAppend AS LOGIC ) AS STRING STRICT
        RETURN cValue


 /// <exclude />
    [Obsolete];
    METHOD __Reset() AS LOGIC STRICT
        RETURN TRUE


 /// <exclude />
    [Obsolete];
    METHOD __SetCursorName( cCursorName AS STRING) AS LOGIC STRICT
        RETURN TRUE


 /// <exclude />
    [Obsolete];
    METHOD __SetNullData( nODBCType AS SHORTINT, nIndex AS DWORD, aData AS ARRAY) AS VOID STRICT
        RETURN


 /// <exclude />
    [Obsolete];
    METHOD __SetScrollOptions( nConcurrency AS DWORD, nKeySet AS DWORD, lAsync AS LOGIC) AS LOGIC STRICT
        RETURN TRUE


 /// <exclude />
    [Obsolete];
    METHOD __SkipCursor( nRecCount AS LONGINT) AS LOGIC STRICT
        RETURN TRUE


 /// <exclude />
    [Obsolete];
    METHOD __UpdateLongData( lAppend AS LOGIC ) AS VOID STRICT
        RETURN
    #endregion




END CLASS




