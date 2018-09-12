//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


USING System
USING System.IO
USING System.Collections.Generic
USING System.Text
USING XSharp.RDD
USING XSharp.RDD.Enums
USING XSharp.RDD.Support

/// <summary>
/// The AdsRDD class. 
/// </summary>
CLASS XSharp.ADS.AdsRDD INHERIT Workarea
    #region Fields
    //PRIVATE m_CallbackFn AS CallbackFn
    //PRIVATE m_CalltraceFile AS System.IO.StreamWriter
    //PRIVATE m_iProgress AS Long
    PRIVATE m_palRlocks AS DWORD[]
    PRIVATE m_dbfName AS STRING
    PRIVATE _Recno  AS DWORD
    INTERNAL _Encoding AS System.Text.Encoding
    INTERNAL _Connection AS System.IntPtr
    INTERNAL _Index AS System.IntPtr
    INTERNAL _Table AS System.IntPtr
    INTERNAL _Collation AS STRING
    INTERNAL _Driver AS STRING
    INTERNAL _CheckRights AS WORD
    INTERNAL _LockType AS WORD
    INTERNAL _TableType AS WORD
    INTERNAL _MaxKeySize AS WORD
    INTERNAL _Ansi  AS LOGIC
    INTERNAL _HasMemo AS LOGIC
    INTERNAL _addFieldPos    AS LONG     // Used by AddFields Method, and SetFieldsExtent
    INTERNAL _fieldCount AS LONG
    
    #endregion
    
    /// <summary>Create instande of RDD </summary>
    CONSTRUCTOR()
        SUPER()
        SELF:_Order         := AdsIndex{SELF}
        SELF:_Memo          := AdsMemo{SELF}
        SELF:_Table       := System.IntPtr.Zero
        SELF:_Index       := System.IntPtr.Zero
        SELF:_Connection  := System.IntPtr.Zero
        SELF:_Driver    := String.Empty
        SELF:_Collation := String.Empty
        SELF:_Driver    := "ADSRDD"
        SELF:m_dbfName      := String.Empty
        
        #region Helper Methods that check for error conditions
        
        INTERNAL METHOD ACECALL(ulRetCode AS DWORD) AS LOGIC
            IF ulRetCode != 0
                SELF:ADSERROR( ulRetCode, 27)
                RETURN FALSE
            ENDIF
            RETURN TRUE
            
        INTERNAL METHOD ADSERROR(iSubCode AS DWORD, iGenCode AS DWORD) AS VOID
            SELF:ADSERROR(iSubCode, iGenCode, String.Empty, String.Empty, XSharp.Severity.ES_ERROR)
            
        INTERNAL METHOD ADSERROR(iSubCode AS DWORD, iGenCode AS DWORD, iSeverity AS DWORD) AS VOID
            SELF:ADSERROR(iSubCode, iGenCode, String.Empty, String.Empty, iSeverity)
            
        INTERNAL METHOD ADSERROR(iSubCode AS DWORD, iGenCode AS DWORD, strFunction AS STRING) AS VOID
            SELF:ADSERROR(iSubCode, iGenCode, strFunction, String.Empty, XSharp.Severity.ES_ERROR)
            
        INTERNAL METHOD ADSERROR(iSubCode AS DWORD, iGenCode AS DWORD, strFunction AS STRING, strMessage AS STRING) AS VOID
            SELF:ADSERROR(iSubCode, iGenCode, strFunction,strMessage, XSharp.Severity.ES_ERROR)
            
        INTERNAL METHOD ADSERROR(iSubCode AS DWORD, iGenCode AS DWORD, strFunction AS STRING, strMessage AS STRING, iSeverity AS DWORD) AS VOID
            LOCAL lastError AS DWORD
            LOCAL pucBuf AS CHAR[]
            LOCAL wBufLen AS WORD
            LOCAL oError AS RddError
            oError := RddError{}
            oError:SubCode := iSubCode
            oError:Gencode := iGenCode
            oError:SubSystem := SELF:_Driver
            oError:Severity := iSeverity
            oError:FuncSym  := strFunction
            oError:FileName := SELF:m_dbfName
            IF strMessage == String.Empty
                //
                pucBuf := CHAR[]{ACE.ADS_MAX_ERROR_LEN}
                wBufLen := (WORD) pucBuf:Length
                IF ((ACE.AdsGetLastError(OUT lastError, pucBuf, REF wBufLen) == 0) .AND. (lastError != 0)) .AND. (wBufLen > 0)
                    oError:Description := STRING{pucBuf, 0, wBufLen}
                ENDIF
            ELSE
                oError:Description := strMessage
            ENDIF
            THROW oError
            
            
        INTERNAL METHOD Unsupported(strFunctionName AS STRING) AS LOGIC
            SELF:ADSERROR(ERDD.UNSUPPORTED, XSharp.Gencode.EG_UnSupported, strFunctionName)
            RETURN FALSE
            
            
            #endregion
        #region Helper Methods
        PRIVATE METHOD ACEORDER() AS System.IntPtr
            IF SELF:_Index != System.IntPtr.Zero
                RETURN SELF:_Index
            ENDIF
            RETURN SELF:_Table
            
        PRIVATE METHOD AxCheckVODeletedFlag() AS DWORD
            RETURN ACE.AdsShowDeleted(IIF(RuntimeState.Deleted,(WORD)0 ,(WORD)1 ))
            
        PRIVATE METHOD AxCheckVODateFormat() AS LOGIC
            ACE.AdsSetDateFormat(RuntimeState.DateFormat)
            ACE.AdsSetExact(IIF(RuntimeState.Exact,(WORD)1 ,(WORD)0 ))
            ACE.AdsSetDecimals((WORD)RuntimeState.Decimals )
            ACE.AdsSetEpoch((WORD)RuntimeState.Epoch )
            RETURN TRUE
            
        METHOD AxCheckRDDInfo() AS VOID
            LOCAL oRet AS OBJECT
            IF AX_AXSLocking()
                SELF:_LockType := ACE.ADS_PROPRIETARY_LOCKING
            ELSE
                SELF:_LockType := ACE.ADS_COMPATIBLE_LOCKING
            ENDIF
            IF AX_RightsCheck()
                SELF:_CheckRights := ACE.ADS_CHECKRIGHTS
            ELSE
                SELF:_CheckRights := ACE.ADS_IGNORERIGHTS
            ENDIF
            oRet := NULL_OBJECT
            IF VODBRDDInfo(_SET_CONNECTION_HANDLE, REF oRet) .AND. oRet != NULL_OBJECT
                IF oRet IS IntPtr
                    SELF:_Connection := (intPtr) oRet
                ELSE
                    SELF:_Connection := IntPtr.Zero
                ENDIF
            ELSE
                SELF:_Connection := IntPtr.Zero
            ENDIF
            IF SELF:_TableType == ACE.ADS_ADT .OR. SELF:_TableType == ACE.ADS_VFP
                oRet := NULL
                IF VODBRDDInfo(_SET_COLLATION_NAME, REF oRet) .AND. oRet != NULL
                    SELF:_Collation := (STRING) oRet
                ELSE
                    SELF:_Collation := string.Empty
                ENDIF
            ENDIF
            RETURN
            
        PRIVATE METHOD AxAnsi2Unicode(source AS CHAR[], iLength AS LONG) AS STRING
            TRY
                RETURN SELF:_Encoding:GetString(SELF:_Encoding:GetBytes(source, 0, iLength))
            CATCH 
                RETURN String.Empty
            END TRY
            
            
            
            #endregion
            
        #region Open and Close
        METHOD AxFieldSub() AS LOGIC
            LOCAL num AS DWORD
            LOCAL sb AS StringBuilder
            LOCAL pusBufLen AS WORD
            LOCAL fi AS RddFieldInfo
            LOCAL wDecimals AS WORD
            LOCAL wFields AS WORD
            IF !SELF:ACECALL(ACE.AdsGetNumFields(SELF:_Table, OUT wFields))
                RETURN FALSE
            ENDIF
            IF !SELF:SetFieldExtent(wFields)
                RETURN FALSE
            ENDIF
            _fieldCount := wFields
            sb := StringBuilder{ACE.ADS_MAX_FIELD_NAME+1}
            FOR num := 1u TO wFields STEP 1
                LOCAL usType AS WORD
                IF !SELF:ACECALL(ACE.AdsGetFieldType(SELF:_Table, num, OUT usType))
                    RETURN FALSE
                ENDIF
                LOCAL ulLength AS DWORD
                IF !SELF:ACECALL(ACE.AdsGetFieldLength(SELF:_Table, num, OUT ulLength))
                    RETURN FALSE
                ENDIF
                pusBufLen := (WORD) sb:Capacity
                IF !SELF:ACECALL(ACE.AdsGetFieldName(SELF:_Table, (WORD)num, sb, REF pusBufLen))
                    RETURN FALSE
                ENDIF
                fi := RddFieldInfo{"Dummy",DbFieldType.Character,1,0}
                fi:Length := (INT) ulLength
                
                BEGIN SWITCH usType
                CASE ACE.ADS_TIMESTAMP
                CASE ACE.ADS_MODTIME
                    fi:Length := 22
                    fi:fieldType := DbFieldType.Character
                CASE ACE.ADS_TIME
                    fi:Length := 11
                    fi:fieldType := DbFieldType.Character
                CASE ACE.ADS_MONEY
                CASE ACE.ADS_ROWVERSION
                    fi:Length := 22
                    fi:fieldType := DbFieldType.Character
                CASE ACE.ADS_STRING
                CASE ACE.ADS_VARCHAR
                CASE ACE.ADS_CISTRING
                CASE ACE.ADS_VARCHAR_FOX
                CASE ACE.ADS_NCHAR
                CASE ACE.ADS_NVARCHAR
                    fi:fieldType := DbFieldType.Character
                CASE ACE.ADS_MEMO
                CASE ACE.ADS_BINARY
                CASE ACE.ADS_IMAGE
                CASE ACE.ADS_NMEMO
                    fi:fieldType := DbFieldType.Memo
                    fi:Length := 10
                CASE ACE.ADS_RAW
                CASE ACE.ADS_VARBINARY_FOX
                    fi:fieldType := DbFieldType.Memo
                CASE ACE.ADS_SHORTINT
                CASE ACE.ADS_AUTOINC
                CASE ACE.ADS_INTEGER
                CASE ACE.ADS_NUMERIC
                    fi:fieldType := DbFieldType.Number
                    IF usType == ACE.ADS_SHORTINT
                        fi:Length := 6
                    ELSEIF usType == ACE.ADS_AUTOINC
                        fi:Length := 10
                    ELSEIF usType == ACE.ADS_INTEGER
                        fi:Length := 11
                    ELSE
                        fi:Length := (INT) ulLength
                    ENDIF
                    IF !SELF:ACECALL(ACE.AdsGetFieldDecimals(SELF:_Table, num, OUT wDecimals))
                        RETURN FALSE
                    ENDIF
                    fi:Decimals := wDecimals
                CASE ACE.ADS_LOGICAL
                    fi:fieldType := DbFieldType.Logic
                CASE ACE.ADS_DATE
                CASE ACE.ADS_COMPACTDATE
                    fi:fieldType := DbFieldType.Date
                    fi:Length := 8
                CASE ACE.ADS_DOUBLE
                CASE ACE.ADS_CURDOUBLE
                    fi:Length := 20
                    fi:fieldType := DbFieldType.Number
                    IF !SELF:ACECALL(ACE.AdsGetFieldDecimals(SELF:_Table, num, OUT wDecimals))
                        RETURN FALSE
                    ENDIF
                    fi:Decimals := wDecimals
                OTHERWISE
                    SELF:ADSERROR(SubCodes.ERDD_CORRUPT_HEADER, EG_DATATYPE)
                    RETURN FALSE
                END SWITCH
                fi:Name := sb:ToString()
                fi:Alias := NULL
                IF ! SELF:AddField(fi)
                    RETURN FALSE
                ENDIF
            NEXT
            RETURN TRUE
            
        METHOD AxGetFieldInfo(strFieldDef REF STRING ) AS LOGIC
            strFieldDef := ""
            FOREACH fld AS RDDFieldInfo IN SUPER:_Fields
                strFieldDef += fld:Name+", "
                SWITCH fld:FieldType
                    CASE DbFieldType.Character
                        strFieldDef += "C,"
                        // allow clipper field lengths
                        strFieldDef += (fld:Length + fld:Decimals*256):ToString()+";" 
                    CASE DbFieldType.Number
                    CASE DbFieldType.Integer
                        strFieldDef += "N;"+fld:Length:ToString()+", "+fld:Decimals:ToString()+";"
                    CASE DbFieldType.Date
                        strFieldDef += "D;"
                    CASE DbFieldType.Logic
                        strFieldDef += "L;"
                    CASE DbFieldType.Memo
                        IF fld:Length == 23 .AND. fld:Decimals == 4
                            strFieldDef += "Money;"
                        ELSE
                            _HasMemo := TRUE
                            strFieldDef += "M;"
                        ENDIF
                    OTHERWISE
                        SELF:ADSERROR(ERDD_CORRUPT, EG_CORRUPTION)
                        RETURN FALSE                
                END SWITCH
            NEXT
            RETURN TRUE
            
        METHOD SetPaths() AS DWORD
            IF !string.IsNullOrEmpty(SetDefault()) .AND. !SELF:ACECALL(ACE.AdsSetDefault(SetDefault()))
                RETURN 1u
            ENDIF
            IF !string.IsNullOrEmpty(SetPath()) .AND. !SELF:ACECALL(ACE.AdsSetSearchPath(SetPath()))
                RETURN 1u
            ENDIF
            RETURN 0u
            
            /// <inheritdoc />
        VIRTUAL METHOD Open(info AS DbOpenInfo) AS LOGIC
            LOCAL mode AS WORD
            LOCAL alias AS STRING
            LOCAL charset AS WORD
            LOCAL usTableType AS WORD
            LOCAL fileName AS STRING
            //
            mode := 0
            SELF:AxCheckRDDInfo()
            alias := Path.GetFileNameWithoutExtension(info:Alias)
            charset := IIF (RuntimeState.CollationMode == CollationMode.Clipper, ACE.ADS_OEM,ACE.ADS_ANSI)
            IF info:ReadOnly
                mode |= (WORD) ACE.ADS_READONLY
            ENDIF
            IF !info:Shared
                mode |= (WORD) ACE.ADS_EXCLUSIVE
            ENDIF
            IF SELF:SetPaths() != 0
                RETURN FALSE
            ENDIF
            IF SELF:_Connection != IntPtr.Zero
                usTableType := ACE.ADS_DATABASE_TABLE
                fileName := Path.GetFileNameWithoutExtension(info:FileName)
            ELSE
                usTableType := SELF:_TableType
                fileName := info:FileName + info:Extension
            ENDIF
            IF alias:Length > 10
                alias := string.Empty
            ENDIF
            IF SELF:_TableType == ACE.ADS_ADT .AND. Path.GetExtension(fileName):ToUpper() == ".DBF"
                fileName := Path.ChangeExtension(fileName, ".ADT")
            ENDIF
            LOCAL result AS DWORD
            result := ACE.AdsOpenTable90(SELF:_Connection, fileName, alias, usTableType, mode, SELF:_LockType, SELF:_CheckRights, charset, SELF:_Collation, OUT SELF:_Table)
            IF result != 0 .AND. SELF:_Connection != IntPtr.Zero
                usTableType := SELF:_TableType
                IF fileName[0] != '#'
                    fileName := info:FileName + info:Extension
                    IF SELF:_TableType == ACE.ADS_ADT .AND. Path.GetExtension(fileName):ToUpper() == ".DBF"
                        fileName := Path.ChangeExtension(fileName, ".ADT")
                    ENDIF
                ENDIF
                result := ACE.AdsOpenTable90(SELF:_Connection, fileName, alias, usTableType, mode, SELF:_LockType, SELF:_CheckRights, charset, SELF:_Collation, OUT SELF:_Table)
            ENDIF
            IF result != 0
                //SELF:PrintCallTrace("AdsOpenTable failed", num3:ToString())
                //RDDBase.RT_NetErr := TRUE
                SELF:Close()
                SELF:ADSERROR(ERDD_OPEN_FILE, EG_OPEN, "Open")
                RETURN FALSE
            ENDIF
            IF !SELF:AxFieldSub()
                SELF:Close()
                RETURN FALSE
            ENDIF
            LOCAL length := MAX_PATH AS WORD
            LOCAL afileName AS CHAR[]
            afileName := CHAR[]{length}
            IF !SELF:ACECALL(ACE.AdsGetTableFilename(SELF:_Table, ACE.ADS_FULLPATHNAME, afileName, REF length))
                RETURN FALSE
            ENDIF
            SELF:_FileName := STRING {aFileName,0, length}
            IF result != 0
                SELF:Close()
                RETURN FALSE
            ENDIF
            LOCAL numIndexes AS WORD
            SELF:ACECALL(ACE.AdsGetNumIndexes(SELF:_Table, OUT numIndexes))
            IF numIndexes > 0 .AND. !SELF:ACECALL(ACE.AdsGetIndexHandleByOrder(SELF:_Table, 1, OUT SELF:_Index))
                RETURN FALSE
            ENDIF
            IF charset == ACE.ADS_ANSI
                SELF:_Encoding := Encoding.GetEncoding((LONG)RuntimeState.WinCodePage)
            ELSE
                SELF:_Encoding := Encoding.GetEncoding((LONG)RuntimeState.DosCodePage)
            ENDIF
            LOCAL blockSize AS WORD
            IF ACE.AdsGetMemoBlockSize(SELF:_Table, OUT blockSize) == 0
                _HasMemo := TRUE
            ELSE
                _HasMemo := FALSE
            ENDIF
            LOCAL dwLength AS DWORD
            IF !SELF:ACECALL(ACE.AdsGetRecordLength(SELF:_Table, OUT dwLength))
                RETURN FALSE
            ENDIF
            SELF:_RecordLength := (LONG) dwLength
            SELF:Alias   := info:Alias
            SELF:Area    := info:WorkArea
            RETURN SELF:RecordMovement()
            
            /// <inheritdoc />
        VIRTUAL METHOD Close() AS LOGIC
            LOCAL result AS LOGIC
            result := SUPER:Close()
            IF SELF:_Table != IntPtr.Zero
                IF !SELF:ACECALL(ACE.AdsCloseTable(SELF:_Table))
                    RETURN FALSE
                ENDIF
                SELF:_Table := IntPtr.Zero
            ENDIF
            SELF:_Index := IntPtr.Zero
            SELF:_CheckRights := 0
            SELF:_LockType := 0
            SELF:_TableType := 0
            SELF:Area  := 0
            RETURN result
            
            /// <inheritdoc />
        VIRTUAL METHOD Create(info AS DbOpenInfo) AS LOGIC
            LOCAL strFieldDef AS STRING
            LOCAL charset AS WORD
            LOCAL alias AS STRING
            LOCAL length AS WORD
            //
            strFieldDef     := string.Empty
            SELF:_Alias     := info:Alias
            SELF:_Area      := info:Workarea
            SELF:_Shared    := info:Shared
            SELF:_ReadOnly  := info:ReadOnly
            SELF:_Ansi      := RuntimeState.Ansi

            IF !SELF:AxGetFieldInfo(REF strFieldDef)
                RETURN FALSE
            ENDIF
            SELF:AxCheckRDDInfo()
            IF SELF:SetPaths() != 0
                RETURN FALSE
            ENDIF
            charset := IIF (RuntimeState.CollationMode == CollationMode.Clipper, ACE.ADS_OEM,ACE.ADS_ANSI)
            alias := Path.GetFileNameWithoutExtension(info:Alias)
            IF alias:Length > 10
                alias := string.Empty
            ENDIF
            IF !SELF:ACECALL(ACE.AdsCreateTable90(SELF:_Connection, info:FileName, alias, SELF:_TableType, charset, SELF:_LockType, SELF:_CheckRights, 0, strFieldDef, 0u, SELF:_Collation, OUT SELF:_Table))
                RETURN FALSE
            ENDIF
            IF charset== ACE.ADS_ANSI
                SELF:_Encoding := Encoding.GetEncoding(RuntimeState.WinCodePage)
            ELSE
                SELF:_Encoding := Encoding.GetEncoding(RuntimeState.DosCodePage)
            ENDIF
            length := MAX_PATH
            LOCAL fileName AS CHAR[]
            fileName := CHAR[]{length}
            IF !SELF:ACECALL(ACE.AdsGetTableFilename(SELF:_Table, ACE.ADS_FULLPATHNAME , fileName, REF length))
                RETURN FALSE
            ENDIF
            SELF:_FileName := STRING{fileName,0, length}
            LOCAL dwLength AS DWORD
            IF !SELF:ACECALL(ACE.AdsGetRecordLength(SELF:_Table, OUT dwLength))
                RETURN FALSE
            ENDIF
            SELF:_RecordLength := (LONG) dwLength
            IF !SUPER:Create(info)
                SELF:Close()
                RETURN FALSE
            ENDIF
            RETURN SELF:RecordMovement()
            // Check if a Field definition is correct :
            // Date Length must be 8, Number are long enough to store Dot and Decs (if any), ...
        PROTECT METHOD _checkFields(info REF RddFieldInfo) AS VOID
            // FieldName
            info:Name := info:Name:ToUpper():Trim()
            IF ( info:Name:Length > 10 )
                info:Name := info:Name:Substring(0,10)
            ENDIF
            //
            SWITCH info:FieldType
                CASE DbFieldType.Character
                    IF ( info:Length == 0 ) .OR. ( info:Decimals > 0 ) .OR. (info:Length > System.UInt16.MaxValue )
                        SELF:ADSERROR(ERDD.CREATE_FILE, XSharp.Gencode.EG_ARG )
                    ENDIF
                CASE DbFieldType.Number
                    IF ( info:Length >= 1 ) .AND. ( info:Length <= 255 )
                        IF ( info:Decimals > 0 )
                            // We must check that we have enough space for DOT and decimal
                            IF ( info:Length <= 2 ) .OR. ( info:Decimals >= info:Length -1 )
                                SELF:ADSERROR( ERDD.CREATE_FILE, XSharp.Gencode.EG_ARG )
                            ENDIF
                        ENDIF
                    ELSE
                        SELF:ADSERROR( ERDD.CREATE_FILE, XSharp.Gencode.EG_ARG )
                    ENDIF
                CASE DbFieldType.Date
                    IF ( info:Length != 8 ) .OR. ( info:Decimals != 0 )
                        SELF:ADSERROR( ERDD.CREATE_FILE, XSharp.Gencode.EG_ARG )
                    ENDIF
                CASE DbFieldType.Logic
                    IF ( info:Length != 1 ) .OR. ( info:Decimals != 0 )
                        SELF:ADSERROR( ERDD.CREATE_FILE, XSharp.Gencode.EG_ARG )
                    ENDIF
                CASE DbFieldType.Memo
                    IF ( info:Length != 10 ) .OR. ( info:Decimals != 0 )
                        SELF:ADSERROR( ERDD.CREATE_FILE, XSharp.Gencode.EG_ARG )
                    ENDIF
                OTHERWISE
                    // To be done : Support of Fox Field Types, ....
                    info:FieldType := DbFieldType.Unknown
            END SWITCH
            RETURN
            
        VIRTUAL METHOD AddField(info AS RddFieldInfo) AS LOGIC
          LOCAL isOk AS LOGIC
            // Check if the FieldName already exists
            isok := SELF:FieldIndex(info:Name) == 0
            IF isOk
                IF ( SELF:_addFieldPos < SELF:_Fields:Length )
                    SELF:_checkFields( info )
                    SELF:_Fields[ SELF:_addFieldPos++ ] := info:Clone()
                    SELF:_RecordLength += (WORD)info:Length
                ELSE
                    isOk := FALSE
                ENDIF
            ENDIF
            IF ( isOk ) .AND. info:FieldType == DbFieldType.Memo
                SELF:_HasMemo := TRUE
            ENDIF
            RETURN isOk
            
            
            #endregion
        #region Navigation
    METHOD RecordMovement() AS LOGIC
        LOCAL atBOF AS WORD
        LOCAL atEOF AS WORD
        LOCAL isFound AS WORD
        LOCAL lRecno AS DWORD
        SELF:ACECALL(ACE.AdsGetRecordNum(SELF:_Table, ACE.ADS_IGNOREFILTERS,  OUT lRecno))
        SELF:_Recno := lRecno
        SELF:ACECALL(ACE.AdsAtBOF(SELF:_Table, OUT atBOF))
        SUPER:_Bof := (atBOF == 1)
        SELF:ACECALL(ACE.AdsAtEOF(SELF:_Table, OUT atEOF))
        SUPER:_Eof := (atEOF == 1)
        SELF:ACECALL(ACE.AdsIsFound(SELF:_Table,  OUT isFound))
        SUPER:_Found := (isFound == 1)
        IF atBOF == 1 .AND. atEOF == 0
            SELF:GoTop()
            SUPER:_Bof := TRUE
        ENDIF
        //IF SUPER:m_lpdbRelations != nul)
        //FOREACH dbrelinfo AS DBRELINFO IN SUPER:m_lpdbRelations
        //(ADSRDD)dbrelinfo:lpaChild :RecordMovement()
        //NEXT
        //ENDIF
        RETURN TRUE
        
        /// <inheritdoc />
    VIRTUAL METHOD GoBottom() AS LOGIC
        SELF:ACECALL(SELF:AxCheckVODeletedFlag())
        SELF:ACECALL(ACE.AdsGotoBottom(SELF:ACEORDER()))
        RETURN SELF:RecordMovement()
        
        /// <inheritdoc />
    VIRTUAL METHOD GoTop() AS LOGIC
        SELF:ACECALL(SELF:AxCheckVODeletedFlag())
        SELF:ACECALL(ACE.AdsGotoTop(SELF:ACEORDER()))
        RETURN SELF:RecordMovement()
        
        /// <inheritdoc />
    VIRTUAL METHOD GoTo(lRec AS DWORD) AS LOGIC
        LOCAL recordnum AS DWORD
        LOCAL atEOF AS WORD
        LOCAL atBOF AS WORD
        SELF:ACECALL(ACE.AdsGetRecordNum(SELF:_Table, ACE.ADS_IGNOREFILTERS, OUT recordnum))
        IF recordnum == lRec
            SELF:ACECALL(ACE.AdsAtEOF(SELF:_Table, OUT atEOF))
            SELF:ACECALL(ACE.AdsAtBOF(SELF:_Table, OUT atBOF))
            IF atEOF== 0 .AND. atBOF == 0
                SELF:ACECALL(ACE.AdsWriteRecord(SELF:_Table))
                SELF:ACECALL(ACE.AdsRefreshRecord(SELF:_Table))
            ENDIF
        ELSE
            SELF:ACECALL(ACE.AdsGotoRecord(SELF:_Table, lRec))
        ENDIF
        RETURN SELF:RecordMovement()
        
        /// <inheritdoc />
    VIRTUAL METHOD GoToId(oRecnum AS OBJECT) AS LOGIC
        LOCAL recNum AS DWORD
        TRY
            recNum := System.Convert.ToUInt32(oRecnum)
        CATCH e AS Exception
            SELF:ADSERROR(ERDD.DATATYPE, XSharp.Gencode.EG_DataType, "GoToId",e:Message)
            RETURN FALSE
        END TRY
        RETURN SELF:GoTo(recNum)
        
        /// <inheritdoc />
    VIRTUAL METHOD Skip(lCount AS LONG) AS LOGIC
        LOCAL result AS DWORD
        LOCAL flag AS LOGIC
        SELF:ACECALL(SELF:AxCheckVODeletedFlag())
        IF lCount == 0
            SELF:ACECALL(ACE.AdsWriteRecord(SELF:_Table))
            result := ACE.AdsRefreshRecord(SELF:_Table)
        ELSE
            result := ACE.AdsSkip(SELF:ACEORDER(), lCount)
        ENDIF
        IF result != ACE.AE_NO_CURRENT_RECORD
            SELF:ACECALL(result)
        ENDIF
        flag := SELF:RecordMovement()
        IF lCount > 0
            SUPER:_Bof := FALSE
            RETURN flag
        ENDIF
        IF lCount < 0
            SUPER:_Eof := FALSE
        ENDIF
        RETURN flag
        
        /// <inheritdoc />
    VIRTUAL METHOD SkipFilter(lCount AS LONG) AS LOGIC
        // No difference with normal skip
        RETURN SELF:Skip(lCount)
        
        /// <inheritdoc />
    VIRTUAL METHOD SkipRaw(lCount AS LONG) AS LOGIC
        // No difference with normal skip
        RETURN SELF:Skip(lCount)
        
        
        /// <inheritdoc />
    VIRTUAL METHOD Seek(seekinfo AS DBSEEKINFO) AS LOGIC
        LOCAL mode AS WORD
        LOCAL found AS WORD
        LOCAL atEOF AS WORD
        LOCAL atBOF AS WORD
        LOCAL Key AS STRING
        IF SELF:_Index == System.IntPtr.Zero
            SELF:ADSERROR(ERDD.DATATYPE, XSharp.Gencode.EG_NOORDER, "Seek")
        ENDIF
        SELF:ACECALL(SELF:AxCheckVODeletedFlag())
        Key := seekinfo:value:ToString()
        IF seekinfo:SoftSeek
            mode := ACE.ADS_SOFTSEEK
        ELSE
            mode := ACE.ADS_HARDSEEK
        ENDIF
        IF seekInfo:Last
            SELF:ACECALL(ACE.AdsSeekLast(SELF:_Index, Key, (WORD)Key:Length , ACE.ADS_STRINGKEY, OUT found))
            IF found== 0 .AND. seekinfo:SoftSeek  
                SELF:ACECALL(ACE.AdsSeek(SELF:_Index, Key, (WORD)Key:Length , ACE.ADS_STRINGKEY, mode, OUT found))
            ENDIF
        ELSE
            SELF:ACECALL(ACE.AdsSeek(SELF:_Index, Key, (WORD)Key:Length , ACE.ADS_STRINGKEY, mode, OUT found))
        ENDIF 
        SELF:ACECALL(ACE.AdsAtEOF(SELF:_Table, OUT atEOF))
        SELF:ACECALL(ACE.AdsAtBOF(SELF:_Table, OUT atBOF))
        SUPER:_Found := found != 0
        SUPER:_Eof := atEOF != 0
        SUPER:_Bof := atBOF != 0
        RETURN TRUE
        
    PRIVATE METHOD AxSetScope(hIndex AS System.IntPtr, usScopeOption AS WORD, oScope AS OBJECT ) AS OBJECT
        LOCAL aScope    AS CHAR[]
        LOCAL wBufLen   AS WORD
        LOCAL ulRetCode AS DWORD
        LOCAL str       AS STRING
        aScope    := CHAR[]{(SELF:_MaxKeySize + 1)}
        wBufLen   := (WORD)(SELF:_MaxKeySize + 1) 
        ulRetCode := ACE.AdsGetScope(hIndex, usScopeOption, aScope, REF wBufLen)
        IF ulRetCode != ACE.AE_NO_SCOPE
            SELF:ACECALL(ulRetCode)
        ENDIF
        str := oScope:ToString()
        IF !String.IsNullOrEmpty(str)
            SELF:ACECALL(ACE.AdsSetScope(hIndex, usScopeOption, str, (WORD)str:Length , ACE.ADS_STRINGKEY))
        ELSE
            SELF:ACECALL(ACE.AdsClearScope(hIndex, usScopeOption))
        ENDIF
        // return the previous scope
        IF ulRetCode == ACE.AE_NO_SCOPE
            RETURN NULL
        ELSE
            RETURN SELF:AxAnsi2Unicode(aScope, wBufLen)
        ENDIF
        
        
        #endregion
    #region Updates 
    
    /// <inheritdoc />
    VIRTUAL METHOD Flush() AS LOGIC
        RETURN SELF:GoCold()
        
        /// <inheritdoc />
    VIRTUAL METHOD GoCold() AS LOGIC
        SELF:ACECALL(ACE.AdsWriteRecord(SELF:_Table))
        RETURN SELF:RecordMovement()
        
        /// <inheritdoc />
    VIRTUAL METHOD GoHot() AS LOGIC
        LOCAL options AS DWORD
        LOCAL isTableLocked AS WORD
        LOCAL isRecordLocked AS WORD
        LOCAL pulRec AS DWORD
        LOCAL numRecs AS DWORD
        LOCAL ulRetCode AS DWORD
        //
        SELF:ACECALL(ACE.AdsGetTableOpenOptions(SELF:_Table, OUT options))
        // GoHot must have lock when not exclusive
        IF (options & ACE.ADS_EXCLUSIVE) != ACE.ADS_EXCLUSIVE
            // Only allowed when not Readonly
            IF (options & ACE.ADS_READONLY) == ACE.ADS_READONLY
                SELF:ADSERROR(ERDD.READONLY, XSharp.Gencode.EG_READONLY)
            ENDIF
            SELF:ACECALL(ACE.AdsIsTableLocked(SELF:_Table, OUT isTableLocked))
            IF isTableLocked == 0
                SELF:ACECALL(ACE.AdsIsRecordLocked(SELF:_Table, 0, OUT isRecordLocked))
                IF isRecordLocked == 0
                    pulRec := 0
                    IF SELF:IsADT
                        ulRetCode := ACE.AdsGetRecordCount(SELF:_Table, ACE.ADS_IGNOREFILTERS, OUT numRecs)
                        IF ulRetCode == 0
                            ulRetCode := ACE.AdsGetRecordNum(SELF:_Table, ACE.ADS_IGNOREFILTERS, OUT pulRec)
                        ENDIF
                        IF ulRetCode == 0 .AND. numRecs < pulRec .AND. ! SUPER:_Bof .AND. ! SUPER:_Eof
                            ulRetCode := ACE.AdsLockRecord(SELF:_Table, 0)
                            SWITCH ulRetCode
                            CASE 0
                                CASE ACE.AE_TABLE_NOT_SHARED
                                    RETURN TRUE
                                END SWITCH
                            SELF:ACECALL(ulRetCode)
                        ENDIF
                    ENDIF
                    SELF:ADSERROR(ERDD.UNLOCKED, XSharp.Gencode.EG_STROVERFLOW, XSharp.Severity.ES_ERROR)
                ENDIF
            ENDIF
        ENDIF
        RETURN TRUE
        
        /// <inheritdoc />
    VIRTUAL METHOD Zap() AS LOGIC
        LOCAL options AS DWORD
        SELF:ACECALL(ACE.AdsGetTableOpenOptions(SELF:_Table, OUT options))
        // Only allowed when opened exclusively
        IF (options & ACE.ADS_EXCLUSIVE) != ACE.ADS_EXCLUSIVE
            SELF:ADSERROR(ACE.AE_TABLE_NOT_EXCLUSIVE, XSharp.Gencode.EG_SHARED , "Zap")
        ENDIF
        // Only allowed when not Readonly
        IF (options & ACE.ADS_READONLY) == ACE.ADS_READONLY
            SELF:ADSERROR(ERDD.READONLY, XSharp.Gencode.EG_READONLY, "Zap")
        ENDIF
        SELF:ACECALL(ACE.AdsZapTable(SELF:_Table))
        RETURN SELF:GoTop()
        
        
        /// <inheritdoc />
    VIRTUAL METHOD Append(fReleaseLocks AS LOGIC) AS LOGIC
        LOCAL result AS DWORD
        LOCAL handleType AS WORD
        LOCAL isTableLocked AS WORD
        //
        IF fReleaseLocks
            //
            SELF:ACECALL(ACE.AdsGetHandleType(SELF:_Table, OUT handleType))
            IF handleType != ACE.ADS_CURSOR
                //
                result := ACE.AdsIsTableLocked(SELF:_Table, OUT isTableLocked)
                IF isTableLocked == 0
                    //
                    result := ACE.AdsUnlockTable(SELF:_Table)
                    // When Unlock fails because not shared or because not locked, then no problem
                    IF result!= ACE.AE_TABLE_NOT_SHARED .AND. result!= ACE.AE_TABLE_NOT_LOCKED  
                        SELF:ACECALL(result)
                    ENDIF
                ENDIF
            ENDIF
        ENDIF
        SELF:ACECALL(ACE.AdsAppendRecord(SELF:_Table))
        result := ACE.AdsLockRecord(SELF:_Table, 0)
        IF result != ACE.AE_TABLE_NOT_SHARED
            SELF:ACECALL(result)
        ENDIF
        RETURN SELF:RecordMovement()
        
    VIRTUAL METHOD DeleteRecord() AS LOGIC
        SELF:ACECALL(ACE.AdsDeleteRecord(SELF:_Table))
        RETURN TRUE
        
    VIRTUAL METHOD Recall() AS LOGIC
        SELF:ACECALL(ACE.AdsRecallRecord(SELF:_Table))
        RETURN SELF:RecordMovement()
        
        #endregion
        
    #region Read and Write
    METHOD GetValue(nFldPos AS INT) AS OBJECT
	    LOCAL result    := 0 AS DWORD
	    LOCAL chars     := NULL AS CHAR[]
	    LOCAL bytes     := NULL AS BYTE[]
	    LOCAL length    := 0 AS DWORD
	    LOCAL isEmpty   := 0 AS WORD
        LOCAL wType     AS WORD
	    IF !SELF:ACECALL(ACE.AdsGetFieldType(SELF:_Table, (DWORD) nFldPos + 1, OUT wType))
		    // Throw exceptio
	    ENDIF
        LOCAL fld       := SELF:_Fields[nFldPos] AS RddFieldInfo
        length  := (DWORD) fld:Length
        SWITCH fld:FieldType
        CASE DbFieldType.Character
            SWITCH wType
            CASE ACE.ADS_TIME
            CASE ACE.ADS_TIMESTAMP
            CASE ACE.ADS_MONEY
            CASE ACE.ADS_ROWVERSION
            CASE ACE.ADS_MODTIME
              	IF !SELF:ACECALL(ACE.AdsIsEmpty(SELF:_Table, (DWORD) nFldPos + 1, OUT isEmpty))
		            // Throw exception
	            ENDIF
                IF isEmpty == 1
                    RETURN NULL
                ENDIF
                chars := CHAR[] {length}
                result := ACE.AdsGetField(SELF:_Table, (DWORD) nFldPos + 1, chars, REF length ,0)
            CASE ACE.ADS_STRING
            CASE ACE.ADS_VARCHAR
            CASE ACE.ADS_CISTRING
            CASE ACE.ADS_VARCHAR_FOX
                chars := CHAR[] {length}
                result := ACE.AdsGetString(SELF:_Table, (DWORD) nFldPos + 1, chars, REF length ,0)
            CASE ACE.ADS_NCHAR
            CASE ACE.ADS_NVARCHAR
                chars := CHAR[] {length}
                result := ACE.AdsGetStringW(SELF:_Table, (DWORD) nFldPos + 1, chars, REF length ,0)
            OTHERWISE
                SELF:ADSERROR(ERDD_DATATYPE, EG_DATATYPE)
            END SWITCH
            SWITCH result
            CASE 0
                IF wType != ACE.ADS_NCHAR .AND. wType != ACE.ADS_NVARCHAR
                    RETURN SELF:AxAnsi2Unicode(chars, (INT) length)
                ELSE
                    RETURN STRING{chars, 0, (INT) length}
                ENDIF
            CASE ACE.AE_NO_CURRENT_RECORD
                RETURN STRING{' ', (INT) length}
            OTHERWISE
                SELF:ACECALL(result)
            END SWITCH
        CASE DbFieldType.Memo
            SWITCH wType
            CASE ACE.ADS_MEMO
            CASE ACE.ADS_BINARY
            CASE ACE.ADS_IMAGE
            CASE ACE.ADS_NMEMO
                result := ACE.AdsGetMemoLength(SELF:_Table, (DWORD) nFldPos + 1, OUT length )
                SWITCH result
                CASE 0
                    IF length == 0
                        RETURN string.Empty
                    ENDIF
                CASE ACE.AE_NO_CURRENT_RECORD
                    IF wType == ACE.ADS_MEMO .OR. wType == ACE.ADS_NMEMO
                        RETURN string.Empty
                    ELSE    // image and binary
                        RETURN NULL
                    ENDIF
                OTHERWISE
                    SELF:ACECALL(result)
                END SWITCH
                SWITCH wType
                CASE ACE.ADS_MEMO
                   chars := CHAR[] {++length}
                   IF SELF:ACECALL(ACE.AdsGetString(SELF:_Table, (DWORD) nFldPos + 1, chars, REF length ,0))
                        RETURN NULL
                   ENDIF
                   RETURN SELF:AxAnsi2Unicode(chars, (INT) length)
                CASE ACE.ADS_NMEMO
                   chars := CHAR[] {++length}
                   IF ! SELF:ACECALL(ACE.AdsGetStringW(SELF:_Table, (DWORD) nFldPos + 1, chars, REF length ,0))
                        RETURN NULL
                   ENDIF
                CASE ACE.ADS_BINARY
                CASE ACE.ADS_IMAGE
                   bytes := BYTE[] {length}
                   IF ! SELF:ACECALL(ACE.AdsGetBinary(SELF:_Table, (DWORD) nFldPos + 1, 0, bytes, REF length ))
                        RETURN NULL
                   ENDIF
                    RETURN bytes
                END SWITCH
                     
            CASE ACE.ADS_RAW
            CASE ACE.ADS_VARBINARY_FOX
              	result := ACE.AdsIsEmpty(SELF:_Table, (DWORD) nFldPos + 1, OUT isEmpty)
                IF result == 0
                    IF isEmpty == 1
                        RETURN NULL
                    ENDIF
                ELSE
                    IF ! SELF:ACECALL(result)
                        // Throw exception
                    ENDIF
                ENDIF
                length := (DWORD) fld:Length
                bytes := BYTE[] {length}
                IF ! SELF:ACECALL(ACE.AdsGetBinary(SELF:_Table, (DWORD) nFldPos + 1, 0, bytes, REF length ))
                    RETURN NULL
                ENDIF
                RETURN bytes
            END SWITCH
        CASE DbFieldType.Number
            SWITCH wType
            CASE ACE.ADS_NUMERIC
            CASE ACE.ADS_DOUBLE
            CASE ACE.ADS_INTEGER
            CASE ACE.ADS_SHORTINT
            CASE ACE.ADS_AUTOINC
            CASE ACE.ADS_CURDOUBLE
                LOCAL r8 AS REAL8
                result := ACE.AdsGetDouble(SELF:_Table, (DWORD) nFldPos + 1, OUT r8)
                IF result == ACE.AE_NO_CURRENT_RECORD
                    r8 := 0.0
                ENDIF
                IF wType == ACE.ADS_DOUBLE .OR. wType == ACE.ADS_CURDOUBLE
                    VAR value := r8:ToString()
                    VAR pos := VALUE:IndexOf('.')
                    RETURN DbFloat{r8, fld:Length, IIF(pos > 0, fld:Length - pos -1, 0)}
                ELSE
                    RETURN DbFloat{r8, fld:Length, fld:Decimals}
                ENDIF
            OTHERWISE
                SELF:ADSERROR(ERDD_DATATYPE, EG_DATATYPE)
            END SWITCH
   
        CASE DbFieldType.Logic
            LOCAL wValue AS WORD
            result := ACE.AdsGetLogical(SELF:_Table, (DWORD) nFldPos + 1, OUT wValue)
            IF result ==  ACE.AE_NO_CURRENT_RECORD
                wValue := 0
            ELSEIF ! SELF:ACECALL(result)
                // Exception
            ENDIF
            RETURN wValue == 0
        CASE DbFieldType.Date
            LOCAL wLength := ACE.ADS_MAX_DATEMASK+1 AS WORD
            LOCAL julianDate AS LONG
            result := ACE.AdsGetJulian(SELF:_Table, (DWORD) nFldPos + 1, OUT julianDate)
            IF result ==  ACE.AE_NO_CURRENT_RECORD
                RETURN DateTime.MinValue
            ELSEIF result == 0
                IF julianDate == 0
                    RETURN DateTime.MinValue
                ENDIF
            ELSE
                SELF:ACECALL(result)
            ENDIF
            chars := CHAR[]{wLength}
            IF ACEUNPUB.AdsConvertJulianToString((REAL8) julianDate, chars, REF wLength) == 0
                TRY
                    RETURN DateTime.ParseExact(STRING{chars, 0, wLength}, "yyyyMMdd",NULL)
                CATCH
                    RETURN DateTime.MinValue
                END TRY
            ELSE
                RETURN DateTime.MinValue
            ENDIF
        OTHERWISE
            SELF:ADSERROR(ERDD_DATATYPE, EG_DATATYPE)
        END SWITCH
        RETURN NULL
        
    METHOD GetValueLength(nFldPos AS LONG) AS LONG
        LOCAL fld AS RddFieldInfo
        LOCAL dwLen AS DWORD
        fld := SELF:_Fields[nFldPos]
        IF fld:FieldType == DbFieldType.Memo
            SELF:ACECALL(ACE.AdsGetFieldLength(SELF:_Table, (DWORD) nFldPos + 1,OUT dwLen))
            RETURN (INT) dwLen
        ELSEIF fld:FieldType == DbFieldType.Date
            LOCAL chars AS CHAR[]
            chars := CHAR[]{ACE.ADS_MAX_DATEMASK+1}
            LOCAL wLen := (WORD) chars:Length AS WORD
            SELF:ACECALL(ACE.AdsGetDateFormat(chars, REF wLen))
            RETURN wLen
        ELSE
            RETURN fld:Length
        ENDIF
        
    METHOD PutValue(nFldPos AS INT, oValue AS OBJECT) AS LOGIC
        LOCAL tc AS TypeCode
        IF ! SELF:GoHot()
            RETURN FALSE
        ENDIF
        IF oValue == NULL
            SELF:ACECALL(ACE.AdsSetEmpty(SELF:_Table, (DWORD) nFldPos+1))
            RETURN TRUE
        ENDIF
        // Handle IDate and IFloat
        IF oValue IS IDate
            LOCAL oDate AS IDate
            tc := TypeCode.DateTime
            oDate := (IDate) oValue
            IF oDate:IsEmpty
                oValue := DateTime.MinValue
            ELSE
                oValue := DateTime{oDate:Year, oDate:Month, oDate:Day}
            ENDIF
        ELSEIF oValue IS IFloat
            tc := TypeCode.Double
            oValue := ((IFloat) oValue):Value
        ELSE
            tc := Type.GetTypeCode(oValue:GetType())
        ENDIF
        LOCAL fld AS RDDFieldInfo
        LOCAL wType AS WORD
        LOCAL result := 0 AS DWORD
        fld  := SELF:_Fields[nFldPos]
        SELF:ACECALL(ACE.AdsGetFieldType(SELF:_Table, (DWORD) nFldPos+1, OUT wType))
        SWITCH fld:FieldType
        CASE DbFieldType.Character
        CASE DbFieldType.Memo
            LOCAL length AS DWORD
            LOCAL strValue AS STRING
            SWITCH wType
            CASE ACE.ADS_STRING
            CASE ACE.ADS_MEMO
            CASE ACE.ADS_VARCHAR
            CASE ACE.ADS_CISTRING
                IF tc != TypeCode.String
                    SELF:ADSERROR(ERDD_DATATYPE, EG_DataType, "PutValue","String expected")
                ENDIF
                strValue := (STRING) oValue
                length := (DWORD) strValue:Length
                IF length == 0
                    result := ACE.AdsSetEmpty(SELF:_Table, (DWORD) nFldPos+1)
                ELSE
                    result := ACE.AdsSetString(SELF:_Table, (DWORD) nFldPos+1, strValue, length)
                ENDIF
            CASE ACE.ADS_NCHAR
            CASE ACE.ADS_NVARCHAR
            CASE ACE.ADS_NMEMO
                IF tc != TypeCode.String
                    SELF:ADSERROR(ERDD_DATATYPE, EG_DataType, "PutValue","String expected")
                ENDIF
                strValue := (STRING) oValue
                length := (DWORD) strValue:Length
                IF length == 0
                    result := ACE.AdsSetEmpty(SELF:_Table, (DWORD) nFldPos+1)
                ELSE
                    result := ACE.AdsSetStringW(SELF:_Table, (DWORD) nFldPos+1, strValue, length)
                ENDIF
            CASE ACE.ADS_TIME
            CASE ACE.ADS_TIMESTAMP
            CASE ACE.ADS_MONEY
            CASE ACE.ADS_ROWVERSION
            CASE ACE.ADS_MODTIME
                IF tc != TypeCode.String
                    SELF:ADSERROR(ERDD_DATATYPE, EG_DataType, "PutValue","String expected")
                ENDIF
                strValue := (STRING) oValue
                length := (DWORD) strValue:Length
                result := ACE.AdsSetField(SELF:_Table, (DWORD) nFldPos+1, strValue, length)
            CASE ACE.ADS_BINARY
            CASE ACE.ADS_IMAGE
            CASE ACE.ADS_RAW
            CASE ACE.ADS_VARBINARY_FOX
                IF tc != TypeCode.String .AND. tc != TypeCode.Object
                    SELF:ADSERROR(ERDD_DATATYPE, EG_DataType, "PutValue","String or Object expected")
                ENDIF
                IF tc != TypeCode.String
                    strValue := (STRING) oValue
                    length := (DWORD) strValue:Length
                    result := ACE.AdsSetString(SELF:_Table, (DWORD) nFldPos+1, strValue, length)
                ELSE
                    LOCAL bytes AS BYTE[]
                    bytes := (BYTE[]) oValue
                    length := (DWORD) bytes:Length
                    IF wType == ACE.ADS_RAW .OR. wType == ACE.ADS_VARBINARY_FOX
                        result := ACE.AdsSetField(SELF:_Table, (DWORD) nFldPos+1, bytes, length)
                    ELSE
                        result := ACE.AdsSetBinary(SELF:_Table, (DWORD) nFldPos+1, wType, length, 0, bytes, length)
                    ENDIF
                ENDIF
            OTHERWISE
                SELF:ADSError(ERDD_DATATYPE, EG_DataType, "PutValue")
            END SWITCH
            IF result != 0 .AND. result != ACE.AE_DATA_TRUNCATED
                SELF:ADSERROR(result, EG_WRITE, "PutValue")
            ENDIF
        CASE DbFieldType.Number
            IF tc != TypeCode.Double .AND. tc != TypeCode.Int32 .AND. tc != TypeCode.Decimal
                SELF:ADSERROR(ERDD_DATATYPE, EG_DataType, "PutValue","Numeric value expected")
            ENDIF
            IF wType != ACE.ADS_AUTOINC
                LOCAL r8 AS REAL8
                r8 := Convert.ToDouble(oValue)
                SELF:ACECALL(ACE.AdsSetDouble(SELF:_Table, (DWORD) nFldPos+1, r8))
            ELSE
                NOP // Do not allow to write to AUTO INC field
            ENDIF
        CASE DbFieldType.Logic
            IF tc != TypeCode.Boolean
                SELF:ADSERROR(ERDD_DATATYPE, EG_DataType, "PutValue","Logic value expected")
            ENDIF
            SELF:ACECALL(ACE.AdsSetLogical(SELF:_Table, (DWORD) nFldPos+1, IIF( (LOGIC) oValue, 1, 0)))
        CASE DbFieldType.Date
            IF tc != TypeCode.DateTime
                SELF:ADSERROR(ERDD_DATATYPE, EG_DataType, "PutValue","Date or DateTime value expected")
            ENDIF
            LOCAL dt   := (DateTime) oValue AS DateTime
            LOCAL text := dt:ToString("yyyyMMdd") AS STRING
            LOCAL r8Julian AS REAL8
            SELF:ACECALL(AceUnPub.AdsConvertStringToJulian(text, (WORD) text:Length, OUT r8Julian))
            SELF:ACECALL(ACE.AdsSetJulian(SELF:_Table, (DWORD) nFldPos+1, (LONG) r8Julian))
        OTHERWISE
           SELF:ADSError(ERDD_DATATYPE, EG_DataType, "PutValue")
        END SWITCH
        
        RETURN FALSE
        
        #endregion
        
    #region Properties
    PROPERTY ACEConnectionHandle AS IntPtr GET _Connection
    PROPERTY ACEIndexHandle AS IntPtr GET _Index
    PROPERTY ACETableHandle AS IntPtr GET _Table
    /// <inheritdoc />
    PROPERTY BOF AS LOGIC GET SUPER:_Bof
    /// <inheritdoc />
    PROPERTY EOF AS LOGIC GET SUPER:_Eof
    /// <inheritdoc />
    PROPERTY Found AS LOGIC GET SUPER:_Found
    PRIVATE PROPERTY IsADT AS LOGIC GET _TableType == ACE.ADS_ADT
    VIRTUAL PROPERTY SysName AS STRING GET _Driver
    
    
    /// <inheritdoc />
    PROPERTY Deleted AS LOGIC
        GET
            LOCAL isDeleted AS WORD
            LOCAL ulRetCode AS DWORD
            //
            ulRetCode := ACE.AdsIsRecordDeleted(SELF:_Table, OUT isDeleted)
            IF ulRetCode == ACE.AE_NO_CURRENT_RECORD
                IF SELF:_Eof
                    RETURN FALSE
                ELSE
                    RETURN TRUE
                ENDIF
            ELSE
                SELF:ACECALL(ulRetCode)
            ENDIF
            RETURN isDeleted != 0
            
        END GET 
    END PROPERTY
    
    
    #endregion
    
    
    #region Locking
    
    
    /// <inheritdoc />
    VIRTUAL METHOD Lock(lockInfo AS DBLOCKINFO) AS LOGIC
        LOCAL lRecno := 0 AS DWORD
        LOCAL result := 0 AS DWORD
        LOCAL handleType AS WORD
        LOCAL isLocked  AS WORD
        //
        lockInfo:Result := FALSE
        IF lockInfo:recID == NULL
            //
            lRecno := SELF:_Recno
        ELSE
            //
            TRY
                lRecno := System.Convert.ToUInt32(lockInfo:recID)
            CATCH e AS Exception
                SELF:ADSERROR(ERDD.DATATYPE, XSharp.Gencode.EG_DATATYPE, "Lock", e:Message)
            END TRY
        ENDIF
        IF lockInfo:@@Method == DBLockInfo.LockMethod.File
            result := ACE.AdsLockTable(SELF:_Table)
            SELF:ACECALL(result);
            
            ENDIF
        SELF:ACECALL(ACE.AdsGetHandleType(SELF:_Table, OUT handleType))
        IF lRecno == 0 .AND. handleType != ACE.ADS_CURSOR
            //
            SELF:ACECALL(ACE.AdsIsTableLocked(SELF:_Table, OUT isLocked))
            IF isLocked == 0
                //
                result := ACE.AdsUnlockTable(SELF:_Table)
                IF result != ACE.AE_TABLE_NOT_LOCKED  .AND. ;
                result != ACE.AE_TABLE_NOT_SHARED
                    SELF:ACECALL(result)
                    result  := ACE.AdsLockRecord(SELF:_Table, lRecno)
                ENDIF
            ENDIF
        ENDIF
        
        IF result != ACE.AE_TABLE_NOT_SHARED
            SELF:ACECALL(result)
            lockInfo:Result := TRUE
        ENDIF
        RETURN TRUE
        
        /// <inheritdoc />
    VIRTUAL METHOD Unlock(recordID AS OBJECT) AS LOGIC
        LOCAL result AS DWORD
        LOCAL numRecord := 0 AS DWORD
        TRY
            numRecord := System.Convert.ToUInt32(recordID)
        CATCH e AS Exception
            SELF:ADSERROR(ERDD.DATATYPE, XSharp.Gencode.EG_DATATYPE, "Unlock",e:Message)
        END TRY
        IF numRecord == 0
            result := ACE.AdsUnlockTable(SELF:_Table)
        ELSE
            result := ACE.AdsUnlockRecord(SELF:_Table, numRecord)
        ENDIF
        IF result != ACE.AE_TABLE_NOT_SHARED
            SELF:ACECALL(result)
        ENDIF
        RETURN TRUE
        
        
        
        #endregion
        
    #region Filters
    /// <inheritdoc />
    VIRTUAL METHOD ClearFilter() AS LOGIC
        LOCAL ulRetCode AS DWORD
        IF SELF:_Table != System.IntPtr.Zero
            // Clear normal filter
            ulRetCode := ACE.AdsClearFilter(SELF:_Table)
            IF ulRetCode != ACE.AE_NO_FILTER 
                SELF:ACECALL(ulRetCode)
            ENDIF
            // Clear optimized filter
            ulRetCode := ACE.AdsClearAOF(SELF:_Table)
            IF ulRetCode != ACE.AE_NO_FILTER 
                SELF:ACECALL(ulRetCode)
            ENDIF
        ENDIF
        SELF:_FilterInfo := DbFilterInfo{}
        RETURN TRUE
        
    METHOD SetFieldExtent( fieldCount AS LONG ) AS LOGIC
        // Todo: Move to workarea later ?
        // Initialize the Fields array
        SELF:_Fields := DbfRddFieldInfo[]{ fieldCount }
        SELF:_addFieldPos := 0
        SELF:_RecordLength := 1 // 1 for DELETED
        SELF:_HasMemo := FALSE
        RETURN TRUE
        
        
        /// <inheritdoc />
    VIRTUAL METHOD SetFilter(fi AS DBFILTERINFO) AS LOGIC
        LOCAL ulRetCode AS DWORD
        // Get the current date format so we can handle literal dates in the filter
        SELF:ACECALL(IIF(SELF:AxCheckVODateFormat(),0,1))
        IF String.IsNullOrEmpty(fi:FilterText)
            // clear filter
            // Ignore "No filter" error
            ulRetCode := ACE.AdsClearFilter(SELF:_Table)
            IF ulRetCode != ACE.AE_NO_FILTER
                SELF:ACECALL(ulRetCode)
            ENDIF
            ulRetCode := ACE.AdsClearAOF(SELF:_Table)
            IF ulRetCode != ACE.AE_NO_FILTER
                SELF:ACECALL(ulRetCode)
            ENDIF
        ELSE
            IF RuntimeState.Optimize
                SELF:ACECALL(ACE.AdsSetAOF(SELF:_Table, fi:FilterText, (WORD) ACE.ADS_RESOLVE_DYNAMIC | ACE.ADS_DYNAMIC_AOF))
            ELSE
                SELF:ACECALL(ACE.AdsSetFilter(SELF:_Table, fi:FilterText))
            ENDIF
        ENDIF
        fi:Active := TRUE
        SELF:_FilterInfo := fi
        RETURN TRUE
        
        #endregion
    #region Relations 
    /// <inheritdoc />
    VIRTUAL METHOD ClearRel() AS LOGIC
        IF SELF:_Table != System.IntPtr.Zero
            SELF:ACECALL(ACE.AdsClearRelation(SELF:_Table))
        ENDIF
        RETURN SUPER:ClearRel()
        
        /// <inheritdoc />
    VIRTUAL METHOD SetRel(relinfo AS DBRELINFO) AS LOGIC
        IF relinfo:Child:Driver != SELF:_Driver
            SELF:ADSERROR(ERDD.UNSUPPORTED, XSharp.Gencode.EG_UNSUPPORTED, "SetRel", "Related workareas must be opened with the same driver.")
            RETURN FALSE
        ENDIF
        VAR child := relInfo:Child ASTYPE ADSRDD
        SELF:ACECALL(ACE.AdsSetRelation(SELF:_Table, child:ACEIndexHandle, relinfo:Key))
        RETURN SUPER:SetRel(relInfo)
        
        #endregion
        
    #region Info
    /// <inheritdoc />
    VIRTUAL METHOD FieldInfo(uiPos AS LONG, uiOrdinal AS INT, oNewValue AS OBJECT) AS OBJECT
        LOCAL fieldType AS WORD
        LOCAL length    AS DWORD
        LOCAL ulRetCode AS DWORD
        SWITCH uiOrdinal
            CASE DBS_BLOB_TYPE
                SELF:ACECALL(ACE.AdsGetFieldType(SELF:_Table, (DWORD)(uiPos + 1) ,  OUT fieldType))
                //
                SWITCH fieldType
                    CASE ACE.ADS_MEMO
                    CASE ACE.ADS_BINARY
                    CASE ACE.ADS_IMAGE
                        RETURN "?"
                END SWITCH
            CASE DBS_BLOB_LEN
                IF SUPER:_fields[uiPos + 1]:fieldType != DBFieldType.Memo  
                    RETURN -1
                ELSE
                    ulRetCode := ACE.AdsGetMemoLength(SELF:_Table, (DWORD)(uiPos + 1) , OUT length)
                    IF ulRetCode != ACE.AE_INVALID_FIELD_TYPE
                        SELF:ACECALL(ulRetCode)
                    ELSE
                        RETURN -1
                    ENDIF
                    RETURN length
                ENDIF
                
            CASE DBS_BLOB_POINTER
                RETURN NULL
                
        END SWITCH
        RETURN SUPER:FieldInfo(uiPos, uiOrdinal, oNewValue)
        
        
        /// <inheritdoc />
    VIRTUAL METHOD Info(uiOrdinal AS LONG, oNewValue AS OBJECT) AS OBJECT
        LOCAL aDate AS CHAR[]
        LOCAL DateLen AS WORD
        LOCAL julDate AS REAL8
        LOCAL isLocked AS WORD
        LOCAL isFound AS WORD
        LOCAL numLocks AS WORD
        LOCAL options AS DWORD
        LOCAL memoBlockSize AS WORD
        LOCAL ulRetCode AS DWORD
        IF uiOrdinal <= DbInfo.DBI_MEMOFIELD  
            SWITCH uiOrdinal
                CASE DBI_ISDBF
                    RETURN !SELF:IsADT
                CASE DBI_CANPUTREC
                    RETURN FALSE
                CASE DBI_GETHEADERSIZE
                    IF SELF:IsADT
                        RETURN 400 + SUPER:_Fields:Length * 200
                    ELSE
                        RETURN 2 + SUPER:_Fields:Length * 32 + 2
                    ENDIF
                CASE DBI_LASTUPDATE
                    aDate := CHAR[]{ACE.ADS_MAX_DATEMASK+1}
                    DateLen := (WORD) aDate:Length
                    SELF:ACECALL(ACE.AdsSetDateFormat("MM/DD/YY"))
                    SELF:ACECALL(ACE.AdsGetLastTableUpdate(SELF:_Table, aDate, REF DateLen))
                    SELF:ACECALL(ACEUNPUB.AdsConvertStringToJulian(aDate, DateLen, OUT julDate))
                    IF !SELF:AxCheckVODateFormat()
                        SELF:ACECALL(1)
                    ENDIF
                    RETURN (LONG)julDate
            CASE DBI_SETDELIMITER 
                CASE DBI_VALIDBUFFER 
                CASE DBI_LOCKOFFSET 
                CASE DBI_MEMOHANDLE 
                CASE DBI_NEWINDEXLOCK 
                CASE DBI_MEMOFIELD 
                    RETURN NULL
                CASE DBI_GETRECSIZE
                    RETURN SUPER:_RecordLength
                CASE DBI_GETLOCKARRAY
                    SELF:ACECALL(ACE.AdsGetNumLocks(SELF:_Table, OUT numLocks))
                    IF numLocks > 0
                        IF SELF:m_palRlocks == NULL .OR. SELF:m_palRlocks:Length < numLocks
                            SELF:m_palRlocks := DWORD[]{numLocks}
                        ENDIF
                        SELF:ACECALL(ACE.AdsGetAllLocks(SELF:_Table, SELF:m_palRlocks, REF numLocks))
                        RETURN SELF:m_palRlocks
                    ELSE
                        RETURN NULL
                    ENDIF
                CASE DBI_TABLEEXT
                    IF SELF:IsADT
                        RETURN ".ADT"
                    ELSE
                        RETURN ".DBF"
                    ENDIF
                CASE DBI_ISFLOCK
                    SELF:ACECALL(ACE.AdsIsTableLocked(SELF:_Table, OUT isLocked))
                    RETURN isLocked != 0
                CASE DBI_FILEHANDLE
                    RETURN IntPtr.Zero
                CASE DbInfo.DBI_FULLPATH
                    RETURN SUPER:_FileName
                CASE DBI_ISANSI
                    RETURN FALSE
                CASE DBI_FOUND
                    SELF:ACECALL(ACE.AdsIsFound(SELF:_Table, OUT isFound))
                    RETURN isFound != 0
                CASE DBI_LOCKCOUNT
                    SELF:ACECALL(ACE.AdsGetNumLocks(SELF:_Table, OUT numLocks))
                    RETURN numLocks
                CASE DBI_SHARED
                    SELF:ACECALL(ACE.AdsGetTableOpenOptions(SELF:_Table, OUT options))
                    RETURN (options & ACE.ADS_EXCLUSIVE) != ACE.ADS_EXCLUSIVE
                CASE DBI_MEMOEXT
                    SWITCH SELF:_TableType 
                        CASE ACE.ADS_ADT
                            RETURN ".ADM"
                    CASE ACE.ADS_CDX
                        CASE ACE.ADS_VFP
                            RETURN ".FPT"
                        OTHERWISE
                            RETURN ".DBT"
                        END SWITCH
                        
                CASE DBI_MEMOBLOCKSIZE
                    ulRetCode := ACE.AdsGetMemoBlockSize(SELF:_Table, OUT memoBlockSize)
                    IF ulRetCode != ACE.AE_NO_MEMO_FILE 
                        SELF:ACECALL(ulRetCode)
                        RETURN memoBlockSize
                    ELSE
                        RETURN NULL
                    ENDIF
                CASE DBI_CODEPAGE
                    RETURN SELF:_Encoding:CodePage
                CASE DBI_DB_VERSION
                    RETURN 0
                CASE DBI_RDD_VERSION
                    RETURN System.Reflection.Assembly.GetExecutingAssembly():GetName():Version:ToString()
                END SWITCH
        ELSE
            SWITCH uiOrdinal
                CASE BLOB_INFO_HANDLE
                CASE BLOB_FILE_RECOVER 
                CASE BLOB_FILE_INTEGRITY 
                CASE BLOB_OFFSET 
                CASE BLOB_POINTER 
                CASE BLOB_LEN 
                CASE BLOB_TYPE 
            CASE BLOB_EXPORT 
                CASE BLOB_ROOT_UNLOCK 
                CASE BLOB_ROOT_PUT 
                CASE BLOB_ROOT_GET 
                CASE BLOB_ROOT_LOCK 
                CASE BLOB_IMPORT 
                CASE BLOB_DIRECT_PUT 
                CASE BLOB_DIRECT_GET 
                CASE BLOB_GET 
                CASE BLOB_DIRECT_EXPORT 
                CASE BLOB_DIRECT_IMPORT 
                CASE BLOB_NMODE 
                CASE BLOB_EXPORT_APPEND 
                CASE BLOB_EXPORT_OVERWRITE 
                    RETURN NULL
                CASE DBI_RL_AND 
                CASE DBI_RL_CLEAR 
                CASE DBI_RL_COUNT 
                CASE DBI_RL_DESTROY 
                CASE DBI_RL_EXFILTER 
                CASE DBI_RL_GETFILTER 
                CASE DBI_RL_HASMAYBE 
                CASE DBI_RL_LEN 
                CASE DBI_RL_MAYBEEVAL 
                CASE DBI_RL_NEW 
                CASE DBI_RL_NEWDUP 
                CASE DBI_RL_NEWQUERY 
                CASE DBI_RL_NEXTRECNO 
                CASE DBI_RL_NOT 
                CASE DBI_RL_OR 
                CASE DBI_RL_PREVRECNO 
                CASE DBI_RL_SET 
                CASE DBI_RL_SETFILTER 
                CASE DBI_RL_TEST  
                    RETURN NULL
                CASE DBI_GET_ACE_TABLE_HANDLE
                    RETURN SELF:_Table
                OTHERWISE
                    RETURN SUPER:Info(uiOrdinal, oNewValue)
                END SWITCH
                
                
        ENDIF
        RETURN SUPER:Info(uiOrdinal, oNewValue)
        
        
        #endregion
        
    #region Unsupported
    /// <inheritdoc />
    VIRTUAL METHOD AppendLock(uiMode AS DbLockMode) AS LOGIC
        RETURN SELF:Unsupported("AppendLock")
        
        
        /// <inheritdoc />
    VIRTUAL METHOD BlobInfo(uiPos AS DWORD, uiOrdinal AS DWORD) AS OBJECT
        SELF:Unsupported("BlobInfo")
        RETURN NULL
        
        /// <inheritdoc />
    VIRTUAL METHOD ForceRel() AS LOGIC
        RETURN SELF:Unsupported("ForceRel")
        
        /// <inheritdoc />
    VIRTUAL METHOD GetRec() AS BYTE[]
        SELF:Unsupported("GetRec")
        RETURN NULL
        
    VIRTUAL METHOD GetValueFile(nFldPos AS INT, cFileName AS STRING) AS LOGIC
        SELF:Unsupported("GetValueFile")
        RETURN FALSE
        
    VIRTUAL METHOD PutValueFile(nFldPos AS INT, cFileName AS STRING) AS LOGIC
        SELF:Unsupported("PutValueFile")
        RETURN FALSE
        
        /// <inheritdoc />
    VIRTUAL METHOD HeaderLock(uiMode AS DbLockMode) AS LOGIC  
        RETURN SELF:Unsupported("HeaderLock")
        
        #endregion
        
END CLASS

