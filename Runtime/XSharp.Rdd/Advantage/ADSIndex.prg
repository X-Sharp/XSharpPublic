//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


USING System
USING System.Collections.Generic
USING System.Text
USING XSharp.RDD
USING XSharp.RDD.Support
USING System.IO
/// <summary>Advantage Index support.</summary>
CLASS XSharp.ADS.ADSIndex INHERIT BaseIndex
    PRIVATE oRDD AS ADSRDD
    PRIVATE _CallbackFn AS CallbackFn
    PRIVATE _iProgress  AS LONG
    PRIVATE _bagName    AS STRING
    #region Helpers
    
    PROPERTY OrderCondInfo AS DBORDERCONDINFO GET oRDD:_OrderCondInfo SET oRDD:_OrderCondInfo := VALUE
    PROPERTY Index AS IntPtr GET oRDD:_Index SET oRDD:_Index := VALUE
    PROPERTY Table AS IntPtr GET oRDD:_Table
    
    PRIVATE METHOD _CheckError(nResult AS DWORD, gencode AS DWORD) AS LOGIC
        IF nResult != 0
            LOCAL lastError AS DWORD
            LOCAL message AS CHAR[]
            LOCAL wBufLen AS WORD
            LOCAL oError AS RddError
            LOCAL strMessage AS STRING
            message := CHAR[]{ACE.ADS_MAX_ERROR_LEN}
            wBufLen := (WORD) message:Length
            IF ACE.AdsGetLastError(OUT lastError, message, REF wBufLen) == 0 .AND. lastError != 0 .AND. wBufLen > 0
                strMessage := STRING{message, 0, wBufLen}
            ELSE
                strMessage := "Unknown Error"
            ENDIF
            oError   := AdsError{strMessage, gencode, lastError, oRDD:Driver , ES_ERROR, Procname(1), _bagName}
            RuntimeState.LastRDDError := oError
            THROW oError
        ENDIF
        RETURN TRUE
        
        #endregion
    /// <inheritdoc />    
    CONSTRUCTOR(oArea AS WorkArea)
        SUPER(oArea)
        oRdd := oArea ASTYPE ADSRDD
        /// <inheritdoc />
    VIRTUAL METHOD OrderCondition(info AS DBORDERCONDINFO ) AS LOGIC
        SELF:OrderCondInfo := info
        RETURN TRUE
        
        /// <inheritdoc />
    VIRTUAL METHOD OrderCreate(info AS DBORDERCREATEINFO ) AS LOGIC
        LOCAL cCondition AS STRING
        LOCAL nonAdditive AS LOGIC
        LOCAL mustEval  AS LOGIC
        LOCAL mode AS DWORD
        LOCAL cTag AS STRING
        LOCAL fileNameWithoutExtension AS STRING
        LOCAL result AS DWORD
        //
        cCondition := string.Empty
        nonAdditive := TRUE
        mustEval := FALSE
        IF ! SELF:oRDD:_CheckDateFormat()
            SELF:oRDD:_CheckError(1, EG_CREATE)
        ENDIF
        mode := 0u
        IF (info:Unique)
            mode |= ACE.ADS_UNIQUE
        ENDIF
        IF (SELF:OrderCondInfo != NULL)
            cCondition := SELF:OrderCondInfo:ForExpression
            IF SELF:OrderCondInfo:Custom
                mode |= ACE.ADS_CUSTOM
            ENDIF
            IF SELF:OrderCondInfo:Descending
                mode |= ACE.ADS_DESCENDING
            ENDIF
            nonAdditive := SELF:OrderCondInfo:Additive
            mustEval    := SELF:OrderCondInfo:EvalBlock != NULL
        ENDIF
        IF (nonAdditive)
            SELF:OrderListDelete(NULL)
        ENDIF
        IF (mustEval)
            IF (SELF:_CallbackFn == NULL)
                SELF:_CallbackFn := ProgressCallback
            ENDIF
            ACE.AdsRegisterCallbackFunction(SELF:_CallbackFn, 0)
        ENDIF
        LOCAL hIndex AS IntPtr
        SELF:_bagName := info:BagName 
        IF info:Order IS STRING 
            cTag := (STRING) info:Order
            IF String.IsNullOrEmpty(SELF:_bagName)
                SELF:_bagName := cTag
            ENDIF
            mode |= ACE.ADS_COMPOUND
            result := ACE.AdsCreateIndex90(SELF:Table, info:BagName, cTag, info:Expression, cCondition, NULL, mode, 0, SELF:oRDD:_Collation, OUT hIndex)
        ELSE
            IF (SELF:oRDD:_TableType == ACE.ADS_NTX)
                result := ACE.AdsCreateIndex(SELF:Table, info:BagName, NULL, info:Expression, cCondition, NULL, mode, OUT hIndex)
            ELSE
                fileNameWithoutExtension := Path.GetFileNameWithoutExtension(info:BagName)
                mode |= ACE.ADS_COMPOUND
                result := ACE.AdsCreateIndex90(SELF:Table, info:BagName, fileNameWithoutExtension, info:Expression, cCondition, NULL, mode, 0u, SELF:oRDD:_Collation, OUT hIndex)
            ENDIF
        ENDIF
        SELF:Index := hIndex  
        IF (mustEval)
            ACE.AdsClearProgressCallback()
            SELF:_CallbackFn := NULL
        ENDIF
        SELF:_CheckError(result,EG_CREATE)
        RETURN SELF:oRdd:GoTop()
        
        /// <inheritdoc />
    VIRTUAL METHOD OrderDestroy(info AS DBORDERINFO ) AS LOGIC
        LOCAL hIndex AS IntPtr
        LOCAL orderName AS STRING
        LOCAL orderNum AS WORD
        //
        hIndex := IntPtr.Zero
        
        IF info:Order IS STRING
            orderName := (STRING) info:Order
            orderName := Path.GetFileNameWithoutExtension(orderName)
            SELF:_CheckError(ACE.AdsGetIndexHandle(SELF:Table, orderName, OUT hIndex),EG_CLOSE)
        ELSE
            TRY
                orderNum := Convert.ToUInt16(info:Order)
            CATCH
                SELF:oRDD:ADSERROR(ERDD_DATATYPE, EG_DATATYPE, "OrderDestroy")
                RETURN FALSE
            END TRY
            IF orderNum > 0
                SELF:_CheckError(ACE.AdsGetIndexHandleByOrder(SELF:Table, orderNum, OUT hIndex), EG_CLOSE)
            ENDIF
        ENDIF
        IF hIndex != IntPtr.Zero
            SELF:_CheckError(ACE.AdsDeleteIndex(hIndex), EG_CLOSE)
            IF hIndex == SELF:Index
                SELF:Index := IntPtr.Zero
            ENDIF
        ENDIF
        RETURN TRUE
        /// <inheritdoc />
    VIRTUAL METHOD OrderInfo(nOrdinal AS DWORD, info AS DbOrderInfo) AS OBJECT
        LOCAL hIndex AS IntPtr
        LOCAL result AS DWORD
        LOCAL orderNum AS WORD
        LOCAL fileName AS STRING
        LOCAL i AS LONG
        LOCAL num AS WORD
        LOCAL chars AS CHAR[]
        //
        hIndex := SELF:Index
        IF (info:Order != NULL)
            IF info:Order IS STRING VAR cOrder
                IF String.IsNullOrEmpty(cOrder)
                    result := 0
                ELSE
                    result := ACE.AdsGetIndexHandle(SELF:Table, cOrder, OUT hIndex)
                ENDIF
            ELSE
                TRY
                    orderNum := Convert.ToUInt16(info:Order)
                CATCH
                    orderNum := 0
                    SELF:oRDD:ADSERROR(ERDD_DATATYPE, EG_DATATYPE, "OrderInfo")
                END TRY
                result := ACE.AdsGetIndexHandleByOrder(SELF:Table, orderNum, OUT hIndex)
            ENDIF
            IF (result != 0)
                hIndex := IntPtr.Zero
            ENDIF
        ENDIF
        BEGIN SWITCH nOrdinal
        CASE DBOI_BAGEXT
            IF SELF:oRDD:IsADT
                info:Result :=  ".ADI"
            ELSE
                IF (SELF:oRDD:_TableType == ACE.ADS_NTX)
                    info:Result :=  ".NTX"
                ELSE
                    info:Result :=  ".CDX"
                ENDIF
            ENDIF
            
    CASE DBOI_BAGNAME
            IF hIndex == IntPtr.Zero
                info:Result := ""
            ELSE
                num := MAX_PATH+1
                chars := CHAR[]{ num}
                SELF:_CheckError(ACE.AdsGetIndexFilename(hIndex, ACE.ADS_BASENAME, chars, REF num), EG_ARG)
                info:Result := STRING{chars, 0, num}
            ENDIF
    CASE DBOI_CONDITION
            IF hIndex == IntPtr.Zero
                info:Result := ""
            ELSE
                num := ACE.ADS_MAX_INDEX_EXPR_LEN +1
                chars := CHAR[]{ num }
                SELF:_CheckError(ACE.AdsGetIndexCondition(hIndex, chars, REF num), EG_ARG)
                info:Result := STRING{chars, 0, num}
            ENDIF
            
    CASE DBOI_CUSTOM
            IF hIndex == IntPtr.Zero
                info:Result := FALSE
            ELSE
                LOCAL wIsCustom AS WORD
                SELF:_CheckError(ACE.AdsIsIndexCustom(hIndex, OUT wIsCustom), EG_ARG)
                info:Result := wIsCustom != 0
            ENDIF            
    CASE DBOI_EXPRESSION
            IF hIndex == IntPtr.Zero
                info:Result := ""
            ELSE
                num := ACE.ADS_MAX_INDEX_EXPR_LEN +1
                chars := CHAR[]{ num }
                SELF:_CheckError(ACE.AdsGetIndexExpr(hIndex, chars, REF num), EG_ARG)
                info:Result := STRING{chars, 0, num}
            ENDIF
            
    CASE DBOI_FILEHANDLE
            info:Result := NULL
            
    CASE DBOI_FULLPATH
            IF hIndex == IntPtr.Zero
                info:Result := ""
            ELSE
                num := MAX_PATH+1
                chars := CHAR[]{ num}
                SELF:_CheckError(ACE.AdsGetIndexFilename(hIndex, ACE.ADS_FULLPATHNAME, chars, REF num), EG_ARG)
                info:Result := STRING{chars, 0, num}
            ENDIF
            
    CASE DBOI_HPLOCKING
            info:Result:= NULL
            
    CASE DBOI_ISCOND
            IF hIndex == IntPtr.Zero
                info:Result := FALSE
            ELSE
                num := ACE.ADS_MAX_INDEX_EXPR_LEN +1
                chars := CHAR[]{ num }
                SELF:_CheckError(ACE.AdsGetIndexCondition(hIndex, chars, REF num), EG_ARG)
                info:Result:= num > 0
            ENDIF
            
    CASE DBOI_ISDESC
            IF hIndex == IntPtr.Zero
                info:Result := FALSE
            ELSE
                LOCAL wDescending AS WORD
                SELF:_CheckError(ACE.AdsIsIndexDescending(hIndex, OUT wDescending), EG_ARG)
                info:Result:= wDescending > 0
            ENDIF
            
    CASE DBOI_KEYADD
            IF hIndex == IntPtr.Zero
                info:Result := FALSE
            ELSE
                SELF:_CheckError(ACE.AdsAddCustomKey(hIndex), EG_CORRUPTION)
                info:Result := TRUE
            ENDIF    
    CASE DBOI_KEYCOUNT
            IF hIndex == IntPtr.Zero
                info:Result:= 0
            ELSE
                LOCAL wOptLevel AS WORD
                num := 0
                result := ACE.AdsGetAOFOptLevel(SELF:Table, OUT wOptLevel, NULL, REF num)
                IF result == ACE.AE_NO_FILTER
                    wOptLevel := ACE.ADS_OPTIMIZED_NONE
                ELSEIF result != 0
                    SELF:_CheckError(result, EG_CORRUPTION)
                ENDIF
                LOCAL dwCount AS DWORD
                IF (wOptLevel != ACE.ADS_OPTIMIZED_FULL)
                    SELF:_CheckError(ACE.AdsGetKeyCount(hIndex, ACE.ADS_RESPECTFILTERS, OUT dwCount), EG_CORRUPTION)
                ELSE
                    SELF:_CheckError(ACE.AdsGetRecordCount(SELF:Table, ACE.ADS_RESPECTFILTERS, OUT dwCount), EG_CORRUPTION)
                ENDIF
                info:Result := dwCount
            ENDIF
            
    CASE DBOI_KEYDEC
    CASE DBOI_KEYSINCLUDED
    CASE DBOI_KEYNORAW
    CASE DBOI_LOCKOFFSET
    CASE DBOI_SETCODEBLOCK
    CASE DBOI_KEYCOUNTRAW
            info:Result := NULL
            
    CASE DBOI_KEYDELETE
            IF hIndex == IntPtr.Zero
                info:Result := FALSE
            ELSE
                SELF:_CheckError(ACE.AdsDeleteCustomKey(hIndex),EG_CORRUPTION)
                info:Result := TRUE
            ENDIF
            
    CASE DBOI_KEYSIZE 
            IF hIndex == IntPtr.Zero
                info:Result := 0
            ELSE
                SELF:_CheckError(ACE.AdsGetKeyLength(hIndex, OUT num),EG_CORRUPTION) 
                info:Result := (LONG)num
            ENDIF
            
    CASE DBOI_KEYTYPE
            IF hIndex == IntPtr.Zero
                info:Result := TypeCode.Empty
            ELSE
                SELF:_CheckError(ACE.AdsGetKeyType(hIndex, OUT num),EG_CORRUPTION)
                    BEGIN SWITCH num
                CASE ACE.ADS_STRING
                    info:Result  := TypeCode.String
                CASE ACE.ADS_NUMERIC
                    info:Result  := TypeCode.Double
                CASE ACE.ADS_DATE
                    info:Result  := TypeCode.DateTime
                CASE ACE.ADS_LOGICAL
                    info:Result  := TypeCode.Boolean
                CASE ACE.ADS_RAW
                    info:Result  := TypeCode.Byte
                OTHERWISE
                    SELF:oRDD:ADSERROR(ERDD_UNSUPPORTED, EG_UNKNOWN, "OrderInfo")
                    END SWITCH
            ENDIF

    CASE DBOI_KEYVAL
            IF hIndex == IntPtr.Zero
                info:Result := ""
            ELSE
                num   := SELF:oRDD:_MaxKeySize + 1
                chars := CHAR[]{num}
            
                SELF:_CheckError(ACE.AdsExtractKey(hIndex, chars, REF num), EG_CORRUPTION)
                info:Result  := STRING{chars, 0, num}
            ENDIF
            
    CASE DBOI_NAME
            IF hIndex == IntPtr.Zero
                info:Result := ""
            ELSE
                num  := MAX_PATH + 1
                chars := CHAR[]{num}
            
                SELF:_CheckError(ACE.AdsGetIndexName(hIndex, chars, REF num), EG_CORRUPTION)
                info:Result := STRING{chars, 0, num}
            ENDIF
            
    CASE DBOI_NUMBER
            IF hIndex == IntPtr.Zero
                info:Result  := 0
            ELSE
                SELF:_CheckError(ACE.AdsGetIndexOrderByHandle(hIndex, OUT num), EG_CORRUPTION)
                info:Result := (LONG)num
            ENDIF
    CASE DBOI_ORDERCOUNT
            IF !string.IsNullOrEmpty(info:BagName)
                LOCAL numindices := 3840 AS WORD
                VAR indices := IntPtr[]{ numindices }
                chars  := CHAR[]{ _MAX_PATH }
                fileName := Path.GetFileName(info:BagName)
                SELF:_CheckError(ACE.AdsGetAllIndexes(SELF:Table, indices, REF numindices), EG_CORRUPTION)
                VAR count := 0L
                FOR i := 0 TO numindices -1 
                    num := (WORD) chars:Length
                    SELF:_CheckError(ACE.AdsGetIndexFilename(indices[i], ACE.ADS_BASENAMEANDEXT, chars, REF num), EG_CORRUPTION)
                    IF string.Compare(STRING{chars, 0, num}, fileName, TRUE) == 0
                        count++
                    ENDIF
                NEXT
                info:Result := count
            ELSE
                SELF:_CheckError(ACE.AdsGetNumIndexes(SELF:Table, OUT num), EG_CORRUPTION)
                info:Result  := (LONG)num
        ENDIF
    CASE DBOI_POSITION
    CASE DBOI_RECNO
            LOCAL dwKeyNo AS DWORD
            IF hIndex == IntPtr.Zero
                info:Result  := 0
            ELSE
                IF AX_SetExactKeyPos()
                    SELF:_CheckError(ACE.AdsGetKeyNum(hIndex, 1, OUT dwKeyNo), EG_CORRUPTION)
                    info:Result  := dwKeyNo
                ELSE
                    LOCAL dwCount AS DWORD
                    LOCAL r8Pos AS REAL8
                    SELF:_CheckError(ACE.AdsGetRecordCount(SELF:Table, ACE.ADS_IGNOREFILTERS, OUT dwCount), EG_CORRUPTION)
                    SELF:_CheckError(ACE.AdsGetRelKeyPos(hIndex, OUT r8Pos), EG_CORRUPTION)
                    info:Result := (DWORD) (r8Pos  * (REAL8) dwCount)
                ENDIF
            ENDIF            
    CASE DBOI_SCOPETOP
    CASE DBOI_SCOPEBOTTOM
            IF hIndex == IntPtr.Zero
                info:Result  := NULL
            ELSE
                LOCAL wScopeOption AS WORD
                wScopeOption  := IIF(nOrdinal != DBOI_SCOPEBOTTOM , 1 , 2)
                IF (info:Result != NULL)
                    RETURN SELF:oRDD:_SetScope(hIndex, wScopeOption, info:Result)
                ENDIF
                num     := SELF:oRDD:_MaxKeySize +1
                chars   := CHAR[]{ SELF:oRDD:_MaxKeySize +1}
                result := ACE.AdsGetScope(hIndex, wScopeOption, chars, REF num)
                IF result != ACE.AE_NO_SCOPE .AND. result != 0
                    SELF:_CheckError(result, EG_ARG)
                ENDIF
                info:Result := SELF:oRDD:_Ansi2Unicode(chars, num)
           ENDIF            
    CASE DBOI_SCOPETOPCLEAR
    CASE DBOI_SCOPEBOTTOMCLEAR
            IF hIndex == IntPtr.Zero
                info:Result  := FALSE
            ELSE
                LOCAL wScopeOption AS WORD
                wScopeOption  := IIF(nOrdinal != DBOI_SCOPEBOTTOMCLEAR , 1 , 2)
                info:Result := NULL
                RETURN SELF:oRDD:_SetScope(hIndex, wScopeOption, info:Result)
            ENDIF            
    CASE DBOI_SKIPUNIQUE
           IF hIndex == IntPtr.Zero
                info:Result  := FALSE
            ELSE

                IF (info:Result != NULL)
                    SELF:_CheckError(ACE.AdsSkipUnique(hIndex, (LONG)info:Result ), EG_CORRUPTION)
                ELSE
                    SELF:_CheckError(ACE.AdsSkipUnique(hIndex, 1), EG_CORRUPTION)
                ENDIF
                info:Result  := SELF:oRDD:RecordMovement()
            ENDIF
    CASE DBOI_UNIQUE
           IF hIndex == IntPtr.Zero
                info:Result  := FALSE
            ELSE
                SELF:_CheckError(ACE.AdsIsIndexUnique(hIndex, OUT num), EG_CORRUPTION)
                info:Result := num != 0
            ENDIF    
    CASE DBOI_AXS_PERCENT_INDEXED
            info:Result := SELF:_iProgress
            
    CASE DBOI_GET_ACE_INDEX_HANDLE
            info:Result := hIndex
            
    OTHERWISE
            SELF:oRDD:ADSERROR(ERDD_UNSUPPORTED, EG_UNSUPPORTED, "OrderInfo")
        RETURN FALSE
        END SWITCH
        RETURN TRUE
        
        
        
        
        
        
    /// <inheritdoc />
    VIRTUAL METHOD OrderListAdd(info AS DBORDERINFO  ) AS LOGIC
        LOCAL wLength AS WORD
        LOCAL wCurrent AS WORD
        LOCAL indices AS IntPtr[]
        LOCAL result AS DWORD
        
        LOCAL i AS LONG
        LOCAL numIndexes AS WORD
        LOCAL cIndexName AS STRING
        IF (SELF:oRDD:_SetPaths() != 0)
            RETURN FALSE
        ENDIF
        SELF:oRDD:_CheckDateFormat()
        SELF:_CheckError(ACE.AdsGetNumIndexes(SELF:Table, OUT numIndexes), EG_OPEN)
        wLength := 1000
        indices := IntPtr[]{ wLength }
        cIndexName := info:BagName
        SELF:_bagName := info:BagName
        LOCAL tries := 0 as LONG
        REPEAT
            // wait 100 ms before retrying
            IF tries > 0
                System.Threading.Thread.Sleep(100)
            ENDIF
            tries += 1
            result := ACE.AdsOpenIndex(SELF:Table, cIndexName, indices, REF wLength)
            IF result == ACE.AE_INDEX_ALREADY_OPEN 
                cIndexName := Path.GetFileNameWithoutExtension(cIndexName)
                LOCAL pathName AS CHAR[]
                pathName := CHAR[]{ MAX_PATH+1 }
                FOR i := 0 TO wLength -1
                    VAR wLen := (WORD)pathName:Length
                    SELF:_CheckError(ACE.AdsGetIndexFilename(indices[i], ACE.ADS_BASENAME, pathName, REF wLen),EG_OPEN)
                    IF String.Compare(cIndexName, String{pathName, 0, wLen}, TRUE) == 0
                        SELF:Index := indices[i]
                        EXIT
                    ENDIF
                NEXT
            ELSEIF result == ACE.AE_SUCCESS
                // Only select the current index when we do not already have an index activated.
                // So opening the second index file will keep focus on the first index
                IF SELF:Index == IntPtr.Zero .AND. wLength > 0
                    wCurrent := numIndexes +1
                    LOCAL hIndex AS IntPtr
                    SELF:_CheckError(ACE.AdsGetIndexHandleByOrder(SELF:Table, wCurrent, OUT hIndex ),EG_OPEN)
                    SELF:Index := hIndex
                ENDIF
            ENDIF
        UNTIL tries == 10 .or. SELF:Index != IntPtr.Zero
        SELF:_CheckError(result,EG_OPEN)
        RETURN SELF:oRDD:GoTop()
        
        /// <inheritdoc />
    VIRTUAL METHOD OrderListDelete(info AS DBORDERINFO ) AS LOGIC
        SELF:_CheckError(ACE.AdsCloseAllIndexes(SELF:Table),EG_CLOSE)
        SELF:Index := IntPtr.Zero
        RETURN TRUE
        
        /// <inheritdoc />
    VIRTUAL METHOD OrderListFocus(info AS DBORDERINFO ) AS LOGIC
        LOCAL oTmpInfo AS DbOrderInfo
        oTmpInfo :=DbOrderInfo{}
        SELF:OrderInfo(DBOI_NAME,oTmpInfo) 
        info:Result := oTmpInfo:Result
        IF (info:Order == NULL)
            SELF:Index := IntPtr.Zero
        ELSEIF info:Order IS STRING VAR orderName
            IF ! String.IsNullOrEmpty(orderName)
                orderName := Path.GetFileNameWithoutExtension(orderName)
                LOCAL hIndex AS IntPtr
                SELF:_CheckError(ACE.AdsGetIndexHandle(SELF:Table, orderName, OUT hIndex),EG_ARG)
                SELF:Index := hIndex
                SELF:oRDD:RecordMovement()
            ENDIF
            SELF:Index := IntPtr.Zero
        ELSE
            LOCAL orderNum := 0 AS WORD
            TRY
                orderNum := Convert.ToUInt16(info:Order)
            CATCH
                SELF:oRDD:ADSERROR(ERDD_DATATYPE, EG_DATATYPE, "OderListFocus")
            END TRY
            IF orderNum > 0
                LOCAL hIndex AS IntPtr
                SELF:_CheckError(ACE.AdsGetIndexHandleByOrder(SELF:Table, orderNum, OUT hIndex),EG_ARG)
                SELF:Index := hIndex
                SELF:oRDD:RecordMovement()
            ELSE
                SELF:Index := IntPtr.Zero
            ENDIF
        ENDIF
        RETURN TRUE
        
        
        /// <inheritdoc />
    VIRTUAL METHOD OrderListRebuild() AS LOGIC
        LOCAL options AS DWORD
        SELF:_CheckError(ACE.AdsGetTableOpenOptions(SELF:Table, OUT options),EG_SHARED)
        IF !AdsRDD._HasFlag(options, ACE.ADS_EXCLUSIVE)
            SELF:oRDD:ADSERROR(ACE.AE_TABLE_NOT_EXCLUSIVE, EG_SHARED, "OrderListRebuild")
            RETURN FALSE
        ENDIF
        SELF:_CheckError(ACE.AdsReindex(SELF:Table),EG_CREATE)
        RETURN SELF:oRDD:GoTop()
        
        
        
        /// <inheritdoc />
    VIRTUAL METHOD Seek(seekinfo AS DBSEEKINFO) AS LOGIC
        LOCAL seekMode AS WORD
        LOCAL found AS WORD
        LOCAL Key AS STRING
        IF SELF:Index == System.IntPtr.Zero
            SELF:oRDD:ADSERROR(ERDD.DATATYPE, XSharp.Gencode.EG_NOORDER, "Seek")
        ENDIF
        SELF:_CheckError(SELF:oRDD:_CheckVODeletedFlag(),EG_CORRUPTION)
        Key := seekinfo:value:ToString()
        seekMode := IIF (seekinfo:SoftSeek, ACE.ADS_SOFTSEEK, ACE.ADS_HARDSEEK)
        IF seekInfo:Last
            SELF:_CheckError(ACE.AdsSeekLast(SELF:Index, Key, (WORD)Key:Length , ACE.ADS_STRINGKEY, OUT found),EG_CORRUPTION)
            IF found== 0 .AND. seekinfo:SoftSeek  
                SELF:_CheckError(ACE.AdsSeek(SELF:Index, Key, (WORD)Key:Length , ACE.ADS_STRINGKEY, seekMode, OUT found),EG_CORRUPTION)
            ENDIF
        ELSE
            SELF:_CheckError(ACE.AdsSeek(SELF:Index, Key, (WORD)Key:Length , ACE.ADS_STRINGKEY, seekMode, OUT found),EG_CORRUPTION)
        ENDIF
        LOCAL atEOF AS WORD
        LOCAL atBOF AS WORD
        SELF:_CheckError(ACE.AdsAtEOF(SELF:Table, OUT atEOF),EG_CORRUPTION)
        SELF:_CheckError(ACE.AdsAtBOF(SELF:Table, OUT atBOF),EG_CORRUPTION)
        oRDD:_Found := found != 0
        oRDD:_Eof := atEOF != 0
        oRDD:_Bof := atBOF != 0
        LOCAL dwRecord AS DWORD
        VAR result := ACE.AdsGetRecordNum(SELF:Table, ACE.ADS_IGNOREFILTERS, OUT dwRecord)
        IF result == ACE.AE_NO_CURRENT_RECORD
            SELF:oRDD:_Recno := 0
        ELSEIF result == 0
            SELF:oRDD:_Recno := dwRecord
        ELSE
            SELF:_CheckError(result,EG_CORRUPTION)
        ENDIF
        RETURN TRUE
    INTERNAL METHOD ProgressCallback(usPercentDone AS WORD , ulCallbackID AS DWORD ) AS DWORD
        IF (usPercentDone > 100)
            SELF:_iProgress := 100
        ELSE
            SELF:_iProgress := usPercentDone
        ENDIF
        IF SELF:OrderCondInfo != NULL .AND. SELF:OrderCondInfo:EvalBlock != NULL
            SELF:OrderCondInfo:EvalBlock:EvalBlock(SELF:_iProgress)
        ENDIF
        RETURN 0u        
END CLASS
