//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


USING System
USING System.Collections.Generic
USING System.Text
USING XSharp
USING XSharp.RDD
USING XSharp.RDD.Support
USING System.IO
/// <summary>Advantage Index support.</summary>
CLASS XSharp.ADS.ADSIndex INHERIT BaseIndex
    PRIVATE oRDD AS ADSRDD
    PRIVATE _CallbackFn AS CallbackFn
    PRIVATE _iProgress AS LONG
    #region Helpers

    PROPERTY OrderCondInfo AS DBORDERCONDINFO GET oRDD:_OrderCondInfo SET oRDD:_OrderCondInfo := VALUE
    PROPERTY Index AS IntPtr GET oRDD:_Index SET oRDD:_Index := VALUE
    PROPERTY Table AS IntPtr GET oRDD:_Table
    
    PRIVATE METHOD ACECall(nResult AS DWORD) AS LOGIC
        RETURN SELF:oRDD:AceCall(nResult)
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
        IF ! SELF:oRDD:_CheckVODateFormat()
            SELF:oRDD:ACECALL(1)
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
        IF info:Order IS STRING 
            cTag := (STRING) info:Order
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
        SELF:ACECALL(result)
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
            SELF:ACECALL(ACE.AdsGetIndexHandle(SELF:Table, orderName, OUT hIndex))
        ELSE
            TRY
                orderNum := Convert.ToUInt16(info:Order)
            CATCH
                SELF:oRDD:ADSERROR(ERDD_DATATYPE, EG_DATATYPE, "OrderDestroy")
                RETURN FALSE
            END TRY
            IF orderNum > 0
                SELF:ACECALL(ACE.AdsGetIndexHandleByOrder(SELF:Table, orderNum, OUT hIndex))
            ENDIF
        ENDIF
        IF hIndex != IntPtr.Zero
            SELF:ACECALL(ACE.AdsDeleteIndex(hIndex))
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
      LOCAL len AS WORD
      LOCAL chars AS CHAR[]
      //
      hIndex := SELF:Index
      IF (info:Order != NULL)
        IF info:Order IS STRING
          VAR cOrder := (STRING) info:Order
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
        len := MAX_PATH+1
        chars := CHAR[]{ len}
        SELF:ACECALL(ACE.AdsGetIndexFilename(hIndex, ACE.ADS_BASENAME, chars, REF len))
        info:Result := STRING{chars, 0, len}
        
    CASE DBOI_CONDITION
        len := ACE.ADS_MAX_INDEX_EXPR_LEN +1
        chars := CHAR[]{ len }
        SELF:ACECALL(ACE.AdsGetIndexCondition(hIndex, chars, REF len))
        info:Result := STRING{chars, 0, len}
        
    CASE DBOI_CUSTOM
        LOCAL wIsCustom AS WORD
        SELF:ACECALL(ACE.AdsIsIndexCustom(hIndex, OUT wIsCustom))
        info:Result := wIsCustom != 0
        
    CASE DBOI_EXPRESSION
        len := ACE.ADS_MAX_INDEX_EXPR_LEN +1
        chars := CHAR[]{ len }
        SELF:ACECALL(ACE.AdsGetIndexExpr(hIndex, chars, REF len))
        info:Result := STRING{chars, 0, len}
        
    CASE DBOI_FILEHANDLE
        info:Result := NULL
        
    CASE DBOI_FULLPATH
        len := MAX_PATH+1
        chars := CHAR[]{ len}
        SELF:ACECALL(ACE.AdsGetIndexFilename(hIndex, ACE.ADS_FULLPATHNAME, chars, REF len))
        info:Result := STRING{chars, 0, len}
        
    CASE DBOI_HPLOCKING
        info:Result:= NULL
        
    CASE DBOI_ISCOND
        len := ACE.ADS_MAX_INDEX_EXPR_LEN +1
        chars := CHAR[]{ len }
        SELF:ACECALL(ACE.AdsGetIndexCondition(hIndex, chars, REF len))
        info:Result:= len > 0
        
    CASE DBOI_ISDESC
        LOCAL wDescending AS WORD
        SELF:ACECALL(ACE.AdsIsIndexDescending(hIndex, OUT wDescending))
        info:Result:= wDescending > 0
        
    CASE DBOI_KEYADD
        SELF:ACECALL(ACE.AdsAddCustomKey(hIndex))
        info:Result := TRUE
        
    CASE DBOI_KEYCOUNT
        IF hIndex == IntPtr.Zero
          info:Result:= 0
        ELSE
          LOCAL wOptLevel AS WORD
          len := 0
          result := ACE.AdsGetAOFOptLevel(SELF:Table, OUT wOptLevel, NULL, REF len)
          IF result == ACE.AE_NO_FILTER
            wOptLevel := ACE.ADS_OPTIMIZED_NONE
          ELSEIF result != 0
            SELF:ACECALL(result)
          ENDIF
          LOCAL dwCount AS DWORD
          IF (wOptLevel != ACE.ADS_OPTIMIZED_FULL)
            SELF:ACECALL(ACE.AdsGetKeyCount(hIndex, ACE.ADS_RESPECTFILTERS, OUT dwCount))
          ELSE
            SELF:ACECALL(ACE.AdsGetRecordCount(SELF:Table, ACE.ADS_RESPECTFILTERS, OUT dwCount))
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
        SELF:ACECALL(ACE.AdsDeleteCustomKey(hIndex))
        info:Result := TRUE
        
    CASE DBOI_KEYSIZE 
        SELF:ACECALL(ACE.AdsGetKeyLength(hIndex, OUT len)) 
        info:Result := (LONG)len
        
    CASE DBOI_KEYTYPE
        SELF:ACECALL(ACE.AdsGetKeyType(hIndex, OUT len))
      BEGIN SWITCH len
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
  CASE DBOI_KEYVAL
      len  := SELF:oRDD:_MaxKeySize + 1
      chars := CHAR[]{len}
      
      SELF:ACECALL(ACE.AdsExtractKey(hIndex, chars, REF len))
      info:Result  := STRING{chars, 0, len}
      
  CASE DBOI_NAME
      len  := MAX_PATH + 1
      chars := CHAR[]{len}
      
      SELF:ACECALL(ACE.AdsGetIndexName(hIndex, chars, REF len))
      info:Result := STRING{chars, 0, len}
      
  CASE DBOI_NUMBER
      IF (hIndex == IntPtr.Zero)
        info:Result  := 0
      ELSE
        SELF:ACECALL(ACE.AdsGetIndexOrderByHandle(hIndex, OUT len))
        info:Result := (LONG)len
    ENDIF
  CASE DBOI_ORDERCOUNT
      IF !string.IsNullOrEmpty(info:BagName)
        LOCAL numindices := 3840 AS WORD
        VAR indices := IntPtr[]{ numindices }
        chars  := CHAR[]{ _MAX_PATH }
        fileName := Path.GetFileName(info:BagName)
        SELF:ACECALL(ACE.AdsGetAllIndexes(SELF:Table, indices, REF numindices))
        VAR count := 0L
        FOR i := 0 TO numindices -1 
          len := (WORD)chars:Length
          SELF:ACECALL(ACE.AdsGetIndexFilename(indices[i], ACE.ADS_BASENAMEANDEXT, chars, REF len))
          IF string.Compare(STRING{chars, 0, len}, fileName, TRUE) == 0
            count++
          ENDIF
        NEXT
        info:Result := count
      ELSE
        SELF:ACECALL(ACE.AdsGetNumIndexes(SELF:Table, OUT len))
        info:Result  := (LONG)len
    ENDIF
  CASE DBOI_POSITION
  CASE DBOI_RECNO
      LOCAL dwKeyNo AS DWORD
      IF AX_SetExactKeyPos()
        SELF:ACECALL(ACE.AdsGetKeyNum(hIndex, 1, OUT dwKeyNo))
        info:Result  := dwKeyNo
      ELSE
        LOCAL dwCount AS DWORD
        LOCAL r8Pos AS REAL8
        SELF:ACECALL(ACE.AdsGetRecordCount(SELF:Table, ACE.ADS_IGNOREFILTERS, OUT dwCount))
        SELF:ACECALL(ACE.AdsGetRelKeyPos(hIndex, OUT r8Pos))
        info:Result := (DWORD) (r8Pos  * (REAL8) dwCount)
      ENDIF
      
  CASE DBOI_SCOPETOP
    CASE DBOI_SCOPEBOTTOM
        LOCAL wScopeOption AS WORD
        wScopeOption  := IIF(nOrdinal != DBOI_SCOPEBOTTOM , 1 , 2)
        IF (info:Result != NULL)
          RETURN SELF:oRDD:_SetScope(hIndex, wScopeOption, info:Result)
        ENDIF
        len := SELF:oRDD:_MaxKeySize +1
        chars := CHAR[]{ SELF:oRDD:_MaxKeySize +1}
        result := ACE.AdsGetScope(hIndex, wScopeOption, chars, REF len)
        IF result != ACE.AE_NO_SCOPE .AND. result != 0
          SELF:ACECALL(result)
        ENDIF
        info:Result := SELF:oRDD:_Ansi2Unicode(chars, len)
        
    CASE DBOI_SCOPETOPCLEAR
    CASE DBOI_SCOPEBOTTOMCLEAR
        LOCAL wScopeOption AS WORD
        wScopeOption  := IIF(nOrdinal != DBOI_SCOPEBOTTOMCLEAR , 1 , 2)
        info:Result := NULL
        RETURN SELF:oRDD:_SetScope(hIndex, wScopeOption, info:Result)
        
    CASE DBOI_SKIPUNIQUE
        IF (info:Result != NULL)
          SELF:ACECALL(ACE.AdsSkipUnique(hIndex, (LONG)info:Result ))
        ELSE
          SELF:ACECALL(ACE.AdsSkipUnique(hIndex, 1))
        ENDIF
      RETURN SELF:oRDD:RecordMovement()
    CASE DBOI_UNIQUE
        SELF:ACECALL(ACE.AdsIsIndexUnique(hIndex, OUT len))
        info:Result := len != 0
        
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
        SELF:oRDD:_CheckVODateFormat()
        SELF:ACECALL(ACE.AdsGetNumIndexes(SELF:Table, OUT numIndexes))
        wLength := 1000
        indices := IntPtr[]{ wLength }
        cIndexName := info:BagName
        result := ACE.AdsOpenIndex(SELF:Table, cIndexName, indices, REF wLength)
        IF result != ACE.AE_INDEX_ALREADY_OPEN 
            SELF:ACECALL(result)
            IF SELF:Index == IntPtr.Zero .AND. wLength > 0
                wCurrent := numIndexes +1
                LOCAL hIndex AS IntPtr
                SELF:ACECALL(ACE.AdsGetIndexHandleByOrder(SELF:Table, wCurrent, OUT hIndex))
                SELF:Index := hIndex
            ENDIF
        ELSE
            cIndexName := Path.GetFileNameWithoutExtension(cIndexName)
            LOCAL pathName AS CHAR[]
            pathName := CHAR[]{ MAX_PATH+1 }
            FOR i := 0 TO wLength -1
                VAR wLen := (WORD)pathName:Length
                SELF:ACECALL(ACE.AdsGetIndexFilename(indices[i], 1, pathName, REF wLen))
                IF string.Compare(info:BagName, STRING{pathName, 0, wLen}, TRUE) == 0
                    SELF:Index := indices[i]
                    EXIT
                ENDIF
            NEXT
        ENDIF
        RETURN SELF:oRDD:GoTop()
        
        /// <inheritdoc />
    VIRTUAL METHOD OrderListDelete(info AS DBORDERINFO ) AS LOGIC
        SELF:ACECALL(ACE.AdsCloseAllIndexes(SELF:Table))
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
        ELSEIF info:Order IS STRING
            VAR orderName := (STRING) info:Order
            IF ! String.IsNullOrEmpty(orderName)
                orderName := Path.GetFileNameWithoutExtension(orderName)
                LOCAL hIndex AS IntPtr
                SELF:ACECALL(ACE.AdsGetIndexHandle(SELF:Table, orderName, OUT hIndex))
                SELF:Index := hIndex
                RETURN SELF:oRDD:GoTop()
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
                SELF:ACECALL(ACE.AdsGetIndexHandleByOrder(SELF:Table, orderNum, OUT hIndex))
                SELF:Index := hIndex
                RETURN SELF:oRDD:GoTop()
            ELSE
                SELF:Index := IntPtr.Zero
            ENDIF
        ENDIF
        RETURN TRUE
        
        
        /// <inheritdoc />
    VIRTUAL METHOD OrderListRebuild() AS LOGIC
        LOCAL options AS DWORD
        SELF:ACECALL(ACE.AdsGetTableOpenOptions(SELF:Table, OUT options))
        IF !AdsRDD._HasFlag(options, ACE.ADS_EXCLUSIVE)
            SELF:oRDD:ADSERROR(ACE.AE_TABLE_NOT_EXCLUSIVE, EG_SHARED, "OrderListRebuild")
            RETURN FALSE
        ENDIF
        SELF:ACECALL(ACE.AdsReindex(SELF:Table))
        RETURN SELF:oRDD:GoTop()
        
        
        
        /// <inheritdoc />
  VIRTUAL METHOD Seek(seekinfo AS DBSEEKINFO) AS LOGIC
    LOCAL seekMode AS WORD
    LOCAL found AS WORD
    LOCAL Key AS STRING
    IF SELF:Index == System.IntPtr.Zero
      SELF:oRDD:ADSERROR(ERDD.DATATYPE, XSharp.Gencode.EG_NOORDER, "Seek")
    ENDIF
    SELF:ACECALL(SELF:oRDD:_CheckVODeletedFlag())
    Key := seekinfo:value:ToString()
    seekMode := IIF (seekinfo:SoftSeek, ACE.ADS_SOFTSEEK, ACE.ADS_HARDSEEK)
    IF seekInfo:Last
      SELF:ACECALL(ACE.AdsSeekLast(SELF:Index, Key, (WORD)Key:Length , ACE.ADS_STRINGKEY, OUT found))
      IF found== 0 .AND. seekinfo:SoftSeek  
        SELF:ACECALL(ACE.AdsSeek(SELF:Index, Key, (WORD)Key:Length , ACE.ADS_STRINGKEY, seekMode, OUT found))
      ENDIF
    ELSE
      SELF:ACECALL(ACE.AdsSeek(SELF:Index, Key, (WORD)Key:Length , ACE.ADS_STRINGKEY, seekMode, OUT found))
    ENDIF
    LOCAL atEOF AS WORD
    LOCAL atBOF AS WORD
    SELF:ACECALL(ACE.AdsAtEOF(SELF:Table, OUT atEOF))
    SELF:ACECALL(ACE.AdsAtBOF(SELF:Table, OUT atBOF))
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
      SELF:ACECALL(result)
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
