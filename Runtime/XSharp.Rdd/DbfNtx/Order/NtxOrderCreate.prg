//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System
USING System.Collections
USING System.Collections.Generic
USING System.Diagnostics
USING System.Globalization
USING System.IO
USING System.Reflection
USING System.Text
USING System.Threading
USING XSharp.RDD.Enums
USING XSharp.RDD.Support

BEGIN NAMESPACE XSharp.RDD.NTX

    INTERNAL DELEGATE ValueBlock( sourceIndex AS LONG, byteArray AS BYTE[]) AS LOGIC
    
    INTERNAL PARTIAL SEALED CLASS NtxOrder
    
        // Methods for Creating Indices
        PUBLIC METHOD Create(createInfo AS DBORDERCREATEINFO ) AS LOGIC
            LOCAL ordCondInfo AS DBORDERCONDINFO
            LOCAL isOk AS LOGIC
            LOCAL orderInfo AS DBORDERINFO
            LOCAL hasForCond AS LOGIC
            LOCAL num AS WORD
            
            ordCondInfo := SELF:_oRdd:_OrderCondInfo
            IF string.IsNullOrEmpty(createInfo:BagName)
                SELF:_oRDD:_dbfError(  SubCodes.EDB_CREATEINDEX, GenCode.EG_ARG,"OrdCreate", "Missing Orderbag Name")
                RETURN FALSE
            ENDIF
            isOk := SELF:_oRdd:GoCold()
            orderInfo := DBORDERINFO{}
            IF !ordCondInfo:Scoped
                orderInfo:AllTags := TRUE
                SELF:_oRdd:OrderListDelete(orderInfo)
            ENDIF
            SELF:_hFile := F_ERROR
            IF ordCondInfo:ForBlock != NULL
                hasForCond := TRUE
                SELF:_ForCodeBlock := ordCondInfo:ForBlock
            ELSE
                hasForCond := FALSE
            ENDIF
            SELF:_KeyExpr := createInfo:Expression
            IF createInfo:Block != NULL
                SELF:_KeyCodeBlock := createInfo:Block
            ENDIF
            IF ordCondInfo != NULL .AND. ordCondInfo:ForExpression != NULL
                SELF:_ForExpr := ordCondInfo:ForExpression
            ELSE
                SELF:_ForExpr := string.Empty
            ENDIF
            SELF:_oRdd:__Goto(1)
            IF ! SELF:EvaluateExpressions()
                RETURN FALSE
            ENDIF
            SELF:_orderName := (STRING)createInfo:Order
            IF string.IsNullOrEmpty(SELF:_orderName)
                SELF:_orderName := Path.GetFileNameWithoutExtension(createInfo:BagName)
            ENDIF
            IF !isOk .OR. SELF:_keySize == 0
                SELF:Close()
                SELF:_oRdd:_dbfError( SubCodes.ERDD_NULLKEY, GenCode.EG_DATAWIDTH,createInfo:BagName)
                RETURN FALSE
            ENDIF
            IF SELF:_keySize > 0
                SELF:_currentvalue   := RddKeyData{_KeySize}
                SELF:_newvalue       := RddKeyData{_KeySize}
            ENDIF
            
            // 8 Bytes : PrevPage (4 bytes) + Recno (4 bytes)
            SELF:_entrySize := SELF:_keySize +  8
            
            num := (  ( BUFF_SIZE - 4) / (SELF:_keySize + 10))
            SELF:_halfPage :=  (num - 1) / 2
            SELF:_MaxEntry := SELF:_halfPage * 2
            SELF:_firstPageOffset := BUFF_SIZE
            SELF:_fileSize := 0
            SELF:_nextUnusedPageOffset := 0
            SELF:_indexVersion := 1
            SELF:_Shared := FALSE
            SELF:_Hot := TRUE
            SELF:_TopStack := 0
            SELF:_Unique := createInfo:Unique
            SELF:_Conditional := FALSE
            SELF:_Descending := FALSE
            SELF:_writeLocks := 0
            SELF:_Partial := ordCondInfo:Scoped
            SELF:_HPLocking := FALSE
            IF ordCondInfo:Active
                SELF:_Descending := ordCondInfo:Descending
                IF hasForCond .AND. !string.IsNullOrEmpty(ordCondInfo:ForExpression)
                    SELF:_Conditional := TRUE
                ENDIF
            ENDIF
            SELF:fileName := createInfo:BagName
            
            TRY
                SELF:_hFile    := FCreate( SELF:FullPath) 
                IF SELF:_hFile != F_ERROR 
                    FClose( SELF:_hFile )
                ENDIF
                SELF:_hFile := F_ERROR
            CATCH
                SELF:Close()
                SELF:_oRdd:_dbfError( SubCodes.ERDD_CREATE_ORDER, GenCode.EG_CREATE,createInfo:BagName)
                RETURN FALSE
            END TRY
            // To create an index we want to open the NTX NOT shared and NOT readonly
            LOCAL openInfo AS DbOpenInfo
            openInfo := SELF:_oRdd:_OpenInfo:Clone()
            openInfo:Shared   := FALSE
            openInfo:ReadOnly := FALSE
            SELF:_hFile    := Fopen(SELF:FullPath, openInfo:FileMode)
            
            IF SELF:_hFile == F_ERROR
                SELF:Close()
                SELF:_oRdd:_dbfError( SubCodes.ERDD_CREATE_ORDER, GenCode.EG_CREATE, createInfo:BagName)
                RETURN FALSE
            ENDIF
            TRY
                IF !SELF:Shared
                    FConvertToMemoryStream(SELF:_hFile)
                ENDIF
                SELF:_PageList := NtxPageList{SELF}
            
                SELF:_midItem                       := NtxNode{SELF:_keySize}
                SELF:_oneItem                       := NtxNode{SELF:_keySize}
                SELF:_maxLockTries  := 99 //(LONG)XSharp.RuntimeState.LockTries
                SELF:_tagNumber     := 1
                IF  XSharp.RuntimeState.NewIndexLock 
                    SELF:_lockOffset := LOCKOFFSET_NEW
                ELSE
                    SELF:_lockOffset := LOCKOFFSET_OLD
                ENDIF
                IF  XSharp.RuntimeState.HPLocking
                    SELF:_HPLocking := TRUE
                ENDIF
                IF !SELF:_HeaderCreate(ordCondInfo:Scoped)
                    isOk := FALSE
                    RETURN FALSE
                ENDIF
                SELF:_fileSize += BUFF_SIZE
                IF !SELF:_Unique .AND. !SELF:_Conditional .AND. !SELF:_Descending .AND. !ordCondInfo:Scoped
                    isOk := SELF:_CreateIndex()
                ELSE
                    isOk := SELF:_CreateUnique(ordCondInfo)
                ENDIF
                SELF:Flush()
            FINALLY
                IF !SELF:Shared
                    FConvertToFileStream(SELF:_hFile)
                ENDIF
                IF !isOk
                    SELF:Close()
                    IF System.IO.File.Exists(SELF:FileName)
                        FErase(SELF:FileName)
                    ENDIF
                ENDIF
                 
            END TRY
            RETURN isOk
            
        PRIVATE METHOD _HeaderCreate(lScoped AS LOGIC) AS LOGIC
            SELF:_Header := NtxHeader{ SELF, SELF:_hFile }
            SELF:_Header:Signature              := NtxHeaderFlags.Default
            SELF:_Header:IndexingVersion        := SELF:_indexVersion
            SELF:_Header:FirstPageOffset        := SELF:_firstPageOffset
            SELF:_Header:NextUnusedPageOffset   := SELF:_nextUnusedPageOffset
            SELF:_Header:EntrySize              := SELF:_entrySize
            SELF:_Header:KeySize                := SELF:_keySize
            SELF:_Header:KeyDecimals            := SELF:_keyDecimals
            SELF:_Header:MaxItem                := SELF:_MaxEntry
            SELF:_Header:HalfPage               := SELF:_halfPage
            SELF:_Header:Unique                 := SELF:_Unique
            SELF:_Header:Descending             := SELF:_Descending
            SELF:_Header:KeyExpression          := SELF:_KeyExpr
            SELF:_Header:ForExpression          := SELF:_ForExpr
            SELF:_Header:OrdName                := SELF:_orderName:ToUpper()
            IF SELF:_Conditional .OR. SELF:_Descending .OR. lScoped
                SELF:_Header:Signature |= NtxHeaderFlags.Conditional
            ENDIF
            IF SELF:_Partial
                SELF:_Header:Signature |= NtxHeaderFlags.Partial
            ENDIF
            IF  XSharp.RuntimeState.NewIndexLock 
                SELF:_Header:Signature |= NtxHeaderFlags.NewLock
            ENDIF
            IF  XSharp.RuntimeState.HPLocking
                SELF:_Header:Signature |= NtxHeaderFlags.Partial
            ENDIF
	    RETURN SELF:_Header:Write()
	    
            
        INTERNAL METHOD _determineSize(toConvert AS OBJECT ) AS LOGIC
            LOCAL expr AS STRING
            LOCAL nPos AS INT

            VAR type := SELF:_oRdd:_getUsualType(toConvert)
            SWITCH type
            CASE __UsualType.String
                SELF:_keySize := (WORD) ((STRING)toConvert):Length
            CASE __UsualType.Long
            CASE __UsualType.Float
                TRY
                    expr := "STR(" + SELF:_KeyExpr + ")"
                    TRY
                        VAR oBlock := SELF:_oRdd:Compile(expr)
                        expr := (STRING) SELF:_oRdd:EvalBlock(oBlock)
                    CATCH
                        RETURN FALSE
                    END TRY
                    SELF:_keySize := (WORD) expr:Length
                    nPos := expr:IndexOfAny(<CHAR>{',', '.' })
                    IF nPos < 0
                        SELF:_keyDecimals := 0
                    ELSE
                        SELF:_keyDecimals := (WORD) (SELF:_keySize - nPos - 1)
                    ENDIF
                    
                CATCH //Exception
                    SELF:_keyDecimals := 0
                END TRY
            CASE __UsualType.Date
                SELF:_keySize := 8
            CASE __UsualType.Logic
                SELF:_keySize := 1
            OTHERWISE
                SELF:_keySize := 0
                RETURN FALSE
            END SWITCH
            
            RETURN TRUE
            
            
            
        PRIVATE METHOD _CondCreate(ordCondInfo AS DBORDERCONDINFO ) AS LOGIC
            LOCAL leadingOrder  := NULL AS NtxOrder
            LOCAL lUseOrder     := FALSE AS LOGIC
            LOCAL hasWhile      := FALSE AS LOGIC
            LOCAL hasEvalBlock  := FALSE AS LOGIC
            LOCAL record        := 1 AS LONG
            LOCAL count         := 1 AS LONG
            LOCAL toDo          := 0 AS LONG
            LOCAL done          := 0 AS LONG
            LOCAL nextRecord    := 0 AS LONG
            LOCAL start         := 0 AS LONG
            LOCAL result        := FALSE AS LOGIC
            LOCAL includeRecord := TRUE AS LOGIC
            
            start := ordCondInfo:StartRecNo
            IF ordCondInfo:Scoped
                IF ordCondInfo:StartRecNo > 0
                    record := ordCondInfo:StartRecNo
                ENDIF
                IF SELF:_oRdd:_indexList:CurrentOrder != NULL
                    leadingOrder := SELF:_oRdd:_indexList:CurrentOrder
                    lUseOrder    := leadingOrder != NULL
                ENDIF
                IF ordCondInfo:All
                    // All overrules start record no
                    IF lUseOrder
                        // start from first record in index
                        record := leadingOrder:_locateKey(NULL, 0, SearchMode.Top)
                    ELSE
                        record := 1 // start from first record in file
                    ENDIF
                ENDIF
            ENDIF
            IF ordCondInfo:RecNo > 0
                // start with record indicated. This overrules ALL again
                record := ordCondInfo:RecNo
                toDo := 1
            ENDIF
            IF ordCondInfo:NextCount > 0
                toDo := ordCondInfo:NextCount
            ENDIF
            SELF:_oRdd:__Goto(record)
            // IF record is EOF then do nothing
            IF !SELF:_oRdd:_isValid .AND. !SELF:_oRdd:_Eof
                SELF:_oRdd:__Goto(start)
                SELF:ClearStack()
                RETURN FALSE
            ENDIF
            IF ordCondInfo:WhileBlock != NULL
                hasWhile := TRUE
            ENDIF
            IF ordCondInfo:EvalBlock != NULL
                hasEvalBlock := TRUE
            ENDIF
            IF lUseOrder .AND. leadingOrder:_TopStack != 0
                result := leadingOrder:_GoToRecno(SELF:_RecNo)
                IF !result
                    RETURN result
                ENDIF
            ENDIF
            DO WHILE TRUE
                IF hasWhile
                    IF ! SELF:_EvalBlock(ordCondInfo:WhileBlock, TRUE)
                        EXIT
                    ENDIF
	    	ENDIF
                IF SELF:_Conditional
                    includeRecord := SELF:_EvalBlock(ordCondInfo:ForBlock, TRUE)
                ENDIF
                IF includeRecord
                   IF !SELF:_keyUpdate( SELF:_RecNo, TRUE)
                       EXIT
	            ENDIF
		ENDIF
                IF hasEvalBlock
                    IF count >= ordCondInfo:StepSize
                        IF ! SELF:_EvalBlock(ordCondInfo:EvalBlock,FALSE)
                            EXIT
                        ENDIF
                        count := 1
                    ELSE
                        count++
                    ENDIF
                ENDIF
                done++
                IF lUseOrder 
                    nextRecord := leadingOrder:_getNextKey(FALSE, SkipDirection.Forward)
                ELSE
                    nextRecord := SELF:_RecNo + 1
                ENDIF
                SELF:_oRdd:__Goto( nextRecord)
                IF todo != 0 .AND. done >= todo
                    EXIT
                ENDIF
                IF SELF:_oRdd:_Eof 
                    EXIT
                ENDIF
            ENDDO
            // evaluate the block once more at eof
            IF hasEvalBlock
                SELF:_EvalBlock(ordCondInfo:EvalBlock,FALSE)
            ENDIF
            SELF:_oRdd:__Goto(start)
            SELF:ClearStack()
            SELF:Flush()
            RETURN TRUE
        PRIVATE METHOD _CreateEmpty() AS LOGIC
            LOCAL nLevel AS NtxLevel
            
            SELF:_firstPageOffset := SELF:_fileSize
            SELF:_fileSize += BUFF_SIZE
            nLevel := NtxLevel{SELF}
            nLevel:InitRefs(SELF:_MaxEntry, SELF:_entrySize)
            nLevel:Write(SELF:_firstPageOffset)
            SELF:ClearStack()
            RETURN TRUE
            
        PRIVATE METHOD _EvalBlock(oBlock AS ICodeBlock, lMustBeLogic AS LOGIC) AS LOGIC
            LOCAL isOk  := FALSE AS LOGIC
            LOCAL error := FALSE AS LOGIC
            TRY
                VAR res := SELF:_oRdd:EvalBlock(oBlock)
                IF res IS LOGIC VAR ok
                    isOk := ok
                ELSEIF lMustBeLogic
                    error := TRUE
                ELSE
                    isOk := TRUE
                ENDIF
            CATCH 
                error := TRUE
                isOk := FALSE
            END TRY
            IF error
                SELF:_oRdd:_dbfError( SubCodes.ERDD_KEY_EVAL,GenCode.EG_DATATYPE, SELF:fileName)
            ENDIF
            RETURN isOk
	    
        INTERNAL METHOD _CreateIndex() AS LOGIC
            LOCAL fType := 0 AS DbFieldType
            LOCAL sourceIndex := 0 AS LONG
            LOCAL evalCount AS LONG
            LOCAL lRecCount AS LONG
            LOCAL sortInfo AS DbSortInfo
            LOCAL hasBlock AS LOGIC
            LOCAL sorting AS RddSortHelper
            LOCAL byteArray AS BYTE[]
            LOCAL result AS LOGIC
            LOCAL ic AS NtxSortCompare
            LOCAL lAscii AS LOGIC
            
            fType := 0
            sourceIndex := 0
            evalCount := 0
            lRecCount := SELF:_oRdd:RecCount
            IF lRecCount == 0
                RETURN SELF:_CreateEmpty()
            ENDIF
            sortInfo := DbSortInfo{0,1}     // 0 trans items, 1 sort item
            hasBlock    := SELF:_oRdd:_OrderCondInfo:EvalBlock != NULL
            evalCount := 1
            SELF:_levelsCount := 1
            IF SELF:_SingleField != -1
                fType := SELF:_oRdd:_Fields[SELF:_SingleField]:fieldType
            // 'C', 'N', 'D', 'L'
            SWITCH fType
            CASE DbFieldType.Character
            CASE DbFieldType.Number
            CASE DbFieldType.Date
            CASE DbFieldType.Logic
                sourceIndex := SELF:_oRdd:_Fields[SELF:_SingleField]:OffSet
            OTHERWISE
                fType := 0
            END SWITCH
	    ELSE
	    	sourceIndex := -1
            ENDIF
            
            sorting := RddSortHelper{SELF:_oRDD, sortInfo, lRecCount}
            sortInfo:Items[0]:Length := SELF:_keySize
            IF SELF:_KeyExprType == __UsualType.String .OR. SELF:_KeyExprType == __UsualType.LOGIC
                lAscii := FALSE
                sortInfo:Items[0]:Flags := DbSortFlags.Default
            ELSE
                lAscii := TRUE
                sortInfo:Items[0]:Flags := DbSortFlags.Ascii
            ENDIF
            IF SELF:_oRdd:_OrderCondInfo:Descending
                sortInfo:Items[0]:Flags += DbSortFlags.Descending
            ENDIF
            sortInfo:Items[0]:OffSet := 0
            SELF:_oRdd:GoTo(1)
            byteArray := BYTE[]{ SELF:_keySize }
            REPEAT
                result := TRUE
                SELF:_oRdd:ReadRecord()
                result := getKeyValue(sourceIndex, byteArray)
                IF !result
                    EXIT
                ENDIF
                VAR toSort := SortRecord{byteArray, SELF:_RecNo}
                sorting:Add(toSort)
                IF hasBlock
                    IF evalCount >= SELF:_oRdd:_OrderCondInfo:StepSize
                        IF ! SELF:_EvalBlock(SELF:_oRdd:_OrderCondInfo:EvalBlock, FALSE)
                            EXIT
                        ENDIF
                        evalCount := 1
                    ELSE
                        evalCount++
                    ENDIF
                ENDIF
                result := SELF:_oRdd:GoTo(SELF:_RecNo + 1)
            UNTIL ! (result .AND. SELF:_oRdd:_isValid)
            IF result
                IF lAscii
                    ic := NTXSortCompareAscii{SELF:_oRdd, sortInfo}
                ELSE
                    ic := NTXSortCompareDefault{SELF:_oRdd, sortInfo}
                ENDIF
                result := sorting:Sort(ic)
            ENDIF
            SELF:_levelsCount := SELF:_initLevels(SELF:_MaxEntry + 1, lRecCount)
            SELF:_outPageNo := 1
            IF result
                result := sorting:Write(SELF)
            ENDIF
            sorting:Clear()
            sorting := NULL
            SELF:_oneItem:PageNo := 0
            SELF:_placeItem(SELF:_oneItem)
            SELF:_firstPageOffset := SELF:_oneItem:PageNo
            SELF:_nextUnusedPageOffset := 0
            VAR lpLevels := SELF:_levels
            FOREACH level AS NtxLevel IN lpLevels 
                IF level != NULL
                    IF level:PageOffset == 0
                        level:PageOffset := SELF:_outPageNo * BUFF_SIZE
                        SELF:_outPageNo++
                    ENDIF
                    level:Write(level:PageOffset)
                ENDIF
            NEXT
            FSeek3( SELF:_hFile, 0, FS_END )
            SELF:_fileSize  := (LONG) FTell( SELF:_hFile ) 
            SELF:_levels := NULL
            RETURN result
            
            // IRddSortWriter Interface, used by RddSortHelper
        PUBLIC METHOD WriteSorted(si AS DbSortInfo , record AS SortRecord) AS LOGIC
            SELF:_oneItem:PageNo    := 0
            SELF:_oneItem:Recno     := record:Recno
            SELF:_oneItem:KeyBytes  := record:Data
            RETURN SELF:_placeItem(SELF:_oneItem)
            
            
        INTERNAL METHOD _CreateUnique(ordCondInfo AS DBORDERCONDINFO ) AS LOGIC
            LOCAL Ok AS LOGIC
            Ok := SELF:_CreateEmpty()
            IF Ok
                IF ordCondInfo:Active
                    RETURN SELF:_CondCreate(ordCondInfo)
                ENDIF
                SELF:_oRdd:GoTo(1)
                IF SELF:_oRdd:_isValid
                    REPEAT
                        SELF:_keyUpdate( SELF:_RecNo, TRUE)
                        SELF:_oRdd:GoTo(SELF:_RecNo + 1)
                    UNTIL ! SELF:_oRdd:_isValid
                ENDIF
                SELF:ClearStack()
            ENDIF
            SELF:Flush()
            RETURN Ok
            
        PRIVATE METHOD _initLevels(uiBOrder AS LONG , keyCount AS LONG ) AS LONG
            LOCAL level AS LONG
            LOCAL exp AS LONG
            LOCAL nLevel AS NtxLevel
            
            level := 0
            exp := 1
            SELF:_levels := NtxLevel[]{ NTX_COUNT }
            DO WHILE exp <= keyCount
                nLevel := NtxLevel{SELF}
                nLevel:Exp := exp
                nLevel:InitRefs(SELF:_MaxEntry, SELF:_entrySize)
                SELF:_levels[level] := nLevel
                level++
                exp *= uiBOrder
            ENDDO
            SELF:_RecalcLevel(level - 1, keyCount, 1)
            RETURN level
            
        PRIVATE METHOD _placeItem(lpNode AS NtxNode ) AS LOGIC
            LOCAL iLevel AS LONG
            LOCAL level AS NtxLevel
            LOCAL node AS NtxNode
            LOCAL page AS LONG
            
            iLevel := 0
            level := SELF:_levels[0]
            DO WHILE iLevel < SELF:_levelsCount .AND. level:NodeCount >= level:Parents
                node := level[level:NodeCount]
                node:PageNo := SELF:_oneItem:PageNo
                page := SELF:_outPageNo * BUFF_SIZE
                SELF:_oneItem:PageNo := page
                level:PageOffset := page
                SELF:_outPageNo++
                iLevel++
                level := SELF:_levels[iLevel]
            ENDDO
            IF iLevel >= SELF:_levelsCount
                RETURN FALSE
            ENDIF
            node := level[level:NodeCount]
            node:KeyBytes := lpNode:KeyBytes
            node:Recno := lpNode:Recno
            node:PageNo := lpNode:PageNo
            level:NodeCount++
            SELF:_oneItem:Clear()
            IF iLevel > 0
                SELF:_ResetLevel(iLevel - 1)
            ENDIF
            RETURN TRUE
            
        PRIVATE METHOD _RecalcLevel(uiLevel AS LONG , lKeys AS LONG , uiBOrder AS LONG ) AS VOID
            LOCAL nLevel AS NtxLevel
            
            nLevel := SELF:_levels[uiLevel]
            nLevel:Write(nLevel:PageOffset)
            nLevel:NodeCount := 0
            nLevel:BaseKeys := lKeys / uiBOrder
            nLevel:ExtraKeys := lKeys - nLevel:BaseKeys * uiBOrder
            nLevel:Keys := nLevel:BaseKeys
            IF nLevel:ExtraKeys > 0
                nLevel:ExtraKeys--
                nLevel:Keys++
            ENDIF
            nLevel:Parents := nLevel:Keys / nLevel:Exp
            IF uiLevel > 0
                SELF:_RecalcLevel(uiLevel - 1, nLevel:Keys - nLevel:Parents, nLevel:Parents + 1)
            ENDIF
            
        PRIVATE METHOD _ResetLevel(uiLevel AS LONG ) AS VOID
            LOCAL nLevel AS NtxLevel
            
            nLevel := SELF:_levels[uiLevel]
            nLevel:Write(nLevel:PageOffset)
            nLevel:NodeCount := 0
            IF nLevel:ExtraKeys > 0
                nLevel:ExtraKeys--
                IF uiLevel > 0
                    SELF:_RecalcLevel(uiLevel - 1, nLevel:Keys - nLevel:Parents, nLevel:Parents + 1)
                ENDIF
            ELSE
                IF nLevel:Keys == nLevel:BaseKeys
                    IF uiLevel > 0
                        SELF:_RecalcLevel(uiLevel - 1, nLevel:Keys - nLevel:Parents, nLevel:Parents + 1)
                    ENDIF
                ELSE
                    nLevel:Keys := nLevel:BaseKeys
                    nLevel:Parents := nLevel:Keys / nLevel:Exp
                    IF uiLevel > 0
                        SELF:_RecalcLevel(uiLevel - 1, nLevel:Keys - nLevel:Parents, nLevel:Parents + 1)
                    ENDIF
                ENDIF
            ENDIF
            
        PUBLIC METHOD Truncate() AS LOGIC
            SELF:_firstPageOffset := BUFF_SIZE
            SELF:_nextUnusedPageOffset := 0
            SELF:_Hot := TRUE
            SELF:ClearStack()
            SELF:_currentvalue:Recno := 0
            FChSize( SELF:_hFile, BUFF_SIZE )
            SELF:_fileSize := BUFF_SIZE
            SELF:Flush()
            RETURN TRUE
            
    END CLASS
    
END NAMESPACE


