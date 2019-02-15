//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System
USING System.Diagnostics
USING XSharp.RDD.Enums
USING XSharp.RDD.Support

BEGIN NAMESPACE XSharp.RDD.CDX

    INTERNAL DELEGATE ValueBlock( sourceIndex AS LONG, byteArray AS BYTE[]) AS LOGIC

    INTERNAL PARTIAL SEALED CLASS CdxTag

        // Methods for Creating Indices
        PUBLIC METHOD Create(createInfo AS DBORDERCREATEINFO ) AS LOGIC
        /*
            LOCAL ordCondInfo AS DBORDERCONDINFO
            LOCAL isOk AS LOGIC
            LOCAL orderInfo AS DBORDERINFO
            LOCAL hasForCond AS LOGIC
            LOCAL Expression AS STRING
            LOCAL num AS WORD
            
            ordCondInfo := SELF:_oRdd:_OrderCondInfo
            IF string.IsNullOrEmpty(createInfo:BagName)
                SELF:_oRDD:_dbfError( GenCode.EG_ARG, SubCodes.EDB_CREATEINDEX)
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
                SELF:_currentKeyBuffer := BYTE[]{_Keysize+1}
                SELF:_newKeyBuffer   := BYTE[]{_Keysize+1}
            ENDIF
            
            // 8 Bytes : PrevPage (4 bytes) + Recno (4 bytes)
            SELF:_entrySize := SELF:_keySize + (WORD) 8
            
            num := (WORD)(  ( BUFF_SIZE - 4) / (SELF:_keySize + 10))
            SELF:_halfPage := (WORD) ((num - 1) / 2)
            SELF:_MaxEntry := (WORD) (SELF:_halfPage * 2)
            SELF:_firstPageOffset := BUFF_SIZE
            SELF:_fileSize := 0
            SELF:_nextUnusedPageOffset := 0
            SELF:_Version := 1
            SELF:_Shared := FALSE
            SELF:_Hot := TRUE
            SELF:_TopStack := 0
            SELF:Unique := createInfo:Unique
            SELF:_Ansi := SELF:_oRdd:_Ansi
            SELF:_Conditional := FALSE
            SELF:_Descending := FALSE
            SELF:_writeLocks := 0
            SELF:Custom := ordCondInfo:Scoped
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
            VAR oldShared   := SELF:_oRDD:_Shared
            VAR oldReadOnly := SELF:_oRDD:_ReadOnly 
            SELF:_oRDD:_Shared := FALSE
            SELF:_oRDD:_ReadOnly  := FALSE
            SELF:_hFile    := Fopen(SELF:FullPath, SELF:_oRDD:_OpenInfo:FileMode)
            SELF:_oRDD:_Shared := oldShared
            SELF:_oRDD:_ReadOnly  := oldReadOnly
            
            IF SELF:_hFile == F_ERROR
                SELF:Close()
                SELF:_oRdd:_dbfError( SubCodes.ERDD_CREATE_ORDER, GenCode.EG_CREATE, createInfo:BagName)
                RETURN FALSE
            ENDIF
            SELF:_PageList := CdxPageList{SELF}
            
//            SELF:_Header := NtxHeader{ SELF:_hFile }
//            SELF:_Header:Signature              := NtxHeaderFlags.Default
//            SELF:_Header:Version                := SELF:_Version
//            SELF:_Header:FirstPageOffset        := SELF:_firstPageOffset
//            SELF:_Header:NextUnusedPageOffset   := SELF:_nextUnusedPageOffset
//            SELF:_Header:EntrySize              := SELF:_entrySize
//            SELF:_Header:KeySize                := SELF:_keySize
//            SELF:_Header:KeyDecimals            := SELF:_keyDecimals
//            SELF:_Header:MaxItem                := SELF:_MaxEntry
//            SELF:_Header:HalfPage               := SELF:_halfPage
//            SELF:_Header:Unique                 := SELF:_Unique
//            SELF:_Header:Descending             := SELF:_Descending
//            SELF:_Header:KeyExpression          := SELF:_KeyExpr
//            SELF:_Header:ForExpression          := SELF:_ForExpr
//            SELF:_Header:OrdName                := SELF:_orderName
//            SELF:_midItem                       := CdxNode{SELF:_keySize}
            SELF:_oneItem                       := CdxNode{SELF:_keySize}
            IF SELF:_Conditional .OR. SELF:_Descending .OR. ordCondInfo:Scoped
                SELF:_Header:Signature |= NtxHeaderFlags.Conditional
            ENDIF
            IF SELF:Custom
                SELF:_Header:Signature |= NtxHeaderFlags.Partial
            ENDIF
            SELF:_maxLockTries  := 99 //(LONG)XSharp.RuntimeState.LockTries
            SELF:_tagNumber     := 1
            IF  XSharp.RuntimeState.NewIndexLock 
                SELF:_Header:Signature |= NtxHeaderFlags.NewLock
                SELF:_lockOffset := LOCKOFFSET_NEW
            ELSE
                SELF:_lockOffset := LOCKOFFSET_OLD
            ENDIF
            IF !SELF:_Header:Write()
                SELF:Close()
                SELF:_oRdd:_dbfError(SubCodes.ERDD_WRITE,GenCode.EG_CREATE,  createInfo:BagName)
                RETURN FALSE
            ENDIF
            SELF:_fileSize += BUFF_SIZE
            IF !SELF:Unique .AND. !SELF:_Conditional .AND. !SELF:_Descending .AND. !ordCondInfo:Scoped
                isOk := SELF:_CreateIndex()
            ELSE
                isOk := SELF:_CreateUnique(ordCondInfo)
            ENDIF
            IF !isOk
                SELF:Flush()
                SELF:Close()
                RETURN isOk
            ENDIF
            */
            RETURN SELF:Flush()
            
            
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
            /*
            LOCAL isOk AS LOGIC
            LOCAL leadingOrder  := NULL AS NtxOrder
            LOCAL lUseOrder     := FALSE AS LOGIC
            LOCAL hasWhile AS LOGIC
            LOCAL hasEvalBlock AS LOGIC
            LOCAL record AS LONG
            LOCAL count AS LONG
            LOCAL toDo AS LONG
            LOCAL done AS LONG
            LOCAL nextRecord AS LONG
            LOCAL start AS LONG
            LOCAL result AS LOGIC
            
            isOk := TRUE
            nOrder := NULL
            hasWhile := FALSE
            hasEvalBlock := FALSE
            record := 1
            count := 1
            nextRecord := 0
            toDo := 0
            done := 0
            start := ordCondInfo:StartRecNo
            IF ordCondInfo:Scoped
                IF ordCondInfo:StartRecNo > 0
                    record := ordCondInfo:StartRecNo
                ENDIF
                IF SELF:_oRdd:_indexList:Focus != 0
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
                SELF:_TopStack := 0
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
                    isOk := TRUE
                    TRY
                        isOk := (LOGIC) SELF:_oRdd:EvalBlock(ordCondInfo:WhileBlock)
                    CATCH
                        SELF:_oRdd:_dbfError(SubCodes.ERDD_KEY_EVAL, GenCode.EG_DATATYPE, SELF:fileName)
                        isOk := FALSE
                    END TRY
                    IF ! isOk
                        EXIT
                    ENDIF
                    
                ENDIF
                IF !SELF:_keyUpdate( SELF:_RecNo, TRUE)
                    EXIT
                ENDIF
                IF hasEvalBlock
                    IF count >= ordCondInfo:StepSize
                        isOk := TRUE
                        TRY
                            isOk := (LOGIC) SELF:_oRdd:EvalBlock(ordCondInfo:EvalBlock)
                        CATCH
                            SELF:_oRdd:_dbfError( SubCodes.ERDD_KEY_EVAL,GenCode.EG_DATATYPE, SELF:fileName)
                            isOk := FALSE
                        END TRY
                        IF ! isOk
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

            SELF:_oRdd:__Goto(start)
            SELF:_TopStack := 0
            SELF:Flush()
            */
            RETURN TRUE
        PRIVATE METHOD _CreateEmpty() AS LOGIC
            /*
            LOCAL nLevel AS NtxLevel
            
            SELF:_firstPageOffset := SELF:_fileSize
            SELF:_fileSize += BUFF_SIZE
            nLevel := NtxLevel{SELF}
            nLevel:InitRefs(SELF:_MaxEntry, SELF:_entrySize)
            nLevel:Write(SELF:_firstPageOffset)
            SELF:ClearStack()
            */
            RETURN TRUE
            
        INTERNAL METHOD _CreateIndex() AS LOGIC
            /*
            LOCAL fType AS DbFieldType
            LOCAL sourceIndex AS LONG
            LOCAL evalCount AS LONG
            LOCAL lRecCount AS LONG
            LOCAL sortInfo AS DbSortInfo
            LOCAL hasBlock AS LOGIC
            LOCAL sorting AS RddSortHelper
            LOCAL byteArray AS BYTE[]
            LOCAL result AS LOGIC
            LOCAL ic AS CdxSortCompare
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
            ENDIF
            // 'C', 'N', 'D'
            SWITCH fType
            CASE DbFieldType.Character
            CASE DbFieldType.Number
            CASE DbFieldType.Date
                sourceIndex := SELF:_oRdd:_Fields[SELF:_SingleField]:OffSet
            OTHERWISE
                fType := 0
            END SWITCH
            
            sorting := RddSortHelper{sortInfo, lRecCount}
            sortInfo:Items[0]:Length := SELF:_keySize
            IF SELF:_KeyExprType == TypeCode.String
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
                        TRY
                            SELF:_oRdd:EvalBlock(SELF:_oRdd:_OrderCondInfo:EvalBlock)
                        CATCH
                            result := FALSE
                        END TRY
                        evalCount := 1
                    ELSE
                        evalCount++
                    ENDIF
                ENDIF
                result := SELF:_oRdd:GoTo(SELF:_RecNo + 1)
            UNTIL ! (result .AND. SELF:_oRdd:_isValid)
            IF result
                IF lAscii
                    ic := CdxSortCompareAscii{SELF:_oRdd, sortInfo}
                ELSE
                    ic := CdxSortCompareDefault{SELF:_oRdd, sortInfo}
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
            */
            RETURN TRUE
            
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
                SELF:_TopStack := 0
            ENDIF
            SELF:Flush()
            RETURN Ok
            
        PRIVATE METHOD _initLevels(uiBOrder AS LONG , keyCount AS LONG ) AS LONG
            LOCAL level := 0 AS LONG
            /*
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
            */
            RETURN level
           
        PRIVATE METHOD _placeItem(lpNode AS CdxNode ) AS LOGIC
            /*
            LOCAL iLevel AS LONG
            LOCAL level AS NtxLevel
            LOCAL node AS CdxNode
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
            */
            RETURN TRUE
            
        PRIVATE METHOD _RecalcLevel(uiLevel AS LONG , lKeys AS LONG , uiBOrder AS LONG ) AS VOID
            /*
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
            */

        PRIVATE METHOD _ResetLevel(uiLevel AS LONG ) AS VOID
            /*
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
            */
        PUBLIC METHOD Truncate() AS LOGIC
//            SELF:_firstPageOffset := CDXPAGE_SIZE
//            SELF:_nextUnusedPageOffset := 0
//            SELF:_Hot := TRUE
//            SELF:ClearStack()
//            SELF:_currentRecno := 0
//            FChSize( SELF:_hFile, CDXPAGE_SIZE )
//            SELF:_fileSize := CDXPAGE_SIZE
//            SELF:Flush()
            RETURN TRUE
            
    END CLASS
END NAMESPACE
