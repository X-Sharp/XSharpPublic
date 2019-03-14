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

    INTERNAL PARTIAL SEALED CLASS NtxOrder
    
        // MEthods for walking indices, so GoTop, GoBottom, Skip and Seek
        
        PUBLIC METHOD GoBottom() AS LOGIC
            LOCAL locked AS LOGIC
            LOCAL result AS LOGIC
            
            locked := FALSE
            TRY
                IF SELF:HasBottomScope
                    result := SELF:_ScopeSeek(DBOrder_Info.DBOI_SCOPEBOTTOM)
                ELSE
                    SELF:_oRdd:GoCold()
                    SELF:_oRdd:_Top := FALSE
                    SELF:_oRdd:_Bottom := TRUE
                    locked := SELF:_lockForRead()
                    IF !locked
                        RETURN FALSE
                    ENDIF
                    VAR recno := SELF:_locateKey(NULL, 0, SearchMode.Bottom)
                    result := SELF:_oRdd:__Goto(recno)
                    IF result
                        result := SELF:_oRdd:SkipFilter(-1)
                    ENDIF
                ENDIF
                RETURN result
            FINALLY
                IF locked
                    SELF:_unLockForRead()
                ENDIF
            END TRY
            
            
        PUBLIC METHOD GoTop() AS LOGIC
            LOCAL locked AS LOGIC
            LOCAL result AS LOGIC
            locked := FALSE
            TRY
                SELF:_oRdd:GoCold()
                
                IF SELF:HasTopScope
                    result := SELF:_ScopeSeek(DBOrder_Info.DBOI_SCOPETOP)
                    IF !SELF:_oRdd:_Found
                        SELF:_oRdd:_Bof := TRUE
                    ENDIF
                ELSE
                    SELF:_oRdd:_Top := TRUE
                    SELF:_oRdd:_Bottom := FALSE
                    locked := SELF:_lockForRead()
                    IF !locked
                        RETURN FALSE
                    ENDIF
                    VAR recno := SELF:_locateKey(NULL, 0, SearchMode.Top)
                    result := SELF:_oRdd:__Goto(recno)
                    IF result
                        result := SELF:_oRdd:SkipFilter(1)
                    ENDIF
                ENDIF
                RETURN result    
            FINALLY
                IF locked
                    result := SELF:_unLockForRead()
                ENDIF
            END TRY
            
            
            
        PUBLIC METHOD Seek(seekInfo AS DBSEEKINFO ) AS LOGIC
            LOCAL uiRealLen AS LONG
            LOCAL byteArray AS BYTE[]
            uiRealLen := 0
            byteArray := BYTE[]{ _keySize }
            // Convert the seeked key to a byte Array
            IF !SELF:_ToString(seekInfo:Value, SELF:_keySize, SELF:_keyDecimals, byteArray, REF uiRealLen)
                SELF:_oRdd:_dbfError( SubCodes.ERDD_VAR_TYPE, GenCode.EG_DATATYPE,SELF:fileName)
                RETURN FALSE
            ENDIF
            IF SELF:HasTopScope
                IF SELF:__Compare(byteArray, SELF:_topScopeBuffer, SELF:_keySize) < 0
                    IF seekInfo:SoftSeek
                        RETURN SELF:_ScopeSeek(DBOrder_Info.DBOI_SCOPETOP)
                    ENDIF
                    RETURN SELF:_oRdd:__Goto(0)
                ENDIF
            ENDIF
            IF SELF:HasBottomScope
                IF SELF:__Compare(byteArray, SELF:_bottomScopeBuffer, SELF:_keySize) > 0
                    RETURN SELF:_oRdd:__Goto(0)
                ENDIF
            ENDIF
            RETURN SELF:_Seek(seekInfo, byteArray)
            
            
            
            
            
        PUBLIC METHOD SkipRaw(nToSkip AS LONG ) AS LOGIC
            LOCAL recno AS LONG
            LOCAL isBof AS LOGIC
            LOCAL isEof AS LOGIC
            LOCAL changedBof AS LOGIC
            LOCAL changedEof AS LOGIC
            LOCAL locked AS LOGIC
            LOCAL orgToSkip AS INT
            LOCAL result := FALSE AS LOGIC
            // Default Position = Current Record
            IF nToSkip == 0
                recno := SELF:_Recno
            ELSE
                recno := 0
            ENDIF
            
            isBof := FALSE
            isEof := FALSE
            changedBof := FALSE
            changedEof := FALSE
            locked := FALSE
            
            TRY
                orgToSkip := nToSkip
                SELF:_oRdd:GoCold()
                locked := SELF:_lockForRead()
                IF !locked
                    RETURN FALSE
                ENDIF
                IF !SELF:_oRdd:_isValid
                    IF nToSkip < 0
                        recno := SELF:_locateKey(NULL, 0, SearchMode.Bottom)
                        nToSkip++
                    ELSE
                        recno := 0
                        nToSkip := 0
                    ENDIF
                ELSE
                    IF SELF:_TopStack == 0
                        SELF:_GoToRecno( SELF:_Recno)
                    ENDIF
                ENDIF
                
                IF orgToSkip != 0
                    IF SELF:HasTopScope .OR. SELF:HasBottomScope
                        isBof := SELF:_oRdd:_Bof
                        isEof := SELF:_oRdd:_Eof
                        recno := SELF:_ScopeSkip(nToSkip)
                        IF isBof != SELF:_oRdd:_Bof
                            changedBof := TRUE
                            isBof := SELF:_oRdd:_Bof
                        ELSE
                            changedBof := FALSE
                        ENDIF
                        IF isEof != SELF:_oRdd:_Eof
                            changedEof := TRUE
                            isEof := SELF:_oRdd:_Eof
                        ELSE
                            changedEof := FALSE
                        ENDIF
                    ELSE
                        IF nToSkip != 0
                            recno := SELF:_nextKey(nToSkip)
                        ENDIF
                    ENDIF
                ENDIF
                result := SELF:_oRdd:__Goto(recno)
                IF !SELF:HasTopScope .AND. !SELF:HasBottomScope
                    RETURN result
                ENDIF
                IF changedBof
                    SELF:_oRdd:_Bof := isBof
                ENDIF
                IF !changedEof
                    RETURN result
                ENDIF
                SELF:_oRdd:_Eof := isEof
                
            CATCH ex AS Exception
                SELF:_oRdd:_dbfError(ex, SubCodes.EDB_SKIP,GenCode.EG_CORRUPTION,  "NtxOrder.SkipRaw") 
            FINALLY
                IF locked
                    result := SELF:_unLockForRead()
                ENDIF
            END TRY
            RETURN result
            
            
        PRIVATE METHOD _getNextKey(thisPage AS LOGIC , moveDirection AS SkipDirection ) AS LONG
            LOCAL page AS NtxPage
            LOCAL node AS NtxPageNode
            // No page loaded ?
            IF SELF:_TopStack == 0
                RETURN 0
            ENDIF
            VAR topStack := SELF:CurrentStack
            page := SELF:_PageList:Read(topStack:Page)
            node := page[topStack:Pos]
            IF thisPage
                IF moveDirection == SkipDirection.Backward
                    topStack:Pos--
                    node:Pos := topStack:Pos
                ENDIF
                SELF:_saveCurrentRecord(node)
                RETURN node:Recno
            ENDIF
            
            IF moveDirection == SkipDirection.Forward
                topStack:Pos++
                node:Pos := topStack:Pos
                IF node:PageNo != 0
                    RETURN SELF:_locate(NULL, 0, SearchMode.Top, node:PageNo)
                ENDIF
                IF topStack:Pos == topStack:Count
                    DO WHILE SELF:_TopStack != 0 .AND. topStack:Pos == topStack:Count
                        topStack := SELF:PopPage()
                    ENDDO
                    RETURN SELF:_getNextKey(TRUE, SkipDirection.Forward)
                ENDIF
                SELF:_saveCurrentRecord(node)
                RETURN node:Recno
            ENDIF
            IF node:PageNo != 0
                RETURN SELF:_locate(NULL, 0, SearchMode.Bottom, node:PageNo)
            ENDIF
            IF topStack:Pos == 0
                DO WHILE SELF:_TopStack != 0 .AND. topStack:Pos == 0
                    topStack := SELF:PopPage()
                ENDDO
                RETURN SELF:_getNextKey(TRUE, SkipDirection.Backward)
            ENDIF
            topStack:Pos--
            node:Pos := topStack:Pos
            SELF:_saveCurrentRecord(node)
            RETURN node:Recno
            
            
        PRIVATE METHOD _findItemPos(record REF LONG , nodePage AS LOGIC ) AS LOGIC
            LOCAL page   AS NtxPage
            LOCAL node   AS NtxPageNode
            IF SELF:_TopStack == 0
                RETURN FALSE
            ENDIF
            VAR topStack := SELF:CurrentStack
            page := SELF:_PageList:Read(topStack:Page)
            node := page[topStack:Pos]
            IF nodePage
                topStack:Pos--
                record++
                RETURN TRUE
            ENDIF
            IF node:PageNo != 0
                SELF:_locate(NULL, 0, SearchMode.Bottom, node:PageNo)
                record += (topStack:Pos + 1)
                topStack:Pos := 0
                RETURN TRUE
            ENDIF
            IF topStack:Pos == 0
                DO WHILE SELF:_TopStack != 0 .AND. topStack:Pos == 0
                    topStack := SELF:PopPage()
                ENDDO
                RETURN SELF:_findItemPos(REF record, TRUE)
            ENDIF
            record += topStack:Pos
            topStack:Pos := 0
            RETURN TRUE
            
          
            
        PRIVATE METHOD _getScopePos() AS LONG
            LOCAL first AS LONG
            LOCAL last AS LONG
            IF SELF:HasTopScope
                IF SELF:__Compare(SELF:_currentValue:Key, SELF:_topScopeBuffer, SELF:_topScopeSize) < 0
                    RETURN 0
                ENDIF
            ENDIF
            IF SELF:HasBottomScope
                IF SELF:__Compare(SELF:_currentValue:Key, SELF:_bottomScopeBuffer, SELF:_bottomScopeSize) > 0
                    RETURN 0
                ENDIF
            ENDIF
            first := 1
            last := 1
            DO WHILE SELF:_findItemPos(REF first, FALSE)
                NOP
            ENDDO
            IF SELF:HasTopScope
                SELF:_ScopeSeek(DBOrder_Info.DBOI_SCOPETOP)
                DO WHILE SELF:_findItemPos(REF last, FALSE)
                    NOP
                ENDDO
            ENDIF
            RETURN first - last + 1
            
            
        INTERNAL METHOD _saveCurrentKey(rcno AS LONG, oData AS RddKeyData) AS LOGIC
            LOCAL isOk AS LOGIC
            
            isOk := TRUE
            IF rcno != oData:Recno .OR. SELF:Shared
                oData:Recno := rcno
                isOk := SELF:getKeyValue(SELF:_SourceIndex, oData:Key)
		        IF SELF:_Conditional
                    oData:ForCond := SELF:_EvalBlock(SELF:_ForCodeBlock, TRUE)
                ENDIF
            ENDIF
            IF !isOk
                SELF:_oRdd:_dbfError(SubCodes.ERDD_KEY_EVAL, GenCode.EG_DATATYPE, SELF:fileName)
            ENDIF
            RETURN isOk
            
            
            
        PRIVATE METHOD _ScopeSkip(lNumKeys AS LONG ) AS LONG
            LOCAL result AS LONG
            LOCAL recno AS LONG
            LOCAL SkipDirection AS SkipDirection
            
            VAR RT_Deleted := XSharp.RuntimeState.Deleted
            result := SELF:_RecNo
            IF lNumKeys == 1
                recno := SELF:_getNextKey(FALSE, SkipDirection.Forward)
                IF RT_Deleted .OR. SELF:_oRdd:_FilterInfo:Active
                    recno := SELF:_skipFilter(recno, SkipDirection.Forward)
                ENDIF
                IF recno == 0
                    SELF:_oRdd:_Eof := TRUE
                    RETURN 0
                ENDIF
                IF SELF:HasBottomScope
                    IF SELF:__Compare(SELF:_currentvalue:Key, SELF:_bottomScopeBuffer, SELF:_bottomScopeSize) > 0
                        SELF:_oRdd:_Eof := TRUE
                        RETURN result
                    ENDIF
                ENDIF
            ELSE
                IF lNumKeys < 0
                    lNumKeys := -lNumKeys
                    SkipDirection := SkipDirection.Backward
                ELSE
                    SkipDirection := SkipDirection.Forward
                ENDIF
                IF lNumKeys != 0
                    REPEAT
                        recno := SELF:_getNextKey(FALSE, SkipDirection)
                        IF rT_Deleted .OR. SELF:_oRdd:_FilterInfo:Active
                            recno := SELF:_skipFilter(recno, SkipDirection)
                        ENDIF
                        lNumKeys--
                        IF SkipDirection == SkipDirection.Backward
                            IF SELF:HasTopScope
                                IF SELF:__Compare(SELF:_currentvalue:Key, SELF:_topScopeBuffer, SELF:_topScopeSize) < 0
                                    recno := SELF:_getNextKey(FALSE, SkipDirection.Forward)
                                    SELF:_oRdd:_Bof := TRUE
                                    EXIT
                                ENDIF
                            ENDIF
                        ELSE
                            IF SELF:HasBottomScope
                                IF recno != 0
                                    SELF:_oRdd:_Eof := TRUE
                                    RETURN result
                                ENDIF
                                SELF:_oRdd:_Bof := FALSE
                                
                                IF SELF:__Compare(SELF:_currentvalue:Key, SELF:_bottomScopeBuffer, SELF:_bottomScopeSize) > 0
                                    SELF:_oRdd:_Eof := TRUE
                                    RETURN result
                                ENDIF
                                result := recno
                            ENDIF
                        ENDIF
                    UNTIL !((recno != 0) .AND. (lNumKeys != 0))
                ELSE
                    recno := 0
                ENDIF
            ENDIF
            RETURN recno
            
            
        PRIVATE METHOD _ScopeSeek(uiScope AS DBOrder_Info ) AS LOGIC
            LOCAL result AS LOGIC
            LOCAL seekInfo AS DbSeekInfo
            LOCAL obj AS OBJECT
            LOCAL mustSeek AS LOGIC
            
            result := TRUE
            seekInfo := DbSeekInfo{}
            IF uiScope == DBOrder_Info.DBOI_SCOPETOP
                obj := SELF:_topScope
                IF obj == NULL
                    result := SELF:GoTop()
                    mustSeek := FALSE
                ELSE
                    seekInfo:Last := FALSE
                    mustSeek := TRUE
                ENDIF
            ELSE
                obj := SELF:_bottomScope
                IF obj == NULL
                    result := SELF:GoBottom()
                    mustSeek := FALSE
                ELSE
                    seekInfo:Last := TRUE
                    mustSeek := TRUE
                ENDIF
            ENDIF
            IF mustSeek
                seekInfo:Value      := obj
                seekInfo:SoftSeek   := TRUE
                result := SELF:_Seek(seekInfo, obj)
                SELF:_oRdd:_Found := SELF:_isInScope()
                IF !SELF:_oRdd:_Found
                    SELF:_oRdd:GoTo(0)
                ENDIF
            ENDIF
            RETURN result
            
            
        PRIVATE METHOD _isInScope() AS LOGIC
            LOCAL isOk AS LOGIC
            LOCAL itmBottomScope AS OBJECT
            
            isOk := SELF:_oRdd:_Found
            IF !isOk .AND. SELF:_RecNo != 0
                IF SELF:HasBottomScope
                    itmBottomScope := SELF:_bottomScope
                    SELF:_ToString(itmBottomScope, SELF:_keySize, SELF:_keyDecimals, SELF:_newValue:Key)
                    IF SELF:__Compare(SELF:_newValue:Key, SELF:_currentValue:Key, SELF:_keySize) >= 0
                        isOk := TRUE
                    ENDIF
                ELSE
                    isOk := TRUE
                ENDIF
            ENDIF
            RETURN isOk
            
            
        PRIVATE METHOD _goRecord(keyBytes AS BYTE[], keyLen AS LONG, gotoRec AS LONG ) AS LONG
            LOCAL recno AS LONG
            // Search the first occurence from the start of the index
            recno := SELF:_locateKey(keyBytes, keyLen, SearchMode.Left)
            // Now, move until we found the right Recno
            DO WHILE recno != 0 .AND. recno != gotoRec
                recno := SELF:_getNextKey(FALSE, SkipDirection.Forward)
            ENDDO
            RETURN recno
            
            
        INTERNAL METHOD _GoToRecno(recno AS LONG ) AS LOGIC
            LOCAL result AS LOGIC
            result := TRUE
            SELF:_saveCurrentKey(recno,SELF:_currentValue)
            IF SELF:_goRecord(SELF:_currentValue:Key, SELF:_keySize, recno) != recno
                IF SELF:_goRecord(NULL, 0, recno) != recno
                    IF !SELF:_Unique .AND. !SELF:_Conditional .AND. !SELF:_Partial
                        SELF:_oRdd:_dbfError( SubCodes.ERDD_RECNO_MISSING, GenCode.EG_CORRUPTION,SELF:fileName)
                        result := FALSE
                    ENDIF
                    SELF:_TopStack := 0
                ENDIF
            ENDIF
            RETURN result
            
        PRIVATE METHOD _locateKey( keyBuffer AS BYTE[] , bufferLen AS LONG , searchMode AS SearchMode ) AS LONG
            // Find Key starting at the top of the index
            SELF:_TopStack := 0
            IF bufferLen > SELF:_keySize
                bufferLen := SELF:_keySize
            ELSE
                IF bufferLen == 0
                    bufferLen := SELF:_keySize
                ENDIF
            ENDIF
            RETURN SELF:_locate(keyBuffer, bufferLen, searchMode, SELF:_firstPageOffset)
            
            
        PRIVATE METHOD _locate(keyBuffer AS BYTE[] , bufferLen AS LONG , searchMode AS SearchMode , pageOffset AS LONG ) AS LONG
            LOCAL foundPos  AS WORD
            LOCAL page   AS NtxPage
            LOCAL nodeCount AS WORD
            LOCAL node      AS NtxPageNode
            LOCAL minPos    AS WORD
            LOCAL maxPos    AS WORD
            // find a key starting at the pageOffSet passed 
            foundPos := 0
            //Load the page at pageOffset
            page := SELF:_PageList:Read(pageOffset)
            IF page == NULL
                SELF:_TopStack := 0
                RETURN 0
            ENDIF
            // How many Items in that page ?
            nodeCount := page:NodeCount
            // Get the first one
            node := page[0]
            
            SWITCH searchMode
            CASE SearchMode.Right
                IF SELF:_Descending
                    // search...
                    minPos := 0
                    maxPos := nodeCount
                    DO WHILE minPos < maxPos
                        node:Pos := foundPos :=  (minPos + maxPos) / 2
                        IF SELF:__Compare(node:KeyBytes, keyBuffer, bufferLen) >= 0
                            minPos := foundPos + 1
                        ELSE
                            maxPos := foundPos
                        ENDIF
                    ENDDO
                    foundPos := minPos
                ELSE
                    minPos := 0
                    maxPos := nodeCount
                    DO WHILE minPos < maxPos
                        node:Pos := foundPos := (WORD) ((minPos + maxPos) / 2)
                        IF SELF:__Compare(node:KeyBytes, keyBuffer, bufferLen) <= 0
                            minPos := foundPos + 1
                        ELSE
                            maxPos := foundPos
                        ENDIF
                    ENDDO
                    foundPos := minPos
                ENDIF
            CASE SearchMode.Left
            CASE SearchMode.LeftFound
                    minPos := 0
                    maxPos := nodeCount
                    DO WHILE minPos < maxPos
                        foundPos := (minPos + maxPos) / 2
                        node:Pos := foundPos
                        IF SELF:_Descending
                            IF SELF:__Compare(node:KeyBytes, keyBuffer, bufferLen) > 0
                                minPos := foundPos + 1
                            ELSE
                                maxPos := foundPos
                            ENDIF
                        ELSE
                            IF SELF:__Compare(node:KeyBytes, keyBuffer, bufferLen) < 0
                                minPos := foundPos + 1
                            ELSE
                                maxPos := foundPos
                            ENDIF
                        ENDIF
                    ENDDO
                    node:Pos := foundPos := minPos
                    IF searchMode == SearchMode.Left .AND. SELF:__Compare(node:KeyBytes, keyBuffer, bufferLen) == 0
                        searchMode := SearchMode.LeftFound
                    ENDIF
                    
            CASE SearchMode.Bottom
                node:Pos := foundPos := nodeCount
                IF node:PageNo == 0 .AND. foundPos > 0
                    foundPos--
                    node:Pos := foundPos
                ENDIF
            CASE SearchMode.Top
                node:Pos := foundPos := 0
            END SWITCH
            // Add info in the stack
            SELF:_TopStack++
            VAR topStack := SELF:CurrentStack
            topStack:Pos      := foundPos
            topStack:Page     := pageOffset
            topStack:Count    := nodeCount
            
            node:Pos := foundPos
            IF node:PageNo != 0
                RETURN SELF:_locate(keyBuffer, bufferLen, searchMode, node:PageNo)
            ENDIF
            
            IF foundPos < nodeCount
                SWITCH searchMode
                CASE SearchMode.LeftFound
                CASE SearchMode.Bottom
                CASE SearchMode.Top
                    SELF:_saveCurrentRecord(node)
                    RETURN node:Recno
                CASE SearchMode.Left
                    IF SELF:__Compare(node:KeyBytes, keyBuffer, bufferLen) == 0
                        SELF:_saveCurrentRecord(node)
                        RETURN node:Recno
                    ENDIF
                    RETURN 0
                CASE SearchMode.Right
                    RETURN 0
                END SWITCH
            ELSEIF searchMode == SearchMode.LeftFound
                DO WHILE SELF:_TopStack != 0 .AND. topStack:Pos == topStack:Count
                    topStack := SELF:PopPage()
                ENDDO
                IF SELF:_TopStack != 0
                    page := SELF:_PageList:Read(topStack:Page)
                    IF page == NULL
                        SELF:ClearStack()
                        RETURN 0
                    ENDIF
                    node := page[topStack:Pos]
                    SELF:_saveCurrentRecord(node)
                    RETURN node:Recno
                ENDIF
            ENDIF
            RETURN 0
            
            
        PRIVATE METHOD _skipFilter(recno AS LONG , iPolar AS SkipDirection ) AS LONG
            IF SELF:_oRdd:__Goto(recno)
                SELF:_oRdd:SkipFilter((INT) iPolar)
                recno := SELF:_Recno
            ENDIF
            RETURN recno

        PRIVATE METHOD _Seek(seekInfo AS DBSEEKINFO , abNewKey AS BYTE[] ) AS LOGIC
            LOCAL recno AS LONG
            LOCAL result AS LOGIC
            LOCAL fSoft AS LOGIC
            LOCAL recnoOk AS LONG
            LOCAL locked AS LOGIC
            LOCAL strCmp AS INT
            LOCAL strCmpMaxMin AS INT
            LOCAL diff AS INT
            LOCAL deletedState AS LOGIC
            LOCAL padLen AS INT
            LOCAL needPadStr AS LOGIC
            LOCAL len AS INT
            LOCAL text AS STRING
            LOCAL temp AS BYTE
            
            recno := 0
            result := FALSE
            fSoft := FALSE
            recnoOK := 0
            locked := FALSE
            TRY
                deletedState := XSharp.RuntimeState.Deleted
                SELF:_oRdd:GoCold()
                locked := SELF:_lockForRead()
                IF locked
                    IF SELF:Shared
                        SELF:_currentValue:Recno := 0
                    ENDIF
                    needPadStr := FALSE
                    IF seekInfo:Value:GetType() == TYPEOF(STRING)
                        text := (STRING)seekInfo:Value
                        len := text:Length
                        padLen := len
                        IF len < SELF:_keySize
                            needPadStr := TRUE
                            IF SELF:_Descending
                                abNewKey[len] := Byte.MaxValue
                            ELSE
                                abNewKey[len] := 1
                            ENDIF
                            padLen := len + 1
                            fSoft := seekInfo:SoftSeek
                            seekInfo:SoftSeek := TRUE
                        ENDIF
                    ELSE
                        len := SELF:_keySize
                        padLen := len
                    ENDIF
                    recno := SELF:_locateKey(abNewKey, padLen, IIF(seekInfo:SoftSeek , SearchMode.LeftFound , SearchMode.Left))
                    result := SELF:_oRdd:__Goto(recno)
                    IF deletedState .OR. SELF:_oRdd:_FilterInfo:Active
                        SELF:_oRdd:SkipFilter(1)
                        recno := SELF:_Recno
                    ENDIF
                    LOCAL found AS LOGIC
                    IF SELF:_oRdd:_isValid
                        // Get Current Key
                        VAR currentKeyBuffer := SELF:_currentvalue:Key
                        IF deletedState .OR. SELF:_oRdd:_FilterInfo:Active .OR. seekInfo:SoftSeek .OR. seekInfo:Last
                            SELF:_ToString(seekInfo:Value, SELF:_keySize, SELF:_keyDecimals, SELF:_newValue:Key, REF SELF:_newKeyLen)
                            strCmp := SELF:__Compare(abNewKey, currentKeyBuffer, len)
                            found := (strCmp == 0)
                            IF needPadStr .AND. !found
                                IF SELF:_Descending
                                    SELF:_newValue:Key[len] := Byte.MaxValue
                                    temp:= currentKeyBuffer[len]
                                    currentKeyBuffer[len] := 1
                                    strCmpMaxMin := SELF:__Compare(abNewKey, currentKeyBuffer, padLen)
                                    IF strCmp < 0 .AND. strCmpMaxMin > 0
                                        found := TRUE
                                    ENDIF
                                    IF !found
                                        SELF:_newValue:Key[len] := 1
                                        currentKeyBuffer[len] := Byte.MaxValue
                                        strCmpMaxMin := SELF:__Compare(abNewKey, currentKeyBuffer, padLen)
                                        IF strCmp > 0 .AND. strCmpMaxMin < 0
                                            found := TRUE
                                        ENDIF
                                    ENDIF
                                ELSE
                                    SELF:_newValue:Key[len] := 1
                                    temp:= currentKeyBuffer[len]
                                    currentKeyBuffer[len] := Byte.MaxValue
                                    strCmpMaxMin := SELF:__Compare(SELF:_newValue:Key, currentKeyBuffer, padLen)
                                    IF strCmp > 0 .AND. strCmpMaxMin < 0
                                        found := TRUE
                                    ENDIF
                                    IF !found
                                        SELF:_newValue:Key[len] := Byte.MaxValue
                                    	currentKeyBuffer[len] := 1
                                        strCmpMaxMin := SELF:__Compare(SELF:_newValue:Key, currentKeyBuffer, padLen)
                                        IF strCmp < 0 .AND. strCmpMaxMin > 0
                                            found := TRUE
                                        ENDIF
                                    ENDIF
                                ENDIF
                                SELF:_newValue:Key[len] := 0
                                currentKeyBuffer[len] := temp
                                seekInfo:SoftSeek := fSoft
                            ENDIF
                            IF found
                                IF seekInfo:Last
                                    DO WHILE strCmp == 0
                                        recnoOK := recno
                                        recno := SELF:_nextKey(1)
                                        IF deletedState .OR. SELF:_oRdd:_FilterInfo:Active
                                            recno := SELF:_skipFilter(recno, SkipDirection.Forward)
                                            IF SELF:_oRdd:_Eof .OR. recno == recnoOK
                                                EXIT
                                            ENDIF
                                        ENDIF
                                        IF recno == 0
                                            EXIT
                                        ENDIF
                                        currentKeyBuffer := SELF:_currentvalue:Key
                                        strCmp := SELF:__Compare(SELF:_newValue:Key, currentKeyBuffer, len)
                                        IF strCmp != 0
                                            recno := SELF:_nextKey(-1)
                                            EXIT
                                        ENDIF
                                    ENDDO
                                    recno := recnoOK
                                    result := SELF:_oRdd:__GoTo(recno)
                                    IF recno != 0
                                        found := TRUE
                                    ENDIF
                                ENDIF
                            ELSE
                                IF seekInfo:Last
                                    diff := strCmp
                                    recno := SELF:_nextKey(-1)
                                    strCmp := SELF:__Compare(SELF:_newValue:Key, currentKeyBuffer, len)
                                    found := (strCmp == 0)
                                    IF found
                                        result := SELF:_oRdd:__Goto(recno)
                                    ELSE
                                        IF diff == -strCmp
                                            found := TRUE
                                            result := SELF:_oRdd:__Goto(recno)
                                        ELSE
                                            result := SELF:_oRdd:__Goto(0)
                                        ENDIF
                                    ENDIF
                                ELSE
                                    IF !seekInfo:SoftSeek
                                        result := SELF:_oRdd:__Goto(0)
                                    ENDIF
                                ENDIF
                            ENDIF
                        ELSE
                            strCmp := SELF:__Compare(abNewKey, currentKeyBuffer, len)
                            found := (strCmp == 0)
                        ENDIF
                    ELSE
                        found := FALSE
                    ENDIF
                    IF !SELF:_oRdd:_isValid
                        SELF:_TopStack := 0
                    ENDIF
                    SELF:_oRdd:_Bof := (SELF:_oRdd:RecCount == 0)
                    SELF:_oRdd:_Found := found
                    RETURN result
                ENDIF
                RETURN FALSE
                
            FINALLY
                IF locked
                    result := SELF:_unLockForRead()
                ENDIF
            END TRY
            
        PRIVATE METHOD _Seek(dbsi AS DBSEEKINFO , lpval AS OBJECT ) AS LOGIC
            LOCAL byteArray AS BYTE[]
            byteArray := BYTE[]{ SELF:_keySize }
            SELF:_ToString(lpval, SELF:_keySize, SELF:_keyDecimals, byteArray)
            dbsi:SoftSeek := TRUE
            RETURN SELF:_Seek(dbsi, byteArray)
            
    END CLASS
    
END NAMESPACE


