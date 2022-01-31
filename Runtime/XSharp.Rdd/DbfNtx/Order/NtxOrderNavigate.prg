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
                IF SELF:_Scopes[BOTTOMSCOPE]:IsSet
                    result := SELF:_ScopeSeek(DbOrder_Info.DBOI_SCOPEBOTTOM)
                ELSE
                    SELF:_oRdd:GoCold()
                    SELF:_oRdd:Top := FALSE
                    SELF:_oRdd:Bottom := TRUE
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

                IF SELF:_Scopes[TOPSCOPE]:IsSet
                    result := SELF:_ScopeSeek(DbOrder_Info.DBOI_SCOPETOP)
                    IF !SELF:_oRdd:Found
                        SELF:_oRdd:_SetBOF(TRUE)
                    ENDIF
                ELSE
                    SELF:_oRdd:Top := TRUE
                    SELF:_oRdd:Bottom := FALSE
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



        OVERRIDE METHOD Seek(seekInfo AS DbSeekInfo ) AS LOGIC
            LOCAL uiRealLen AS LONG
            LOCAL byteArray AS BYTE[]
            LOCAL nLen      AS LONG
            uiRealLen := 0
            byteArray := BYTE[]{ _keySize }
            // Convert the key to a byte Array
            IF !SELF:_ToString(seekInfo:Value, SELF:_keySize, SELF:_keyDecimals, byteArray, REF uiRealLen)
                SELF:_oRdd:_dbfError( Subcodes.ERDD_VAR_TYPE, Gencode.EG_DATATYPE,SELF:FileName)
                RETURN FALSE
            ENDIF
            IF SELF:_Scopes[TOPSCOPE]:IsSet
                nLen := Math.Min(SELF:_Scopes[TOPSCOPE]:Size , uiRealLen)
                IF SELF:__Compare(byteArray, SELF:_Scopes[TOPSCOPE]:Buffer, nLen) < 0
                    IF seekInfo:SoftSeek
                        RETURN SELF:_ScopeSeek(DbOrder_Info.DBOI_SCOPETOP)
                    ENDIF
                    RETURN SELF:_oRdd:__Goto(0)
                ENDIF
            ENDIF
            IF SELF:_Scopes[BOTTOMSCOPE]:IsSet
                nLen := Math.Min(SELF:_Scopes[BOTTOMSCOPE]:Size , uiRealLen)
                IF SELF:__Compare(byteArray, SELF:_Scopes[BOTTOMSCOPE]:Buffer, nLen) > 0
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
            LOCAL forward := FALSE AS LOGIC
            // Default Position = Current Record
            IF nToSkip == 0
                recno := SELF:_RecNo
            ELSE
                recno := 0
            ENDIF
            forward := nToSkip > 0
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
                        SELF:_oRdd:_SetBOF(recno == 0)
                        SELF:_oRdd:_SetEOF(recno == 0)
                    ELSE
                        recno := 0
                        nToSkip := 0
                    ENDIF
                ELSE
                    IF SELF:_TopStack == 0
                        SELF:_GoToRecno( SELF:_RecNo)
                    ENDIF
                ENDIF

                IF orgToSkip != 0
                    IF SELF:HasScope
                        isBof := SELF:_oRdd:BoF
                        isEof := SELF:_oRdd:EoF
                        var newrec := SELF:_ScopeSkip(nToSkip)
                        IF newrec != -1 // -1  means that there was nothing to do
                            recno := newrec
                        ENDIF
                        IF isBof != SELF:_oRdd:BoF
                            changedBof := TRUE
                            isBof := SELF:_oRdd:BoF
                        ELSE
                            changedBof := FALSE
                        ENDIF
                        IF isEof != SELF:_oRdd:EoF
                            changedEof := TRUE
                            isEof := SELF:_oRdd:EoF
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
                 if recno == 0
                    if forward
                        SELF:_oRdd:_SetEOF(TRUE)
                    else
                        SELF:_oRdd:_SetBOF(TRUE)
                    endif
                else
                    IF !SELF:HasScope
                        RETURN result
                    ENDIF
                    IF changedBof
                        SELF:_oRdd:_SetBOF(isBof)
                    ENDIF
                    IF changedEof
                        SELF:_oRdd:_SetEOF(isEof)
                    ENDIF
                endif
            CATCH ex AS Exception
                SELF:_oRdd:_dbfError(ex, Subcodes.EDB_SKIP,Gencode.EG_CORRUPTION,  "NtxOrder.SkipRaw")
            FINALLY
                IF locked
                    result := SELF:_unLockForRead() .AND. result
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
            page := SELF:_PageList:Read(SELF:CurrentStack:Page)
            node := page[SELF:CurrentStack:Pos]
            IF thisPage
                IF moveDirection == SkipDirection.Backward
                    SELF:CurrentStack:Pos--
                    node:Pos := SELF:CurrentStack:Pos
                ENDIF
                SELF:_saveCurrentRecord(node)
                RETURN node:Recno
            ENDIF

            IF moveDirection == SkipDirection.Forward
                SELF:CurrentStack:Pos++
                node:Pos := SELF:CurrentStack:Pos
                IF node:PageNo != 0
                    RETURN SELF:_locate(NULL, 0, SearchMode.Top, node:PageNo)
                ENDIF
                IF SELF:CurrentStack:Pos == SELF:CurrentStack:Count
                    DO WHILE SELF:_TopStack != 0 .AND. SELF:CurrentStack:Pos == SELF:CurrentStack:Count
                        SELF:PopPage()
                    ENDDO
                    RETURN SELF:_getNextKey(TRUE, SkipDirection.Forward)
                ENDIF
                SELF:_saveCurrentRecord(node)
                RETURN node:Recno
            ENDIF
            IF node:PageNo != 0
                RETURN SELF:_locate(NULL, 0, SearchMode.Bottom, node:PageNo)
            ENDIF
            IF SELF:CurrentStack:Pos == 0
                DO WHILE SELF:_TopStack != 0 .AND. SELF:CurrentStack:Pos == 0
                    SELF:PopPage()
                ENDDO
                RETURN SELF:_getNextKey(TRUE, SkipDirection.Backward)
            ENDIF
            SELF:CurrentStack:Pos--
            node:Pos := SELF:CurrentStack:Pos
            SELF:_saveCurrentRecord(node)
            RETURN node:Recno


        PRIVATE METHOD _findItemPos(record REF LONG , nodePage AS LOGIC ) AS LOGIC
            LOCAL page   AS NtxPage
            LOCAL node   AS NtxPageNode
            IF SELF:_TopStack == 0
                RETURN FALSE
            ENDIF
            page := SELF:_PageList:Read(SELF:CurrentStack:Page)
            node := page[SELF:CurrentStack:Pos]
            IF nodePage
                SELF:CurrentStack:Pos--
                record++
                RETURN TRUE
            ENDIF
            IF node:PageNo != 0
                SELF:_locate(NULL, 0, SearchMode.Bottom, node:PageNo)
                record += (SELF:CurrentStack:Pos + 1)
                SELF:CurrentStack:Pos := 0
                RETURN TRUE
            ENDIF
            IF SELF:CurrentStack:Pos == 0
                DO WHILE SELF:_TopStack != 0 .AND. SELF:CurrentStack:Pos == 0
                    SELF:PopPage()
                ENDDO
                RETURN SELF:_findItemPos(REF record, TRUE)
            ENDIF
            record += SELF:CurrentStack:Pos
            SELF:CurrentStack:Pos := 0
            RETURN TRUE



        PRIVATE METHOD _getScopePos() AS LONG
            LOCAL first AS LONG
            LOCAL last AS LONG
            IF SELF:_Scopes[TOPSCOPE]:IsSet
                IF SELF:__Compare(SELF:_currentvalue:Key, SELF:_Scopes[TOPSCOPE]:Buffer, SELF:_Scopes[TOPSCOPE]:Size) < 0
                    RETURN 0
                ENDIF
            ENDIF
            IF SELF:_Scopes[BOTTOMSCOPE]:IsSet
                IF SELF:__Compare(SELF:_currentvalue:Key, SELF:_Scopes[BOTTOMSCOPE]:Buffer, SELF:_Scopes[BOTTOMSCOPE]:Size) > 0
                    RETURN 0
                ENDIF
            ENDIF
            first := 0
            last := 0
            DO WHILE SELF:_findItemPos(REF last, FALSE)
                NOP
            ENDDO
            IF SELF:_Scopes[TOPSCOPE]:IsSet
                SELF:_ScopeSeek(DbOrder_Info.DBOI_SCOPETOP)
                DO WHILE SELF:_findItemPos(REF first, FALSE)
                    NOP
                ENDDO
            ENDIF
            IF last > first
            RETURN last - first + 1
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
                SELF:_oRdd:_dbfError(Subcodes.ERDD_KEY_EVAL, Gencode.EG_DATATYPE, SELF:FileName)
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
                IF RT_Deleted .OR. SELF:_oRdd:FilterInfo:Active
                    recno := SELF:_skipFilter(recno, SkipDirection.Forward)
                ENDIF
                IF recno == 0
                    SELF:_oRdd:_SetEOF(TRUE)
                    RETURN 0
                ENDIF
                IF SELF:_Scopes[BOTTOMSCOPE]:IsSet
                    VAR nRes := SELF:__Compare(SELF:_currentvalue:Key, SELF:_Scopes[BOTTOMSCOPE]:Buffer, SELF:_Scopes[BOTTOMSCOPE]:Size)
                    VAR lEOF := IIF(SELF:Descending, nRes < 0, nRes > 0)
                    IF lEOF
                        SELF:_oRdd:_SetEOF(TRUE)
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
                        IF RT_Deleted .OR. SELF:_oRdd:FilterInfo:Active
                            recno := SELF:_skipFilter(recno, SkipDirection)
                        ENDIF
                        lNumKeys--
                        IF SkipDirection == SkipDirection.Backward
                            IF SELF:_Scopes[TOPSCOPE]:IsSet
                                VAR nRes := SELF:__Compare(SELF:_currentvalue:Key, SELF:_Scopes[TOPSCOPE]:Buffer, SELF:_Scopes[TOPSCOPE]:Size)
                                VAR lBOF := IIF(SELF:Descending, nRes > 0, nRes < 0)
                                IF lBOF
	                    	        recno := SELF:_getNextKey(FALSE, SkipDirection.Forward)
	                                SELF:_oRdd:_SetBOF(TRUE)
                                    EXIT
                                ENDIF
                            ENDIF
                        ELSE
                            IF SELF:_Scopes[BOTTOMSCOPE]:IsSet
                                IF recno != 0
                                    SELF:_oRdd:_SetEOF(TRUE)
                                    RETURN result
                                ENDIF
                                SELF:_oRdd:_SetBOF(FALSE)
                                VAR nRes := SELF:__Compare(SELF:_currentvalue:Key, SELF:_Scopes[BOTTOMSCOPE]:Buffer, SELF:_Scopes[BOTTOMSCOPE]:Size)
                                VAR lEOF := IIF(SELF:Descending, nRes < 0, nRes > 0)
                                IF lEOF
                                    SELF:_oRdd:_SetEOF(TRUE)
                                    RETURN result
                                ENDIF
                                result := recno
                            ENDIF
                        ENDIF
                    UNTIL !((recno != 0) .AND. (lNumKeys != 0))
                ELSE
                    recno := -1
                ENDIF
            ENDIF
            RETURN recno


        PRIVATE METHOD _ScopeSeek(uiScope AS DbOrder_Info ) AS LOGIC
            LOCAL result AS LOGIC
            LOCAL seekInfo AS DbSeekInfo
            LOCAL obj AS OBJECT
            LOCAL mustSeek AS LOGIC

            result := TRUE
            seekInfo := DbSeekInfo{}
            IF uiScope == DbOrder_Info.DBOI_SCOPETOP
                obj := SELF:_Scopes[TOPSCOPE]:Value
                IF obj == NULL
                    result := SELF:GoTop()
                    mustSeek := FALSE
                ELSE
                    seekInfo:Last := FALSE
                    mustSeek := TRUE
                ENDIF
            ELSE
                obj := SELF:_Scopes[BOTTOMSCOPE]:Value
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
                SELF:_oRdd:Found := SELF:_isBeforeBottomScope()
                IF !SELF:_oRdd:Found
                    SELF:_oRdd:GoTo(0)
                ENDIF
            ENDIF
            RETURN result


        PRIVATE METHOD _isBeforeBottomScope() AS LOGIC
            LOCAL isOk AS LOGIC
            LOCAL itmBottomScope AS OBJECT

            isOk := SELF:_oRdd:Found
            IF !isOk .AND. SELF:_RecNo != 0
                IF SELF:_Scopes[BOTTOMSCOPE]:IsSet
                    itmBottomScope := SELF:_Scopes[BOTTOMSCOPE]:Value
                    SELF:_ToString(itmBottomScope, SELF:_keySize, SELF:_keyDecimals, SELF:_newvalue:Key)
                    // Make sure we only compare the # of characters defined for the bottomScope
                    local nKeyComp as INT
                    nKeyComp := SELF:_Scopes[BOTTOMSCOPE]:Size
                    IF SELF:__Compare(SELF:_newvalue:Key, SELF:_currentvalue:Key, nKeyComp) >= 0
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
            SELF:_oRdd:__Goto(recno)
            SELF:_saveCurrentKey(recno,SELF:_currentvalue)
            IF SELF:_goRecord(SELF:_currentvalue:Key, SELF:_keySize, recno) != recno
                IF SELF:_goRecord(NULL, 0, recno) != recno .and. recno <= SELF:_oRdd:RecCount
                    IF !SELF:_Unique .AND. !SELF:_Conditional .AND. !SELF:_Partial
                        SELF:_oRdd:_dbfError( Subcodes.ERDD_RECNO_MISSING, Gencode.EG_CORRUPTION,SELF:FileName)
                        result := FALSE
                    ENDIF
                    SELF:ClearStack()
                ENDIF
            ENDIF
            IF result
                SELF:_oRdd:__Goto(recno)
            ENDIF
            RETURN result

        PRIVATE METHOD _locateKey( keyBuffer AS BYTE[] , bufferLen AS LONG , searchMode AS SearchMode ) AS LONG
            // Find Key starting at the top of the index
            SELF:ClearStack()
            IF bufferLen > SELF:_keySize
                bufferLen := SELF:_keySize
            ELSE
                IF bufferLen == 0
                    bufferLen := SELF:_keySize
                ENDIF
            ENDIF
            RETURN SELF:_locate(keyBuffer, bufferLen, searchMode, SELF:_firstPageOffset)


        PRIVATE METHOD _locate(keyBuffer AS BYTE[] , keyLength AS LONG , searchMode AS SearchMode , pageOffset AS LONG ) AS LONG
            LOCAL foundPos  AS WORD
            LOCAL page      AS NtxPage
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
                        node:Pos := foundPos := (WORD) ( (minPos + maxPos) / 2)
                        IF SELF:__Compare(node:KeyBytes, keyBuffer, keyLength) >= 0
                            minPos := (WORD) (foundPos + 1)
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
                        IF SELF:__Compare(node:KeyBytes, keyBuffer, keyLength) <= 0
                            minPos := (WORD) (foundPos + 1)
                        ELSE
                            maxPos := foundPos
                        ENDIF
                    ENDDO
                    foundPos := minPos
                ENDIF
            CASE SearchMode.Left
            CASE SearchMode.SoftSeek
                    minPos := 0
                    maxPos := nodeCount
                    DO WHILE minPos < maxPos
                        foundPos := (WORD) ((minPos + maxPos) / 2)
                        node:Pos := foundPos
                        IF SELF:_Descending
                            IF SELF:__Compare(node:KeyBytes, keyBuffer, keyLength) > 0
                                minPos := (WORD) (foundPos + 1)
                            ELSE
                                maxPos := foundPos
                            ENDIF
                        ELSE
                            IF SELF:__Compare(node:KeyBytes, keyBuffer, keyLength) < 0
                                minPos := (WORD) (foundPos + 1)
                            ELSE
                                maxPos := foundPos
                            ENDIF
                        ENDIF
                    ENDDO
                    node:Pos := foundPos := minPos
                    IF searchMode == SearchMode.Left .AND. SELF:__Compare(node:KeyBytes, keyBuffer, keyLength) == 0
                        searchMode := SearchMode.SoftSeek
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
            SELF:CurrentStack:Pos      := foundPos
            SELF:CurrentStack:Page     := pageOffset
            SELF:CurrentStack:Count    := nodeCount

            node:Pos := foundPos
            IF node:PageNo != 0
                RETURN SELF:_locate(keyBuffer, keyLength, searchMode, node:PageNo)
            ENDIF

            IF foundPos < nodeCount .AND. foundPos >= 0
                SWITCH searchMode
                CASE SearchMode.SoftSeek
                CASE SearchMode.Bottom
                CASE SearchMode.Top
                    SELF:_saveCurrentRecord(node)
                    RETURN node:Recno
                CASE SearchMode.Left
                    IF SELF:__Compare(node:KeyBytes, keyBuffer, keyLength) == 0
                        SELF:_saveCurrentRecord(node)
                        RETURN node:Recno
                    ENDIF
                    RETURN 0
                CASE SearchMode.Right
                    RETURN 0
                END SWITCH
            ELSEIF searchMode == SearchMode.SoftSeek
                DO WHILE SELF:_TopStack != 0 .AND. SELF:CurrentStack:Pos == SELF:CurrentStack:Count
                    SELF:PopPage()
                ENDDO
                IF SELF:_TopStack != 0
                    page := SELF:_PageList:Read(SELF:CurrentStack:Page)
                    IF page == NULL
                        SELF:ClearStack()
                        RETURN 0
                    ENDIF
                    node := page[SELF:CurrentStack:Pos]
                    SELF:_saveCurrentRecord(node)
                    RETURN node:Recno
                ENDIF
            ENDIF
            RETURN 0


        PRIVATE METHOD _skipFilter(recno AS LONG , iPolar AS SkipDirection ) AS LONG
            IF SELF:_oRdd:__Goto(recno)
                SELF:_oRdd:SkipFilter((INT) iPolar)
                recno := SELF:_RecNo
            ENDIF
            RETURN recno

        PRIVATE METHOD _Seek(seekInfo AS DbSeekInfo , bSearchKey AS BYTE[] ) AS LOGIC
            LOCAL recno := 0 AS LONG
            LOCAL result := FALSE  AS LOGIC
            LOCAL fSoft := FALSE AS LOGIC
            LOCAL recnoOk := 0 AS LONG
            LOCAL locked := FALSE AS LOGIC
            LOCAL strCmp AS INT
            LOCAL strCmpMaxMin AS INT
            LOCAL diff AS INT
            LOCAL padLen AS INT
            LOCAL needPadStr AS LOGIC
            LOCAL len AS INT
            LOCAL text AS STRING
            LOCAL temp AS BYTE
            LOCAL activeFilter as LOGIC

            activeFilter := XSharp.RuntimeState.Deleted .OR. SELF:_oRdd:FilterInfo:Active
            TRY
                SELF:_oRdd:GoCold()
                locked := SELF:_lockForRead()
                IF ! locked
                   RETURN FALSE
                ENDIF
                    IF SELF:Shared
                        SELF:_currentvalue:Recno := 0
                    ENDIF
                    needPadStr := FALSE
                    IF seekInfo:Value:GetType() == TYPEOF(STRING)
                        text := (STRING)seekInfo:Value
                        len := text:Length
                        padLen := len
                        IF len < SELF:_keySize
                            needPadStr := TRUE
                            IF SELF:_Descending
                            	bSearchKey[len] := Byte.MaxValue
                            ELSE
                            	bSearchKey[len] := 1
                            ENDIF
                            padLen := len + 1
                            fSoft := seekInfo:SoftSeek
                            seekInfo:SoftSeek := TRUE
                        ENDIF
                    ELSE
                        len := SELF:_keySize
                        padLen := len
                    ENDIF
                recno := SELF:_locateKey(bSearchKey, padLen, IIF(seekInfo:SoftSeek , SearchMode.SoftSeek , SearchMode.Left))
                result := SELF:_oRdd:__Goto(recno)
                IF activeFilter
                    SELF:_oRdd:SkipFilter(1)
                    recno := SELF:_RecNo
                ENDIF
                LOCAL found := false AS LOGIC
                IF SELF:_oRdd:_isValid
                    // Get Current Key
                    VAR currentKeyBuffer := SELF:_currentvalue:Key
                    // Note: Softseek will also be set when an incomplete key is passed
                    IF activeFilter .OR. seekInfo:SoftSeek .OR. seekInfo:Last
                            SELF:_ToString(seekInfo:Value, SELF:_keySize, SELF:_keyDecimals, SELF:_newvalue:Key, REF SELF:_newKeyLen)
                            strCmp := SELF:__Compare(bSearchKey, currentKeyBuffer, len)
                            found := (strCmp == 0)
                            IF needPadStr .AND. !found
                                IF SELF:_Descending
                                    SELF:_newvalue:Key[len] := Byte.MaxValue
                                    temp:= currentKeyBuffer[len]
                                    currentKeyBuffer[len] := 1
                                    strCmpMaxMin := SELF:__Compare(bSearchKey, currentKeyBuffer, padLen)
                                    IF strCmp < 0 .AND. strCmpMaxMin > 0
                                        found := TRUE
                                    ENDIF
                                    IF !found
                                        SELF:_newvalue:Key[len] := 1
                                        currentKeyBuffer[len] := Byte.MaxValue
                                    strCmpMaxMin := SELF:__Compare(bSearchKey, currentKeyBuffer, padLen)
                                        IF strCmp > 0 .AND. strCmpMaxMin < 0
                                            found := TRUE
                                        ENDIF
                                    ENDIF
                                ELSE
                                    SELF:_newvalue:Key[len] := 1
                                    temp:= currentKeyBuffer[len]
                                    currentKeyBuffer[len] := Byte.MaxValue
                                    strCmpMaxMin := SELF:__Compare(SELF:_newvalue:Key, currentKeyBuffer, padLen)
                                    IF strCmp > 0 .AND. strCmpMaxMin < 0
                                        found := TRUE
                                    ENDIF
                                    IF !found
                                        SELF:_newvalue:Key[len] := Byte.MaxValue
                                    	currentKeyBuffer[len] := 1
                                        strCmpMaxMin := SELF:__Compare(SELF:_newvalue:Key, currentKeyBuffer, padLen)
                                        IF strCmp < 0 .AND. strCmpMaxMin > 0
                                            found := TRUE
                                        ENDIF
                                    ENDIF
                                ENDIF
                                SELF:_newvalue:Key[len] := 0
                                currentKeyBuffer[len] := temp
                                seekInfo:SoftSeek := fSoft
                            ENDIF
                            IF found
                            // we are on the first matching key. When we seek Last then we
                            // skip to the last record that matches the key that we searched for
                                IF seekInfo:Last
                                    DO WHILE strCmp == 0
                                        recnoOk := recno
                                        recno := SELF:_nextKey(1)
                                        IF activeFilter
                                            recno := SELF:_skipFilter(recno, SkipDirection.Forward)
                                            IF SELF:_oRdd:EoF .OR. recno == recnoOk
                                                EXIT
                                            ENDIF
                                        ENDIF
                                        IF recno == 0
                                            EXIT
                                        ENDIF
                                        currentKeyBuffer := SELF:_currentvalue:Key
                                        strCmp := SELF:__Compare(SELF:_newvalue:Key, currentKeyBuffer, len)
                                        IF strCmp != 0
                                            recno := SELF:_nextKey(-1)
                                            EXIT
                                        ENDIF
                                    ENDDO
                                    recno := recnoOk
                                result := SELF:_GoToRecno(recno)
                                    IF recno != 0
                                        found := TRUE
                                    ENDIF
                                ENDIF
                            ELSE
                            // Not found, why are we doing this ?
                                IF seekInfo:Last
                                    diff := strCmp
                                    recno := SELF:_nextKey(-1)
                                    strCmp := SELF:__Compare(SELF:_newvalue:Key, currentKeyBuffer, len)
                                    found := (strCmp == 0)
                                    IF found
                                        result := SELF:_oRdd:__Goto(recno)
                                    ELSE
                                        IF diff == -strCmp
                                            found := TRUE
                                        result := SELF:_GoToRecno(recno)
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
                    ENDIF
                    strCmp := SELF:__Compare(bSearchKey, currentKeyBuffer, len)
                    found := (strCmp == 0)
                ELSE
                    found := FALSE
                ENDIF
                IF !SELF:_oRdd:_isValid
                    SELF:ClearStack()
                ENDIF
                SELF:_oRdd:_SetBOF(SELF:_oRdd:RecCount == 0)
                SELF:_oRdd:Found := found
                RETURN result

            FINALLY
                IF locked
                    result := SELF:_unLockForRead()
                ENDIF
            END TRY

        PRIVATE METHOD _Seek(dbsi AS DbSeekInfo , lpval AS OBJECT ) AS LOGIC
            LOCAL byteArray AS BYTE[]
            byteArray := BYTE[]{ SELF:_keySize }
            SELF:_ToString(lpval, SELF:_keySize, SELF:_keyDecimals, byteArray)
            dbsi:SoftSeek := TRUE
            RETURN SELF:_Seek(dbsi, byteArray)

    END CLASS

END NAMESPACE


