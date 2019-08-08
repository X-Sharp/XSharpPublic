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

BEGIN NAMESPACE XSharp.RDD.CDX

    INTERNAL PARTIAL SEALED CLASS CdxTag

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
                    locked := SELF:SLock()
                    IF !locked
                        RETURN FALSE
                    ENDIF
                    LOCAL recno AS LONG
                    SELF:ClearStack()
                    IF SELF:Descending
                        recno := SELF:_locateFirst(SELF:_rootPage)
                    ELSE
                        recno := SELF:_locateLast(SELF:_rootPage)
                    ENDIF

                    result := SELF:_oRdd:__Goto(recno)
                    IF result
                        result := SELF:_oRdd:SkipFilter(-1)
                    ENDIF
                ENDIF
                RETURN result
            FINALLY
                IF locked
                    SELF:UnLock()
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
                    locked := SELF:SLock()
                    IF !locked
                        RETURN FALSE
                    ENDIF
                    LOCAL recno AS LONG
                    SELF:ClearStack()
                    IF SELF:Descending
                        recno := SELF:_locateLast(SELF:_rootPage)
                    ELSE
                        recno := SELF:_locateFirst(SELF:_rootPage)
                    ENDIF
                    result := SELF:_oRdd:__Goto(recno)
                    IF result
                        result := SELF:_oRdd:SkipFilter(1)  
                    ENDIF
                ENDIF
                RETURN result    
            FINALLY
                IF locked
                    result := SELF:UnLock()
                ENDIF
            END TRY
            
            
            
        PUBLIC METHOD Seek(seekInfo AS DBSEEKINFO ) AS LOGIC
            LOCAL uiRealLen AS LONG
            LOCAL byteArray AS BYTE[]
            LOCAL nLen      AS LONG
            uiRealLen := 0
            byteArray := BYTE[]{ _keySize }
            // Convert the key to a byte Array
            IF !SELF:_ToString(seekInfo:Value, SELF:_keySize, byteArray, REF uiRealLen)
                SELF:_oRdd:_dbfError( SubCodes.ERDD_VAR_TYPE, GenCode.EG_DATATYPE,SELF:fileName)
                RETURN FALSE
            ENDIF
            LOCAL nScopeTop AS LONG
            LOCAL nScopeBottom AS LONG
            nScopeTop    := SELF:TopScopeNo
            nScopeBottom := SELF:BottomScopeNo
            // compare with scopes widhout checking for descending
            IF SELF:_Scopes[nScopeTop]:IsSet
                nLen := Math.Min(SELF:_Scopes[nScopeTop]:Size, uiRealLen)
                IF SELF:__Compare(byteArray, SELF:_Scopes[nScopeTop]:Buffer, nLen) < 0
                    IF seekInfo:SoftSeek
                        RETURN SELF:_ScopeSeek(DBOrder_Info.DBOI_SCOPETOP)
                    ENDIF
                    RETURN SELF:_oRdd:__Goto(0)
                ENDIF
            ENDIF
            IF SELF:_Scopes[nScopeBottom]:IsSet
                nLen := Math.Min(SELF:_Scopes[nScopeBottom]:Size, uiRealLen)
                IF SELF:__Compare(byteArray, SELF:_Scopes[nScopeBottom]:Buffer, nLen) > 0
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
                locked := SELF:SLock()
                IF !locked
                    RETURN FALSE
                ENDIF
                IF SELF:Descending
                    nToSkip := - nToSkip
                ENDIF
                IF !SELF:_oRdd:_isValid
                    IF nToSkip < 0
                        recno := SELF:_locateKey(NULL, 0, SearchMode.Bottom,0)
                        nToSkip++
                    ELSE
                        recno := 0
                        nToSkip := 0
                    ENDIF
                ELSE
                    IF SELF:Stack:Empty
                        SELF:_GoToRecno( SELF:_Recno)
                    ENDIF
                ENDIF
                
                IF orgToSkip != 0
                    IF SELF:HasScope
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
                IF !SELF:HasScope
                    RETURN result
                ENDIF
                IF changedBof
                    SELF:_oRdd:_Bof := isBof
                ENDIF
                IF changedEof
                    SELF:_oRdd:_Eof := isEof
                ENDIF
            CATCH ex AS Exception
                SELF:_oRdd:_dbfError(ex, SubCodes.EDB_SKIP,GenCode.EG_CORRUPTION,  "CdxTag.SkipRaw") 
            FINALLY
                IF locked
                    result := SELF:UnLock() .AND. result
                ENDIF
            END TRY
            RETURN result
            
            
        PRIVATE METHOD _getNextKey(moveDirection AS SkipDirection ) AS LONG
            LOCAL page  AS CdxTreePage
            LOCAL node  AS CdxPageNode
            // No page loaded ?
            IF SELF:Stack:Empty
                RETURN 0
            ENDIF
            VAR topStack := SELF:CurrentStack
            page    := topStack:Page
            node    := page[topStack:Pos]

            IF moveDirection == SkipDirection.Forward
                topStack:Pos++
                node:Pos := topStack:Pos
                IF node:Pos < page:NumKeys .AND. node:ChildPageNo != 0
                    RETURN SELF:_locate(NULL, 0, SearchMode.Top, node:ChildPageNo,0)
                ENDIF
                // Once we are at the bottom level then we simply skip forward using the Right Pointers
                IF topStack:Pos == page:Numkeys
                    IF page:HasRight
                        VAR rightPtr := page:RightPtr
                        VAR newpage := SELF:GetPage(rightPtr)
                        SELF:Stack:Replace(page, newpage, 0)
                        node    := newpage[0]
                        SELF:_saveCurrentRecord(node)
                        RETURN node:Recno
                    ELSE
                        // At the end of the leaf list
                        SELF:ClearStack()
                        RETURN 0
                    ENDIF
                ENDIF
                IF node:Pos >= page:NumKeys
                    RETURN 0
                ENDIF
                SELF:_saveCurrentRecord(node)
                RETURN node:Recno
            ENDIF
            IF node:ChildPageNo != 0
                RETURN SELF:_locate(NULL, 0, SearchMode.Bottom, node:ChildPageNo,0)
            ENDIF
            IF topStack:Pos == 0
                IF page:HasLeft
                    VAR leftPtr := page:LeftPtr
                    VAR newpage   := SELF:GetPage(leftPtr)
                    SELF:Stack:Replace(page, newpage, newpage:NumKeys)
                ELSE
                    // At the end of the leaf list
                    SELF:ClearStack()
                    RETURN 0
                ENDIF
                RETURN SELF:_getNextKey(SkipDirection.Backward)
            ENDIF
            topStack:Pos--
            node:Pos := topStack:Pos
            SELF:_saveCurrentRecord(node)
            RETURN node:Recno
            
            
        PRIVATE METHOD _findItemPos() AS LONG
            
            IF SELF:Stack:Empty
                return -1
            ENDIF
            // this assumes the top of the stack has the leaf page where our key is.
            // we count the # of keys to the right of that page and add the pos on the top of the stack
            VAR topStack := SELF:Stack:Top
            local page  := (CdxTreePage) topStack:Page as CdxTreePage
            local pos as LONG
            pos := TopStack:Pos
            do while page:HasLeft
                var nextPage := page:LeftPtr
                page := self:GetPage(nextPage)
                pos += page:NumKeys
            enddo
            RETURN pos
            
              
            
        PRIVATE METHOD _getScopePos() AS LONG
            LOCAL first AS LONG
            LOCAL last AS LONG
            local nDiff as LONG
            // Use numbers and not properties here for simplicity of code.
            // that way we don't have to check for the direction of the difference in combination with the descending flag
            IF SELF:_Scopes[0]:IsSet
                nDiff := SELF:__Compare(SELF:_currentValue:Key, SELF:_Scopes[0]:Buffer, SELF:_Scopes[0]:Size) 
                IF nDiff < 0
                    RETURN 0
                ENDIF
            ENDIF
            IF SELF:_Scopes[1]:IsSet
                nDiff := SELF:__Compare(SELF:_currentValue:Key, SELF:_Scopes[1]:Buffer, SELF:_Scopes[1]:Size)
                IF nDiff > 0
                    RETURN 0
                ENDIF
            ENDIF
            
            last := SELF:_findItemPos()
            IF SELF:HasTopScope
                SELF:_ScopeSeek(DBOrder_Info.DBOI_SCOPETOP)
                first := SELF:_findItemPos()
            ELSE
                first := 1
            ENDIF
            IF last > first
                RETURN last - first + 1
            ENDIF
            RETURN first - last + 1
            
            
        INTERNAL METHOD _saveCurrentKey(rcno AS LONG, oData AS RddKeyData) AS LOGIC
            LOCAL isOk AS LOGIC

            isOk := TRUE
            oData:Recno := rcno
            isOk := SELF:getKeyValue(SELF:_SourceIndex, oData:Key)
            IF SELF:_Conditional
                oData:ForCond := SELF:_EvalBlock(SELF:_ForCodeBlock, TRUE)
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
                recno := SELF:_getNextKey(SkipDirection.Forward)
                IF RT_Deleted .OR. SELF:_oRdd:_FilterInfo:Active
                    recno := SELF:_skipFilter(recno, SkipDirection.Forward)
                ENDIF
                IF recno == 0
                    SELF:_oRdd:_Eof := TRUE
                    RETURN 0
                ENDIF
                // Note we hardcoded the ranges here on purpose. Otherwise it does NOT work
                IF SELF:_Scopes[BOTTOMSCOPE]:IsSet
                    LOCAL nRes AS LONG
                    nRes := SELF:__Compare(SELF:_currentvalue:Key, SELF:_Scopes[BOTTOMSCOPE]:Buffer, SELF:_Scopes[BOTTOMSCOPE]:Size)
                    VAR lEOF := nRes > 0
                    IF lEOF
                        IF SELF:Descending
                            recno := SELF:_getNextKey(SkipDirection.Forward)
                            SELF:_oRdd:_Bof := TRUE
                        ELSE
                            SELF:_oRdd:_Eof := TRUE
                        ENDIF
                        RETURN result
                    ENDIF
                ENDIF
            ELSE
                IF lNumKeys < 0
                    lNumKeys        := -lNumKeys
                    SkipDirection   := SkipDirection.Backward
                ELSE
                    SkipDirection   := SkipDirection.Forward
                ENDIF
                IF lNumKeys != 0
                    REPEAT
                        recno := SELF:_getNextKey( SkipDirection)
                        IF rT_Deleted .OR. SELF:_oRdd:_FilterInfo:Active
                            recno := SELF:_skipFilter(recno, SkipDirection)
                        ENDIF
                        lNumKeys--
                        // Note we hardcoded the ranges here on purpose. Otherwise it does NOT work
                        IF SkipDirection == SkipDirection.Backward
                            IF SELF:_Scopes[TopScope]:IsSet
                                LOCAL nRes AS LONG
                                nRes := SELF:__Compare(SELF:_currentvalue:Key, SELF:_Scopes[TopScope]:Buffer, SELF:_Scopes[TopScope]:Size) 
                                VAR lBOF := nRes < 0
                                IF lBOF
                                    IF SELF:Descending
                                        SELF:_oRdd:_Eof := TRUE
                                    ELSE
                                        recno := SELF:_getNextKey(SkipDirection.Forward)
                                        SELF:_oRdd:_Bof := TRUE
                                    ENDIF
                                    EXIT
                                ENDIF
                            ENDIF
                        ELSE
                            IF SELF:_Scopes[BottomScope]:IsSet
                                SELF:_oRdd:_Bof := FALSE
                                LOCAL nRes AS LONG
                                nRes := SELF:__Compare(SELF:_currentvalue:Key, SELF:_Scopes[BottomScope]:Buffer, SELF:_Scopes[BottomScope]:Size) 
                                VAR lEOF := nRes > 0
                                IF lEOF
                                    IF SELF:Descending
                                        recno := SELF:_getNextKey(SkipDirection.Forward)
                                        SELF:_oRdd:_Bof := TRUE
                                    ELSE
                                        SELF:_oRdd:_Eof := TRUE
                                    ENDIF
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
                obj     := SELF:_Scopes[TopScopeNo]:Value
                IF obj == NULL
                    result      := SELF:GoTop()
                    mustSeek    := FALSE
                ELSE
                    seekInfo:Last := SELF:Descending
                    mustSeek      := TRUE
                ENDIF
            ELSE
                obj := SELF:_Scopes[BottomScopeNo]:Value
                IF obj == NULL
                    result      := SELF:GoBottom()
                    mustSeek    := FALSE
                ELSE
                    seekInfo:Last   := !SELF:Descending
                    mustSeek        := TRUE
                ENDIF
            ENDIF
            IF mustSeek
                seekInfo:Value      := obj
                seekInfo:SoftSeek   := TRUE
                result              := SELF:_Seek(seekInfo, obj)
                SELF:_oRdd:_Found   := SELF:_isBeforeBottomScope()
                IF !SELF:_oRdd:_Found
                    SELF:_oRdd:GoTo(0)
                ENDIF
            ENDIF
            RETURN result
            
            
        PRIVATE METHOD _isBeforeBottomScope() AS LOGIC
            LOCAL isOk AS LOGIC
            LOCAL itmBottomScope AS OBJECT
            isOk := SELF:_oRdd:_Found
            IF !isOk .AND. SELF:_RecNo != 0
                IF SELF:HasBottomScope
                    itmBottomScope := SELF:_Scopes[BottomScopeNo]:VALUE
                    SELF:_ToString(itmBottomScope, SELF:_keySize, SELF:_newValue:Key)
                    // Make sure we only compare the # of characters defined for the bottomScope
                    local nKeyComp as INT
                    nKeyComp := SELF:_Scopes[BottomScopeNo]:Size
                    IF SELF:__Compare(SELF:_newValue:Key, SELF:_currentValue:Key, nKeyComp) >= 0
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
            recno := SELF:_locateKey(keyBytes, keyLen, SearchMode.Left, gotoRec)
            // Now, move until we found the right Recno
            DO WHILE recno != 0 .AND. recno != gotoRec
                recno := SELF:_getNextKey(SkipDirection.Forward)
            ENDDO
            RETURN recno
            
            
        INTERNAL METHOD _GoToRecno(recno AS LONG ) AS LOGIC
            LOCAL result AS LOGIC
            result := TRUE
            SELF:_oRDD:__GoTo(recno)
            SELF:_saveCurrentKey(recno,SELF:_currentValue)
            IF SELF:_goRecord(SELF:_currentValue:Key, SELF:_keySize, recno) != recno
                IF SELF:_goRecord(NULL, 0, recno) != recno .and. recno <= self:_oRDD:Reccount
                    IF !SELF:Unique .AND. !SELF:Conditional .AND. !SELF:Custom
                        SELF:_oRdd:_dbfError( SubCodes.ERDD_RECNO_MISSING, GenCode.EG_CORRUPTION,SELF:fileName)
                        result := FALSE
                    ENDIF
                    SELF:Stack:Clear()
                ENDIF
            ENDIF
            IF result
                SELF:_oRDD:__GoTo(recno)
            ENDIF
            RETURN result
            
        PRIVATE METHOD _locateKey( keyBuffer AS BYTE[] , bufferLen AS LONG , searchMode AS SearchMode,recNo AS LONG ) AS LONG
            // Find Key starting at the top of the index
            SELF:ClearStack()
            IF bufferLen > SELF:_keySize
                bufferLen := SELF:_keySize
            ELSE
                IF bufferLen == 0
                    bufferLen := SELF:_keySize
                ENDIF
            ENDIF
            RETURN SELF:_locate(keyBuffer, bufferLen, searchMode, SELF:_rootPage, recNo)
            
        PRIVATE METHOD _locateFirst(pageOffSet AS LONG) AS LONG
            VAR page := SELF:GetPage(pageOffset)
            IF page == NULL
                SELF:ClearStack()
                RETURN 0
            ENDIF
            SELF:PushPage(page, 0)
            IF page IS CdxBranchPage VAR branchPage
                LOCAL nChildPage AS LONG
                nChildPage := branchPage:GetChildPage(0)
                RETURN SELF:_locateFirst(nChildPage)
            ENDIF
            VAR node := page[0]
            SELF:_saveCurrentRecord(node)
            RETURN node:Recno
               
            
        PRIVATE METHOD _locateLast(pageOffSet AS LONG) AS LONG
            VAR page := SELF:GetPage(pageOffset)
            IF page == NULL
                SELF:ClearStack()
                RETURN 0
            ENDIF
            SELF:PushPage(page, page:NumKeys-1)
            IF page IS CdxBranchPage VAR branchPage
                LOCAL nChildPage AS LONG
                nChildPage := branchPage:GetChildPage(page:NumKeys-1)
                RETURN SELF:_locateLast(nChildPage)
            ENDIF
            VAR node := page[page:NumKeys-1]
            SELF:_saveCurrentRecord(node)
            RETURN node:Recno


        PRIVATE METHOD _locate(keyBuffer AS BYTE[] , keyLength AS LONG , searchMode AS SearchMode , pageOffset AS LONG, recNo AS LONG) AS LONG
            LOCAL foundPos  AS WORD
            LOCAL page      AS CdxTreePage
            LOCAL nodeCount AS WORD
            LOCAL node      AS CdxPageNode
            LOCAL minPos    AS WORD
            LOCAL maxPos    AS WORD
            LOCAL atEOF     AS LOGIC
            // find a key starting at the pageOffSet passed 
            foundPos := 0
            atEOF    := FALSE
            //Load the page at pageOffset
            page := SELF:GetPage(pageOffset)
            IF page == NULL
                SELF:ClearStack()
                RETURN 0
            ENDIF
            VAR topStack      := SELF:CurrentStack
            // How many Items in that page ?
            nodeCount := page:NumKeys
            IF (nodeCount == 0)
                SELF:PushPage(page, 0)
                RETURN   0
            ENDIF
            // Get the first node on the page
            node := page[0]
            
            SWITCH searchMode
            CASE SearchMode.Right
                foundPos := page:FindKey(keyBuffer, recNo, keyLength)
                IF page IS CdxBranchPage .AND. foundPos >= nodeCount
                    foundPos := nodeCount-1
                ENDIF
                node:Pos := foundPos
            CASE SearchMode.Left
            CASE SearchMode.LeftFound
                minPos := 0
                maxPos := nodeCount
                VAR found := FALSE 
                DO WHILE minPos < maxPos
                    foundPos := (minPos + maxPos) / 2
                    node:Pos := foundPos
                    VAR cmp := SELF:__Compare(node:KeyBytes, keyBuffer, keyLength)
                    IF cmp >= 0
                        found := TRUE
                    ENDIF
                        IF cmp  < 0
                        minPos := foundPos + 1
                    ELSE
                        maxPos := foundPos
                    ENDIF
                    IF minPos >= maxPos .AND. ! found
                        // all keys are smaller than what we are looking for
                        IF page:HasRight
                            pageOffset  := page:RightPtr
                            page        := SELF:GetPage(pageOffset)
                            nodeCount   := page:NumKeys
                            minPos := 0
                            maxPos := nodeCount
                        ELSE
                            // the key we are looking for is at EOF of the file
                            atEOF := TRUE
                        ENDIF
                    ENDIF
                    
                ENDDO
                foundPos := minPos
                IF foundPos >= nodeCount
                    foundPos -= 1
                ENDIF
                node:Pos := foundPos
                IF searchMode == SearchMode.Left .AND. foundPos < nodeCount .AND. SELF:__Compare(node:KeyBytes, keyBuffer, keyLength) == 0
                    searchMode := SearchMode.LeftFound
                ENDIF
            CASE SearchMode.Bottom
                IF nodeCount > 0
                    foundPos := nodeCount-1
                    node:Pos := foundPos
                ELSE
                    foundPos := 0
                    node:Pos := foundPos
                ENDIF
            CASE SearchMode.Top
                foundPos := 0
                node:Pos := foundPos
            END SWITCH
            // Add info in the stack

            IF atEOF .AND. searchMode != SearchMode.Bottom
                SELF:_locateKey(NULL, 0, SearchMode.Bottom,0)
                RETURN 0
            ENDIF

            SELF:PushPage(page, foundPos)
            IF page IS CdxBranchPage VAR bPage
#ifdef TESTCDX
                //bPage:ValidateLevel()
#endif
                RETURN SELF:_locate(keyBuffer, keyLength, searchMode, node:ChildPageNo,recNo)
            ENDIF
            
            IF foundPos < nodeCount .AND. foundPos >= 0
                SWITCH searchMode
                CASE SearchMode.LeftFound
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
            ELSEIF searchMode == SearchMode.LeftFound
                DO WHILE ! SELF:Stack:Empty .AND. topStack:Pos == topStack:Page:NumKeys
                    topStack := SELF:PopPage()
                ENDDO
                IF ! SELF:Stack:Empty
                    page := SELF:Stack:Top:Page
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
            LOCAL oldDescend as LOGIC
            recno := 0
            result := FALSE
            fSoft := FALSE
            recnoOK := 0
            locked := FALSE
            oldDescend   := SELF:Descending
            deletedState := XSharp.RuntimeState.Deleted
            TRY
                SELF:Descending := FALSE
                if oldDescend
                    seekInfo:Last := ! seekInfo:Last
                ENDIF
                SELF:_oRdd:GoCold()
                locked := SELF:SLock()
                IF locked
                    IF SELF:Shared
                        SELF:_currentValue:Recno := 0
                    ENDIF
                    needPadStr := FALSE
                    IF seekInfo:Value:GetType() == TYPEOF(STRING)
                        text    := (STRING)seekInfo:Value
                        len     := text:Length
                        IF SELF:_Collation != NULL
                            len := 0
                            DO WHILE abNewKey[len] != 0 .AND. len < abNewKey:Length
                                len++
                            ENDDO
                        ENDIF
                        padLen := len
                        IF len < SELF:_sourcekeySize
                            needPadStr := TRUE
                            abNewKey[len] := 1
                            padLen := len + 1
                            fSoft := seekInfo:SoftSeek
                            seekInfo:SoftSeek := TRUE
                        ENDIF
                    ELSE
                        len := SELF:_keySize
                        padLen := len
                    ENDIF
                    recno := SELF:_locateKey(abNewKey, padLen, IIF(seekInfo:SoftSeek , SearchMode.LeftFound , SearchMode.Left),0)
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
                            SELF:_ToString(seekInfo:Value, SELF:_keySize, SELF:_newValue:Key, REF SELF:_newKeyLen)
                            strCmp := SELF:__Compare(abNewKey, currentKeyBuffer, len)
                            found := (strCmp == 0)
                            IF needPadStr .AND. !found
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
                                    result := SELF:_GotoRecno(recno)
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
                                            result := SELF:_GotoRecno(recno)
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
                        SELF:ClearStack()
                    ENDIF
                    SELF:_oRdd:_Bof := (SELF:_oRdd:RecCount == 0)
                    SELF:_oRdd:_Found := found
                    RETURN result
                ENDIF
                RETURN FALSE
                
            FINALLY
                SELF:Descending := oldDescend
                IF locked
                    result := SELF:UnLock()
                ENDIF
            END TRY
            
        PRIVATE METHOD _Seek(dbsi AS DBSEEKINFO , lpval AS OBJECT ) AS LOGIC
            LOCAL byteArray AS BYTE[]
            byteArray := BYTE[]{ SELF:_keySize }
            SELF:_ToString(lpval, SELF:_keySize, byteArray)
            dbsi:SoftSeek := TRUE
            RETURN SELF:_Seek(dbsi, byteArray)
            
    END CLASS
    
END NAMESPACE


