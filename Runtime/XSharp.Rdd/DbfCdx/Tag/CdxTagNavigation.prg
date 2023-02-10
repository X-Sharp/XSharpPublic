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

        // Methods for walking indices, so GoTop, GoBottom, Skip and Seek
        PRIVATE _mustCheckEof AS LOGIC
        INTERNAL PROPERTY EmptyResultSet AS LOGIC
            GET
                IF !SELF:_Valid
                    SELF:_oRdd:_SetEOF(TRUE)
                    RETURN TRUE
                ENDIF
                IF SELF:Shared .OR. _mustCheckEof .OR. SELF:_oRdd:MustForceRel
                    RETURN FALSE
                ENDIF
                SELF:_mustCheckEof := FALSE
                RETURN SELF:_oRdd:EoF .AND. SELF:_oRdd:BoF
            END GET
        END PROPERTY

        INTERNAL METHOD ThrowException(nSubcode AS Subcodes, nGenCode AS Gencode, cMessage AS STRING) AS VOID
            SELF:_oRdd:_dbfError( nSubcode, nGenCode,cMessage)
            RETURN
        INTERNAL METHOD ThrowException(nSubcode AS Subcodes, nGenCode AS Gencode, cFunction AS STRING, cMessage AS STRING) AS VOID
            SELF:_oRdd:_dbfError( nSubcode, nGenCode,cFunction, cMessage)
            RETURN
        INTERNAL METHOD ThrowException(ex AS Exception, nSubcode AS Subcodes, nGenCode AS Gencode, cMessage AS STRING) AS VOID
            SELF:_oRdd:_dbfError( ex, nSubcode, nGenCode,cMessage)
            RETURN

        INTERNAL METHOD GoBottom() AS LOGIC
            LOCAL locked AS LOGIC
            LOCAL result AS LOGIC
            IF SELF:EmptyResultSet
                RETURN TRUE
            ENDIF
            locked := FALSE
            result := FALSE
            TRY
                local done := FALSE AS LOGIC
                IF SELF:HasBottomScope
                    IF SELF:_scopeEmpty .AND. ! SELF:Shared
                        RETURN TRUE
                    ENDIF
                    result := SELF:_ScopeSeek(DbOrder_Info.DBOI_SCOPEBOTTOM)
                    done := ! self:_oRdd:EoF
                ENDIF
                IF ! done
                    SELF:_oRdd:GoCold()
                    SELF:_oRdd:Top := FALSE
                    SELF:_oRdd:Bottom := TRUE
                    locked := SELF:Slock()
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
                SELF:ValidateScopes()
                RETURN result
            FINALLY
                IF locked
                    SELF:UnLock()
                ENDIF
            END TRY

        INTERNAL METHOD GoTop() AS LOGIC
            LOCAL locked AS LOGIC
            LOCAL result AS LOGIC
            IF SELF:EmptyResultSet
                RETURN TRUE
            ENDIF
            locked := FALSE
            TRY
                SELF:_oRdd:GoCold()
                IF SELF:HasTopScope
                    IF SELF:_scopeEmpty .AND. ! SELF:Shared
                        RETURN TRUE
                    ENDIF
                    result := SELF:_ScopeSeek(DbOrder_Info.DBOI_SCOPETOP)
                    IF !SELF:_oRdd:Found
                        SELF:_oRdd:_SetBOF(TRUE)
                        SELF:_oRdd:_SetEOF(TRUE)
                        SELF:_scopeEmpty := TRUE
                    ENDIF
                ELSE
                    SELF:_oRdd:Top := TRUE
                    SELF:_oRdd:Bottom := FALSE
                    locked := SELF:Slock()
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
                    IF (recno > 0)
                        result := SELF:_oRdd:__Goto(recno)
                        IF result
                            result := SELF:_oRdd:SkipFilter(1)
                        ENDIF
                    ELSE
                        result := SELF:_oRdd:__Goto(0)
                        SELF:_oRdd:_SetEOF(TRUE)
                        result := TRUE
                    ENDIF
                ENDIF
                SELF:ValidateScopes()
                RETURN result
            FINALLY
                IF locked
                    result := SELF:UnLock()
                ENDIF
            END TRY

        PRIVATE METHOD __ScopeCompare(bLhs as Byte[], nScope as LONG, nLen := -1 as LONG) AS LONG
            if nLen < 0
                nLen := _Scopes[nScope]:Size
            ENDIF
            RETURN SELF:__Compare(bLhs, _Scopes[nScope]:Buffer, nLen , 0, 0)

        INTERNAL METHOD Seek(seekInfo AS DbSeekInfo ) AS LOGIC
            LOCAL uiRealLen AS LONG
            LOCAL byteArray AS BYTE[]
            LOCAL nLen      AS LONG
            IF SELF:EmptyResultSet
                SELF:_oRdd:Found := FALSE
                RETURN TRUE
            ENDIF
            uiRealLen := 0
            byteArray := BYTE[]{ _keySize }
            // Convert the key to a byte Array
            IF !SELF:_ToString(seekInfo:Value, SELF:_keySize, byteArray, REF uiRealLen)
                SELF:ThrowException(Subcodes.ERDD_VAR_TYPE, Gencode.EG_DATATYPE,SELF:FileName)
                RETURN FALSE
            ENDIF
            LOCAL nScopeTop AS LONG
            LOCAL nScopeBottom AS LONG
            nScopeTop    := SELF:TopScopeNo
            nScopeBottom := SELF:BottomScopeNo
            // compare with scopes widhout checking for descending
            IF SELF:_Scopes[nScopeTop]:IsSet
                nLen := Math.Min(SELF:_Scopes[nScopeTop]:Size, uiRealLen)
                IF SELF:__ScopeCompare(byteArray, nScopeTop, nLen) < 0
                    IF seekInfo:SoftSeek
                        RETURN SELF:_ScopeSeek(DbOrder_Info.DBOI_SCOPETOP)
                    ENDIF
                    RETURN SELF:_oRdd:__Goto(0)
                ENDIF
            ENDIF
            IF SELF:_Scopes[nScopeBottom]:IsSet
                nLen := Math.Min(SELF:_Scopes[nScopeBottom]:Size, uiRealLen)
                IF SELF:__ScopeCompare(byteArray, nScopeBottom, nLen) > 0
                    RETURN SELF:_oRdd:__Goto(0)
                ENDIF
            ENDIF
            var result := SELF:_Seek(seekInfo, byteArray)
            SELF:ValidateScopes()
            RETURN result
        INTERNAL METHOD ValidateScopes() AS LOGIC
            LOCAL nLen, nDiff as LONG
            LOCAL nScopeTop AS LONG
            LOCAL nScopeBottom AS LONG
            LOCAL lTopOk := TRUE as LOGIC
            LOCAL lBotOk := TRUE as LOGIC

            nScopeTop    := SELF:TopScopeNo
            nScopeBottom := SELF:BottomScopeNo

            IF SELF:_Scopes[nScopeTop]:IsSet .and. SELF:_Scopes[nScopeBottom]:IsSet
                LOCAL lEmpty := FALSE AS LOGIC
                nLen := Math.Min(SELF:_Scopes[nScopeTop]:Size, SELF:_Scopes[nScopeBottom]:Size)
                nDiff := SELF:__ScopeCompare(SELF:_Scopes[nScopeTop]:Buffer, nScopeBottom, nLen)
                if nDiff != 0 // not equal
                    IF SELF:Descending
                        // top should be bigger than bottom
                        lEmpty := nDiff < 0
                    ELSE
                        // top should be smaller than bottom
                        lEmpty := nDiff > 0
                    ENDIF
                endif
                IF lEmpty
                    SELF:_oRdd:__Goto(0)
                    SELF:_oRdd:_SetBOF(TRUE)

                    RETURN FALSE
                ENDIF
            endif
            IF SELF:_Scopes[nScopeTop]:IsSet
                nLen := SELF:_Scopes[nScopeTop]:Size
                nDiff := SELF:__ScopeCompare(SELF:_currentvalue:Key, nScopeTop, nLen)
                IF SELF:Descending  // B - A, so larger value is before top
                    IF nDiff > 0
                        lTopOk := FALSE
                    ENDIF
                ELSE   // A - B , so smaller value is before top
                    IF nDiff < 0
                        lTopOk := FALSE
                    ENDIF
                ENDIF
            ENDIF
            IF SELF:_Scopes[nScopeBottom]:IsSet
                nLen := SELF:_Scopes[nScopeBottom]:Size
                nDiff := SELF:__ScopeCompare(SELF:_currentvalue:Key, nScopeBottom, nLen)
                IF SELF:Descending  // B - A, so smaller value is after bottom
                    IF nDiff < 0
                        lBotOk := FALSE
                    ENDIF
                ELSE   // A - B , so larger value is after bottom
                    IF nDiff > 0
                        lBotOk := FALSE
                    ENDIF
                ENDIF
            ENDIF
            if !(lTopOk .and. lBotOk)
                SELF:_oRdd:__Goto(0)
                SELF:_oRdd:_SetEOF(TRUE)
                if ! lTopOk
                    SELF:_oRdd:_SetBOF(TRUE)
                ENDIF
            ENDIF
            RETURN TRUE
        INTERNAL METHOD SkipRaw(nToSkip AS LONG ) AS LOGIC
            LOCAL recno AS LONG
            LOCAL isBof AS LOGIC
            LOCAL isEof AS LOGIC
            LOCAL orgBof AS LOGIC
            LOCAL orgEof AS LOGIC
            LOCAL locked AS LOGIC
            LOCAL orgToSkip AS INT
            LOCAL result := FALSE AS LOGIC
            LOCAL forward := FALSE AS LOGIC
            IF SELF:EmptyResultSet
                RETURN TRUE
            ENDIF
            // Default Position = Current Record
            IF nToSkip == 0
                recno := SELF:_RecNo
            ELSE
                recno := 0
            ENDIF
            forward := nToSkip > 0
            orgBof := SELF:_oRdd:BoF
            orgEof := SELF:_oRdd:EoF
            isBof := FALSE
            isEof := FALSE
            locked := FALSE

            TRY
                orgToSkip := nToSkip
                SELF:_oRdd:GoCold()
                locked := SELF:Slock()
                IF !locked
                    RETURN FALSE
                ENDIF
                IF SELF:Descending
                    nToSkip := - nToSkip
                ENDIF
                IF !SELF:_oRdd:_isValid // we're at EOF
                    IF nToSkip < 0
                        IF !SELF:Descending
                            SELF:GoBottom()
                        ELSE
                            SELF:GoTop()
                        ENDIF
                        nToSkip ++
                        recno := SELF:_oRdd:RecNo
                        SELF:_oRdd:_SetBOF(recno == 0)
                        SELF:_oRdd:_SetEOF(recno == 0)
                    ELSE
                        recno := 0
                        nToSkip := 0
                    ENDIF
                ELSE
                    IF SELF:Stack:Empty
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
                            isBof := SELF:_oRdd:BoF
                        ENDIF
                        IF isEof != SELF:_oRdd:EoF
                            isEof := SELF:_oRdd:EoF
                        ENDIF
                    ELSE
                        IF nToSkip != 0
                            recno := SELF:_nextKey(nToSkip)
                            IF recno == -1
                                recno := SELF:_locateFirst(SELF:_rootPage)
                                isBof := TRUE
                            ENDIF
                        ENDIF
                    ENDIF
                ENDIF
                result := SELF:_oRdd:__Goto(recno)
                IF orgToSkip == 0
                    SELF:_oRdd:_SetBOF(orgBof)
                    SELF:_oRdd:_SetEOF(orgEof)
                ELSEIF recno == 0
                    IF forward
                        SELF:_oRdd:_SetEOF(TRUE)
                        SELF:_oRdd:_SetBOF(FALSE)
                    ELSE
                        SELF:_oRdd:_SetBOF(TRUE)
                        SELF:_oRdd:_SetEOF(FALSE)
                    ENDIF
                ELSE
                    IF isBof
                        SELF:_oRdd:_SetBOF(isBof)
                    ENDIF
                    IF isEof
                        SELF:_oRdd:_SetEOF(isEof)
                    ENDIF
                ENDIF
                IF !SELF:HasScope
                    RETURN result
                ENDIF
            CATCH ex AS Exception
               SELF:ThrowException(ex, Subcodes.EDB_SKIP,Gencode.EG_CORRUPTION,  "CdxTag.SkipRaw")
            FINALLY
                IF locked
                    result := SELF:UnLock() .AND. result
                ENDIF
            END TRY
            RETURN result
        PUBLIC METHOD SkipUnique(nDir as LONG) AS LONG
            local moveDirection as SkipDirection
            if nDir >= 0
                moveDirection := SkipDirection.Forward
            else
                moveDirection := SkipDirection.Backward
            endif
            local currentKey as byte[]
            currentKey := (byte[]) self:_currentvalue:Key:Clone()
            do while true
                IF SELF:_getNextKey(moveDirection) == 0
                   SELF:_oRdd:_SetEOF(true)
                   exit
                endif
                IF SELF:__Compare(currentKey, self:_currentvalue:Key, currentKey:Length,0, 0) != 0
                    SELF:_GoToRecno( SELF:_currentvalue:Recno)
                    EXIT
                ENDIF
            enddo
            RETURN SELF:_oRdd:RecNo
        PRIVATE METHOD _getNextKey(moveDirection AS SkipDirection ) AS LONG
            LOCAL page  AS CdxTreePage
            LOCAL node  AS CdxPageNode
            // No page loaded ?
            IF SELF:Stack:Empty
                RETURN 0
            ENDIF
            page    := SELF:CurrentStack:Page
            node    := page[SELF:CurrentStack:Pos]

            IF moveDirection == SkipDirection.Forward
                SELF:CurrentStack:Pos++
                node:Pos := SELF:CurrentStack:Pos
                IF node:Pos < page:NumKeys .AND. node:ChildPageNo != 0
                    RETURN SELF:_locate(NULL, 0, SearchMode.Top, node:ChildPageNo,0)
                ENDIF
                // Once we are at the bottom level then we simply skip forward using the Right Pointers
                IF SELF:CurrentStack:Pos == page:NumKeys
#ifdef TESTCDX
                    IF ! page:ValidateSiblings()
                        SELF:ThrowException(Subcodes.ERDD_INVALID_ORDER,Gencode.EG_CORRUPTION,  "CdxTag._getNextKey","Incorrect link between sibling pages for page: "+page:PageNoX)
                    ENDIF
#endif
                    IF page:HasRight
                        VAR rightPtr := page:RightPtr
                        VAR newpage := SELF:GetPage(rightPtr)

                        SELF:Stack:Replace(page, newpage, 0)
                        // Normally we should not find empty pages, but if we do, we simply skip them.
                        DO WHILE newpage:NumKeys == 0 .AND. newpage:HasRight
                            rightPtr := newpage:RightPtr
                            newpage := SELF:GetPage(rightPtr)
                        ENDDO
                        IF newpage:NumKeys > 0
                            node    := newpage[0]
                            SELF:_saveCurrentRecord(node)
                            RETURN node:Recno
                        ELSE
                            // NewPage has no right and no keys. Blank page At the end of the leaf list
                            SELF:ClearStack()
                            RETURN 0
                        ENDIF
                    ELSE
                        // This page is at the end of the leaf list
                        RETURN 0
                    ENDIF
                ENDIF
                IF node:Pos >= page:NumKeys
                    // Skipped to EOF
                    RETURN 0
                ENDIF
                SELF:_saveCurrentRecord(node)
                RETURN node:Recno
            ENDIF
            IF node:ChildPageNo != 0
                RETURN SELF:_locate(NULL, 0, SearchMode.Bottom, node:ChildPageNo,0)
            ENDIF
            IF SELF:CurrentStack:Pos == 0
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
            SELF:CurrentStack:Pos--
            node:Pos := SELF:CurrentStack:Pos
            SELF:_saveCurrentRecord(node)
            RETURN node:Recno


        PRIVATE METHOD _findItemPos() AS LONG

            IF SELF:Stack:Empty
                RETURN -1
            ENDIF
            // this assumes the top of the stack has the leaf page where our key is.
            // we count the # of keys to the right of that page and add the pos on the top of the stack
            LOCAL page  := (CdxTreePage) SELF:CurrentStack:Page AS CdxTreePage
            LOCAL pos AS LONG
            pos := SELF:CurrentStack:Pos
            DO WHILE page:HasLeft
                VAR nextPage := page:LeftPtr
                page := SELF:GetPage(nextPage)
                pos += page:NumKeys
            ENDDO
            RETURN pos+1



        PRIVATE METHOD _getScopePos() AS LONG
            LOCAL first AS LONG
            LOCAL last AS LONG
            IF SELF:_Scopes[TOPSCOPE]:IsSet
                IF SELF:__ScopeCompare(SELF:_currentvalue:Key, TOPSCOPE)  < 0
                    RETURN 0
                ENDIF
            ENDIF
            IF SELF:_Scopes[BOTTOMSCOPE]:IsSet
                IF SELF:__ScopeCompare(SELF:_currentvalue:Key, BOTTOMSCOPE) > 0
                    RETURN 0
                ENDIF
            ENDIF
            first := 1
            last := 1
            last := SELF:_findItemPos()
            IF SELF:HasTopScope
                SELF:_ScopeSeek(DbOrder_Info.DBOI_SCOPETOP)
                first := SELF:_findItemPos()
                SELF:ClearStack()
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
                SELF:ThrowException(Subcodes.ERDD_KEY_EVAL, Gencode.EG_DATATYPE, SELF:FileName)
            ENDIF
            RETURN isOk



        PRIVATE METHOD _ScopeSkip(lNumKeys AS LONG ) AS LONG
            LOCAL result AS LONG
            LOCAL recno AS LONG
            LOCAL SkipDirection AS SkipDirection
            IF SELF:_scopeEmpty
                RETURN 0
            ENDIF
            VAR RT_Deleted := XSharp.RuntimeState.Deleted
            result := SELF:_RecNo
            IF lNumKeys == 1
                recno := SELF:_getNextKey(SkipDirection.Forward)
                IF RT_Deleted .OR. SELF:_oRdd:FilterInfo:Active
                    recno := SELF:_skipFilter(recno, SkipDirection.Forward)
                ENDIF
                IF recno == 0
                    SELF:_oRdd:_SetEOF(TRUE)
                    RETURN 0
                ENDIF
                // Note we hardcoded the ranges here on purpose. Otherwise it does NOT work
                IF SELF:_Scopes[BOTTOMSCOPE]:IsSet
                    LOCAL nRes AS LONG
                    nRes := SELF:__ScopeCompare(SELF:_currentvalue:Key, BOTTOMSCOPE)
                    VAR lEOF := nRes > 0
                    IF lEOF
                        IF SELF:Descending
                            recno := SELF:_getNextKey(SkipDirection.Forward)
                            SELF:_oRdd:_SetBOF(TRUE)
                        ELSE
                            SELF:_oRdd:_SetEOF(TRUE)
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
                        IF RT_Deleted .OR. SELF:_oRdd:FilterInfo:Active
                            recno := SELF:_skipFilter(recno, SkipDirection)
                        ENDIF
                        lNumKeys--
                        if recno == 0
                            recno := -1
                            EXIT
                        else
                            // Note we hardcoded the ranges here on purpose. Otherwise it does NOT work
                            IF SkipDirection == SkipDirection.Backward
                                IF SELF:_Scopes[TOPSCOPE]:IsSet
                                    LOCAL nRes AS LONG
                                    nRes := SELF:__ScopeCompare(SELF:_currentvalue:Key, TOPSCOPE)
                                    VAR lBOF := nRes < 0
                                    IF lBOF
                                        IF SELF:Descending
                                            SELF:_oRdd:_SetEOF(TRUE)
                                            recno := 0
                                        ELSE
                                            recno := SELF:_getNextKey(SkipDirection.Forward)
                                            SELF:_oRdd:_SetBOF(TRUE)
                                        ENDIF
                                        EXIT
                                    ENDIF
                                ENDIF
                            ELSE
                                IF SELF:_Scopes[BOTTOMSCOPE]:IsSet
                                    SELF:_oRdd:_SetBOF(FALSE)
                                    LOCAL nRes AS LONG
                                    nRes := SELF:__ScopeCompare(SELF:_currentvalue:Key, BOTTOMSCOPE)
                                    VAR lEOF := nRes > 0
                                    IF lEOF
                                        recno := 0
                                        IF SELF:Descending
                                            recno := SELF:_getNextKey(SkipDirection.Forward)
                                            SELF:_oRdd:_SetBOF(TRUE)
                                        ELSE
                                            SELF:_oRdd:_SetEOF(TRUE)
                                        ENDIF
                                        RETURN result
                                    ENDIF
                                    result := recno
                                ENDIF
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
                obj     := SELF:_Scopes[TopScopeNo]:Value
                IF obj == NULL
                    result      := SELF:GoTop()
                    mustSeek    := FALSE
                ELSE
                    seekInfo:Last   := FALSE
                    mustSeek      := TRUE
                ENDIF
            ELSE
                obj := SELF:_Scopes[BottomScopeNo]:Value
                IF obj == NULL
                    result      := SELF:GoBottom()
                    mustSeek    := FALSE
                ELSE
                    seekInfo:Last   := TRUE
                    mustSeek        := TRUE
                ENDIF
            ENDIF
            IF mustSeek
                SELF:_oRdd:_SetBOF(FALSE)
                SELF:_oRdd:_SetEOF(FALSE)
                seekInfo:Value      := obj
                seekInfo:SoftSeek   := TRUE
                result              := SELF:_Seek(seekInfo, obj)
                SELF:_oRdd:Found    := SELF:_isInScope()
                IF !SELF:_oRdd:Found
                    SELF:_oRdd:GoTo(0)
                     SELF:_oRdd:_SetBOF(TRUE)
                     SELF:_oRdd:_SetEOF(TRUE)
                    SELF:_scopeEmpty := TRUE
                ENDIF
            ENDIF
            RETURN result


        PRIVATE METHOD _isInScope() AS LOGIC
            LOCAL isOk AS LOGIC
            isOk := SELF:_oRdd:Found
            IF !SELF:Descending
                IF !isOk
                    IF SELF:HasBottomScope
                        IF SELF:__ScopeCompare(SELF:_currentvalue:Key, BottomScopeNo) <= 0
                            isOk := TRUE
                        ENDIF
                    ELSE
                        isOk := TRUE
                    ENDIF
                    IF isOk .AND. SELF:HasTopScope
                        IF SELF:__ScopeCompare(SELF:_currentvalue:Key, TopScopeNo) >= 0
                            isOk := TRUE
                        ELSE
                            isOk := FALSE
                        ENDIF
                    ENDIF
               ENDIF
            ELSE
               isOk := FALSE
               IF SELF:HasBottomScope
                    IF SELF:__ScopeCompare(SELF:_currentvalue:Key, BottomScopeNo) >= 0
                            isOk := TRUE
                    ENDIF
                ELSE
                    isOk := TRUE
                ENDIF
                IF isOk .AND. SELF:HasTopScope
                    IF SELF:__ScopeCompare(SELF:_currentvalue:Key, TopScopeNo) <= 0
                        isOk := TRUE
                    ELSE
                        isOk := FALSE
                    ENDIF
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
            SELF:_oRdd:__Goto(recno)
            SELF:_saveCurrentKey(recno,SELF:_currentvalue)
            IF SELF:_goRecord(SELF:_currentvalue:Key, SELF:_keySize, recno) != recno
                IF SELF:_goRecord(NULL, 0, recno) != recno .AND. recno <= SELF:_oRdd:RecCount
                    IF !SELF:Unique .AND. !SELF:Conditional .AND. !SELF:Custom
                        SELF:ThrowException(Subcodes.ERDD_RECNO_MISSING, Gencode.EG_CORRUPTION,SELF:FileName)
                        result := FALSE
                    ENDIF
                    SELF:Stack:Clear()
                ENDIF
            ENDIF
            IF result
                SELF:_oRdd:__Goto(recno)
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

        PRIVATE METHOD _locateFirst(pageOffset AS LONG) AS LONG
            VAR page := SELF:GetPage(pageOffset)
            LOCAL result := 0 AS LONG
            IF page == NULL
                SELF:ClearStack()
                RETURN 0
            ENDIF
            SELF:_oRdd:_SetBOF(false)
            SELF:_oRdd:_SetEOF(false)
            SELF:PushPage(page, 0)
            IF page IS CdxBranchPage VAR branchPage
                LOCAL nChildPage AS LONG
                nChildPage := branchPage:GetChildPage(0)
                RETURN SELF:_locateFirst(nChildPage)
            ENDIF
            IF page:NumKeys > 0
                VAR node := page[0]
                SELF:_saveCurrentRecord(node)
                result := node:Recno
            ENDIF
            #ifdef TESTCDX
                IF VALIDATETREE
                    LOCAL iLevel AS INT
                    FOREACH entry AS CdxStackEntry IN SELF:_stack:Entries
                        ++ iLevel
                        VAR epage := entry:Page
                        VAR level := epage:CurrentLevel
                        VAR nStart := -1
                        VAR nRight := -1
                        LOCAL oLast AS CdxTreePage
                        FOREACH VAR element IN level
                            IF element:LeftPtr != nStart
                                element:Debug("Incorrect LeftPtr at stack level ", iLevel)
                            ENDIF
                            IF element:PageNo != nRight .AND. nRight != -1
                                oLast:Debug("Incorrect right Ptr")
                            ENDIF
                            nStart := element:PageNo
                            nRight := element:RightPtr
                            oLast := element
                        NEXT
                        IF oLast:HasRight
                            oLast:Debug("Incorrect right Ptr")
                        ENDIF
                    NEXT
                ENDIF
            #endif
            RETURN result


        PRIVATE METHOD _locateLast(pageOffSet AS LONG) AS LONG
            VAR page := SELF:GetPage(pageOffSet)
            IF page == NULL
                SELF:ClearStack()
                RETURN 0
            ENDIF
            SELF:_oRdd:_SetBOF(false)
            SELF:_oRdd:_SetEOF(false)
            SELF:PushPage(page, (WORD) (page:NumKeys-1))
            IF page IS CdxBranchPage VAR branchPage
                LOCAL nChildPage AS LONG
                nChildPage := branchPage:GetChildPage(page:NumKeys-1)
                RETURN SELF:_locateLast(nChildPage)
            ENDIF
            VAR node := page[(WORD) (page:NumKeys-1)]
            SELF:_saveCurrentRecord(node)
            RETURN node:Recno


        PRIVATE METHOD FixBrokenBranches(branch AS CdxBranchPage) AS LOGIC
              // try to copy the first key from the right page
               //Debug.Assert(nodeCount > 0,"Branchpage with 0 keys found")
               _DebOut32("DBFCDX Detected empty Branch page "+ branch:PageNoX+", attempting to fix")
               VAR isFixed := FALSE
               IF branch:HasRight
                  VAR oRight := (CdxBranchPage) SELF:GetPage(branch:RightPtr)
                  IF oRight:NumKeys > 1
                     VAR recno := oRight:GetRecno(0)
                     VAR key   := oRight:GetKey(0)
                     VAR child := oRight:GetChildPage(0)
                     oRight:Delete(0)
                     oRight:Write()
                     branch:Add(recno, child, key)
                     branch:Write()
                     VAR action := CdxAction.ChangeParent(branch)
                     DO WHILE action != CdxAction.Ok
                        action := SELF:DoAction(action)
                     ENDDO
                     isFixed := TRUE
                     _DebOut32("DBFCDX Fixed by moving first key from Right Sibling "+ oRight:PageNoX)
                  ENDIF
               ENDIF
               IF ! isFixed .AND. branch:HasLeft
                  VAR oLeft := (CdxBranchPage) SELF:GetPage(branch:LeftPtr)
                  IF oLeft:NumKeys > 1
                     VAR keyno := oLeft:NumKeys -1
                     VAR recno := oLeft:GetRecno(keyno)
                     VAR key   := oLeft:GetKey(keyno)
                     VAR child := oLeft:GetChildPage(keyno)
                     oLeft:Delete(keyno)
                     oLeft:Write()
                     VAR action := CdxAction.ChangeParent(oLeft)
                     DO WHILE action != CdxAction.Ok
                        action := SELF:DoAction(action)
                     ENDDO
                     branch:Add(recno, child, key)
                     branch:Write()
                     action := CdxAction.ChangeParent(branch)
                     DO WHILE action != CdxAction.Ok
                        action := SELF:DoAction(action)
                     ENDDO
                      isFixed := TRUE
                     _DebOut32("DBFCDX Fixed by moving last key from Left Sibling "+ oLeft:PageNoX)
                  ENDIF
                ENDIF
#ifdef TESTCDX
               IF isFixed
                  branch:ValidateLevel()
                ENDIF
#endif
               RETURN isFixed


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
            // How many Items in that page ?
            nodeCount := page:NumKeys
            IF nodeCount == 0
               _DebOut32("DBFCDX Detected empty page "+ page:PageNoX)
            ENDIF
            IF (nodeCount == 0 .AND. page IS CdxBranchPage VAR branch)
               VAR isFixed := SELF:FixBrokenBranches(branch)
               IF ! isFixed
                  SELF:_UpdateError(NULL, "CdxTag._locate","Found Branch Page with 0 keys and no Siblings")
               ELSE
                   RETURN 0
               ENDIF
            ENDIF
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
                        foundPos := (WORD) (nodeCount-1)
                    ENDIF
                    node:Pos := foundPos
                CASE SearchMode.Left
                CASE SearchMode.SoftSeek
                    minPos := 0
                    maxPos := nodeCount
                    VAR found := FALSE
                    DO WHILE minPos < maxPos
                        foundPos := (WORD) ((minPos + maxPos) / 2)
                        node:Pos := foundPos
                        VAR nodeBytes := node:KeyBytes
                        VAR cmp := SELF:__Compare(nodeBytes, keyBuffer, keyLength, node:Recno,recNo)
                        IF cmp >= 0
                            found := TRUE
                        ENDIF
                        IF cmp  < 0
                            minPos := (WORD) (foundPos + 1)
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
                    IF searchMode == SearchMode.Left .AND. foundPos < nodeCount .AND. SELF:__Compare(node:KeyBytes, keyBuffer, keyLength, node:Recno, recNo) == 0
                        searchMode := SearchMode.SoftSeek
                    ENDIF
                CASE SearchMode.Bottom
                    IF nodeCount > 0
                        foundPos := (WORD) (nodeCount-1)
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
                    CASE SearchMode.SoftSeek
                    CASE SearchMode.Bottom
                    CASE SearchMode.Top
                        SELF:_saveCurrentRecord(node)
                        RETURN node:Recno
                    CASE SearchMode.Left
                        IF SELF:__Compare(node:KeyBytes, keyBuffer, keyLength, node:Recno, recNo) == 0
                            SELF:_saveCurrentRecord(node)
                            RETURN node:Recno
                        ENDIF
                        RETURN 0
                    CASE SearchMode.Right
                        RETURN 0
                END SWITCH
            ELSEIF searchMode == SearchMode.SoftSeek
                DO WHILE ! SELF:Stack:Empty .AND. SELF:CurrentStack:Pos == SELF:CurrentStack:Page:NumKeys
                    SELF:PopPage()
                ENDDO
                IF ! SELF:Stack:Empty
                    page := SELF:Stack:Top:Page
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

        PRIVATE METHOD _skipFilter(recno AS LONG , direction AS SkipDirection ) AS LONG
            LOCAL recordHidden AS LOGIC
            LOCAL cbFilter     AS ICodeblock
            LOCAL fRtDeleted   AS LOGIC
            LOCAL fi           AS DbFilterInfo
            fi := _oRdd:FilterInfo
            IF SELF:_oRdd:__Goto(recno)
                fRtDeleted := RuntimeState.Deleted
                IF fi:Active
                    cbFilter    := fi:FilterBlock
                ELSE
                    cbFilter    := NULL
                ENDIF
                IF cbFilter == NULL .AND. ! fRtDeleted
                    // No filter and not SetDeleted(TRUE), so nothing to do.
                    RETURN recno
                ENDIF
                recordHidden:= TRUE
                DO WHILE recordHidden
                    // Check deleted first, that is easier and has less overhead
                    IF fRtDeleted
                        recordHidden := SELF:_oRdd:Deleted
                    ELSE
                        recordHidden := FALSE
                    ENDIF

                    IF ! recordHidden .AND. cbFilter != NULL
                        recordHidden := ! (LOGIC) SELF:_oRdd:EvalBlock(cbFilter)
                    ENDIF
                    IF recordHidden
                        recno := SELF:_getNextKey(direction)
                        IF recno == 0
                            EXIT
                        ENDIF
                        SELF:_oRdd:__Goto(recno)
                    ELSE
                        EXIT
                    ENDIF
                ENDDO
            ENDIF
            RETURN recno

        PRIVATE METHOD _Seek(seekInfo AS DbSeekInfo , bSearchKey AS BYTE[] ) AS LOGIC
            LOCAL recno := 0 AS LONG
            LOCAL result := FALSE  AS LOGIC
            LOCAL fOriginalSoftSeekValue AS LOGIC
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
            LOCAL oldDescend AS LOGIC
            LOCAL activeFilter AS LOGIC
            oldDescend   := SELF:Descending
            activeFilter := XSharp.RuntimeState.Deleted .OR. SELF:_oRdd:FilterInfo:Active
            fOriginalSoftSeekValue := seekInfo:SoftSeek
            TRY
                SELF:Descending := FALSE
                IF oldDescend
                    seekInfo:Last := ! seekInfo:Last
                ENDIF
                SELF:_oRdd:GoCold()
                locked := SELF:Slock()
                IF ! locked
                    RETURN FALSE
                ENDIF

                IF SELF:Shared
                    SELF:_currentvalue:Recno := 0
                ENDIF
                needPadStr := FALSE
                IF seekInfo:Value:GetType() == TYPEOF(STRING)
                    text    := (STRING)seekInfo:Value
                    len     := text:Length
                    IF SELF:_Collation != NULL
                        len := 0
                        DO WHILE bSearchKey[len] != 0 .AND. len < bSearchKey:Length
                            len++
                        ENDDO
                    ENDIF
                    padLen := len
                    IF len < SELF:_keySize
                        needPadStr := TRUE
                        bSearchKey[len] := 1
                        padLen := len + 1
                        seekInfo:SoftSeek := TRUE
                    ENDIF
                ELSE
                    len := SELF:_keySize
                    padLen := len
                ENDIF
                recno  := SELF:_locateKey(bSearchKey, padLen, IIF(seekInfo:SoftSeek , SearchMode.SoftSeek , SearchMode.Left),recno)
                result := SELF:_oRdd:__Goto(recno)
                IF activeFilter
                    SELF:_oRdd:SkipFilter(1)
                    recno := SELF:_RecNo
                ENDIF
                LOCAL found := FALSE AS LOGIC
                IF SELF:_oRdd:_isValid
                    // Get Current Key
                    SELF:_saveCurrentKey(recno,SELF:_currentvalue)
                    VAR currentKeyBuffer := SELF:_currentvalue:Key
                    // Note: Softseek will also be set when an incomplete key is passed
                    IF activeFilter .OR. seekInfo:SoftSeek .OR. seekInfo:Last
                        SELF:_ToString(seekInfo:Value, SELF:_keySize, SELF:_newvalue:Key, REF SELF:_newKeyLen)
                        strCmp := SELF:__Compare(bSearchKey, currentKeyBuffer, len,0, 0)
                        found := (strCmp == 0)
                        IF needPadStr .AND. !found
                            SELF:_newvalue:Key[len] := 1
                            temp:= currentKeyBuffer[len]
                            currentKeyBuffer[len] := Byte.MaxValue
                            strCmpMaxMin := SELF:__Compare(SELF:_newvalue:Key, currentKeyBuffer, padLen, 0, 0)
                            IF strCmp > 0 .AND. strCmpMaxMin < 0
                                found := TRUE
                            ENDIF
                            IF !found
                                SELF:_newvalue:Key[len] := Byte.MaxValue
                                currentKeyBuffer[len] := 1
                                strCmpMaxMin := SELF:__Compare(SELF:_newvalue:Key, currentKeyBuffer, padLen,0,0)
                                IF strCmp < 0 .AND. strCmpMaxMin > 0
                                    found := TRUE
                                ENDIF
                            ENDIF
                            SELF:_newvalue:Key[len] := 0
                            currentKeyBuffer[len] := temp
                            seekInfo:SoftSeek := fOriginalSoftSeekValue
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
                                    strCmp := SELF:__Compare(SELF:_newvalue:Key, currentKeyBuffer, len,0,0)
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
                            // This code gets executed when doing a GoBottom in a scoped index when the bottom scope does not exist
                            IF seekInfo:Last
                                diff := strCmp
                                recno := SELF:_nextKey(-1)
                                IF activeFilter
                                    recno := SELF:_skipFilter(recno, SkipDirection.Backward)
                                ENDIF
                                strCmp := SELF:__Compare(SELF:_newvalue:Key, currentKeyBuffer, len, 0, 0)
                                found := (strCmp == 0)
                                IF found
                                    result := SELF:_oRdd:__Goto(recno)
                                ELSE
                                    IF diff == -strCmp .and. fOriginalSoftSeekValue
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
                    strCmp := SELF:__Compare(bSearchKey, currentKeyBuffer, len,0,0)
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
                SELF:Descending := oldDescend
                IF locked
                    result := SELF:UnLock()
                ENDIF
            END TRY

        PRIVATE METHOD _Seek(dbsi AS DbSeekInfo , lpval AS OBJECT ) AS LOGIC
            LOCAL byteArray AS BYTE[]
            byteArray := BYTE[]{ SELF:_keySize }
            SELF:_ToString(lpval, SELF:_keySize, byteArray)
            dbsi:SoftSeek := TRUE
            RETURN SELF:_Seek(dbsi, byteArray)

    END CLASS

END NAMESPACE


