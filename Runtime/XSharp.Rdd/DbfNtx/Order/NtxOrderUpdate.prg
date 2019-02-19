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
        // Methods for updating (adding, inserting, deleting) keys into indices
            
        PRIVATE METHOD _keyUpdate(recordNo AS LONG , lNewRecord AS LOGIC ) AS LOGIC
            LOCAL condFor := TRUE AS LOGIC
            LOCAL num AS LONG
            LOCAL noMoreLock AS LOGIC
            LOCAL errorLevel AS LONG
            LOCAL lockCount AS LONG
            num := 0
            noMoreLock := TRUE
            errorLevel := 0
            lockCount := 0
            DO WHILE TRUE
                IF SELF:Shared
                    IF SELF:_HPLocking
                        lockCount := SELF:_readLocks
                        DO WHILE SELF:_readLocks != 0
                            noMoreLock := SELF:_ReadUnLock()
                            IF !noMoreLock
                                errorLevel := 2
                                EXIT
                            ENDIF
                        ENDDO
                    ENDIF
                    IF noMoreLock .AND. !SELF:_WriteLock()
                        errorLevel := 2
                        EXIT
                    ENDIF
                ENDIF
                LOCAL evalOk AS LOGIC
                IF SELF:_Conditional
                    evalOk := TRUE
                    TRY
                        condFor := (LOGIC)SELF:_oRdd:EvalBlock(SELF:_ForCodeBlock)
                    CATCH
                        evalOk := FALSE
                        SELF:_oRdd:_dbfError( SubCodes.ERDD_KEY_EVAL,GenCode.EG_DATATYPE, SELF:fileName)
                    END TRY
                    IF !evalOk
                        errorLevel := 1
                        EXIT
                    ENDIF
                ENDIF
                IF lNewRecord .AND. ! condFor
                    // No need to update or delete anything
                    RETURN TRUE
                ENDIF

                IF SELF:getKeyValue(SELF:_SourceIndex, SELF:_newKeyBuffer)
                    LOCAL changed := FALSE AS LOGIC
                    IF !lNewRecord
                        changed := SELF:__Compare(SELF:_newKeyBuffer, SELF:_currentKeyBuffer, SELF:_keySize) != 0
                        IF changed
                            SELF:_TopStack := 0
                        ENDIF
                        num := SELF:_goRecord(SELF:_currentKeyBuffer, SELF:_keySize, recordNo)
                        IF (SELF:_TopStack != 0 .AND. !SELF:_Conditional) .OR. num != 0
                            IF changed .OR. !condFor
                                SELF:_deleteKey()
                            ENDIF
                        ELSE
                            IF !SELF:_Unique .AND. !SELF:_Conditional .AND. !SELF:_Partial
                                SELF:_oRdd:_dbfError( SubCodes.ERDD_KEY_NOT_FOUND, GenCode.EG_DATATYPE,SELF:fileName)
                            ENDIF
                        ENDIF
                    ENDIF
                    IF (lNewRecord .OR. changed) .AND. condFor
                        SELF:_midItem:KeyBytes := SELF:_newKeyBuffer
                        SELF:_midItem:PageNo := 0
                        SELF:_midItem:Recno := recordNo
                        SELF:_TopStack := 0
                        IF SELF:_Unique
                            IF SELF:_locate(SELF:_midItem:KeyBytes, SELF:_keySize, SearchMode.Left, SELF:_firstPageOffset) == 0
                                SELF:_addKey()
                            ELSE
                                SELF:_TopStack := 0
                            ENDIF
                        ELSE
                            SELF:_locate(SELF:_midItem:KeyBytes, SELF:_keySize, SearchMode.Right, SELF:_firstPageOffset)
                            SELF:_addKey()
                        ENDIF
                        SELF:_TopStack := 0
                        SELF:_Hot := TRUE
                    ENDIF
                    Array.Copy(SELF:_newKeyBuffer, SELF:_currentKeyBuffer, SELF:_keySize + 1)
                    SELF:_currentRecno := recordNo
                    errorLevel := 0
                ENDIF
                EXIT
            ENDDO 
            IF errorLevel <= 1
                IF SELF:Shared
                    SELF:_PageList:Flush(TRUE)
                    SELF:_indexVersion++
                    SELF:_PutHeader()
                    SELF:_Hot := FALSE
                    FFlush( SELF:_hFile )
                    SELF:_WriteUnLock()
                    IF SELF:_HPLocking
                        DO WHILE lockCount != 0 .AND. !SELF:_ReadLock()
                            lockCount--
                        ENDDO
                    ENDIF
                ENDIF
                RETURN TRUE
            ENDIF
            IF errorLevel == 2
                SELF:_oRdd:_dbfError( SubCodes.ERDD_KEY_EVAL, GenCode.EG_DATATYPE)
                RETURN FALSE
            ENDIF
            RETURN TRUE
            
        PRIVATE METHOD _addKey() AS LOGIC
            LOCAL uiHalfPage    AS WORD
            LOCAL page          AS NtxPage
            LOCAL pageNo        AS LONG
            LOCAL offset        AS WORD
            LOCAL node          AS NtxNode
            
            SELF:_Hot := TRUE
            uiHalfPage := SELF:_halfPage
            IF SELF:_TopStack == 0
                // new root 
                page := SELF:AllocPage()
                pageNo := page:PageOffset
                page:InitRefs(_MaxEntry, _EntrySize)
                node            := page[0]
                node:PageNo     := SELF:_firstPageOffset
                node:Recno      := SELF:_midItem:Recno
                node:KeyBytes   := SELF:_midItem:KeyBytes
                page[1]:PageNo  := SELF:_midItem:PageNo
                page:NodeCount  := 1
                SELF:_firstPageOffset := pageNo
                RETURN FALSE
            ENDIF
            VAR page2 := SELF:_PageList:Update(SELF:CurrentStack:Page)
            IF SELF:_insertKey(page2)
                // Split pages
                // Write Left page
                page2:NodeCount := uiHalfPage
                // New right page
                page := SELF:AllocPage()
                // Copy references from left page
                // and shift Left
                Array.Copy(page2:Bytes, page:Bytes, BUFF_SIZE)
                FOR VAR i := 0 TO uiHalfPage
                    offset := page:GetRef(i)
                    page:SetRef(i, page:GetRef(i + uiHalfPage))
                    page:SetRef(i + uiHalfPage, offset)
                NEXT
                page[0]:PageNo := SELF:_midItem:PageNo
                SELF:_midItem:PageNo := page:PageOffset
                SELF:_TopStack--
                SELF:_addKey()
                RETURN FALSE
            ENDIF
            RETURN TRUE
            
            
        PRIVATE METHOD _deleteKey() AS VOID
            LOCAL lPage AS LONG
            LOCAL uiPos AS LONG
            LOCAL page AS NtxPage
            LOCAL node AS NtxNode
            LOCAL nodeCount AS LONG
            LOCAL offset AS WORD
            LOCAL i AS LONG
            VAR topStack := SELF:CurrentStack
            lPage := topStack:Page
            uiPos := topStack:Pos
            page := SELF:_PageList:Read(lPage)
            node := page[uiPos]
            IF node:PageNo != 0
                // move key to leaf (copy leaf entry to current)
                SELF:_locate(NULL, 0, SearchMode.Bottom, node:PageNo)
                page := SELF:_PageList:Read(topStack:Page)
                // get leaf
                node    := page[topStack:Pos]
                SELF:_midItem:Recno := node:Recno
                SELF:_midItem:KeyBytes := node:KeyBytes
                // update parent
                page        := SELF:_PageList:Update(lPage)
                node        := page[uiPos]
                node:Recno  := SELF:_midItem:Recno
                node:KeyBytes := SELF:_midItem:KeyBytes
                // get back leaf
                lPage := topStack:Page
                uiPos := topStack:Pos
                page  := SELF:_PageList:Read(lPage)
                node  := page[uiPos]
            ENDIF
            // delete leaf entry
            nodeCount := page:NodeCount
            offset := page:GetRef(uiPos)
            
            FOR i := uiPos TO nodeCount -1
                // Copy the next Item offset at the current place
                page:SetRef(i, page:GetRef(i + 1))
            NEXT
            page:SetRef(nodeCount, offset)
            IF nodeCount > 0
                page:NodeCount--
            ENDIF
            topStack:Count := page:NodeCount
            topStack:Pos := page:NodeCount
            SELF:_PageList:Write(lPage)
            IF page:NodeCount < SELF:_halfPage .AND. SELF:_TopStack > 1
                SELF:_Balance()
            ENDIF
            
            
        PRIVATE METHOD _balance() AS VOID
            LOCAL leftPageNo AS LONG
            LOCAL uiCount AS LONG
            LOCAL pageLeft AS NtxPage
            LOCAL pageRight AS NtxPage
            LOCAL nodeLeft AS NtxNode
            LOCAL nodeRight AS NtxNode
            LOCAL iPos AS LONG
            LOCAL rightPageNo AS LONG
            LOCAL num2 AS LONG
            LOCAL offset AS WORD
            LOCAL num4 AS LONG
            VAR topStack := SELF:CurrentStack
            leftPageNo  := topStack:Page
            uiCount     := topStack:Count
            IF uiCount >= SELF:_halfPage
                // nothing to do
                RETURN
            ENDIF
            IF SELF:_TopStack == 1
                IF uiCount == 0
                    // delete root
                    pageLeft := SELF:_PageList:Update(leftPageNo)
                    
                    nodeLeft := pageLeft[0]
                    SELF:_firstPageOffset := nodeLeft:PageNo
                    // add to list of deleted pages
                    nodeLeft:PageNo := SELF:_nextUnusedPageOffset 
                    SELF:_nextUnusedPageOffset := leftPageNo
                ENDIF
            ELSE
                // get parent page
                --SELF:_TopStack
                topStack := SELF:CurrentStack
                iPos     := topStack:Pos
                
                pageLeft := SELF:_PageList:Read(topStack:Page)
                // setup left and right siblings
                IF iPos == topStack:Count
                    // underflow page was a right pointer from parent 
                    rightPageNo := pageLeft[iPos]:PageNo
                    num2 := rightPageNo
                    iPos := --topStack:Pos
                    leftPageNo := pageLeft[iPos]:PageNo
                ELSE
                    // underflow page was a left pointer from parent 
                    leftPageNo := pageLeft[iPos]:PageNo
                    num2 := leftPageNo
                    rightPageNo := pageLeft[iPos + 1]:PageNo
                ENDIF
                // delete parent entry into nodeMid
                SELF:_delToMid(pageLeft, iPos)
                topStack:Count--
                SELF:_PageList:Write(topStack:Page)
                // read sibling pages
                pageLeft := SELF:_PageList:Read(leftPageNo)
                pageRight := SELF:_PageList:Read(rightPageNo)
                // insert parent information into underflow page
                IF num2 == leftPageNo
                    // save at the end
                    iPos := pageLeft:NodeCount
                    nodeLeft := pageLeft[iPos]
                    nodeLeft:Recno := SELF:_midItem:Recno
                    nodeLeft:KeyBytes := SELF:_midItem:KeyBytes
                    nodeLeft := pageLeft[iPos + 1]
                    nodeRight := pageRight[0]
                    nodeLeft:PageNo := nodeRight:PageNo
                    nodeRight:PageNo := -1
                    pageLeft:NodeCount++
                ELSE
                    // save at the front
                    uiCount := pageRight:NodeCount
                    offset := pageRight:GetRef(uiCount + 1)
                    //Init
                    VAR num3 := uiCount + 1
                    DO WHILE num3 > 0
                        pageRight:SetRef(num3, pageRight:GetRef(num3 - 1))
                        //Iterators
                        num3--
                    ENDDO
                    pageRight:SetRef(0, offset)
                    nodeRight := pageRight[0]
                    // copoy data from Mid
                    nodeRight:Recno := SELF:_midItem:Recno
                    nodeRight:KeyBytes := SELF:_midItem:KeyBytes
                    nodeRight:PageNo := -1
                    pageRight:NodeCount++
                ENDIF
                iPos := pageLeft:NodeCount
                uiCount := iPos + pageRight:NodeCount
                IF uiCount == SELF:_MaxEntry
                    // the pages can be combined 
                    uiCount := 0
                    nodeLeft := pageLeft[iPos]
                    nodeRight := pageRight[uiCount]
                    DO WHILE iPos < SELF:_MaxEntry
                        nodeLeft:Recno := nodeRight:Recno
                        nodeLeft:KeyBytes := nodeRight:KeyBytes
                        uiCount++
                        iPos++
                        nodeLeft := pageLeft[iPos]
                        nodeRight := pageRight[uiCount]
                        nodeLeft:PageNo := nodeRight:PageNo
                    ENDDO
                    // left page contains all entries 
                    pageLeft:NodeCount := SELF:_MaxEntry
                    // right page is deleted
                    pageRight[0]:PageNo := SELF:_nextUnusedPageOffset
                    SELF:_nextUnusedPageOffset := rightPageNo
                    SELF:_PageList:Write(leftPageNo)
                    SELF:_PageList:Write(rightPageNo)
                    // the stack points to the parent, which may need balancing
                    SELF:_Balance()
                ELSE
                    num4 := (uiCount - 1) / 2
                    IF iPos <= num4
                        uiCount := 0
                        nodeLeft := pageLeft[iPos]
                        nodeRight := pageRight[uiCount]
                        DO WHILE iPos < num4
                            nodeLeft:Recno := nodeRight:Recno
                            nodeLeft:KeyBytes := nodeRight:KeyBytes
                            uiCount++
                            iPos++
                            nodeLeft := pageLeft[iPos]
                            nodeRight := pageRight[uiCount]
                            nodeLeft:PageNo := nodeRight:PageNo
                        END DO
                        pageLeft:NodeCount := (WORD)iPos
                        nodeRight := pageRight[uiCount]
                        SELF:_midItem:Recno := nodeRight:Recno
                        SELF:_midItem:KeyBytes := nodeRight:KeyBytes
                        uiCount++
                        num4 := pageRight:NodeCount
                        iPos := 0
                        //Init
                        DO WHILE uiCount <= num4
                            offset := pageRight:GetRef(iPos)
                            pageRight:SetRef(iPos, pageRight:GetRef(uiCount))
                            pageRight:SetRef(uiCount, offset)
                            iPos++
                            //Iterators
                            uiCount++
                        ENDDO
                        pageRight:NodeCount := (WORD)(iPos - 1)
                    ELSE
                        uiCount := pageRight:NodeCount
                        num4++
                        DO WHILE iPos > num4
                            offset := pageRight:GetRef(uiCount + 1)
                            //Init
                            VAR num3 := uiCount + 1
                            DO WHILE num3 > 0
                                pageRight:SetRef(num3, pageRight:GetRef(num3 - 1))
                                //Iterators
                                num3--
                            ENDDO
                            pageRight:SetRef(0, offset)
                            pageRight[1]:PageNo := pageLeft[iPos]:PageNo
                            iPos--
                            nodeRight := pageRight[0]
                            nodeLeft := pageLeft[iPos]
                            nodeRight:Recno := nodeLeft:Recno
                            nodeRight:KeyBytes := nodeLeft:KeyBytes
                            uiCount++
                        ENDDO
                        pageRight[0]:PageNo := pageLeft[iPos]:PageNo
                        pageRight:NodeCount := (WORD)uiCount
                        iPos--
                        nodeLeft := pageLeft[iPos]
                        SELF:_midItem:Recno := nodeLeft:Recno
                        SELF:_midItem:KeyBytes := nodeLeft:KeyBytes
                        pageLeft:NodeCount := (WORD)iPos
                    ENDIF
                    SELF:_midItem:PageNo := rightPageNo
                    SELF:_PageList:Write(leftPageNo)
                    SELF:_PageList:Write(rightPageNo)
                    SELF:_addKey()
                ENDIF
            ENDIF
            RETURN
            
        PRIVATE METHOD _delToMid(page AS NtxPage , uiPos AS LONG ) AS VOID
            // copy entry into mid, then delete from page
            LOCAL nodeCount AS LONG
            LOCAL leftPageNo AS LONG
            LOCAL offSet AS WORD
            LOCAL i AS LONG
            
            VAR node := page[uiPos]
            SELF:_midItem:Recno     := node:Recno
            SELF:_midItem:KeyBytes  := node:KeyBytes
            // delete key from page
            nodeCount   := page:NodeCount
            leftPageNo  := node:PageNo
            offSet      := page:GetRef(uiPos)
            // shift references
            FOR i := uiPos TO nodeCount -1
                page:SetRef(i, page:GetRef(i + 1))
            NEXT
            page:SetRef(nodeCount, offSet)
            // restore left page pointer (the right one is deleted)
            node        := page[uiPos]
            node:PageNo := leftPageNo
            page:NodeCount--
            RETURN
            
        PRIVATE METHOD _insertKey(page AS NtxPage ) AS LOGIC
            LOCAL nodeCount AS INT
            LOCAL uiPos AS WORD
            LOCAL offset AS WORD
            LOCAL num AS INT
            LOCAL uiHalfPage AS INT
            LOCAL num2 AS INT
            LOCAL shift AS INT
            LOCAL nStep AS INT
            LOCAL pageNo AS LONG
            
            nodeCount := page:NodeCount
            VAR topStack := SELF:CurrentStack
            uiPos := topStack:Pos
            IF nodeCount < SELF:_MaxEntry
                // it fits, so make space
                offset := page:GetRef(nodeCount + 1)
                num := nodeCount + 1
                DO WHILE num > uiPos
                    page:SetRef(num, page:GetRef(num - 1))
                    num--
                ENDDO
                page:SetRef(uiPos, offset)
                page[uiPos]:PageNo      := page[uiPos + 1]:PageNo
                page[uiPos+ 1]:PageNo   := SELF:_midItem:PageNo
                page[uiPos]:Recno       := SELF:_midItem:Recno
                page[uiPos]:KeyBytes    := SELF:_midItem:KeyBytes
                page:NodeCount++
                RETURN FALSE
            ENDIF
            // else split
            uiHalfPage := SELF:_halfPage
            IF uiPos == SELF:_halfPage
                RETURN TRUE
            ENDIF
            IF uiPos < SELF:_halfPage
                num2 := -1
                shift := 1
                nStep := -1
            ELSE
                num2 := 0
                shift := 0
                nStep := 1
            ENDIF
            VAR nodeOnPage := page[uiHalfPage + num2]
            VAR nodeTemp  := NtxNode{SELF:_keySize}
            nodeTemp:Recno := nodeOnPage:Recno
            nodeTemp:KeyBytes := nodeOnPage:KeyBytes
            nodeOnPage := page[uiHalfPage + num2 + 1]
            nodeTemp:PageNo := nodeOnPage:PageNo
            page[uiHalfPage + num2 + 1]:PageNo := page[uiHalfPage + num2]:PageNo
            offset := page:GetRef(uiHalfPage + num2)
            //Shift up to half
            num := uiHalfPage + num2
            DO WHILE num + shift != uiPos
                page:SetRef(num, page:GetRef(num + nStep))
                num += nStep
            ENDDO
            page:SetRef(uiPos - (shift + nStep), offset)
            VAR node2 := page[uiPos + shift]
            pageNo := node2:PageNo
            node2:PageNo := SELF:_midItem:PageNo
            node2 := page[uiPos + shift - 1]
            node2:PageNo := pageNo
            node2:Recno := SELF:_midItem:Recno
            node2:KeyBytes := SELF:_midItem:KeyBytes
            SELF:_midItem:Recno := nodeTemp:Recno
            SELF:_midItem:KeyBytes := nodeTemp:KeyBytes
            SELF:_midItem:PageNo := nodeTemp:PageNo
            RETURN TRUE
            
    END CLASS
    
END NAMESPACE


