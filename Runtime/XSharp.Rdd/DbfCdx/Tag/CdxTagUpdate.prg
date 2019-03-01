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
        // Methods for updating (adding, inserting, deleting) keys into indices
        PRIVATE METHOD _UpdateError(ex AS Exception, strFunction AS STRING, strMessage AS STRING) AS VOID
            SELF:RDD:_dbfError(ERDD.CREATE_ORDER, GenCode.EG_CORRUPTION, strFunction, strMessage+" for "+SELF:Filename+" "+SELF:OrderName )
            RETURN
            
        INTERNAL METHOD NewLeafPage() AS CdxLeafPage
            LOCAL oLeaf := NULL AS CdxLeafPage
            TRY
                LOCAL buffer AS BYTE[]
                buffer := _bag:AllocBuffer()
                oLeaf  := CdxLeafPage{_bag, -1, buffer, SELF:KeyLength}
                oLeaf:InitBlank(SELF)
                oLeaf:Write() // will give it a pagenumber
                SELF:OrderBag:SetPage(oLeaf)
            CATCH ex AS Exception
                _UpdateError(ex,"CdxTag.NewLeafPage","Could not allocate Leaf page")
            END TRY
            RETURN oLeaf

        INTERNAL METHOD NewBranchPage() AS CdxBranchPage
            LOCAL oBranch := NULL AS CdxBranchPage
            TRY
                LOCAL buffer AS BYTE[]
                // Allocate new Branch Page
                buffer  := _bag:AllocBuffer()
                oBranch := CdxBranchPage{_bag, -1, buffer, SELF:KeyLength}
                oBranch:InitBlank(SELF)
                oBranch:Tag    := SELF
                oBranch:Write() // will give it a pagenumber
                SELF:OrderBag:SetPage(oBranch)
            CATCH ex AS Exception
                _UpdateError(ex,"CdxTag.NewBranchPage","Could not allocate Branch page")
            END TRY
            RETURN oBranch

        INTERNAL METHOD DoAction(action AS CdxResult) AS CdxResult
            // Dispatcher that evaluates actions that have to be done.
            // action is a Flag value, so we look for each of the following actions
            LOCAL nextAction AS CdxResult
            LOCAL nextLevel  AS LOGIC
            LOCAL stackSave AS INT
            stackSave   := SELF:_topStack
            nextAction := CdxResult.OK
            DO WHILE action != CdxResult.OK
                FOREACH flag AS CdxResult IN possibleActions
                    IF action:HasFlag(flag)
                        nextLevel := TRUE
                        SWITCH flag
                        CASE CdxResult.SplitLeaf
                            nextAction |= SELF:SplitLeaf()
                            // after splitting we want to write a new key to the newly allocated page
                            nextLevel := FALSE
                        CASE CdxResult.AddLeaf
                            // after adding the leaf we want to write a new key to the newly allocated page
                            nextAction |= SELF:AddLeaf()
                            nextLevel := FALSE
                        CASE CdxResult.Delete
                            // this will return DeleteFromParent so we increase the next level
                            nextAction |= SELF:DeletePage()

                        CASE CdxResult.ChangeParent
                            // can't pass oLeaf because we also change the parent of branch pages
                            // after this is done we may have to continue on the upper level if the last
                            // key on the parents page was changed
                            nextAction |= SELF:ChangeParent()

                        CASE CdxResult.InsertParent
                            // insert parent above current level on the stack
                            nextAction |= SELF:InsertParent()
                            stackSave++
                            nextLevel := FALSE
                        CASE CdxResult.SplitParent
                            // split parent. This may trigger an insert of a higher level parent
                            // and will most likely also trigger an update of the higher level parent for the
                            // last key on the LHS of the split
                            nextAction |= SELF:SplitParent()
                            nextLevel := FALSE
                        CASE CdxResult.DeleteFromParent
                            // This removes the key for a page from its parent page
                            nextAction |= SELF:DeleteFromParent()
                            nextLevel := FALSE
                        CASE CdxResult.OutOfBounds
                            // Throw an exception
                            _UpdateError(NULL, "CdxTag.DoAction","Out of Bounds when writing to a page")
                        CASE CdxResult.ExpandRecnos
                            // after expanding we either need to split or update the current page, so no nextlevel
                            nextAction |= SELF:ExpandRecnos()
                            nextLevel := FALSE
                        OTHERWISE
                            _UpdateError(NULL, "CdxTag.DoAction","Unexpected Enum value "+flag:ToString())
                        END SWITCH
                        action := _AND (Action, _NOT(flag))
                        IF nextLevel
                            SELF:_topStack --
                        ENDIF
                    ENDIF
                    IF action == CdxResult.OK
                        EXIT
                    ENDIF
                    
                NEXT
                action := nextAction
                nextAction := CdxResult.OK
                
            ENDDO
            // maybe we need to handle insert of new root page
            SELF:_topStack := stackSave 
            RETURN cdxResult.Ok

        INTERNAL METHOD DeletePage() AS CdxResult
            // Establish Link between our Left and Right
            VAR oPage := SELF:CurrentTop
            IF oPage == NULL
                // Should not happen...
                RETURN CdxResult.Ok
            ENDIF
            IF oPage:HasLeft
                VAR pageL := SELF:GetPage(oPage:LeftPtr)
                IF pageL != NULL
                    pageL:RightPtr := oPage:RightPtr
                    pageL:Write()
                ENDIF
                oPage:LeftPtr := -1
            ENDIF
            IF oPage:HasRight
                VAR pageR := SELF:GetPage(oPage:RightPtr)
                pageR:LeftPtr := oPage:LeftPtr
                pageR:Write()
                oPage:RightPtr := -1
            ENDIF
            // now update the reference to this page in the parent node
            VAR oParent := SELF:GetParent(oPage)
            IF oParent == NULL
                // Top level page, don't remove
                oPage:InitBlank(SELF)
                RETURN CdxResult.Ok
            ELSE
                SELF:OrderBag:FreePage(oPage)
                RETURN CdxResult.DeleteFromParent
            ENDIF

        INTERNAL METHOD DeleteFromParent() AS CdxResult
            VAR oTop    := SELF:CurrentTop ASTYPE CdxBranchPage
            VAR oParent := SELF:GetParent(oTop)
            VAR result := CdxResult.OK
            IF oParent != NULL_PTR
                // this can be the top level. In that case we should not get here at all
                LOCAL nPos AS LONG
                nPos := oParent:FindPage(oTop:PageNo)
                IF nPos != -1
                    result := oParent:Delete(nPos)
                    // when the last key of the parent was changed then
                    // we need to propagate that to the top
                    IF nPos == oParent:NumKeys -1
                        oParent := SELF:GetParent(oParent)
                        IF oParent != NULL
                            result := CdxResult.ChangeParent
                        ENDIF
                    ENDIF
                ELSE
                    // Todo: this is a logical problem
                     _UpdateError(NULL, "CdxTag.DeleteFromParent","Could not find entry for page # ";
                        +oTop:PageNo:ToString() +" in page "+oParent:PageNo:ToString())
                ENDIF 
                
            ENDIF
            RETURN CdxResult.Ok
        PRIVATE oPendingNode AS CdxPageNode
        INTERNAL METHOD ChangeParent() AS CdxResult
            VAR oTop      := SELF:CurrentTop 
            VAR oParent   := SELF:GetParent(oTop)
            VAR result    := CdxResult.Ok
            // the page on the top is the new page.
            oTop          := SELF:GetPage(oTop:LeftPtr)
            
            VAR oLast     := oTop:LastNode 
            IF oParent != NULL_PTR
                LOCAL nPos AS LONG
                nPos := oParent:FindPage(oTop:PageNo)
                IF nPos != -1
                    result := oParent:Replace(nPos, oLast)
                    // when the last key of the parent was changed then
                    // we need to propagate that to the top
                    IF nPos == oParent:NumKeys -1
                        oParent := SELF:GetParent(oParent)
                        IF oParent != NULL
                            result := CdxResult.ChangeParent
                        ENDIF
                    ENDIF
                ELSE
                    // insert page in parent
                    result := oParent:Add(oLast)
                    IF result == CdxResult.SplitParent
                        // failed to insert the key
                        oPendingNode := oLast
                    ENDIF
                ENDIF
            ENDIF
            RETURN result

        INTERNAL METHOD InsertParent() AS CdxResult
            LOCAL oParent AS CdxBranchPage
            LOCAL oTop    AS CdxTreePage
            LOCAL nTop    AS LONG
            nTop    := SELF:CurrentStack:Page
            oTop    := SELF:GetPage(nTop)
            oParent := SELF:NewBranchPage()
            SELF:InsertOnStack(oParent, oTop)
            RETURN CdxResult.Ok

        INTERNAL METHOD SplitParent() AS CdxResult
            LOCAL oNewPage AS CdxBranchPage
            LOCAL oBranch  AS CdxBranchPage
            VAR result   := CdxResult.Ok
            oBranch  := SELF:CurrentTop
            oNewPage := SELF:NewBranchPage()
            IF oNewPage != NULL_PTR
                oNewPage:LeftPtr := oBranch:PageNo
                oBranch:RightPtr := oNewPage:PageNo
            ENDIF
            IF SELF:GetParent(oBranch) == NULL
                result := CdxResult.InsertParent
            ENDIF
            result |= CdxResult.ChangeParent
            IF oPendingNode != NULL
                oNewPage:Add(oPendingNode)
                oPendingNode := NULL
            ENDIF
            SELF:SetPage(oNewPage, oNewPage:NumKeys)
            RETURN result


        INTERNAL METHOD AddLeaf() AS CdxResult
            // Allocate a new leaf page and leave it empty
            VAR result      := CdxResult.OK
            VAR oPage       := SELF:CurrentLeaf
            VAR pageR       := SELF:NewLeafPage()
            IF  oPage != NULL
                oPage:RightPtr := pageR:PageNo
                pageR:LeftPtr  := oPage:PageNo
                oPage:Write()
            ENDIF
            // when we are at the top then we must insert a parent above this level
            IF SELF:_topStack == 1
                result |= CdxResult.InsertParent
            ENDIF
            // make sure that the last node from the current leaf is added to the parent
            result |= CdxResult.ChangeParent
            // set the new page as "current" page on the stack
            SELF:SetPage(pageR, pageR:NumKeys)
            RETURN result

        INTERNAL METHOD SplitLeaf() AS CdxResult
            // Assumes Leaf page is on top of the stack
            LOCAL oNewPage  AS CdxLeafPage
            VAR oPage     := SELF:CurrentLeaf
            // allocate a new page and also takes care of updating the parent level when needed
            VAR result    := SELF:AddLeaf()         
            oNewPage      := SELF:CurrentLeaf    // should be the new page
            LOCAL numKeys := oPage:NumKeys AS LONG
            LOCAL nRight  := numKeys / 2 AS LONG
            LOCAL nLeft   := numKeys - nRight AS LONG
            // Force expand on oPage
            LOCAL oLeaves := oPage:GetLeaves() AS IList<CdxLeaf>
            // Copy 2nd half to page on the right
            oNewPage:InitBlank(SELF)
            FOR VAR i := nLeft TO oLeaves:Count -1
                result := oNewPage:Add(oLeaves[i]:Recno, oLeaves[i]:Key)
                IF result != CdxResult.Ok
                    // should not happen
                    _UpdateError(NULL, "CdxTag.SplitLeaf","Could not insert key in Right Page")
                ENDIF
            NEXT
            oNewPage:LeftPtr := oPage:PageNo
            // Copy 1st half to page on the Left
            // In theory it would be enough to set the numkeys and recalc the Freespace
            // but this works easier
            oPage:InitBlank(SELF)
            FOR VAR i := 0 TO nLeft -1
                result := oPage:Add(oLeaves[i]:Recno, oLeaves[i]:Key)
                IF result != CdxResult.Ok
                    // should not happen
                    _UpdateError(NULL,"CdxTag.SplitLeaf","Could not insert key in Left Page")
                ENDIF
            NEXT
            oPage:RightPtr := oNewPage:PageNo
            // we must add the last record of the current page to 
            result |= CdxResult.ChangeParent
            RETURN result

        PRIVATE METHOD ExpandRecnos() AS CdxResult
            VAR oLeaf := SELF:CurrentLeaf
            IF oLeaf == NULL
                _UpdateError(NULL, "CdxTag.ExpandRecnos","Attempt to Expand recnos when top of stack is not a leaf")
            ENDIF
            VAR result := oLeaf:ExpandRecnos()
            RETURN result
            
            

        PRIVATE METHOD SplitBranch(oPage AS CdxBranchPage, oNewPage OUT CdxBranchPage) AS CdxResult
            LOCAL result AS CdxResult
            LOCAL nParentPos AS LONG
            VAR parent := SELF:GetParent(oPage)
            IF parent == NULL
                // This can only happen when oLeaf was top level and we will add a parent to the leaf
                Debug.Assert(_Header:RootPage == oPage:PageNo)
                parent := NewBranchPage()
                nParentPos := -1
            ELSE
                VAR oNode  := oPage:LastNode
                nParentPos := parent:FindPage(oPage:PageNo)       // position of our lastkey on the parent page
                IF parent:NumKeys != 0 .AND. nParentPos == -1
                    _UpdateError(NULL, "CdxTag.SplitBranch","Could not locate reference to Branch page in parent")
                ENDIF
            ENDIF
            oNewPage := SELF:NewBranchPage()
            LOCAL numKeys := oPage:NumKeys AS LONG
            LOCAL nRight  := numKeys / 2 AS LONG
            LOCAL nLeft   := numKeys - nRight AS LONG
            // Force expand on oLeafPage
            LOCAL oBranches := oPage:Branches AS IList<CdxBranch>
            
            // Copy 2nd half to page on the right
            oNewPage:InitBlank(SELF)
            FOR VAR i := nLeft TO oBranches:Count -1
                result := oNewPage:Add(oBranches[i]:Recno, oBranches[i]:ChildPage ,oBranches[i]:Key)
                IF result != CdxResult.Ok
                    // should not happen
                    _UpdateError(NULL, "CdxTag.SplitLeaf","Could not insert key in Right Page")
                ENDIF
            NEXT
            oNewPage:LeftPtr := oPage:PageNo
            oNewPage:Write()
            // Copy 1st half to page on the Left
            // In theory it would be enough to set the numkeys and recalc the Freespace
            // but this works easier
            oPage:InitBlank(SELF)
            FOR VAR i := 0 TO nLeft -1
                result := oPage:Add(oBranches[i]:Recno, oBranches[i]:ChildPage ,oBranches[i]:Key)
                IF result != CdxResult.Ok
                    // should not happen
                    _UpdateError(NULL,"CdxTag.SplitBranch","Could not insert key in Left Page")
                ENDIF
            NEXT
            oPage:RightPtr := oNewPage:PageNo
            oPage:Write()
            // if there was a parent then we need to remove the old node from the parent
            // and insert 2 nodes at that position
            // The delete and insert methods on the Branch page will take care of updating a parent page when this parent exists
            IF nParentPos != -1
                result      := parent:Replace(nParentPos, oNewPage:LastNode)
                result      := parent:Insert(nParentPos, oPage:LastNode)
            ELSE
                // this can/should only happen if the parent is new
                result  := parent:Insert(0, oNewPage:LastNode)
                result  := parent:Insert(0, oPage:LastNode)
            ENDIF
//            IF result == CdxResult.Split
//                // The parent node is full and must be extended to the right
//                LOCAL newParent AS CdxBranchPage
//                result := SELF:SplitBranch(parent, OUT newParent)
//                IF result == CdxResult.Ok
//                    result := newParent.Insert(newParent.NumKeys, oPage:LastNode)
//                ENDIF
//                IF result != CdxResult.Ok
//                    _UpdateError(NULL,"CdxTag.SplitBranch","Split failed")            
//                ENDIF
//            ENDIF
//            IF result != CdxResult.Ok
//                _UpdateError(NULL,"CdxTag.SplitBranch","Split failed")            
//            ENDIF
            RETURN result
  
            
            
        PRIVATE METHOD _insertKey(oLeafPage AS CdxLeafPage, nRecord AS LONG, aBytes AS BYTE[], nPos := -1 AS LONG) AS CdxResult
            RETURN CdxResult.Ok
            
        PRIVATE METHOD _keyUpdate(recordNo AS LONG , lNewRecord AS LOGIC ) AS LOGIC
            IF SELF:Shared
                SELF:XLock()
            ENDIF
            SELF:_saveCurrentKey(recordNo, SELF:_newvalue)
            IF lNewRecord .AND. ! _newvalue:ForCond
                // New record and it does not meet the for condition, so no need to update or delete anything
                SELF:_newValue:CopyTo(SELF:_currentValue)
                RETURN TRUE
            ENDIF
            LOCAL changed := FALSE AS LOGIC
            IF !lNewRecord
                // find and delete existing key
                changed := SELF:_compareFunc(SELF:_newValue:Key, SELF:_currentValue:Key, SELF:_keySize) != 0
                IF changed
                    SELF:_topStack := 0
                ENDIF
                VAR recno := SELF:_goRecord(SELF:_currentValue:Key, SELF:_keySize, recordNo)
                IF (SELF:_TopStack != 0 .AND. !SELF:_Conditional) .OR. recno  != 0
                    IF changed .OR. !_newValue:ForCond
                        SELF:_deleteKey()
                    ENDIF
                ELSE
                    IF !SELF:Unique .AND. !SELF:_Conditional .AND. !SELF:Custom
                        SELF:_oRdd:_dbfError( SubCodes.ERDD_KEY_NOT_FOUND, GenCode.EG_DATATYPE,SELF:fileName)
                    ENDIF
                ENDIF
            ENDIF
            IF (lNewRecord .OR. changed) .AND. _newvalue:ForCond
                // new record or changed record, so insert the new key in the tree
                SELF:ClearStack()
                IF SELF:Unique
                    // Todo
                    IF SELF:_locate(SELF:_newValue:Key, SELF:_keySize, SearchMode.Left, SELF:_rootPage) == 0
                        SELF:_addKey()
                    ELSE
                        SELF:ClearStack()
                    ENDIF
                ELSE
                    // position on the new key
                    SELF:_locate(SELF:_newValue:Key, SELF:_keySize, SearchMode.Right, SELF:_rootPage)
                    SELF:_addKey()
                ENDIF
                SELF:ClearStack()
                SELF:_Hot := TRUE
            ENDIF
            SELF:_newValue:CopyTo(SELF:_currentValue)

            IF SELF:Shared
                SELF:_Header:Version++
                SELF:_Header:Write()
                SELF:GoCold()
                SELF:UnLock()
            ENDIF
            RETURN TRUE
            
        PRIVATE METHOD _addKey() AS LOGIC
            LOCAL page   AS CdxLeafPage
            LOCAL pos    AS LONG
            LOCAL result AS CdxResult
            page    := SELF:_readPagesFromStack()
            pos     := SELF:CurrentStack:Pos
            IF pos == SELF:CurrentStack:Count
                // append at end of page
                result := page:Insert(pos, SELF:_newValue)
            ELSE
                // insert on the page
                result := page:Insert(pos, SELF:_newValue)
            ENDIF
            IF pos == page:NumKeys -1
                // Todo must update the parent of the page
//                IF page:Parent != NULL
//                    LOCAL parentPos := page:parent:FindPage(page:PageNo) AS LONG
//                    IF parentPos != -1
//                        page:parent:SetRecno(parentPos, _newValue:Recno)
//                        page:parent:SetKey(parentPos, _newValue:Key)
//                    ENDIF
//                ENDIF
            ENDIF
            IF result != CdxResult.OK
                result := SELF:DoAction(result)
            ENDIF
            RETURN TRUE

        PRIVATE METHOD _readPagesFromStack AS CdxLeafPage
            VAR pageNo  := SELF:CurrentStack:Page
            VAR page    := (CdxLeafPage) SELF:OrderBag:GetPage(pageNo, _keySize, SELF)
            RETURN page
            
        PRIVATE METHOD _deleteKey() AS VOID
            LOCAL page   AS CdxLeafPage
            LOCAL pos    AS LONG
            LOCAL result AS CdxResult
            page := SELF:_readPagesFromStack()
            pos     := SELF:CurrentStack:Pos
            result  := page:Delete(pos)
            IF result != CdxResult.OK
                result := SELF:DoAction(result)
            ENDIF
            RETURN
            
    END CLASS
    
END NAMESPACE


