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

        INTERNAL METHOD DoAction(action AS CdxAction) AS CdxAction
            // Dispatcher that evaluates actions that have to be done.
            // action is a Flag value, so we look for each of the following actions
            TRY
                DO WHILE action:Type != CdxActionType.OK
                    SWITCH action:Type
                    CASE CdxActionType.AddKey
                        // Called during index creation
                        action := SELF:AddKey(action)
                    CASE CdxActionType.DeleteKey
                        // after adding the leaf we want to write a new key to the newly allocated page
                        action := SELF:DeleteKey(action)

                    CASE CdxActionType.InsertKey
                        // after adding the leaf we want to write a new key to the newly allocated page
                        action := SELF:InsertKey(action)

                    CASE CdxActionType.AddLeaf
                        // after adding the leaf we want to write a new key to the newly allocated page
                        action := SELF:AddLeaf(action)

                    CASE CdxActionType.Delete
                        // this will return DeleteFromParent so we increase the next level
                        action := SELF:DeletePage(action)

                    CASE CdxActionType.ChangeParent
                        // when the last key of a page was changed we need to update the parent
                        action := SELF:ChangeParent(action)

                    CASE CdxActionType.InsertParent
                        // insert parent above current level on the stack
                        action := SELF:InsertParent(action)

                    CASE CdxActionType.SplitBranch
                        // split parent. This may trigger an insert of a higher level parent
                        // and will most likely also trigger an update of the higher level parent for the
                        // last key on the LHS of the split
                        action := SELF:SplitBranch(action)

                    CASE CdxActionType.DeleteFromParent
                        // This removes the key for a page from its parent page
                        action := SELF:DeleteFromParent(action)

                    CASE CdxActionType.OutOfBounds
                        // Throw an exception
                        _UpdateError(NULL, "CdxTag.DoAction","Out of Bounds when writing to a page")
                        action := CdxAction.Ok
                    CASE CdxActionType.ExpandRecnos
                        // after expanding we either need to split or update the current page, so no nextlevel
                        action := SELF:ExpandRecnos(action)
                    OTHERWISE
                        _UpdateError(NULL, "CdxTag.DoAction","Unexpected Enum value "+action:Type:ToString())
                    END SWITCH
                ENDDO
            CATCH ex as Exception
                _UpdateError(ex,"CdxTag.Doaction","Error performing action "+action:Type:ToString())

            END TRY
            RETURN action

        PRIVATE METHOD DeletePage(action as CdxAction) AS CdxAction
            // Establish Link between our Left and Right
            VAR oPage := action:Page 
            IF oPage == NULL
                // Should not happen...
                RETURN CdxAction.Ok
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
            VAR oParent := SELF:Stack:GetParent(oPage)
            IF oParent == NULL
                // Top level page, don't remove, but change from Root Branch to Leaf
                oPage:SetEmptyRoot()
                SELF:OrderBag:FlushPages()
                SELF:ClearStack()
                return CdxAction.OK
            ELSE
                //SELF:SetChildToProcess(oPage:PageNo)
                SELF:OrderBag:FreePage(oPage)
                RETURN CdxAction.DeleteFromParent(oPage)
            ENDIF

        PRIVATE METHOD DeleteFromParent(action as CdxAction) AS CdxAction
            VAR oParent := SELF:Stack:GetParent(action:Page) astype CdxbranchPage
            VAR result := CdxAction.OK
            IF oParent != NULL_PTR
                // this can be the top level. In that case we should not get here at all
                LOCAL nPos AS LONG
                nPos := oParent:FindPage(action:Page:PageNo)
                IF nPos != -1
                    result := oParent:Delete(nPos)
                ELSE
                    // Todo: this is a logical problem
                     _UpdateError(NULL, "CdxTag.DeleteFromParent","Could not find entry for child on parent page")
                ENDIF 
                
            ENDIF
            RETURN result

        INTERNAL METHOD ChangeParent(action as CdxAction) AS CdxAction
            VAR oTop      := action:Page 
            VAR oParent   := SELF:Stack:Getparent(oTop) ASTYPE  CdxBranchPage
            VAR result    := CdxAction.Ok
            VAR oLast     := oTop:LastNode
            LOCAL oPageR  := NULL as CdxTreepage
            if oParent == NULL_OBJECT
                // this may happen after a SplitBranch. The new page is then on the stack
                // and the oldpage is in the action
                IF oTop:HasRight
                    oPageR := SELF:Getpage(oTop:RightPtr)
                    oParent := SELF:Stack:Getparent(oPageR) ASTYPE  CdxBranchPage
                ENDIF
            ENDIF
            IF oParent != NULL_OBJECT
                LOCAL nPos AS LONG
                nPos := oParent:FindPage(oTop:PageNo)
                IF nPos != -1
                    result := oParent:Replace(nPos, oLast)
                    if oPageR != NULL
                        nPos := oParent:FindPage(oPageR:PageNo)
                        if nPos == -1
                            result := oParent:Add(oPageR:LastNode)
                            result := SELF:DoAction(result)
                        endif
                    endif
                    // when the last key of the parent was changed then
                    // we need to propagate that to the top
                    if result:Type == CdxActionType.OK
                        IF nPos == oParent:NumKeys -1
                            var oGrandParent := SELF:Stack:GetParent(oParent)
                            IF oGrandParent != NULL
                                result := CdxAction.ChangeParent(oParent)
                            ENDIF
                        ENDIF
                    ENDIF
                ELSE
                    // insert page in parent
                    result := oParent:Add(oLast)
                    if result:Type == CdxActionType.OK
                        var oGrandParent := SELF:Stack:GetParent(oParent)
                        IF oGrandParent != NULL
                            result := CdxAction.ChangeParent(oParent)
                        ENDIF
                    endif
                ENDIF
            ENDIF
            RETURN result
        
        INTERNAL METHOD InsertParent(action as CdxAction) AS CdxAction
            // We assume the page in the action is the right of the two pages that need to get a parent
            LOCAL oParent AS CdxBranchPage
            LOCAL oTop    AS CdxTreePage
            oTop    := action:Page
            oParent := SELF:NewBranchPage()
            SELF:InsertOnStack(oParent, oTop)
            SELF:SetRoot(oParent)
            IF oTop:NumKeys > 0
                oParent:Add(oTop:LastNode)
            ENDIF
            // make sure new parent has both of its children
            IF oTop:HasLeft
                oTop := SELF:GetPage(oTop:LeftPtr)
                IF oTop != NULL_OBJECT
                    oParent:Insert(0, oTop:LastNode)
                ENDIF
            ENDIF
            RETURN CdxAction.Ok
        
        INTERNAL METHOD SetRoot(oPage AS CdxTreePage) AS VOID
            local oldRoot as CdxTreePage
            oldRoot := SELF:GetPage(_rootPage)
            if oldRoot != NULL
                if oldRoot:PageType:HasFlag(CdxPageType.Root)
                    oldRoot:PageType := _AND(oldRoot:PageType, _NOT(CdxPageType.Root))
                    oldRoot:Write()
                ENDIF
            ENDIF
            Header:RootPage := oPage:PageNo
            Header:Write()
            oPage:SetRoot()
            oPage:Write()
            _rootPage := oPage:PageNo
            RETURN

        INTERNAL METHOD SplitBranch(action as CdxAction) AS CdxAction
            LOCAL oPageR AS CdxBranchPage
            LOCAL oPageL  AS CdxBranchPage
            VAR result   := CdxAction.Ok
            oPageL  := (CdxBranchPage) action:Page 
            oPageR := SELF:NewBranchPage()
            IF oPageR != NULL_PTR
                oPageR:LeftPtr := oPageL:PageNo
                oPageL:RightPtr := oPageR:PageNo
            ENDIF
            IF action:Recno != 0
                oPageR:Add(action:Recno, action:ChildPage, action:Key)
            ENDIF
            VAR oParent := SELF:Stack:GetParent(oPageL) ASTYPE CdxbranchPage
            IF oParent == NULL
                result := CdxAction.InsertParent(oPageR)
            ELSE
                result := CdxAction.ChangeParent(oPageL)
                result := SELF:DoAction(result)
                result := oParent:Add(oPageR:LastNode)
            ENDIF
            SELF:AdjustStack(oPageL, oPageR, oPageR:NumKeys)
            RETURN result

        INTERNAL METHOD AddKey(action as CdxAction) AS CdxAction
            // This is called during index creation.
            // Please note that we do not update the link to the parent on top
            // because that will slow down indexing
            // When the page is split then the topreference will be updated
            // and at the end of the indexing the top reference for the last page will be written.
            var page := SELF:Stack:Top:Page astype CdxLeafPage
            IF page is CdxLeafPage leaf
                action := leaf:Add(action:Recno, action:Key)
                if action:Type == CdxActionType.OK
                    SELF:Stack:Top:Pos++
                endif
            ELSE
                _UpdateError(null,"CdxTag.AddKey","Page is not a Leaf page")
            ENDIF
            RETURN action

        INTERNAL METHOD DeleteKey(action as CdxAction) AS CdxAction
            IF action:Page is CdxLeafPage leaf
                return leaf:Delete(action:Pos)
            ENDIF
            _UpdateError(null,"CdxTag.DeleteKey","Page is not a Leaf page")
            RETURN CdxAction.OK

        INTERNAL METHOD InsertKey(action as CdxAction) AS CdxAction
            IF action:Page is CdxLeafPage leaf
                return leaf:Insert(action:Pos, action:Recno, action:Key)
            ENDIF
            _UpdateError(null,"CdxTag.AddKey","Page is not a Leaf page")
            RETURN CdxAction.OK

        INTERNAL METHOD AddLeaf(action as CdxAction) AS CdxAction
            // Allocate a new leaf page and add recno and key from action
            VAR oPageL      := action:Page
            VAR oPageR      := SELF:NewLeafPage()
            IF  oPageL != NULL
                oPageL:RightPtr := oPageR:PageNo
                oPageR:LeftPtr  := oPageL:PageNo
                oPageL:Write()
            ENDIF
            // when we are at the top then we must insert a parent above this level
            action := oPageR:Add(action:Recno, action:Key)
            if action:Type != CdxActionType.Ok
                _UpdateError(null,"CdxTag.AddLeaf","Could not insert key into new leaf page")
            ENDIF
            IF SELF:Stack:Count == 1
                action := CdxAction.InsertParent(oPageR)
            ELSE
                action := CdxAction.ChangeParent(oPageL)
            ENDIF
            // set the new page as "current" page on the stack
            SELF:AdjustStack(oPageL, oPageR, oPageR:NumKeys)
            RETURN action

     
        PRIVATE METHOD ExpandRecnos(action as CdxAction) AS CdxAction
            VAR oLeaf := SELF:CurrentLeaf
            IF oLeaf == NULL
                _UpdateError(NULL, "CdxTag.ExpandRecnos","Attempt to Expand recnos when top of stack is not a leaf")
            ENDIF
            VAR result := oLeaf:ExpandRecnos()
            RETURN result
            
            
        /*
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
        */

        INTERNAL METHOD AddKey(recordNo as LONG) AS LOGIC
            IF ! SELF:_Custom
                RETURN FALSE
            ENDIF
            SELF:_saveCurrentKey(recordNo, SELF:_newvalue)
            var nRec := SELF:_locate(SELF:_newValue:Key, SELF:_keySize, SearchMode.Right, SELF:_rootPage)
            var page := SELF:Stack:Top:Page
            var pos  := SELF:Stack:Top:Pos
            SELF:DoAction(CdxAction.InsertKey(page, pos, SELF:_newValue:Recno, SELF:_newValue:Key))
            return TRUE

        INTERNAL METHOD SetCustom() as LOGIC
            local lOld := SELF:_Custom as LOGIC
            SELF:Header:Options |= CdxOptions.Custom
            SELF:_Custom := TRUE
            RETURN lOld

        INTERNAL METHOD DeleteKey(recordNo as LONG) AS LOGIC
            IF ! SELF:_Custom
                RETURN FALSE
            ENDIF
            SELF:_saveCurrentKey(recordNo, SELF:_currentValue)
            VAR recno := SELF:_goRecord(SELF:_currentValue:Key, SELF:_keySize, recordNo)
            if recno == recordNo
                var page := SELF:Stack:Top:Page
                var pos  := SELF:Stack:Top:Pos
                SELF:DoAction(CdxAction.DeleteKey(page, pos))
                return TRUE
            endif
            return FALSE

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
            changed := SELF:_newValue:ForCond != SELF:_currentValue:ForCond
            IF !lNewRecord
                // find and delete existing key
                IF ! changed
                    changed := SELF:_compareFunc(SELF:_newValue:Key, SELF:_currentValue:Key, SELF:_keySize) != 0
                ENDIF
                IF changed
                    SELF:Stack:Clear()
                ENDIF
                if _currentValue:ForCond
                    VAR recno := SELF:_goRecord(SELF:_currentValue:Key, SELF:_keySize, recordNo)
                    IF ! SELF:Stack:Empty  .OR. recno  != 0
                        IF changed .AND. _currentValue:ForCond .and. recno == recordNo
                            var page := SELF:Stack:Top:Page
                            var pos  := SELF:Stack:Top:Pos
                            SELF:DoAction(CdxAction.DeleteKey(page, pos))
                        ENDIF
                    ELSE
                        IF !SELF:Unique .AND. !SELF:_Conditional .AND. !SELF:Custom
                            SELF:_oRdd:_dbfError( SubCodes.ERDD_KEY_NOT_FOUND, GenCode.EG_DATATYPE,SELF:fileName)
                        ENDIF
                    ENDIF
                ENDIF
            ENDIF
            IF (lNewRecord .OR. changed) .AND. _newvalue:ForCond
                // new record or changed record, so insert the new key in the tree
                SELF:ClearStack()
                IF SELF:Unique
                    // Todo
                    IF SELF:_locate(SELF:_newValue:Key, SELF:_keySize, SearchMode.Left, SELF:_rootPage) == 0
                        var page := SELF:Stack:Top:Page
                        var pos  := SELF:Stack:Top:Pos+1
                        SELF:DoAction(CdxAction.InsertKey(page, pos, SELF:_newValue:Recno, SELF:_newValue:Key))
                    ELSE
                        SELF:ClearStack()
                    ENDIF
                ELSE
                    // position on the new key
                    SELF:_locate(SELF:_newValue:Key, SELF:_keySize, SearchMode.Right, SELF:_rootPage)
                    var page := SELF:Stack:Top:Page
                    var pos  := SELF:Stack:Top:Pos
                    SELF:DoAction(CdxAction.InsertKey(page, pos, SELF:_newValue:Recno, SELF:_newValue:Key))
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
            
        PRIVATE METHOD _getLeaf AS CdxLeafPage
            VAR page    := SELF:CurrentStack:Page
            IF page IS CdxLeafPage leaf
                RETURN leaf
            ENDIF
            IF page:NumKeys > 0
                var pageNo  := page:LastNode:ChildPageNo
                page    := SELF:OrderBag:GetPage(pageNo, _keySize, SELF)
                IF page IS CdxLeafPage leaf
                    SELF:PushPage(page, 0)
                    RETURN leaf
                ENDIF
            ENDIF
            RETURN NULL
            
    END CLASS
    
END NAMESPACE


