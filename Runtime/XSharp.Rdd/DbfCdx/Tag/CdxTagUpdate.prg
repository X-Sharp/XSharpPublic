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

                    CASE CdxActionType.AddBranch
                        // after adding the leaf we want to write a new key to the newly allocated page
                        action := SELF:AddBranch(action)

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
            CATCH ex AS Exception
                _UpdateError(ex,"CdxTag.Doaction","Error performing action "+action:Type:ToString())

            END TRY
            RETURN action

        PRIVATE METHOD DeletePage(action AS CdxAction) AS CdxAction
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
                RETURN CdxAction.OK
            ELSE
                //SELF:SetChildToProcess(oPage:PageNo)
                SELF:OrderBag:FreePage(oPage)
                RETURN CdxAction.DeleteFromParent(oPage)
            ENDIF

        PRIVATE METHOD DeleteFromParent(action AS CdxAction) AS CdxAction
            VAR oParent := SELF:Stack:GetParent(action:Page) ASTYPE CdxbranchPage
            VAR result := CdxAction.OK
            IF oParent != NULL_OBJECT
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

        INTERNAL METHOD ChangeParent(action AS CdxAction) AS CdxAction
            VAR oTop      := action:Page 
            VAR oParent   := SELF:Stack:Getparent(oTop) ASTYPE  CdxBranchPage
            VAR result    := CdxAction.Ok
            VAR oLast     := oTop:LastNode
            LOCAL oPageR  := NULL AS CdxTreepage
            IF oParent == NULL_OBJECT
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
                    IF oPageR != NULL
                        nPos := oParent:FindPage(oPageR:PageNo)
                        IF nPos == -1 .AND. oPageR:LastNode != NULL
                            result := oParent:Add(oPageR:LastNode)
                            result := SELF:DoAction(result)
                        ENDIF
                    ENDIF
                    // when the last key of the parent was changed then
                    // we need to propagate that to the top
                    IF result:Type == CdxActionType.OK
                        IF nPos == oParent:NumKeys -1
                            VAR oGrandParent := SELF:Stack:GetParent(oParent)
                            IF oGrandParent != NULL
                                result := CdxAction.ChangeParent(oParent)
                            ENDIF
                        ENDIF
                    ENDIF
                ELSE
                    nPos := oParent:FindKey(oLast:KeyBytes)
                    IF nPos == -1
                        result := oParent:Add(oLast)
                    ELSE
                        result := oParent:Insert(nPos, oLast)
                    ENDIF
                    IF ! result.IsOk()
                        result := SELF:DoAction(result)
                    ENDIF
                    IF result:IsOk()
                        VAR oGrandParent := SELF:Stack:GetParent(oParent)
                        IF oGrandParent != NULL
                            result := CdxAction.ChangeParent(oParent)
                        ENDIF
                    ENDIF
                ENDIF
            ENDIF
            RETURN result
        
        INTERNAL METHOD InsertParent(action AS CdxAction) AS CdxAction
            // We assume the page in the action is the right of the two pages that need to get a parent
            LOCAL oParent AS CdxBranchPage
            LOCAL oTop    AS CdxTreePage
            oTop    := action:Page
            oParent := SELF:NewBranchPage()
            SELF:InsertOnTop(oParent)
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
            LOCAL oldRoot AS CdxTreePage
            IF _rootPage != oPage:PageNo
                oldRoot := SELF:GetPage(_rootPage)
                IF oldRoot != NULL .AND. oldRoot != oPage
                    IF oldRoot:IsRoot
                        oldRoot:ClearRoot()
                        oldRoot:Write()
                    ENDIF
                ENDIF
                Header:RootPage := oPage:PageNo
                Header:Write()
            ENDIF
            oPage:SetRoot()
            oPage:Write()
            _rootPage := oPage:PageNo
            RETURN

        INTERNAL METHOD AddBranch(action AS CdxAction) AS CdxAction
            LOCAL oPageR AS CdxBranchPage
            LOCAL oPageL  AS CdxBranchPage
            oPageL  := (CdxBranchPage) action:Page 
            oPageR := SELF:NewBranchPage()
            oPageL:AddRightSibling(oPageR)
            IF action:Pos > -1
                action := oPageL:Split(oPageR, action)
            ELSE
                action := oPageR:Add(action:Recno, action:ChildPage, action:Key)
            ENDIF
            IF action:Type != CdxActionType.Ok
                _UpdateError(NULL,"CdxTag.AddBranch","Could not insert key into new Branch page")
            ENDIF

            IF oPageL:IsRoot
                oPageL:ClearRoot()
                action := CdxAction.InsertParent(oPageR)
                SELF:AdjustStack(oPageL, oPageR, oPageR:NumKeys)
            ELSE
                action := CdxAction.ChangeParent(oPageL)
                action := SELF:DoAction(action)
                SELF:AdjustStack(oPageL, oPageR, oPageR:NumKeys)
                action := CdxAction.ChangeParent(oPageR)
            ENDIF
            action := SELF:DoAction(action)
            RETURN action


        INTERNAL METHOD AddKey(action AS CdxAction) AS CdxAction
            // This is called during index creation.
            // Please note that we do not update the link to the parent on top
            // because that will slow down indexing
            // When the page is split then the topreference will be updated
            // and at the end of the indexing the top reference for the last page will be written.
            VAR page := SELF:Stack:Top:Page ASTYPE CdxLeafPage
            IF page IS CdxLeafPage
                VAR leaf := (CdxLeafPage) page
                action := leaf:Add(action:Recno, action:Key)
                IF action:Type == CdxActionType.OK
                    SELF:Stack:Top:Pos++
                ENDIF
            ELSE
                _UpdateError(NULL,"CdxTag.AddKey","Page is not a Leaf page")
            ENDIF
            RETURN action

        INTERNAL METHOD DeleteKey(action AS CdxAction) AS CdxAction
            IF action:Page IS CdxLeafPage
                VAR leaf := (CdxLeafPage) action:Page
                RETURN leaf:Delete(action:Pos)
            ENDIF
            _UpdateError(NULL,"CdxTag.DeleteKey","Page is not a Leaf page")
            RETURN CdxAction.OK

        INTERNAL METHOD InsertKey(action AS CdxAction) AS CdxAction
            IF action:Page IS CdxLeafPage
                VAR leaf := (CdxLeafPage) action:Page
                RETURN leaf:Insert(action:Pos, action:Recno, action:Key)
            ENDIF
            _UpdateError(NULL,"CdxTag.AddKey","Page is not a Leaf page")
            RETURN CdxAction.OK

   

        INTERNAL METHOD AddLeaf(action AS CdxAction) AS CdxAction
            // Allocate a new leaf page and add recno and key from action
            VAR oPageL      := action:Page ASTYPE CdxLeafPage
            VAR oPageR      := SELF:NewLeafPage()
            oPageL:AddRightSibling(oPageR)
            IF action:Pos > -1
                action := oPageL:Split(oPageR, action)
            ELSE
                action := oPageR:Add(action:Recno, action:Key)
            ENDIF
            IF action:Type != CdxActionType.Ok
                _UpdateError(NULL,"CdxTag.AddLeaf","Could not insert key into new leaf page")
            ENDIF
            IF oPageL:IsRoot
                oPageL:ClearRoot()
                action := CdxAction.InsertParent(oPageR)
                SELF:AdjustStack(oPageL, oPageR, oPageR:NumKeys)
            ELSE
                action := CdxAction.ChangeParent(oPageL)
                action := SELF:DoAction(action)
                SELF:AdjustStack(oPageL, oPageR, oPageR:NumKeys)
                action := CdxAction.ChangeParent(oPageR)
            ENDIF
            action := SELF:DoAction(action)
            // set the new page as "current" page on the stack
            RETURN action

     
        PRIVATE METHOD ExpandRecnos(action AS CdxAction) AS CdxAction
            VAR oLeaf := SELF:CurrentLeaf
            IF oLeaf == NULL
                _UpdateError(NULL, "CdxTag.ExpandRecnos","Attempt to Expand recnos when top of stack is not a leaf")
            ENDIF
            VAR result := oLeaf:ExpandRecnos()
            RETURN result
            

        INTERNAL METHOD AddKey(recordNo AS LONG) AS LOGIC
            IF ! SELF:_Custom
                RETURN FALSE
            ENDIF
            SELF:_saveCurrentKey(recordNo, SELF:_newvalue)
            SELF:_locate(SELF:_newValue:Key, SELF:_keySize, SearchMode.Right, SELF:_rootPage)
            VAR page := SELF:Stack:Top:Page
            VAR pos  := SELF:Stack:Top:Pos
            SELF:DoAction(CdxAction.InsertKey(page, pos, SELF:_newValue:Recno, SELF:_newValue:Key))
            RETURN TRUE

        INTERNAL METHOD SetCustom() AS LOGIC
            LOCAL lOld := SELF:_Custom AS LOGIC
            SELF:Header:Options |= CdxOptions.Custom
            SELF:_Custom := TRUE
            RETURN lOld

        INTERNAL METHOD DeleteKey(recordNo AS LONG) AS LOGIC
            IF ! SELF:_Custom
                RETURN FALSE
            ENDIF
            SELF:_saveCurrentKey(recordNo, SELF:_currentValue)
            VAR recno := SELF:_goRecord(SELF:_currentValue:Key, SELF:_keySize, recordNo)
            IF recno == recordNo
                VAR page := SELF:Stack:Top:Page
                VAR pos  := SELF:Stack:Top:Pos
                SELF:DoAction(CdxAction.DeleteKey(page, pos))
                RETURN TRUE
            ENDIF
            RETURN FALSE

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
                    changed := SELF:__Compare(SELF:_newValue:Key, SELF:_currentValue:Key, SELF:_keySize) != 0
                ENDIF
                IF changed
                    SELF:Stack:Clear()
                ENDIF
                IF _currentValue:ForCond
                    VAR recno := SELF:_goRecord(SELF:_currentValue:Key, SELF:_keySize, recordNo)
                    IF ! SELF:Stack:Empty  .OR. recno  != 0
                        IF changed .AND. _currentValue:ForCond .AND. recno == recordNo
                            VAR page := SELF:Stack:Top:Page
                            VAR pos  := SELF:Stack:Top:Pos
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
                    IF SELF:_locate(SELF:_newValue:Key, SELF:_keySize, SearchMode.Left, SELF:_rootPage)  == 0
                        VAR page := SELF:Stack:Top:Page
                        VAR pos  := SELF:Stack:Top:Pos+1
                        SELF:DoAction(CdxAction.InsertKey(page, pos, SELF:_newValue:Recno, SELF:_newValue:Key))
                    ELSE
                        SELF:ClearStack()
                    ENDIF
                ELSE
                    SELF:_locate(SELF:_newValue:Key, SELF:_keySize, SearchMode.Right, SELF:_rootPage)  
                    VAR page := SELF:Stack:Top:Page
                    VAR pos  := SELF:Stack:Top:Pos
                    SELF:DoAction(CdxAction.InsertKey(page, pos, SELF:_newValue:Recno, SELF:_newValue:Key))
                ENDIF
                SELF:ClearStack()
                SELF:_Hot := TRUE
            ENDIF
            SELF:_newValue:CopyTo(SELF:_currentValue)

            IF SELF:Shared
                SELF:_Header:Version++
                SELF:_Header:Write()
                SELF:UnLock()
            ENDIF
            RETURN TRUE
            
        PRIVATE METHOD _getLeaf AS CdxLeafPage
            VAR page    := SELF:CurrentStack:Page
            IF page IS CdxLeafPage
                RETURN (CdxLeafPage) page
            ENDIF
            IF page:NumKeys > 0
                VAR pageNo  := page:LastNode:ChildPageNo
                page    := SELF:OrderBag:GetPage(pageNo, _keySize, SELF)
                IF page IS CdxLeafPage 
                    SELF:PushPage(page, 0)
                    RETURN (CdxLeafPage) page
                ENDIF
            ENDIF
            RETURN NULL
            
    END CLASS
    
END NAMESPACE


