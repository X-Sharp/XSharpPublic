//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING System.Collections.Generic
USING System.Text
using XSharp.RDD.Enums
using System.Diagnostics
BEGIN NAMESPACE XSharp.RDD.FlexFile


internal abstract class Index
private enum SeekTypes
    member SoftSeek_HalfKey
    member SoftSeek_FullKey
    member ExactSeek_HalfKey
    member ExactSeek_FullKey
    member PrevSeek_HalfKey
end enum

    private  IndexStack as IndexStack
    private  bFound as Logic
    protected area as FlexArea
    internal DiskCache as Cache
    internal CurrentKey as IndexKey

    internal constructor()
    end constructor

    internal method Init(ffarea as FlexArea) as void
        self:area       := ffarea
        self:bFound     := false
        self:IndexStack  := IndexStack{}
        self:CurrentKey := IndexKey{}
        self:DiskCache  := Cache{ffarea}
    end method
    internal method Clear as VOID
        self:DiskCache := NULL
        RETURN

    internal method Close() as void
        self:Clear()
    end method

    private method IndexSeek(Seektype as SeekTypes , ulValue1 as DWord , ulValue2 as DWord ) as Logic
        local lpNode as IndexNode
        local bDone as Logic
        var uiKey := 0U
        lpNode := null
        self:bFound := false
        bDone := false
        self:IndexStack:Reset()
        var ulDiskPos := self:RootNodePos
        if ulDiskPos != 0
            repeat
                lpNode := self:DiskCache:Get(ulDiskPos, false)
                if lpNode:bIsLeafNode
#ifdef DEBUG
                    Debug.Assert(lpNode.usKeyCount <= IndexNode.KEYS_PER_LEAF)
#endif
                    uiKey := 0
                    switch Seektype
                    case SeekTypes.SoftSeek_HalfKey
                    case SeekTypes.PrevSeek_HalfKey
                        FOR uiKey := 0 to lpNode:usKeyCount -1
                            if lpNode:GetLeafKey(uiKey):item1 >= ulValue1
                                self:bFound := true
                                exit
                            endif
                        NEXT

                    case SeekTypes.SoftSeek_FullKey
                        FOR uiKey := 0 to lpNode:usKeyCount -1
                            var key := lpNode:GetLeafKey(uiKey)
                            if key:item1 > ulValue1 .OR. ;
                                    (key:item1 == ulValue1 .AND. key:item2 >= ulValue2)
                                self:bFound := true
                                exit
                            endif
                        next


                    case SeekTypes.ExactSeek_HalfKey
                        FOR uiKey := 0 to lpNode:usKeyCount -1
                            var key := lpNode:GetLeafKey(uiKey)
                            if key:item1 == ulValue1
                                self:bFound := true
                                exit
                            endif
                        next

                    case SeekTypes.ExactSeek_FullKey
                        FOR uiKey := 0 to lpNode:usKeyCount -1
                            var key := lpNode:GetLeafKey(uiKey)
                            if (key:item1 == ulValue1) .AND. (key:item2 == ulValue2)
                                self:bFound := true
                                exit
                            endif
                        next
#ifdef DEBUG
                    otherwise
                        Debug.Assert(false)
#endif
                    end switch
                    self:IndexStack:Push(ulDiskPos, uiKey)
                    bDone := true
                    loop
                endif

#ifdef DEBUG
                Debug.Assert(lpNode.usKeyCount <= IndexNode.KEYS_PER_BRANCH)
#endif
                if ulValue2 == 0
                    FOR uiKey := 0 to lpNode:usKeyCount -1
                        var key := lpNode:GetBranchKey(uiKey)
                        if key:item1 >= ulValue1
                            exit
                        endif
                    next

                else
                    FOR uiKey := 0 to lpNode:usKeyCount -1
                        var key := lpNode:GetBranchKey(uiKey)
                        if (key:item1 > ulValue1) .OR. (((key:item1 == ulValue1) .AND. (key:item2 >= ulValue2)))
                            exit
                        endif
                    next

                endif
                if (uiKey == lpNode:usKeyCount) .AND. (Seektype != SeekTypes.PrevSeek_HalfKey)
                    // If we're not doing a SeekPrevious, then there's no
                    // point in going on because the highest key in this
                    // branch is less than the key we're seeking for.
                    bDone := true
                else
                    if uiKey == lpNode:usKeyCount
                        // The highest key in this branch is less than what
                        // we're looking for, but for a SeekPrevious we must
                        // go all the way to the last key in the index, since
                        // that's the key we want to return.
                        //
                        // Set uiKey to the last key in the branch and press on...
                        uiKey--
                    endif

                    // Always push leaf position even if key isn't found,
                    // SEEKPREVIOUS depends on this.

                    self:IndexStack:Push(ulDiskPos, uiKey)
                    ulDiskPos := lpNode:GetBranchKey(uiKey):ulDiskOffset
                endif
            until !(!bDone)

            if Seektype == SeekTypes.PrevSeek_HalfKey

                // This essentially does a skip previous after a softseek.
                // At this point, we are on a leaf node on a key that is equal to
                // or higher than the desired key.

                if (uiKey := self:IndexStack:KeyPos) != 0
                    // If we're on a key other than key 0, then simply skip
                    // backwards one key and we're there.

                    // If the softseek failed, we'll be 1 key past the end of
                    // the index, so skipping backwards will place us on the
                    // last key in the index.

                    self:IndexStack:KeyPos := --uiKey
                    lpNode := self:DiskCache:Get(self:IndexStack:DiskOffset, false)
                    self:bFound := true
                else
                    // Otherwise we landed on key zero, so we have to walk
                    // backwards to the nearest branch node that brached off
                    // on a key other than key zero.
                    bDone := false
                    self:bFound := false
                    repeat
                        self:IndexStack:Pop()
                        if self:IndexStack:Level == 0
                            // We're all the way back to the root,
                            // so no previous key was found.
                            bDone := true
                        else
                            if (uiKey := self:IndexStack:KeyPos) != 0
                                // We're on a branch node that branched
                                // off on a key greater than key zero.

                                // Get this branch in the cache again...
                                lpNode := self:DiskCache:Get(self:IndexStack:DiskOffset, false)
                                // Decrement key position and store...

                                self:IndexStack:KeyPos := --uiKey
                                ulDiskPos := lpNode:GetBranchKey(uiKey):ulDiskOffset
                                lpNode := self:DiskCache:Get(ulDiskPos, false)
                                while !lpNode:bIsLeafNode
                                    uiKey := (DWord)(lpNode:usKeyCount - 1)
                                    self:IndexStack:Push(ulDiskPos, uiKey)
                                    ulDiskPos := lpNode:GetBranchKey(uiKey):ulDiskOffset
                                    lpNode := self:DiskCache:Get(ulDiskPos, false)
                                end while
                                uiKey := (DWord)(lpNode:usKeyCount - 1)
                                self:IndexStack:Push(ulDiskPos, uiKey)
                                bDone := true
                                self:bFound := true
                            endif
                        endif
                    until bDone
                endif
            endif
        endif
        if self:bFound
            self:CurrentKey := lpNode:GetLeafKey(uiKey)
        else
            self:CurrentKey:Clear()
        endif
        return self:bFound
    end method


    private method InsertKey(lpLeafNode as IndexNode , NewKey as IndexKey ) as Logic
        local ulNewLeafNodePos := 0u as DWord
        local ulNewBranchNodePos := 0 as DWord
        local bOk as Logic
        local lpNewLeafNode as IndexNode
        local lpNewBranchNode as IndexNode
        local uiKeypos as DWord
        local lpParentNode as IndexNode
        local lpInsertInto as IndexNode
#ifdef DEBUG
        Debug.Assert(lpLeafNode.bIsLeafNode)
#endif

        var NewChildBranchKey := BranchKey{}
        var ChildBranchKey := BranchKey{}
        bOk := true

        // Insert the new key
        self:DiskCache:Lock(lpLeafNode)

        if lpLeafNode:usKeyCount == IndexNode.KEYS_PER_LEAF
            // Leaf node must be split...

            lpNewLeafNode := self:DiskCache:New(ref ulNewLeafNodePos, false)
            if lpNewLeafNode == null
                bOk := false
            else
                lpLeafNode:Split(lpNewLeafNode)
                // what node does new key go into?
                if lpLeafNode:FirstLeafKey > NewKey
                    lpInsertInto := lpLeafNode
                else
                    lpInsertInto := lpNewLeafNode
                endif
                uiKeypos := 0
                for var i := 0u  to IndexNode.KEYS_PER_LEAF / 2
                    if (lpInsertInto:GetLeafKey(i) > NewKey)
                        uiKeypos := i
                        exit
                    endif
                next
#ifdef DEBUG
                Debug.Assert(uiKeypos <= IndexNode.KEYS_PER_LEAF / 2)
#endif
                lpInsertInto:InsertKey(uiKeypos, NewKey)
                // Create a key that points to the child that was just
                // split.  This will replace the existing key in the parent.

                ChildBranchKey := BranchKey{}
                ChildBranchKey:Assign(lpLeafNode:LastLeafKey)
                ChildBranchKey:ulDiskOffset := self:IndexStack:DiskOffset

                // Create a key that points to the new leaf node we just created.
                // This will be inserted into the parent.

                NewChildBranchKey := BranchKey{}
                NewChildBranchKey:Assign(lpNewLeafNode:LastLeafKey)
                NewChildBranchKey:ulDiskOffset := ulNewLeafNodePos
                self:DiskCache:Unlock(lpNewLeafNode,  true)
            endif
        else
            // Leaf node has room for new key...

            uiKeypos := self:IndexStack:KeyPos
            lpLeafNode:InsertKey(uiKeypos, NewKey)
            // If last key was changed, save it so parent can be adjusted...

            if uiKeypos == lpLeafNode:usKeyCount - 1
                ChildBranchKey := BranchKey{}
                ChildBranchKey:Assign(lpLeafNode:GetLeafKey(uiKeypos))
                ChildBranchKey:ulDiskOffset := self:IndexStack:DiskOffset
            else
                ChildBranchKey:Clear()
            endif
            // No new node was created...
            NewChildBranchKey:Clear()
        endif
        self:DiskCache:Unlock(lpLeafNode, true)

        // Adjust the parent nodes

        while (bOk .AND. self:IndexStack:Level != 0) .AND. !ChildBranchKey:IsEmpty
            self:IndexStack:Pop()
            if self:IndexStack:Level == 0
                // No more levels to go back

                if !NewChildBranchKey:IsEmpty
                    // If at level 0 there is no parent.  If there is a new
                    // child branch key, then the root has to be split.

                    lpNewBranchNode := self:DiskCache:New(ref ulNewBranchNodePos, true)
                    if lpNewBranchNode == null
                        bOk := false
                    else
                        lpNewBranchNode:SetBranchKey(0, ChildBranchKey)
                        lpNewBranchNode:SetBranchKey(1, NewChildBranchKey)

                        lpNewBranchNode:usKeyCount := 2
                        lpNewBranchNode:bIsLeafNode := false

                        self:RootNodePos := ulNewBranchNodePos

                        self:DiskCache:Unlock(lpNewBranchNode, true)
                    endif
                endif
            else
                lpParentNode := self:DiskCache:Get(self:IndexStack:DiskOffset, true)
                uiKeypos := self:IndexStack:KeyPos
#ifdef DEBUG
                Debug.Assert(!lpParentNode.bIsLeafNode)
#endif
                // Replace key that pointed to child node with new key

                if !(lpParentNode:GetBranchKey(uiKeypos) == ChildBranchKey)
                    lpParentNode:SetBranchKey(uiKeypos, ChildBranchKey)
                    self:DiskCache:GoHot(lpParentNode)

                    // If highest key changed, then store it for updating
                    // this node's parent, otherwise zero it out to indicate
                    // that the parent doesn't have to be modified

                    if uiKeypos == lpParentNode:usKeyCount - 1
                        ChildBranchKey := lpParentNode:GetBranchKey(uiKeypos)
                        ChildBranchKey:ulDiskOffset := self:IndexStack:DiskOffset
                    else
                        ChildBranchKey:Clear()
                    endif
                else
                    ChildBranchKey:Clear()
                endif
                // Now insert the key for the new node (if any)

                if !NewChildBranchKey:IsEmpty
                    if lpParentNode:usKeyCount < IndexNode.KEYS_PER_BRANCH
                        // If there's room in this branch for another key,
                        // the new leaf key goes right next to the key of
                        // the leaf that was split.

                        uiKeypos++
                        lpParentNode:InsertKey(uiKeypos, NewChildBranchKey)

                        // If highest key changed, then store it for updating
                        // this node's parent.

                        if uiKeypos == lpParentNode:usKeyCount - 1
                            ChildBranchKey := lpParentNode:GetBranchKey(uiKeypos)
                            ChildBranchKey:ulDiskOffset := self:IndexStack:DiskOffset
                        endif
                        // We didn't add a new branch, so zero key out to
                        // indicate that...
                        NewChildBranchKey:Clear()
                        self:DiskCache:GoHot(lpParentNode)
                    else
                        // The parent branch is full, so it has to be split.

                        lpNewBranchNode := self:DiskCache:New(ref ulNewBranchNodePos, true)
                        if lpNewBranchNode == null
                            bOk := false
                        else
                            lpParentNode:Split(lpNewBranchNode)
                            if (lpNewBranchNode.FirstBranchKey > NewChildBranchKey)
                                lpInsertInto := lpParentNode
                            else
                                lpInsertInto := lpNewBranchNode
                            endif

                            for var i := 0U to IndexNode.KEYS_PER_BRANCH / 2
                                if (lpInsertInto:GetBranchKey(i) > NewChildBranchKey)
                                    uiKeypos := i
                                    exit
                                endif
                            next

#ifdef DEBUG
                            Debug.Assert(uiKeypos <= IndexNode.KEYS_PER_BRANCH / 2)
#endif

                            lpInsertInto:InsertKey(uiKeypos, NewKey)

                            ChildBranchKey := lpParentNode:LastBranchKey
                            ChildBranchKey:ulDiskOffset := self:IndexStack:DiskOffset

                            NewChildBranchKey := lpNewBranchNode:LastBranchKey
                            NewChildBranchKey:ulDiskOffset := ulNewLeafNodePos

                            self:DiskCache:Unlock(lpNewBranchNode, true)
                            self:DiskCache:GoHot(lpParentNode)
                        endif
                    endif
                endif
                self:DiskCache:Unlock(lpParentNode, false)
            endif
        end while
        return bOk
    end method


    protected virtual method TestIndexNode(ulDiskPos as DWord ) as Logic
        return false
    end method

    internal method Delete() as Logic
        local lpLeafNode    as IndexNode
        local lpBranchNode  as IndexNode
        local lpRootNode    as IndexNode
        local ulDiskPos     as DWord
        local uiKeypos      as DWord
        local NewHighestKey := null as IndexKey
        local bAdjustParent as Logic
        local bDeleteParentKey as Logic
        local bDone as Logic
        local bOk as Logic
        //
        bOk := true
        if self:bFound
            bAdjustParent := false
            bDeleteParentKey := false
            ulDiskPos   := self:IndexStack:DiskOffset
            uiKeypos    := self:IndexStack:KeyPos
            lpLeafNode  := self:DiskCache:Get(ulDiskPos, false)
#ifdef DEBUG
            Debug.Assert(lpLeafNode.bIsLeafNode)
            Debug.Assert(uiKeypos < lpLeafNode.usKeyCount)
#endif
            if lpLeafNode:usKeyCount == 1
                // Deleting only key in node...
                self:area:DeadIndexBlocks:Push(ulDiskPos)
                self:DiskCache:Kill(ulDiskPos)

                bAdjustParent := true
                bDeleteParentKey := true
            else
                if uiKeypos == (DWord)(lpLeafNode:usKeyCount - 1)

                    // Deleting highest key in node...

                    NewHighestKey := lpLeafNode:GetLeafKey(uiKeypos - 1)
                    bAdjustParent := true
                    lpLeafNode:usKeyCount--
                else
                    lpLeafNode:usKeyCount--
                    lpLeafNode:DeleteLeafKey(uiKeypos)
                endif
                self:DiskCache:GoHot(lpLeafNode)
            endif

            // Walk back up through index and adjust highest key in each node

            while (bAdjustParent) .AND. (self:IndexStack:Level > 1)
                bAdjustParent := false
                self:IndexStack:Pop()
                ulDiskPos := self:IndexStack:DiskOffset
                uiKeypos := self:IndexStack:KeyPos
                lpBranchNode := self:DiskCache:Get(ulDiskPos, false)
#ifdef DEBUG
                Debug.Assert(!lpBranchNode.bIsLeafNode)
                Debug.Assert(uiKeypos < lpBranchNode.usKeyCount)
#endif
                if bDeleteParentKey
                    bDeleteParentKey := false
                    if lpBranchNode:usKeyCount == 1
                        // Deleting only key in node...
                        self:area:DeadIndexBlocks:Push(ulDiskPos)
                        self:DiskCache:Kill(ulDiskPos)
                        bAdjustParent := true
                        bDeleteParentKey := true
                    else
                        if uiKeypos == (DWord)(lpBranchNode:usKeyCount - 1)
                            // Deleting highest key in node...
                            NewHighestKey := lpBranchNode:GetBranchKey(uiKeypos - 1)
                            bAdjustParent := true
                            lpBranchNode:usKeyCount--
                        else
                            lpBranchNode:usKeyCount--
                            lpBranchNode:DeleteBranchKey(uiKeypos)
                        endif
                        self:DiskCache:GoHot(lpBranchNode)
                    endif
                else
                    var key := BranchKey{}
                    key:Assign(NewHighestKey)
                    lpBranchNode:SetBranchKey(uiKeypos, key)
                    if uiKeypos == (DWord)(lpBranchNode:usKeyCount - 1)
                        NewHighestKey := lpBranchNode:GetBranchKey(uiKeypos)
                        bAdjustParent := true
                    endif
                    self:DiskCache:GoHot(lpBranchNode)
                endif
            enddo
            if self:IndexStack:Level == 1
                if bDeleteParentKey
                    // There is no index left
                    self:RootNodePos := 0
                else
                    bDone := false;

                    while !bDone
                        ulDiskPos := self:RootNodePos
                        lpRootNode := self:DiskCache:Get(ulDiskPos, false)

                        if (!lpRootNode:bIsLeafNode) .AND. (lpRootNode:usKeyCount == 1)
                            // Root node is a branch and it only has one key, so
                            // kill off the root node and make the node it points to
                            // the new root.

                            self:area:DeadIndexBlocks:Push(ulDiskPos)
                            self:DiskCache:Kill(ulDiskPos)
                            self:RootNodePos := lpRootNode:FirstBranchKey:ulDiskOffset
                        else
                            bDone := true
                        endif
                    end while
                endif
            endif
            self:DiskCache:Flush(Cache.CACHE_FLUSH_ALL)
            self:bFound := false
            self:CurrentKey:Clear()
        endif
        return bOk
    end method
    internal method Insert(item1 as DWord , item2 as DWord ) as Logic
        local lpThisNode as IndexNode
        local NewKey as IndexKey
        local bOk as Logic
        local ulDiskPos as DWord
        //
        lpThisNode := null
        NewKey := IndexKey{}
        bOk := true
        NewKey:item1 := item1
        NewKey:item2 := item2
        self:IndexStack:Reset()
        self:bFound := false
        ulDiskPos := self:RootNodePos
        if ulDiskPos == 0
            lpThisNode := self:DiskCache:New(ref ulDiskPos, false)
            if lpThisNode == null
                bOk := false
            else
                self:DiskCache:Unlock(lpThisNode, true)
                self:RootNodePos := ulDiskPos
                self:IndexStack:Push(ulDiskPos, 0u)
            endif
        else
            repeat
                if self:IndexStack:Find(ulDiskPos) // index is recursive
                    bOk := false
                    exit
                endif
                lpThisNode := self:DiskCache:Get(ulDiskPos, false)
                if lpThisNode == null
                    bOk := false
                    exit
                endif

                // search for insert point

                if lpThisNode:bIsLeafNode
#ifdef DEBUG
                    Debug.Assert(lpThisNode.usKeyCount <= IndexNode.KEYS_PER_LEAF)
#endif
                    local uiPos as DWORD
                    for uiPos := 0 to lpThisNode:usKeyCount - 1
                        if lpThisNode:GetLeafKey(uiPos) > NewKey
                            exit
                        endif
                    next
                    self:IndexStack:Push(ulDiskPos, uiPos)

                else  // branch node
#ifdef DEBUG
                    Debug.Assert(lpThisNode.usKeyCount <= IndexNode.KEYS_PER_BRANCH)
#endif

                    //
                    // There is no need to test last key in a branch
                    // node.  If we end up on the last key, that is
                    // the only possible path to take anyhow...
                    local uiPos as DWORD
                    for uiPos := 0 to lpThisNode:usKeyCount - 1
                        if lpThisNode:GetBranchKey(uiPos) >= NewKey
                            exit
                        endif
                    next

                    self:IndexStack:Push(ulDiskPos, uiPos)
                    ulDiskPos := lpThisNode:GetBranchKey(uiPos):ulDiskOffset
                endif
            until !(!lpThisNode:bIsLeafNode)
        endif
        if bOk
            self:InsertKey(lpThisNode, NewKey)
            self:DiskCache:Flush(Cache.CACHE_FLUSH_ALL)
        endif
        return bOk
    end method


    internal method Integrity() as Logic
        local result as Logic
        local RootNodePos as DWord
        //
        result := true
        RootNodePos := self:RootNodePos
        if RootNodePos != 0
            result := self:TestIndexNode(RootNodePos)
        endif
        return result
    end method

    internal method KillIndex() as void
        if self:DiskCache != null
            self:RootNodePos := 0
            self:IndexStack:Reset()
            self:DiskCache:Clear()
            self:CurrentKey:Clear()
            self:bFound := false
        endif
    end method

    internal method Seek(ulValue as DWord ) as Logic
        return self:IndexSeek(SeekTypes.ExactSeek_HalfKey, ulValue, 0u)
    end method


    internal method Seek(ulValue1 as DWord , ulValue2 as DWord ) as Logic
        return self:IndexSeek(SeekTypes.ExactSeek_FullKey, ulValue1, ulValue2)
    end method

    internal method SeekPrevious(ulValue as DWord ) as Logic
        return self:IndexSeek(SeekTypes.PrevSeek_HalfKey, ulValue, 0u)
    end method


    internal method SeekSoft(ulValue as DWord ) as Logic
        return self:IndexSeek(SeekTypes.SoftSeek_HalfKey, ulValue, 0u)
    end method

    internal abstract property RootNodePos as DWord GET SET
	internal abstract property CurrentLen as DWord GET
	internal abstract property CurrentPos as DWord GET


end class

END NAMESPACE // XSharp.RDD.FlexFile
