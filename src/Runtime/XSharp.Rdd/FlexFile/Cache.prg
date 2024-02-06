//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


USING System
USING System.Collections.Generic
USING System.Text
using XSharp.RDD.Enums
using System.IO
using System.Diagnostics
BEGIN NAMESPACE XSharp.RDD.FlexFile
internal class CacheBlock
    internal Node as IndexNode
    internal ulFilepos as DWord
    internal uiLockCount as DWord
    internal uiAccessCount as DWord
    internal constructor(area as FlexArea)
        self:Node := IndexNode{area, 0}
end class

internal class Cache
    private lpBlocks as CacheBlock[]
    private m_area as FlexArea

    internal constructor(area as FlexArea )
        self:m_area := area
        self:Clear()
    property Stream as FileStream GET m_area:Stream
    destructor()
        self:lpBlocks := NULL

    private method FindBlock(ulFilepos as DWord ) as Long
        var result := NO_FIND
        for var i := 0 to CACHE_BLOCKS - 1
            if self:lpBlocks[i]:ulFilepos == ulFilepos
                result := i
                exit
            endif
        next
        return result


    private method FindBlock(lpNode as IndexNode ) as Long
        var result := NO_FIND
        for var i := 0 to CACHE_BLOCKS - 1
            if self:lpBlocks[i]:Node == lpNode
                result := i
                exit
            endif
        next
        return result


    private method LRUBlock() as Long
        //
        var count       := UInt32.MaxValue
        var iBlockNo    := NO_FIND
        for var i := 0 to CACHE_BLOCKS - 1
            if self:lpBlocks[i]:uiLockCount == 0
                if self:lpBlocks[i]:uiAccessCount == 0
                    iBlockNo := i
                    exit
                endif
                if self:lpBlocks[i]:uiAccessCount < count
                    count := self:lpBlocks[i]:uiAccessCount
                    iBlockNo := i
                endif
            endif
        next
        return iBlockNo


    private method IncrementAccessCount(iBlockNo as Long ) as void
        local uiLowestAccessCount as DWord
#ifdef DEBUG
            Debug.Assert(iBlockNo != NO_FIND)
            Debug.Assert(iBlockNo < CACHE_BLOCKS)
#endif

        if self:lpBlocks[iBlockNo]:uiAccessCount == UInt32.MaxValue
            uiLowestAccessCount := UInt32.MaxValue
            for var i := 0 to CACHE_BLOCKS - 1
                if (self:lpBlocks[i]:uiAccessCount != 0) .AND. (self:lpBlocks[i]:uiAccessCount < uiLowestAccessCount)
                    uiLowestAccessCount := self:lpBlocks[i]:uiAccessCount
                endif
            next

            for var i := 0 to CACHE_BLOCKS - 1
                if self:lpBlocks[i]:uiAccessCount != 0
                    self:lpBlocks[i]:uiAccessCount -= uiLowestAccessCount
                endif
            next
        else
            self:lpBlocks[iBlockNo]:uiAccessCount++
        endif




    internal method Get(ulFilepos as DWord , bLock as Logic ) as IndexNode
        local num as Long
        local IndexNode as IndexNode

#ifdef DEBUG
            Debug.Assert(ulFilepos % m_area.BlockSize == 0)
#endif
        //
        num := self:FindBlock(ulFilepos)
        if num == NO_FIND
            num := self:LRUBlock()
            self:Flush(num)
            self:m_area:Stream:Seek(ulFilepos, SeekOrigin.Begin)
            IndexNode := IndexNode{m_area, ulFilepos}
            if IndexNode:Read()
                IndexNode:bIsHot := false
                self:lpBlocks[num]:Node := IndexNode
                self:lpBlocks[num]:ulFilepos := ulFilepos
                self:lpBlocks[num]:uiLockCount := IIF(bLock , 1u , 0u)
                self:lpBlocks[num]:uiAccessCount := 1u
#ifdef DEBUG
//                     if (lpBlocks[iEntry].Node.bIsLeafNode)
//                     {
//                         if (lpBlocks[iEntry].Node.usKeyCount > IndexNode.KEYS_PER_LEAF)
//                         {
//                             FLEX_THROW("Corruption in data structure; use v_Recover()", "Cache::Get()");
//                         }
//                     }
//                     else
//                     {
//                         if (lpBlocks[iEntry].Node.usKeyCount > IndexNode.KEYS_PER_BRANCH)
//                         {
//                             FLEX_THROW("Corruption in data structure; use v_Recover()", "Cache::Get()");
//                         }
//                     }
                #endif
            else
                nop
                // Read Error
            endif
        else
            self:IncrementAccessCount(num)
            if bLock
                self:lpBlocks[num]:uiLockCount++
            endif
        endif
        var lpNode := self:lpBlocks[num]:Node
#ifdef DEBUG
        Debug.Assert(lpNode != null)
#endif
        return lpNode


    internal method Lock(lpNode as IndexNode ) as void
        var iEntry := self:FindBlock(lpNode)
        if iEntry != NO_FIND
            self:lpBlocks[iEntry]:uiLockCount++
            self:IncrementAccessCount(iEntry)
        endif


    internal method New(ulpDataPos ref DWord , lBranch as LOGIC) as IndexNode
        local ulFilePos as DWORD
        local uiBlockPad as INT64
        local ulFileEnd as INT64
        //
        var iEntry := self:LRUBlock()
        self:Flush(iEntry)
        if self:m_area:DeadIndexBlocks:Count > 0
            ulFilePos := self:m_area:DeadIndexBlocks:PopSmallest()
        else
            ulFilePos := (DWORD) self:m_area:RoundToBlockSize((WORD) self:m_area:Stream:Length)
            uiBlockPad := self:m_area:CalculateFillerSpace(IndexNode.INDEXNODE_SIZE)
            ulFileEnd := ulFilePos + IndexNode.INDEXNODE_SIZE + uiBlockPad
#ifdef DEBUG
                Debug.Assert(ulFileEnd % m_area.BlockSize == 0)
#endif
            try
                self:m_area:Stream:SetLength(ulFileEnd)
                self:m_area:SetNewFileLength(ulFileEnd)

#ifdef DEBUG
                Debug.Assert(m_area.Stream.Length == m_area:NextFree * m_area:BlockSize)
#endif
            catch e as Exception
                self:m_area:Error(e,EG_WRITE, EG_WRITE, __ENTITY__)
            end try
        endif
        var node := IndexNode{m_area, ulFilePos}
        node:bIsHot := true
        node:Init(lBranch)
        self:lpBlocks[iEntry]:Node := node
        self:lpBlocks[iEntry]:ulFilepos := ulFilePos
        self:lpBlocks[iEntry]:uiLockCount := 1u
        self:lpBlocks[iEntry]:uiAccessCount := 1u
        ulpDataPos := ulFilePos
        return self:lpBlocks[iEntry]:Node


    internal method Unlock(lpData as IndexNode , bGoHot as Logic ) as void
        var iEntry := self:FindBlock(lpData)
        if iEntry != NO_FIND
#ifdef DEBUG
            Debug.Assert(lpBlocks[iEntry].uiLockCount > 0)
#endif
            self:lpBlocks[iEntry]:uiLockCount--
            if bGoHot
                self:lpBlocks[iEntry]:Node:bIsHot := true
            endif
        endif


    internal method GoHot(lpNode as IndexNode ) as void
        var iEntry := self:FindBlock(lpNode)
        if iEntry != NO_FIND
            self:lpBlocks[iEntry]:Node:bIsHot := true
            self:IncrementAccessCount(iEntry)
        endif


    internal method Flush(iEntry as Long ) as void
        local iEnd as Long
        if iEntry == CACHE_FLUSH_ALL
            iEntry := 0
            iEnd := CACHE_BLOCKS - 1
        else
            iEnd := iEntry
        endif
        //Init
        for var i := iEntry to iEnd
            if self:lpBlocks[i]:ulFilepos != 0
#ifdef DEBUG
                Debug.Assert(lpBlocks[i].uiLockCount == 0)
#endif
                IF self:lpBlocks[i]:Node != null .AND. self:lpBlocks[i]:Node:bIsHot
                    self:m_area:Stream:Seek(self:lpBlocks[i]:ulFilepos, SeekOrigin.Begin)
                    if self:lpBlocks[i]:Node:Write()
                        self:lpBlocks[i]:Node:bIsHot := false
                    endif
                ENDIF

            endif
        next


    internal method Clear() as void
        self:lpBlocks := CacheBlock[]{CACHE_BLOCKS }
        for var i := 0 to CACHE_BLOCKS - 1
            self:lpBlocks[i] := CacheBlock{m_area}
        next


    internal method Kill(ulFilepos as DWord ) as void
        var iBlockNo := self:FindBlock(ulFilepos)
#ifdef DEBUG
            Debug.Assert(iBlockNo != NO_FIND)
#endif
        if iBlockNo != NO_FIND
            var block := self:lpBlocks[iBlockNo]
            block:Node:bIsHot := false
            block:ulFilepos := 0u
            block:uiLockCount := 0u
            block:uiAccessCount := 0u
        endif

#region Constants
    internal const CACHE_BLOCKS     := 3 as Long        // must be at least 3
    internal const CACHE_FLUSH_ALL  := -1 as Long
    internal const CACHE_GO_HOT     := 1 as Long        // for UnLock
    internal const CACHE_LOCK       := 1 as Long        // for Get
    internal const NO_FIND          := -1 as Long
    internal const UINT_MAX         := UInt32.MaxValue as DWord
#endregion

end class

END NAMESPACE // XSharp.RDD.FlexFile
