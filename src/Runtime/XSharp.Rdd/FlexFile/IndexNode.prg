//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

// see Readme.txt for the structure of the index nodes
using System
using System.Collections.Generic
using System.Text
using System.IO
using System.Diagnostics
using XSharp.RDD.Enums
begin namespace XSharp.RDD.FlexFile

internal class IndexNode
    internal m_bytes as Byte[]
    internal bIsLeafNode as Logic
    internal bIsHot as Logic
    internal usKeyCount as Word
    internal m_area as FlexArea
    internal m_offset as DWORD

    property FilePos as DWORD Get m_offset SET m_offset := value

    internal constructor(area as FlexArea, nFilePos as DWORD)
        self:m_bytes := Byte[]{ INDEXNODE_DATASIZE }
        self:bIsHot := false
        self:bIsLeafNode := false
        self:usKeyCount := 0
        self:m_area := area
        SELF:m_offset := nFilePos
    end constructor
    private method LeafKeyOffSet(iKey as Dword ) as Long
        return (Long) KEYS_OFFSET +  iKey * LEAFKEY_SIZE
    end method

    private method BranchKeyOffSet(iKey as Dword ) as Long
        return (Long) KEYS_OFFSET + iKey * BRANCHKEY_SIZE
    end method


    internal method DeleteLeafKey(iKey as Dword ) as void
        var iOffSet := self:LeafKeyOffSet(iKey)
        Array.Copy(self:m_bytes, iOffSet + LEAFKEY_SIZE, self:m_bytes, iOffSet, INDEXNODE_DATASIZE - iOffSet - 8)
        self:bIsHot := true
    end method


    internal method GetLeafKey(iKey as Dword ) as IndexKey
        var oKey := IndexKey{}
        var iOffSet := self:LeafKeyOffSet(iKey)
        oKey:item1 := BitConverter.ToUInt32(self:m_bytes, iOffSet)
        oKey:item2 := BitConverter.ToUInt32(self:m_bytes, iOffSet + 4)
        self:bIsHot := true
        return oKey
    end method

    internal property FirstLeafKey as IndexKey GET self:GetLeafKey(0)
    internal property LastLeafKey  as IndexKey GET self:GetLeafKey(self:usKeyCount - 1)

    internal method SetLeafKey(iKey as Dword , oKey as IndexKey ) as void
        var iOffSet := self:LeafKeyOffSet(iKey)
        Array.Copy(BitConverter.GetBytes(oKey:item1), 0, self:m_bytes, iOffSet, 4)
        Array.Copy(BitConverter.GetBytes(oKey:item2), 0, self:m_bytes, iOffSet + 4, 4)
        self:bIsHot := true
    end method

    internal property LastBranchKey as BranchKey GET self:GetBranchKey(self:usKeyCount - 1)
    internal property FirstBranchKey as BranchKey GET self:GetBranchKey(0)

    internal method GetBranchKey(iKey as Dword ) as BranchKey
        var oKey := BranchKey{}
        var iOffSet         := self:BranchKeyOffSet(iKey)
        oKey:item1          := BitConverter.ToUInt32(self:m_bytes, iOffSet)
        oKey:item2          := BitConverter.ToUInt32(self:m_bytes, iOffSet + 4)
        oKey:ulDiskOffset   := BitConverter.ToUInt32(self:m_bytes, iOffSet + 8)
        return oKey
    end method

    internal method DeleteBranchKey(iKey as Dword ) as void
        var iOffSet := self:BranchKeyOffSet(iKey)
        Array.Copy(self:m_bytes, iOffSet + BRANCHKEY_SIZE, self:m_bytes, iOffSet, INDEXNODE_DATASIZE - iOffSet - BRANCHKEY_SIZE)
        self:bIsHot := true
    end method



    internal method SetBranchKey(iKey as Dword , oKey as BranchKey ) as void
        var iOffSet := (long) self:BranchKeyOffSet(iKey)
        Array.Copy(BitConverter.GetBytes(oKey:item1), 0, self:m_bytes, iOffSet, 4)
        Array.Copy(BitConverter.GetBytes(oKey:item2), 0, self:m_bytes, iOffSet + 4, 4)
        Array.Copy(BitConverter.GetBytes(oKey:ulDiskOffset), 0, self:m_bytes, iOffSet + 8, 4)
        self:bIsHot := true
    end method


    internal method Read() as Logic
        var result := true
        try
            m_area:Stream:SafeSetPos(m_offset)
            var token := FlexMemoToken{m_bytes, m_area:Stream}
            result := token:Read()
            if result
                result := m_area:Stream:Read(self:m_bytes, 0, INDEXNODE_DATASIZE) == INDEXNODE_DATASIZE
            endif
            var uiNodeInfo      := BitConverter.ToUInt16(self:m_bytes, 0)
            self:bIsLeafNode    := uiNodeInfo % 2 == 1
            self:bIsHot         := uiNodeInfo % 4 > 1
            self:usKeyCount     := (Word)(uiNodeInfo >> 2)
            return result

        catch as IOException
            return false
        end try
    end method


    internal method Write() as Logic
        try
            var tokenBytes := byte[]{ FlexMemoToken.TokenLength }
            var token := FlexMemoToken{tokenBytes, m_area:Stream}
            token:DataType := FlexFieldType.IndexBlock
            token:Length   := INDEXNODE_DATASIZE
            m_area:Stream:SafeSetPos(m_offset)
            if ! token:Write(FlexMemoToken.TokenLength)
                m_area:Error(FException(), Subcodes.ERDD_WRITE, Gencode.EG_WRITE, "IndexNode.Write")
            endif
            var uiNodeInfo := (Word)(self:usKeyCount << 2)
            if self:bIsLeafNode
                uiNodeInfo += 1
            endif
            if self:bIsHot
                uiNodeInfo +=2
            endif
            Array.Copy(BitConverter.GetBytes(uiNodeInfo), 0, self:m_bytes, 0, 2)
            if ! m_area:Stream:SafeWrite(self:m_bytes, INDEXNODE_DATASIZE)
                m_area:Error(FException(), Subcodes.ERDD_WRITE, Gencode.EG_WRITE, "IndexNode.Write")
            endif
            m_area:Stream:Flush()
            return true

        catch as IOException
            return false
        end try
    end method


    internal method Init(lBranch as LOGIC) as void
        local i as Long
        var token := FlexMemoToken{m_bytes, m_area:Stream}
        for i := KEYS_OFFSET to INDEXNODE_DATASIZE -1
            self:m_bytes[i] := INDEX_NODE_FILLER
        next
        token:DataType := FlexFieldType.IndexBlock
        token:Length   := INDEXNODE_DATASIZE
        m_area:Stream:SafeSetPos(self:m_offset)
        if ! token:Write()
            m_area:Error(FException(), Subcodes.ERDD_WRITE, Gencode.EG_WRITE, "IndexNode.Init")
        endif
        self:usKeyCount   := 0
        self:bIsLeafNode  := ! lBranch
        self:bIsHot       := false
     end method



    internal method InsertKey(uiInsertAt as Dword, NewKey as IndexKey ) as void
#ifdef DEBUG
            Debug.Assert(bIsLeafNode)
            Debug.Assert(usKeyCount < KEYS_PER_LEAF)
            Debug.Assert(uiInsertAt < KEYS_PER_LEAF)
#endif
        if self:usKeyCount > 0 .AND. uiInsertAt < self:usKeyCount
            var offSet1 := (int) self:LeafKeyOffSet( uiInsertAt)
            var offSet2 := offSet1 + LEAFKEY_SIZE
            Array.Copy(self:m_bytes, offSet1, self:m_bytes, offSet2, INDEXNODE_DATASIZE - offSet2)
        endif
        self:SetLeafKey( uiInsertAt, NewKey)
        self:usKeyCount++
        self:bIsHot := true
    end method


    internal method InsertKey(uiInsertAt as Dword , NewKey as BranchKey ) as void
#ifdef DEBUG
            Debug.Assert(!bIsLeafNode)
            Debug.Assert(usKeyCount < KEYS_PER_BRANCH)
            Debug.Assert(uiInsertAt < KEYS_PER_BRANCH)
#endif
        if self:usKeyCount > 0 .AND. uiInsertAt < self:usKeyCount
            var offSet1 := (long) self:BranchKeyOffSet(uiInsertAt)
            var offSet2 := offSet1 + BRANCHKEY_SIZE
            Array.Copy(self:m_bytes, offSet1, self:m_bytes, offSet2, INDEXNODE_DATASIZE - offSet2)
        endif
        self:SetBranchKey(uiInsertAt, NewKey)
        self:usKeyCount++
        self:bIsHot := true
    end method


    internal method Split(lpNewNode as IndexNode ) as void
        if self:bIsLeafNode
            var iOffSet := self:LeafKeyOffSet(KEYS_PER_LEAF)
            Array.Copy(self:m_bytes, iOffSet, lpNewNode:m_bytes, 0, (KEYS_PER_LEAF / 2) * LEAFKEY_SIZE)
            self:usKeyCount := lpNewNode:usKeyCount := KEYS_PER_LEAF /2
            for var i := iOffSet to INDEXNODE_DATASIZE -1
                self:m_bytes[i] := INDEX_NODE_FILLER
            next
        else
            var iOffSet := self:BranchKeyOffSet(KEYS_PER_BRANCH / 2)
            Array.Copy(self:m_bytes, iOffSet, lpNewNode:m_bytes, 0, (KEYS_PER_BRANCH / 2) * BRANCHKEY_SIZE)
            self:usKeyCount := lpNewNode:usKeyCount := KEYS_PER_BRANCH / 2
            //Init
            for var i := iOffSet to INDEXNODE_DATASIZE -1
                self:m_bytes[i] := INDEX_NODE_FILLER
            next
        endif
        lpNewNode:bIsLeafNode := self:bIsLeafNode
        lpNewNode:bIsHot := true
        self:bIsHot := true
    end method

#region Constants
    public const INDEX_NODE_FILLER := 0xAD as Byte // '╜'  // new index nodes
    public const INDEXNODE_SIZE := 1018 as Long
    public const INDEXNODE_DATASIZE := 1010 as Long
    public const KEYS_PER_LEAF := 126 as Long   // Note: Must be an even number
    public const KEYS_PER_BRANCH := 84 as Long  // Note: Must be 2/3rds of KEYS_PER_LEAF
    public const NODEINFO_OFFSET := 0 as Long
    public const KEYS_OFFSET := 2 as Long
    public const LEAFKEY_SIZE := 8 as Long
    public const BRANCHKEY_SIZE := 12 as Long
#endregion

end class

end namespace // XSharp.RDD.FlexFile
