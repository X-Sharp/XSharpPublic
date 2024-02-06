//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
begin namespace XSharp.RDD.FlexFile
internal class IndexStack

    private structure IndexLevel
        internal ulDiskOffset   as DWord
        internal uiKeyPos       as DWord
    end structure

    private const MAX_INDEX_LEVELS := 10 as Long

    private Items as IndexLevel[]

    private iLevel as DWord

    internal property DiskOffset as DWord GET self:Items[self:iLevel - 1]:ulDiskOffset

    internal property Level as DWord GET self:iLevel

    internal property KeyPos as DWord GET self:Items[self:iLevel - 1]:uiKeyPos SET self:Items[self:iLevel - 1]:uiKeyPos := value

    internal constructor()
        self:Reset()
    end constructor

    internal method Reset() as void
        self:Items := IndexLevel[]{ MAX_INDEX_LEVELS }
        for var i := 0 to MAX_INDEX_LEVELS-1
            self:Items[i] := IndexLevel{}
        next
        self:iLevel := 0u
    end method


    internal method Pop() as void
        self:iLevel--
    end method

    internal method Push(ulDiskOffset as DWord , uiKeyPos as DWORD ) as void
        self:Items[self:iLevel]:ulDiskOffset := ulDiskOffset
        self:Items[self:iLevel]:uiKeyPos     := uiKeyPos
        self:iLevel++
    end method

    internal method Find(ulValue as DWord ) as Logic
        IF iLevel > 0
            for var i := 0 to self:iLevel-1
                if self:Items[i]:ulDiskOffset == ulValue
                    return true
                endif
            next
        endif
        return false
    end method


end class
end namespace
