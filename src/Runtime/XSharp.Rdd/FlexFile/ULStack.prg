//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System.Collections.Generic


begin namespace XSharp.RDD.FlexFile
    // We are not using the normal stack because of the PopSmallest method
internal class ULStack
    private aItems as List<DWORD>

    internal property Count as Long get aItems:Count
    internal constructor()
        self:aItems := List<DWORD>{100}
    end constructor
    internal method Clear() as void
        self:aItems:Clear()
    end method

    internal method Push(ulValue as DWORD ) as void
        self:aItems:Add(ulValue)
    end method
    internal method Pop() as DWORD
        local index as Long
        local result as DWORD
        //
        index   := self:aItems:Count - 1
        result  := self:aItems[index]
        self:aItems:RemoveAt(index)
        return result
    end method

    internal method PopSmallest() as DWORD
        local uiLowestValue as DWORD
        local iLowestItem as Long
        local iItem as Long
        //
        uiLowestValue := self:aItems[0]
        iLowestItem := 0
        iItem := 0
        foreach uiValue as DWORD in SELF:aItems
            if uiValue < uiLowestValue
                uiLowestValue := uiValue
                iLowestItem := iItem
            endif
            iItem++
        next
        self:aItems:RemoveAt(iLowestItem)
        return uiLowestValue
    end method

end class

end namespace // XSharp.RDD.FlexFile
