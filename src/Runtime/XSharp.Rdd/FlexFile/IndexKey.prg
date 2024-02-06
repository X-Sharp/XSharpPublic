//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System
using System.Collections.Generic
using System.Text

begin namespace XSharp.RDD.FlexFile

internal class IndexKey
    internal item1 as DWord
    internal item2 as DWord

    internal constructor( )
        item1 := 0
        item2 := 0
        RETURN

    public override method Equals(k2 as Object ) as Logic
        if k2 == null
            return false
        endif
        if (k2 is IndexKey var ik2)
            if self:item1 == ik2:item1
                return self:item2 == ik2:item2
            endif
        endif
        return false

    public override method GetHashCode() as Long
        return (Long)(self:item1 ^ self:item2)

    operator ==(Key1 as IndexKey , Key2 as IndexKey ) as Logic
        if Key1:item1 == Key2:item1
            return Key1:item2 == Key2:item2
        endif
        return false
    end operator

    operator !=(Key1 as IndexKey , Key2 as IndexKey ) as Logic
        if Key1:item1 == Key2:item1
            return Key1:item2 != Key2:item2
        endif
        return true
    end operator

    operator <(Key1 as IndexKey , Key2 as IndexKey ) as Logic
        return Key1:item1 <= Key2:item1
    end operator

    operator >(Key1 as IndexKey , Key2 as IndexKey ) as Logic
        if Key1:item1 <= Key2:item1
            if Key1:item1 == Key2:item1
                return Key1:item2 > Key2:item2
            endif
            return false
        endif
        return true
    end operator

    operator >=(Key1 as IndexKey , Key2 as IndexKey ) as Logic
        if Key1:item1 <= Key2:item1
            if Key1:item1 == Key2:item1
                return Key1:item2 >= Key2:item2
            endif
            return false
        endif
        return true

    end operator

    operator <=(Key1 as IndexKey , Key2 as IndexKey ) as Logic
        if Key1:item1 >= Key2:item1
            if Key1:item1 == Key2:item1
                return Key1:item2 <= Key2:item2
            endif
            return false
        endif
        return true
    end operator

    internal method Clear() as void
        self:item1 := 0
        self:item2 := 0
    end method

    internal property IsEmpty as Logic => self:item1 == 0 .and. self:item2 == 0

end class

internal class BranchKey inherit IndexKey
    internal ulDiskOffset as DWord

    internal constructor( )
        super()
        ulDiskOffset := 0
    end constructor

    internal method Assign(Key2 as IndexKey ) as void
        item1 := Key2:item1
        item2 := Key2:item2
        return
    end method

    internal method Assign(Key2 as BranchKey ) as void
        item1 := Key2:item1
        item2 := Key2:item2
        self:ulDiskOffset := Key2:ulDiskOffset
        return
    end method
end class

end namespace // XSharp.RDD.FlexFile
