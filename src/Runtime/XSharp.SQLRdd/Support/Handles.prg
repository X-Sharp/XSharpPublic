//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


using System
using System.Collections.Generic
using System.Text

begin namespace XSharp.RDD.SqlRDD

/// <summary>
/// The Handles class.
/// </summary>
internal static class SqlDbHandles
    private static RandomGenerator  as Random
    private static Objects          as Dictionary<IntPtr, SqlDbHandleObject>

    static constructor()
        var seed := (Int32) (DateTime.Now:Ticks % Int32.MaxValue)
        RandomGenerator := Random{ seed }
        Objects         := Dictionary<IntPtr, SqlDbHandleObject>{}

        return

    static method GetId(MaxValue as Int32) as IntPtr
        return IntPtr{RandomGenerator:Next()} % MaxValue

    static method GetHandle(oObject as SqlDbHandleObject) as IntPtr
        local ok := true as logic
        local id as IntPtr
        begin lock Objects
            repeat
                id := GetId(Int32.MaxValue)
                ok := ! Objects:ContainsKey(id)
                if ok .and. oObject != null
                    Objects:Add(id, oObject)
                endif
            until ok
        end lock
        return id

    static method Remove(oObject as SqlDbHandleObject) as logic
        begin lock Objects
            if Objects:TryGetValue(oObject:Handle, out var result)
                if result == oObject
                    Objects:Remove(oObject:Handle)
                    return true
                endif
            endif
        end lock
        return false

    static method FindById(nId as IntPtr) as SqlDbHandleObject
        if Objects:TryGetValue(nId, out var result)
            return result
        endif
        return null

end class

end namespace // XSharp.RDD.SqlRDD.SupportClasses
