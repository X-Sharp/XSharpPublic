//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


USING System
USING System.Collections.Generic
USING System.Text

BEGIN NAMESPACE XSharp.RDD.SqlRDD

/// <summary>
/// The Handles class.
/// </summary>
STATIC CLASS SqlDbHandles
    PRIVATE STATIC RandomGenerator  AS Random
    PRIVATE STATIC Objects          AS Dictionary<IntPtr, SqlDbHandleObject>

    STATIC CONSTRUCTOR()
        var seed := (Int32) (DateTime.Now:Ticks % Int32.MaxValue)
        RandomGenerator := Random{ seed }
        Objects         := Dictionary<IntPtr, SqlDbHandleObject>{}

        RETURN
    STATIC METHOD GetId(maxValue as Int32) as IntPtr
        return IntPtr{RandomGenerator:Next()} % maxValue

    STATIC METHOD GetHandle(oObject as SqlDbHandleObject) as IntPtr
        local ok := TRUE as LOGIC
        local id as IntPtr
        BEGIN LOCK Objects
            REPEAT
                id := GetId(Int32.MaxValue)
                ok := ! Objects:ContainsKey(id)
                if ok .and. oObject != NULL
                    Objects:Add(id, oObject)
                endif
            UNTIL ok
        END LOCK
        return id

    STATIC Method Remove(oObject as SqlDbHandleObject) AS LOGIC
        BEGIN LOCK Objects
            if Objects:ContainsKey(oObject:Handle)
                if Objects[oObject:Handle] == oObject
                    Objects:Remove(oObject:Handle)
                    RETURN TRUE
                endif
            ENDIF
        END LOCK
        RETURN FALSE
    STATIC METHOD FindById(nId as IntPtr) AS SqlDbHandleObject
        IF Objects:ContainsKey(nId)
            return Objects[nId]
        endif
        return null

END CLASS
/// <summary>
/// The HandleObject class.
/// </summary>
CLASS SqlDbHandleObject INHERIT SqlDbObject
    PROPERTY Handle             AS IntPtr AUTO
    CONSTRUCTOR(cName as STRING)
        SUPER(cName)
        SELF:Handle := SqlDbHandles.GetHandle(SELF)
        RETURN
    DESTRUCTOR
        SqlDbHandles.Remove(SELF)
        RETURN
END CLASS
END NAMESPACE // XSharp.RDD.SqlRDD.SupportClasses
