//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING System.Collections
USING System.Text
USING XSharp.RDD.Support
USING System.Diagnostics
BEGIN NAMESPACE XSharp.RDD

INTERNAL STRUCT ScopeInfo
    INTERNAL Buffer AS BYTE[]
    INTERNAL @@Value  AS OBJECT
    INTERNAL Size   AS LONG
    INTERNAL PROPERTY IsSet  AS LOGIC GET @@Value != NULL
    INTERNAL METHOD Clear() AS VOID
        @@Value := NULL
        Size  := 0
        RETURN
    INTERNAL METHOD SetBuffer(nSize AS LONG) AS VOID
        SELF:Buffer := BYTE[]{ nSize }
        RETURN
END STRUCT

INTERNAL ENUM SkipDirection
    MEMBER Backward := -1
    MEMBER Forward := 1

END ENUM

INTERNAL CLASS SortRecord
    PRIVATE _data AS BYTE[]
    PRIVATE _Recno AS DWORD
    INTERNAL PROPERTY Duplicate AS LOGIC AUTO

    INTERNAL PROPERTY Data AS BYTE[] GET _data

    INTERNAL PROPERTY Recno AS DWORD GET _Recno

    INTERNAL CONSTRUCTOR(data AS BYTE[] , lRecno AS DWORD )
        SELF:_data  := (BYTE[])data:Clone()
        SELF:_Recno := lRecno
        SELF:Duplicate := FALSE

END CLASS
INTERNAL ENUM SearchMode
    MEMBER Left
    MEMBER SoftSeek
    MEMBER Right
    MEMBER Bottom
    MEMBER Top

END ENUM

// Rdd Stack item
// Keep informations
[DebuggerDisplay("Page {Page}, Pos {Pos}, Count {Count}")];
INTERNAL CLASS RddStack
    INTERNAL Page   AS LONG
    INTERNAL Pos    AS WORD
    INTERNAL Count  AS WORD

    INTERNAL METHOD Clear() AS VOID
        SELF:Page := 0
        SELF:Count := 0
        SELF:Pos := 0

END CLASS

INTERNAL CLASS RddKeyData
    PROPERTY Recno   AS DWORD AUTO
    PROPERTY Key     AS BYTE[] AUTO
    PROPERTY ForCond AS LOGIC AUTO
    INTERNAL CONSTRUCTOR (nKeyLen AS LONG)
        SELF:ForCond := TRUE
        SELF:Recno   := MISSING_RECNO
        SELF:Key     := BYTE[]{nKeyLen}
        RETURN
#ifdef __DEBUG__
    METHOD ToAscii (bytes AS BYTE[], lHex := FALSE AS LOGIC) AS STRING
        VAR sb := System.Text.StringBuilder{}
        IF bytes == NULL
            RETURN ""
        ENDIF
        IF lHex
            FOREACH VAR b IN bytes
                //IF b > 0
                sb:Append( String.Format("{0:X2}",b))
                //ENDIF
            NEXT
            sb:Append(" ")
        ENDIF
        FOREACH VAR b IN bytes
            IF b > 31 .AND. b < 128
                sb:Append( (CHAR) b)
            ELSE
                sb:Append('.')
            ENDIF
        NEXT
        RETURN sb:ToString()
    OVERRIDE METHOD ToString() AS STRING
        RETURN SELF:ToAscii(SELF:Key, FALSE) + " "+Recno:ToString()
#endif
    INTERNAL METHOD CopyTo(oOther AS RddKeyData) AS VOID
        oOther:ForCond := SELF:ForCond
        oOther:Recno   := SELF:Recno
        IF oOther:Key:Length != SELF:Key:Length
            oOther:Key  := (BYTE[]) SELF:Key:Clone()
        ELSE
            System.Array.Copy(SELF:Key, oOther:Key, SELF:Key:Length)
        ENDIF
END CLASS

STRUCTURE LockTimer
    PRIVATE _start AS INT64
    PROPERTY Started AS LOGIC GET _start != 0
    METHOD Start() AS VOID
        _start := System.DateTime.Now.Ticks

    METHOD TimeOut(cFileName as STRING, nOffSet as INT64, nLen as INT64) AS LOGIC
        IF  System.DateTime.Now.Ticks > _start + 300_000_000
            THROW TimeoutException{i"Timeout locking file {cFileName} Offset {nOffSet} Len {nLen}"}
        ENDIF
        RETURN FALSE

END STRUCTURE


END NAMESPACE


INTERNAL FUNCTION JulianToDate(julian AS LONG) AS DbDate
    VAR jd := (REAL8) julian
    VAR z  := Math.Floor(jd + 0.5)
    VAR w  := Math.Floor( (z -1867216.25) / 36524.25)
    VAR x  := Math.Floor(w / 4)
    VAR aa := Math.Floor(z + 1 + w - x)
    VAR bb := Math.Floor(aa + 1524)
    VAR cc := Math.Floor((bb - 122.1) / 365.25)
    VAR dd := Math.Floor(365.25 * cc)
    VAR ee := Math.Floor((bb - dd) / 30.6001)
    VAR ff := Math.Floor(30.6001 * ee)
    VAR day := bb - dd - ff
    LOCAL month AS REAL8
    LOCAL year AS REAL8

    IF ((ee - 13) <= 12 .AND.  (ee - 13) > 0)
        month := ee - 13
    ELSE
        month := ee - 1
    ENDIF

    IF (month == 1 || month == 2)
        year := cc - 4715
    ELSE
        year := cc - 4716
    ENDIF
    RETURN DbDate { (INT)year, (INT) month, (INT) day}

