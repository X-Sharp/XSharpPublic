//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System
using System.Collections.Generic
using System.Diagnostics

begin namespace XSharp.RDD.SqlRDD

/// <summary>
/// Results of identical read statements of the RDD, reused for a short time
/// (see SqlDbConnection.ResultCacheMs).
/// </summary>
/// <remarks>
/// Code written for DBF/ADS repeats the same lookups over and over (the same seek into a tax or
/// parameter table for every line of a document): free with a local index, a round trip each with
/// a server. The entries of a table are dropped as soon as this connection writes to the table or
/// locks a record in it; changes of other users are seen at the latest when an entry expires.
/// </remarks>
internal class SqlDbResultCache
    private _entries as Dictionary<string, SqlDbResultCacheEntry>
    private _lock as object
    private const MaxEntries := 1000 as long
    /// <summary>Larger results (pages of a sequential read) are not kept: they are hardly repeated, and
    /// copying them cost more than it saved.</summary>
    internal const MaxRows := 20 as long

    /// <summary>Maximum age of an entry in milliseconds.</summary>
    property MaxAge as long auto

    constructor(nMaxAge as long)
        _entries := Dictionary<string, SqlDbResultCacheEntry>{StringComparer.Ordinal}
        _lock    := object{}
        MaxAge   := nMaxAge
        return

    method TryGet(cStatement as string, oEntry out SqlDbResultCacheEntry) as logic
        begin lock _lock
            if _entries:TryGetValue(cStatement, out oEntry)
                if (Stopwatch.GetTimestamp() - oEntry:Created) * 1000 / Stopwatch.Frequency <= MaxAge
                    return true
                endif
                _entries:Remove(cStatement)
            endif
        end lock
        oEntry := null
        return false

    method Put(cStatement as string, oEntry as SqlDbResultCacheEntry) as void
        oEntry:Created := Stopwatch.GetTimestamp()
        begin lock _lock
            if _entries:Count >= MaxEntries
                SELF:_RemoveExpired()
                if _entries:Count >= MaxEntries
                    _entries:Clear()
                endif
            endif
            _entries[cStatement] := oEntry
        end lock
        return

    private method _RemoveExpired() as void
        var nNow  := Stopwatch.GetTimestamp()
        var aKeys := List<string>{}
        foreach var oPair in _entries
            if (nNow - oPair:Value:Created) * 1000 / Stopwatch.Frequency > MaxAge
                aKeys:Add(oPair:Key)
            endif
        next
        foreach var cKey in aKeys
            _entries:Remove(cKey)
        next
        return

    /// <summary>Drop the entries of a table.</summary>
    method Invalidate(cTable as string) as void
        begin lock _lock
            if _entries:Count > 0
                var aKeys := List<string>{}
                foreach var oPair in _entries
                    if String.Equals(oPair:Value:Table, cTable, StringComparison.OrdinalIgnoreCase)
                        aKeys:Add(oPair:Key)
                    endif
                next
                foreach var cKey in aKeys
                    _entries:Remove(cKey)
                next
            endif
        end lock
        return

    method Clear() as void
        begin lock _lock
            _entries:Clear()
        end lock
        return
end class

/// <summary>The result of one statement: rows (object arrays) or a scalar.</summary>
internal class SqlDbResultCacheEntry
    property Table      as string auto
    property Created    as int64 auto
    property Rows       as List<object[]> auto
    property Columns    as long auto
    property Scalar     as object auto
    property IsScalar   as logic auto
end class

end namespace
