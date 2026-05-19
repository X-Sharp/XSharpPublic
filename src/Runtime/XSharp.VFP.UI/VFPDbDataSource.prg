// VFPDbDataSource.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
// Subclass of XSharp.DbDataSource, owned by XSharp.VFP.UI so that fixes can be
// applied and tested independently of the main XSharp runtime.
//
// Changes vs the original DbDataSource:
//  1. AddNew(): hides base method to reset _index := -1 after Append() so the next
//     indexer access uses an absolute GoTop+Skip instead of a stale relative skip
//     that would overshoot EOF.
//  2. AppendRecord(): new method used by Grid._StartNewRow(). Appends a record and
//     resets _index. Grid calls this then calls BindingSource.ResetBindings(false) to
//     make DataGridView rebuild its row collection (ListChanged(Reset) bypasses the
//     addNewIsBeingAdded flag that BindingSource.AddNew() would set, which causes
//     DataGridView to skip the row insertion).

USING XSharp.RDD
USING XSharp.RDD.Support

BEGIN NAMESPACE XSharp.VFP.UI

/// <summary>
/// <see cref="DbDataSource"/> subclass that fixes two bugs for VFP grid support.<br/>
/// 1. <see cref="AddNew"/> resets <c>_index</c> to -1 after <c>Append()</c> so the next indexer read
///    uses <c>GoTop+Skip</c> rather than a stale relative skip that overshoots EOF.<br/>
/// 2. <see cref="AppendRecord"/> is a new method used by <c>Grid._StartNewRow()</c>: appends a record
///    and resets <c>_index</c> so the caller can follow with <c>BindingSource.ResetBindings(false)</c>
///    to force <c>DataGridView</c> to rebuild its row collection without triggering the
///    <c>addNewIsBeingAdded</c> flag that <c>BindingSource.AddNew()</c> would set.
/// </summary>
CLASS VFPDbDataSource INHERIT DbDataSource

CONSTRUCTOR(oRDD AS IRdd)
    SUPER(oRDD)
    RETURN

/// <summary>Hides the base <c>AddNew()</c> to reset <c>_index</c> to -1 after <c>Append()</c>, preventing a stale relative skip on the next indexer access. Direct callers on this concrete type get the fix; <c>BindingSource</c> interface-vtable callers still hit the base.</summary>
METHOD AddNew() AS OBJECT STRICT
    LOCAL result := SUPER:AddNew() AS OBJECT
    SELF:_index := -1
    RETURN result

/// <summary>Appends a blank record to the RDD and resets <c>_index</c>. Called by <c>Grid._StartNewRow()</c>, which then calls <c>BindingSource.ResetBindings(false)</c> to re-sync the <c>DataGridView</c> row count.</summary>
INTERNAL METHOD AppendRecord() AS LOGIC
    IF SELF:_oRDD:Append(TRUE)
        SELF:_index := -1
        RETURN TRUE
    ENDIF
    RETURN FALSE

/// <summary>Factory method: retrieves the <see cref="XSharp.RDD.IRdd"/> object for the current work area and wraps it in a <see cref="VFPDbDataSource"/>. Returns <c>NULL</c> if no work area is open.</summary>
STATIC METHOD CreateForCurrentArea() AS VFPDbDataSource
    LOCAL oResult := NULL AS OBJECT
    IF CoreDb.Info(DBI_RDD_OBJECT, REF oResult)
        RETURN VFPDbDataSource{(IRdd)oResult}
    ENDIF
    RETURN NULL

END CLASS

END NAMESPACE
