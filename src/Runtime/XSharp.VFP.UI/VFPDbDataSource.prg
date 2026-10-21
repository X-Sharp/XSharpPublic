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

/// <summary>
/// Hides the base <c>AddNew()</c> to reset <c>_index</c> to -1 after <c>Append()</c>, preventing a stale relative skip on the next indexer access. Direct callers on this concrete type get the fix; <c>BindingSource</c> interface-vtable callers still hit the base.
/// </summary>
METHOD AddNew() AS OBJECT STRICT
    LOCAL result := SUPER:AddNew() AS OBJECT
    SELF:_index := -1
    RETURN result

/// <summary>
/// Appends a blank record to the RDD and resets <c>_index</c>. Called by <c>Grid._StartNewRow()</c>, which then calls <c>BindingSource.ResetBindings(false)</c> to re-sync the <c>DataGridView</c> row count.
/// </summary>
INTERNAL METHOD AppendRecord() AS LOGIC
    IF SELF:_oRDD:Append(TRUE)
        SELF:_index := -1
        RETURN TRUE
    ENDIF
    RETURN FALSE

/// <summary>Returns the current RDD record number — used by <c>Grid.Refresh()</c> to save position before repaint.</summary>
INTERNAL METHOD SavePosition() AS DWORD
    RETURN SELF:_oRDD:RecNo

/// <summary>Current RDD BOF flag — saved by <c>Grid.Refresh()</c> alongside <see cref="SavePosition"/>.</summary>
INTERNAL PROPERTY BoF AS LOGIC GET SELF:_oRDD:BoF

/// <summary>
/// Restores the RDD to a previously saved position, including EOF/BOF state.<br/>
/// <list type="bullet">
/// <item>EOF: <c>GoBottom()</c> + <c>Skip(1)</c> — lands past the last record with <c>EoF = .T.</c></item>
/// <item>BOF: <c>GoTop()</c> + <c>Skip(-1)</c> — lands before the first record with <c>BoF = .T.</c></item>
/// <item>Normal: <c>GoTo(nRecno)</c> — positions on that physical record.</item>
/// </list>
/// A plain <c>GoTo</c> always clears both flags, so EOF/BOF must be replayed explicitly.
/// </summary>
INTERNAL METHOD RestorePosition(nRecno AS DWORD, lEof AS LOGIC, lBof AS LOGIC) AS VOID
    IF lEof
        SELF:_oRDD:GoBottom()
        SELF:_oRDD:Skip(1)
        // Clear the row cache: a stale entry for the last row would cause the next
        // indexer access ([i]) to call GoTo(lastRecNo), silently clearing EOF.
        SELF:_records:Clear()
    ELSEIF lBof
        SELF:_oRDD:GoTop()
        SELF:_oRDD:Skip(-1)
        SELF:_records:Clear()
    ELSE
        SELF:_oRDD:GoTo(nRecno)
    ENDIF
    SELF:_index := -1

/// <summary>Restores the RDD to a previously saved record number and resets the internal index so the next access uses absolute navigation.</summary>
INTERNAL METHOD RestorePosition(nRecno AS DWORD) AS VOID
    SELF:RestorePosition(nRecno, FALSE, FALSE)

/// <summary>
/// Factory method: retrieves the <see cref="XSharp.RDD.IRdd"/> object for the current work area and wraps it in a <see cref="VFPDbDataSource"/>. Returns <c>NULL</c> if no work area is open.
/// </summary>
STATIC METHOD CreateForCurrentArea() AS VFPDbDataSource
    LOCAL oResult := NULL AS OBJECT
    IF CoreDb.Info(DBI_RDD_OBJECT, REF oResult)
        RETURN VFPDbDataSource{(IRdd)oResult}
    ENDIF
    RETURN NULL

END CLASS

END NAMESPACE
