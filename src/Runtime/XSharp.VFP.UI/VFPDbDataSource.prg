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

CLASS VFPDbDataSource INHERIT DbDataSource

CONSTRUCTOR(oRDD AS IRdd)
    SUPER(oRDD)
    RETURN

// Hides base AddNew() (not virtual in DbDataSource) to reset _index after Append().
// Direct callers on this concrete type get the fix; interface-vtable callers (BindingSource)
// still hit the base — but Grid._StartNewRow() uses AppendRecord() instead, so this
// path is only reached if external code calls AddNew() directly.
METHOD AddNew() AS OBJECT STRICT
    LOCAL result := SUPER:AddNew() AS OBJECT
    SELF:_index := -1
    RETURN result

// AppendRecord: append to the RDD and reset _index.
// Grid._StartNewRow() calls this, then calls BindingSource.ResetBindings(false) to
// force DataGridView to re-sync its row count from RecCount (now N+1).
INTERNAL METHOD AppendRecord() AS LOGIC
    IF SELF:_oRDD:Append(TRUE)
        SELF:_index := -1
        RETURN TRUE
    ENDIF
    RETURN FALSE

// Factory: creates a VFPDbDataSource for the current work area.
STATIC METHOD CreateForCurrentArea() AS VFPDbDataSource
    LOCAL oResult := NULL AS OBJECT
    IF CoreDb.Info(DBI_RDD_OBJECT, REF oResult)
        RETURN VFPDbDataSource{(IRdd)oResult}
    ENDIF
    RETURN NULL

END CLASS

END NAMESPACE
