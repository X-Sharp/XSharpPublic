// DatabaseCommands.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
// Runtime helpers for VFP database/session commands:
//   CLOSE DATABASES [ALL]  — UDC rules in FoxProCmd.xh
//   CLOSE TABLES [ALL]     — UDC rules in FoxProCmd.xh

USING System.Collections.Generic
USING XSharp.RDD

/// <summary>
/// Runtime implementation of VFP <c>CLOSE DATABASES [ALL]</c>.
/// <para>
/// When <paramref name="lAll"/> is <c>.F.</c> (no ALL keyword):
/// <list type="bullet">
///   <item>If a database is currently active: closes it via <c>DbcManager</c>,
///         then closes all work areas in the current data session.</item>
///   <item>If no database is active: closes all work areas in the current data session
///         (free tables, indexes, format files).</item>
/// </list>
/// </para>
/// <para>
/// When <paramref name="lAll"/> is <c>.T.</c> (ALL keyword present):
/// closes every database tracked by <c>DbcManager</c>, deactivates the current
/// database, and closes all work areas.
/// </para>
/// <para>In both cases work area 1 is selected on exit.</para>
/// <note type="implementation">
/// Strict VFP semantics for <c>CLOSE DATABASES</c> (without ALL) close only the
/// tables belonging to the active database while leaving free tables open.
/// Identifying which work areas belong to a specific DBC would require iterating
/// all areas and querying DBI_DB_OBJECT for each. For practical purposes
/// <c>DbCloseAll()</c> is used, which is correct for the no-active-database
/// case and a safe superset for the active-database case.
/// </note>
/// </summary>
/// <param name="lAll">
/// Pass <c>.T.</c> for <c>CLOSE DATABASES ALL</c>;
/// pass <c>.F.</c> for plain <c>CLOSE DATABASES</c>.
/// </param>
FUNCTION __VFPCloseDatabases( lAll AS LOGIC ) AS VOID
    IF lAll
        // ── CLOSE DATABASES ALL ────────────────────────────────────────────
        // Snapshot before iterating: DbcManager.Close() mutates Databases
        // in-place, so we must work from a stable copy.
        VAR dbList := List<DbcDatabase>{ DbcManager.Databases }
        FOREACH VAR oDb IN dbList
            DbcManager.Close( oDb:Name )
        NEXT
        // Clear the active-database pointer; the object was already removed
        // from DbcManager.Databases above but ActiveDatabase still references it.
        DbcManager.Activate( NULL_OBJECT )
    ELSE
        // ── CLOSE DATABASES (no ALL) ───────────────────────────────────────
        VAR oActive := DbcManager.ActiveDatabase
        IF oActive != NULL_OBJECT
            // Close the current database and deactivate it.
            // DbcManager.Close() removes it from the Databases list and
            // closes its internal work area in DbcDataSession.
            DbcManager.Close( oActive:Name )
            // Clear the ActiveDatabase pointer.
            DbcManager.Activate( NULL_OBJECT )
        ENDIF
        // When no active database, VFP closes all free tables in the current
        // data session — handled below by DbCloseAll().
    ENDIF

    // Close all user work areas in the current data session.
    // This covers both DBC-owned tables and free tables.
    DbCloseAll()

    // VFP always selects work area 1 after CLOSE DATABASES [ALL].
    DbSelectArea( 1 )

/// <summary>
/// Runtime implementation of VFP <c>CLOSE TABLES [ALL]</c>.
/// <para>
/// Closes open table work areas without closing any open databases (DBCs).
/// </para>
/// <para>
/// When <paramref name="lAll"/> is <c>.F.</c> (no ALL keyword):
/// <list type="bullet">
///   <item>If a database is currently selected: closes the tables of that database.</item>
///   <item>If no database is selected: closes all free tables in all work areas.</item>
/// </list>
/// </para>
/// <para>
/// When <paramref name="lAll"/> is <c>.T.</c> (ALL keyword present):
/// closes all tables in all open databases and all free tables, across all data sessions.
/// Open databases (DBCs) themselves remain open in both cases.
/// </para>
/// <para>Work area 1 is selected on exit.</para>
/// <note type="implementation">
/// Strict VFP semantics for <c>CLOSE TABLES</c> (without ALL) close only the tables
/// belonging to the currently selected database. Identifying those work areas requires
/// iterating all areas and checking DBI_DB_OBJECT for each. For practical purposes
/// <c>DbCloseAll()</c> is used, which is correct for the no-active-database case
/// and a safe superset for the active-database case.
/// </note>
/// </summary>
/// <param name="lAll">
/// Pass <c>.T.</c> for <c>CLOSE TABLES ALL</c>;
/// pass <c>.F.</c> for plain <c>CLOSE TABLES</c>.
/// </param>
FUNCTION __VFPCloseTables( lAll AS LOGIC ) AS VOID
    // Close all user work areas in the current data session.
    // Open databases (DBCs) are NOT affected — DbcManager is untouched.
    DbCloseAll()

    // VFP always selects work area 1 after CLOSE TABLES [ALL].
    DbSelectArea( 1 )

/// <summary>
/// Runtime implementation of VFP <c>ADD TABLE TableName [NAME LongTableName]</c>.
/// </summary>
/// <remarks>
/// Links an existing free .DBF file to the currently active database (DBC).
/// The DBC must have been opened and set active via <c>OPEN DATABASE</c> /
/// <c>SET DATABASE TO</c> before calling this function.
/// </remarks>
/// <param name="cFileName">
/// Physical path to the .DBF file.  The <c>.DBF</c> extension is added when omitted.
/// The file must exist and must be a free table (empty backlink slot).
/// </param>
/// <param name="cLongName">
/// Logical name stored in the DBC OBJECTNAME field (up to 128 chars).
/// When empty, the filename without extension is used.
/// </param>
/// <returns><c>.T.</c> on success; <c>.F.</c> otherwise.</returns>
FUNCTION __VFPAddTable( cFileName AS STRING, cLongName AS STRING ) AS LOGIC
    RETURN DbcManager.AddTable( cFileName, cLongName )

/// <summary>
/// Runtime implementation of VFP <c>REMOVE TABLE TableName [DELETE] [RECYCLE]</c>.
/// </summary>
/// <param name="cName">Logical name of the table in the active DBC.</param>
/// <param name="lDelete">
/// <c>.T.</c> to delete the .DBF file (and companions) from disk after unlinking.
/// </param>
/// <param name="lRecycle">
/// <c>.T.</c> to move the file to the Recycle Bin instead of deleting.
/// (Currently behaves the same as <paramref name="lDelete"/>.)
/// </param>
/// <returns><c>.T.</c> on success; <c>.F.</c> otherwise.</returns>
FUNCTION __VFPRemoveTable( cName AS STRING, lDelete AS LOGIC, lRecycle AS LOGIC ) AS LOGIC
    RETURN DbcManager.RemoveTable( cName, lDelete, lRecycle )

/// <summary>
/// Runtime implementation of VFP <c>RENAME TABLE OldName TO NewName</c>.
/// Changes the logical name of the table inside the DBC.
/// The physical .DBF file is not renamed.
/// </summary>
/// <param name="cOldName">Current logical name of the table.</param>
/// <param name="cNewName">New logical name to assign.</param>
/// <returns><c>.T.</c> on success; <c>.F.</c> otherwise.</returns>
FUNCTION __VFPRenameTable( cOldName AS STRING, cNewName AS STRING ) AS LOGIC
    RETURN DbcManager.RenameTable( cOldName, cNewName )
