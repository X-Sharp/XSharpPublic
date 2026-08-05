// Cursor.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.

USING System
USING System.Collections.Generic
USING System.Text
USING System.Windows.Forms
USING System.ComponentModel
USING XSharp.RDD

BEGIN NAMESPACE XSharp.VFP.UI
	/// <summary>
	/// VFP-compatible <c>Cursor</c> object that describes a table or view opened by the <c>DataEnvironment</c>.<br/>
	/// Supports both free tables (<see cref="IsFreeTable"/> — <see cref="CursorSource"/> is a .dbf path) and
	/// DBC-bound tables (<see cref="DataBase"/> is set).<br/>
	/// <see cref="Open"/> opens the work area via <c>DbUseArea</c>, attaches a <see cref="DbDataSource"/>, and
	/// applies <see cref="Order"/>, <see cref="Filter"/>, and <see cref="NoDataOnLoad"/> (sets a <c>1=0</c> filter
	/// so the table opens empty until <c>REQUERY()</c> is called).<br/>
	/// <see cref="Sync"/> synchronises the <see cref="BindingSource"/> position to the current RDD record number.
	/// </summary>
	PARTIAL CLASS DbCursor //IMPLEMENTS XSharp.IDbNotify

		// Common properties that all VFP Objects support
		#include "Headers/VFPObject.xh"
		/// <summary>
		/// The Name of the Working Area
		/// </summary>
		PROPERTY Alias AS STRING AUTO

		/// <summary>
		/// For DBC, the Table Name
		/// For free tables, specifies the full path to the free table.
		/// </summary>
		PROPERTY CursorSource AS STRING AUTO


		/// <summary>
		/// Specifies whether a table associated with a Cursor object is opened exclusively.
		/// </summary>
		PROPERTY Exclusive AS LOGIC AUTO


		/// <summary>
		/// Excludes records that do not meet the criteria in the specified expression
		/// </summary>
		/// <value></value>
		PROPERTY Filter AS STRING AUTO

		/// <summary>
		/// The Name of the Cursor
		/// </summary>
		PROPERTY Name AS STRING AUTO


		/// <summary>
		/// Causes the view associated with a Cursor to activate without downloading data.
		/// </summary>
		PROPERTY NoDataOnLoad AS LOGIC AUTO

		/// <summary>
		/// Specifies an ascending or descending order for the controlling index tag specified by the Order property of a Cursor object.
		/// </summary>
		PROPERTY OrderDirection  AS LONG AUTO

		/// <summary>
		/// The Name of the Order
		/// </summary>
		PROPERTY Order AS STRING AUTO


		/// <summary>
		/// Specifies whether a table or view associated with a Cursor object allows updates.
		/// </summary>
		PROPERTY ReadOnly  AS LOGIC AUTO

		/// <summary>
		/// The DataSource stored in the BindingSource of this Cursor
		/// </summary>
		PROPERTY DataSource AS OBJECT
		GET
			IF SELF:BindingSource != NULL
				IF SELF:BindingSource:DataSource == NULL
					SELF:Open()
				ENDIF
				RETURN SELF:BindingSource:DataSource
			ENDIF
			RETURN NULL
		END GET
		SET
			IF SELF:BindingSource != NULL
				SELF:BindingSource:DataSource := VALUE
			ENDIF
		END SET
		END PROPERTY

		/// <summary>
		/// The path to the database (DBC) that contains the table or view associated with a cursor.
		/// If the cursor is based on a free table, Database contains an empty string ("").
		/// </summary>
		/// <value></value>
		PROPERTY DataBase AS STRING AUTO

		/// <summary>
		/// <c>.T.</c> when <see cref="DataBase"/> is empty, meaning the cursor points to a free table rather than a DBC-bound table.
		/// </summary>
		PROPERTY IsFreeTable AS LOGIC GET String.IsNullOrEmpty( SELF:DataBase )

		/// <summary>
		/// The BindingSource stored in this Cursor
		/// </summary>
		PROPERTY BindingSource AS BindingSource AUTO

		/// <summary>
		/// VFP BufferModeOverride stub — stored for source compatibility. Not implemented.
		/// </summary>
		PROPERTY BufferModeOverride AS INT AUTO

		/// <summary>
		/// The underlying RDD object obtained from <c>CoreDb.Info(DBI_RDD_OBJECT)</c> after <see cref="Open"/>. Used by <see cref="Sync"/>.
		/// </summary>
		PROTECTED PROPERTY _oRDD AS IRdd AUTO

		CONSTRUCTOR(  )
			SELF:BindingSource := BindingSource{}
			RETURN

		/// <summary>
		/// Opens the table or view in a new work area, attaches a <see cref="DbDataSource"/> to <see cref="BindingSource"/>, and applies <see cref="Order"/>, <see cref="Filter"/>, and <see cref="NoDataOnLoad"/>.
		/// </summary>
		METHOD Open() AS VOID
			// Free Table
			IF SELF:IsFreeTable
				IF String.IsNullOrEmpty( SELF:Alias )
					SELF:Alias := System.IO.Path.GetFileNameWithoutExtension( SELF:CursorSource )
				ENDIF
				// Open the Table
				DbUseArea(TRUE, "DBFVFP", SELF:CursorSource, SELF:Alias, !SELF:Exclusive, SELF:ReadOnly )
				// And Attached the corresponding DataSource
				LOCAL oResult := NULL AS OBJECT
				IF CoreDb.Info(DBI_RDD_OBJECT,REF oResult)
					SELF:_oRDD := (IRdd) oResult
					SELF:BindingSource:DataSource := DbDataSource{SELF:_oRDD}
					// Do we have an Auto Refresh of the BindingSource if the underlying DataBase is moving/changing ??
					//CoreDb.Notify += SELF:Notify
				ENDIF
				// Apply index order if specified
				IF !String.IsNullOrEmpty( SELF:Order )
					OrdSetFocus( SELF:Order )
				ENDIF
				// NoDataOnLoad: open empty — a later REQUERY() call loads the real data
				IF SELF:NoDataOnLoad
					DbSetFilter( , "1 = 0" )
				ELSEIF !String.IsNullOrEmpty( SELF:Filter )
					DbSetFilter( , SELF:Filter )
				ENDIF
			ELSE
				// DBC table
				IF String.IsNullOrEmpty( SELF:Alias )
					SELF:Alias := SELF:CursorSource
				ENDIF
				// Resolve to absolute path so DbcManager.FindDatabase() can match the key
				// stored by ExtendDbName() when the DBC was first opened.
				VAR dbAbsPath := System.IO.Path.GetFullPath( SELF:DataBase )
				VAR dbAbsDir  := System.IO.Path.GetDirectoryName( dbAbsPath )
				// Open the DBC — silently ignored if already open
				DbcManager.Open( dbAbsPath, TRUE, SELF:ReadOnly, FALSE )
				// Resolve full .dbf path via the DBC metadata, fall back to DBC directory
				LOCAL cFullPath AS STRING
				LOCAL oDb AS DbcDatabase
				oDb := DbcManager.FindDatabase( dbAbsPath )
				IF oDb != NULL
					LOCAL oTable AS DbcTable
					oTable := oDb:FindTable( SELF:CursorSource )
					IF oTable != NULL .AND. !String.IsNullOrEmpty( oTable:Path )
						// oTable:Path holds the real filename from the DBC PROPERTY blob —
						// it may differ from CursorSource (e.g. spaces vs underscores).
						// Use it directly if absolute and valid; otherwise resolve its
						// filename against the DBC directory.
						IF System.IO.Path.IsPathRooted( oTable:Path ) .AND. System.IO.File.Exists( oTable:Path )
							cFullPath := oTable:Path
						ELSE
							cFullPath := System.IO.Path.Combine( dbAbsDir, System.IO.Path.GetFileName( oTable:Path ) )
						ENDIF
					ELSE
						cFullPath := System.IO.Path.Combine( dbAbsDir, SELF:CursorSource + ".dbf" )
					ENDIF
				ELSE
					cFullPath := System.IO.Path.Combine( dbAbsDir, SELF:CursorSource + ".dbf" )
				ENDIF
				// Open the table in its own work area
				DbUseArea( TRUE, "DBFVFP", cFullPath, SELF:Alias, !SELF:Exclusive, SELF:ReadOnly )
				// Attach the DataSource
				LOCAL oResult := NULL AS OBJECT
				IF CoreDb.Info( DBI_RDD_OBJECT, REF oResult )
					SELF:_oRDD := (IRdd) oResult
					SELF:BindingSource:DataSource := DbDataSource{ SELF:_oRDD }
				ENDIF
				// Apply index order if specified
				IF !String.IsNullOrEmpty( SELF:Order )
					OrdSetFocus( SELF:Order )
				ENDIF
				// NoDataOnLoad: open empty — a later REQUERY() call loads the real data
				IF SELF:NoDataOnLoad
					DbSetFilter( , "1 = 0" )
				ELSEIF !String.IsNullOrEmpty( SELF:Filter )
					DbSetFilter( , SELF:Filter )
				ENDIF
			ENDIF

		/// <summary>
		/// Closes the work area opened by <see cref="Open"/>. For DBC cursors, only the work area is closed — the DBC itself remains open as it may be shared by other cursors.
		/// </summary>
		METHOD Close() AS VOID
			// Free Table
			IF SELF:IsFreeTable
				DbCloseArea( SELF:Alias )
				// Todo : Don't forget to remove the Notify EventHandler...
				//CoreDb.Notify -= SELF:Notify
			ELSE
				// DBC table — close work area only, not the DBC (may be shared by other cursors)
				DbCloseArea( SELF:Alias )
			ENDIF

		/// <summary>
		/// Moves the <see cref="BindingSource"/> position to match the current RDD record number (1-based → 0-based). Call this after any programmatic <c>DbGo*</c> / <c>DbSkip</c> to keep the grid in sync.
		/// </summary>
		METHOD Sync() AS VOID
			LOCAL posRdd AS DWORD
			//LOCAL posSrc AS INT
			//
			IF SELF:_oRDD != NULL
				posRdd := SELF:_oRDD:RecNo
				//posSrc := SELF:BindingSource:Position
				// BindingSource Position is zero-Based
				SELF:BindingSource:Position := (INT) (posRdd - 1)
			ENDIF

			//		METHOD Notify(osender AS XSharp.RDD.IRdd, e AS XSharp.DbNotifyEventArgs) AS VOID
			//			LOCAL posRdd AS INT
			//			LOCAL posSrc AS INT
			//			// What will append if the move comes from the BindingSource...???
			//			IF ( e:Type == XSharp.RDD.Enums.DbNotificationType.AfterMove )
			//				posRdd := osender:RecNo
			//				posSrc := SELF:BindingSource:Position
			//				// BindingSource Position is zero-Based
			//				SELF:BindingSource:Position = posRdd - 1
			//			ENDIF

	END CLASS

END NAMESPACE
