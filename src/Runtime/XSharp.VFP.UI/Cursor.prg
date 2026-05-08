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
	/// The VFP compatible Cursor class.
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

		PROPERTY IsFreeTable AS LOGIC GET String.IsNullOrEmpty( SELF:DataBase )

		/// <summary>
		/// The BindingSource stored in this Cursor
		/// </summary>
		PROPERTY BindingSource AS BindingSource AUTO

			// Todo
		PROPERTY BufferModeOverride AS INT AUTO

		PROTECTED PROPERTY _oRDD AS IRdd AUTO

		CONSTRUCTOR(  )
			SELF:BindingSource := BindingSource{}
			RETURN

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
				// Open the DBC — silently ignored if already open
				DbcManager.Open( SELF:DataBase, TRUE, SELF:ReadOnly, FALSE )
				// Resolve full .dbf path via the DBC metadata, fall back to DBC directory
				LOCAL cFullPath AS STRING
				LOCAL oDb AS DbcDatabase
				oDb := DbcManager.FindDatabase( SELF:DataBase )
				IF oDb != NULL
					LOCAL oTable AS DbcTable
					oTable := oDb:FindTable( SELF:CursorSource )
					IF oTable != NULL .AND. !String.IsNullOrEmpty( oTable:Path )
						cFullPath := oTable:Path
					ELSE
						cFullPath := System.IO.Path.Combine( ;
							System.IO.Path.GetDirectoryName( SELF:DataBase ), ;
							SELF:CursorSource + ".dbf" )
					ENDIF
				ELSE
					cFullPath := System.IO.Path.Combine( ;
						System.IO.Path.GetDirectoryName( SELF:DataBase ), ;
						SELF:CursorSource + ".dbf" )
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
