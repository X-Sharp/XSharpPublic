// DataEnvironment.prg
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
	/// VFP-compatible DataEnvironment that manages the cursor (work-area) and relation lifecycle
	/// for a form.<br/>
	/// On <see cref="Init"/>, when <see cref="AutoOpenTables"/> is <c>.T.</c>, opens all
	/// <see cref="Cursors"/> in order, then applies each <see cref="Relations"/> entry via
	/// <c>DbSetRelation</c>. After all cursors are open, selects the
	/// <see cref="InitialSelectedAlias"/> work area (or falls back to the first cursor's
	/// <c>BindingSource</c>).<br/>
	/// On form close (<see cref="DataEnvironment_FormClosing"/>), when <see cref="AutoCloseTables"/>
	/// is <c>.T.</c>, closes all work areas and then closes each referenced DBC exactly once.<br/>
	/// Individual cursors can be looked up by alias via the indexer <c>DataEnvironment["alias"]</c>.
	/// </summary>
	PARTIAL CLASS DataEnvironment

		/// <summary>When <c>.T.</c> (default), all work areas opened by <see cref="Init"/> are closed automatically when the form closes.</summary>
		PROPERTY AutoCloseTables AS LOGIC AUTO
		/// <summary>When <c>.T.</c> (default), <see cref="Init"/> opens all <see cref="Cursors"/> automatically before the form displays.</summary>
		PROPERTY AutoOpenTables AS LOGIC AUTO

		/// <summary>Bound data source; set to the first cursor's <c>BindingSource</c> when no <see cref="InitialSelectedAlias"/> is specified.</summary>
		PROPERTY DataSource AS OBJECT AUTO
		/// <summary>Programmatic name of the DataEnvironment object.</summary>
		PROPERTY Name AS STRING AUTO

		/// <summary>VFP Visible stub — stored for source compatibility.</summary>
		PROPERTY Visible AS LOGIC AUTO
		/// <summary>VFP TabStop stub — stored for source compatibility.</summary>
		PROPERTY TabStop AS LOGIC AUTO

		/// <summary>Ordered list of <see cref="DbCursor"/> objects that this DataEnvironment opens and closes.</summary>
		PROPERTY Cursors AS List<DbCursor> AUTO

		/// <summary>List of <see cref="Relation"/> objects applied (via <c>DbSetRelation</c>) after all cursors are open.</summary>
		PROPERTY Relations AS List<Relation> AUTO

		/// <summary>Returns the <see cref="DbCursor"/> whose <c>Alias</c> matches <paramref name="cursorName"/> (case-insensitive), or <c>NULL</c> if not found.</summary>
		PROPERTY SELF[ cursorName AS STRING ] AS DbCursor
			GET
				FOREACH cursor AS DbCursor IN Cursors
					IF String.Compare( cursorName, cursor:Alias, TRUE ) == 0
						RETURN cursor
					ENDIF
				NEXT
				RETURN NULL
			END GET
		END PROPERTY

		CONSTRUCTOR( )
			SELF:Cursors   := List<DbCursor>{}
			SELF:Relations := List<Relation>{}
			SELF:AutoCloseTables := TRUE
			SELF:AutoOpenTables := TRUE
			RETURN

		/// <summary>
		/// Opens cursors (when <see cref="AutoOpenTables"/> is <c>.T.</c>), applies relations,
		/// and selects the <see cref="InitialSelectedAlias"/> work area. Called by the owning
		/// form during its initialisation sequence.
		/// </summary>
		METHOD Init() AS VOID
			//
			IF SELF:AutoOpenTables
				FOREACH cursor AS DbCursor IN Cursors
					cursor:Open()
				NEXT
			ENDIF
			// Apply relations after all cursors are open
			FOREACH VAR rel IN SELF:Relations
				IF !String.IsNullOrEmpty(rel:ParentAlias)
					DbSelectArea(rel:ParentAlias)
				ENDIF
				IF !String.IsNullOrEmpty((STRING)rel:ChildOrder)
					OrdSetFocus(rel:ChildOrder, rel:ChildAlias)
				ENDIF
				DbSetRelation(rel:ChildAlias, MCompile((STRING)rel:RelationalExpr), (STRING)rel:RelationalExpr)
			NEXT
			//
			IF !String.IsNullOrEmpty( SELF:InitialSelectedAlias )
				VAR selectedCursor := SELF[ SELF:InitialSelectedAlias ]
				IF ( selectedCursor != NULL )
					DbSelectArea( SELF:InitialSelectedAlias )
				ENDIF
			ELSE
				IF SELF:Cursors:Count > 0
					SELF:DataSource := SELF:Cursors[0]:BindingSource
				ENDIF
			ENDIF

		/// <summary>
		/// Form-closing handler wired by the owning form. When <see cref="AutoCloseTables"/> is
		/// <c>.T.</c>, closes all work areas and then closes each referenced DBC exactly once
		/// (deduplication via a <c>HashSet</c> on the database file path).
		/// </summary>
		METHOD DataEnvironment_FormClosing( sender AS OBJECT, e AS System.Windows.Forms.FormClosingEventArgs) AS VOID
			//
			IF SELF:AutoCloseTables
				// First close all work areas
				FOREACH cursor AS DbCursor IN Cursors
					cursor:Close()
				NEXT
				// Then close each unique DBC that was used (once per distinct database file)
				VAR closedDbs := HashSet<STRING>{ StringComparer.InvariantCultureIgnoreCase }
				FOREACH cursor AS DbCursor IN Cursors
					IF !cursor:IsFreeTable .AND. !String.IsNullOrEmpty( cursor:DataBase )
						IF closedDbs:Add( cursor:DataBase )
							// Look up by filename and close by logical name
							VAR oDb := DbcManager.FindDatabase( cursor:DataBase )
							IF oDb != NULL
								DbcManager.Close( oDb:Name )
							ENDIF
						ENDIF
					ENDIF
				NEXT
			ENDIF

		/// <summary>Alias of the work area to select after <see cref="Init"/> opens all cursors. When empty, the first cursor's <c>BindingSource</c> is set as <see cref="DataSource"/>.</summary>
		PROPERTY InitialSelectedAlias AS STRING AUTO

	END CLASS

END NAMESPACE


