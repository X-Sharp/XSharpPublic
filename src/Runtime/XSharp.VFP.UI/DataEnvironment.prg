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
	/// The VFP compatible DataEnvironment class.
	/// </summary>
	PARTIAL CLASS DataEnvironment

		PROPERTY AutoCloseTables AS LOGIC AUTO
		PROPERTY AutoOpenTables AS LOGIC AUTO

		PROPERTY DataSource AS OBJECT AUTO
		PROPERTY Name AS STRING AUTO

		// Todo
		PROPERTY Visible AS LOGIC AUTO
		// Todo
		PROPERTY TabStop AS LOGIC AUTO

		PROPERTY Cursors AS List<DbCursor> AUTO

		PROPERTY Relations AS List<Relation> AUTO

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

		PROPERTY InitialSelectedAlias AS STRING AUTO

	END CLASS

END NAMESPACE


