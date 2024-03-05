USING System
USING System.Collections.Generic
USING System.Text
USING System.Windows.Forms
USING System.ComponentModel

BEGIN NAMESPACE XSharp.VFP.UI
	/// <summary>
	/// The DataEnvironment class.
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
			SELF:Cursors := List<DbCursor>{}
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
			//
			IF !String.IsNullOrEmpty( SELF:InitialSelectedAlias )
				VAR selectedCursor := SELF[ SELF:InitialSelectedAlias ]
				IF ( selectedCursor != NULL )
					IF selectedCursor:IsFreeTable
						DbSelectArea( SELF:InitialSelectedAlias )
					ELSE
						NOP
					ENDIF
				ENDIF
			ELSE
				IF SELF:Cursors:Count > 0
					SELF:DataSource := SELF:Cursors[0]:BindingSource
				ENDIF
			ENDIF

		METHOD DataEnvironment_FormClosing( sender AS OBJECT, e AS System.Windows.Forms.FormClosingEventArgs) AS VOID
			//
			IF SELF:AutoCloseTables
				FOREACH cursor AS DbCursor IN Cursors
					cursor:Close()
				NEXT
			ENDIF

		PROPERTY InitialSelectedAlias AS STRING AUTO

	END CLASS

END NAMESPACE


