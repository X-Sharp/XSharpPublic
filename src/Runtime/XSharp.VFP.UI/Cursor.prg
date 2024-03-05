USING System
USING System.Collections.Generic
USING System.Text
USING System.Windows.Forms
USING System.ComponentModel

BEGIN NAMESPACE XSharp.VFP.UI
	/// <summary>
	/// The Cursor class.
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
				DbUseArea(TRUE, "DBFVFP", SELF:CursorSource , SELF:Alias )
				// And Attached the corresponding DataSource
				LOCAL oResult := NULL AS OBJECT
				IF CoreDb.Info(DBI_RDD_OBJECT,REF oResult)
					SELF:_oRDD := (IRdd) oResult
					SELF:BindingSource:DataSource := DbDataSource{SELF:_oRDD}
					// Do we have an Auto Refresh of the BindingSource if the underlying DataBase is moving/changing ??
					//CoreDb.Notify += SELF:Notify
				ENDIF
			ELSE
                    NOP
				// DBC
				NOP
			ENDIF

		METHOD Close() AS VOID
			// Free Table
			IF SELF:IsFreeTable
				DbCloseArea( SELF:Alias )
				// Todo : Don't forget to remove the Notify EventHandler...
				//CoreDb.Notify -= SELF:Notify
			ELSE
				NOP
				// DBC
			ENDIF

		METHOD Sync() AS VOID
			LOCAL posRdd AS INT
			//LOCAL posSrc AS INT
			//
			IF SELF:_oRDD != NULL
				posRdd := SELF:_oRDD:RecNo
				//posSrc := SELF:BindingSource:Position
				// BindingSource Position is zero-Based
				SELF:BindingSource:Position := posRdd - 1
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
