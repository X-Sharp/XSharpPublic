// VFPForm.prg

USING System
USING System.Collections.Generic
USING System.Text
USING System.Windows.Forms
USING System.ComponentModel

BEGIN NAMESPACE XSharp.VFP.UI
	/// <summary>
	/// The VFPForm class.
	/// </summary>
	PARTIAL CLASS Form INHERIT System.Windows.Forms.Form IMPLEMENTS IDynamicProperties, IDynamicProperties2, IVFPOwner

#include ".\Headers\FontProperties.xh"

#include ".\Headers\MousePointer.xh"


#region VFP Form Properties to emulate

			// Todo
		PROPERTY ScaleMode AS INT AUTO
			// Todo
		PROPERTY DoCreate AS LOGIC AUTO
			// Todo
		PROPERTY AutoCenter AS LOGIC AUTO
			// Todo
		PROPERTY Movable AS LOGIC AUTO

			// Ease the change of Type
		NEW PROPERTY WindowState AS INT GET (INT)SUPER:WindowState SET SUPER:WindowState := (System.Windows.Forms.FormWindowState)VALUE


			// Todo  0 = Modeless; 1 = Modal
		PROPERTY WindowType AS INT AUTO
			// Todo
		PROPERTY ScrollBars AS INT AUTO
			// Todo
		PROPERTY BorderStyle AS INT AUTO
			// Todo
		PROPERTY ColorSource AS INT AUTO
					// Todo
		PROPERTY ShowWindowState AS INT AUTO

			// The Form is returned as Object to force late-bound code
		PROPERTY ThisForm AS OBJECT GET SELF

        // If the Form belongs to a FormSet, the "Owner" is available here.
        PROPERTY ThisFormSet AS FormSet AUTO


		[System.ComponentModel.Category("VFP Properties"),System.ComponentModel.Description("Get/Set the Text of the Form (Title)")];
		PROPERTY Caption AS STRING GET SELF:Text SET SELF:Text :=VALUE

		[System.ComponentModel.Category("VFP Properties"),System.ComponentModel.Description("Map to MaximizeBox")];
		PROPERTY MaxButton AS LOGIC GET SELF:MaximizeBox SET SELF:MaximizeBox := VALUE
		[System.ComponentModel.Category("VFP Properties"),System.ComponentModel.Description("Map to MinimizeBox")];
		PROPERTY MinButton AS LOGIC GET SELF:MinimizeBox SET SELF:MinimizeBox := VALUE

			// Todo Add/Remove the Close item to the System Menu
		[System.ComponentModel.Category("VFP Properties"),System.ComponentModel.Description("Indicate if ControlBox is visible")];
		PROPERTY Closable AS LOGIC GET SELF:ControlBox SET SELF:ControlBox := VALUE

		[System.ComponentModel.Category("VFP Properties"),System.ComponentModel.Description("Set the FormBorderStyle")];
		PROPERTY TitleBar AS INT
		SET
			IF ( VALUE == 0 )
				SELF:FormBorderStyle := FormBorderStyle.None
			ELSEIF ( VALUE == 1 )
				SELF:FormBorderStyle := FormBorderStyle.Sizable
			ENDIF
		END SET
		END PROPERTY

		[System.ComponentModel.Category("VFP Properties"),System.ComponentModel.Description("Indicate if the Form is MDI")];
		PROPERTY MDIForm AS LOGIC AUTO := FALSE

		// Ok,BindControls, but it seems that Grids are Binding based on RecordSource setting..??
		PROPERTY BindControls AS LOGIC AUTO

		#include ".\Headers\Tooltips.xh"

#endregion

#region Support of VFP DataBinding, DataEnvironment, Cursors

		METHOD DataSession( setSession AS INT ) AS VOID

#include ".\Headers\Anchor.xh"

#include ".\Headers\ControlSource.xh"

		PROPERTY DataEnvironment AS DataEnvironment AUTO

		METHOD DoBindings( ) AS VOID
			LOCAL ds AS OBJECT
			LOCAL sourceName AS STRING
			LOCAL fieldName AS STRING
			LOCAL sender AS System.Windows.Forms.Control
			LOCAL cursor AS DbCursor
			//
			IF SELF:DataEnvironment != NULL .AND. SELF:DataEnvironment:DataSource != NULL
				IF SELF:BindingDefinition == NULL
					SELF:BindingDefinition := Dictionary<System.Windows.Forms.Control,STRING>{}
				ENDIF
				// Need to add some CustomControl
				IF SELF:Controls:Count > 0
					SELF:PopulateBindings( SELF:Controls )
				ENDIF
				//
				FOREACH VAR bindingInfo IN SELF:BindingDefinition
					ds := SELF:DataEnvironment:DataSource
					fieldName := bindingInfo:Value
					sender := bindingInfo:Key
					// Do we have a DataSource Name ?
					IF fieldName:IndexOf(".") > 0
						sourceName := fieldName.Substring(0,fieldName:IndexOf("."))
						cursor := SELF:DataEnvironment[ sourceName ]
						IF cursor != NULL
							ds := cursor:BindingSource
						ENDIF
						fieldName := fieldName.Substring(fieldName:IndexOf(".")+1)
					ENDIF
					IF ds != NULL
						// Always "Text"...We may have to change that
						sender.DataBindings.Add( Binding{ "Text", ds , fieldName } )
					ENDIF
				NEXT
			ENDIF

		METHOD PopulateBindings( controls AS System.Windows.Forms.Control.ControlCollection ) AS VOID
			LOCAL bDef AS OBJECT
			FOREACH VAR child IN controls
				//
				bDef := child:GetType():GetProperty("BindingDefinition")
				IF bDef != NULL
					VAR bDefValue := Send( bDef,"GetValue", child )
					IF bDefValue IS Dictionary<System.Windows.Forms.Control,STRING> VAR bindingDefs
						FOREACH VAR kvp IN bindingDefs
							TRY
								SELF:BindingDefinition:Add( kvp:Key, kvp:Value )
							CATCH
								NOP
							END TRY
						NEXT
					ENDIF
				ENDIF
				// The Child is a Container ?
				VAR chld := child ASTYPE System.Windows.Forms.Control
				IF ( chld != NULL) AND ( chld:Controls:Count > 0)
					SELF:PopulateBindings( chld:Controls )
				ENDIF
			NEXT


#include ".\Headers\FormOverride.xh"
			//
		METHOD Refresh() AS VOID
			// Refresh the BindingSource of the Current Workarea/Cursor
			// Should we refresh all attached Cursors ??
			IF SELF:DataEnvironment != NULL .AND. SELF:DataEnvironment:DataSource != NULL
				VAR currentAlias := Alias( DbGetSelect() )
				IF ( currentAlias != NULL )
					VAR vfpCursor := SELF:DataEnvironment[ currentAlias ]
					IF vfpCursor != NULL
						vfpCursor:Sync()
					ENDIF
				ENDIF
			ENDIF
			//
			IF SELF:_VFPRefresh != NULL
				SELF:_VFPRefresh:Call()
			ENDIF
			// The Windows Form Refresh redraw itself and any child controls.
			SUPER:Refresh()

		PROTECTED _validQueryUnload AS LOGIC
		INTERNAL _handledKeypress AS LOGIC

		METHOD Release() AS VOID
			//
			SELF:_validQueryUnload := TRUE
			SELF:Close()

#endregion


		CONSTRUCTOR(  ) STRICT
			SUPER()
			// Default Values
			SELF:BindControls := TRUE


		PRIVATE _VFPLoad AS VFPOverride
		[System.ComponentModel.Category("VFP Events"),System.ComponentModel.Description("Get/Set the name of the Load method. Occurs just before an object is created.")];
		PROPERTY vfpLoad AS STRING GET _VFPLoad?:SendTo SET Set_Load( VFPOverride{SELF, VALUE} )

		METHOD Set_Load( methodCall AS VFPOverride ) AS VOID
			SELF:Load += System.EventHandler{ SELF, @OnVFPLoadCall() }
			SELF:_VFPLoad := methodCall

		PRIVATE METHOD OnVFPLoadCall( sender AS OBJECT, e AS System.EventArgs) AS VOID
			//
			IF SELF:_VFPLoad != NULL
				SELF:_VFPLoad:Call( )
			ENDIF

		PRIVATE _VFPQueryUnload AS VFPOverride
		[System.ComponentModel.Category("VFP Events"),System.ComponentModel.Description("Get/Set the name of the QueryUnload method. Occurs before a form is unloaded.")];
		PROPERTY vfpQueryUnLoad AS STRING GET _VFPQueryUnload?:SendTo SET Set_QueryUnload( VFPOverride{SELF, VALUE} )

		METHOD Set_QueryUnload( methodCall AS VFPOverride ) AS VOID
			SELF:FormClosing += System.Windows.Forms.FormClosingEventHandler{ SELF, @OnVFPQueryUnload() }
			SELF:_VFPQueryUnload := methodCall

		PRIVATE METHOD OnVFPQueryUnload( sender AS OBJECT, e AS FormClosingEventArgs) AS VOID
			//
			IF SELF:_VFPQueryUnload != NULL
				SELF:_validQueryUnload := TRUE
				SELF:_VFPQueryUnload:Call( )
				IF !SELF:_validQueryUnload
					e:Cancel := TRUE
				ENDIF
			ENDIF

#include ".\Headers\InitCall.xh"


		METHOD NODEFAULT() AS VOID
			// Reset all settings....
			SELF:_validQueryUnload := FALSE
			SELF:_handledKeypress := TRUE


		NEW METHOD Show() AS VOID
			IF SELF:MDIForm
				SUPER:Show()
			ELSE
				SELF:ShowDialog()
			ENDIF
		END METHOD

		NEW PROPERTY Visible AS LOGIC
			GET
				RETURN SUPER:Visible
			END GET
			SET
				IF ( SUPER:Visible != VALUE )
					IF VALUE
						SELF:Show()
					ELSE
						SUPER:Visible := VALUE
					ENDIF
				ENDIF
			END SET
		END PROPERTY

	END CLASS

END NAMESPACE
