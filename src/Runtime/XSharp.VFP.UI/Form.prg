// Form.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.


USING System
USING System.Collections.Generic
USING System.Text
USING System.Windows.Forms
USING System.ComponentModel

BEGIN NAMESPACE XSharp.VFP.UI
	/// <summary>
	/// The VFP compatible Form class.
	/// </summary>
	PARTIAL CLASS Form INHERIT System.Windows.Forms.Form IMPLEMENTS IDynamicProperties, IDynamicProperties2, IVFPOwner

#include "FontProperties.xh"

#include "MousePointer.xh"


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
			// BorderStyle: VFP values 0=None, 1=Fixed Single, 2=Fixed Dialog, 3=Sizable (default), 4=Fixed ToolWindow, 5=Sizable ToolWindow
		PROPERTY BorderStyle AS INT
			GET
				SWITCH SELF:FormBorderStyle
					CASE FormBorderStyle.None            ; RETURN 0
					CASE FormBorderStyle.FixedSingle     ; RETURN 1
					CASE FormBorderStyle.FixedDialog     ; RETURN 2
					CASE FormBorderStyle.Sizable         ; RETURN 3
					CASE FormBorderStyle.FixedToolWindow ; RETURN 4
					CASE FormBorderStyle.SizableToolWindow ; RETURN 5
				END SWITCH
				RETURN 3
			END GET
			SET
				SWITCH VALUE
					CASE 0 ; SELF:FormBorderStyle := FormBorderStyle.None
					CASE 1 ; SELF:FormBorderStyle := FormBorderStyle.FixedSingle
					CASE 2 ; SELF:FormBorderStyle := FormBorderStyle.FixedDialog
					CASE 3 ; SELF:FormBorderStyle := FormBorderStyle.Sizable
					CASE 4 ; SELF:FormBorderStyle := FormBorderStyle.FixedToolWindow
					CASE 5 ; SELF:FormBorderStyle := FormBorderStyle.SizableToolWindow
				END SWITCH
			END SET
		END PROPERTY
			// Todo
		PROPERTY ColorSource AS INT AUTO
				// VFP ShowWindow: 0=MDI child of _SCREEN (default), 1=Modal top-level, 2=Top-level modeless
		PRIVATE _showWindow AS INT
		VIRTUAL PROPERTY ShowWindow AS INT
			GET ; RETURN _showWindow ; END GET
			SET ; _showWindow := VALUE ; END SET
		END PROPERTY

			// The Form is returned as Object to force late-bound code
		PROPERTY ThisForm AS OBJECT GET SELF

        // If the Form belongs to a FormSet, the "Owner" is available here.
        PROPERTY ThisFormSet AS FormSet AUTO


		[System.ComponentModel.Category("VFP Properties"),System.ComponentModel.Description("Get/Set the Text of the Form (Title)")];
[System.ComponentModel.DefaultValue("")];
        PROPERTY Caption AS STRING GET SELF:Text SET SELF:Text :=VALUE

		[System.ComponentModel.Category("VFP Properties"),System.ComponentModel.Description("Map to MaximizeBox")];
        [System.ComponentModel.DefaultValue(true)];
        PROPERTY MaxButton AS LOGIC GET SELF:MaximizeBox SET SELF:MaximizeBox := VALUE
		[System.ComponentModel.Category("VFP Properties"),System.ComponentModel.Description("Map to MinimizeBox")];
        [System.ComponentModel.DefaultValue(true)];
        PROPERTY MinButton AS LOGIC GET SELF:MinimizeBox SET SELF:MinimizeBox := VALUE

			// Todo Add/Remove the Close item to the System Menu
		[System.ComponentModel.Category("VFP Properties"),System.ComponentModel.Description("Indicate if ControlBox is visible")];
        [System.ComponentModel.DefaultValue(true)];
        PROPERTY Closable AS LOGIC GET SELF:ControlBox SET SELF:ControlBox := VALUE

		[System.ComponentModel.Category("VFP Properties"),System.ComponentModel.Description("Set the FormBorderStyle")];
        [System.ComponentModel.DefaultValue(1)];
        PROPERTY TitleBar AS INT
		SET
			// 0 = no title bar (None), 1 = title bar (Sizable); other values not supported in WinForms
			SWITCH VALUE
				CASE 0 ; SELF:FormBorderStyle := FormBorderStyle.None
				CASE 1 ; SELF:FormBorderStyle := FormBorderStyle.Sizable
				OTHERWISE
					THROW NotSupportedException{"TitleBar value " + VALUE:ToString() + " is not supported; use 0 (no title) or 1 (title)."}
			END SWITCH
		END SET
		END PROPERTY

		[System.ComponentModel.Category("VFP Properties"),System.ComponentModel.Description("Indicate if the Form is MDI")];
        [System.ComponentModel.DefaultValue(false)];
        PROPERTY MDIForm AS LOGIC AUTO := FALSE

		// Ok,BindControls, but it seems that Grids are Binding based on RecordSource setting..??
		PROPERTY BindControls AS LOGIC AUTO

		#include "Tooltips.xh"

#endregion

#region Support of VFP DataBinding, DataEnvironment, Cursors

		METHOD DataSession( setSession AS INT ) AS VOID
		// TODO: full DataSession switching support
		END METHOD

		// HWND: returns the native window handle as an integer (VFP compatibility)
		PROPERTY HWND AS INT GET SELF:Handle:ToInt32()

#include "Anchor.xh"

#include "ControlSource.xh"

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
						LOCAL propName AS STRING
						DO CASE
						CASE sender IS System.Windows.Forms.CheckBox
							propName := "Checked"
						CASE sender IS System.Windows.Forms.NumericUpDown
							propName := "Value"
						CASE sender IS System.Windows.Forms.ComboBox
							propName := "SelectedValue"
						CASE sender IS System.Windows.Forms.ListBox
							propName := "SelectedValue"
						OTHERWISE
							propName := "Text"
						END CASE
						sender.DataBindings.Add( Binding{ propName, ds , fieldName } )
					ENDIF
				NEXT
				// Apply Grid RecordSources now that tables are open
				FOREACH VAR ctrl IN SELF:Controls
					IF ctrl IS Grid VAR grid
						grid:ApplyRecordSource()
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


#include "FormOverride.xh"
			//
		OVERRIDE METHOD Refresh() AS VOID
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

		METHOD Release() AS USUAL CLIPPER
			//
			SELF:_validQueryUnload := TRUE
            SELF:Close()
            RETURN NIL

#endregion


		CONSTRUCTOR(  ) STRICT
			SUPER()
			// Default Values
            SELF:BindControls := TRUE
            SELF:Size := System.Drawing.Size{375, 250}


		PRIVATE _VFPLoad AS VFPOverride
		[System.ComponentModel.Category("VFP Events"),System.ComponentModel.Description("Get/Set the name of the Load method. Occurs just before an object is created.")];
        [System.ComponentModel.DefaultValue("")];
        PROPERTY vfpLoad AS STRING GET _VFPLoad?:SendTo SET Set_Load( VFPOverride{SELF, VALUE} )

		METHOD Set_Load( methodCall AS VFPOverride ) AS VOID
			SELF:_VFPLoad := methodCall

		PROTECTED OVERRIDE METHOD OnLoad(e AS System.EventArgs) AS VOID
			SUPER:OnLoad(e)
			MainWindow.Current?:RegisterForm(SELF)
			SELF:Load()

		PRIVATE METHOD OnVFPLoadCall( sender AS OBJECT, e AS System.EventArgs) AS VOID
			SELF:Load()

		// ── B-2: Load virtual stub — subclass overrides are called ────────────
		NEW METHOD Load() AS VOID
			IF SELF:_VFPLoad != NULL
				SELF:_VFPLoad:Call()
			ENDIF

		// ── B-2: Unload virtual stub ─────────────────────────────────────────
		PRIVATE _VFPUnload AS VFPOverride
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.Description("Fires when the form is unloaded.")];
		[System.ComponentModel.DefaultValue("")];
		PROPERTY vfpUnload AS STRING GET _VFPUnload?:SendTo SET Set_Unload( VFPOverride{SELF, VALUE} )

		METHOD Set_Unload( methodCall AS VFPOverride ) AS VOID
			SELF:FormClosed += System.Windows.Forms.FormClosedEventHandler{ SELF, @OnVFPUnloadCall() }
			SELF:_VFPUnload := methodCall

		PRIVATE METHOD OnVFPUnloadCall( sender AS OBJECT, e AS System.Windows.Forms.FormClosedEventArgs ) AS VOID
			SELF:Unload()

		VIRTUAL METHOD Unload() AS VOID
			IF SELF:_VFPUnload != NULL
				SELF:_VFPUnload:Call()
			ENDIF

		PRIVATE _VFPQueryUnload AS VFPOverride
		[System.ComponentModel.Category("VFP Events"),System.ComponentModel.Description("Get/Set the name of the QueryUnload method. Occurs before a form is unloaded.")];
        [System.ComponentModel.DefaultValue("")];
        PROPERTY vfpQueryUnLoad AS STRING GET _VFPQueryUnload?:SendTo SET Set_QueryUnload( VFPOverride{SELF, VALUE} )

		METHOD Set_QueryUnload( methodCall AS VFPOverride ) AS VOID
			SELF:FormClosing += System.Windows.Forms.FormClosingEventHandler{ SELF, @OnVFPQueryUnload() }
			SELF:_VFPQueryUnload := methodCall

		// ── C-18: QueryUnload cancel-logic state machine ─────────────────────
		// Flow: FormClosing fires → _validQueryUnload := TRUE (allow close by default)
		//   → VFP string-wired handler runs (vfpQueryUnLoad assignment)
		//     → if it calls NODEFAULT(), _validQueryUnload becomes FALSE → e.Cancel := TRUE
		//   → virtual QueryUnload stub runs (subclass overrides call NODEFAULT() to cancel)
		//     → if _validQueryUnload still FALSE → e.Cancel := TRUE
		// NODEFAULT() also sets _handledKeypress := TRUE (shared flag; both live in the same method).
		PRIVATE METHOD OnVFPQueryUnload( sender AS OBJECT, e AS FormClosingEventArgs) AS VOID
			IF SELF:_VFPQueryUnload != NULL
				SELF:_validQueryUnload := TRUE
				SELF:_VFPQueryUnload:Call( )
				IF !SELF:_validQueryUnload
					e:Cancel := TRUE
				ENDIF
			ENDIF
			SELF:QueryUnload((INT) e:CloseReason)
			IF !SELF:_validQueryUnload
				e:Cancel := TRUE
			ENDIF

		// ── B-2: QueryUnload virtual stub ────────────────────────────────────
		VIRTUAL METHOD QueryUnload(nUnloadType AS INT) AS VOID
			// Override in subclass; call NODEFAULT() to cancel the close
			NOP

#include "InitCall.xh"


		METHOD NODEFAULT() AS VOID
			// Reset all settings....
			SELF:_validQueryUnload := FALSE
			SELF:_handledKeypress := TRUE


		NEW METHOD Show() AS VOID
			// MDIForm = TRUE means this form IS its own MDI container — show normally.
			IF SELF:MDIForm
				SUPER:Show()
				RETURN
			ENDIF
			LOCAL screen AS MainWindow
			screen := MainWindow.Current
			SWITCH SELF:ShowWindow
			CASE 0  // MDI child of _SCREEN
				IF screen != NULL .AND. screen:IsMdiContainer .AND. SELF:MdiParent == NULL .AND. !SELF:IsMdiContainer
					SELF:MdiParent := screen
				ENDIF
				IF SELF:WindowType == 1
					SUPER:ShowDialog()
				ELSE
					SUPER:Show()
				ENDIF
			CASE 1  // Modal top-level
				SUPER:ShowDialog()
			OTHERWISE  // 2 = top-level modeless (or any future value)
				SUPER:Show()
			END SWITCH
		END METHOD

		// ── B-5: ActiveControl ───────────────────────────────────────────────
		NEW PROPERTY ActiveControl AS USUAL
			GET
				RETURN SUPER:ActiveControl
			END GET
		END PROPERTY

		// ── B-3: KeyPreview ──────────────────────────────────────────────────
		NEW PROPERTY KeyPreview AS LOGIC
			GET
				RETURN SUPER:KeyPreview
			END GET
			SET
				SUPER:KeyPreview := VALUE
			END SET
		END PROPERTY

		// ── B-1: Activate / Deactivate ───────────────────────────────────────
		PRIVATE _VFPActivate AS VFPOverride
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.Description("Fires when the form receives focus.")];
		[System.ComponentModel.DefaultValue("")];
		PROPERTY vfpActivate AS STRING GET _VFPActivate?:SendTo SET SELF:_VFPActivate := VFPOverride{SELF, VALUE}

		NEW METHOD Activate() AS VOID
			IF SELF:_VFPActivate != NULL
				SELF:_VFPActivate:Call()
			ENDIF

		PROTECTED OVERRIDE METHOD OnActivated(e AS System.EventArgs) AS VOID
			SUPER:OnActivated(e)
			SELF:Activate()

		PRIVATE _VFPDeactivate AS VFPOverride
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.Description("Fires when the form loses focus.")];
		[System.ComponentModel.DefaultValue("")];
		PROPERTY vfpDeactivate AS STRING GET _VFPDeactivate?:SendTo SET SELF:_VFPDeactivate := VFPOverride{SELF, VALUE}

		NEW METHOD Deactivate() AS VOID
			IF SELF:_VFPDeactivate != NULL
				SELF:_VFPDeactivate:Call()
			ENDIF

		PROTECTED OVERRIDE METHOD OnDeactivate(e AS System.EventArgs) AS VOID
			SUPER:OnDeactivate(e)
			SELF:Deactivate()

		// ── B-4: GotFocus / LostFocus ────────────────────────────────────────
		PRIVATE _VFPGotFocus AS VFPOverride
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.Description("Fires when the form receives keyboard focus.")];
		[System.ComponentModel.DefaultValue("")];
		PROPERTY vfpGotFocus AS STRING GET _VFPGotFocus?:SendTo SET SELF:_VFPGotFocus := VFPOverride{SELF, VALUE}

		NEW METHOD GotFocus() AS VOID
			IF SELF:_VFPGotFocus != NULL
				SELF:_VFPGotFocus:Call()
			ENDIF

		PROTECTED OVERRIDE METHOD OnGotFocus(e AS System.EventArgs) AS VOID
			SUPER:OnGotFocus(e)
			SELF:GotFocus()

		PRIVATE _VFPLostFocus AS VFPOverride
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.Description("Fires when the form loses keyboard focus.")];
		[System.ComponentModel.DefaultValue("")];
		PROPERTY vfpLostFocus AS STRING GET _VFPLostFocus?:SendTo SET SELF:_VFPLostFocus := VFPOverride{SELF, VALUE}

		NEW METHOD LostFocus() AS VOID
			IF SELF:_VFPLostFocus != NULL
				SELF:_VFPLostFocus:Call()
			ENDIF

		PROTECTED OVERRIDE METHOD OnLostFocus(e AS System.EventArgs) AS VOID
			SUPER:OnLostFocus(e)
			SELF:LostFocus()

		// ── B-4: Click / DblClick ────────────────────────────────────────────
		PRIVATE _VFPClick AS VFPOverride
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.Description("Fires when the user clicks the form.")];
		[System.ComponentModel.DefaultValue("")];
		PROPERTY vfpClick AS STRING GET _VFPClick?:SendTo SET SELF:_VFPClick := VFPOverride{SELF, VALUE}

		NEW METHOD Click() AS VOID
			IF SELF:_VFPClick != NULL
				SELF:_VFPClick:Call()
			ENDIF

		PROTECTED OVERRIDE METHOD OnClick(e AS System.EventArgs) AS VOID
			SUPER:OnClick(e)
			SELF:Click()

		PRIVATE _VFPDblClick AS VFPOverride
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.Description("Fires when the user double-clicks the form.")];
		[System.ComponentModel.DefaultValue("")];
		PROPERTY vfpDblClick AS STRING GET _VFPDblClick?:SendTo SET SELF:_VFPDblClick := VFPOverride{SELF, VALUE}

		METHOD DblClick() AS VOID
			IF SELF:_VFPDblClick != NULL
				SELF:_VFPDblClick:Call()
			ENDIF

		PROTECTED OVERRIDE METHOD OnDoubleClick(e AS System.EventArgs) AS VOID
			SUPER:OnDoubleClick(e)
			SELF:DblClick()

		// ── B-4: KeyPress ────────────────────────────────────────────────────
		PRIVATE _VFPKeyPress AS VFPOverride
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.Description("Fires when the user presses a key while the form has focus.")];
		[System.ComponentModel.DefaultValue("")];
		PROPERTY vfpKeyPress AS STRING GET _VFPKeyPress?:SendTo SET SELF:_VFPKeyPress := VFPOverride{SELF, VALUE}

		NEW METHOD KeyPress(nKeyCode AS INT, nShiftAltCtrl AS INT) AS VOID
			IF SELF:_VFPKeyPress != NULL
				SELF:_VFPKeyPress:Call()
			ENDIF

		PROTECTED OVERRIDE METHOD OnKeyPress(e AS System.Windows.Forms.KeyPressEventArgs) AS VOID
			SUPER:OnKeyPress(e)
			SELF:KeyPress((INT) e:KeyChar, 0)

		// ── B-4: Resize ──────────────────────────────────────────────────────
		PRIVATE _VFPResize AS VFPOverride
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.Description("Fires when the form is resized.")];
		[System.ComponentModel.DefaultValue("")];
		PROPERTY vfpResize AS STRING GET _VFPResize?:SendTo SET SELF:_VFPResize := VFPOverride{SELF, VALUE}

		NEW METHOD Resize() AS VOID
			IF SELF:_VFPResize != NULL
				SELF:_VFPResize:Call()
			ENDIF

		PROTECTED OVERRIDE METHOD OnResize(e AS System.EventArgs) AS VOID
			SUPER:OnResize(e)
			SELF:Resize()

		// ── B-4: Paint ───────────────────────────────────────────────────────
		PRIVATE _VFPPaint AS VFPOverride
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.Description("Fires when the form is repainted.")];
		[System.ComponentModel.DefaultValue("")];
		PROPERTY vfpPaint AS STRING GET _VFPPaint?:SendTo SET SELF:_VFPPaint := VFPOverride{SELF, VALUE}

		NEW METHOD Paint() AS VOID
			IF SELF:_VFPPaint != NULL
				SELF:_VFPPaint:Call()
			ENDIF

		PROTECTED OVERRIDE METHOD OnPaint(e AS System.Windows.Forms.PaintEventArgs) AS VOID
			SUPER:OnPaint(e)
			SELF:Paint()

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
