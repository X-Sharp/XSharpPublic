// Form.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.


USING System
USING System.IO
USING System.Collections.Generic
USING System.Text
USING System.Windows.Forms
USING System.ComponentModel

BEGIN NAMESPACE XSharp.VFP.UI
	/// <summary>
	/// VFP-compatible form that wraps <see cref="System.Windows.Forms.Form"/>.<br/>
	/// Supports the full VFP form lifecycle: <see cref="Load"/> / <see cref="Unload"/> /
	/// <see cref="QueryUnload"/> virtual stubs (string-wired via <see cref="vfpLoad"/> /
	/// <see cref="vfpUnload"/> / <see cref="vfpQueryUnLoad"/>), <see cref="NODEFAULT"/> cancel
	/// semantics, and <see cref="Release"/> for programmatic close.<br/>
	/// Data binding is handled by <see cref="DoBindings"/> (walks all child controls and calls
	/// <c>Binding.Add</c>) coordinated with the <see cref="DataEnvironment"/> property.<br/>
	/// Display mode is controlled by <see cref="ShowWindow"/> (0=MDI child, 1=modal, 2=modeless)
	/// and <see cref="WindowType"/>; MDI container behaviour by <see cref="MDIForm"/>.<br/>
	/// VFP events exposed: <see cref="vfpLoad"/>, <see cref="vfpUnload"/>,
	/// <see cref="vfpQueryUnLoad"/>, <see cref="vfpActivate"/>, <see cref="vfpDeactivate"/>,
	/// <see cref="vfpGotFocus"/>, <see cref="vfpLostFocus"/>, <see cref="vfpClick"/>,
	/// <see cref="vfpDblClick"/>, <see cref="vfpKeyPress"/>, <see cref="vfpResize"/>,
	/// <see cref="vfpPaint"/>, plus the standard events from the shared includes.
	/// </summary>
	PARTIAL CLASS Form INHERIT System.Windows.Forms.Form IMPLEMENTS IDynamicProperties, IDynamicProperties2, IVFPOwner

#include "FontProperties.xh"

#include "MousePointer.xh"


#region VFP Form Properties to emulate

		/// <summary>
		/// VFP ScaleMode stub — stored for source compatibility.
		/// </summary>
		PROPERTY ScaleMode AS INT AUTO
		/// <summary>
		/// VFP DoCreate stub — stored for source compatibility.
		/// </summary>
		PROPERTY DoCreate AS LOGIC AUTO
		/// <summary>
		/// VFP AutoCenter stub — stored for source compatibility (centering is handled by WinForms <c>StartPosition</c>).
		/// </summary>
		PROPERTY AutoCenter AS LOGIC AUTO
		/// <summary>
		/// VFP Movable stub — stored for source compatibility.
		/// </summary>
		PROPERTY Movable AS LOGIC AUTO

		/// <summary>
		/// VFP WindowState: 0=Normal, 1=Minimized, 2=Maximized. Maps to <see cref="System.Windows.Forms.FormWindowState"/> (cast).
		/// </summary>
		NEW PROPERTY WindowState AS INT GET (INT)SUPER:WindowState SET SUPER:WindowState := (System.Windows.Forms.FormWindowState)VALUE

		/// <summary>
		/// VFP WindowType: 0=Modeless, 1=Modal. Used by <see cref="Show"/> to choose between <c>Show()</c> and <c>ShowDialog()</c>.
		/// </summary>
		PROPERTY WindowType AS INT AUTO
		/// <summary>
		/// VFP ScrollBars stub — stored for source compatibility.
		/// </summary>
		PROPERTY ScrollBars AS INT AUTO
			/// <summary>
		/// VFP border style:<br/>
		/// 0=None, 1=Fixed Single, 2=Fixed Dialog, 3=Sizable (default), 4=Fixed ToolWindow, 5=Sizable ToolWindow.<br/>
		/// Maps to <see cref="System.Windows.Forms.Form.FormBorderStyle"/>.
		/// </summary>
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


        PRIVATE _picture AS STRING
        /// <summary>
        /// Path to the image file displayed in the normal enabled state. Setting this loads the image immediately via <c>VFPTools.ImageFromFile</c>.
        /// </summary>
        [System.ComponentModel.DefaultValue("")];
        PROPERTY Picture AS STRING
            GET
                IF SELF:_picture == NULL
                    SELF:_picture := ""
                ENDIF
				RETURN SELF:_picture
			END GET

			SET
                SELF:BackgroundImage := VFPTools.ImageFromFile( VALUE )
                SELF: BackgroundImageLayout := ImageLayout.Stretch
                SELF:_picture := value
			END SET
        END PROPERTY



		/// <summary>
		/// VFP ColorSource stub — stored for source compatibility.
		/// </summary>
		PROPERTY ColorSource AS INT AUTO
		/// <summary>
		/// VFP ShowWindow: 0=In Screen (MDI child of <c>_SCREEN</c>, default),
		/// 1=In Top-Level Form (MDI child of the nearest <c>MDIForm=.T.</c> form),
		/// 2=As Top-Level Form (standalone top-level window).<br/>
		/// Modality is controlled separately by <see cref="WindowType"/> (1=modal), not by this property.<br/>
		/// Consumed by <see cref="Show"/> to choose the appropriate WinForms presentation mode.
		/// </summary>
		PRIVATE _showWindow AS INT
		VIRTUAL PROPERTY ShowWindow AS INT
			GET ; RETURN _showWindow ; END GET
			SET ; _showWindow := VALUE ; END SET
		END PROPERTY

		/// <summary>
		/// Returns the form itself as <c>OBJECT</c> to force late-bound code (matches VFP's <c>THISFORM</c> keyword).
		/// </summary>
		PROPERTY ThisForm AS OBJECT GET SELF

		/// <summary>
		/// The <see cref="FormSet"/> that owns this form, when the form belongs to a form set. <c>NULL</c> for standalone forms.
		/// </summary>
		PROPERTY ThisFormSet AS FormSet AUTO


		/// <summary>
		/// Title text of the form. Maps to <see cref="System.Windows.Forms.Form.Text"/>.
		/// </summary>
		[System.ComponentModel.Category("VFP Properties"),System.ComponentModel.Description("Get/Set the Text of the Form (Title)")];
		[System.ComponentModel.DefaultValue("")];
		PROPERTY Caption AS STRING GET SELF:Text SET SELF:Text :=VALUE

		/// <summary>
		/// When <c>.T.</c>, shows the maximize button. Maps to <see cref="System.Windows.Forms.Form.MaximizeBox"/>.
		/// </summary>
		[System.ComponentModel.Category("VFP Properties"),System.ComponentModel.Description("Map to MaximizeBox")];
		[System.ComponentModel.DefaultValue(true)];
		PROPERTY MaxButton AS LOGIC GET SELF:MaximizeBox SET SELF:MaximizeBox := VALUE
		/// <summary>
		/// When <c>.T.</c>, shows the minimize button. Maps to <see cref="System.Windows.Forms.Form.MinimizeBox"/>.
		/// </summary>
		[System.ComponentModel.Category("VFP Properties"),System.ComponentModel.Description("Map to MinimizeBox")];
		[System.ComponentModel.DefaultValue(true)];
		PROPERTY MinButton AS LOGIC GET SELF:MinimizeBox SET SELF:MinimizeBox := VALUE

		/// <summary>
		/// When <c>.T.</c>, the control box (close button and system menu) is visible. Maps to <see cref="System.Windows.Forms.Form.ControlBox"/>.
		/// </summary>
		[System.ComponentModel.Category("VFP Properties"),System.ComponentModel.Description("Indicate if ControlBox is visible")];
		[System.ComponentModel.DefaultValue(true)];
		PROPERTY Closable AS LOGIC GET SELF:ControlBox SET SELF:ControlBox := VALUE

		/// <summary>
		/// VFP TitleBar: 0=no title bar (<c>FormBorderStyle.None</c>), 1=title bar (<c>Sizable</c>). Other values throw <c>NotSupportedException</c>.
		/// </summary>
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

		/// <summary>
		/// When <c>.T.</c>, this form is an MDI container — other forms with <c>ShowWindow=1</c> will live
		/// inside it. Automatically sets <see cref="System.Windows.Forms.Form.IsMdiContainer"/>.
		/// </summary>
		[System.ComponentModel.Category("VFP Properties"),System.ComponentModel.Description("Indicate if the Form is MDI")];
		[System.ComponentModel.DefaultValue(false)];
		PRIVATE _mdiForm AS LOGIC
		PROPERTY MDIForm AS LOGIC
			GET ; RETURN _mdiForm ; END GET
			SET
				_mdiForm := VALUE
				SELF:IsMdiContainer := VALUE
			END SET
		END PROPERTY

		/// <summary>
		/// When <c>.T.</c>, <see cref="DoBindings"/> wires child-control data bindings via the <see cref="DataEnvironment"/>.
		/// </summary>
		PROPERTY BindControls AS LOGIC AUTO

		#include "Tooltips.xh"

#endregion

#region Support of VFP DataBinding, DataEnvironment, Cursors

		/// <summary>
		/// VFP DataSession stub — full session switching not yet implemented.
		/// </summary>
		METHOD DataSession( setSession AS INT ) AS VOID
		// TODO: full DataSession switching support
		END METHOD

		/// <summary>
		/// Native window handle as a 32-bit integer. VFP compatibility — maps to <c>Handle.ToInt32()</c>.
		/// </summary>
		PROPERTY HWND AS INT GET SELF:Handle:ToInt32()

#include "Anchor.xh"

#include "ControlSource.xh"

		/// <summary>
		/// The <see cref="XSharp.VFP.UI.DataEnvironment"/> that manages cursors (work areas) for this form. Set by generated code before <see cref="Load"/> fires.
		/// </summary>
		PROPERTY DataEnvironment AS DataEnvironment AUTO

		/// <summary>
		/// Walks all child controls, collects their <c>BindingDefinition</c> dictionaries, and creates
		/// <see cref="System.Windows.Forms.Binding"/> objects from the <see cref="DataEnvironment"/>'s
		/// binding sources. Also calls <c>ApplyRecordSource()</c> on any <see cref="Grid"/> controls.
		/// </summary>
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

		/// <summary>
		/// Recursively collects <c>BindingDefinition</c> entries from all controls in <paramref name="controls"/> into <c>SELF:BindingDefinition</c>. Called by <see cref="DoBindings"/>.
		/// </summary>
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
		/// <summary>
		/// Synchronises the binding source for the current work area, fires <c>vfpRefresh</c> if set,
		/// then calls <c>SUPER:Refresh()</c> to repaint the form and all children.
		/// </summary>
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

		/// <summary>
		/// Programmatically closes the form. Sets the internal allow-close flag so <see cref="QueryUnload"/> does not cancel the close, then calls <c>Close()</c>.
		/// </summary>
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
		/// <summary>
		/// Name of the VFP method called when the form is first loaded. Fires before the form is visible.
		/// </summary>
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

		/// <summary>
		/// VFP Load virtual stub — calls the <see cref="vfpLoad"/> handler. Override in a subclass to add load logic.
		/// </summary>
		NEW METHOD Load() AS VOID
			IF SELF:_VFPLoad != NULL
				SELF:_VFPLoad:Call()
			ENDIF

		PRIVATE _VFPUnload AS VFPOverride
		/// <summary>
		/// Name of the VFP method called when the form is fully closed (after <c>FormClosed</c>).
		/// </summary>
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.Description("Fires when the form is unloaded.")];
		[System.ComponentModel.DefaultValue("")];
		PROPERTY vfpUnload AS STRING GET _VFPUnload?:SendTo SET Set_Unload( VFPOverride{SELF, VALUE} )

		METHOD Set_Unload( methodCall AS VFPOverride ) AS VOID
			SELF:FormClosed += System.Windows.Forms.FormClosedEventHandler{ SELF, @OnVFPUnloadCall() }
			SELF:_VFPUnload := methodCall

		PRIVATE METHOD OnVFPUnloadCall( sender AS OBJECT, e AS System.Windows.Forms.FormClosedEventArgs ) AS VOID
			SELF:Unload()

		/// <summary>
		/// VFP Unload virtual stub — calls the <see cref="vfpUnload"/> handler. Override in a subclass to add cleanup logic.
		/// </summary>
		VIRTUAL METHOD Unload() AS VOID
			IF SELF:_VFPUnload != NULL
				SELF:_VFPUnload:Call()
			ENDIF

		PRIVATE _VFPQueryUnload AS VFPOverride
		/// <summary>
		/// Name of the VFP method called just before the form closes. Call <see cref="NODEFAULT"/> inside the handler to cancel the close.
		/// </summary>
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

		/// <summary>
		/// VFP QueryUnload virtual stub — called from the <c>FormClosing</c> handler. Override in a subclass and call <see cref="NODEFAULT"/> to cancel the close.
		/// </summary>
		VIRTUAL METHOD QueryUnload(nUnloadType AS INT) AS VOID
			// Override in subclass; call NODEFAULT() to cancel the close
			NOP

#include "InitCall.xh"


		/// <summary>
		/// Cancels the current operation — sets the internal allow-close flag to <c>.F.</c> so the
		/// pending <c>FormClosing</c> event is cancelled, and marks the current key press as handled.
		/// Call from inside <see cref="QueryUnload"/> or a key-press handler to suppress the default action.
		/// </summary>
		METHOD NODEFAULT() AS VOID
			// Reset all settings....
			SELF:_validQueryUnload := FALSE
			SELF:_handledKeypress := TRUE


		/// <summary>
		/// Displays the form according to <see cref="ShowWindow"/> and <see cref="WindowType"/>:<br/>
		/// 0 = MDI child of <c>MainWindow.Current</c>;<br/>
		/// 1 = MDI child of the nearest open <c>MDIForm=.T.</c> form (falls back to <c>MainWindow</c>);<br/>
		/// 2 = standalone top-level window.<br/>
		/// Modality (<c>ShowDialog</c> vs <c>Show</c>) is driven by <see cref="WindowType"/>==1 in all cases.<br/>
		/// When <see cref="MDIForm"/> is <c>.T.</c>, the form is its own MDI container and is shown as a normal top-level.
		/// </summary>
		NEW METHOD Show() AS VOID
			// MDIForm = .T. means this form IS an MDI container — show as a normal top-level.
			// IsMdiContainer was already set by the MDIForm setter.
			IF SELF:MDIForm
				SUPER:Show()
				RETURN
			ENDIF
			LOCAL screen AS MainWindow
			screen := MainWindow.Current
			SWITCH SELF:ShowWindow
			CASE 0  // In Screen — MDI child of _SCREEN
				IF screen != NULL .AND. screen:IsMdiContainer .AND. SELF:MdiParent == NULL .AND. !SELF:IsMdiContainer
					SELF:MdiParent := screen
				ENDIF
				IF SELF:WindowType == 1
					SUPER:ShowDialog()
				ELSE
					SUPER:Show()
				ENDIF
			CASE 1  // In Top-Level Form — MDI child of the nearest MDIForm=.T. form
				IF SELF:MdiParent == NULL
					// Find the nearest open VFP form that is an MDI container
					FOREACH VAR frm IN System.Windows.Forms.Application.OpenForms
						IF frm IS Form VAR vfpFrm .AND. vfpFrm:MDIForm
							SELF:MdiParent := vfpFrm
							EXIT
						ENDIF
					NEXT
					// Fall back to MainWindow if no dedicated MDI container is open
					IF SELF:MdiParent == NULL .AND. screen != NULL .AND. screen:IsMdiContainer
						SELF:MdiParent := screen
					ENDIF
				ENDIF
				IF SELF:WindowType == 1
					SUPER:ShowDialog()
				ELSE
					SUPER:Show()
				ENDIF
			OTHERWISE  // 2 = As Top-Level Form (standalone) or any future value
				IF SELF:WindowType == 1
					SUPER:ShowDialog()
				ELSE
					SUPER:Show()
				ENDIF
			END SWITCH
		END METHOD

		/// <summary>
		/// The control that currently has focus on the form. Returns <c>SUPER:ActiveControl</c> typed as <c>USUAL</c> for VFP late-binding compatibility.
		/// </summary>
		NEW PROPERTY ActiveControl AS USUAL
			GET
				RETURN SUPER:ActiveControl
			END GET
		END PROPERTY

		/// <summary>
		/// When <c>.T.</c>, the form receives key events before the focused child control. Maps to <see cref="System.Windows.Forms.Form.KeyPreview"/>.
		/// </summary>
		NEW PROPERTY KeyPreview AS LOGIC
			GET
				RETURN SUPER:KeyPreview
			END GET
			SET
				SUPER:KeyPreview := VALUE
			END SET
        END PROPERTY

		PRIVATE _IconFilename AS STRING
		/// <summary>
		/// Path to the form's icon file. When set to an existing file, loads and applies it as
		/// <see cref="System.Windows.Forms.Form.Icon"/>; load failures are silently swallowed.
		/// </summary>
		NEW PROPERTY Icon AS STRING
            GET
                IF (_IconFilename == NULL)
                    _IconFilename := String.Empty
                ENDIF
                RETURN _IconFilename
            END GET
            SET
                IF !String.IsNullOrEmpty(VALUE)
                    _IconFilename := VALUE
                    IF File.Exists( _IconFilename )
                        TRY
                            SUPER:Icon := System.Drawing.Icon{_IconFilename}
                        CATCH
                            NOP
                        END TRY
                    ENDIF
                ENDIF
            END SET
        END PROPERTY

		// ── B-1: Activate / Deactivate ───────────────────────────────────────
		PRIVATE _VFPActivate AS VFPOverride
		/// <summary>
		/// Name of the VFP method called when the form is activated (receives focus from another window).
		/// </summary>
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.Description("Fires when the form receives focus.")];
		[System.ComponentModel.DefaultValue("")];
		PROPERTY vfpActivate AS STRING GET _VFPActivate?:SendTo SET SELF:_VFPActivate := VFPOverride{SELF, VALUE}

		/// <summary>
		/// VFP Activate virtual stub — calls the <see cref="vfpActivate"/> handler. Override in a subclass to add activation logic.
		/// </summary>
		NEW METHOD Activate() AS VOID
			IF SELF:_VFPActivate != NULL
				SELF:_VFPActivate:Call()
			ENDIF

		PROTECTED OVERRIDE METHOD OnActivated(e AS System.EventArgs) AS VOID
			SUPER:OnActivated(e)
			SELF:Activate()

		PRIVATE _VFPDeactivate AS VFPOverride
		/// <summary>
		/// Name of the VFP method called when the form is deactivated (loses focus to another window).
		/// </summary>
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.Description("Fires when the form loses focus.")];
		[System.ComponentModel.DefaultValue("")];
		PROPERTY vfpDeactivate AS STRING GET _VFPDeactivate?:SendTo SET SELF:_VFPDeactivate := VFPOverride{SELF, VALUE}

		/// <summary>
		/// VFP Deactivate virtual stub — calls the <see cref="vfpDeactivate"/> handler.
		/// </summary>
		NEW METHOD Deactivate() AS VOID
			IF SELF:_VFPDeactivate != NULL
				SELF:_VFPDeactivate:Call()
			ENDIF

		PROTECTED OVERRIDE METHOD OnDeactivate(e AS System.EventArgs) AS VOID
			SUPER:OnDeactivate(e)
			SELF:Deactivate()

		// ── B-4: GotFocus / LostFocus ────────────────────────────────────────
		PRIVATE _VFPGotFocus AS VFPOverride
		/// <summary>
		/// Name of the VFP method called when the form receives keyboard focus.
		/// </summary>
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.Description("Fires when the form receives keyboard focus.")];
		[System.ComponentModel.DefaultValue("")];
		PROPERTY vfpGotFocus AS STRING GET _VFPGotFocus?:SendTo SET SELF:_VFPGotFocus := VFPOverride{SELF, VALUE}

		/// <summary>
		/// VFP GotFocus virtual stub — calls the <see cref="vfpGotFocus"/> handler.
		/// </summary>
		NEW METHOD GotFocus() AS VOID
			IF SELF:_VFPGotFocus != NULL
				SELF:_VFPGotFocus:Call()
			ENDIF

		PROTECTED OVERRIDE METHOD OnGotFocus(e AS System.EventArgs) AS VOID
			SUPER:OnGotFocus(e)
			SELF:GotFocus()

		PRIVATE _VFPLostFocus AS VFPOverride
		/// <summary>
		/// Name of the VFP method called when the form loses keyboard focus.
		/// </summary>
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.Description("Fires when the form loses keyboard focus.")];
		[System.ComponentModel.DefaultValue("")];
		PROPERTY vfpLostFocus AS STRING GET _VFPLostFocus?:SendTo SET SELF:_VFPLostFocus := VFPOverride{SELF, VALUE}

		/// <summary>
		/// VFP LostFocus virtual stub — calls the <see cref="vfpLostFocus"/> handler.
		/// </summary>
		NEW METHOD LostFocus() AS VOID
			IF SELF:_VFPLostFocus != NULL
				SELF:_VFPLostFocus:Call()
			ENDIF

		PROTECTED OVERRIDE METHOD OnLostFocus(e AS System.EventArgs) AS VOID
			SUPER:OnLostFocus(e)
			SELF:LostFocus()

		// ── B-4: Click / DblClick ────────────────────────────────────────────
		PRIVATE _VFPClick AS VFPOverride
		/// <summary>
		/// Name of the VFP method called when the user clicks the form background.
		/// </summary>
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.Description("Fires when the user clicks the form.")];
		[System.ComponentModel.DefaultValue("")];
		PROPERTY vfpClick AS STRING GET _VFPClick?:SendTo SET SELF:_VFPClick := VFPOverride{SELF, VALUE}

		/// <summary>
		/// VFP Click virtual stub — calls the <see cref="vfpClick"/> handler.
		/// </summary>
		NEW METHOD Click() AS VOID
			IF SELF:_VFPClick != NULL
				SELF:_VFPClick:Call()
			ENDIF

		PROTECTED OVERRIDE METHOD OnClick(e AS System.EventArgs) AS VOID
			SUPER:OnClick(e)
			SELF:Click()

		PRIVATE _VFPDblClick AS VFPOverride
		/// <summary>
		/// Name of the VFP method called when the user double-clicks the form background.
		/// </summary>
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.Description("Fires when the user double-clicks the form.")];
		[System.ComponentModel.DefaultValue("")];
		PROPERTY vfpDblClick AS STRING GET _VFPDblClick?:SendTo SET SELF:_VFPDblClick := VFPOverride{SELF, VALUE}

		/// <summary>
		/// VFP DblClick virtual stub — calls the <see cref="vfpDblClick"/> handler.
		/// </summary>
		METHOD DblClick() AS VOID
			IF SELF:_VFPDblClick != NULL
				SELF:_VFPDblClick:Call()
			ENDIF

		PROTECTED OVERRIDE METHOD OnDoubleClick(e AS System.EventArgs) AS VOID
			SUPER:OnDoubleClick(e)
			SELF:DblClick()

		// ── B-4: KeyPress ────────────────────────────────────────────────────
		PRIVATE _VFPKeyPress AS VFPOverride
		/// <summary>
		/// Name of the VFP method called when the user presses a key while the form has focus (requires <see cref="KeyPreview"/> = <c>.T.</c>).
		/// </summary>
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.Description("Fires when the user presses a key while the form has focus.")];
		[System.ComponentModel.DefaultValue("")];
		PROPERTY vfpKeyPress AS STRING GET _VFPKeyPress?:SendTo SET SELF:_VFPKeyPress := VFPOverride{SELF, VALUE}

		/// <summary>
		/// VFP KeyPress virtual stub — calls the <see cref="vfpKeyPress"/> handler with the key code and shift state.
		/// </summary>
		NEW METHOD KeyPress(nKeyCode AS INT, nShiftAltCtrl AS INT) AS VOID
			IF SELF:_VFPKeyPress != NULL
				SELF:_VFPKeyPress:Call()
			ENDIF

		PROTECTED OVERRIDE METHOD OnKeyPress(e AS System.Windows.Forms.KeyPressEventArgs) AS VOID
			SUPER:OnKeyPress(e)
			SELF:KeyPress((INT) e:KeyChar, 0)

		// ── B-4: Resize ──────────────────────────────────────────────────────
		PRIVATE _VFPResize AS VFPOverride
		/// <summary>
		/// Name of the VFP method called when the form is resized.
		/// </summary>
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.Description("Fires when the form is resized.")];
		[System.ComponentModel.DefaultValue("")];
		PROPERTY vfpResize AS STRING GET _VFPResize?:SendTo SET SELF:_VFPResize := VFPOverride{SELF, VALUE}

		/// <summary>
		/// VFP Resize virtual stub — calls the <see cref="vfpResize"/> handler.
		/// </summary>
		NEW METHOD Resize() AS VOID
			IF SELF:_VFPResize != NULL
				SELF:_VFPResize:Call()
			ENDIF

		PROTECTED OVERRIDE METHOD OnResize(e AS System.EventArgs) AS VOID
			SUPER:OnResize(e)
			SELF:Resize()

		// ── B-4: Paint ───────────────────────────────────────────────────────
		PRIVATE _VFPPaint AS VFPOverride
		/// <summary>
		/// Name of the VFP method called when the form is repainted.
		/// </summary>
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.Description("Fires when the form is repainted.")];
		[System.ComponentModel.DefaultValue("")];
		PROPERTY vfpPaint AS STRING GET _VFPPaint?:SendTo SET SELF:_VFPPaint := VFPOverride{SELF, VALUE}

		/// <summary>
		/// VFP Paint virtual stub — calls the <see cref="vfpPaint"/> handler.
		/// </summary>
		NEW METHOD Paint() AS VOID
			IF SELF:_VFPPaint != NULL
				SELF:_VFPPaint:Call()
			ENDIF

		PROTECTED OVERRIDE METHOD OnPaint(e AS System.Windows.Forms.PaintEventArgs) AS VOID
			SUPER:OnPaint(e)
			SELF:Paint()

		/// <summary>
		/// Controls form visibility. Setting to <c>.T.</c> calls <see cref="Show"/> (which respects
		/// <see cref="ShowWindow"/> and <see cref="WindowType"/>); setting to <c>.F.</c> hides the form
		/// via the base <c>Visible</c> setter. No-op when the value does not change.
		/// </summary>
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
