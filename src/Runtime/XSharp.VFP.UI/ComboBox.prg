// ComboBox.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.


USING System
USING System.Collections.Generic
USING System.Text
USING System.Windows.Forms
USING System.ComponentModel
USING System.Drawing

BEGIN NAMESPACE XSharp.VFP.UI

	/// <summary>
	/// VFP-compatible combo-box control that wraps <see cref="System.Windows.Forms.ComboBox"/>.<br/>
	/// Adds VFP list-population: <see cref="RowSourceType"/> (0=programmatic, 1=CSV string, 5=array)
	/// and <see cref="RowSource"/> / <see cref="SetRowSourceArray"/> drive <c>Items</c> automatically.
	/// Exposes <see cref="Style"/> (0=editable dropdown, 1=simple always-open, 2=non-editable list),
	/// <see cref="ReadOnly"/> (forces <c>DropDownList</c> without disabling the control),
	/// <see cref="DisplayCount"/> (max drop-down rows), <see cref="ListCount"/>,
	/// <see cref="ListIndex"/> (1-based), <see cref="ListItem"/>, <see cref="AddItem"/>,
	/// <see cref="RemoveItem"/>, <see cref="Clear"/>, <see cref="Requery"/>,
	/// <see cref="Value"/> (selected text / typed text as USUAL), <see cref="DisplayValue"/>,
	/// <see cref="SelectOnEntry"/>, and <see cref="SelStart"/> / <see cref="SelLength"/> /
	/// <see cref="SelText"/>.<br/>
	/// Events: <see cref="vfpDropDown"/>, <see cref="vfpProgrammaticChange"/>,
	/// plus the standard VFP events from the shared includes.
	/// </summary>
	PARTIAL CLASS ComboBox INHERIT System.Windows.Forms.ComboBox

		// Common properties that all VFP Objects support
		#include "Headers/VFPObject.xh"

#include "VFPProperties.xh"

		CONSTRUCTOR(  ) STRICT
            SUPER()
            SELF:Size := Size{100,24}
			RETURN

#include "ControlProperties.xh"

#include "FontProperties.xh"

#include "ControlSource.xh"

		/// <summary>Maximum number of items visible in the open drop-down list. Maps to <see cref="System.Windows.Forms.ComboBox.MaxDropDownItems"/>.</summary>
		PROPERTY DisplayCount AS LONG
			GET
				RETURN SELF:MaxDropDownItems
			END GET
			SET
				SELF:MaxDropDownItems := (INT) VALUE
			END SET
		END PROPERTY
		/// <summary>VFP InputMask — stored for source compatibility; ComboBox editing uses the raw text box.</summary>
		PROPERTY InputMask AS STRING AUTO
		/// <summary>String displayed when the selected value is <c>NULL</c> — stored for source compatibility.</summary>
		PROPERTY NullDisplay AS String AUTO
		/// <summary>VFP OLEDropTextInsertion stub — stored for source compatibility.</summary>
		PROPERTY OLEDropTextInsertion AS LONG AUTO
		/// <summary>VFP Picture stub — stored for source compatibility.</summary>
		PROPERTY Picture AS STRING AUTO
		/// <summary>VFP PictureSelectionDisplay stub — stored for source compatibility.</summary>
		PROPERTY PictureSelectionDisplay  AS LONG AUTO

		/// <summary>
		/// When <c>.T.</c>, forces <c>DropDownStyle = DropDownList</c> so the user cannot type freely.
		/// The previous style is saved and restored when set back to <c>.F.</c>.
		/// </summary>
		PRIVATE _savedDropDownStyle AS ComboBoxStyle

		PROPERTY ReadOnly AS LOGIC
			GET
				RETURN SELF:DropDownStyle == ComboBoxStyle.DropDownList
			END GET
			SET
				IF VALUE
					_savedDropDownStyle := SELF:DropDownStyle
					SELF:DropDownStyle := ComboBoxStyle.DropDownList
				ELSE
					SELF:DropDownStyle := _savedDropDownStyle
				ENDIF
			END SET
		END PROPERTY

		/// <summary>
		/// VFP combo-box style:<br/>
		/// 0 = editable dropdown (<c>DropDown</c>); 1 = always-open list with editable text (<c>Simple</c>);
		/// 2 = non-editable dropdown list (<c>DropDownList</c>).<br/>
		/// Maps to <see cref="System.Windows.Forms.ComboBox.DropDownStyle"/>.
		/// </summary>
		PROPERTY Style AS LONG
			GET
				DO CASE
				CASE SELF:DropDownStyle == ComboBoxStyle.DropDownList
					RETURN 2
				CASE SELF:DropDownStyle == ComboBoxStyle.Simple
					RETURN 1
				OTHERWISE
					RETURN 0
				END CASE
			END GET
			SET
				DO CASE
				CASE VALUE == 2
					SELF:DropDownStyle := ComboBoxStyle.DropDownList
				CASE VALUE == 1
					SELF:DropDownStyle := ComboBoxStyle.Simple
				OTHERWISE
					SELF:DropDownStyle := ComboBoxStyle.DropDown
				END CASE
			END SET
		END PROPERTY
		/// <summary>Number of selected characters in the combo's text box.</summary>
		PROPERTY SelLength AS LONG GET SELF:SelectionLength SET SELF:SelectionLength := VALUE
		/// <summary>Zero-based caret position in the combo's text box.</summary>
		PROPERTY SelStart  AS LONG GET SELF:SelectionStart  SET SELF:SelectionStart  := VALUE
		/// <summary>Currently selected text in the combo's text box.</summary>
		PROPERTY SelText   AS STRING GET SELF:SelectedText  SET SELF:SelectedText    := VALUE
		/// <summary>When <c>.T.</c>, all text in the combo is selected when the control receives focus.</summary>
		PROPERTY SelectOnEntry AS LOGIC AUTO

		// ── RowSource / RowSourceType ────────────────────────────────────────
		/// <summary>
		/// How the combo's item list is populated:<br/>
		/// 0=programmatic (<see cref="AddItem"/>); 1=CSV string in <see cref="RowSource"/>;
		/// 5=.NET array supplied via <see cref="SetRowSourceArray"/>.<br/>
		/// Types 2–4, 6–9 (data environment sources) are not yet implemented.
		/// Changing this property immediately re-populates the list.
		/// </summary>
		PRIVATE _rowSource AS STRING
		PRIVATE _rowSourceType AS LONG
		PRIVATE _rowSourceArray AS System.Array

		PROPERTY RowSourceType AS LONG
			GET
				RETURN _rowSourceType
			END GET
			SET
				_rowSourceType := VALUE
				SELF:ApplyRowSource()
			END SET
		END PROPERTY

		/// <summary>Item source string. For <see cref="RowSourceType"/> 1, a comma-separated list of values loaded directly into <c>Items</c>.</summary>
		PROPERTY RowSource AS STRING
			GET
				RETURN _rowSource
			END GET
			SET
				_rowSource := VALUE
				SELF:ApplyRowSource()
			END SET
		END PROPERTY

		/// <summary>Supplies a .NET array as the item source for <see cref="RowSourceType"/>=5. If <c>RowSourceType</c> is already 5, the list is repopulated immediately.</summary>
		METHOD SetRowSourceArray( arr AS System.Array ) AS VOID
			SELF:_rowSourceArray := arr
			IF SELF:_rowSourceType == 5
				SELF:ApplyRowSource()
			ENDIF

		/// <summary>Clears and repopulates <c>Items</c> from <see cref="RowSource"/> / <see cref="SetRowSourceArray"/> according to the current <see cref="RowSourceType"/>. Called automatically when either property changes.</summary>
		PRIVATE METHOD ApplyRowSource() AS VOID
			DO CASE
			CASE _rowSourceType == 1 .AND. !String.IsNullOrEmpty(_rowSource)
				// RowSourceType 1: comma-delimited value list
				SELF:Items:Clear()
				VAR parts := _rowSource:Split( <CHAR>{ Char.Parse(",") } )
				FOREACH VAR part IN parts
					SELF:Items:Add( part:Trim() )
				NEXT
			CASE _rowSourceType == 5 .AND. SELF:_rowSourceArray != NULL_OBJECT
				// RowSourceType 5: array — load from SetRowSourceArray()
				SELF:Items:Clear()
				FOREACH VAR item IN SELF:_rowSourceArray
					IF item != NULL_OBJECT
						SELF:Items:Add( item:ToString() )
					ENDIF
				NEXT
			// RowSourceType 0 = programmatic (AddItem); nothing to do here
			// Types 2/3/4/6/7/8/9 require data environment — not yet implemented
			END CASE
		END METHOD

		// ── ListCount / ListIndex ────────────────────────────────────────────
		/// <summary>Number of items in the combo's list.</summary>
		PROPERTY ListCount AS LONG GET SELF:Items:Count

		/// <summary>1-based index of the selected item, or 0 when nothing is selected. Maps to <c>SelectedIndex + 1</c>.</summary>
		PROPERTY ListIndex AS LONG
			GET
				RETURN SELF:SelectedIndex + 1  // VFP is 1-based
			END GET
			SET
				SELF:SelectedIndex := VALUE - 1
			END SET
		END PROPERTY

		// ── AddItem / RemoveItem / Clear / Requery ───────────────────────────
		/// <summary>Inserts <c>cItem</c> at the optional 1-based <c>nIndex</c> position, or appends it when <c>nIndex</c> is out of range.</summary>
		METHOD AddItem(cItem , nIndex , nColumn) AS VOID  CLIPPER
			IF nIndex > 0 .AND. nIndex <= SELF:Items:Count + 1
				SELF:Items:Insert( nIndex - 1, cItem )
			ELSE
				SELF:Items:Add( cItem )
			ENDIF
		END METHOD

		/// <summary>Removes the item at the given 1-based <c>nIndex</c>. Silently ignored when out of range.</summary>
		METHOD RemoveItem( nIndex AS LONG ) AS VOID
			IF nIndex > 0 .AND. nIndex <= SELF:Items:Count
				SELF:Items:RemoveAt( nIndex - 1 )
			ENDIF
		END METHOD

		/// <summary>Removes all items from the list.</summary>
		METHOD Clear() AS VOID CLIPPER
			SELF:Items:Clear()
		END METHOD

		/// <summary>Re-runs <see cref="ApplyRowSource"/> to reload items from the current <see cref="RowSource"/> / array.</summary>
		METHOD Requery() AS VOID STRICT
			SELF:ApplyRowSource()
		END METHOD

		/// <summary>
		/// The current value as a VFP USUAL — the selected item's display text, or the typed text
		/// when no item is selected.<br/>
		/// Setting selects the matching item by text; if not found, sets the raw text.
		/// Fires <see cref="vfpProgrammaticChange"/> on assignment.
		/// </summary>
		PRIVATE _isProgrammatic   AS LOGIC

		PROPERTY Value AS USUAL
			GET
				IF SELF:SelectedItem != NULL
					RETURN (USUAL) SELF:SelectedItem:ToString()
				ENDIF
				RETURN (USUAL) SELF:Text
			END GET
			SET
				LOCAL cVal AS STRING
				cVal := Str(VALUE)
				_isProgrammatic := TRUE
				VAR idx := SELF:Items:IndexOf( cVal )
				IF idx >= 0
					SELF:SelectedIndex := idx
				ELSE
					SELF:Text := cVal
				ENDIF
				_isProgrammatic := FALSE
				SELF:OnVFPProgrammaticChange()
			END SET
		END PROPERTY

		/// <summary>Display text of the currently selected item. Getter is identical to <see cref="Value"/>; setter delegates to <see cref="Value"/>. Implements <c>IVFPList.DisplayValue</c>.</summary>
		PROPERTY DisplayValue AS USUAL
			GET
				IF SELF:SelectedItem != NULL
					RETURN (USUAL) SELF:SelectedItem:ToString()
				ENDIF
				RETURN (USUAL) SELF:Text
			END GET
			SET
				SELF:Value := VALUE
			END SET
		END PROPERTY

		/// <summary>Returns the display text of the item at the given 1-based index, or an empty string when out of range.</summary>
		METHOD ListItem(nIndex AS LONG) AS STRING
			IF nIndex >= 1 .AND. nIndex <= SELF:Items:Count
				RETURN SELF:Items[nIndex - 1]:ToString()
			ENDIF
			RETURN ""

		// ── ProgrammaticChange ───────────────────────────────────────────────
		PRIVATE _VFPProgrammaticChange AS VFPOverride
		[Category("VFP Events"), Description("Occurs when the value of a control is changed through code.")];
		[DefaultValue(NULL)];
		/// <summary>Name of the VFP method called when <see cref="Value"/> is set programmatically.</summary>
		PROPERTY vfpProgrammaticChange AS STRING GET _VFPProgrammaticChange?:SendTo SET SELF:_VFPProgrammaticChange := VFPOverride{SELF, VALUE}

		PRIVATE METHOD OnVFPProgrammaticChange() AS VOID
			IF SELF:_VFPProgrammaticChange != NULL
				SELF:_VFPProgrammaticChange:Call()
			ENDIF

		// ── DropDown event ───────────────────────────────────────────────────
		PRIVATE _VFPDropDown AS VFPOverride
		[System.ComponentModel.Category("VFP Events"),System.ComponentModel.DefaultValue("")];
		/// <summary>Name of the VFP method called when the combo's drop-down list opens.</summary>
		PROPERTY vfpDropDown AS STRING GET _VFPDropDown?:SendTo SET Set_DropDown( VFPOverride{SELF, VALUE} )

		METHOD Set_DropDown( methodCall AS VFPOverride ) AS VOID
			SELF:_VFPDropDown := methodCall

		PROTECTED OVERRIDE METHOD OnDropDown(e AS System.EventArgs) AS VOID
			SUPER:OnDropDown(e)
			SELF:FireDropDown()

		VIRTUAL METHOD FireDropDown() AS VOID
			IF SELF:_VFPDropDown != NULL
				SELF:_VFPDropDown:Call()
			ENDIF

		// WinForms ComboBox does not raise KeyPress automatically for the edit portion.
		// Override here so vfpKeyPress subscribers fire correctly.
		PROTECTED OVERRIDE METHOD OnKeyPress(e AS System.Windows.Forms.KeyPressEventArgs) AS VOID
			SELF:OnVFPKeyPress(SELF, e)
			SUPER:OnKeyPress(e)

		// ── SelectOnEntry ────────────────────────────────────────────────────

		PROTECTED OVERRIDE METHOD OnGotFocus(e AS System.EventArgs) AS VOID
			SUPER:OnGotFocus(e)
			IF SELF:SelectOnEntry
				SELF:SelectionStart := 0
				SELF:SelectionLength := SELF:Text:Length
			ENDIF

		// ── DisabledBackColor / DisabledForeColor ────────────────────────────
		// Declared in VFPItems.xh (via ComboBox.generated.prg) — no re-declaration needed here.

		PROTECTED OVERRIDE METHOD OnEnabledChanged(e AS System.EventArgs) AS VOID
			SUPER:OnEnabledChanged(e)
			IF !SELF:Enabled
				IF SELF:DisabledBackColor != System.Drawing.Color.Empty
					SELF:BackColor := SELF:DisabledBackColor
				ENDIF
				IF SELF:DisabledForeColor != System.Drawing.Color.Empty
					SELF:ForeColor := SELF:DisabledForeColor
				ENDIF
			ELSE
				SELF:ResetBackColor()
				SELF:ResetForeColor()
			ENDIF
		END METHOD

		// ── InteractiveChange ────────────────────────────────────────────────
		// OnTextChanged fires InteractiveChange for both user typing and item
		// selection. _isProgrammatic gates out programmatic Value assignments.

		PROTECTED OVERRIDE METHOD OnTextChanged( e AS System.EventArgs ) AS VOID
			SUPER:OnTextChanged( e )
			IF !_isProgrammatic
				SELF:OnVFPInteractiveChange( SELF, e )
			ENDIF
		END METHOD

	END CLASS
END NAMESPACE // XSharp.VFP.UI
