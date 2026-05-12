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
    /// The VFP compatible ComboBox class.
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

		PROPERTY DisplayCount AS LONG
			GET
				RETURN SELF:MaxDropDownItems
			END GET
			SET
				SELF:MaxDropDownItems := (INT) VALUE
			END SET
		END PROPERTY
		PROPERTY InputMask AS STRING AUTO
		PROPERTY NullDisplay AS String AUTO
		PROPERTY OLEDropTextInsertion AS LONG AUTO

		PROPERTY Picture AS STRING AUTO
		PROPERTY PictureSelectionDisplay  AS LONG AUTO
		// ── ReadOnly ─────────────────────────────────────────────────────────
		// .T. → force DropDownList (non-editable); .F. → restore previous style.

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

		// ── Style ────────────────────────────────────────────────────────────
		// VFP Style: 0=dropdown combo (editable+list), 1=text only, 2=dropdown list (non-editable)
		// Maps to WinForms DropDownStyle: DropDown=editable, Simple=always-open, DropDownList=non-editable

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
		PROPERTY SelLength AS LONG GET SELF:SelectionLength SET SELF:SelectionLength := VALUE
		PROPERTY SelStart  AS LONG GET SELF:SelectionStart  SET SELF:SelectionStart  := VALUE
		PROPERTY SelText   AS STRING GET SELF:SelectedText  SET SELF:SelectedText    := VALUE
		PROPERTY SelectOnEntry AS LOGIC AUTO

		// ── RowSource / RowSourceType ────────────────────────────────────────
		// Override the AUTO stubs from VFPList.xh with real implementations.
		// RowSourceType: 0=None, 1=Value (CSV), 5=Array — others are TODO.
		// For type 5, populate via SetRowSourceArray() before or after setting RowSourceType.

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

		PROPERTY RowSource AS STRING
			GET
				RETURN _rowSource
			END GET
			SET
				_rowSource := VALUE
				SELF:ApplyRowSource()
			END SET
		END PROPERTY

		/// <summary>Supply a .NET array as the item source for RowSourceType=5.</summary>
		METHOD SetRowSourceArray( arr AS System.Array ) AS VOID
			SELF:_rowSourceArray := arr
			IF SELF:_rowSourceType == 5
				SELF:ApplyRowSource()
			ENDIF

		/// <summary>Populate Items from RowSource according to RowSourceType.</summary>
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

		PROPERTY ListCount AS LONG GET SELF:Items:Count

		PROPERTY ListIndex AS LONG
			GET
				RETURN SELF:SelectedIndex + 1  // VFP is 1-based
			END GET
			SET
				SELF:SelectedIndex := VALUE - 1
			END SET
		END PROPERTY

		// ── AddItem / RemoveItem / Clear / Requery ───────────────────────────

		METHOD AddItem(cItem , nIndex , nColumn) AS VOID  CLIPPER
			IF nIndex > 0 .AND. nIndex <= SELF:Items:Count + 1
				SELF:Items:Insert( nIndex - 1, cItem )
			ELSE
				SELF:Items:Add( cItem )
			ENDIF
		END METHOD

		METHOD RemoveItem( nIndex AS LONG ) AS VOID
			IF nIndex > 0 .AND. nIndex <= SELF:Items:Count
				SELF:Items:RemoveAt( nIndex - 1 )
			ENDIF
		END METHOD

		METHOD Clear() AS VOID CLIPPER
			SELF:Items:Clear()
		END METHOD

		METHOD Requery() AS VOID STRICT
			SELF:ApplyRowSource()
		END METHOD

		// ── Value ────────────────────────────────────────────────────────────
		// In VFP, Value holds the selected item text (or numeric index depending
		// on Style). We implement the most common case: string of selected item.

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

		// ── DisplayValue ─────────────────────────────────────────────────────
		// Implements DisplayValue from IVFPList — returns the selected item text.
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

		// ── ListItem ─────────────────────────────────────────────────────────
		// VFP ListItem(nIndex): returns display text of item at 1-based index.
		METHOD ListItem(nIndex AS LONG) AS STRING
			IF nIndex >= 1 .AND. nIndex <= SELF:Items:Count
				RETURN SELF:Items[nIndex - 1]:ToString()
			ENDIF
			RETURN ""

		// ── ProgrammaticChange ───────────────────────────────────────────────
		PRIVATE _VFPProgrammaticChange AS VFPOverride
		[Category("VFP Events"), Description("Occurs when the value of a control is changed through code.")];
		[DefaultValue(NULL)];
		PROPERTY vfpProgrammaticChange AS STRING GET _VFPProgrammaticChange?:SendTo SET SELF:_VFPProgrammaticChange := VFPOverride{SELF, VALUE}

		PRIVATE METHOD OnVFPProgrammaticChange() AS VOID
			IF SELF:_VFPProgrammaticChange != NULL
				SELF:_VFPProgrammaticChange:Call()
			ENDIF

		// ── DropDown event ───────────────────────────────────────────────────

		PRIVATE _VFPDropDown AS VFPOverride
		[System.ComponentModel.Category("VFP Events"),System.ComponentModel.DefaultValue("")];
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
