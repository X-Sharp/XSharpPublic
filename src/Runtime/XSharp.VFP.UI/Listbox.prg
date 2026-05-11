// Listbox.prg
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
	/// The VFP compatible Listbox class.
	/// </summary>
	PARTIAL CLASS ListBox INHERIT System.Windows.Forms.ListBox
		// Common properties that all VFP Objects support
#include "Headers/VFPObject.xh"

#include "VFPProperties.xh"

		CONSTRUCTOR(  ) STRICT
            SUPER()
            SELF:Size := Size{100,170}

			RETURN

#include "ControlProperties.xh"

#include "FontProperties.xh"

#include "ControlSource.xh"

		// C-11: was LONG — must be LOGIC to match VFP type
		PROPERTY AutoHideScrollBar AS LOGIC AUTO
		// VFP Style: 0=standard list, 1=checkbox list (no direct WinForms ListBox equivalent — stub only)
		PROPERTY Style AS LONG AUTO
		// C-10: MoverBars / MoveItem — UI drag-reorder; requires owner-draw overlay (not yet implemented)
		PROPERTY MoverBars AS LOGIC AUTO
		METHOD MoveItem(nFrom AS LONG, nTo AS LONG) AS VOID
			// NOT IMPLEMENTED — full implementation requires owner-draw item reordering
			NOP
		END METHOD
		PROPERTY NullDisplay AS String AUTO
		PROPERTY Picture AS STRING AUTO

		// ── MultiSelect ──────────────────────────────────────────────────────
		// VFP MultiSelect: 0=none, 1=standard (simple toggle), 2=extended (shift/ctrl)
		// Maps to WinForms SelectionMode: One, MultiSimple, MultiExtended

		PROPERTY MultiSelect AS LONG
			GET
				DO CASE
				CASE SELF:SelectionMode == SelectionMode.MultiSimple
					RETURN 1
				CASE SELF:SelectionMode == SelectionMode.MultiExtended
					RETURN 2
				OTHERWISE
					RETURN 0
				END CASE
			END GET
			SET
				DO CASE
				CASE VALUE == 1
					SELF:SelectionMode := SelectionMode.MultiSimple
				CASE VALUE == 2
					SELF:SelectionMode := SelectionMode.MultiExtended
				OTHERWISE
					SELF:SelectionMode := SelectionMode.One
				END CASE
			END SET
		END PROPERTY

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

		PRIVATE _isProgrammatic AS LOGIC

		PROPERTY Value AS USUAL
			GET
				IF SELF:SelectedItem != NULL
					RETURN (USUAL) SELF:SelectedItem:ToString()
				ENDIF
				RETURN (USUAL) ""
			END GET
			SET
				LOCAL cVal AS STRING
				cVal := Str(VALUE)
				VAR idx := SELF:Items:IndexOf( cVal )
				IF idx >= 0
					_isProgrammatic := TRUE
					SELF:SelectedIndex := idx
					_isProgrammatic := FALSE
					SELF:OnVFPProgrammaticChange()
				ENDIF
			END SET
		END PROPERTY

		// ── DisplayValue ─────────────────────────────────────────────────────
		// Implements DisplayValue from IVFPList — returns the selected item text.
		PROPERTY DisplayValue AS USUAL
			GET
				IF SELF:SelectedItem != NULL
					RETURN (USUAL) SELF:SelectedItem:ToString()
				ENDIF
				RETURN (USUAL) ""
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

		// ── Selected ─────────────────────────────────────────────────────────
		// VFP Selected(nIndex): returns .T. if item at 1-based index is selected.
		METHOD Selected(nIndex AS LONG) AS LOGIC
			IF nIndex >= 1 .AND. nIndex <= SELF:Items:Count
				RETURN SELF:GetSelected(nIndex - 1)
			ENDIF
			RETURN FALSE

		// ── ProgrammaticChange ───────────────────────────────────────────────
		PRIVATE _VFPProgrammaticChange AS VFPOverride
		[Category("VFP Events"), Description("Occurs when the value of a control is changed through code.")];
		[DefaultValue(NULL)];
		PROPERTY vfpProgrammaticChange AS STRING GET _VFPProgrammaticChange?:SendTo SET SELF:_VFPProgrammaticChange := VFPOverride{SELF, VALUE}

		PRIVATE METHOD OnVFPProgrammaticChange() AS VOID
			IF SELF:_VFPProgrammaticChange != NULL
				SELF:_VFPProgrammaticChange:Call()
			ENDIF

		// ── DisabledBackColor / DisabledForeColor ────────────────────────────

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

		// WinForms ListBox does not raise KeyPress automatically.
		// Override so vfpKeyPress subscribers fire correctly.
		PROTECTED OVERRIDE METHOD OnKeyPress(e AS System.Windows.Forms.KeyPressEventArgs) AS VOID
			SELF:OnVFPKeyPress(SELF, e)
			SUPER:OnKeyPress(e)

		// ── InteractiveChange ────────────────────────────────────────────────

		PROTECTED METHOD OnSelectedIndexChanged( e AS System.EventArgs ) AS VOID
			SUPER:OnSelectedIndexChanged( e )
			IF !_isProgrammatic
				SELF:OnVFPInteractiveChange( SELF, e )
			ENDIF
		END METHOD

	END CLASS
END NAMESPACE // XSharp.VFP.UI
