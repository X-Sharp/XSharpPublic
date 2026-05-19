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
	/// VFP-compatible list box control that wraps <see cref="System.Windows.Forms.ListBox"/>.<br/>
	/// Adds VFP list-population: <see cref="RowSourceType"/> (0=programmatic, 1=CSV string, 5=array)
	/// and <see cref="RowSource"/> / <see cref="SetRowSourceArray"/> drive <c>Items</c> automatically.
	/// Exposes <see cref="MultiSelect"/> (0=single, 1=simple toggle, 2=extended shift/ctrl),
	/// <see cref="ListCount"/>, <see cref="ListIndex"/> (1-based), <see cref="ListItem"/>,
	/// <see cref="Selected"/>, <see cref="AddItem"/>, <see cref="RemoveItem"/>, <see cref="Clear"/>,
	/// <see cref="Requery"/>, <see cref="Value"/> (selected item text as USUAL),
	/// and <see cref="DisplayValue"/>.<br/>
	/// Events: <see cref="vfpProgrammaticChange"/> plus the standard VFP events from the shared includes.
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

		/// <summary>When <c>.T.</c>, the vertical scroll bar hides automatically when all items fit. Maps to <c>ScrollAlwaysVisible</c> (inverted).</summary>
		PROPERTY AutoHideScrollBar AS LOGIC AUTO
		/// <summary>VFP Style: 0=standard list, 1=checkbox list. Checkbox style has no direct WinForms <c>ListBox</c> equivalent — stored for source compatibility.</summary>
		PROPERTY Style AS LONG AUTO
		/// <summary>VFP MoverBars — drag-to-reorder UI; no WinForms <c>ListBox</c> equivalent. Stored for source compatibility.</summary>
		PROPERTY MoverBars AS LOGIC AUTO
		/// <summary>Moves the item at 1-based <c>nFrom</c> to 1-based <c>nTo</c>. Not yet implemented — owner-draw reordering is required.</summary>
		METHOD MoveItem(nFrom AS LONG, nTo AS LONG) AS VOID
			// NOT IMPLEMENTED — full implementation requires owner-draw item reordering
			NOP
		END METHOD
		/// <summary>String displayed when the selected value is <c>NULL</c> — stored for source compatibility.</summary>
		PROPERTY NullDisplay AS String AUTO
		/// <summary>VFP Picture stub — stored for source compatibility.</summary>
		PROPERTY Picture AS STRING AUTO

		/// <summary>
		/// VFP multi-selection mode:<br/>
		/// 0 = single selection (<c>SelectionMode.One</c>);<br/>
		/// 1 = simple toggle on click (<c>MultiSimple</c>);<br/>
		/// 2 = extended Shift/Ctrl selection (<c>MultiExtended</c>).
		/// </summary>
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
		/// <summary>
		/// How the list's items are populated:<br/>
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

		/// <summary>Number of items currently in the list. Read-only.</summary>
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

		// ── Value ────────────────────────────────────────────────────────────

		PRIVATE _isProgrammatic AS LOGIC

		/// <summary>
		/// The currently selected item as a VFP USUAL (item text), or an empty string when nothing is selected.<br/>
		/// Setting converts the value to a string and selects the matching item by text.
		/// Fires <see cref="vfpProgrammaticChange"/> on assignment.
		/// </summary>
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
		/// <summary>Display text of the currently selected item. Identical to <see cref="Value"/>; setter delegates to <see cref="Value"/>. Implements <c>IVFPList.DisplayValue</c>.</summary>
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
		/// <summary>Returns the display text of the item at the given 1-based index, or an empty string when out of range.</summary>
		METHOD ListItem(nIndex AS LONG) AS STRING
			IF nIndex >= 1 .AND. nIndex <= SELF:Items:Count
				RETURN SELF:Items[nIndex - 1]:ToString()
			ENDIF
			RETURN ""

		// ── Selected ─────────────────────────────────────────────────────────
		/// <summary>Returns <c>.T.</c> if the item at the given 1-based index is selected, <c>.F.</c> otherwise. Out-of-range indices return <c>.F.</c>.</summary>
		METHOD Selected(nIndex AS LONG) AS LOGIC
			IF nIndex >= 1 .AND. nIndex <= SELF:Items:Count
				RETURN SELF:GetSelected(nIndex - 1)
			ENDIF
			RETURN FALSE

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
