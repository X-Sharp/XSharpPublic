// Popup.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.

USING System
USING System.Collections.Generic
USING System.Windows.Forms

BEGIN NAMESPACE XSharp.VFP.UI

	/// <summary>
	/// VFP-compatible Popup class. Represents a dropdown menu attached to a Pad.
	/// Maps to System.Windows.Forms.ToolStripDropDownMenu.
	/// Add items via AddBar(). A caption of "--" inserts a separator.
	/// </summary>
	PARTIAL CLASS Popup INHERIT System.Windows.Forms.ToolStripDropDownMenu

		PRIVATE STATIC _registry AS Dictionary<STRING, Popup>

		PRIVATE _bars AS List<Bar>

		CONSTRUCTOR() STRICT
			SUPER()
			SELF:_bars := List<Bar>{}

		// ── Named registry ────────────────────────────────────────────────────
		// Popups register themselves by Name so MRKBAR/PRMBAR can look them up.
		NEW PROPERTY Name AS STRING
			GET ; RETURN SUPER:Name ; END GET
			SET
				IF !String.IsNullOrEmpty(SUPER:Name) .AND. _registry != NULL
					_registry:Remove(SUPER:Name)
				ENDIF
				SUPER:Name := VALUE
				IF !String.IsNullOrEmpty(VALUE)
					IF _registry == NULL
						_registry := Dictionary<STRING, Popup>{}
					ENDIF
					_registry[VALUE] := SELF
				ENDIF
			END SET
		END PROPERTY

		STATIC METHOD Find( cName AS STRING ) AS Popup
			IF _registry != NULL .AND. _registry:ContainsKey(cName)
				RETURN _registry[cName]
			ENDIF
			RETURN NULL

		// Returns a snapshot of all currently registered popup names.
		STATIC METHOD GetAllNames() AS List<STRING>
			IF _registry == NULL
				RETURN List<STRING>{}
			ENDIF
			RETURN List<STRING>{ _registry:Keys }

		// Releases every registered popup (used by RELEASE POPUPS with no list).
		STATIC METHOD ReleaseAll() AS VOID
			IF _registry == NULL
				RETURN
			ENDIF
			LOCAL values AS List<Popup>
			values := List<Popup>{ _registry:Values }
			FOREACH VAR oPopup IN values
				oPopup:Release()
			NEXT

		// ── BarCount ──────────────────────────────────────────────────────────
		// Number of Bar items (separators are not counted).
		PROPERTY BarCount AS LONG
			GET
				RETURN (LONG) SELF:_bars:Count
			END GET
		END PROPERTY

		// ── Bars[i] ───────────────────────────────────────────────────────────
		// 1-based indexed access to Bar items (separators excluded).
		PROPERTY Bars[ i AS LONG ] AS Bar
			GET
				RETURN SELF:_bars[ (INT) i - 1 ]
			END GET
		END PROPERTY

		// ── AddBar ────────────────────────────────────────────────────────────
		// cCaption "--" inserts a ToolStripSeparator; anything else creates a Bar.
		METHOD AddBar( cName, cCaption ) AS USUAL CLIPPER
			LOCAL sName    AS STRING
			LOCAL sCaption AS STRING
			sName    := (STRING) cName
			sCaption := (STRING) cCaption
			IF sCaption == "--"
				LOCAL sep AS ToolStripSeparator
				sep      := ToolStripSeparator{}
				sep:Name := sName
				SELF:Items:Add( sep )
			ELSE
				LOCAL oBar AS Bar
				oBar         := Bar{}
				oBar:Name    := sName
				oBar:Caption := sCaption
				SELF:Items:Add( oBar )
				SELF:_bars:Add( oBar )
				SELF:AddProperty( sName, oBar, PropertyVisibility.Public, "AddBar" )
			ENDIF
			RETURN NIL

		// ── Skip ──────────────────────────────────────────────────────────────
		// VFP SET SKIP OF POPUP .T. = disabled.
		PROPERTY Skip AS LOGIC
			GET ; RETURN !SELF:Enabled ; END GET
			SET ; SELF:Enabled := !VALUE ; END SET
		END PROPERTY

		// ── IndexOf ───────────────────────────────────────────────────────────
		// Returns the 1-based Bar index of the given bar (0 = not found).
		// Used by Bar.OnVFPClick to populate MenuState.LastBar.
		INTERNAL METHOD IndexOf( bar AS Bar ) AS INT
			LOCAL idx := SELF:_bars:IndexOf(bar) AS INT
			RETURN IIF( idx < 0, 0, idx + 1 )

		// ── Release ───────────────────────────────────────────────────────────
		METHOD Release() AS USUAL CLIPPER
			IF !String.IsNullOrEmpty(SELF:Name) .AND. _registry != NULL
				_registry:Remove(SELF:Name)
			ENDIF
			SELF:Dispose()
			RETURN NIL

		// ── Lifecycle stubs ───────────────────────────────────────────────────
		VIRTUAL METHOD Init() AS USUAL CLIPPER
			RETURN NIL

		VIRTUAL METHOD Destroy() AS USUAL CLIPPER
			RETURN NIL

	END CLASS

END NAMESPACE
