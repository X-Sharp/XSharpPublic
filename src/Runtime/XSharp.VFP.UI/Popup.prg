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

		PRIVATE _bars AS List<Bar>

		CONSTRUCTOR() STRICT
			SUPER()
			SELF:_bars := List<Bar>{}

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

		// ── Lifecycle stubs ───────────────────────────────────────────────────
		VIRTUAL METHOD Init() AS USUAL CLIPPER
			RETURN NIL

		VIRTUAL METHOD Destroy() AS USUAL CLIPPER
			RETURN NIL

	END CLASS

END NAMESPACE
