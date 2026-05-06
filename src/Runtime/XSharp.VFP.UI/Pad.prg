// Pad.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.

USING System
USING System.Windows.Forms

BEGIN NAMESPACE XSharp.VFP.UI

	/// <summary>
	/// VFP-compatible Pad class. Represents a top-level item on a Menu bar
	/// (e.g. File, Edit, Help). Maps to System.Windows.Forms.ToolStripMenuItem
	/// hosted directly in a MenuStrip.
	/// Assign a Popup to the Popup property to attach a dropdown.
	/// </summary>
	PARTIAL CLASS Pad INHERIT System.Windows.Forms.ToolStripMenuItem

		PRIVATE _caption AS STRING
		PRIVATE _popup   AS Popup

		CONSTRUCTOR() STRICT
			SUPER()

		// ── Caption ───────────────────────────────────────────────────────────
		// VFP uses \< to mark the accelerator key; WinForms uses &.
		PROPERTY Caption AS STRING
			GET
				RETURN SELF:_caption
			END GET
			SET
				SELF:_caption := VALUE
				SELF:Text := IIF( String.IsNullOrEmpty(VALUE), VALUE, VALUE:Replace("\<", "&") )
			END SET
		END PROPERTY

		// ── Message ───────────────────────────────────────────────────────────
		// Status bar hint shown when the pad is highlighted.
		PROPERTY Message AS STRING AUTO

		// ── Popup ─────────────────────────────────────────────────────────────
		// Attaches a Popup as this pad's dropdown menu.
		PROPERTY Popup AS Popup
			GET
				RETURN SELF:_popup
			END GET
			SET
				SELF:_popup  := VALUE
				SELF:DropDown := VALUE
			END SET
		END PROPERTY

		// ── Lifecycle stubs ───────────────────────────────────────────────────
		VIRTUAL METHOD Init() AS USUAL CLIPPER
			RETURN NIL

		VIRTUAL METHOD Destroy() AS USUAL CLIPPER
			RETURN NIL

	END CLASS

END NAMESPACE
