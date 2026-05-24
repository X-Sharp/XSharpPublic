// Separator.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.

USING System
USING System.Windows.Forms
USING System.ComponentModel

BEGIN NAMESPACE XSharp.VFP.UI

	/// <summary>
	/// The VFP compatible Separator class.
	/// Maps to System.Windows.Forms.ToolStripSeparator.
	/// In VFP, Separator is used inside ToolBars to visually divide groups of buttons.
	/// </summary>
	PARTIAL CLASS Separator INHERIT System.Windows.Forms.ToolStripSeparator

		// Note: VFPObject.xh is included by Separator.generated.prg — do not include again here.

		CONSTRUCTOR() STRICT
			SUPER()

		/// <summary>
		/// Forwards VFP-style construction arguments; args are accepted for source compatibility
		/// but are not passed to the base (ToolStripSeparator has no params constructor).
		/// </summary>
		CONSTRUCTOR(args PARAMS USUAL[])
			SUPER()

		// ── Non-Control property stubs ────────────────────────────────────────
		// ToolStripSeparator inherits ToolStripItem (not Control), so the standard
		// WinForms Control properties used by generated designer code must be stubbed.

		/// <summary>VFP AutoScaleMode stub — stored for source compatibility.</summary>
		PROPERTY AutoScaleMode AS System.Windows.Forms.AutoScaleMode AUTO

		/// <summary>
		/// Position of the separator. Stored for source compatibility;
		/// ToolStripSeparator position is managed by the owning ToolStrip.
		/// </summary>
		PROPERTY Location AS System.Drawing.Point AUTO

		// ── Style ─────────────────────────────────────────────────────────────
		// VFP Separator.Style: 0=Standard, 1=Graphical.
		PRIVATE _style AS LONG
		PROPERTY Style AS LONG
			GET ; RETURN SELF:_style
			END GET
			SET ; SELF:_style := VALUE
			END SET
		END PROPERTY

	END CLASS

END NAMESPACE
