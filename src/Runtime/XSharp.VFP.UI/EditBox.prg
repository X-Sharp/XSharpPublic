// EditBox.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.

USING System
USING System.Collections.Generic
USING System.Text
USING System.Windows.Forms
USING System.Drawing
USING System.ComponentModel

BEGIN NAMESPACE XSharp.VFP.UI
	/// <summary>
	/// VFP-compatible multi-line text editing control.<br/>
	/// Inherits from <see cref="TextBox"/> (XSharp.VFP.UI) — all TextBox properties and Format codes
	/// (including K, Z, F, T, L, A, !) are available automatically.<br/>
	/// Adds VFP-specific multi-line behaviour: <see cref="AddLineFeeds"/> for CRLF normalisation,
	/// <see cref="AllowTabs"/> for in-field tab insertion, and a VFP-style <see cref="ScrollBars"/> property.
	/// </summary>
	PARTIAL CLASS EditBox INHERIT TextBox

		/// <summary>
		/// When <c>.T.</c>, the <see cref="Value"/> getter normalises line endings to CRLF pairs
		/// (CR+LF) before returning the text — matching VFP behaviour where <c>Value</c> always
		/// returns CRLF-delimited content regardless of how the data arrived.
		/// </summary>
		PROPERTY AddLineFeeds AS LOGIC AUTO

		/// <summary>
		/// When <c>.T.</c>, pressing Tab inserts a literal tab character (<c>CHR(9)</c>) at the
		/// caret instead of moving focus to the next control in the tab order.
		/// </summary>
		PROPERTY AllowTabs AS LOGIC AUTO

		CONSTRUCTOR( )
			SUPER()
			SELF:Multiline    := TRUE
			SELF:ScrollBars   := 3           // 3 = Both (VFP convention)
			SELF:Size         := Size{100, 75}
			SELF:AcceptsTab   := FALSE    // controlled by AllowTabs

		// ── AllowTabs — override OnKeyDown to intercept Tab ───────────────────
		OVERRIDE PROTECTED METHOD OnKeyDown( e AS KeyEventArgs ) AS VOID
			IF SELF:AllowTabs .AND. e:KeyCode == Keys.Tab
				// Insert a tab character at the caret
				VAR start  := SELF:SelectionStart
				VAR len    := SELF:SelectionLength
				VAR txt    := SELF:Text
				SELF:Text           := txt:Remove(start, len):Insert(start, Chr(9))
				SELF:SelectionStart := start + 1
				e:Handled           := TRUE
				e:SuppressKeyPress  := TRUE
				RETURN
			ENDIF
			SUPER:OnKeyDown(e)

		/// <summary>
		/// Overrides <see cref="TextBox.Value"/> to apply <see cref="AddLineFeeds"/> normalisation.<br/>
		/// When <c>AddLineFeeds</c> is <c>.T.</c>, all bare CR characters in the raw value are
		/// expanded to CRLF pairs before the value is returned, matching VFP's behaviour where
		/// <c>Value</c> always yields CRLF-delimited memo content.
		/// Setting the value delegates directly to the base <see cref="TextBox.Value"/> setter.
		/// </summary>
		NEW PROPERTY Value AS USUAL
			GET
				VAR raw := (USUAL) SUPER:Value
				IF SELF:AddLineFeeds .AND. raw != NIL
					VAR s := Str(raw)
					// Normalise: first collapse any existing CRLF, then ensure all CR → CRLF
					s := s:Replace(Chr(13) + Chr(10), Chr(13))
					s := s:Replace(Chr(13), Chr(13) + Chr(10))
					RETURN (USUAL) s
				ENDIF
				RETURN raw
			END GET
			SET
				SUPER:Value := VALUE
			END SET
		END PROPERTY

		/// <summary>
		/// VFP ScrollBars setting: 0=None, 1=Horizontal, 2=Vertical, 3=Both (default).<br/>
		/// Maps to <see cref="System.Windows.Forms.TextBox.ScrollBars"/>. Values outside 0–3 are ignored.
		/// </summary>
		NEW PROPERTY ScrollBars AS LONG
			GET
				RETURN (LONG) SUPER:ScrollBars
			END GET
			SET
				IF VALUE >= 0 .AND. VALUE <= 3
					SUPER:ScrollBars := (System.Windows.Forms.ScrollBars) VALUE
				ENDIF
			END SET
		END PROPERTY

	END CLASS

END NAMESPACE
