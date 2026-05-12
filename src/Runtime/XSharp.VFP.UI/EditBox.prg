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
	/// The VFP compatible EditBox class.
	/// </summary>
	PARTIAL CLASS EditBox INHERIT TextBox

		/// <summary>
		/// When .T., inserts CHR(10) after each CHR(13) in the text whenever Value
		/// is read — matching VFP behaviour where the Value property returns CRLF pairs.
		/// </summary>
		PROPERTY AddLineFeeds AS LOGIC AUTO

		/// <summary>
		/// When .T., pressing Tab inserts a tab character instead of moving focus
		/// to the next control.
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

		// ── AddLineFeeds — override Value getter to normalise line endings ─────
		// VFP AddLineFeeds: Value returns text with CRLF (CR+LF) pairs.
		// The underlying storage uses bare CR (WinForms multiline uses CRLF natively,
		// but VFP ControlSource data often arrives with bare CRs).
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

		// ── ScrollBars — expose as settable VFP property ──────────────────────
		// VFP: 0=None, 1=Horizontal, 2=Vertical, 3=Both
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
