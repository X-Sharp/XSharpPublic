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

		PRIVATE _caption     AS STRING
		PRIVATE _popup       AS Popup
		PRIVATE _vfpClick    AS VFPOverride
		PRIVATE _clickBlock  AS USUAL

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

		// ── Skip ──────────────────────────────────────────────────────────────
		// VFP SET SKIP OF PAD .T. = disabled; maps to !Enabled.
		PROPERTY Skip AS LOGIC
			GET ; RETURN !SELF:Enabled ; END GET
			SET ; SELF:Enabled := !VALUE ; END SET
		END PROPERTY

		// ── vfpClick — VFPOverride event ──────────────────────────────────────
		// Used when the pad has a direct action (no Popup). Mirrors Bar.vfpClick.
		[System.ComponentModel.Category("VFP Events"),System.ComponentModel.Description("Fires when the user clicks this pad (only when no Popup is assigned)")];
		[System.ComponentModel.DefaultValue(NULL)];
		PROPERTY vfpClick AS STRING
			GET
				RETURN SELF:_vfpClick?:SendTo
			END GET
			SET
				SELF:Set_Click( VFPOverride{SELF, VALUE} )
			END SET
		END PROPERTY

		METHOD Set_Click( methodCall AS VFPOverride ) AS VOID
			SELF:Click += System.EventHandler{ SELF, @OnVFPClick() }
			SELF:_vfpClick := methodCall

		PRIVATE METHOD OnVFPClick( sender AS OBJECT, e AS System.EventArgs ) AS VOID
			IF SELF:_vfpClick != NULL
				LOCAL sMethod AS STRING
				sMethod := SELF:_vfpClick:SendTo
				// Prefer the method on the Pad subclass itself; fall back to the owner Menu.
				IF IsMethod( SELF, sMethod )
					SELF:_vfpClick:Call()
				ELSEIF SELF:Owner IS Menu VAR oMenu .AND. IsMethod( oMenu, sMethod )
					Send( oMenu, sMethod )
				ENDIF
			ENDIF

		// ── SetClickAction ────────────────────────────────────────────────────
		// Stores a codeblock as the click handler. Used by VFPMenuSupport.xh
		// UDCs when the ON SELECTION PAD command is arbitrary code.
		METHOD SetClickAction( block AS USUAL ) AS VOID
			SELF:Click += System.EventHandler{ SELF, @OnBlockClick() }
			SELF:_clickBlock := block

		PRIVATE METHOD OnBlockClick( sender AS OBJECT, e AS System.EventArgs ) AS VOID
			IF SELF:_clickBlock != NIL
				Eval( SELF:_clickBlock )
			ENDIF

		// ── Lifecycle stubs ───────────────────────────────────────────────────
		VIRTUAL METHOD Init() AS USUAL CLIPPER
			RETURN NIL

		VIRTUAL METHOD Destroy() AS USUAL CLIPPER
			RETURN NIL

	END CLASS

END NAMESPACE
