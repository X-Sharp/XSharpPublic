// Bar.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.

USING System
USING System.Windows.Forms
USING System.Drawing

BEGIN NAMESPACE XSharp.VFP.UI

	/// <summary>
	/// VFP-compatible Bar class. Represents a single menu item (or separator)
	/// inside a Popup. Maps to System.Windows.Forms.ToolStripMenuItem.
	/// A Caption of "--" signals a separator; add it via Popup.AddBar() which
	/// inserts a ToolStripSeparator instead.
	/// </summary>
	PARTIAL CLASS Bar INHERIT System.Windows.Forms.ToolStripMenuItem

		PRIVATE _caption     AS STRING
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
				SELF:Text := IIF( String.IsNullOrEmpty(VALUE), VALUE, __VFPConvertCaption(VALUE) )
			END SET
		END PROPERTY

		// ── Message ───────────────────────────────────────────────────────────
		// Status bar hint text shown when the user highlights this item.
		PROPERTY Message AS STRING AUTO

		// ── Shortcut ──────────────────────────────────────────────────────────
		// Stored as a Keys enum ordinal (LONG); maps directly to ShortcutKeys.
		PROPERTY Shortcut AS LONG
			GET
				RETURN (LONG) SELF:ShortcutKeys
			END GET
			SET
				SELF:ShortcutKeys := (Keys) VALUE
			END SET
		END PROPERTY

		// ── Skip ──────────────────────────────────────────────────────────────
		// VFP SET SKIP OF BAR .T. = disabled; maps to !Enabled.
		PROPERTY Skip AS LOGIC
			GET ; RETURN !SELF:Enabled ; END GET
			SET ; SELF:Enabled := !VALUE ; END SET
		END PROPERTY

		// ── vfpClick — VFPOverride event ──────────────────────────────────────
		[System.ComponentModel.Category("VFP Events"),System.ComponentModel.Description("Fires when the user clicks this menu item")];
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
			// Update global menu state so BAR(), PAD(), POPUP(), MENU() work in handlers
			IF SELF:Owner IS Popup VAR oParentPopup
				MenuState.LastBar   := oParentPopup:IndexOf( SELF )
				MenuState.LastPopup := oParentPopup:Name
				IF oParentPopup:OwnerItem IS Pad VAR oPadOwner
					MenuState.LastPad := oPadOwner:Name
					IF oPadOwner:Owner IS Menu VAR oMenuOwner
						MenuState.LastMenu := oMenuOwner:Name
					ENDIF
				ENDIF
			ELSEIF SELF:Owner IS ContextMenu VAR oCtx
				MenuState.LastBar   := oCtx:IndexOf( SELF )
				MenuState.LastPopup := oCtx:Name
				MenuState.LastPad   := ""
				MenuState.LastMenu  := ""
			ENDIF
			IF SELF:_vfpClick != NULL
				LOCAL sMethod AS STRING
				sMethod := SELF:_vfpClick:SendTo
				// Prefer the method on the Bar subclass itself; fall back to the
				// owner Menu/ContextMenu so VFPXPorter-generated subclasses work naturally.
				IF IsMethod( SELF, sMethod )
					SELF:_vfpClick:Call()
				ELSE
					LOCAL oTarget AS OBJECT
					oTarget := SELF:FindDispatchTarget()
					IF oTarget != NULL_OBJECT .AND. IsMethod( oTarget, sMethod )
						Send( oTarget, sMethod )
					ENDIF
				ENDIF
			ENDIF

		// Locate the object that owns the action methods for this Bar.
		// Chains tried (first non-null wins):
		//   ContextMenu: Bar.Owner → ContextMenu (direct)
		//   Menu bar, primary:   Bar.Owner → Popup → Popup.OwnerMenu (set by Pad.Popup setter)
		//   Menu bar, fallback:  Bar.Owner → Popup → Popup.OwnerItem (Pad) → Pad.Owner → Menu
		PRIVATE METHOD FindDispatchTarget() AS OBJECT
			IF SELF:Owner IS ContextMenu VAR oCtx
				RETURN oCtx
			ENDIF
			IF SELF:Owner IS Popup VAR oPopup
				// Primary: direct reference set at init time (robust against MDI merging)
				IF oPopup:OwnerMenu != NULL
					RETURN oPopup:OwnerMenu
				ENDIF
				// Fallback: walk the WinForms OwnerItem chain
				IF oPopup:OwnerItem IS Pad VAR oPad
					IF oPad:Owner IS Menu VAR oMenu
						RETURN oMenu
					ENDIF
				ENDIF
			ENDIF
			RETURN NULL_OBJECT

		// ── Popup ─────────────────────────────────────────────────────────────
		// Attaches a Popup as this bar's sub-menu dropdown.
		// Mirrors Pad.Popup so nested menus follow the same VFP convention.
		PROPERTY Popup AS Popup
			GET
				RETURN SELF:DropDown ASTYPE Popup
			END GET
			SET
				SELF:DropDown := VALUE
			END SET
		END PROPERTY

		// ── SetClickAction ────────────────────────────────────────────────────
		// Stores a codeblock as the click handler. Used by VFPMenuSupport.xh
		// UDCs when the ON SELECTION command is arbitrary code rather than a
		// plain DO <procName>.
		METHOD SetClickAction( block AS USUAL ) AS VOID
			SELF:Click += System.EventHandler{ SELF, @OnBlockClick() }
			SELF:_clickBlock := block

		PRIVATE METHOD OnBlockClick( sender AS OBJECT, e AS System.EventArgs ) AS VOID
			// Populate MenuState the same way OnVFPClick does.
			IF SELF:Owner IS Popup VAR oParentPopup
				MenuState.LastBar   := oParentPopup:IndexOf( SELF )
				MenuState.LastPopup := oParentPopup:Name
				IF oParentPopup:OwnerItem IS Pad VAR oPadOwner
					MenuState.LastPad := oPadOwner:Name
					IF oPadOwner:Owner IS Menu VAR oMenuOwner
						MenuState.LastMenu := oMenuOwner:Name
					ENDIF
				ENDIF
			ELSEIF SELF:Owner IS ContextMenu VAR oCtx
				MenuState.LastBar   := oCtx:IndexOf( SELF )
				MenuState.LastPopup := oCtx:Name
				MenuState.LastPad   := ""
				MenuState.LastMenu  := ""
			ENDIF
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
