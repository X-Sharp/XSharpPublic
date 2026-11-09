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

		// ── Number ────────────────────────────────────────────────────────────
		// The number BAR() reports: the one given in DEFINE BAR, which the UDC
		// stores as the Name. A bar created from code without a numeric name
		// falls back to its position in the popup, separators not counted.
		INTERNAL PROPERTY Number AS INT
			GET
				LOCAL nNumber AS INT
				IF Int32.TryParse( SELF:Name, OUT nNumber )
					RETURN nNumber
				ENDIF
				IF SELF:Owner IS Popup VAR oPopup
					RETURN oPopup:IndexOf( SELF )
				ELSEIF SELF:Owner IS ContextMenu VAR oCtx
					RETURN oCtx:IndexOf( SELF )
				ENDIF
				RETURN 0
			END GET
		END PROPERTY

		METHOD Set_Click( methodCall AS VFPOverride ) AS VOID
			SELF:Click += System.EventHandler{ SELF, @OnVFPClick() }
			SELF:_vfpClick := methodCall

		PRIVATE METHOD OnVFPClick( sender AS OBJECT, e AS System.EventArgs ) AS VOID
			SELF:UpdateMenuState()
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

		// Fill MenuState so BAR(), POPUP(), PAD() and MENU() describe this selection,
		// inside the handler and after it. BAR() is the number from DEFINE BAR.
		PRIVATE METHOD UpdateMenuState() AS VOID
			MenuState.LastBar   := SELF:Number
			MenuState.LastPopup := ""
			MenuState.LastPad   := ""
			MenuState.LastMenu  := ""
			IF SELF:Owner IS ContextMenu VAR oCtx
				MenuState.LastPopup := oCtx:Name
				RETURN
			ENDIF
			IF SELF:Owner IS Popup VAR oPopup
				MenuState.LastPopup := oPopup:Name
				LOCAL oPad := oPopup:FindOwnerPad() AS Pad
				IF oPad != NULL
					MenuState.LastPad := oPad:Name
					IF oPad:Owner IS Menu VAR oMenu
						MenuState.LastMenu := oMenu:Name
					ELSEIF oPopup:OwnerMenu != NULL
						MenuState.LastMenu := oPopup:OwnerMenu:Name
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
				IF VALUE != NULL
					VALUE:OwnerBar := SELF
				ENDIF
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
			SELF:UpdateMenuState()
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
