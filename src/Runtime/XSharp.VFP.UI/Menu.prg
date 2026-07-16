// Menu.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.

USING System
USING System.Collections.Generic
USING System.Windows.Forms

BEGIN NAMESPACE XSharp.VFP.UI

	/// <summary>
	/// VFP-compatible Menu class. Represents a complete menu bar.
	/// Maps to System.Windows.Forms.MenuStrip.
	/// Use AddPad() to add top-level entries, then attach Popup objects to each Pad.
	/// Call Activate(oForm) to attach the menu to a Form.
	/// </summary>
	PARTIAL CLASS Menu INHERIT System.Windows.Forms.MenuStrip

		PRIVATE STATIC _registry AS Dictionary<STRING, Menu>

		PRIVATE _pads    AS List<Pad>
		PRIVATE _theForm AS Form

		CONSTRUCTOR() STRICT
			SUPER()
			SELF:_pads := List<Pad>{}

		// ── Named registry ────────────────────────────────────────────────────
		// Menus register themselves by Name so MENU() and lookup functions work.
		NEW PROPERTY Name AS STRING
			GET ; RETURN SUPER:Name ; END GET
			SET
				IF !String.IsNullOrEmpty(SUPER:Name) .AND. _registry != NULL
					_registry:Remove(SUPER:Name)
				ENDIF
				SUPER:Name := VALUE
				IF !String.IsNullOrEmpty(VALUE)
					IF _registry == NULL
						_registry := Dictionary<STRING, Menu>{}
					ENDIF
					_registry[VALUE] := SELF
				ENDIF
			END SET
		END PROPERTY

		STATIC METHOD Find( cName AS STRING ) AS Menu
			IF _registry != NULL .AND. _registry:ContainsKey(cName)
				RETURN _registry[cName]
			ENDIF
			RETURN NULL

		// Releases every registered menu (used by RELEASE MENUS with no list).
		STATIC METHOD ReleaseAll() AS VOID
			IF _registry == NULL
				RETURN
			ENDIF
			LOCAL values AS List<Menu>
			values := List<Menu>{ _registry:Values }
			FOREACH VAR oMenu IN values
				oMenu:Release()
			NEXT

		// ── Skip ──────────────────────────────────────────────────────────────
		PROPERTY Skip AS LOGIC
			GET ; RETURN !SELF:Enabled ; END GET
			SET ; SELF:Enabled := !VALUE ; END SET
		END PROPERTY

		// ── ThisForm ──────────────────────────────────────────────────────────
		// Set automatically by Activate(). Lets menu handler code use SELF:ThisForm:
		// the same way VFP's THISFORM keyword works.
		PROPERTY ThisForm AS Form
			GET
				RETURN SELF:_theForm
			END GET
		END PROPERTY

		// ── PadCount ──────────────────────────────────────────────────────────
		PROPERTY PadCount AS LONG
			GET
				RETURN (LONG) SELF:_pads:Count
			END GET
		END PROPERTY

		// ── Pads[i] ───────────────────────────────────────────────────────────
		// 1-based indexed access to Pad items.
		PROPERTY Pads[ i AS LONG ] AS Pad
			GET
				RETURN SELF:_pads[ (INT) i - 1 ]
			END GET
		END PROPERTY

		// ── AddPad ────────────────────────────────────────────────────────────
		// Creates a Pad, registers it, and returns it so the caller can chain
		// a Popup onto it immediately.
		METHOD AddPad( cName, cCaption ) AS Pad CLIPPER
			LOCAL sName    AS STRING
			LOCAL sCaption AS STRING
			LOCAL oPad     AS Pad
			sName    := (STRING) cName
			sCaption := (STRING) cCaption
			oPad         := Pad{}
			oPad:Name    := sName
			oPad:Caption := sCaption
			SELF:Items:Add( oPad )
			SELF:_pads:Add( oPad )
			SELF:AddProperty( sName, oPad, PropertyVisibility.Public, "AddPad" )
			RETURN oPad

		// ── Activate ──────────────────────────────────────────────────────────
		// Attaches this menu to a Form.
		METHOD Activate( oForm ) AS VOID CLIPPER
			IF oForm IS Form VAR frm
				IF !frm:Controls:Contains( SELF )
					frm:Controls:Add( SELF )
				ENDIF
				frm:MainMenuStrip := SELF
				SELF:_theForm := frm
			ENDIF
			IF oForm IS MainWindow VAR mw
				mw:ActiveMenu := SELF
			ENDIF

		// ── Deactivate ────────────────────────────────────────────────────────
		// Detaches from the hosting form without destroying the object.
		METHOD Deactivate() AS VOID STRICT
			LOCAL frm AS System.Windows.Forms.Form
			frm := SELF:FindForm()
			IF frm != NULL_OBJECT
				frm:MainMenuStrip := NULL_OBJECT
				frm:Controls:Remove( SELF )
			ENDIF
			SELF:_theForm := NULL_OBJECT
			IF MainWindow.Current != NULL_OBJECT .AND. MainWindow.Current:ActiveMenu == SELF
				MainWindow.Current:ActiveMenu := NULL_OBJECT
			ENDIF

		// ── Release ───────────────────────────────────────────────────────────
		METHOD Release() AS USUAL CLIPPER
			SELF:Deactivate()
			SELF:Dispose()
			RETURN NIL

		// ── Lifecycle stubs ───────────────────────────────────────────────────
		VIRTUAL METHOD Init() AS USUAL CLIPPER
			RETURN NIL

		VIRTUAL METHOD Destroy() AS USUAL CLIPPER
			RETURN NIL

	END CLASS

END NAMESPACE
