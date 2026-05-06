// MainWindow.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.


USING System
USING System.Collections.Generic
USING System.Text
USING System.ComponentModel

BEGIN NAMESPACE XSharp.VFP.UI

	/// <summary>
	/// The VFP compatible MainWindow class.
	/// Used to emulate the Visual FoxPro _Screen object.
	/// Tracks all living forms (Forms / FormCount) and the active menu (ActiveMenu).
	/// </summary>
	CLASS MainWindow INHERIT Form

		// ── Singleton ─────────────────────────────────────────────────────────
		// Set in the constructor so Form.prg can reach _Screen without depending
		// on the generated _Screen GLOBAL (which lives in a different assembly).
		STATIC PROPERTY Current AS MainWindow AUTO

		PRIVATE _Forms AS List<Form>

		// ── ActiveMenu ────────────────────────────────────────────────────────
		// Set by Menu.Activate() when the menu attaches to this window.
		// Replaces the old MainMenu (System.Windows.Forms.MainMenu, deprecated).
		PROPERTY ActiveMenu AS Menu AUTO

		CONSTRUCTOR()
			SUPER()
			MainWindow.Current := SELF
			SELF:_Forms := List<Form>{}
			SELF:Size   := System.Drawing.Size{ 800, 600 }
			RETURN

		// ── Forms collection ──────────────────────────────────────────────────
		PUBLIC METHOD Forms( i AS INT ) AS Form
			RETURN SELF:_Forms[ i - 1 ]

		PROPERTY FormCount AS LONG GET SELF:_Forms:Count

		// ── Registration ──────────────────────────────────────────────────────
		// Called by Form.OnLoad; wires removal automatically on FormClosed.
		PUBLIC METHOD RegisterForm( frm AS Form ) AS VOID
			IF !SELF:_Forms:Contains( frm )
				SELF:_Forms:Add( frm )
				frm:FormClosed += System.Windows.Forms.FormClosedEventHandler{ SELF, @OnFormClosed() }
			ENDIF

		PRIVATE METHOD OnFormClosed( sender AS OBJECT, e AS System.Windows.Forms.FormClosedEventArgs ) AS VOID
			IF sender IS Form VAR frm
				SELF:_Forms:Remove( frm )
			ENDIF

	END CLASS
END NAMESPACE // XSharp.VFP.UI
