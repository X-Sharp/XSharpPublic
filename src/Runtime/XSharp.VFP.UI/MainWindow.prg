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

        /// <summary>
        /// The code to execute when the MainWindow is first shown.
        /// </summary>
        /// <value></value>
        PROPERTY StartFunction AS STRING AUTO

        /// <summary>
        /// The Class/Form that will be instantiated and shown when the MainWindow is first displayed.
        /// Like a DO FORM <StartForm> in VFP.
        /// </summary>
        /// <value></value>
        PROPERTY StartForm AS STRING AUTO

		// MainWindow is always the top-level MDI container — never a child or dialog.
		// Overriding ShowWindow prevents Form.Show() from trying to set MdiParent on itself.
		OVERRIDE PROPERTY ShowWindow AS INT
			GET ; RETURN 2 ; END GET   // always top-level modeless
			SET ; NOP ; END SET         // immutable for the container
		END PROPERTY

		CONSTRUCTOR()
			SUPER()
			MainWindow.Current  := SELF
			SELF:_Forms         := List<Form>{}
			SELF:Size           := System.Drawing.Size{ 800, 600 }
			SELF:IsMdiContainer := TRUE
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

		PROTECTED OVERRIDE METHOD OnShown(e AS System.EventArgs) AS VOID
            SUPER:OnShown(e)
            // Call the StartFunction if set
            IF !String.IsNullOrEmpty(SELF:StartFunction)
                VAR startFunc := MCompile("{|| " + SELF:StartFunction + "}")
                Eval(startFunc)
            ELSEIF !String.IsNullOrEmpty(SELF:StartForm)
                // Reproduce the DO FORM UDC
                XSharp.VFP.UI.__VFPDoForm.InitParam()
                XSharp.VFP.UI.__VFPDoForm.Create( SELF:StartForm )
            ENDIF

	END CLASS
END NAMESPACE // XSharp.VFP.UI
