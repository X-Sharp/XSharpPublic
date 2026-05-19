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
		/// <summary>
		/// The singleton <see cref="MainWindow"/> instance. Set in the constructor so <c>Form.prg</c> can reach <c>_Screen</c> without depending on the generated <c>_Screen</c> GLOBAL in another assembly.
		/// </summary>
		STATIC PROPERTY Current AS MainWindow AUTO

		PRIVATE _Forms AS List<Form>

		// ── ActiveMenu ────────────────────────────────────────────────────────
		/// <summary>
		/// The <see cref="Menu"/> currently attached to this window. Set by <c>Menu.Activate()</c>. Replaces the deprecated <c>System.Windows.Forms.MainMenu</c>.
		/// </summary>
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

		/// <summary>
		/// Always returns 2 (top-level modeless). Overriding this prevents <see cref="Form.Show"/> from trying to set <c>MdiParent</c> on the container itself. The setter is a no-op.
		/// </summary>
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
		/// <summary>
		/// Returns the live <see cref="Form"/> at VFP 1-based position <paramref name="i"/> in the registered-forms list. Equivalent to VFP <c>_Screen.Forms(i)</c>.
		/// </summary>
		PUBLIC METHOD Forms( i AS INT ) AS Form
			RETURN SELF:_Forms[ i - 1 ]

		/// <summary>
		/// Number of forms currently registered with this window. Equivalent to VFP <c>_Screen.FormCount</c>.
		/// </summary>
		PROPERTY FormCount AS LONG GET SELF:_Forms:Count

		// ── Registration ──────────────────────────────────────────────────────
		/// <summary>
		/// Registers <paramref name="frm"/> in the live-forms list. Called by <c>Form.OnLoad</c>; automatically wires <c>FormClosed</c> to remove the form when it closes.
		/// </summary>
		PUBLIC METHOD RegisterForm( frm AS Form ) AS VOID
			IF !SELF:_Forms:Contains( frm )
				SELF:_Forms:Add( frm )
				frm:FormClosed += System.Windows.Forms.FormClosedEventHandler{ SELF, @OnFormClosed() }
			ENDIF

		/// <summary>
		/// Removes the closed form from the live-forms list. Wired automatically by <see cref="RegisterForm"/>.
		/// </summary>
		PRIVATE METHOD OnFormClosed( sender AS OBJECT, e AS System.Windows.Forms.FormClosedEventArgs ) AS VOID
			IF sender IS Form VAR frm
				SELF:_Forms:Remove( frm )
            ENDIF

		/// <summary>
		/// After the window is shown, evaluates <see cref="StartFunction"/> as a macro or launches <see cref="StartForm"/> via <c>__VFPDoForm.Create</c>, matching VFP start-up behaviour.
		/// </summary>
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
