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
        PRIVATE mainStatusStrip AS System.Windows.Forms.StatusStrip
        PRIVATE infoStripLabel AS System.Windows.Forms.ToolStripStatusLabel

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
        /// The Menu that will be instantiated and shown when the MainWindow is first displayed.
        /// </summary>
        /// <value></value>
        PROPERTY StartMenu AS STRING AUTO

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
            SELF:StartPosition  := System.Windows.Forms.FormStartPosition.CenterScreen
            //
            SELF:mainStatusStrip := System.Windows.Forms.StatusStrip{}
            SELF:infoStripLabel	:=	System.Windows.Forms.ToolStripStatusLabel{}
            //
            // mainStatusStrip
            //
            SELF:mainStatusStrip:ImageScalingSize := System.Drawing.Size{20, 20}
            SELF:mainStatusStrip:Location := System.Drawing.Point{0, 587}
            SELF:mainStatusStrip:Name := "mainStatusStrip"
            SELF:mainStatusStrip:Size := System.Drawing.Size{1083, 22}
            SELF:mainStatusStrip:TabIndex := 1
            SELF:mainStatusStrip:Text := "status"
            SELF:mainStatusStrip:Items:AddRange(<System.Windows.Forms.ToolStripItem>{ SELF:infoStripLabel })
            //
			//	infoStripLabel
			//
			SELF:infoStripLabel:Name	:=	"infoStripLabel"
			SELF:infoStripLabel:Size	:=	System.Drawing.Size{0, 17}
            //
            SELF:Controls:Add(SELF:mainStatusStrip)
            //
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
            ELSEIF !String.IsNullOrEmpty(SELF:StartMenu)
                // Look for a user-defined Menu subclass before falling back to the
                // runtime lookup, which would incorrectly resolve "menu" → XSharp.VFP.UI.Menu.
                LOCAL oMenu AS Menu
                VAR menuType := MainWindow.FindUserMenuType(SELF:StartMenu)
                IF menuType != NULL
                    oMenu := (Menu) System.Activator.CreateInstance(menuType)
                ELSE
                    oMenu := (Menu) __VFPCreateInstance(SELF:StartMenu)
                ENDIF
                IF oMenu != NULL
                    Send(oMenu, "Init")
                    oMenu:Activate( SELF )
                ENDIF
            ENDIF

        // Searches all loaded assemblies except XSharp.VFP.UI for a class that inherits
        // from Menu and whose short name matches className (case-insensitive).
        // Returns NULL when no user-defined subclass is found.
        PRIVATE STATIC METHOD FindUserMenuType(className AS STRING) AS System.Type
            LOCAL thisAsm AS System.Reflection.Assembly
            thisAsm := System.Reflection.Assembly.GetExecutingAssembly()
            FOREACH VAR asm IN System.AppDomain.CurrentDomain:GetAssemblies()
                IF asm == thisAsm
                    LOOP
                ENDIF
                TRY
                    FOREACH VAR t IN asm:GetExportedTypes()
                        IF String.Equals(t:Name, className, StringComparison.OrdinalIgnoreCase) ;
                           .AND. t:IsSubclassOf(TYPEOF(Menu))
                            RETURN t
                        ENDIF
                    NEXT
                CATCH
                    NOP
                END TRY
            NEXT
            RETURN NULL

	END CLASS
END NAMESPACE // XSharp.VFP.UI
