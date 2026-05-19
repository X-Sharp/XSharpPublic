// Container.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.

USING System
USING System.Collections.Generic
USING System.Text
USING System.Windows.Forms
USING System.Drawing
USING System.Drawing.Drawing2D
USING System.ComponentModel


BEGIN NAMESPACE XSharp.VFP.UI


	// TODO Check IDynamicProperties -> XSharp.RT

	/// <summary>
	/// VFP-compatible generic container control that wraps <see cref="System.Windows.Forms.UserControl"/>.<br/>
	/// Supports a transparent default background (<c>SupportsTransparentBackColor</c>), a custom
	/// <see cref="BorderColor"/> painted in <c>OnPaint</c> when <c>BorderStyle=0</c>, and
	/// <see cref="BackStyle"/> (0=Transparent, 1=Opaque).<br/>
	/// <see cref="Refresh"/> repaints the container and recursively refreshes all child controls,
	/// then fires <c>vfpRefresh</c> if set. <c>vfpKeyPress</c> is dispatched via the overridden
	/// <c>OnKeyPress</c>.
	/// </summary>
	PARTIAL CLASS Container INHERIT System.Windows.Forms.UserControl


         #include "VFPProperties.xh"
		 #include "ControlProperties.xh"
		 #include "FontProperties.xh"

		 #include "Tooltips.xh"

			// This is a fake property, just here to ease Code Generation
			//PROPERTY AutoScaleMode AS System.Windows.Forms.AutoScaleMode AUTO

		CONSTRUCTOR( ) STRICT
			SUPER()
			SELF:SetStyle( ControlStyles.SupportsTransparentBackColor, true)
            SELF:BackColor := Color.Transparent
            SELF:Size := Size{75,75}
			RETURN


        /// <summary>Colour of the custom border drawn in <c>OnPaint</c> when <c>BorderStyle=0</c> (None). Set to <c>Color.Empty</c> to suppress the border.</summary>
        PROPERTY BorderColor AS System.Drawing.Color
            GET ; RETURN _borderColor ; END GET
            SET ; _borderColor := VALUE ; SELF:Invalidate() ; END SET
        END PROPERTY
        PRIVATE _borderColor AS System.Drawing.Color

		/// <summary>VFP BackStyle: 0=Transparent (sets <c>BackColor</c> to <c>Transparent</c>), 1=Opaque/default (resets to <c>SystemColors.Control</c>).</summary>
		PRIVATE _backStyle := 1 AS INT
		[System.ComponentModel.EditorBrowsable(System.ComponentModel.EditorBrowsableState.Never)];
		PROPERTY BackStyle AS INT
			GET
				RETURN _backStyle
			END GET
			SET
				_backStyle := VALUE
				IF (VALUE == 0 )
					SELF:BackColor := System.Drawing.Color.Transparent
				ELSE
					SELF:BackColor := System.Drawing.SystemColors.Control
				ENDIF
			END SET
		END PROPERTY

        // ── OnKeyPress ────────────────────────────────────────────────────────
        PROTECTED OVERRIDE METHOD OnKeyPress(e AS System.Windows.Forms.KeyPressEventArgs) AS VOID
            SUPER:OnKeyPress(e)
            SELF:OnVFPKeyPress(SELF, e)

        // ── OnPaint ───────────────────────────────────────────────────────────
        PROTECTED OVERRIDE METHOD OnPaint(e AS PaintEventArgs) AS VOID
            SUPER:OnPaint(e)
            // Only paint a custom border when BorderStyle=0 (None) — WinForms FixedSingle
            // draws its own border when BorderStyle=1 is set on the UserControl.
            IF SELF:BorderStyle == 0 .AND. SELF:_borderColor != System.Drawing.Color.Empty
                VAR pw := 1
                VAR pen := Pen{ _borderColor, (SINGLE) pw }
                e:Graphics:DrawRectangle( pen, 0, 0, SELF:ClientSize:Width - 1, SELF:ClientSize:Height - 1 )
                pen:Dispose()
            ENDIF

        /// <summary>
        /// Repaints the container, fires <c>vfpRefresh</c> (if set), then recursively calls
        /// <c>Refresh()</c> on all child <see cref="System.Windows.Forms.Control"/> instances.
        /// </summary>
        OVERRIDE METHOD Refresh() AS VOID
            SUPER:Refresh()
            IF SELF:_VFPRefresh != NULL
                SELF:_VFPRefresh:Call()
            ENDIF
            FOREACH VAR ctrl IN SELF:Controls
                IF ctrl IS System.Windows.Forms.Control VAR c
                    c:Refresh()
                ENDIF
            NEXT
        END METHOD


	END CLASS
END NAMESPACE // XSharp.VFP.UI
