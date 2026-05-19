// Label.prg
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
	/// VFP-compatible static text control that wraps <see cref="System.Windows.Forms.Label"/>.<br/>
	/// Adds VFP-specific properties: <see cref="Alignment"/> (0/1/2 → Left/Right/Center),
	/// <see cref="Style"/> (0=opaque, 1=transparent background), <see cref="WordWrap"/>
	/// (controls <c>AutoSize</c>), <see cref="Rotation"/> (arbitrary-angle text via GDI+),
	/// and <see cref="DisabledBackColor"/> / <see cref="DisabledForeColor"/> applied when
	/// <c>Enabled</c> is set to <c>.F.</c>.
	/// </summary>
	PARTIAL CLASS Label INHERIT System.Windows.Forms.Label

		// Common properties that all VFP Objects support
		#include "Headers/VFPObject.xh"

	    #include "VFPProperties.xh"

		/// <summary>Horizontal text alignment: 0=Left, 1=Right, 2=Center. Maps to <see cref="System.Windows.Forms.Label.TextAlign"/>.</summary>
		PROPERTY Alignment AS INT
			GET
				RETURN VFPAlignmentConvert( SELF:TextAlign )
			END GET
			SET
				SELF:TextAlign := VFPAlignmentConvert(VALUE)
			END SET
		END PROPERTY

		/// <summary>
		/// VFP Style: 0=Standard (opaque background), 1=Transparent.<br/>
		/// Setting to 1 sets <c>BackColor</c> to <see cref="System.Drawing.Color.Transparent"/>;
		/// setting to 0 resets it to the default system colour.
		/// </summary>
		PRIVATE _style AS INT
		PROPERTY Style AS INT
			GET
				RETURN _style
			END GET
			SET
				_style := VALUE
				IF VALUE == 1
					SELF:BackColor := System.Drawing.Color.Transparent
				ELSE
					SELF:ResetBackColor()
				ENDIF
			END SET
		END PROPERTY

		/// <summary>Clockwise rotation angle in degrees applied to the label text via GDI+ transform. 0 = no rotation (standard rendering path).</summary>
		PROPERTY Rotation AS INT AUTO

		/// <summary>Background colour applied when <c>Enabled</c> is set to <c>.F.</c>. Has no effect when the control is enabled.</summary>
		PROPERTY DisabledBackColor AS System.Drawing.Color AUTO

		/// <summary>Foreground (text) colour applied when <c>Enabled</c> is set to <c>.F.</c>. Has no effect when the control is enabled.</summary>
        PROPERTY DisabledForeColor AS System.Drawing.Color AUTO

		/// <summary>
		/// When <c>.T.</c>, text wraps within the label's fixed bounds (<c>AutoSize = .F.</c>).<br/>
		/// When <c>.F.</c> (default), the label auto-sizes to a single line (<c>AutoSize = .T.</c>).
		/// </summary>
		PROPERTY WordWrap AS LOGIC
			GET
				RETURN SELF:_wordWrap
			END GET
			SET
				SELF:_wordWrap := VALUE
				IF VALUE
					SELF:AutoSize := FALSE
				ELSE
					SELF:AutoSize := TRUE
				ENDIF
			END SET
		END PROPERTY
		PRIVATE _wordWrap AS LOGIC

		CONSTRUCTOR(  ) STRICT
			SUPER()
			SELF:SetStyle( ControlStyles.SupportsTransparentBackColor, TRUE)
			SELF:BackColor := Color.Transparent

        PROTECTED OVERRIDE METHOD OnPaint( e AS PaintEventArgs ) AS VOID STRICT
            IF SELF:Rotation != 0
                // Fill background first (respects BackStyle/BackColor)
                IF SELF:BackColor != Color.Transparent
                    e:Graphics:Clear( SELF:BackColor )
                ENDIF
                VAR b := SolidBrush{ SELF:ForeColor }
                VAR sf := StringFormat{ StringFormatFlags.NoClip }
                sf:Alignment     := StringAlignment.Center
                sf:LineAlignment := StringAlignment.Center
                // Translate to the center of the control, rotate, then draw at origin
                e:Graphics:TranslateTransform( (REAL4)(SELF:Width / 2), (REAL4)(SELF:Height / 2) )
                e:Graphics:RotateTransform( SELF:Rotation )
                VAR sz := e:Graphics:MeasureString( SELF:Text, SELF:Font )
                e:Graphics:DrawString( SELF:Text, SELF:Font, b, RectangleF{ -sz:Width/2, -sz:Height/2, sz:Width, sz:Height }, sf )
            ELSE
                SUPER:OnPaint( e )
            ENDIF

		// ── DisabledBackColor / DisabledForeColor ────────────────────────────

		PROTECTED OVERRIDE METHOD OnEnabledChanged(e AS System.EventArgs) AS VOID
			SUPER:OnEnabledChanged(e)
			IF !SELF:Enabled
				IF SELF:DisabledBackColor != System.Drawing.Color.Empty
					SELF:BackColor := SELF:DisabledBackColor
				ENDIF
				IF SELF:DisabledForeColor != System.Drawing.Color.Empty
					SELF:ForeColor := SELF:DisabledForeColor
				ENDIF
			ELSE
				SELF:ResetBackColor()
				SELF:ResetForeColor()
			ENDIF
		END METHOD

		#include "TextControlProperties.xh"

		#include "FontProperties.xh"

	END CLASS

END NAMESPACE
