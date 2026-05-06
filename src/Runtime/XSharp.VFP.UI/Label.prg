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
	/// The VFP compatible Label class.
	/// </summary>
	PARTIAL CLASS Label INHERIT System.Windows.Forms.Label

		// Common properties that all VFP Objects support
		#include "Headers/VFPObject.xh"

	    #include "VFPProperties.xh"

		PROPERTY Alignment AS INT
			GET
				RETURN VFPAlignmentConvert( SELF:TextAlign )
			END GET
			SET
				SELF:TextAlign := VFPAlignmentConvert(VALUE)
			END SET
		END PROPERTY

		// VFP Style: 0=Standard (opaque background), 1=Transparent.
		// Mirrors BackStyle — setting Style also sets BackStyle.
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

		PROPERTY Rotation AS INT AUTO

		[System.ComponentModel.DefaultValue(0)];
        PROPERTY DisabledBackColor AS LONG AUTO

        [System.ComponentModel.DefaultValue(0)];
        PROPERTY DisabledForeColor AS LONG AUTO

		// WordWrap: .T. = fixed-size label, text wraps within bounds (AutoSize=.F.)
		//           .F. = label auto-sizes to single line (AutoSize=.T.)
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
				IF SELF:DisabledBackColor != 0
					SELF:BackColor := VFPTools.ColorFromVFP(SELF:DisabledBackColor)
				ENDIF
				IF SELF:DisabledForeColor != 0
					SELF:ForeColor := VFPTools.ColorFromVFP(SELF:DisabledForeColor)
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
