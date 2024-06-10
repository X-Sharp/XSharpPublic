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
USING System.ComponentModel

BEGIN NAMESPACE XSharp.VFP.UI
	/// <summary>
	/// The VFP compatible Label class.
	/// </summary>
	PARTIAL CLASS Label INHERIT System.Windows.Forms.Label

		// Common properties that all VFP Objects support
		#include "Headers/VFPObject.xh"

	    #include "XSharp\VFPProperties.xh"

		PROPERTY Alignment AS INT
			GET
				RETURN VFPAlignmentConvert( SELF:TextAlign )
			END GET
			SET

				SELF:TextAlign := VFPAlignmentConvert(VALUE)
			END SET
		END PROPERTY

		// Don't Care
		[System.ComponentModel.EditorBrowsable(System.ComponentModel.EditorBrowsableState.Never)];
		PROPERTY Style AS INT AUTO

		PROPERTY Rotation AS INT AUTO

		CONSTRUCTOR(  ) STRICT
			SUPER()
			SELF:SetStyle( ControlStyles.SupportsTransparentBackColor, TRUE)
			SELF:BackColor := Color.Transparent


			#include ".\Headers\TextControlProperties.xh"

			#include ".\Headers\FontProperties.xh"


		PROPERTY DisabledBackColor AS LONG AUTO
		PROPERTY DisabledForeColor AS LONG AUTO
        PROPERTY WordWrap AS LOGIC AUTO
        PROTECTED OVERRIDE METHOD OnPaint( e AS PaintEventArgs ) AS VOID STRICT
            IF SELF:Rotation != 0
                var b := SolidBrush{ SELF:ForeColor }
                e:Graphics:TranslateTransform(self:Width, self:Height/2)
                e:Graphics:RotateTransform( SELF:Rotation )
                e:Graphics:DrawString(SELF:Text, SELF:Font, b, PointF{0,0})
            endif
            SUPER:OnPaint( e )


	END CLASS

END NAMESPACE
