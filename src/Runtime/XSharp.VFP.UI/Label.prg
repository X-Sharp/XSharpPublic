// VFPLabel.prg
USING System
USING System.Collections.Generic
USING System.Text
USING System.Windows.Forms
USING System.Drawing
USING System.ComponentModel
USING System.ComponentModel

BEGIN NAMESPACE XSharp.VFP.UI
	/// <summary>
	/// The VFPTextBox class.
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

			// TODO : Sorry not supported by now, but we may write the OnPaint code to support it ??
		[Obsolete("Not Supported currently")];
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

	END CLASS

END NAMESPACE