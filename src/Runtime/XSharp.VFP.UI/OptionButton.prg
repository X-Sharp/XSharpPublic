// OptionButton.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.

USING System
USING System.Collections.Generic
USING System.Text
USING System.Windows.Forms
USING System.ComponentModel
USING System.Drawing
USING System.ComponentModel


BEGIN NAMESPACE XSharp.VFP.UI
	/// <summary>
	/// The VFP compatible OptionButton class.
	/// </summary>
	PARTIAL CLASS OptionButton INHERIT System.Windows.Forms.RadioButton

		// Common properties that all VFP Objects support
		#include "Headers/VFPObject.xh"

		#include ".\XSharp\VFPProperties.xh"

		/// <summary>
		/// Backing fields for VFP properties
		/// </summary>
		PRIVATE _value AS LOGIC
		PRIVATE _vfpAlignment AS INT
		PRIVATE _vfpStyle AS INT

		/// <summary>
		/// Constructor for OptionButton control.
		/// </summary>
		CONSTRUCTOR(  ) STRICT
			SUPER()
			SELF:Size := Size{100, 17}
			SELF:_value := FALSE
			SELF:_vfpAlignment := 0
			SELF:_vfpStyle := 0

		#include ".\Headers\ControlProperties.xh"
        #include ".\Headers\ControlFocus.xh"
		#include ".\Headers\FontProperties.xh"

		#include ".\Headers\ControlSource.xh"

		/// <summary>
		/// Gets or sets the value of the option button.
		/// In VFP, this is TRUE if selected, FALSE otherwise.
		/// </summary>
		/// <value>True if selected; otherwise, false.</value>
		[DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)];
		[EditorBrowsable(EditorBrowsableState.Never)];
		[Bindable(FALSE)];
		[Browsable(FALSE)];
		PROPERTY Value AS LOGIC
			GET
				RETURN SELF:Checked
			END GET
			SET
				SELF:_value := VALUE
				SELF:Checked := VALUE
			END SET
		END PROPERTY

		/// <summary>
		/// Gets or sets the alignment of the option button text.
		/// 0 = Left (text on right), 1 = Right (text on left).
		/// Equivalent to VFP's Alignment property.
		/// </summary>
		/// <value>0 for left, 1 for right. Default is 0.</value>
		[Category("VFP Properties"), Description("Text alignment: 0=Left, 1=Right")];
		[DefaultValue(0)];
		PROPERTY Alignment AS INT
			GET
				RETURN SELF:_vfpAlignment
			END GET
			SET
				SELF:_vfpAlignment := VALUE
				IF VALUE == 0
					SELF:TextAlign := ContentAlignment.MiddleLeft
					SELF:CheckAlign := ContentAlignment.MiddleLeft
				ELSE
					SELF:TextAlign := ContentAlignment.MiddleRight
					SELF:CheckAlign := ContentAlignment.MiddleRight
				ENDIF
			END SET
		END PROPERTY

		/// <summary>
		/// Gets or sets the style of the option button.
		/// 0 = Standard, 1 = Graphical.
		/// Equivalent to VFP's Style property.
		/// </summary>
		/// <value>The option button style value. Default is 0.</value>
		[Category("VFP Properties"), Description("Style: 0=Standard, 1=Graphical")];
		[DefaultValue(0)];
		PROPERTY Style AS INT
			GET
				RETURN SELF:_vfpStyle
			END GET
			SET
				SELF:_vfpStyle := VALUE
				IF VALUE == 1
					SELF:Appearance := Appearance.Button
				ELSE
					SELF:Appearance := Appearance.Normal
				ENDIF
			END SET
		END PROPERTY

	END CLASS

END NAMESPACE
