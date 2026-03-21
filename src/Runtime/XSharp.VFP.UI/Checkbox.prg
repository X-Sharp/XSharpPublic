// Checkbox.prg
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
	/// The VFP compatible Checkbox class.
	/// </summary>
	PARTIAL CLASS CheckBox INHERIT System.Windows.Forms.CheckBox

		// Common properties that all VFP Objects support
		#include "Headers/VFPObject.xh"

		/// <summary>
		/// Backing fields for VFP properties
		/// </summary>
		PRIVATE _value AS USUAL
		PRIVATE _vfpAlignment AS INT
		PRIVATE _vfpStyle AS INT

		/// <summary>
		/// Constructor for CheckBox control.
		/// </summary>
		CONSTRUCTOR(  ) STRICT
			SUPER()
			SELF:Size := Size{100, 17}
			SELF:_value := FALSE
			SELF:_vfpAlignment := 0
			SELF:_vfpStyle := 0
			RETURN

		#include ".\Headers\ControlProperties.xh"
		#include ".\Headers\ControlFocus.xh"

		#include ".\Headers\ControlSource.xh"

		/// <summary>
		/// Gets or sets the value of the checkbox.
		/// In VFP, this can be .F. (False), .T. (True), or .NULL.
		/// WinForms Checked is TRUE/FALSE only.
		/// </summary>
		/// <value>The checkbox value as USUAL (TRUE, FALSE, or NIL for .NULL.).</value>
		[DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)];
		[EditorBrowsable(EditorBrowsableState.Never)];
		[Bindable(FALSE)];
		[Browsable(FALSE)];
		PROPERTY Value AS USUAL
			GET
				RETURN SELF:_value
			END GET
			SET
				SELF:_value := VALUE
			IF IsNil(VALUE)
				// .NULL. - set to indeterminate if ThreeState is enabled
				SELF:CheckState := CheckState.Indeterminate
			ELSE
				// Try to convert to boolean
				TRY
					SELF:Checked := (LOGIC)VALUE
					CATCH
						SELF:Checked := FALSE
					END TRY
				ENDIF
			END SET
		END PROPERTY

		/// <summary>
		/// Gets or sets the alignment of the checkbox text.
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
		/// Gets or sets the style of the checkbox.
		/// 0 = Standard, 1 = Graphical.
		/// Equivalent to VFP's Style property.
		/// </summary>
		/// <value>The checkbox style value. Default is 0.</value>
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

		PROPERTY Centered AS LOGIC AUTO
		PROPERTY ReadOnly  AS LOGIC AUTO
		PROPERTY WordWrap AS LOGIC AUTO

	END CLASS
END NAMESPACE // XSharp.VFP.UI
