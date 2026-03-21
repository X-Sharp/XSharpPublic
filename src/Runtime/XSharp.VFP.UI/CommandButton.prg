// CommandButton.prg
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

BEGIN NAMESPACE XSharp.VFP.UI
	/// <summary>
	/// The VFP compatible CommandButton class.
	/// </summary>
	PARTIAL CLASS CommandButton INHERIT System.Windows.Forms.Button

		// Common properties that all VFP Objects support
		#include "Headers/VFPObject.xh"

		/// <summary>
		/// Backing fields for VFP properties
		/// </summary>
		PRIVATE _vfpStyle AS INT
		PRIVATE _default AS LOGIC
		PRIVATE _cancel AS LOGIC

		#include ".\Headers\ButtonControlProperties.xh"

		#include ".\Headers\FontProperties.xh"

		/// <summary>
		/// Constructor for CommandButton control.
		/// </summary>
		CONSTRUCTOR(  ) STRICT
			SUPER()
			SELF:SetStyle( ControlStyles.SupportsTransparentBackColor, TRUE)
			SELF:BackColor := Color.Transparent
			SELF:_vfpStyle := 0
			SELF:_default := FALSE
			SELF:_cancel := FALSE
			SELF:Size := Size{100, 17}
			RETURN

		/// <summary>
		/// Gets or sets whether this button is the default button.
		/// In VFP, this is the Default property.
		/// </summary>
		/// <value>True if this is the default button; otherwise, false.</value>
		[Category("VFP Properties"), Description("Set as default button")];
		[DefaultValue(FALSE)];
		PROPERTY Default AS LOGIC
			GET
				RETURN SELF:_default
			END GET
			SET
				SELF:_default := VALUE
				// If this button is on a form, set the form's AcceptButton
				VAR form := SELF:FindForm()
				IF form != NULL .AND. VALUE
					form:AcceptButton := SELF
				ENDIF
			END SET
		END PROPERTY

		/// <summary>
		/// Gets or sets whether this button is the cancel button.
		/// In VFP, this is the Cancel property.
		/// </summary>
		/// <value>True if this is the cancel button; otherwise, false.</value>
		[Category("VFP Properties"), Description("Set as cancel button")];
		[DefaultValue(FALSE)];
		PROPERTY Cancel AS LOGIC
			GET
				RETURN SELF:_cancel
			END GET
			SET
				SELF:_cancel := VALUE
				// If this button is on a form, set the form's CancelButton
				VAR form := SELF:FindForm()
				IF form != NULL .AND. VALUE
					form:CancelButton := SELF
				ENDIF
			END SET
		END PROPERTY

		/// <summary>
		/// Gets or sets the picture displayed on the button.
		/// Equivalent to VFP's ButtonPicture property.
		/// </summary>
		/// <value>The button image path or null.</value>
		[Category("VFP Properties"), Description("Picture displayed on the button")];
		PROPERTY ButtonPicture AS STRING AUTO

		/// <summary>
		/// Gets or sets the style of the button.
		/// 0 = Standard, 1 = Graphical.
		/// Equivalent to VFP's Style property.
		/// </summary>
		/// <value>The button style (0 or 1). Default is 0.</value>
		[Category("VFP Properties"), Description("Button style: 0=Standard, 1=Graphical")];
		[DefaultValue(0)];
		PROPERTY Style AS INT
			GET
				RETURN SELF:_vfpStyle
			END GET
			SET
				SELF:_vfpStyle := VALUE
				IF VALUE == 1
					SELF:FlatStyle := FlatStyle.Flat
				ELSE
					SELF:FlatStyle := FlatStyle.Standard
				ENDIF
			END SET
		END PROPERTY

		/// <summary>
		/// Programmatically clicks the button.
		/// Equivalent to VFP's Click() method.
		/// </summary>
		[Category("VFP Properties"), Description("Programmatically click the button")];
		PUBLIC METHOD Click() AS VOID
			SELF:PerformClick()

	END CLASS

END NAMESPACE
