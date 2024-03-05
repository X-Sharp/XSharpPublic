// VFPCheckbox.prg
// Created by    : fabri
// Creation Date : 9/20/2022 10:40:00 PM
// Created for   :
// WorkStation   : FABXPS


USING System
USING System.Collections.Generic
USING System.Text
USING System.Windows.Forms
USING System.ComponentModel
USING System.Drawing
USING System.ComponentModel

BEGIN NAMESPACE XSharp.VFP.UI

	/// <summary>
	/// The VFPCheckbox class.
	/// </summary>
	PARTIAL CLASS CheckBox INHERIT System.Windows.Forms.CheckBox

		// Common properties that all VFP Objects support
#include "Headers/VFPObject.xh"

		CONSTRUCTOR(  ) STRICT
			SUPER()
			RETURN

#include ".\Headers\ControlProperties.xh"

#include ".\Headers\ControlSource.xh"

		PROPERTY Centered AS LOGIC AUTO
		PROPERTY ReadOnly  AS LOGIC AUTO
		PROPERTY WordWrap AS LOGIC AUTO

	END CLASS
END NAMESPACE // XSharp.VFP.UI