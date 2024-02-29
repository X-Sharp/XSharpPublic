// VFPListbox.prg
// Created by    : fabri
// Creation Date : 9/20/2022 10:43:19 PM
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
	/// The VFPListbox class.
	/// </summary>
	PARTIAL CLASS ListBox INHERIT System.Windows.Forms.ListBox
		// Common properties that all VFP Objects support
#include "Headers/VFPObject.xh"

		CONSTRUCTOR(  ) STRICT
			SUPER()
			RETURN

#include ".\Headers\ControlProperties.xh"

#include ".\Headers\ControlSource.xh"

		PROPERTY AutoHideScrollBar AS LONG AUTO
		PROPERTY NullDisplay AS String AUTO
		PROPERTY Picture AS STRING AUTO


	END CLASS
END NAMESPACE // XSharp.VFP.UI