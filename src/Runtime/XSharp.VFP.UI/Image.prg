// VFPImage.prg
// Created by    : fabri
// Creation Date : 9/20/2022 8:42:45 PM
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
	/// The VFPImage class.
	/// </summary>
	PARTIAL CLASS Image INHERIT System.Windows.Forms.PictureBox

		// Common properties that all VFP Objects support
		#include "Headers/VFPObject.xh"

		CONSTRUCTOR(  ) STRICT
			SUPER()
			RETURN

#include ".\Headers\ControlProperties.xh"


	END CLASS
END NAMESPACE // XSharp.VFP.UI