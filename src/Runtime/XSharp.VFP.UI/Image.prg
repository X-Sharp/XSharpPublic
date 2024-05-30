// Image.prg
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
	/// The VFP compatible Image class.
	/// </summary>
	PARTIAL CLASS Image INHERIT System.Windows.Forms.PictureBox

		// Common properties that all VFP Objects support
		#include "Headers/VFPObject.xh"

		CONSTRUCTOR(  ) STRICT
            SUPER()
            SELF:Size := System.Drawing.Size{100, 17}
			RETURN

#include ".\Headers\ControlProperties.xh"


	END CLASS
END NAMESPACE // XSharp.VFP.UI
