// Listbox.prg
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
	/// The VFP compatible Listbox class.
	/// </summary>
	PARTIAL CLASS ListBox INHERIT System.Windows.Forms.ListBox
		// Common properties that all VFP Objects support
#include "Headers/VFPObject.xh"

		CONSTRUCTOR(  ) STRICT
            SUPER()
            SELF:Size := Size{100,170}

			RETURN

#include ".\Headers\ControlProperties.xh"

#include ".\Headers\ControlSource.xh"

		PROPERTY AutoHideScrollBar AS LONG AUTO
		PROPERTY NullDisplay AS String AUTO
		PROPERTY Picture AS STRING AUTO


	END CLASS
END NAMESPACE // XSharp.VFP.UI
