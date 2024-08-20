﻿// CommandButton.prg
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
		PRIVATE _vfpStyle AS INT

		// Common properties that all VFP Objects support
		#include "Headers/VFPObject.xh"

		#include ".\Headers\ButtonControlProperties.xh"

		#include ".\Headers\FontProperties.xh"


		CONSTRUCTOR(  ) STRICT
			SUPER()

			SELF:SetStyle( ControlStyles.SupportsTransparentBackColor, TRUE)
			SELF:BackColor := Color.Transparent
			//
            SELF:_vfpStyle := 0
            SELF:Size := Size{100, 17}
			//



		[System.ComponentModel.EditorBrowsable(System.ComponentModel.EditorBrowsableState.Never)];
		PROPERTY Style AS INT
			GET
				RETURN SELF:_vfpStyle
			END GET

			SET
				SELF:_vfpStyle := VALUE
				IF ( SELF:_vfpStyle == 1 )
					NOP
					//SELF:Visible := FALSE
				ENDIF
			END SET
		END PROPERTY




	END CLASS

END NAMESPACE
