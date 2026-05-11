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

BEGIN NAMESPACE XSharp.VFP.UI
	/// <summary>
	/// The VFP compatible OptionButton class.
	/// </summary>
	PARTIAL CLASS OptionButton INHERIT System.Windows.Forms.RadioButton
		// Common properties that all VFP Objects support
		#include "Headers/VFPObject.xh"

		#include "VFPProperties.xh"

		#include "ControlProperties.xh"

		#include "FontProperties.xh"

		#include "ControlSource.xh"

		#include "Headers/VFPButtonImage.xh"

		// ── Value ────────────────────────────────────────────────────────────
		// VFP OptionButton Value: .T. = selected, .F. = not selected.
		PROPERTY Value AS LOGIC
			GET ; RETURN SELF:Checked ; END GET
			SET ; SELF:Checked := VALUE ; END SET
		END PROPERTY

		// WinForms RadioButton does not raise KeyPress automatically.
		// Override so vfpKeyPress subscribers fire correctly.
		PROTECTED OVERRIDE METHOD OnKeyPress(e AS System.Windows.Forms.KeyPressEventArgs) AS VOID
			SELF:OnVFPKeyPress(SELF, e)
			SUPER:OnKeyPress(e)

		// ── InteractiveChange ────────────────────────────────────────────────

		PROTECTED METHOD OnCheckedChanged(e AS System.EventArgs) AS VOID
			SUPER:OnCheckedChanged(e)
			SELF:_ApplyPicture()
			SELF:OnVFPInteractiveChange(SELF, e)
		END METHOD

		CONSTRUCTOR() STRICT
			SUPER()
			SELF:Size := Size{10, 16}

	END CLASS

END NAMESPACE
