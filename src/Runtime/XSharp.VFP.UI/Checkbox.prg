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

BEGIN NAMESPACE XSharp.VFP.UI

	/// <summary>
	/// The VFP compatible Checkbox class.
	/// </summary>
	PARTIAL CLASS CheckBox INHERIT System.Windows.Forms.CheckBox

		// Common properties that all VFP Objects support
#include "Headers/VFPObject.xh"

#include "VFPProperties.xh"

#include "ControlProperties.xh"

#include "FontProperties.xh"

#include "ControlSource.xh"

#include "Headers/VFPButtonImage.xh"

		// ── Centered ─────────────────────────────────────────────────────────
		// VFP Centered = .T. centres the check mark — maps to MiddleCenter.
		// Centered = .F. restores the default MiddleLeft alignment.

		PRIVATE _centered AS LOGIC
		PROPERTY Centered AS LOGIC
			GET
				RETURN _centered
			END GET
			SET
				_centered := VALUE
				IF VALUE
					SELF:CheckAlign := System.Drawing.ContentAlignment.MiddleCenter
				ELSE
					SELF:CheckAlign := System.Drawing.ContentAlignment.MiddleLeft
				ENDIF
			END SET
		END PROPERTY

		// ── ReadOnly ─────────────────────────────────────────────────────────
		// VFP ReadOnly = .T. prevents the user from changing the check state.

		PRIVATE _readOnly AS LOGIC
		PROPERTY ReadOnly AS LOGIC
			GET
				RETURN _readOnly
			END GET
			SET
				_readOnly := VALUE
			END SET
		END PROPERTY

		PROTECTED METHOD OnClick( e AS System.EventArgs ) AS VOID
			IF _readOnly
				// Suppress the state change — restore previous CheckState
				SELF:Checked := !SELF:Checked
				RETURN
			ENDIF
			SUPER:OnClick( e )
		END METHOD

		// ── Value ────────────────────────────────────────────────────────────
		// VFP Value: 0=Unchecked, 1=Checked, 2=Indeterminate.
		// Also accepts .T./.F. for backwards compatibility.

		PROPERTY Value AS USUAL
			GET
				RETURN (USUAL)(LONG) SELF:CheckState
			END GET
			SET
				LOCAL state AS System.Windows.Forms.CheckState
				DO CASE
				CASE IsLogic(VALUE)
					state := IIF( (LOGIC)VALUE, CheckState.Checked, CheckState.Unchecked )
				CASE IsLong(VALUE)
					LOCAL n AS LONG
					n := (LONG) VALUE
					DO CASE
					CASE n == 0
						state := CheckState.Unchecked
					CASE n == 1
						state := CheckState.Checked
					OTHERWISE
						state := CheckState.Indeterminate
					END CASE
				OTHERWISE
					RETURN
				END CASE
				SELF:CheckState := state
				SELF:OnVFPProgrammaticChange()
			END SET
		END PROPERTY

		// ── ProgrammaticChange ───────────────────────────────────────────────

		PRIVATE _VFPProgrammaticChange AS VFPOverride
		[Category("VFP Events"), Description("Occurs when the value of a control is changed through code.")];
		[DefaultValue(NULL)];
		PROPERTY vfpProgrammaticChange AS STRING GET _VFPProgrammaticChange?:SendTo SET SELF:_VFPProgrammaticChange := VFPOverride{SELF, VALUE}

		PRIVATE METHOD OnVFPProgrammaticChange() AS VOID
			IF SELF:_VFPProgrammaticChange != NULL
				SELF:_VFPProgrammaticChange:Call()
			ENDIF

		// ── InteractiveChange ────────────────────────────────────────────────

		PROTECTED METHOD OnCheckedChanged( e AS System.EventArgs ) AS VOID
			SUPER:OnCheckedChanged( e )
			IF !_readOnly
				SELF:_ApplyPicture()
				SELF:OnVFPInteractiveChange( SELF, e )
			ENDIF
		END METHOD

		// WinForms CheckBox does not raise KeyPress automatically.
		// Override so vfpKeyPress subscribers fire correctly.
		PROTECTED OVERRIDE METHOD OnKeyPress(e AS System.Windows.Forms.KeyPressEventArgs) AS VOID
			SELF:OnVFPKeyPress(SELF, e)
			SUPER:OnKeyPress(e)

		CONSTRUCTOR(  ) STRICT
            SUPER()
            SELF:Size := Size{100, 17}
			RETURN

	END CLASS
END NAMESPACE // XSharp.VFP.UI
