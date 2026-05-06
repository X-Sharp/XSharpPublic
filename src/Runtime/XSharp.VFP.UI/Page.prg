// Page.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.

USING System
USING System.Windows.Forms
USING System.Drawing
USING System.ComponentModel

BEGIN NAMESPACE XSharp.VFP.UI

	/// <summary>
	/// The VFP compatible Page class.
	/// Maps to System.Windows.Forms.TabPage.
	/// </summary>
	PARTIAL CLASS Page INHERIT System.Windows.Forms.TabPage

		// Note: VFPObject.xh and VFPProperties.xh are included by Page.generated.prg
		// (via VFPControl.xh + VFPContainer.xh) — do not include again here.

		#include "ControlProperties.xh"

		#include "FontProperties.xh"

		CONSTRUCTOR() STRICT
			SUPER()
			SELF:Size := Size{290, 170}

		// ── Caption ───────────────────────────────────────────────────────────
		// VFP Caption is the tab label text; maps to TabPage.Text.
		// ControlProperties.xh already maps Caption -> Text via the base include,
		// but TabPage uses Text as the tab header — no override needed.

		// ── PageOrder ─────────────────────────────────────────────────────────
		// Returns/sets the 1-based position of this page within its parent PageFrame.
		PROPERTY PageOrder AS USUAL
			GET
				VAR parent := SELF:Parent ASTYPE System.Windows.Forms.TabControl
				IF parent != NULL_OBJECT
					RETURN parent:TabPages:IndexOf(SELF) + 1
				ENDIF
				RETURN 1
			END GET
			SET
				VAR parent := SELF:Parent ASTYPE System.Windows.Forms.TabControl
				IF parent == NULL_OBJECT
					RETURN
				ENDIF
				VAR targetIdx := (INT) VALUE - 1
				VAR currentIdx := parent:TabPages:IndexOf(SELF)
				IF targetIdx == currentIdx .OR. targetIdx < 0 .OR. targetIdx >= parent:TabPages:Count
					RETURN
				ENDIF
				// WinForms TabControl does not support direct reordering;
				// simulate by remove + re-insert.
				parent:TabPages:Remove(SELF)
				parent:TabPages:Insert(targetIdx, SELF)
			END SET
		END PROPERTY

		// ── Picture ───────────────────────────────────────────────────────────
		// VFP Picture sets a background image on the page.
		PRIVATE _picture AS STRING
		PROPERTY Picture AS STRING
			GET
				RETURN SELF:_picture
			END GET
			SET
				SELF:_picture := VALUE
				IF !String.IsNullOrEmpty(VALUE)
					SELF:BackgroundImage := VFPTools.ImageFromFile(VALUE)
					SELF:BackgroundImageLayout := ImageLayout.Stretch
				ELSE
					SELF:BackgroundImage := NULL_OBJECT
				ENDIF
			END SET
		END PROPERTY

		// ── Activate / Deactivate events ──────────────────────────────────────
		// Fired by the parent PageFrame when this page is selected/deselected.
		PRIVATE _VFPActivate   AS VFPOverride
		PRIVATE _VFPDeactivate AS VFPOverride

		[System.ComponentModel.Category("VFP Events")];
		[System.ComponentModel.DefaultValue(NULL)];
		PROPERTY vfpActivate AS STRING
			GET
				RETURN SELF:_VFPActivate?:SendTo
			END GET
			SET
				SELF:_VFPActivate := VFPOverride{ SELF, VALUE }
			END SET
		END PROPERTY

		[System.ComponentModel.Category("VFP Events")];
		[System.ComponentModel.DefaultValue(NULL)];
		PROPERTY vfpDeactivate AS STRING
			GET
				RETURN SELF:_VFPDeactivate?:SendTo
			END GET
			SET
				SELF:_VFPDeactivate := VFPOverride{ SELF, VALUE }
			END SET
		END PROPERTY

		METHOD FireActivate() AS VOID STRICT
			IF SELF:_VFPActivate != NULL
				SELF:_VFPActivate:Call()
			ENDIF

		METHOD FireDeactivate() AS VOID STRICT
			IF SELF:_VFPDeactivate != NULL
				SELF:_VFPDeactivate:Call()
			ENDIF

		// ── BackStyle ─────────────────────────────────────────────────────────
		PRIVATE _backStyle := 1 AS INT
		PROPERTY BackStyle AS INT
			GET ; RETURN _backStyle ; END GET
			SET
				_backStyle := VALUE
				SELF:BackColor := IIF(VALUE == 0, System.Drawing.Color.Transparent, System.Drawing.SystemColors.Control)
			END SET
		END PROPERTY

		// ── Resize / Moved events ─────────────────────────────────────────────
		PRIVATE _VFPResize AS VFPOverride
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
		PROPERTY vfpResize AS STRING GET _VFPResize?:SendTo SET _VFPResize := VFPOverride{SELF, VALUE}

		PROTECTED OVERRIDE METHOD OnResize(e AS System.EventArgs) AS VOID
			SUPER:OnResize(e)
			IF SELF:_VFPResize != NULL ; SELF:_VFPResize:Call() ; ENDIF

		PRIVATE _VFPMoved AS VFPOverride
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
		PROPERTY vfpMoved AS STRING GET _VFPMoved?:SendTo SET _VFPMoved := VFPOverride{SELF, VALUE}

		PROTECTED OVERRIDE METHOD OnMove(e AS System.EventArgs) AS VOID
			SUPER:OnMove(e)
			IF SELF:_VFPMoved != NULL ; SELF:_VFPMoved:Call() ; ENDIF

	END CLASS

END NAMESPACE
