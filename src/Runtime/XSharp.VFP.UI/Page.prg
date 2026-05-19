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
	/// VFP-compatible tab page that wraps <see cref="System.Windows.Forms.TabPage"/>.<br/>
	/// Hosted inside a <see cref="PageFrame"/>. Supports VFP properties: <see cref="PageOrder"/>
	/// (1-based reordering), <see cref="Picture"/> (background image), <see cref="BackStyle"/>
	/// (0=Transparent/1=Opaque), and events <see cref="vfpActivate"/> / <see cref="vfpDeactivate"/>
	/// fired by the parent PageFrame when this page is selected or deselected.
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
		/// <summary>1-based position of this page within its parent <see cref="PageFrame"/>. Setting reorders the page by removing and re-inserting it at the target index.</summary>
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
		PRIVATE _picture AS STRING
		/// <summary>Path to a background image for the page. Setting loads the image via <c>VFPTools.ImageFromFile</c> and stretches it to fill the page; clearing restores no-image.</summary>
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

		/// <summary>Name of the VFP method called when this page becomes the active tab. Fired by <see cref="FireActivate"/> from the parent <see cref="PageFrame"/>.</summary>
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

		/// <summary>Name of the VFP method called when this page is deselected. Fired by <see cref="FireDeactivate"/> from the parent <see cref="PageFrame"/>.</summary>
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

		/// <summary>Dispatches the <see cref="vfpActivate"/> handler. Called by the parent <see cref="PageFrame"/> on <c>SelectedIndexChanged</c>.</summary>
		METHOD FireActivate() AS VOID STRICT
			IF SELF:_VFPActivate != NULL
				SELF:_VFPActivate:Call()
			ENDIF

		/// <summary>Dispatches the <see cref="vfpDeactivate"/> handler. Called by the parent <see cref="PageFrame"/> on <c>SelectedIndexChanged</c>.</summary>
		METHOD FireDeactivate() AS VOID STRICT
			IF SELF:_VFPDeactivate != NULL
				SELF:_VFPDeactivate:Call()
			ENDIF

		// ── BackStyle ─────────────────────────────────────────────────────────
		PRIVATE _backStyle := 1 AS INT
		/// <summary>VFP BackStyle: 0=Transparent (sets <c>BackColor</c> to <c>Transparent</c>), 1=Opaque/default (restores <c>SystemColors.Control</c>).</summary>
		PROPERTY BackStyle AS INT
			GET ; RETURN _backStyle ; END GET
			SET
				_backStyle := VALUE
				SELF:BackColor := IIF(VALUE == 0, System.Drawing.Color.Transparent, System.Drawing.SystemColors.Control)
			END SET
		END PROPERTY

		// ── Resize / Moved events ─────────────────────────────────────────────
		PRIVATE _VFPResize AS VFPOverride
		/// <summary>Name of the VFP method called when this page is resized.</summary>
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
		PROPERTY vfpResize AS STRING GET _VFPResize?:SendTo SET _VFPResize := VFPOverride{SELF, VALUE}

		/// <summary>Fires the <c>vfpResize</c> handler when the page is resized.</summary>
		PROTECTED OVERRIDE METHOD OnResize(e AS System.EventArgs) AS VOID
			SUPER:OnResize(e)
			IF SELF:_VFPResize != NULL ; SELF:_VFPResize:Call() ; ENDIF

		PRIVATE _VFPMoved AS VFPOverride
		/// <summary>Name of the VFP method called when this page is moved.</summary>
		[System.ComponentModel.Category("VFP Events"), System.ComponentModel.DefaultValue("")];
		PROPERTY vfpMoved AS STRING GET _VFPMoved?:SendTo SET _VFPMoved := VFPOverride{SELF, VALUE}

		/// <summary>Fires the <c>vfpMoved</c> handler when the page is moved.</summary>
		PROTECTED OVERRIDE METHOD OnMove(e AS System.EventArgs) AS VOID
			SUPER:OnMove(e)
			IF SELF:_VFPMoved != NULL ; SELF:_VFPMoved:Call() ; ENDIF

	END CLASS

END NAMESPACE
