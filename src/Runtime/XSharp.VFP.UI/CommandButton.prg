// CommandButton.prg
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
		PRIVATE _vfpStyle        AS INT
		PRIVATE _cancel          AS LOGIC
		PRIVATE _default         AS LOGIC
		PRIVATE _picturePosition AS LONG

		// Common properties that all VFP Objects support
		#include "Headers/VFPObject.xh"

		#include "VFPProperties.xh"

		#include "ButtonControlProperties.xh"

		#include "FontProperties.xh"


		CONSTRUCTOR(  ) STRICT
			SUPER()

			SELF:SetStyle( ControlStyles.SupportsTransparentBackColor, TRUE)
			SELF:BackColor := Color.Transparent
			SELF:_vfpStyle := 0
			SELF:Size := Size{100, 17}

		PROTECTED METHOD OnParentChanged(e AS System.EventArgs) AS VOID
			SUPER:OnParentChanged(e)
			SELF:ReWireFormButtons()

		// ── Style ─────────────────────────────────────────────────────────────
		// 0 = Standard (default WinForms button)
		// 1 = Graphical: borderless flat button; image replaces text rendering
		[System.ComponentModel.EditorBrowsable(System.ComponentModel.EditorBrowsableState.Never)];
		PROPERTY Style AS INT
			GET
				RETURN SELF:_vfpStyle
			END GET
			SET
				SELF:_vfpStyle := VALUE
				IF VALUE == 1
					// Graphical mode: remove border, transparent bg
					SELF:FlatStyle                        := FlatStyle.Flat
					SELF:FlatAppearance:BorderSize        := 0
					SELF:FlatAppearance:MouseOverBackColor := Color.Transparent
					SELF:FlatAppearance:MouseDownBackColor := Color.Transparent
					SELF:BackColor                        := Color.Transparent
					SELF:TextAlign                        := ContentAlignment.BottomCenter
					// Re-apply cached images so they take effect immediately
					SELF:ApplyGraphicalImages()
				ELSE
					// Restore standard appearance
					SELF:FlatStyle  := FlatStyle.Standard
					SELF:BackColor  := Color.Transparent
					SELF:TextAlign  := ContentAlignment.MiddleCenter
					// Reload normal image from the Picture file path
					IF !String.IsNullOrEmpty(SELF:_picture)
						SELF:Image := VFPTools.ImageFromFile(SELF:_picture)
					ENDIF
				ENDIF
			END SET
		END PROPERTY

		// ── Mouse-down / mouse-up: swap to DownPicture ────────────────────────
		PROTECTED METHOD OnMouseDown(e AS System.Windows.Forms.MouseEventArgs) AS VOID
			SUPER:OnMouseDown(e)
			IF !String.IsNullOrEmpty(SELF:DownPicture)
				SELF:Image := VFPTools.ImageFromFile(SELF:DownPicture)
			ENDIF

		PROTECTED METHOD OnMouseUp(e AS System.Windows.Forms.MouseEventArgs) AS VOID
			SUPER:OnMouseUp(e)
			SELF:ApplyGraphicalImages()

		// ── EnabledChanged: swap picture + apply DisabledBackColor/ForeColor ──
		PROTECTED METHOD OnEnabledChanged(e AS System.EventArgs) AS VOID
			SUPER:OnEnabledChanged(e)
			SELF:ApplyGraphicalImages()
			IF !SELF:Enabled
				IF SELF:DisabledBackColor != 0
					SELF:BackColor := VFPTools.ColorFromVFP(SELF:DisabledBackColor)
				ENDIF
				IF SELF:DisabledForeColor != 0
					SELF:ForeColor := VFPTools.ColorFromVFP(SELF:DisabledForeColor)
				ENDIF
			ELSE
				SELF:ResetBackColor()
				SELF:ResetForeColor()
			ENDIF

		// ── Helper: apply correct image for current state ─────────────────────
		// Priority: DisabledPicture (when disabled) > DownPicture (on mouse-down,
		// handled separately) > Picture (normal).
		PRIVATE METHOD ApplyGraphicalImages() AS VOID
			IF !SELF:Enabled .AND. !String.IsNullOrEmpty(SELF:DisabledPicture)
				SELF:Image := VFPTools.ImageFromFile(SELF:DisabledPicture)
			ELSEIF !String.IsNullOrEmpty(SELF:_picture)
				SELF:Image := VFPTools.ImageFromFile(SELF:_picture)
			ELSE
				SELF:Image := NULL_OBJECT
			ENDIF

		// ── PicturePosition ───────────────────────────────────────────────────
		// VFP: 0-2 image left, 3-5 image above, 6-8 image right, 9-11 image below,
		//      12-15 corner overlay, 16 center/background.
		PROPERTY PicturePosition AS LONG
			GET
				RETURN SELF:_picturePosition
			END GET
			SET
				SELF:_picturePosition := VALUE
				SWITCH VALUE
				CASE 0  ; SELF:TextImageRelation := TextImageRelation.ImageBeforeText ; SELF:ImageAlign := ContentAlignment.TopLeft    ; SELF:TextAlign := ContentAlignment.MiddleRight
				CASE 1  ; SELF:TextImageRelation := TextImageRelation.ImageBeforeText ; SELF:ImageAlign := ContentAlignment.MiddleLeft  ; SELF:TextAlign := ContentAlignment.MiddleRight
				CASE 2  ; SELF:TextImageRelation := TextImageRelation.ImageBeforeText ; SELF:ImageAlign := ContentAlignment.BottomLeft  ; SELF:TextAlign := ContentAlignment.MiddleRight
				CASE 3  ; SELF:TextImageRelation := TextImageRelation.ImageAboveText  ; SELF:ImageAlign := ContentAlignment.TopLeft    ; SELF:TextAlign := ContentAlignment.BottomCenter
				CASE 4  ; SELF:TextImageRelation := TextImageRelation.ImageAboveText  ; SELF:ImageAlign := ContentAlignment.TopCenter  ; SELF:TextAlign := ContentAlignment.BottomCenter
				CASE 5  ; SELF:TextImageRelation := TextImageRelation.ImageAboveText  ; SELF:ImageAlign := ContentAlignment.TopRight   ; SELF:TextAlign := ContentAlignment.BottomCenter
				CASE 6  ; SELF:TextImageRelation := TextImageRelation.TextBeforeImage ; SELF:ImageAlign := ContentAlignment.TopRight   ; SELF:TextAlign := ContentAlignment.MiddleLeft
				CASE 7  ; SELF:TextImageRelation := TextImageRelation.TextBeforeImage ; SELF:ImageAlign := ContentAlignment.MiddleRight ; SELF:TextAlign := ContentAlignment.MiddleLeft
				CASE 8  ; SELF:TextImageRelation := TextImageRelation.TextBeforeImage ; SELF:ImageAlign := ContentAlignment.BottomRight ; SELF:TextAlign := ContentAlignment.MiddleLeft
				CASE 9  ; SELF:TextImageRelation := TextImageRelation.TextAboveImage  ; SELF:ImageAlign := ContentAlignment.BottomLeft  ; SELF:TextAlign := ContentAlignment.TopCenter
				CASE 10 ; SELF:TextImageRelation := TextImageRelation.TextAboveImage  ; SELF:ImageAlign := ContentAlignment.BottomCenter ; SELF:TextAlign := ContentAlignment.TopCenter
				CASE 11 ; SELF:TextImageRelation := TextImageRelation.TextAboveImage  ; SELF:ImageAlign := ContentAlignment.BottomRight ; SELF:TextAlign := ContentAlignment.TopCenter
				CASE 12 ; SELF:TextImageRelation := TextImageRelation.Overlay         ; SELF:ImageAlign := ContentAlignment.TopLeft
				CASE 13 ; SELF:TextImageRelation := TextImageRelation.Overlay         ; SELF:ImageAlign := ContentAlignment.TopRight
				CASE 14 ; SELF:TextImageRelation := TextImageRelation.Overlay         ; SELF:ImageAlign := ContentAlignment.BottomLeft
				CASE 15 ; SELF:TextImageRelation := TextImageRelation.Overlay         ; SELF:ImageAlign := ContentAlignment.BottomRight
				OTHERWISE ; SELF:TextImageRelation := TextImageRelation.Overlay       ; SELF:ImageAlign := ContentAlignment.MiddleCenter
				END SWITCH
			END SET
		END PROPERTY

		// ── WordWrap ──────────────────────────────────────────────────────────
		// .T. = text wraps at button boundary (AutoSize off, natural wrap).
		// .F. = single line with AutoEllipsis when text overflows.
		PROPERTY WordWrap AS LOGIC
			GET
				RETURN !SELF:AutoEllipsis
			END GET
			SET
				IF VALUE
					SELF:AutoEllipsis := FALSE
					SELF:AutoSize     := FALSE
				ELSE
					SELF:AutoEllipsis := TRUE
					SELF:AutoSize     := FALSE
				ENDIF
			END SET
		END PROPERTY




		/// <summary>
		/// When TRUE, pressing Escape fires this button's Click.
		/// Wires itself as Form.CancelButton on the owning Form.
		/// </summary>
		[System.ComponentModel.DefaultValue(FALSE)];
		PROPERTY Cancel AS LOGIC
			GET
				RETURN SELF:_cancel
			END GET
			SET
				SELF:_cancel := VALUE
				VAR oForm := SELF:FindForm() ASTYPE XSharp.VFP.UI.Form
				IF oForm != NULL_OBJECT
					IF VALUE
						oForm:CancelButton := SELF
					ELSEIF oForm:CancelButton == (System.Windows.Forms.IButtonControl) SELF
						oForm:CancelButton := NULL_OBJECT
					ENDIF
				ENDIF
			END SET
		END PROPERTY

		/// <summary>
		/// When TRUE, pressing Enter fires this button's Click.
		/// Wires itself as Form.AcceptButton on the owning Form.
		/// </summary>
		[System.ComponentModel.DefaultValue(FALSE)];
		PROPERTY Default AS LOGIC
			GET
				RETURN SELF:_default
			END GET
			SET
				SELF:_default := VALUE
				VAR oForm := SELF:FindForm() ASTYPE XSharp.VFP.UI.Form
				IF oForm != NULL_OBJECT
					IF VALUE
						oForm:AcceptButton := SELF
					ELSEIF oForm:AcceptButton == (System.Windows.Forms.IButtonControl) SELF
						oForm:AcceptButton := NULL_OBJECT
					ENDIF
				ENDIF
			END SET
		END PROPERTY

		/// <summary>
		/// Called by the Form after all controls are sited (e.g. from Form.Init),
		/// so Cancel/Default wiring fires even when the button was added before the
		/// form handle existed.
		/// </summary>
		METHOD ReWireFormButtons() AS VOID STRICT
			IF SELF:_cancel
				SELF:Cancel := TRUE
			ENDIF
			IF SELF:_default
				SELF:Default := TRUE
			ENDIF
		END METHOD

	END CLASS

END NAMESPACE
