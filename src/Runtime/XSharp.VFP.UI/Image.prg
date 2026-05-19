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

BEGIN NAMESPACE XSharp.VFP.UI

	/// <summary>
	/// VFP-compatible image display control that wraps <see cref="System.Windows.Forms.PictureBox"/>.<br/>
	/// Loads images from file paths via <see cref="Picture"/> (uses <see cref="VFPTools.ImageFromFile"/>),
	/// maps VFP's <see cref="Stretch"/> values (0–3) to <c>PictureBoxSizeMode</c>,
	/// supports arbitrary <see cref="RotateFlip"/> transforms (<c>System.Drawing.RotateFlipType</c> codes),
	/// and exposes a VFP-numeric <see cref="BorderStyle"/> (0=None, 1=FixedSingle).
	/// </summary>
	PARTIAL CLASS Image INHERIT System.Windows.Forms.PictureBox

		// Common properties that all VFP Objects support
		#include "Headers/VFPObject.xh"

		CONSTRUCTOR(  ) STRICT
            SUPER()
            SELF:Size := System.Drawing.Size{100, 100}
			SELF:SizeMode := System.Windows.Forms.PictureBoxSizeMode.Normal
			RETURN

		/// <summary>
		/// File path of the image to display. Setting this property loads the image via
		/// <see cref="VFPTools.ImageFromFile"/> and applies any pending <see cref="RotateFlip"/>
		/// transform. Set to an empty string to clear the image.
		/// </summary>
		PRIVATE _picture AS STRING
		PROPERTY Picture AS STRING
			GET
				IF SELF:_picture == NULL
					SELF:_picture := ""
				ENDIF
				RETURN SELF:_picture
			END GET
			SET
				SELF:_picture := VALUE
				IF !STRING.IsNullOrEmpty(VALUE)
					SELF:Image := VFPTools.ImageFromFile( VALUE )
					IF SELF:_rotateFlip != 0 .AND. SELF:Image != NULL
						SELF:Image:RotateFlip((System.Drawing.RotateFlipType) SELF:_rotateFlip)
					ENDIF
				ELSE
					SELF:Image := NULL
				ENDIF
			END SET
		END PROPERTY

		/// <summary>
		/// Controls how the image is sized within the control:<br/>
		/// 0 = Clip — image shown at its natural size, clipped if larger than the control (<c>Normal</c>);<br/>
		/// 1 = Isometric — image scaled proportionally to fit (<c>Zoom</c>);<br/>
		/// 2 = Stretch — image scaled to fill the control exactly (<c>StretchImage</c>);<br/>
		/// 3 = AutoSize — control resizes to match the image (<c>AutoSize</c>).
		/// </summary>
		PROPERTY Stretch AS LONG
			GET
				DO CASE
				CASE SELF:SizeMode == System.Windows.Forms.PictureBoxSizeMode.Zoom
					RETURN 1
				CASE SELF:SizeMode == System.Windows.Forms.PictureBoxSizeMode.StretchImage
					RETURN 2
				CASE SELF:SizeMode == System.Windows.Forms.PictureBoxSizeMode.AutoSize
					RETURN 3
				OTHERWISE
					RETURN 0
				ENDCASE
			END GET
			SET
				DO CASE
				CASE VALUE == 1
					SELF:SizeMode := System.Windows.Forms.PictureBoxSizeMode.Zoom
				CASE VALUE == 2
					SELF:SizeMode := System.Windows.Forms.PictureBoxSizeMode.StretchImage
				CASE VALUE == 3
					SELF:SizeMode := System.Windows.Forms.PictureBoxSizeMode.AutoSize
				OTHERWISE
					SELF:SizeMode := System.Windows.Forms.PictureBoxSizeMode.Normal
				ENDCASE
			END SET
		END PROPERTY

#include "ControlProperties.xh"

		// ── RotateFlip ───────────────────────────────────────────────────────
		/// <summary>
		/// Rotation/flip transform applied to the image, using <see cref="System.Drawing.RotateFlipType"/>
		/// integer codes (0=none, 1=90°CW, 2=180°, 3=270°CW, 4=FlipX, …).<br/>
		/// Changing this reloads the image from <see cref="Picture"/> to avoid stacking
		/// transforms on an already-transformed bitmap.
		/// </summary>
		PRIVATE _rotateFlip AS LONG

		PROPERTY RotateFlip AS LONG
			GET
				RETURN _rotateFlip
			END GET
			SET
				_rotateFlip := VALUE
				// Reload from source to avoid stacking transforms on the existing image
				IF !STRING.IsNullOrEmpty(SELF:_picture)
					SELF:Image := VFPTools.ImageFromFile(SELF:_picture)
					IF VALUE != 0 .AND. SELF:Image != NULL
						SELF:Image:RotateFlip((System.Drawing.RotateFlipType) VALUE)
					ENDIF
					SELF:Invalidate()
				ENDIF
			END SET
		END PROPERTY

		// ── BorderStyle ──────────────────────────────────────────────────────
		/// <summary>VFP BorderStyle: 0=None, 1=FixedSingle. Wraps the base <c>PictureBox.BorderStyle</c> as a VFP-numeric property.</summary>
		NEW PROPERTY BorderStyle AS LONG
			GET
				IF SUPER:BorderStyle == System.Windows.Forms.BorderStyle.FixedSingle
					RETURN 1
				ENDIF
				RETURN 0
			END GET
			SET
				IF VALUE == 1
					SUPER:BorderStyle := System.Windows.Forms.BorderStyle.FixedSingle
				ELSE
					SUPER:BorderStyle := System.Windows.Forms.BorderStyle.None
				ENDIF
			END SET
		END PROPERTY

	END CLASS
END NAMESPACE // XSharp.VFP.UI
