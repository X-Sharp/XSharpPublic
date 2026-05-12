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
	/// The VFP compatible Image class.
	/// </summary>
	PARTIAL CLASS Image INHERIT System.Windows.Forms.PictureBox

		// Common properties that all VFP Objects support
		#include "Headers/VFPObject.xh"

		CONSTRUCTOR(  ) STRICT
            SUPER()
            SELF:Size := System.Drawing.Size{100, 100}
			SELF:SizeMode := System.Windows.Forms.PictureBoxSizeMode.Normal
			RETURN

		// Picture — loads image from file path using VFPTools.ImageFromFile
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

		// Stretch — maps VFP values to PictureBoxSizeMode:
		//   0 = Clip (Normal)
		//   1 = Isometric / proportional (Zoom)
		//   2 = Stretch (StretchImage)
		//   3 = AutoSize (control resizes to image)
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
		// Stored and applied whenever Picture is loaded or RotateFlip is changed.

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
		// VFP: 0=None, 1=FixedSingle

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
