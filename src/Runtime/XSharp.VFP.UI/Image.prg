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

		/// <summary>
		/// Backing fields for VFP properties
		/// </summary>
		PRIVATE _picture AS STRING
		PRIVATE _stretch AS INT

		/// <summary>
		/// Constructor for Image control.
		/// </summary>
		CONSTRUCTOR(  ) STRICT
			SUPER()
			SELF:Size := System.Drawing.Size{100, 100}
			SELF:_picture := ""
			SELF:_stretch := 0
			RETURN

		#include ".\Headers\ControlProperties.xh"
        #include ".\Headers\ControlFocus.xh"
		/// <summary>
		/// Gets or sets the path to the image file.
		/// Equivalent to VFP's Picture property.
		/// </summary>
		/// <value>The file path to the image.</value>
		[Category("VFP Properties"), Description("Path to the image file")];
		[DefaultValue("")];
		PROPERTY Picture AS STRING
			GET
				RETURN SELF:_picture
			END GET
			SET
				SELF:_picture := VALUE
				IF !String.IsNullOrEmpty(VALUE)
					TRY
						SELF:Image := System.Drawing.Image.FromFile(VALUE)
					CATCH ex AS Exception
						// Ignore file not found errors
						SELF:Image := NULL
					END TRY
				ELSE
					SELF:Image := NULL
				ENDIF
			END SET
		END PROPERTY

		/// <summary>
		/// Gets or sets how the image is sized within the control.
		/// Equivalent to VFP's Stretch property.
		/// </summary>
		/// <value>0=Clip, 1=Isometric, 2=Stretch. Default is 0.</value>
		[Category("VFP Properties"), Description("Image sizing: 0=Clip, 1=Isometric, 2=Stretch")];
		[DefaultValue(0)];
		PROPERTY Stretch AS INT
			GET
				RETURN SELF:_stretch
			END GET
			SET
				SELF:_stretch := VALUE
				SWITCH VALUE
					CASE 0
						SELF:SizeMode := PictureBoxSizeMode.Normal
					CASE 1
						SELF:SizeMode := PictureBoxSizeMode.Zoom
					CASE 2
						SELF:SizeMode := PictureBoxSizeMode.StretchImage
				END SWITCH
			END SET
		END PROPERTY

		/// <summary>
		/// Gets or sets whether the image is centered in the control.
		/// Equivalent to VFP's Centered property.
		/// </summary>
		/// <value>True if centered; otherwise, false.</value>
		[Category("VFP Properties"), Description("Center the image")];
		PROPERTY Centered AS LOGIC AUTO

	END CLASS
END NAMESPACE // XSharp.VFP.UI
