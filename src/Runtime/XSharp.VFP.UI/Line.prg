// Line.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.


USING System
USING System.Collections.Generic
USING System.Text
USING System.Windows.Forms
USING System.Drawing
USING System.ComponentModel

BEGIN NAMESPACE XSharp.VFP.UI

	/// <summary>
	/// The VFP compatible Line class.
	/// Draws a line on the form.
	/// </summary>
	PARTIAL CLASS Line INHERIT System.Windows.Forms.Control

		// Common properties that all VFP Objects support
		#include "Headers\VFPObject.xh"

		/// <summary>
		/// Backing fields for VFP properties
		/// </summary>
		PRIVATE _borderColor AS Color
		PRIVATE _borderWidth AS INT
		PRIVATE _drawMode AS INT
		PRIVATE _lineSlant AS STRING

		/// <summary>
		/// Constructor for Line control.
		/// </summary>
		CONSTRUCTOR(  ) STRICT
			SUPER()
			SELF:Size := Size{100, 0}
			SELF:_borderColor := Color.Black
			SELF:_borderWidth := 1
			SELF:_drawMode := 13  // Copy pen
			SELF:_lineSlant := "/"  // Forward slash by default
			SELF:SetStyle(ControlStyles.UserPaint, TRUE)
			SELF:SetStyle(ControlStyles.AllPaintingInWmPaint, TRUE)
			SELF:SetStyle(ControlStyles.DoubleBuffer, TRUE)
			SELF:SetStyle(ControlStyles.Selectable, FALSE)
			RETURN

		#include ".\Headers\ControlProperties.xh"

		/// <summary>
		/// Gets or sets the border color of the line.
		/// Equivalent to VFP's BorderColor property.
		/// </summary>
		/// <value>The border color. Default is black.</value>
		[Category("VFP Properties"), Description("Line color")];
		[DefaultValue(typeof(Color), "Black")];
		PROPERTY BorderColor AS Color
			GET
				RETURN SELF:_borderColor
			END GET
			SET
				SELF:_borderColor := VALUE
				SELF:Invalidate()
			END SET
		END PROPERTY

		/// <summary>
		/// Gets or sets the border width (thickness) of the line.
		/// Equivalent to VFP's BorderWidth property.
		/// </summary>
		/// <value>The line thickness in pixels. Default is 1.</value>
		[Category("VFP Properties"), Description("Line thickness in pixels")];
		[DefaultValue(1)];
		PROPERTY BorderWidth AS INT
			GET
				RETURN SELF:_borderWidth
			END GET
			SET
				SELF:_borderWidth := VALUE
				SELF:Invalidate()
			END SET
		END PROPERTY

		/// <summary>
		/// Gets or sets the draw mode.
		/// Equivalent to VFP's DrawMode property.
		/// </summary>
		/// <value>The draw mode. Default is 13 (Copy Pen).</value>
		[Category("VFP Properties"), Description("Draw mode")];
		[DefaultValue(13)];
		PROPERTY DrawMode AS INT
			GET
				RETURN SELF:_drawMode
			END GET
			SET
				SELF:_drawMode := VALUE
			END SET
		END PROPERTY

		/// <summary>
		/// Gets or sets the line slant direction.
		/// "/" for forward slash (bottom-left to top-right)
		/// "\" for backslash (top-left to bottom-right)
		/// Equivalent to VFP's LineSlant property.
		/// </summary>
		/// <value>The line slant direction. Default is "/".</value>
		[Category("VFP Properties"), Description("Line slant: / or \\")];
		[DefaultValue("/")];
		PROPERTY LineSlant AS STRING
			GET
				RETURN SELF:_lineSlant
			END GET
			SET
				IF VALUE == "/" .OR. VALUE == "\"
					SELF:_lineSlant := VALUE
					SELF:Invalidate()
				ENDIF
			END SET
		END PROPERTY

		/// <summary>
		/// Handles the Paint event to draw the line.
		/// </summary>
		PROTECTED OVERRIDE METHOD OnPaint( e AS PaintEventArgs ) AS VOID
			LOCAL g AS Graphics
			LOCAL pen AS Pen
			LOCAL startPoint AS Point
			LOCAL endPoint AS Point

			g := e:Graphics
			pen := Pen{SELF:_borderColor, SELF:_borderWidth}

			// Determine start and end points based on LineSlant
			IF SELF:_lineSlant == "/"
				// Forward slash: bottom-left to top-right
				startPoint := Point{0, SELF:Height - 1}
				endPoint := Point{SELF:Width - 1, 0}
			ELSE
				// Backslash: top-left to bottom-right
				startPoint := Point{0, 0}
				endPoint := Point{SELF:Width - 1, SELF:Height - 1}
			ENDIF

			g:DrawLine(pen, startPoint, endPoint)

			pen:Dispose()
			RETURN

	END CLASS

END NAMESPACE
