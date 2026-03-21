// Shape.prg
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
	/// The VFP compatible Shape class.
	/// Provides shape drawing capabilities (rectangle, rounded rectangle, oval, etc.)
	/// </summary>
	PARTIAL CLASS Shape INHERIT System.Windows.Forms.Panel

		// Common properties that all VFP Objects support
		#include "Headers\VFPObject.xh"

		/// <summary>
		/// Backing fields for VFP properties
		/// </summary>
		PRIVATE _borderColor AS Color
		PRIVATE _borderWidth AS INT
		PRIVATE _fillColor AS Color
		PRIVATE _fillStyle AS INT
		PRIVATE _curvature AS INT
		PRIVATE _drawMode AS INT
		PRIVATE _vfpStyle AS INT

		/// <summary>
		/// Shape style constants
		/// </summary>
		CONSTANT SHAPE_SQUARE := 0
		CONSTANT SHAPE_RECTANGLE := 1
		CONSTANT SHAPE_OVAL := 2
		CONSTANT SHAPE_CIRCLE := 3
		CONSTANT SHAPE_ROUNDED_RECTANGLE := 4
		CONSTANT SHAPE_ROUNDED_SQUARE := 5

		/// <summary>
		/// Constructor for Shape control.
		/// </summary>
		CONSTRUCTOR(  ) STRICT
			SUPER()
			SELF:Size := Size{100, 100}
			SELF:_borderColor := Color.Black
			SELF:_borderWidth := 1
			SELF:_fillColor := Color.White
			SELF:_fillStyle := 1  // Transparent
			SELF:_curvature := 0
			SELF:_drawMode := 13  // Copy pen
			SELF:_vfpStyle := 1   // Rectangle by default
			SELF:SetStyle(ControlStyles.UserPaint, TRUE)
			SELF:SetStyle(ControlStyles.AllPaintingInWmPaint, TRUE)
			SELF:SetStyle(ControlStyles.DoubleBuffer, TRUE)
			RETURN

		#include ".\Headers\ControlProperties.xh"

		/// <summary>
		/// Gets or sets the border color of the shape.
		/// Equivalent to VFP's BorderColor property.
		/// </summary>
		/// <value>The border color. Default is black.</value>
		[Category("VFP Properties"), Description("Border color")];
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
		/// Gets or sets the border width in pixels.
		/// Equivalent to VFP's BorderWidth property.
		/// </summary>
		/// <value>The border width in pixels. Default is 1.</value>
		[Category("VFP Properties"), Description("Border width in pixels")];
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
		/// Gets or sets the fill color of the shape.
		/// Equivalent to VFP's FillColor property.
		/// </summary>
		/// <value>The fill color. Default is white.</value>
		[Category("VFP Properties"), Description("Fill color")];
		[DefaultValue(typeof(Color), "White")];
		PROPERTY FillColor AS Color
			GET
				RETURN SELF:_fillColor
			END GET
			SET
				SELF:_fillColor := VALUE
				SELF:Invalidate()
			END SET
		END PROPERTY

		/// <summary>
		/// Gets or sets the fill style of the shape.
		/// 0=Solid, 1=Transparent, 2=Horizontal Line, 3=Vertical Line, etc.
		/// Equivalent to VFP's FillStyle property.
		/// </summary>
		/// <value>The fill style. Default is 1 (Transparent).</value>
		[Category("VFP Properties"), Description("Fill style: 0=Solid, 1=Transparent")];
		[DefaultValue(1)];
		PROPERTY FillStyle AS INT
			GET
				RETURN SELF:_fillStyle
			END GET
			SET
				SELF:_fillStyle := VALUE
				SELF:Invalidate()
			END SET
		END PROPERTY

		/// <summary>
		/// Gets or sets the curvature of the shape (0-100).
		/// 0 = No curvature, 100 = Maximum curvature.
		/// Equivalent to VFP's Curvature property.
		/// </summary>
		/// <value>The curvature value (0-100). Default is 0.</value>
		[Category("VFP Properties"), Description("Curvature: 0=None, 100=Maximum")];
		[DefaultValue(0)];
		PROPERTY Curvature AS INT
			GET
				RETURN SELF:_curvature
			END GET
			SET
				IF VALUE < 0
					VALUE := 0
				ELSEIF VALUE > 100
					VALUE := 100
				ENDIF
				SELF:_curvature := VALUE
				SELF:Invalidate()
			END SET
		END PROPERTY

		/// <summary>
		/// Gets or sets the style of the shape.
		/// 0=Square, 1=Rectangle, 2=Oval, 3=Circle, 4=Rounded Rectangle, 5=Rounded Square.
		/// Equivalent to VFP's Style property.
		/// </summary>
		/// <value>The shape style. Default is 1 (Rectangle).</value>
		[Category("VFP Properties"), Description("Shape: 0=Square, 1=Rectangle, 2=Oval, 3=Circle, 4=Rounded Rectangle, 5=Rounded Square")];
		[DefaultValue(1)];
		PROPERTY Style AS INT
			GET
				RETURN SELF:_vfpStyle
			END GET
			SET
				SELF:_vfpStyle := VALUE
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
		/// Handles the Paint event to draw the shape.
		/// </summary>
		PROTECTED OVERRIDE METHOD OnPaint( e AS PaintEventArgs ) AS VOID
			LOCAL g AS Graphics
			LOCAL pen AS Pen
			LOCAL brush AS Brush
			LOCAL rect AS Rectangle

			g := e:Graphics
			pen := Pen{SELF:_borderColor, SELF:_borderWidth}
			rect := Rectangle{0, 0, SELF:Width-1, SELF:Height-1}

			// Draw fill if not transparent
			IF SELF:_fillStyle == 0  // Solid
				brush := SolidBrush{SELF:_fillColor}
			ELSE
				brush := SolidBrush{Color.Transparent}
			ENDIF

			// Draw based on style
			SWITCH SELF:_vfpStyle
				CASE 0  // Square
					SELF:DrawSquare(g, pen, brush, rect)
				CASE 1  // Rectangle
					SELF:DrawRectangle(g, pen, brush, rect)
				CASE 2  // Oval
					SELF:DrawOval(g, pen, brush, rect)
				CASE 3  // Circle
					SELF:DrawCircle(g, pen, brush, rect)
				CASE 4  // Rounded Rectangle
					SELF:DrawRoundedRectangle(g, pen, brush, rect)
				CASE 5  // Rounded Square
					SELF:DrawRoundedSquare(g, pen, brush, rect)
			END SWITCH

			pen:Dispose()
			brush:Dispose()
			RETURN

		/// <summary>
		/// Draws a square shape.
		/// </summary>
		PRIVATE METHOD DrawSquare( g AS Graphics, pen AS Pen, brush AS Brush, rect AS Rectangle ) AS VOID
			LOCAL size AS INT
			size := Math:Min(rect:Width, rect:Height)
			SELF:DrawRoundedRect(g, pen, brush, Rectangle{0, 0, size, size}, 0)

		/// <summary>
		/// Draws a rectangle shape.
		/// </summary>
		PRIVATE METHOD DrawRectangle( g AS Graphics, pen AS Pen, brush AS Brush, rect AS Rectangle ) AS VOID
			SELF:DrawRoundedRect(g, pen, brush, rect, 0)

		/// <summary>
		/// Draws an oval shape.
		/// </summary>
		PRIVATE METHOD DrawOval( g AS Graphics, pen AS Pen, brush AS Brush, rect AS Rectangle ) AS VOID
			g:FillEllipse(brush, rect)
			g:DrawEllipse(pen, rect)

		/// <summary>
		/// Draws a circle shape.
		/// </summary>
		PRIVATE METHOD DrawCircle( g AS Graphics, pen AS Pen, brush AS Brush, rect AS Rectangle ) AS VOID
			LOCAL size AS INT
			LOCAL circleRect AS Rectangle
			size := Math:Min(rect:Width, rect:Height)
			circleRect := Rectangle{0, 0, size, size}
			g:FillEllipse(brush, circleRect)
			g:DrawEllipse(pen, circleRect)

		/// <summary>
		/// Draws a rounded rectangle shape.
		/// </summary>
		PRIVATE METHOD DrawRoundedRectangle( g AS Graphics, pen AS Pen, brush AS Brush, rect AS Rectangle ) AS VOID
			LOCAL radius AS INT
			radius := (SELF:_curvature * Math:Min(rect:Width, rect:Height)) / 200
			SELF:DrawRoundedRect(g, pen, brush, rect, radius)

		/// <summary>
		/// Draws a rounded square shape.
		/// </summary>
		PRIVATE METHOD DrawRoundedSquare( g AS Graphics, pen AS Pen, brush AS Brush, rect AS Rectangle ) AS VOID
			LOCAL size AS INT
			LOCAL squareRect AS Rectangle
			LOCAL radius AS INT
			size := Math:Min(rect:Width, rect:Height)
			squareRect := Rectangle{0, 0, size, size}
			radius := (SELF:_curvature * size) / 200
			SELF:DrawRoundedRect(g, pen, brush, squareRect, radius)

		/// <summary>
		/// Helper method to draw a rounded rectangle.
		/// </summary>
		PRIVATE METHOD DrawRoundedRect( g AS Graphics, pen AS Pen, brush AS Brush, rect AS Rectangle, radius AS INT ) AS VOID
			IF radius <= 0
				g:FillRectangle(brush, rect)
				g:DrawRectangle(pen, rect)
			ELSE
				// Create rounded rectangle path
				VAR path := System.Drawing.Drawing2D.GraphicsPath{}
				path:AddArc(rect:X, rect:Y, radius, radius, 180, 90)
				path:AddArc(rect:X + rect:Width - radius, rect:Y, radius, radius, 270, 90)
				path:AddArc(rect:X + rect:Width - radius, rect:Y + rect:Height - radius, radius, radius, 0, 90)
				path:AddArc(rect:X, rect:Y + rect:Height - radius, radius, radius, 90, 90)
				path:CloseFigure()
				g:FillPath(brush, path)
				g:DrawPath(pen, path)
				path:Dispose()
			ENDIF

	END CLASS

END NAMESPACE
