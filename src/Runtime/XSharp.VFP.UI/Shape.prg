// Shape.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.

USING System
USING System.Windows.Forms
USING System.Drawing
USING System.Drawing.Drawing2D
USING System.ComponentModel

BEGIN NAMESPACE XSharp.VFP.UI

	/// <summary>
	/// The VFP compatible Shape class.
	/// Draws a rectangle, square, ellipse, circle, or rounded rectangle.
	/// VFP Style: 0=Rectangle, 1=Square, 2=Ellipse, 3=Circle,
	///            4=Rounded Rectangle, 5=Rounded Square.
	/// </summary>
	PARTIAL CLASS Shape INHERIT System.Windows.Forms.UserControl

		// Note: VFPObject.xh is included by Shape.generated.prg — do not include again here.

		#include "ControlProperties.xh"

		CONSTRUCTOR() STRICT
			SUPER()
			SELF:SetStyle( ControlStyles.SupportsTransparentBackColor ;
			             | ControlStyles.AllPaintingInWmPaint ;
			             | ControlStyles.UserPaint, TRUE )
			SELF:BackColor     := Color.Transparent
			SELF:_borderColor  := ColorTranslator.ToOle( Color.Black )
			SELF:_borderWidth  := 1
			SELF:_curvature    := 0
			SELF:_fillColor    := ColorTranslator.ToOle( Color.White )
			SELF:_fillStyle    := 1   // 1 = Transparent (VFP default)
			SELF:_style        := 0   // Rectangle
			SELF:Size          := Size{100, 60}

		// ── BorderColor ───────────────────────────────────────────────────────
		PRIVATE _borderColor AS LONG
		PROPERTY BorderColor AS LONG
			GET ; RETURN SELF:_borderColor
			END GET
			SET ; SELF:_borderColor := VALUE ; SELF:Invalidate()
			END SET
		END PROPERTY

		// ── BorderWidth ───────────────────────────────────────────────────────
		PRIVATE _borderWidth AS LONG
		PROPERTY BORDERWIDTH AS USUAL
			GET ; RETURN SELF:_borderWidth
			END GET
			SET ; SELF:_borderWidth := (LONG) VALUE ; SELF:Invalidate()
			END SET
		END PROPERTY

		// ── Curvature ─────────────────────────────────────────────────────────
		// 0 = sharp corners, 99 = fully rounded (circle/ellipse).
		// Used when Style = 4 or 5.
		PRIVATE _curvature AS LONG
		PROPERTY CURVATURE AS USUAL
			GET ; RETURN SELF:_curvature
			END GET
			SET
				SELF:_curvature := Math.Max(0, Math.Min(99, (LONG) VALUE))
				SELF:Invalidate()
			END SET
		END PROPERTY

		// ── FillColor ─────────────────────────────────────────────────────────
		PRIVATE _fillColor AS LONG
		PROPERTY FillColor AS LONG
			GET ; RETURN SELF:_fillColor
			END GET
			SET ; SELF:_fillColor := VALUE ; SELF:Invalidate()
			END SET
		END PROPERTY

		// ── FillStyle ─────────────────────────────────────────────────────────
		// VFP: 0=Solid, 1=Transparent, 2-7=hatch patterns.
		PRIVATE _fillStyle AS LONG
		PROPERTY FillStyle AS USUAL
			GET ; RETURN SELF:_fillStyle
			END GET
			SET ; SELF:_fillStyle := (LONG) VALUE ; SELF:Invalidate()
			END SET
		END PROPERTY

		// ── BorderStyle ──────────────────────────────────────────────────────
		// VFP: 0=Solid, 1=Dash, 2=Dot, 3=DashDot, 4=DashDotDot, 5=Invisible, 6=InsideSolid
		PRIVATE _borderStyle AS LONG
		PROPERTY BorderStyle AS LONG
			GET ; RETURN SELF:_borderStyle
			END GET
			SET ; SELF:_borderStyle := VALUE ; SELF:Invalidate()
			END SET
		END PROPERTY

		// ── Style ─────────────────────────────────────────────────────────────
		// 0=Rectangle, 1=Square, 2=Ellipse, 3=Circle, 4=Rounded Rect, 5=Rounded Square
		PRIVATE _style AS LONG
		PROPERTY Style AS LONG
			GET ; RETURN SELF:_style
			END GET
			SET ; SELF:_style := VALUE ; SELF:Invalidate()
			END SET
		END PROPERTY

		// ── OnPaint ───────────────────────────────────────────────────────────
		OVERRIDE PROTECTED METHOD OnPaint( e AS PaintEventArgs ) AS VOID
			SUPER:OnPaint(e)
			VAR g     := e:Graphics
			g:SmoothingMode := SmoothingMode.AntiAlias

			VAR pw    := Math.Max(1, SELF:_borderWidth)
			VAR r     := Rectangle{ pw/2, pw/2, SELF:ClientSize:Width - pw, SELF:ClientSize:Height - pw }

			// For square/circle styles, constrain to a square bounding box
			IF SELF:_style == 1 .OR. SELF:_style == 3 .OR. SELF:_style == 5
				VAR side := Math.Min(r:Width, r:Height)
				r := Rectangle{ r:X, r:Y, side, side }
			ENDIF

			// Fill
			IF SELF:_fillStyle == 0
				VAR brush := SolidBrush{ ColorTranslator.FromOle(SELF:_fillColor) }
				SELF:PaintShape(g, r, brush)
				brush:Dispose()
			ELSEIF SELF:_fillStyle >= 2 .AND. SELF:_fillStyle <= 7
				// Map VFP hatch styles (2-7) to HatchStyle enum (0-5)
				VAR hatch := (HatchStyle)(SELF:_fillStyle - 2)
				VAR brush := HatchBrush{ hatch, ColorTranslator.FromOle(SELF:_fillColor), Color.Transparent }
				SELF:PaintShape(g, r, brush)
				brush:Dispose()
			ENDIF

			// Border
			IF pw > 0 .AND. SELF:_borderStyle != 5
				VAR pen := Pen{ ColorTranslator.FromOle(SELF:_borderColor), (SINGLE) pw }
				SWITCH SELF:_borderStyle
				CASE 1 ; pen:DashStyle := DashStyle.Dash
				CASE 2 ; pen:DashStyle := DashStyle.Dot
				CASE 3 ; pen:DashStyle := DashStyle.DashDot
				CASE 4 ; pen:DashStyle := DashStyle.DashDotDot
				OTHERWISE ; pen:DashStyle := DashStyle.Solid
				END SWITCH
				SELF:StrokeShape(g, r, pen)
				pen:Dispose()
			ENDIF
		END METHOD

		PRIVATE METHOD PaintShape( g AS Graphics, r AS Rectangle, brush AS Brush ) AS VOID
			IF SELF:_style == 2 .OR. SELF:_style == 3
				g:FillEllipse( brush, r )
			ELSEIF SELF:_style == 4 .OR. SELF:_style == 5
				VAR radius := (INT)( r:Width * SELF:_curvature / 100.0 )
				SELF:FillRoundRect( g, brush, r, radius )
			ELSE
				g:FillRectangle( brush, r )
			ENDIF
		END METHOD

		PRIVATE METHOD StrokeShape( g AS Graphics, r AS Rectangle, pen AS Pen ) AS VOID
			IF SELF:_style == 2 .OR. SELF:_style == 3
				g:DrawEllipse( pen, r )
			ELSEIF SELF:_style == 4 .OR. SELF:_style == 5
				VAR radius := (INT)( r:Width * SELF:_curvature / 100.0 )
				SELF:DrawRoundRect( g, pen, r, radius )
			ELSE
				g:DrawRectangle( pen, r )
			ENDIF
		END METHOD

		PRIVATE METHOD FillRoundRect( g AS Graphics, brush AS Brush, r AS Rectangle, radius AS INT ) AS VOID
			IF radius <= 0
				g:FillRectangle(brush, r)
				RETURN
			ENDIF
			VAR path := SELF:RoundRectPath(r, radius)
			g:FillPath(brush, path)
			path:Dispose()
		END METHOD

		PRIVATE METHOD DrawRoundRect( g AS Graphics, pen AS Pen, r AS Rectangle, radius AS INT ) AS VOID
			IF radius <= 0
				g:DrawRectangle(pen, r)
				RETURN
			ENDIF
			VAR path := SELF:RoundRectPath(r, radius)
			g:DrawPath(pen, path)
			path:Dispose()
		END METHOD

		PRIVATE METHOD RoundRectPath( r AS Rectangle, radius AS INT ) AS GraphicsPath
			VAR d    := radius * 2
			VAR path := GraphicsPath{}
			path:AddArc( r:X,              r:Y,              d, d, 180, 90 )
			path:AddArc( r:Right - d,      r:Y,              d, d, 270, 90 )
			path:AddArc( r:Right - d,      r:Bottom - d,     d, d,   0, 90 )
			path:AddArc( r:X,              r:Bottom - d,     d, d,  90, 90 )
			path:CloseFigure()
			RETURN path
		END METHOD

	END CLASS

END NAMESPACE
