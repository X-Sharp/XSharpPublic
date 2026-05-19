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
	/// VFP-compatible shape control that wraps <see cref="System.Windows.Forms.UserControl"/> with fully custom owner-drawing.<br/>
	/// <see cref="Style"/> selects the geometry: 0=Rectangle, 1=Square, 2=Ellipse, 3=Circle, 4=Rounded Rectangle, 5=Rounded Square.<br/>
	/// Fill is controlled by <see cref="FillStyle"/> (0=Solid, 1=Transparent, 2â€“7=GDI+ hatch patterns)
	/// and <see cref="FillColor"/>.<br/>
	/// The border is controlled by <see cref="BorderColor"/>, <see cref="BORDERWIDTH"/>, and
	/// <see cref="BorderStyle"/> (0=Solid, 1=Dash, 2=Dot, 3=DashDot, 4=DashDotDot, 5=Invisible, 6=InsideSolid).<br/>
	/// For rounded styles (4/5), <see cref="CURVATURE"/> (0â€“99) controls the corner radius as a
	/// percentage of the bounding-box width. The background is always transparent.
	/// </summary>
	PARTIAL CLASS Shape INHERIT System.Windows.Forms.UserControl

		// Note: VFPObject.xh is included by Shape.generated.prg â€” do not include again here.

		#include "ControlProperties.xh"

		CONSTRUCTOR() STRICT
			SUPER()
			SELF:SetStyle( ControlStyles.SupportsTransparentBackColor ;
			             | ControlStyles.AllPaintingInWmPaint ;
			             | ControlStyles.UserPaint, TRUE )
			SELF:BackColor     := Color.Transparent
			SELF:_borderColor  := Color.Black
			SELF:_borderWidth  := 1
			SELF:_curvature    := 0
			SELF:_fillColor    := Color.White
			SELF:_fillStyle    := 1   // 1 = Transparent (VFP default)
			SELF:_style        := 0   // Rectangle
			SELF:Size          := Size{100, 60}

		// â”€â”€ BorderColor â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
		PRIVATE _borderColor AS System.Drawing.Color
		/// <summary>
		/// Colour of the shape's border. Triggers a repaint on change.
		/// </summary>
		PROPERTY BorderColor AS System.Drawing.Color
			GET ; RETURN SELF:_borderColor
			END GET
			SET ; SELF:_borderColor := VALUE ; SELF:Invalidate()
			END SET
		END PROPERTY

		// â”€â”€ BorderWidth â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
		PRIVATE _borderWidth AS LONG
		/// <summary>
		/// Width of the shape's border in pixels. Accepts a VFP <c>USUAL</c> for source compatibility. Triggers a repaint on change.
		/// </summary>
		PROPERTY BORDERWIDTH AS USUAL
			GET ; RETURN SELF:_borderWidth
			END GET
			SET ; SELF:_borderWidth := (LONG) VALUE ; SELF:Invalidate()
			END SET
		END PROPERTY

		// â”€â”€ Curvature â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
		PRIVATE _curvature AS LONG
		/// <summary>
		/// Corner radius as a percentage of the bounding-box width (clamped 0â€“99).<br/>
		/// 0=sharp corners; 99=fully rounded. Only used when <see cref="Style"/> is 4 (Rounded Rectangle) or 5 (Rounded Square).
		/// </summary>
		PROPERTY CURVATURE AS USUAL
			GET ; RETURN SELF:_curvature
			END GET
			SET
				SELF:_curvature := Math.Max(0, Math.Min(99, (LONG) VALUE))
				SELF:Invalidate()
			END SET
		END PROPERTY

		// â”€â”€ FillColor â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
		PRIVATE _fillColor AS System.Drawing.Color
		/// <summary>
		/// Interior fill colour used when <see cref="FillStyle"/> is 0 (Solid) or 2â€“7 (hatch patterns). Triggers a repaint on change.
		/// </summary>
		PROPERTY FillColor AS System.Drawing.Color
			GET ; RETURN SELF:_fillColor
			END GET
			SET ; SELF:_fillColor := VALUE ; SELF:Invalidate()
			END SET
		END PROPERTY

		// â”€â”€ FillStyle â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
		PRIVATE _fillStyle AS LONG
		/// <summary>
		/// VFP fill style: 0=Solid (<see cref="FillColor"/>); 1=Transparent (no fill, default);
		/// 2â€“7=GDI+ hatch patterns (<c>HatchStyle</c> 0â€“5) drawn with <see cref="FillColor"/>.
		/// Triggers a repaint on change.
		/// </summary>
		PROPERTY FillStyle AS USUAL
			GET ; RETURN SELF:_fillStyle
			END GET
			SET ; SELF:_fillStyle := (LONG) VALUE ; SELF:Invalidate()
			END SET
		END PROPERTY

		// â”€â”€ BorderStyle â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
		PRIVATE _borderStyle AS LONG
		/// <summary>
		/// VFP border line style: 0=Solid, 1=Dash, 2=Dot, 3=DashDot, 4=DashDotDot,
		/// 5=Invisible (border suppressed), 6=InsideSolid (treated as Solid).<br/>
		/// Maps to <see cref="System.Drawing.Drawing2D.DashStyle"/> on the border <c>Pen</c>.
		/// </summary>
		PROPERTY BorderStyle AS LONG
			GET ; RETURN SELF:_borderStyle
			END GET
			SET ; SELF:_borderStyle := VALUE ; SELF:Invalidate()
			END SET
		END PROPERTY

		// â”€â”€ Style â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
		PRIVATE _style AS LONG
		/// <summary>
		/// Shape geometry: 0=Rectangle, 1=Square, 2=Ellipse, 3=Circle,
		/// 4=Rounded Rectangle, 5=Rounded Square.<br/>
		/// Square/Circle/Rounded Square constrain painting to a square bounding box.
		/// Rounded styles use <see cref="CURVATURE"/> for the corner radius.
		/// </summary>
		PROPERTY Style AS LONG
			GET ; RETURN SELF:_style
			END GET
			SET ; SELF:_style := VALUE ; SELF:Invalidate()
			END SET
		END PROPERTY

		// â”€â”€ OnPaint â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
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
				VAR brush := SolidBrush{ SELF:_fillColor }
				SELF:PaintShape(g, r, brush)
				brush:Dispose()
			ELSEIF SELF:_fillStyle >= 2 .AND. SELF:_fillStyle <= 7
				// Map VFP hatch styles (2-7) to HatchStyle enum (0-5)
				VAR hatch := (HatchStyle)(SELF:_fillStyle - 2)
				VAR brush := HatchBrush{ hatch, SELF:_fillColor, Color.Transparent }
				SELF:PaintShape(g, r, brush)
				brush:Dispose()
			ENDIF

			// Border
			IF pw > 0 .AND. SELF:_borderStyle != 5
				VAR pen := Pen{ SELF:_borderColor, (SINGLE) pw }
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

		/// <summary>
		/// Fills the shape geometry dictated by <see cref="Style"/> using the supplied <paramref name="brush"/>.
		/// </summary>
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

		/// <summary>
		/// Strokes the shape outline dictated by <see cref="Style"/> using the supplied <paramref name="pen"/>.
		/// </summary>
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

		/// <summary>
		/// Fills a rounded-rectangle path; falls back to <c>FillRectangle</c> when <paramref name="radius"/> is zero.
		/// </summary>
		PRIVATE METHOD FillRoundRect( g AS Graphics, brush AS Brush, r AS Rectangle, radius AS INT ) AS VOID
			IF radius <= 0
				g:FillRectangle(brush, r)
				RETURN
			ENDIF
			VAR path := SELF:RoundRectPath(r, radius)
			g:FillPath(brush, path)
			path:Dispose()
		END METHOD

		/// <summary>
		/// Strokes a rounded-rectangle path; falls back to <c>DrawRectangle</c> when <paramref name="radius"/> is zero.
		/// </summary>
		PRIVATE METHOD DrawRoundRect( g AS Graphics, pen AS Pen, r AS Rectangle, radius AS INT ) AS VOID
			IF radius <= 0
				g:DrawRectangle(pen, r)
				RETURN
			ENDIF
			VAR path := SELF:RoundRectPath(r, radius)
			g:DrawPath(pen, path)
			path:Dispose()
		END METHOD

		/// <summary>
		/// Builds a <see cref="System.Drawing.Drawing2D.GraphicsPath"/> for a rounded rectangle with the given corner <paramref name="radius"/>.
		/// </summary>
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
