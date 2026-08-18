// Shape.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.

USING System
USING System.Windows.Forms
USING System.Windows.Forms.VisualStyles
USING System.Drawing
USING System.Drawing.Drawing2D
USING System.ComponentModel

BEGIN NAMESPACE XSharp.VFP.UI

	/// <summary>
	/// VFP-compatible shape control that wraps <see cref="System.Windows.Forms.UserControl"/> with fully custom owner-drawing.<br/>
	/// Geometry is driven solely by <see cref="CURVATURE"/> (0–99): 0=sharp rectangle, 1–98=progressively
	/// rounded rectangle, 99=full ellipse/circle. <see cref="Style"/> does NOT select geometry — it only
	/// selects 0=Normal (border via <see cref="BorderColor"/>/<see cref="BorderStyle"/>/<see cref="BORDERWIDTH"/>)
	/// or 3=Themed (border drawn via the OS visual-style renderer; <see cref="CURVATURE"/> is disregarded while themed).<br/>
	/// Fill is controlled by <see cref="FillStyle"/> (0=Solid, 1=Transparent/default, 2–7=GDI+ hatch patterns)
	/// and <see cref="FillColor"/>.<br/>
	/// <see cref="BorderStyle"/> (0=Transparent/no border, 1=Solid, 2=Dash, 3=Dot, 4=DashDot, 5=DashDotDot,
	/// 6=InsideSolid) is only honored when <see cref="BORDERWIDTH"/> is 1 — wider borders always render solid.
	/// The background follows <see cref="BackStyle"/> (0=Transparent, 1=Opaque/default) and <c>BackColor</c>,
	/// independently of <see cref="FillStyle"/>.
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
			SELF:_backStyle    := 1   // 1 = Opaque (VFP default)
			SELF:_borderColor  := Color.Black
			SELF:_borderWidth  := 1
			SELF:_curvature    := 0
			SELF:_fillColor    := Color.White
			SELF:_fillStyle    := 1   // 1 = Transparent (VFP default)
			SELF:_style        := 0   // Rectangle
			SELF:Size          := Size{100, 60}

		// -- BackStyle -----------------------------------------------------------
		PRIVATE _backStyle AS INT
		/// <summary>
		/// VFP BackStyle: 0=Transparent (sets <c>BackColor</c> to <c>Transparent</c>), 1=Opaque/default.<br/>
		/// Note: switching from 0 back to 1 does not restore a previously set opaque colour;
		/// re-assign <c>BackColor</c> if needed. Triggers a repaint on change.
		/// </summary>
		PROPERTY BackStyle AS INT
			GET ; RETURN SELF:_backStyle
			END GET
			SET
				SELF:_backStyle := VALUE
				IF ( VALUE == 0 )
					SELF:BackColor := Color.Transparent
				ENDIF
				SELF:Invalidate()
			END SET
		END PROPERTY

		// -- BorderColor ---------------------------------------------------------
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

		// ── BorderWidth ───────────────────────────────────────────────────────
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

		// ── Curvature ─────────────────────────────────────────────────────────
		PRIVATE _curvature AS LONG
		/// <summary>
		/// Shape geometry, clamped 0–99. 0=sharp rectangle; 1–98=progressively rounded rectangle
		/// (higher=more rounded); 99=full circle/ellipse. Disregarded (treated as 0) while
		/// <see cref="Style"/> is 3 (Themed).
		/// </summary>
		PROPERTY CURVATURE AS USUAL
			GET ; RETURN SELF:_curvature
			END GET
			SET
				SELF:_curvature := Math.Max(0, Math.Min(99, (LONG) VALUE))
				SELF:Invalidate()
			END SET
		END PROPERTY

		// ── FillColor ─────────────────────────────────────────────────────────
		PRIVATE _fillColor AS System.Drawing.Color
		/// <summary>
		/// Interior fill colour used when <see cref="FillStyle"/> is 0 (Solid) or 2–7 (hatch patterns). Triggers a repaint on change.
		/// </summary>
		PROPERTY FillColor AS System.Drawing.Color
			GET ; RETURN SELF:_fillColor
			END GET
			SET ; SELF:_fillColor := VALUE ; SELF:Invalidate()
			END SET
		END PROPERTY

		// ── FillStyle ─────────────────────────────────────────────────────────
		PRIVATE _fillStyle AS LONG
		/// <summary>
		/// VFP fill style: 0=Solid (<see cref="FillColor"/>); 1=Transparent (no fill, default);
		/// 2–7=GDI+ hatch patterns (<c>HatchStyle</c> 0–5) drawn with <see cref="FillColor"/>.
		/// Triggers a repaint on change.
		/// </summary>
		PROPERTY FillStyle AS USUAL
			GET ; RETURN SELF:_fillStyle
			END GET
			SET ; SELF:_fillStyle := (LONG) VALUE ; SELF:Invalidate()
			END SET
		END PROPERTY

		// ── BorderStyle ──────────────────────────────────────────────────────
		PRIVATE _borderStyle AS LONG
		/// <summary>
		/// VFP border line style: 0=Transparent (no border drawn), 1=Solid, 2=Dash, 3=Dot,
		/// 4=DashDot, 5=DashDotDot, 6=InsideSolid (treated as Solid).<br/>
		/// Dash/dot patterns (2–5) are only honored when <see cref="BORDERWIDTH"/> is 1 —
		/// wider borders always render solid, matching VFP. Ignored entirely while
		/// <see cref="Style"/> is 3 (Themed), which draws its own OS-themed border.
		/// </summary>
		PROPERTY BorderStyle AS LONG
			GET ; RETURN SELF:_borderStyle
			END GET
			SET ; SELF:_borderStyle := VALUE ; SELF:Invalidate()
			END SET
		END PROPERTY

		// ── Style ─────────────────────────────────────────────────────────────
		PRIVATE _style AS LONG
		/// <summary>
		/// VFP <c>nType</c>: 0=Normal (default) draws the border via <see cref="BorderColor"/>/
		/// <see cref="BorderStyle"/>/<see cref="BORDERWIDTH"/>. 3=Themed draws the border using the
		/// OS visual-style renderer instead, and disregards <see cref="CURVATURE"/> while active.<br/>
		/// Does NOT select shape geometry — that is <see cref="CURVATURE"/>'s sole responsibility.
		/// Any value other than 3 is treated as Normal.
		/// </summary>
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

			VAR isThemed  := SELF:_style == 3
			VAR curvature := IIF( isThemed, 0, SELF:_curvature )
			VAR pw        := Math.Max(1, SELF:_borderWidth)
			VAR r         := Rectangle{ pw/2, pw/2, SELF:ClientSize:Width - pw, SELF:ClientSize:Height - pw }

			// Fill
			IF SELF:_fillStyle == 0
				VAR brush := SolidBrush{ SELF:_fillColor }
				SELF:FillGeometry(g, r, brush, curvature)
				brush:Dispose()
			ELSEIF SELF:_fillStyle >= 2 .AND. SELF:_fillStyle <= 7
				// Map VFP hatch styles (2-7) to HatchStyle enum (0-5)
				VAR hatch := (HatchStyle)(SELF:_fillStyle - 2)
				VAR brush := HatchBrush{ hatch, SELF:_fillColor, Color.Transparent }
				SELF:FillGeometry(g, r, brush, curvature)
				brush:Dispose()
			ENDIF

			// Border
			IF isThemed
				SELF:PaintThemedBorder(g)
			ELSEIF pw > 0 .AND. SELF:_borderStyle != 0
				VAR pen := Pen{ SELF:_borderColor, (SINGLE) pw }
				IF pw == 1
					SWITCH SELF:_borderStyle
					CASE 2 ; pen:DashStyle := DashStyle.Dash
					CASE 3 ; pen:DashStyle := DashStyle.Dot
					CASE 4 ; pen:DashStyle := DashStyle.DashDot
					CASE 5 ; pen:DashStyle := DashStyle.DashDotDot
					OTHERWISE ; pen:DashStyle := DashStyle.Solid
					END SWITCH
				ELSE
					// Dash/dot patterns are only honored at BorderWidth==1, matching VFP
					pen:DashStyle := DashStyle.Solid
				ENDIF
				SELF:StrokeGeometry(g, r, pen, curvature)
				pen:Dispose()
			ENDIF
		END METHOD

		/// <summary>
		/// Draws the OS-themed border used when <see cref="Style"/> is 3 (Themed), via
		/// <see cref="VisualStyleRenderer"/>. Falls back to a plain 1px solid rectangle when
		/// visual styles are unavailable.
		/// </summary>
		PRIVATE METHOD PaintThemedBorder( g AS Graphics ) AS VOID
			VAR r := Rectangle{ 0, 0, SELF:ClientSize:Width, SELF:ClientSize:Height }
			IF System.Windows.Forms.Application.RenderWithVisualStyles .AND. VisualStyleRenderer.IsElementDefined( VisualStyleElement.Button.GroupBox.Normal )
				VAR renderer := VisualStyleRenderer{ VisualStyleElement.Button.GroupBox.Normal }
				renderer:DrawBackground( g, r )
			ELSE
				VAR pen := Pen{ SELF:_borderColor, 1 }
				g:DrawRectangle( pen, Rectangle{ r:X, r:Y, r:Width - 1, r:Height - 1 } )
				pen:Dispose()
			ENDIF
		END METHOD

		/// <summary>
		/// Fills the shape geometry dictated by <paramref name="curvature"/> (0=rectangle, 99=ellipse)
		/// using the supplied <paramref name="brush"/>.
		/// </summary>
		PRIVATE METHOD FillGeometry( g AS Graphics, r AS Rectangle, brush AS Brush, curvature AS LONG ) AS VOID
			IF curvature >= 99
				g:FillEllipse( brush, r )
			ELSEIF curvature <= 0
				g:FillRectangle( brush, r )
			ELSE
				VAR radius := SELF:CurvatureRadius(r, curvature)
				SELF:FillRoundRect( g, brush, r, radius )
			ENDIF
		END METHOD

		/// <summary>
		/// Strokes the shape outline dictated by <paramref name="curvature"/> (0=rectangle, 99=ellipse)
		/// using the supplied <paramref name="pen"/>.
		/// </summary>
		PRIVATE METHOD StrokeGeometry( g AS Graphics, r AS Rectangle, pen AS Pen, curvature AS LONG ) AS VOID
			IF curvature >= 99
				g:DrawEllipse( pen, r )
			ELSEIF curvature <= 0
				g:DrawRectangle( pen, r )
			ELSE
				VAR radius := SELF:CurvatureRadius(r, curvature)
				SELF:DrawRoundRect( g, pen, r, radius )
			ENDIF
		END METHOD

		/// <summary>
		/// Corner radius for a given <paramref name="curvature"/> (1–98), scaled so that 99
		/// would reach exactly half of the shorter side — the true-ellipse case is handled
		/// separately by the caller.
		/// </summary>
		PRIVATE METHOD CurvatureRadius( r AS Rectangle, curvature AS LONG ) AS INT
			RETURN (INT)( Math.Min(r:Width, r:Height) / 2.0 * curvature / 99.0 )
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
