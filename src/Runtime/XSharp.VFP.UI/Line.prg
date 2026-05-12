// Line.prg
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
	/// The VFP compatible Line class.
	/// Draws a straight line inside a transparent UserControl.
	/// VFP properties: BorderColor, BorderWidth, LineSlant ("/", "\"), Rotation.
	/// </summary>
	PARTIAL CLASS Line INHERIT System.Windows.Forms.UserControl

		// Note: VFPObject.xh is included by Line.generated.prg — do not include again here.

		#include "ControlProperties.xh"

		CONSTRUCTOR() STRICT
			SUPER()
			SELF:SetStyle( ControlStyles.SupportsTransparentBackColor ;
			             | ControlStyles.AllPaintingInWmPaint ;
			             | ControlStyles.UserPaint, TRUE )
			SELF:BackColor    := Color.Transparent
			SELF:_borderColor := Color.Black
			SELF:_borderWidth := 1
			SELF:_lineSlant   := "\"
			SELF:Size         := Size{100, 2}

		// ── BorderColor ───────────────────────────────────────────────────────
		PRIVATE _borderColor AS System.Drawing.Color
		PROPERTY BorderColor AS System.Drawing.Color
			GET
				RETURN SELF:_borderColor
			END GET
			SET
				SELF:_borderColor := VALUE
				SELF:Invalidate()
			END SET
		END PROPERTY

		// ── BorderWidth ───────────────────────────────────────────────────────
		PRIVATE _borderWidth AS LONG
		PROPERTY BorderWidth AS USUAL
			GET
				RETURN SELF:_borderWidth
			END GET
			SET
				SELF:_borderWidth := (LONG) VALUE
				SELF:Invalidate()
			END SET
		END PROPERTY

		// ── BorderStyle ──────────────────────────────────────────────────────
		// VFP: 0=Solid, 1=Dash, 2=Dot, 3=DashDot, 4=DashDotDot, 5=Invisible, 6=InsideSolid
		PRIVATE _borderStyle AS LONG
		PROPERTY BorderStyle AS LONG
			GET
				RETURN SELF:_borderStyle
			END GET
			SET
				SELF:_borderStyle := VALUE
				SELF:Invalidate()
			END SET
		END PROPERTY

		// ── LineSlant ─────────────────────────────────────────────────────────
		// "\" = top-left to bottom-right (default)
		// "/" = bottom-left to top-right
		PRIVATE _lineSlant AS STRING
		PROPERTY LineSlant AS USUAL
			GET
				RETURN SELF:_lineSlant
			END GET
			SET
				SELF:_lineSlant := Str(VALUE)
				SELF:Invalidate()
			END SET
		END PROPERTY

		// ── OnPaint ───────────────────────────────────────────────────────────
		OVERRIDE PROTECTED METHOD OnPaint( e AS PaintEventArgs ) AS VOID
			SUPER:OnPaint(e)
			VAR g     := e:Graphics
			VAR color := SELF:_borderColor
			VAR pen   := Pen{ color, (SINGLE) SELF:_borderWidth }
			SWITCH SELF:_borderStyle
			CASE 1 ; pen:DashStyle := DashStyle.Dash
			CASE 2 ; pen:DashStyle := DashStyle.Dot
			CASE 3 ; pen:DashStyle := DashStyle.DashDot
			CASE 4 ; pen:DashStyle := DashStyle.DashDotDot
			OTHERWISE ; pen:DashStyle := DashStyle.Solid
			END SWITCH
			VAR w := SELF:ClientSize:Width  - 1
			VAR h := SELF:ClientSize:Height - 1
			IF SELF:_borderStyle != 5
				IF SELF:_lineSlant == "/"
					g:DrawLine( pen, 0, h, w, 0 )
				ELSE
					// Default: "\" — top-left to bottom-right
					g:DrawLine( pen, 0, 0, w, h )
				ENDIF
			ENDIF
			pen:Dispose()
		END METHOD

	END CLASS

END NAMESPACE
