USING System
USING System.ComponentModel
USING System.Drawing
USING System.Windows.Forms

BEGIN NAMESPACE XSharp.VFP.UI

/// <summary>
/// Shape control - GDI+ graphics primitive for drawing shapes
/// Custom control for rendering rectangles, circles, and rounded rectangles
/// Supports customizable fill and border colors and widths
/// </summary>
PUBLIC CLASS Shape INHERIT System.Windows.Forms.Control IMPLEMENTS IVFPObject, IVFPControl

	// ============================================================================
	// Include VFPObject base implementation (IVFPObject, IVFPHelp)
	// ============================================================================
	#include "Headers/VFPObject.xh"

	PRIVATE _backColor AS Color
	PRIVATE _fillColor AS Color
	PRIVATE _borderColor AS Color
	PRIVATE _borderWidth AS INT32
	PRIVATE _shapeType AS INT32  // 0=Rectangle, 1=Circle, 2=RoundedRect
	PRIVATE _baseClass AS STRING
	PRIVATE _class AS STRING
	PRIVATE _classLibrary AS STRING
	PRIVATE _comment AS STRING
	PRIVATE _helpContextID AS LONG
	PRIVATE _whatsThisHelpID AS LONG
	PRIVATE _dragMode AS INT
	PRIVATE _dragIcon AS STRING

	/// <summary>
	/// Gets or sets the background color of the shape
	/// </summary>
	PUBLIC PROPERTY ShapeBackColor AS Color
		GET
			RETURN SELF:_backColor
		END GET
		SET
			SELF:_backColor := VALUE
			SELF:BackColor := VALUE
		END SET
	END PROPERTY

	/// <summary>
	/// Gets or sets the fill color of the shape
	/// </summary>
	PUBLIC PROPERTY FillColor AS Color
		GET
			RETURN SELF:_fillColor
		END GET
		SET
			SELF:_fillColor := VALUE
			SELF:Invalidate()
		END SET
	END PROPERTY

	/// <summary>
	/// Gets or sets the border color of the shape
	/// </summary>
	PUBLIC PROPERTY BorderColor AS Color
		GET
			RETURN SELF:_borderColor
		END GET
		SET
			SELF:_borderColor := VALUE
			SELF:Invalidate()
		END SET
	END PROPERTY

	/// <summary>
	/// Gets or sets the border width in pixels
	/// </summary>
	PUBLIC PROPERTY BorderWidth AS INT32
		GET
			RETURN SELF:_borderWidth
		END GET
		SET
			SELF:_borderWidth := VALUE
			SELF:Invalidate()
		END SET
	END PROPERTY

	/// <summary>
	/// Gets or sets the shape type (0=Rectangle, 1=Circle, 2=RoundedRect)
	/// </summary>
	PUBLIC PROPERTY ShapeType AS INT32
		GET
			RETURN SELF:_shapeType
		END GET
		SET
			SELF:_shapeType := VALUE
			SELF:Invalidate()
		END SET
	END PROPERTY

	/// <summary>
	/// Constructor - initializes the Shape control
	/// </summary>
	PUBLIC CONSTRUCTOR()
		SUPER()
		SELF:_backColor := System.Drawing.Color.White
		SELF:_fillColor := System.Drawing.Color.White
		SELF:_borderColor := System.Drawing.Color.Black
		SELF:_borderWidth := 1
		SELF:_shapeType := 0  // Rectangle by default
		SELF:_baseClass := "Shape"
		SELF:_class := "Shape"
		SELF:_classLibrary := ""
		SELF:_comment := ""
		SELF:_helpContextID := 0
		SELF:_whatsThisHelpID := 0
		SELF:_dragMode := 0
		SELF:_dragIcon := ""
		SELF:BackColor := SELF:_backColor
		SELF:Width := 100
		SELF:Height := 100
		SELF:DoubleBuffered := TRUE
	END CONSTRUCTOR



	// ============================================================================
	// IVFPControl Implementation
	// ============================================================================

	[Category("VFP Behavior")];
	[Description("Drag icon path")];
	[DefaultValue("")];
	PROPERTY DragIcon AS STRING
		GET
			RETURN SELF:_dragIcon
		END GET
		SET
			SELF:_dragIcon := VALUE
		END SET
	END PROPERTY

	[Category("VFP Behavior")];
	[Description("Drag mode (0=manual, 1=automatic)")];
	[DefaultValue(0)];
	PROPERTY DragMode AS LONG
		GET
			RETURN SELF:_dragMode
		END GET
		SET
			SELF:_dragMode := VALUE
		END SET
	END PROPERTY

	PUBLIC METHOD Drag(nAction ) AS USUAL CLIPPER
		// Placeholder - Drag operation
		RETURN NIL
	END METHOD

	PUBLIC METHOD SetFocus() AS VOID STRICT
		SELF:Focus()
	END METHOD

	/// <summary>
	/// Handles the Paint event to draw the shape
	/// </summary>
	PROTECTED OVERRIDE METHOD OnPaint(e AS PaintEventArgs) AS VOID
		SUPER:OnPaint(e)
		LOCAL brush AS Brush
		LOCAL pen AS Pen

		brush := SolidBrush{SELF:_fillColor}
		pen := pen{SELF:_borderColor, SELF:_borderWidth}

		TRY
			DO CASE
			CASE SELF:_shapeType == 0  // Rectangle
				e:Graphics:FillRectangle(brush, 0, 0, SELF:Width, SELF:Height)
				e:Graphics:DrawRectangle(pen, 0, 0, SELF:Width - 1, SELF:Height - 1)
			CASE SELF:_shapeType == 1  // Circle/Ellipse
				e:Graphics:FillEllipse(brush, 0, 0, SELF:Width, SELF:Height)
				e:Graphics:DrawEllipse(pen, 0, 0, SELF:Width - 1, SELF:Height - 1)
			CASE SELF:_shapeType == 2  // Rounded Rectangle
				SELF:DrawRoundedRectangle(e:Graphics, brush, pen)
			END CASE
		FINALLY
			brush:Dispose()
			pen:Dispose()
		END TRY
	END METHOD

	/// <summary>
	/// Draws a rounded rectangle
	/// </summary>
	PRIVATE METHOD DrawRoundedRectangle(g AS Graphics, brush AS Brush, pen AS Pen) AS VOID
		LOCAL radius AS INT32
		LOCAL path AS System.Drawing.Drawing2D.GraphicsPath

		radius := 10
		path := System.Drawing.Drawing2D.GraphicsPath{}

		TRY
			// Add rounded rectangle path
			path:AddArc(0, 0, radius, radius, 180, 90)
			path:AddArc(SELF:Width - radius, 0, radius, radius, 270, 90)
			path:AddArc(SELF:Width - radius, SELF:Height - radius, radius, radius, 0, 90)
			path:AddArc(0, SELF:Height - radius, radius, radius, 90, 90)
			path:CloseFigure()

			g:FillPath(brush, path)
			g:DrawPath(pen, path)
		FINALLY
			path:Dispose()
		END TRY
	END METHOD

	/// <summary>
	/// Sets the shape to a rectangle
	/// </summary>
	PUBLIC METHOD SetRectangle() AS VOID
		SELF:ShapeType := 0
	END METHOD

	/// <summary>
	/// Sets the shape to a circle/ellipse
	/// </summary>
	PUBLIC METHOD SetCircle() AS VOID
		SELF:ShapeType := 1
		SELF:Width := SELF:Height
	END METHOD

	/// <summary>
	/// Sets the shape to a rounded rectangle
	/// </summary>
	PUBLIC METHOD SetRoundedRectangle() AS VOID
		SELF:ShapeType := 2
	END METHOD

	/// <summary>
	/// Sets both fill and border colors
	/// </summary>
	PUBLIC METHOD SetColors(oFill AS Color, oBorder AS Color) AS VOID
		SELF:FillColor := oFill
		SELF:BorderColor := oBorder
	END METHOD

	/// <summary>
	/// Resets shape to default appearance
	/// </summary>
	PUBLIC METHOD Reset() AS VOID
		SELF:ShapeType := 0
		SELF:FillColor := System.Drawing.Color.White
		SELF:BorderColor := System.Drawing.Color.Black
		SELF:BorderWidth := 1
	END METHOD

END CLASS

END NAMESPACE
