USING System
USING System.ComponentModel
USING System.Drawing
USING System.Windows.Forms

BEGIN NAMESPACE XSharp.VFP.UI

/// <summary>
/// Line control - GDI+ graphics primitive for drawing lines
/// Custom control that renders a line using GDI+ drawing
/// Supports customizable color and width properties
/// </summary>
PUBLIC CLASS Line INHERIT System.Windows.Forms.Control IMPLEMENTS IVFPObject, IVFPControl

	// ============================================================================
	// Include VFPObject base implementation (IVFPObject, IVFPHelp)
	// ============================================================================
	#include "Headers/VFPObject.xh"

	PRIVATE _borderColor AS Color
	PRIVATE _borderWidth AS INT32
	PRIVATE _visible AS LOGIC
	PRIVATE _baseClass AS STRING
	PRIVATE _class AS STRING
	PRIVATE _classLibrary AS STRING
	PRIVATE _comment AS STRING
	PRIVATE _helpContextID AS LONG
	PRIVATE _whatsThisHelpID AS LONG
	PRIVATE _dragMode AS INT
	PRIVATE _dragIcon AS STRING

	/// <summary>
	/// Gets or sets the color of the line
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
	/// Gets or sets the width of the line in pixels
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
	/// Gets or sets whether the line is visible
	/// </summary>
	PUBLIC PROPERTY LineVisible AS LOGIC
		GET
			RETURN SELF:_visible
		END GET
		SET
			SELF:_visible := VALUE
			SELF:Invalidate()
		END SET
	END PROPERTY

	/// <summary>
	/// Constructor - initializes the Line control
	/// </summary>
	PUBLIC CONSTRUCTOR()
		SUPER()
		SELF:_borderColor := System.Drawing.Color.Black
		SELF:_borderWidth := 1
		SELF:_visible := TRUE
		SELF:_baseClass := "Line"
		SELF:_class := "Line"
		SELF:_classLibrary := ""
		SELF:_comment := ""
		SELF:_helpContextID := 0
		SELF:_whatsThisHelpID := 0
		SELF:_dragMode := 0
		SELF:_dragIcon := ""
		SELF:BackColor := System.Drawing.Color.Transparent
		SELF:Height := 2
		SELF:Width := 100
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
	/// Handles the Paint event to draw the line
	/// </summary>
	PROTECTED OVERRIDE METHOD OnPaint(e AS PaintEventArgs) AS VOID
		SUPER:OnPaint(e)
		IF SELF:_visible
			LOCAL pen AS Pen
			pen := pen{SELF:_borderColor, SELF:_borderWidth}
			TRY
				// Draw horizontal line from left to right
				e:Graphics:DrawLine(pen, 0, SELF:Height / 2, SELF:Width, SELF:Height / 2)
			FINALLY
				pen:Dispose()
			END TRY
		ENDIF
	END METHOD

	/// <summary>
	/// Sets the line to horizontal orientation
	/// </summary>
	PUBLIC METHOD SetHorizontal() AS VOID
		SELF:Height := SELF:_borderWidth + 2
	END METHOD

	/// <summary>
	/// Sets the line to vertical orientation
	/// </summary>
	PUBLIC METHOD SetVertical() AS VOID
		LOCAL temp AS INT32
		temp := SELF:Width
		SELF:Width := SELF:Height
		SELF:Height := temp
	END METHOD

	/// <summary>
	/// Sets both color and width properties
	/// </summary>
	PUBLIC METHOD SetLineStyle(oColor AS Color, nWidth AS INT32) AS VOID
		SELF:BorderColor := oColor
		SELF:BorderWidth := nWidth
	END METHOD

	/// <summary>
	/// Resets line to default appearance
	/// </summary>
	PUBLIC METHOD Reset() AS VOID
		SELF:BorderColor := System.Drawing.Color.Black
		SELF:BorderWidth := 1
		SELF:LineVisible := TRUE
		SELF:SetHorizontal()
	END METHOD

END CLASS

END NAMESPACE
