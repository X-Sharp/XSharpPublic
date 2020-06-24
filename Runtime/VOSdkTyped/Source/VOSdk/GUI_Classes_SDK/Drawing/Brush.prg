USING VOSDK := XSharp.VO.SDK


CLASS Brush INHERIT VObject
	PROTECT oBrush   AS System.Drawing.Brush
	PROTECT _hParent AS IntPtr
	PROTECT oColor   as Color

	ACCESS Color AS VOSDK.Color
		RETURN oColor

	[Obsolete];
	METHOD __SetBrushOrg(_hDc AS IntPtr, hClient AS IntPtr) AS VOID STRICT
		RETURN
		
	METHOD CreateNew(xColor, kHatchStyle) 
		LOCAL argTypeError AS LOGIC

		IF (oBrush != NULL_OBJECT)
			oBrush:Dispose()
			oBrush := NULL_OBJECT
		ENDIF

		oColor := NULL_OBJECT
		IF IsInstanceOfUsual(xColor, #Color)
			oColor := xColor
			Default(@kHatchStyle, HATCHSOLID)

			IF IsNumeric(kHatchStyle)
				IF (kHatchStyle == HATCHSOLID)
					oBrush := System.Drawing.SolidBrush{System.Drawing.Color.FromArgb((int) oColor:ColorRef)}
				ELSE
					oBrush := System.Drawing.Drawing2D.HatchBrush{__ConvertHatch(kHatchStyle), System.Drawing.Color.FromArgb((int) oColor:ColorRef)}
				ENDIF
			ELSE
				argTypeError := TRUE
			ENDIF

		ELSEIF IsInstanceOfUsual(xColor, #Bitmap)
			LOCAL oBmp AS Bitmap
			IF IsNil(kHatchStyle)
				oBmp := xColor
				oBrush := System.Drawing.TextureBrush{ oBmp:__Image}
			ELSE
				argTypeError := TRUE
			ENDIF

		ELSEIF IsNumeric(xColor)
			IF IsNil(kHatchStyle)
				oBrush := __ConvertBrush(xColor)
			ELSE
				argTypeError := TRUE
			ENDIF

		ELSE
			argTypeError := TRUE
		ENDIF

		IF argTypeError
			WCError{#Init, #Brush, __WCSTypeError}:@@Throw()
		ENDIF
		RETURN SELF

	METHOD Destroy() AS USUAL CLIPPER
		
		IF (oBrush != NULL_OBJECT)
			oBrush:Dispose()
			oBrush := NULL_OBJECT
		ENDIF

		SUPER:Destroy()

		RETURN NIL

	METHOD Handle() AS System.Drawing.Brush
		RETURN oBrush

	CONSTRUCTOR(xColor, kHatchStyle, oParent) 
		SUPER()
		SELF:CreateNew(xColor, kHatchStyle)
		SELF:Parent := oParent
		RETURN 

	ASSIGN Parent (oWindow) 
		LOCAL oParent AS Window

		IF IsInstanceOfUsual(oWindow, #Window)
			oParent := oWindow
			_hParent := oParent:Handle()
		ELSE
			_hParent := NULL_PTR
		ENDIF
		RETURN 

	OPERATOR IMPLICIT ( c AS System.Drawing.Color) AS Brush
		RETURN Brush{Color{c:R, c:B, c:G}}



	OPERATOR IMPLICIT ( c AS VOSDK.Color) AS Brush
		RETURN Brush{c}

	OPERATOR IMPLICIT ( b AS Brush) AS System.Drawing.Color
		RETURN (System.Drawing.Color) b:Color

	OPERATOR IMPLICIT ( b AS Brush) AS Color
		RETURN b:Color


STATIC METHOD __ConvertBrush(brushType AS INT) AS System.Drawing.Brush STRICT
	LOCAL retVal AS System.Drawing.Brush

	DO CASE
	CASE brushType == BRUSHBLACK
		retVal := System.Drawing.Brushes.Black
	CASE brushType == BRUSHDARK
		retVal := System.Drawing.Brushes.DarkGray
	CASE brushType == BRUSHMEDIUM
		retVal := System.Drawing.Brushes.Gray
	CASE brushType == BRUSHLIGHT
		retVal := System.Drawing.Brushes.LightGray
	CASE brushType == BRUSHHOLLOW
		retVal := System.Drawing.Brushes.Transparent
	CASE brushType == BRUSHCLEAR
		retVal := System.Drawing.Brushes.Transparent
	OTHERWISE
		retVal := System.Drawing.Brushes.White
	ENDCASE

	RETURN retVal



STATIC METHOD __ConvertHatch(hatchStyle AS INT) AS System.Drawing.Drawing2D.HatchStyle STRICT
	LOCAL retVal AS System.Drawing.Drawing2D.HatchStyle

	DO CASE
	CASE hatchStyle == HATCHDIAGONAL45
		retVal := System.Drawing.Drawing2D.HatchStyle.ForwardDiagonal
	CASE hatchStyle == HATCHVERTICAL
		retVal := System.Drawing.Drawing2D.HatchStyle.Vertical
	CASE hatchStyle == HATCHDIAGONAL135
		retVal := System.Drawing.Drawing2D.HatchStyle.BackwardDiagonal
	CASE hatchStyle == HATCHHORIZONTAL
		retVal := System.Drawing.Drawing2D.HatchStyle.Horizontal
	CASE hatchStyle == HATCHORTHOGONALCROSS
		retVal := System.Drawing.Drawing2D.HatchStyle.Cross
	OTHERWISE
		retVal := System.Drawing.Drawing2D.HatchStyle.SmallGrid
	ENDCASE

	RETURN retVal


END CLASS

