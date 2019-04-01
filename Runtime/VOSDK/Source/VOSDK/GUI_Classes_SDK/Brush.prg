CLASS Brush INHERIT VObject
	PROTECT hBrush   AS PTR
	PROTECT _hParent AS PTR

	METHOD __SetBrushOrg(_hDc AS PTR, hClient AS PTR) AS VOID STRICT 
	LOCAL sRect   	IS _winRect
	LOCAL sPoint	IS _winPoint	
	LOCAL hParent AS PTR

	IF _hParent == NULL_PTR
		//PP-040416 Issue 12706
		//for compatibility with the VO 2.6 behaviour, if no _hParent is known
		IF __WCGetControlByHandle(hClient) != NULL_OBJECT
			//hParent is the handle of the FormSurface
			hParent := GetParent(hClient)
			UnrealizeObject(hBrush)
			//RvdH 020705 ClientToScreen wants a Point not a Rect !
			ClientToScreen(hParent, @sPoint)
			SetBrushOrgEx(_hDc, sPoint:x, sPoint:y, NULL_PTR)
		ELSE
			RETURN
		ENDIF
	ELSE
		hParent := _hParent
	ENDIF

	IF hParent != hClient
		UnrealizeObject(hBrush)                                   
		GetWindowRect(hClient, @sRect)
		//RvdH 020705 ClientToScreen wants a Point not a Rect !
		sPoint:x := sRect:left
		sPoint:y := sRect:top
		ScreenToClient(hParent, @sPoint)
		SetStretchBltMode(_hDc, HALFTONE)
		SetBrushOrgEx(_hDc, -sPoint:x, -sPoint:y, NULL_PTR)
	ELSE
		UnrealizeObject(hBrush)
		SetBrushOrgEx(_hDc, 0, 0, NULL_PTR)
	ENDIF
	RETURN

METHOD CreateNew(xColor, kHatchStyle) 
	LOCAL argTypeError AS LOGIC

	

	IF (hBrush != NULL_PTR)
		DeleteObject(hBrush)
		hBrush := NULL_PTR
	ENDIF

	IF IsInstanceOfUsual(xColor, #Color)
		DEFAULT(@kHatchStyle, HATCHSOLID)

		IF IsNumeric(kHatchStyle)
			IF (kHatchStyle == HATCHSOLID)
				hBrush := CreateSolidBrush(xColor:ColorRef)
			ELSE
				hBrush := CreateHatchBrush(__ConvertHatch(kHatchStyle), xColor:ColorRef)
			ENDIF
		ELSE
			argTypeError := TRUE
		ENDIF

	ELSEIF IsInstanceOfUsual(xColor, #Bitmap)
		IF IsNil(kHatchStyle)
			hBrush := CreatePatternBrush(xColor:Handle())
		ELSE
			argTypeError := TRUE
		ENDIF

	ELSEIF IsNumeric(xColor)
		IF IsNil(kHatchStyle)
			hBrush := GetStockObject(__ConvertBrush(xColor))
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

METHOD Destroy() 
	

	IF (hBrush != NULL_PTR)
		DeleteObject(hBrush)
		hBrush := NULL_PTR
	ENDIF

	SUPER:Destroy()

	RETURN NIL

METHOD Handle() AS PTR
	

	RETURN hBrush

CONSTRUCTOR(xColor, kHatchStyle, oParent) 

	

	SUPER()

	SELF:CreateNew(xColor, kHatchStyle)

	SELF:Parent := oParent

	

	RETURN 

ASSIGN Parent (oWindow) 
	LOCAL oParent AS Window

	IF IsInstanceOfUsual(oWindow, #Window)
		IF IsInstanceOf(oWindow, #DataWindow)
			oParent := oWindow:__GetFormSurface()
		ELSE
			oParent := oWindow
		ENDIF
		_hParent := oParent:Handle()
	ELSE
		_hParent := NULL_PTR
	ENDIF
   RETURN 

END CLASS

FUNCTION __ConvertHatch(hatchStyle AS INT) AS INT STRICT
	LOCAL retVal AS INT

	SWITCH hatchStyle
	CASE HATCHDIAGONAL45
		retVal := HS_BDIAGONAL
	CASE  HATCHVERTICAL
		retVal := HS_VERTICAL
	CASE HATCHDIAGONAL135
		retVal := HS_FDIAGONAL
	CASE HATCHHORIZONTAL
		retVal := HS_HORIZONTAL
	CASE HATCHORTHOGONALCROSS
		retVal := HS_CROSS
	OTHERWISE
		retVal := HS_DIAGCROSS
	END SWITCH

	RETURN retVal

FUNCTION __ConvertBrush(brushType AS INT) AS INT STRICT
	LOCAL retVal AS INT

	SWITCH brushType 
	CASE BRUSHBLACK
		retVal := BLACK_BRUSH
	CASE BRUSHDARK
		retVal := DKGRAY_BRUSH
	CASE BRUSHMEDIUM
		retVal := GRAY_BRUSH
	CASE BRUSHLIGHT
		retVal := LTGRAY_BRUSH
	CASE BRUSHHOLLOW
		retVal := HOLLOW_BRUSH
	CASE BRUSHCLEAR
		retVal := NULL_BRUSH
	OTHERWISE
		retVal := WHITE_BRUSH
	END SWITCH

	RETURN retVal
