PARTIAL CLASS Brush INHERIT VObject
	PROTECT hBrush   AS PTR
	PROTECT _hParent AS PTR

	METHOD __SetBrushOrg(_hDc AS PTR, hClient AS PTR) AS VOID STRICT 
	LOCAL sRect   	IS _winRect
	LOCAL sPoint	IS _winPoint	
	LOCAL hParent AS PTR

	IF _hParent == Null_Ptr
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
		SetBrushOrgEx(_hDc, -sPoint:x, -sPoint:y, Null_Ptr)
	ELSE
		UnrealizeObject(hBrush)
		SetBrushOrgEx(_hDc, 0, 0, Null_Ptr)
	ENDIF
	RETURN

METHOD CreateNew(xColor, kHatchStyle) 
	LOCAL argTypeError AS LOGIC

	

	IF (hBrush != NULL_PTR)
		DeleteObject(hBrush)
		hBrush := NULL_PTR
	ENDIF

	IF IsInstanceOfUsual(xColor, #Color)
		Default(@kHatchStyle, HATCHSOLID)

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
		_hParent := Null_Ptr
	ENDIF
   RETURN 

END CLASS

FUNCTION __ConvertHatch(hatchStyle AS INT) AS INT STRICT
	LOCAL retVal AS INT

	DO CASE
	CASE hatchStyle == HATCHDIAGONAL45
		retVal := HS_BDIAGONAL
	CASE hatchStyle == HATCHVERTICAL
		retVal := HS_VERTICAL
	CASE hatchStyle == HATCHDIAGONAL135
		retVal := HS_FDIAGONAL
	CASE hatchStyle == HATCHHORIZONTAL
		retVal := HS_HORIZONTAL
	CASE hatchStyle == HATCHORTHOGONALCROSS
		retVal := HS_CROSS
	OTHERWISE
		retVal := HS_DIAGCROSS
	ENDCASE

	RETURN retVal

FUNCTION __ConvertBrush(brushType AS INT) AS INT STRICT
	LOCAL retVal AS INT

	DO CASE
	CASE brushType == BRUSHBLACK
		retVal := BLACK_BRUSH
	CASE brushType == BRUSHDARK
		retVal := DKGRAY_BRUSH
	CASE brushType == BRUSHMEDIUM
		retVal := GRAY_BRUSH
	CASE brushType == BRUSHLIGHT
		retVal := LTGRAY_BRUSH
	CASE brushType == BRUSHHOLLOW
		retVal := HOLLOW_BRUSH
	CASE brushType == BRUSHCLEAR
		retVal := NULL_BRUSH
	OTHERWISE
		retVal := WHITE_BRUSH
	ENDCASE

	RETURN retVal
