CLASS Pointer INHERIT VObject
	PROTECT hPointer AS PTR

METHOD Confine(oRect) 
	LOCAL oOrigin AS Point
	LOCAL oExtent AS Point
	LOCAL rect IS _WINRECT

	

	IF oRect:Left!=0 .OR. oRect:Right!=0 .OR. oRect:Top!=0 .OR. oRect:Bottom!=0
		oOrigin := __WCConvertPoint(NULL_OBJECT, oRect:Origin)
		oExtent := oRect:Extent

		rect:left := oOrigin:X
		rect:top := oOrigin:Y	
		IF WCGetCoordinateSystem() // cartesian
			rect:top := rect:top - oExtent:Y
		ENDIF
		rect:right := rect:left + oExtent:X
		rect:bottom	:= rect:top + oExtent:X

		ClipCursor(@rect)
	ELSE
		ClipCursor(NULL_PTR)
	ENDIF

	RETURN NIL

METHOD Handle() AS PTR
	

	RETURN hPointer

METHOD Hide() 
	

	RETURN ShowCursor(FALSE)

CONSTRUCTOR(xResourceID) 
	LOCAL hInst AS PTR
	LOCAL lpPointer AS PTR

	
	SUPER()

	DEFAULT(@xResourceID, POINTERARROW)

	IF IsNumeric(xResourceID) .OR. IsSymbol(xResourceID) .OR. IsString(xResourceID)
		hPointer := LoadCursor(0, __WCConvertPointer(xResourceID))
	ELSEIF IsInstanceOfUsual(xResourceID, #ResourceID)
		hInst := xResourceID:Handle()
		lpPointer := xResourceID:Address()

		hPointer := LoadCursor(hInst, lpPointer)
	ELSE
		WCError{#Init, #Pointer, __WCSTypeError}:@@Throw()
	ENDIF

	RETURN 

ACCESS Position 
	LOCAL pt IS _WINPOINT

	

	GetCursorPos(@pt)
	RETURN __WCConvertPoint(NULL_OBJECT, Point{pt:x, pt:y})

ASSIGN Position(oPoint) 
	LOCAL oTmp AS Point

	

	oTmp := __WCConvertPoint(NULL_OBJECT, oPoint)
	SetCursorPos(oTmp:X, oTmp:Y)

	RETURN 

METHOD Show()
	

	RETURN ShowCursor(TRUE)

END CLASS

FUNCTION __WCConvertPointer(pointerType AS INT) AS PSZ
	LOCAL retVal AS PTR


	SWITCH pointerType
	CASE PointerCrossHairs
		retVal := IDC_CROSS
	CASE PointerIBeam
		retVal := IDC_IBEAM
	CASE PointerIcon
		retVal := IDC_ICON
	CASE PointerFourArrow
		retVal := IDC_SIZEALL
	CASE PointerUpArrow
		retVal := IDC_UPARROW
	CASE PointerHourGlass
		retVal := IDC_WAIT
	CASE PointerAppStarting
		retVal := IDC_APPSTARTING
	OTHERWISE
		retVal := IDC_ARROW
	END SWITCH

	RETURN PSZ(_CAST,retVal)

