CLASS Dimension INHERIT Pair
	ACCESS Height AS LONGINT STRICT 
	RETURN iInt2

ASSIGN Height(nHeight AS LONGINT)  STRICT 
	RETURN iInt2 := nHeight

CONSTRUCTOR(nWidth, nHeight) 
	SUPER(nWidth, nHeight)
	RETURN 

ACCESS Width AS LONGINT STRICT 
	RETURN iInt1

ASSIGN Width(nWidth AS LONGINT)  STRICT 
	RETURN iInt1 := nWidth

END CLASS

CLASS Pair INHERIT VObject
	PROTECT iInt1 AS INT
	PROTECT iInt2 AS INT

CONSTRUCTOR(uInt1, uInt2) 
	SUPER()   
	IF !IsNil(uInt1)
		iInt1 := uInt1
	ENDIF
	IF !IsNil(uInt2)
		iInt2 := uInt2
	ENDIF

	RETURN 

END CLASS

CLASS Point INHERIT Pair
	METHOD ConvertToScreen(oWindow) 
   LOCAL sPoint IS _winPoint
   LOCAL sRect  IS _winRect
   LOCAL hWnd   AS PTR

   IF IsPtr(oWindow)
      hWnd := oWindow
      IF hWnd = NULL_PTR
         RETURN FALSE
      ENDIF
      sRect:left := 0
   ELSEIF IsInstanceOfUsual(oWindow, #Window)
      hWnd       := oWindow:Handle(4)
      sRect:left := 1
   ELSEIF IsInstanceOfUsual(oWindow, #Control)
      hWnd       := oWindow:Handle()
      sRect:left := 0
   ELSE
         RETURN FALSE
   ENDIF

   sPoint:x := iInt1
   sPoint:y := iInt2

   IF WCGetCoordinateSystem()
      IF sRect:left = 1
         GetClientRect(hWnd, @sRect)
      ELSE
         GetWindowRect(hWnd, @sRect)
      ENDIF
      sPoint:y := sRect:bottom - sRect:top - sPoint:y
      ClientToScreen(hWnd, @sPoint)
      sPoint:y := GetSystemMetrics(SM_CYSCREEN) - sPoint:y
   ELSE
      ClientToScreen(hWnd, @sPoint)
   ENDIF

   iInt1 := sPoint:x
   iInt2 := sPoint:y
	RETURN TRUE

CONSTRUCTOR(nX, nY) 
	SUPER(nX, nY)
	RETURN 

ACCESS X  AS LONGINT STRICT 
	RETURN iInt1

ASSIGN X(nX AS LONGINT)   STRICT 
	RETURN iInt1 := nX

ACCESS Y  AS LONGINT STRICT 
	RETURN iInt2

ASSIGN Y(nY AS LONGINT)   STRICT 
	RETURN iInt2 := nY

END CLASS

CLASS Range INHERIT Pair        
	CONSTRUCTOR(nMin, nMax) 
	SUPER(nMin, nMax)
	RETURN 

METHOD IsInRange(nValue) 
   //SE-060525
   LOCAL iVal AS INT
   IF IsNumeric(nValue)
  	   iVal := nValue
  	   IF iVal >= iInt1 .AND. iVal <= iInt2
  	  	   RETURN TRUE
  	   ENDIF
   ENDIF

   RETURN FALSE

ACCESS Max AS LONGINT STRICT 
	RETURN iInt2

ASSIGN Max(nMax AS LONGINT)  STRICT 
	RETURN iInt2 := nMax

ACCESS Min AS LONGINT STRICT 
	RETURN iInt1

ASSIGN Min(nMin AS LONGINT)  STRICT 
	RETURN iInt1 := nMin

END CLASS

CLASS Selection INHERIT Pair 
	ACCESS Finish AS LONGINT STRICT 
	RETURN iInt2

ASSIGN Finish(nFinish AS LONGINT)  STRICT 
	RETURN iInt2 := nFinish

CONSTRUCTOR(nStart, nFinish) 
	SUPER(nStart, nFinish)
	RETURN 

ACCESS Start AS LONGINT STRICT 
	RETURN iInt1

ASSIGN Start(nStart AS LONGINT)  STRICT 
	RETURN iInt1 := nStart

END CLASS

