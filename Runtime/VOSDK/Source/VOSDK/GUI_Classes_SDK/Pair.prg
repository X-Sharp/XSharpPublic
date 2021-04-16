/// <include file="Gui.xml" path="doc/Dimension/*" />
CLASS Dimension INHERIT Pair
/// <include file="Gui.xml" path="doc/Dimension.Height/*" />
	ACCESS Height AS LONGINT STRICT 
	RETURN iInt2


/// <include file="Gui.xml" path="doc/Dimension.Height/*" />
ASSIGN Height(nHeight AS LONGINT)  STRICT 
	RETURN iInt2 := nHeight


/// <include file="Gui.xml" path="doc/Dimension.ctor/*" />
CONSTRUCTOR(nWidth, nHeight) 
	SUPER(nWidth, nHeight)
	RETURN 


/// <include file="Gui.xml" path="doc/Dimension.Width/*" />
ACCESS Width AS LONGINT STRICT 
	RETURN iInt1


/// <include file="Gui.xml" path="doc/Dimension.Width/*" />
ASSIGN Width(nWidth AS LONGINT)  STRICT 
	RETURN iInt1 := nWidth


END CLASS


/// <include file="Gui.xml" path="doc/Pair/*" />
CLASS Pair INHERIT VObject
	PROTECT iInt1 AS INT
	PROTECT iInt2 AS INT


/// <include file="Gui.xml" path="doc/Pair.ctor/*" />
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


/// <include file="Gui.xml" path="doc/Point/*" />
CLASS Point INHERIT Pair
/// <include file="Gui.xml" path="doc/Point.ConvertToScreen/*" />
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


/// <include file="Gui.xml" path="doc/Point.ctor/*" />
CONSTRUCTOR(nX, nY) 
	SUPER(nX, nY)
	RETURN 


/// <include file="Gui.xml" path="doc/Point.X/*" />
ACCESS X  AS LONGINT STRICT 
	RETURN iInt1


/// <include file="Gui.xml" path="doc/Point.X/*" />
ASSIGN X(nX AS LONGINT)   STRICT 
	RETURN iInt1 := nX


/// <include file="Gui.xml" path="doc/Point.Y/*" />
ACCESS Y  AS LONGINT STRICT 
	RETURN iInt2


/// <include file="Gui.xml" path="doc/Point.Y/*" />
ASSIGN Y(nY AS LONGINT)   STRICT 
	RETURN iInt2 := nY


END CLASS


/// <include file="Gui.xml" path="doc/Range/*" />
CLASS Range INHERIT Pair        
/// <include file="Gui.xml" path="doc/Range.ctor/*" />
	CONSTRUCTOR(nMin, nMax) 
	SUPER(nMin, nMax)
	RETURN 


/// <include file="Gui.xml" path="doc/Range.IsInRange/*" />
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


/// <include file="Gui.xml" path="doc/Range.Max/*" />
ACCESS Max AS LONGINT STRICT 
	RETURN iInt2


/// <include file="Gui.xml" path="doc/Range.Max/*" />
ASSIGN Max(nMax AS LONGINT)  STRICT 
	RETURN iInt2 := nMax


/// <include file="Gui.xml" path="doc/Range.Min/*" />
ACCESS Min AS LONGINT STRICT 
	RETURN iInt1


/// <include file="Gui.xml" path="doc/Range.Min/*" />
ASSIGN Min(nMin AS LONGINT)  STRICT 
	RETURN iInt1 := nMin


END CLASS


/// <include file="Gui.xml" path="doc/Selection/*" />
CLASS Selection INHERIT Pair 
/// <include file="Gui.xml" path="doc/Selection.Finish/*" />
	ACCESS Finish AS LONGINT STRICT 
	RETURN iInt2


/// <include file="Gui.xml" path="doc/Selection.Finish/*" />
ASSIGN Finish(nFinish AS LONGINT)  STRICT 
	RETURN iInt2 := nFinish


/// <include file="Gui.xml" path="doc/Selection.ctor/*" />
CONSTRUCTOR(nStart, nFinish) 
	SUPER(nStart, nFinish)
	RETURN 


/// <include file="Gui.xml" path="doc/Selection.Start/*" />
ACCESS Start AS LONGINT STRICT 
	RETURN iInt1


/// <include file="Gui.xml" path="doc/Selection.Start/*" />
ASSIGN Start(nStart AS LONGINT)  STRICT 
	RETURN iInt1 := nStart


END CLASS


