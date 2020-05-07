

#USING System.Diagnostics

[DebuggerStepThrough];
[DebuggerDisplay("{iInt1}, {iInt2}")];
CLASS Pair INHERIT VObject
	PROTECT iInt1 AS INT
	PROTECT iInt2 AS INT
	CONSTRUCTOR() STRICT
		SUPER()
	
	CONSTRUCTOR(Int1 AS INT, Int2 AS INT) 
		SUPER()   
		iInt1 := Int1
		iInt2 := Int2
		RETURN 

	ACCESS Empty AS LOGIC
		RETURN iInt1 == 0 .and. iInt2 == 0

//   OPERATOR ==( lhs AS Pair, rhs AS Pair ) AS LOGIC
//      RETURN lhs:Equals( rhs )
      
//   OPERATOR !=( lhs AS Pair, rhs AS Pair ) AS LOGIC
//      RETURN ! lhs:Equals( rhs )

//	METHOD Equals( p AS Pair ) AS LOGIC
//      LOCAL ret AS LOGIC
//	  ret := (p:iInt1 == iInt1 .and. p:iInt2 == iInt2)
//      RETURN ret

//	METHOD Equals( p AS OBJECT ) AS LOGIC
//      RETURN SELF:Equals( (Pair) p)

	METHOD GetHashCode AS LONG STRICT
		return iInt1:GetHashCode() + iInt2:GetHashCode()

END CLASS

[DebuggerStepThrough];
[DebuggerDisplay("Width: {Width}, Height: {Height}")];
CLASS Dimension INHERIT Pair

	CONSTRUCTOR() STRICT
		SUPER()

	CONSTRUCTOR(nWidth AS INT, nHeight AS INT) 
		SUPER(nWidth, nHeight)
		RETURN 

	PROPERTY Width  AS LONGINT  GET iInt1 SET iInt1 := Value
	PROPERTY Height  AS LONGINT  GET iInt2 SET iInt2 := Value
	
	METHOD Clone() AS Dimension
		RETURN Dimension{SELF:Width, SELF:Height}
		
	OPERATOR IMPLICIT ( s AS System.Drawing.Size) AS Dimension
		RETURN Dimension{s:Width, s:Height}

	OPERATOR IMPLICIT ( d AS Dimension ) AS System.Drawing.Size
		//IF d == NULL_OBJECT
		//	RETURN System.Drawing.Size.Empty
		//ENDIF
		RETURN System.Drawing.Size{d:Width, d:Height}

	OPERATOR IMPLICIT ( r AS System.Drawing.Rectangle) AS Dimension
		RETURN Dimension{r:Width, r:Height}

END CLASS


[DebuggerStepThrough];
[DebuggerDisplay("X: {X}, Y: {Y}")];
CLASS Point INHERIT Pair

	CONSTRUCTOR() STRICT
		SUPER()
		RETURN 

	CONSTRUCTOR(nX AS INT, nY AS INT) 
		SUPER(nX, nY)
		RETURN 

	METHOD ConvertToScreen(hWnd AS IntPtr) AS LOGIC 
		RETURN SELF:ConvertToScreen(hWnd, TRUE)

	METHOD ConvertToScreen(hWnd AS IntPtr, lWinRect AS LOGIC) AS LOGIC 
		//Todo
		LOCAL sPoint := WINPOINT{} AS WINPOINT

		IF hWnd = IntPtr.Zero
			RETURN FALSE
		ENDIF

		sPoint:x := iInt1
		sPoint:y := iInt2

		Win32.ClientToScreen(hWnd, REF sPoint)

		iInt1 := sPoint:x
		iInt2 := sPoint:y
		
		RETURN TRUE

	METHOD ConvertToScreen(oWindow AS OBJECT) AS LOGIC  
		LOCAL hWnd   AS PTR
		LOCAL lOk AS LOGIC
		IF IsInstanceOfUsual(oWindow, #Window)
			hWnd    := oWindow:Handle(4)
			lOk		:= SELF:ConvertToScreen(hWnd, TRUE)
		ELSEIF IsInstanceOfUsual(oWindow, #Control)
			hWnd    := oWindow:Handle()
			lOk		:= SELF:ConvertToScreen(hWnd, FALSE)
		ELSE
			lOk := FALSE
		ENDIF
		RETURN lOk

	PROPERTY X  AS LONGINT  GET iInt1 SET iInt1 := Value
	PROPERTY Y  AS LONGINT  GET iInt2 SET iInt2 := Value

	METHOD Clone() AS Point
		RETURN Point{SELF:X, SELF:Y}

	OPERATOR IMPLICIT ( p AS System.Drawing.Point) AS Point
		RETURN Point{p:X, p:Y}

	OPERATOR IMPLICIT ( p AS Point ) AS System.Drawing.Point
		//IF p == NULL_OBJECT
		//	RETURN System.Drawing.Point.Empty
		//ENDIF

		RETURN System.Drawing.Point{p:X, p:Y}

	OPERATOR + (p1 AS Point, p2 AS Point) AS Point
		LOCAL r AS Point
		IF p2:Empty
			RETURN p1
		ELSE
			r := p1:Clone()
			r:iInt1 += p2:iInt1
			r:iInt2 += p2:iInt2
			RETURN r
		ENDIF

	OPERATOR - (p1 AS Point, p2 AS Point) AS Point
		LOCAL r AS Point
		IF p2:Empty
			RETURN p1
		ELSE
			r := p1:Clone()
			r:iInt1 -= p2:iInt1
			r:iInt2 -= p2:iInt2
			RETURN r
		ENDIF		


END CLASS

[DebuggerStepThrough];
[DebuggerDisplay("Min: {Min}, Max: {Max}")];
CLASS Range INHERIT Pair        
	CONSTRUCTOR(nMin, nMax) 
		SUPER(nMin, nMax)
		RETURN 

	METHOD IsInRange(nValue AS INT) AS LOGIC
		LOCAL iVal AS INT
		iVal := nValue
		IF iVal >= iInt1 .AND. iVal <= iInt2
			RETURN TRUE
		ENDIF

		RETURN FALSE

	PROPERTY Min AS LONGINT  GET iInt1 SET iInt1 := Value
	PROPERTY Max AS LONGINT  GET iInt2 SET iInt2 := Value

END CLASS

[DebuggerStepThrough];
[DebuggerDisplay("Start: {Start}, Finish: {Finish}")];
CLASS Selection INHERIT Pair 

	PROPERTY Start  AS LONGINT  GET iInt1 SET iInt1 := Value
	PROPERTY Finish AS LONGINT  GET iInt2 SET iInt2 := Value

	CONSTRUCTOR(nStart, nFinish) 
		SUPER(nStart, nFinish)
		RETURN 


END CLASS

