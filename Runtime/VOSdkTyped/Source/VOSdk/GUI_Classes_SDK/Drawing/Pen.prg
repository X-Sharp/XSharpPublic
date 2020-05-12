

CLASS Pen INHERIT VObject
	HIDDEN hPen AS System.Drawing.Pen

METHOD Destroy() AS USUAL CLIPPER
	IF (hPen != NULL_OBJECT)
		hPen:Dispose()
		hPen := NULL_OBJECT
	ENDIF

	RETURN SELF

METHOD Handle  as System.Drawing.Pen STRICT
	RETURN hPen

CONSTRUCTOR(uColor, uLineStyle, uWidth) 
	LOCAL liStyle AS LONGINT
	LOCAL oColor as Color
	LOCAL liWidth as LONGINT

	SUPER()
	IF !IsNil(uColor)
		IF !IsInstanceOfUsual(uColor,#Color)
			WCError{#Init,#Pen,__WCSTypeError,uColor,1}:@@Throw()
		ENDIF
		oColor := uColor
	ELSE
		oColor:=Color{0}
		oColor:ColorRef := 0
	ENDIF

	IF !IsNil(uLineStyle)
		IF !IsLong(uLineStyle)
			WCError{#Init,#Pen,__WCSTypeError,uLineStyle,2}:@@Throw()
		ENDIF
		liStyle:=uLineStyle
	ENDIF

	IF !IsNil(uWidth)
		IF !IsLong(uWidth)
			WCError{#Init,#Pen,__WCSTypeError,uWidth,3}:@@Throw()
		ENDIF
		liWidth := uWidth
	ELSE
		liWidth:=1
	ENDIF
	hPen:=System.Drawing.Pen{oColor}
	hPen:Width := liWidth
	if liStyle != 0
		hPen:DashStyle := (System.Drawing.Drawing2D.DashStyle) liStyle
	ENDIF
	RETURN 
END CLASS

