CLASS Pen INHERIT VObject
	HIDDEN hPen AS PTR

METHOD Destroy() 
	

	IF (hPen != NULL_PTR)
		DeleteObject(hPen)
		hPen := NULL_PTR
	ENDIF

	RETURN SELF

METHOD Handle ( uType ) 
	
	IF !IsNil(uType)
		IF !IsLong(uType)
			WCError{#Handle,#Pen,__WCSTypeError,uType}:@@Throw()
		ENDIF
	ENDIF

	RETURN hPen

CONSTRUCTOR(uColor, uLineStyle, uWidth) 
	LOCAL liStyle AS LONGINT

	
	SUPER()
	IF !IsNil(uColor)
		IF !IsInstanceOfUsual(uColor,#Color)
			WCError{#Init,#Pen,__WCSTypeError,uColor,1}:@@Throw()
		ENDIF
	ELSE
		uColor:=Color{}
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
	ELSE
		uWidth:=1
	ENDIF

	
	hPen:=CreatePen(liStyle,uWidth,uColor:ColorRef)

	RETURN 
END CLASS

