/// <include file="Gui.xml" path="doc/Color/*" />
CLASS Color INHERIT VObject
	PROTECT dwColorRef AS DWORD


	STATIC METHOD FromColorRef(nColor AS DWORD)
		LOCAL oCol AS Color
		oCol := Color{nColor}
		oCol:ColorRef := nColor
		RETURN oCol
/// <include file="Gui.xml" path="doc/Color.Blue/*" />

	ACCESS Blue AS BYTE
		RETURN Color.GetBValue(dwColorRef)

/// <include file="Gui.xml" path="doc/Color.Blue/*" />
	ASSIGN Blue(nBlue AS BYTE)
		dwColorRef := _OR(_AND(dwColorRef, 0X0000FFFF), _AND(DWORD(nBlue), 255) << 16)
		RETURN
/// <include file="Gui.xml" path="doc/Color.ColorRef/*" />

	PROPERTY ColorRef AS DWORD GET dwColorRef SET dwColorRef := Value

/// <include file="Gui.xml" path="doc/Color.Green/*" />
	ACCESS Green AS BYTE
		RETURN Color.GetGValue(dwColorRef)

/// <include file="Gui.xml" path="doc/Color.Green/*" />
	ASSIGN Green(nGreen AS BYTE)
		dwColorRef := _OR(_AND(dwColorRef, 0X00FF00FF), _AND(DWORD(nGreen), 255) << 8)
		RETURN

/// <include file="Gui.xml" path="doc/Color.ctor/*" />

	CONSTRUCTOR(nColor AS DWORD) STRICT
		LOCAL dwBlue  := 0 AS BYTE
		LOCAL dwRed   := 0 AS BYTE
		LOCAL dwGreen := 0 AS BYTE

		SUPER()

		IF (nColor <= 8)
			IF nColor=ColorRed .OR. ;
			    nColor=ColorMagenta .OR. ;
			    nColor=ColorYellow .OR. ;
			    nColor=ColorWhite

				dwRed:= 0XFF
			ENDIF

			IF nColor=ColorGreen .OR. ;
			    nColor=ColorCyan .OR. ;
			    nColor=ColorYellow .OR. ;
			    nColor=ColorWhite

				dwGreen:= 0XFF
			ENDIF

			IF nColor=ColorBlue .OR. ;
			    nColor=ColorCyan .OR. ;
			    nColor=ColorMagenta .OR. ;
			    nColor=ColorWhite

				dwBlue:= 0XFF
			ENDIF
			dwColorRef := (DWORD) RGB(dwRed, dwGreen, dwBlue)
		ELSE
			dwColorRef := (DWORD) nColor
		ENDIF

		RETURN


/// <include file="Gui.xml" path="doc/Color.ctor/*" />

	CONSTRUCTOR(nRed AS DWORD, nGreen AS DWORD, nBlue AS DWORD)
		SELF( (BYTE) nRed, (BYTE) nGreen, (BYTE) nBlue)
		RETURN

/// <include file="Gui.xml" path="doc/Color.ctor/*" />

	CONSTRUCTOR(nRed AS LONG, nGreen AS LONG, nBlue AS LONG)
		SELF( (BYTE) nRed, (BYTE) nGreen, (BYTE) nBlue)
		RETURN

/// <include file="Gui.xml" path="doc/Color.ctor/*" />

	CONSTRUCTOR(nRed AS BYTE, nGreen AS BYTE, nBlue AS BYTE)
		SUPER()
		dwColorRef := (DWORD) RGB(nRed, nGreen, nBlue)
		RETURN
/// <include file="Gui.xml" path="doc/Color.Red/*" />

	ACCESS Red AS BYTE
		RETURN Color.GetRValue(dwColorRef)
/// <include file="Gui.xml" path="doc/Color.Red/*" />

	ASSIGN Red(nRed AS BYTE)
		dwColorRef := _OR(_AND(dwColorRef, 0X00FFFF00), _AND(DWORD(nRed), 255))
		RETURN

	STATIC METHOD GetRValue(rgb AS DWORD) AS BYTE
		LOCAL val AS BYTE

		val:=BYTE(_CAST,rgb)
		RETURN val

	STATIC METHOD GetGValue(rgb AS DWORD) AS BYTE
		LOCAL val AS WORD
		LOCAL retval AS BYTE
		val := (WORD(_CAST,rgb))>>8
		retval := BYTE(_CAST,val)
		RETURN retval

	STATIC METHOD GetBValue(rgb AS DWORD) AS BYTE
		LOCAL val AS DWORD
		LOCAL retval AS BYTE

		val := rgb>>16
		retval := BYTE(_CAST,val)
		RETURN retval

	OPERATOR IMPLICIT ( c AS System.Drawing.Color) AS Color
		RETURN Color{c:R, c:G, c:B}

	OPERATOR IMPLICIT ( c AS Color ) AS System.Drawing.Color
		RETURN System.Drawing.Color.FromArgb(c:Red, c:Green, c:Blue)


END CLASS

