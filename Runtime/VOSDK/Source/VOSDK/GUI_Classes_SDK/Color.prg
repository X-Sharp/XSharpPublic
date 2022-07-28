/// <include file="Gui.xml" path="doc/Color/*" />
CLASS Color INHERIT VObject
	PROTECT dwColorRef AS DWORD


/// <include file="Gui.xml" path="doc/Color.Blue/*" />
ACCESS Blue


	RETURN GetBValue(dwColorRef)


/// <include file="Gui.xml" path="doc/Color.Blue/*" />
ASSIGN Blue(nBlue)


	dwColorRef := _OR(_AND(dwColorRef, 0x0000FFFF), _AND(DWORD(nBlue), 255) << 16)
	RETURN


/// <include file="Gui.xml" path="doc/Color.ColorRef/*" />
ACCESS ColorRef
	RETURN dwColorRef


/// <include file="Gui.xml" path="doc/Color.ColorRef/*" />
ASSIGN ColorRef(n)
	RETURN dwColorRef := n


/// <include file="Gui.xml" path="doc/Color.Green/*" />
ACCESS Green


	RETURN GetGValue(dwColorRef)


/// <include file="Gui.xml" path="doc/Color.Green/*" />
ASSIGN Green(nGreen)


	dwColorRef := _OR(_AND(dwColorRef, 0x00FF00FF), _AND(DWORD(nGreen), 255) << 8)
	RETURN


/// <include file="Gui.xml" path="doc/Color.ctor/*" />
CONSTRUCTOR(nRed, nGreen, nBlue)
	LOCAL nColor  AS LONGINT
	LOCAL dwBlue  AS DWORD
	LOCAL dwRed   AS DWORD
	LOCAL dwGreen AS DWORD
	//RvdH 050909 Optimize by checking PCOUNT
	//SE-070427 removed PCount() for future Vulcan compatibility


	SUPER()


	IF IsNumeric(nRed) .AND. IsNil(nBlue)
		IF (nRed < 8)
			IF IsNil(nGreen)		// e.g. Color{ColorMagenta}
				nColor:=nRed
				IF nColor=ColorRed .OR. ;
						nColor=ColorMagenta .OR. ;
						nColor=ColorYellow .OR. ;
						nColor=ColorWhite


					dwRed:= 0xFF
				ENDIF


				IF nColor=ColorGreen .OR. ;
						nColor=ColorCyan .OR. ;
						nColor=ColorYellow .OR. ;
						nColor=ColorWhite


					dwGreen:= 0xFF
				ENDIF


				IF nColor=ColorBlue .OR. ;
						nColor=ColorCyan .OR. ;
						nColor=ColorMagenta .OR. ;
						nColor=ColorWhite


					dwBlue:= 0xFF
				ENDIF
				dwColorRef := RGB(BYTE(dwRed), BYTE(dwGreen), BYTE(dwBlue))
			ELSEIF IsNumeric(nGreen) .AND. nGreen < 0  	// e.g. Color{0xFF00FF, -1} for Magenta
				dwColorRef := (DWORD) _AND(LONGINT(nRed), 0x00FFFFFF)
			ENDIF
		ELSE
			dwColorRef := nRed
		ENDIF
	ELSE
		IF IsNumeric(nRed)
			dwRed := _AND(DWORD(nRed), 0xFF)
		ENDIF
		IF IsNumeric(nBlue)
			dwBlue := _AND(DWORD(nBlue), 0xFF)
		ENDIF
		IF IsNumeric(nGreen)
			dwGreen:=_AND(DWORD(nGreen), 0xFF)
		ENDIF
		dwColorRef := RGB(BYTE(dwRed), BYTE(dwGreen), BYTE(dwBlue))
	ENDIF


	RETURN


/// <include file="Gui.xml" path="doc/Color.Red/*" />
ACCESS Red


	RETURN GetRValue(dwColorRef)


/// <include file="Gui.xml" path="doc/Color.Red/*" />
ASSIGN Red(nRed)




	dwColorRef := _OR(_AND(dwColorRef, 0x00FFFF00), _AND(DWORD(nRed), 255))


	RETURN
END CLASS


