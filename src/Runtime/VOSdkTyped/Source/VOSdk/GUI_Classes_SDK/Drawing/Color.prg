//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

/// <include file="Gui.xml" path="doc/Color/*" />
CLASS Color INHERIT VObject
	PROTECT dwColorRef AS DWORD


    OPERATOR IMPLICIT ( li as LONG) AS Color
        return (Color) (DWORD) li
	OPERATOR IMPLICIT ( dw as DWORD) AS Color
        RETURN Color{} { ColorRef := dw}

	OPERATOR IMPLICIT ( oCol AS Color) AS DWORD
        RETURN oCol:ColorRef

    OPERATOR IMPLICIT ( oCol AS Color) AS LONG
        RETURN (LONG) oCol:ColorRef

/// <include file="Gui.xml" path="doc/Color.Blue/*" />

    PROPERTY Blue AS BYTE GET Color.GetBValue(dwColorRef) ;
        SET dwColorRef := _OR(_AND(dwColorRef, 0X0000FFFF), _AND(DWORD(value), 255) << 16)
/// <include file="Gui.xml" path="doc/Color.ColorRef/*" />

	PROPERTY ColorRef AS DWORD GET dwColorRef SET dwColorRef := Value

/// <include file="Gui.xml" path="doc/Color.Green/*" />
    PROPERTY Green AS BYTE GET Color.GetGValue(dwColorRef) ;
        SET dwColorRef := _OR(_AND(dwColorRef, 0X00FF00FF), _AND(DWORD(value), 255) << 8)

/// <include file="Gui.xml" path="doc/Color.ctor/*" />
        CONSTRUCTOR() STRICT
            SUPER()

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

    Property Red AS BYTE GET Color.GetRValue(dwColorRef) ;
        SET dwColorRef := _OR(_AND(dwColorRef, 0X00FFFF00), _AND(DWORD(value), 255))

    /// <exclude/>
	PRIVATE STATIC METHOD GetRValue(rgb AS DWORD) AS BYTE
		RETURN (BYTE) rgb

    /// <exclude/>
	PRIVATE STATIC METHOD GetGValue(rgb AS DWORD) AS BYTE
		LOCAL val AS WORD
		val := (WORD(_CAST,rgb))>>8
		RETURN (BYTE) val

    /// <exclude/>
	PRIVATE STATIC METHOD GetBValue(rgb AS DWORD) AS BYTE
		LOCAL val AS DWORD
		val := rgb>>16
		RETURN (BYTE) val

    /// <summary>
    /// Implicit operator to System.Drawing.Color to VO Color
    /// </summary>
    /// <param name="c">Color</param>
    /// <returns>Color</returns>

	OPERATOR IMPLICIT ( c AS System.Drawing.Color) AS Color
		RETURN Color{c:R, c:G, c:B}

    /// <summary>
    /// Implicit operator to convert VO Color to System.Drawing.Color
    /// </summary>
    /// <param name="c">Color</param>
    /// <returns>Color</returns>
	OPERATOR IMPLICIT ( c AS Color ) AS System.Drawing.Color
		RETURN System.Drawing.Color.FromArgb(c:Red, c:Green, c:Blue)


END CLASS

