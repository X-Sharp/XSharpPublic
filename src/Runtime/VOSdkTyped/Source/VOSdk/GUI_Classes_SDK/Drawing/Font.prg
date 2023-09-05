//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//



#define __WCFixedPitch 1
#define __WCVariablePitch 2
USING System.Runtime.InteropServices
/// <include file="Gui.xml" path="doc/Font/*" />
CLASS Font INHERIT VObject
    PROTECT oFont 			AS System.Drawing.Font
    INTERNAL oLogFont		AS Font.LOGFONT
    PROTECT lStdFont 		AS LOGIC
    PROTECT iStdFontType	AS INT
    PROTECT bFontChanged	AS LOGIC
    PROTECT iPointSize 		AS INT
    STATIC PROTECT aFontNames AS System.Collections.Generic.Dictionary<STRING,STRING>
    STATIC CONSTRUCTOR()
        aFontNames := System.Collections.Generic.Dictionary<STRING,STRING>{}


    ACCESS __Font AS System.Drawing.Font
        IF (oFont == NULL_OBJECT .or. SELF:bFontChanged)
            SELF:Create()
        ENDIF
        RETURN oFont

    ASSIGN __Font (oF AS System.Drawing.Font)
        oFont := oF
        SELF:__ToLogFont()

    METHOD __ToLogFont() AS VOID STRICT
        IF oFont != NULL_OBJECT
            oFont:ToLogFont(oLogFont)
            iPointSize := Convert.ToInt32(oFont:SizeInPoints)
        ENDIF


    ACCESS __FontCharSet AS INT STRICT
        RETURN oLogFont:CharSet

    /// <exclude />
    ACCESS __FontFaceName AS STRING STRICT
        SELF:Create()
        RETURN oFont:Name

    /// <exclude />
    ACCESS __FontHeight AS INT STRICT
        SELF:Create()
        RETURN oLogFont:Height

    /// <exclude />
    ACCESS __FontPitchAndFamily AS INT STRICT
        SELF:Create()
        RETURN oLogFont:PitchAndFamily

    /// <exclude />
    ACCESS __PointSize AS INT STRICT
        RETURN iPointSize

    /// <include file="Gui.xml" path="doc/Font.PointSize/*" />
    ACCESS PointSize AS INT STRICT
        RETURN iPointSize

    /// <exclude />
    ASSIGN __PointSize(x AS INT)  STRICT
        iPointSize := x
        bFontChanged := TRUE

    /// <include file="Gui.xml" path="doc/Font.Bold/*" />
    ACCESS Bold AS LOGIC
        SELF:Create()
        RETURN oLogFont:Weight == FontWeight.Heavy

    /// <include file="Gui.xml" path="doc/Font.Bold/*" />
    ASSIGN Bold(lfState AS LOGIC)
        LOCAL lfNewWeight AS WORD

        IF lfState == TRUE
            lfNewWeight := (WORD) FontWeight.Heavy
        ELSE
            lfNewWeight := (WORD) FontWeight.Normal
        ENDIF

        IF oLogFont:Weight != lfNewWeight
            oLogFont:Weight := lfNewWeight
            bFontChanged := TRUE
        ENDIF

        RETURN

    /// <include file="Gui.xml" path="doc/Font.CharSet/*" />
    ACCESS CharSet  AS BYTE
        SELF:Create()
        RETURN oLogFont:CharSet

    /// <include file="Gui.xml" path="doc/Font.ClipPrecision/*" />
    PROPERTY ClipPrecision AS BYTE
    GET
        SELF:Create()
        RETURN oLogFont:ClipPrecision
    END GET
    SET
        SELF:Create()
        IF oLogFont:ClipPrecision<>value
            oLogFont:ClipPrecision := value
            SELF:bFontChanged := TRUE
        ENDIF
    END SET
    END PROPERTY
    /// <include file="Gui.xml" path="doc/Font.ConvPntToDim/*" />
    METHOD ConvPntToDim(nPntSize AS LONG, hDCConv AS IntPtr) AS Dimension
        LOCAL hDC AS IntPtr
        LOCAL wDim AS Dimension

        IF (hDCConv == NULL_PTR) .AND. ((hDC := GuiWin32.GetDC(NULL_PTR)) != NULL_PTR)
            wDim := Dimension{0, -(GuiWin32.MulDiv(nPntSize, GuiWin32.GetDeviceCaps(hDC, LOGPIXELSY), 72))}
            GuiWin32.ReleaseDC(NULL_PTR, hDC)
        ELSEIF (hDCConv != NULL_PTR)
            wDim := Dimension{0, -(GuiWin32.MulDiv(nPntSize, GuiWin32.GetDeviceCaps(hDCConv, LOGPIXELSY), 72))}
        ELSE
            wDim := Dimension{nPntSize, nPntSize}
        ENDIF

        RETURN wDim

    /// <include file="Gui.xml" path="doc/Font.Create/*" />
    METHOD Create() STRICT
        RETURN SELF:Create(FALSE, NULL_PTR)

    /// <include file="Gui.xml" path="doc/Font.Create/*" />
    METHOD Create(lPrinter := FALSE AS LOGIC, uhdc := NIL AS USUAL)
        LOCAL oDim	  AS Dimension
        LOCAL hdc	  AS IntPtr
        IF uhdc != NIL
            hdc := (IntPtr) uhdc
        ENDIF

        IF (oFont != NULL_OBJECT) .AND. bFontChanged
            oFont:Dispose()
            oFont := NULL_OBJECT
        ENDIF

        IF (oFont == NULL_OBJECT)

            IF oLogFont:Height == 0 .and. oLogFont:Height == 0 .and. iPointSize > 0
                oDim 			:= SELF:ConvPntToDim(iPointSize, hdc)
                oLogFont:Height	:= oDim:Height
                oLogFont:Width 	:= oDim:Width
            ENDIF
            IF lPrinter .AND. (hdc != NULL_PTR)
                IF lStdFont
                    oLogFont:Height := -(((GuiWin32.GetDeviceCaps(hdc, LOGPIXELSY) * StdFontPointSize[iStdFontType + 1]) + 36) / 72)
                ELSEIF (iPointSize > 0)
                    oLogFont:Height := -GuiWin32.MulDiv(iPointSize, GuiWin32.GetDeviceCaps(hdc, LOGPIXELSY), 72)
                ENDIF
            ENDIF

            // Check if we know the font name
            IF ! aFontNames:ContainsKey(oLogFont:FaceName)
                oFont := System.Drawing.Font{oLogFont:FaceName, iPointSize, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point}
                aFontNames:Add(oLogFont:FaceName, oFont:Name)
                oFont:Dispose()
            ENDIF
            oLogFont:FaceName := aFontNames[oLogFont:FaceName]
            TRY
                //oLogFont:Height := (INT)  Convert.ToInt32(oLogFont:Height * 0.95)
                //oFont := System.Drawing.Font.FromLogFont(oLogFont)
                LOCAL nStyle AS System.Drawing.FontStyle
                IF oLogFont:Weight > FontWeight.Normal
                    nStyle := System.Drawing.FontStyle.Bold
                ELSE
                    nStyle := System.Drawing.FontStyle.Regular
                ENDIF
                IF oLogFont:Italic != 0
                    nStyle |= System.Drawing.FontStyle.Italic
                ENDIF
                IF oLogFont:Underline != 0
                    nStyle |= System.Drawing.FontStyle.Underline
                ENDIF
                IF oLogFont:StrikeOut != 0
                    nStyle |= System.Drawing.FontStyle.Strikeout
                ENDIF
                oFont := System.Drawing.Font{oLogFont:FaceName, (REAL4)(iPointSize),  nStyle, System.Drawing.GraphicsUnit.Point}

            CATCH   AS Exception
                oFont := NULL_OBJECT
            END TRY
            IF oFont == NULL_OBJECT
                IF iPointSize == 0
                    iPointSize := 8
                ENDIF
                oFont := System.Drawing.Font{oLogFont:FaceName, iPointSize, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point}
            ENDIF
            SELF:__ToLogFont()
        ENDIF

        bFontChanged := FALSE
        RETURN SELF


    /// <include file="Gui.xml" path="doc/Font.Destroy/*" />
    METHOD Destroy() AS USUAL  CLIPPER

        SUPER:Destroy()

        IF (oFont != NULL_OBJECT)
            oFont:Dispose()
            oFont := NULL_OBJECT
        ENDIF

        RETURN NIL

    /// <include file="Gui.xml" path="doc/Font.Escapement/*" />
    PROPERTY Escapement AS INT
    GET
        SELF:Create()
        RETURN oLogFont:Escapement
    END GET
    END PROPERTY

    /// <include file="Gui.xml" path="doc/Font.FaceName/*" />
    PROPERTY FaceName AS STRING
    GET
        SELF:Create()
        RETURN oFont:Name
    END GET
    END PROPERTY
    /// <include file="Gui.xml" path="doc/Font.Handle/*" />
    METHOD Handle() AS IntPtr STRICT
        SELF:Create()
        RETURN oFont:ToHfont()

    /// <include file="Gui.xml" path="doc/Font.Height/*" />
    PROPERTY Height AS INT
    GET
        SELF:Create()
        RETURN oLogFont:Height
    END GET
    SET
        SELF:Create()
        IF oLogFont:Height != value
            SELF:bFontChanged := TRUE
            oLogFont:Height:= value
        ENDIF
    END SET
    END PROPERTY


    /// <include file="Gui.xml" path="doc/Font.ctor/*" />
    CONSTRUCTOR(kFont, oDimension, sTypeFace)
        SUPER()
        oLogFont := Font.LOGFONT{}
        IF kFont  is System.Drawing.Font VAR oFt
            oFont := oFt
            SELF:__ToLogFont()
            RETURN
        ENDIF
        oLogFont:Weight := FontWeight.Normal

        //IF WC.IsTrueTypeEnabled()
        //	IF WC.UseTrueTypeOnly()
        oLogFont:OutPrecision := OUT_TT_ONLY_PRECIS
        //	ELSE
        //		oLogFont:OutPrecision := OUT_TT_PRECIS
        //	ENDIF
        //ELSE
        //	oLogFont:OutPrecision := OUT_RASTER_PRECIS
        //ENDIF

        oLogFont:Quality 			:= DRAFT_QUALITY
        oLogFont:ClipPrecision 		:= CLIP_STROKE_PRECIS
        oLogFont:CharSet 			:= ANSI_CHARSET
        oLogFont:PitchAndFamily 	:= 0 // _Or(DEFAULT_PITCH, FF_DONTCARE)

        IF IsNil(oDimension)
            lStdFont 	 := TRUE
            iStdFontType := kFont

            oLogFont:Height := GetStdFontScreenHeight(iStdFontType)
            oLogFont:Width := 0

            oLogFont:PitchAndFamily := (BYTE) _OR(oLogFont:PitchAndFamily, WORD(_CAST, StdFontFamily[iStdFontType + 1]))
        ELSE
            IF IsNumeric(oDimension)
                iPointSize := oDimension
            elseif oDimension is Dimension var oDim
                oLogFont:Height := oDim:Height
                oLogFont:Width := oDim:Width
            ENDIF

            DEFAULT( REF kFont, FONTANY)

            IF !IsNil(sTypeFace)
                oLogFont:FaceName := sTypeFace
                oLogFont:CharSet := DEFAULT_CHARSET
            ENDIF

            oLogFont:PitchAndFamily := (BYTE) _OR(oLogFont:PitchAndFamily, ConvertFont(kFont))

            IF kFont == FontScript
                oLogFont:OutPrecision := OUT_STROKE_PRECIS
                oLogFont:CharSet := DEFAULT_CHARSET
            ENDIF
        ENDIF

        bFontChanged := TRUE
        RETURN

    /// <include file="Gui.xml" path="doc/Font.Italic/*" />
    PROPERTY Italic  AS LOGIC
    GET
        SELF:Create()
        RETURN oLogFont:Italic != 0
    END GET
    SET
        SELF:Create()
        IF oLogFont:Italic != iif(value,1,0)
            oLogFont:Italic := iif(value,1,0)
            bFontChanged := TRUE
        ENDIF
    END SET
    END PROPERTY


    /// <include file="Gui.xml" path="doc/Font.Light/*" />
    PROPERTY Light  AS LOGIC
    GET
        SELF:Create()
        RETURN oLogFont:Weight == FontWeight.Light
    END GET

    SET
        SELF:Create()
        VAR lfNewWeight := (WORD) iif (value, FontWeight.Light, FontWeight.Normal)
        IF oLogFont:Weight != lfNewWeight
            oLogFont:Weight := lfNewWeight
            bFontChanged := TRUE
        ENDIF

    END SET
    END PROPERTY

    /// <include file="Gui.xml" path="doc/Font.Normal/*" />
    PROPERTY Normal  AS LOGIC
    GET
        SELF:Create()
        RETURN oLogFont:Weight == FontWeight.Normal
    END GET
    SET
        SELF:Create()
        VAR lfNewWeight := (WORD) iif (value, FontWeight.Normal, FontWeight.Heavy)
        IF oLogFont:Weight != lfNewWeight
            oLogFont:Weight := lfNewWeight
            bFontChanged := TRUE
        ENDIF

    END SET
    END PROPERTY

    /// <include file="Gui.xml" path="doc/Font.Orientation/*" />
    PROPERTY Orientation AS LONG GET oLogFont:Orientation

    /// <include file="Gui.xml" path="doc/Font.OutPrecision/*" />
    PROPERTY OutPrecision  AS BYTE
    GET
        SELF:Create()
        RETURN oLogFont:OutPrecision
    END GET
    SET
        SELF:Create()
        IF oLogFont:OutPrecision<>value
            oLogFont:OutPrecision := value
            SELF:bFontChanged := TRUE
        ENDIF

    END SET
    END PROPERTY

    /// <include file="Gui.xml" path="doc/Font.PitchAndFamily/*" />
    ACCESS PitchAndFamily AS BYTE
        SELF:Create()
        RETURN oLogFont:PitchAndFamily

    /// <include file="Gui.xml" path="doc/Font.Family/*" />
    ASSIGN Family(nFamily AS BYTE)
        LOCAL lfNewPitchAndFamily	AS BYTE
        LOCAL lfFamily				AS WORD
        SELF:Create()

        lfFamily := nFamily
        lfNewPitchAndFamily := (BYTE) _OR(_AND(oLogFont:PitchAndFamily,0X0F),lfFamily)

        IF lfNewPitchAndFamily <> oLogFont:PitchAndFamily
            oLogFont:PitchAndFamily := lfNewPitchAndFamily
            SELF:bFontChanged := TRUE
        ENDIF

        RETURN


    /// <include file="Gui.xml" path="doc/Font.PitchFixed/*" />
    PROPERTY PitchFixed  AS LOGIC GET !SELF:PitchVariable SET SELF:PitchVariable := !value

    /// <include file="Gui.xml" path="doc/Font.PitchVariable/*" />
    PROPERTY PitchVariable AS LOGIC
    GET
        SELF:Create()
        RETURN _AND(oLogFont:PitchAndFamily, 0x03) == 2
    END GET
    SET
        LOCAL lfNewPitchAndFamily AS BYTE
        SELF:Create()
        IF value == TRUE
            lfNewPitchAndFamily := (BYTE) _OR(_AND(oLogFont:PitchAndFamily, 0xFC), __WCVariablePitch)
        ELSE
            lfNewPitchAndFamily := (BYTE) _OR(_AND(oLogFont:PitchAndFamily, 0xFC), __WCFixedPitch)
        ENDIF

        IF oLogFont:PitchAndFamily != lfNewPitchAndFamily
            oLogFont:PitchAndFamily := lfNewPitchAndFamily
            bFontChanged := TRUE
        ENDIF
    END SET
    END PROPERTY

    /// <include file="Gui.xml" path="doc/Font.Quality/*" />
    PROPERTY Quality AS BYTE
        GET
            SELF:Create()
            RETURN oLogFont:Quality
    END GET
    END PROPERTY

    /// <include file="Gui.xml" path="doc/Font.Size/*" />
    PROPERTY Size AS LONG GET SELF:iPointSize

    /// <include file="Gui.xml" path="doc/Font.SizeText/*" />
    METHOD SizeText(cString AS STRING)  AS Dimension
        LOCAL oSize AS System.Drawing.Size
        LOCAL oDim AS Dimension
        oSize := System.Windows.Forms.TextRenderer.MeasureText(cString, oFont)
        oDim := oSize
        RETURN oDim


    /// <include file="Gui.xml" path="doc/Font.Strikethru/*" />
    PROPERTY Strikethru  AS LOGIC
    GET
        RETURN oLogFont:StrikeOut != 0
    END GET
    SET
        IF oLogFont:StrikeOut != iif(value,1,0)
            oLogFont:StrikeOut := iif(value,1,0)
            bFontChanged := TRUE
        ENDIF
    END SET
    END PROPERTY
   /// <include file="Gui.xml" path="doc/Font.Underline/*" />
    PROPERTY Underline AS LOGIC
    GET
        SELF:Create()
        RETURN oLogFont:Underline != 0
    END GET
    SET
        SELF:Create()
        IF oLogFont:Underline != iif(value,1,0)
            oLogFont:Underline := iif(value,1,0)
            bFontChanged := TRUE
        ENDIF
    END SET
    END PROPERTY

    PROPERTY Weight AS LONG GET oLogFont:Weight
    PROPERTY Width  AS LONG GET oLogFont:Width

    [StructLayout(LayoutKind.Sequential, CharSet:=System.Runtime.InteropServices.CharSet.Auto)];
    INTERNAL CLASS LOGFONT
        PUBLIC Height := 0         AS INT
        PUBLIC Width := 0          AS INT
        PUBLIC Escapement := 0     AS INT
        PUBLIC Orientation := 0    AS INT
        PUBLIC Weight := 0         AS INT
        PUBLIC Italic := 0         AS BYTE
        PUBLIC Underline := 0      AS BYTE
        PUBLIC StrikeOut := 0      AS BYTE
        PUBLIC CharSet := 0        AS BYTE
        PUBLIC OutPrecision := 0   AS BYTE
        PUBLIC ClipPrecision := 0  AS BYTE
        PUBLIC Quality := 0        AS BYTE
        PUBLIC PitchAndFamily := 0 AS BYTE
            [MarshalAs(UnmanagedType.ByValTStr, SizeConst:=32)];
        EXPORT FaceName := ""      AS STRING

    END CLASS

    /// <summary>
    /// Implicit operator to convert System.Drawing.Font to VO Font
    /// </summary>
    /// <param name="f">Font</param>
    /// <returns>Font</returns>
    OPERATOR IMPLICIT ( f AS System.Drawing.Font) AS Font
        RETURN Font{f}

    /// <summary>
    /// Implicit operator to convert VO Font to System.Drawing.Font
    /// </summary>
    /// <param name="f">Font</param>
    /// <returns>Font</returns>
    OPERATOR IMPLICIT ( f AS Font ) AS System.Drawing.Font
        RETURN f:__Font

    STATIC METHOD ConvertFont(family AS WORD) AS WORD STRICT
        LOCAL retVal AS WORD

        DO CASE
        CASE family == FontDecorative
            retVal := FF_DECORATIVE
        CASE family == FontModern
            retVal := FF_MODERN
        CASE family == FontRoman
            retVal := FF_ROMAN
        CASE family == FontScript
            retVal := FF_SCRIPT
        CASE family == FontSwiss
            retVal := FF_SWISS
        OTHERWISE
            retVal := FF_DONTCARE
        ENDCASE

        RETURN retVal

    STATIC METHOD GetStdFontScreenHeight(kStandardFont AS INT) AS INT STRICT
        LOCAL hDC AS PTR
        LOCAL iHeight AS INT

        hDC		:= GuiWin32.CreateIC("DISPLAY", NULL_STRING, NULL_STRING, IntPtr.Zero)
        iHeight := -(((GuiWin32.GetDeviceCaps(hDC, LOGPIXELSY) * StdFontPointSize[kStandardFont + 1]) + 36) / 72)
        GuiWin32.DeleteDC(hDC)

        RETURN iHeight
    STATIC INITONLY PROTECT StdFontFamily 	:= {FF_MODERN, FF_MODERN, FF_MODERN, FF_ROMAN, FF_ROMAN, FF_ROMAN, FF_ROMAN, FF_ROMAN, FF_ROMAN, FF_SWISS, FF_SWISS, FF_SWISS, FF_SWISS, FF_SWISS, FF_SWISS, FF_DONTCARE} AS ARRAY
    STATIC INITONLY PROTECT StdFontPointSize := {8, 10, 12, 8, 10, 12, 14, 18, 24, 8, 10, 12, 14, 18, 24, 8} AS ARRAY


    INTERNAL ENUM FontWeight
        MEMBER Heavy := 700
        MEMBER Light := 200
        MEMBER Normal := 400

    END ENUM
END CLASS




