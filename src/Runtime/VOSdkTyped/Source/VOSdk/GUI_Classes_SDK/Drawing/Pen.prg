//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

/// <include file="Gui.xml" path="doc/Pen/*" />

CLASS Pen INHERIT VObject
    HIDDEN hPen AS System.Drawing.Pen

    /// <inheritdoc />

    METHOD Destroy() AS USUAL CLIPPER
        IF (hPen != NULL_OBJECT)
            hPen:Dispose()
            hPen := NULL_OBJECT
        ENDIF

        RETURN SELF

    /// <include file="Gui.xml" path="doc/Pen.Handle/*" />
    METHOD Handle  AS System.Drawing.Pen STRICT
        RETURN hPen

    /// <include file="Gui.xml" path="doc/Pen.ctor/*" />
    CONSTRUCTOR(uColor, uLineStyle, uWidth)
        LOCAL liStyle AS LONGINT
        LOCAL oColor AS Color
        LOCAL liWidth AS LONGINT

        SUPER()
        IF !IsNil(uColor)
            IF !(uColor IS Color)
                WCError{#Init,#Pen,__WCSTypeError,uColor,1}:Throw()
            ENDIF
            oColor := uColor
        ELSE
            oColor:=Color{0}
        ENDIF

        IF !IsNil(uLineStyle)
            IF !IsLong(uLineStyle)
                WCError{#Init,#Pen,__WCSTypeError,uLineStyle,2}:Throw()
            ENDIF
            liStyle:=uLineStyle
        ENDIF

        IF !IsNil(uWidth)
            IF !IsLong(uWidth)
                WCError{#Init,#Pen,__WCSTypeError,uWidth,3}:Throw()
            ENDIF
            liWidth := uWidth
        ELSE
            liWidth:=1
        ENDIF
        hPen:=System.Drawing.Pen{oColor, liWidth}
        IF liStyle != 0
            hPen:DashStyle := (System.Drawing.Drawing2D.DashStyle) liStyle
        ENDIF
        RETURN
END CLASS

