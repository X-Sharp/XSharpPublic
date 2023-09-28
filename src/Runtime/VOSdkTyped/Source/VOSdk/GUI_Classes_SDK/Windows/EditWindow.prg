//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

/// <include file="Gui.xml" path="doc/EditWindow/*" />
CLASS EditWindow INHERIT ControlWindow
    PROTECT oMle as MultiLineEdit

    METHOD AsString()
        RETURN SELF:TextValue

    /// <include file="Gui.xml" path="doc/EditWindow.Clear/*" />
    METHOD Clear()   AS VOID STRICT => oMle:Clear()

    /// <include file="Gui.xml" path="doc/EditWindow.Copy/*" />
    METHOD Copy()   AS VOID STRICT => oMle:Copy()

    /// <include file="Gui.xml" path="doc/EditWindow.Cut/*" />
    METHOD Cut()   AS VOID STRICT => oMle:Cut()

    /// <include file="Gui.xml" path="doc/EditWindow.Font/*" />
    PROPERTY Font[oNewFont AS Font] SET oFont := oNewFont , SELF:__SetFont()

    /// <include file="Gui.xml" path="doc/EditWindow.GetLine/*" />
    METHOD GetLine(nLineNumber:= 0 as LONG, nMaxLength := 0 as LONG) AS STRING
        RETURN oMle:GetLine(nLineNumber, nMaxLength)

    /// <include file="Gui.xml" path="doc/EditWindow.ctor/*" />
#ifndef DOCUMENTATION
    constructor(oOwner, xID, oPoint, oDimension)
        oMle := MultiLineEdit{oOwner,xID,oPoint,oDimension}
        super(oMle)
        return
#endif
    /// <include file="Gui.xml" path="doc/EditWindow.Length/*" />
    PROPERTY Length AS LONG => oMle:Length

    /// <include file="Gui.xml" path="doc/EditWindow.LineCount/*" />
    PROPERTY LineCount  AS LONG => oMle:LineCount

    /// <include file="Gui.xml" path="doc/EditWindow.LineDown/*" />
    METHOD LineDown() AS VOID STRICT => oMle:LineDown()

    /// <include file="Gui.xml" path="doc/EditWindow.LineUp/*" />
    METHOD LineUp() AS VOID STRICT => oMle:LineUp()

    /// <include file="Gui.xml" path="doc/EditWindow.PageDown/*" />
    METHOD PageDown() AS VOID STRICT => oMle:PageDown()

    /// <include file="Gui.xml" path="doc/EditWindow.PageUp/*" />
    METHOD PageUp() AS VOID STRICT => oMle:PageUp()

    /// <include file="Gui.xml" path="doc/EditWindow.Paste/*" />
    METHOD Paste(cNewString as string) AS VOID STRICT => oMle:Paste(cNewString)

    /// <include file="Gui.xml" path="doc/EditWindow.ScrollHorizontal/*" />
    METHOD ScrollHorizontal(nChars AS LONG) AS VOID STRICT  => oMle:ScrollHorizontal(nChars)

    /// <include file="Gui.xml" path="doc/EditWindow.ScrollVertical/*" />
    METHOD ScrollVertical(nLines AS LONG) AS VOID STRICT  => oMle:ScrollVertical(nLines)

    /// <include file="Gui.xml" path="doc/EditWindow.Selection/*" />
    PROPERTY Selection AS Selection GET oMle:Selection SET oMle:Selection:=value

    /// <include file="Gui.xml" path="doc/EditWindow.TextLimit/*" />
    PROPERTY TextLimit[nChars AS LONG] SET oMle:TextLimit:=nChars

    /// <include file="Gui.xml" path="doc/EditWindow.TextValue/*" />
    PROPERTY TextValue AS STRING GET oMle:TextValue SET oMle:TextValue:=value

    /// <include file="Gui.xml" path="doc/EditWindow.Undo/*" />
    METHOD Undo() AS VOID STRICT  => oMle:Undo()

    /// <include file="Gui.xml" path="doc/EditWindow.Value/*" />
    PROPERTY Value AS USUAL GET SELF:TextValue SET SELF:TextValue := AsString(value)

END CLASS

