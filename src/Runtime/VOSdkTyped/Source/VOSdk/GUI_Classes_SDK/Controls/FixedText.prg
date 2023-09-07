//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
/// <include file="Gui.xml" path="doc/FixedText/*" />
[XSharp.Internal.TypesChanged];
CLASS FixedText INHERIT TextControl
    PROTECT _dwDrawStyle AS DWORD
    PROTECT _dwMargin	 AS LONG
    PROPERTY lUseDrawText AS LOGIC AUTO
    PROPERTY lDrawThemeBackground AS LOGIC AUTO

    PROPERTY ControlType AS Controltype GET ControlType.FixedText


    /// <include file="Gui.xml" path="doc/FixedText.ctor/*" />
    CONSTRUCTOR(oOwner, xID, oPoint, oDimension, cText, lDataAware)
        LOCAL cClass AS USUAL
        LOCAL lResID AS LOGIC


        // Text will be displayed using API DrawText(). Default style set below.
        // Standard styles will cause the appropriate draw style to be used.
        // DT_END_ELLIPSIS will put ... if text does not fit in space provided
        SELF:lUseDrawText := TRUE
        SELF:lDrawThemeBackground := TRUE
        //PP-040317. Issue 12806. Added DT_EXPANDTABS for default behaviour more like old fixed text
        SELF:_dwDrawStyle := _OR(DT_WORDBREAK,DT_EXPANDTABS)

        DEFAULT( REF lDataAware, TRUE)
        lResID:=(xID IS ResourceID)
        IF !lResID
            cClass:="Static"
        ENDIF

        SUPER(oOwner, xID, oPoint, oDimension, cClass, SS_Left, lDataAware)

        IF !lResID
            IF !IsNil(cText)
                cWindowName := cText
                SELF:Caption := cText
            ENDIF
        ENDIF

        RETURN

    /// <exclude />

    PROPERTY __Label AS VOLabel GET (VOLabel) oCtrl

        [Obsolete];
    METHOD __SetColors(_hDC AS IntPtr) AS IntPtr STRICT
        RETURN IntPtr.Zero

    /// <exclude />
    METHOD __SetText(cNewText AS STRING) AS STRING STRICT
        //PP-030915
        IF SELF:ValidateControl()
            //PP-040107
            SUPER:__SetText(cNewText)
            cCaption := cNewText
            oCtrl:Invalidate()
        ENDIF
        RETURN cNewText

    /// <exclude />
    METHOD OnPaint (e AS System.Windows.Forms.PaintEventArgs) AS LOGIC
        LOCAL liSSStyle AS LONG
        LOCAL dwDrawStyle AS System.Windows.Forms.TextFormatFlags
        LOCAL oRect AS System.Drawing.Rectangle
        IF ! SELF:lUseDrawText .or. oCtrl == NULL_OBJECT
            RETURN FALSE
        ENDIF
        oRect := oCtrl:ClientRectangle
        IF _dwMargin != 0
            LOCAL oBB AS BoundingBox
            oBB := oRect
            oBB:Left += _dwMargin
            oBB:Top  += _dwMargin
            oRect := oBB
        ENDIF
        liSSStyle := SELF:dwStyle

        dwDrawStyle := (System.Windows.Forms.TextFormatFlags) _dwDrawStyle
        IF LOGIC(_CAST, _AND(liSSStyle, SS_CENTERIMAGE))
            //PP-040812 DT_VCENTER needs DT_SINGLELINE to work - see MS docs
            dwDrawStyle |= System.Windows.Forms.TextFormatFlags.SingleLine | System.Windows.Forms.TextFormatFlags.VerticalCenter
        ENDIF
        IF _AND(liSSStyle, SS_CENTER) != 0
            dwDrawStyle |= System.Windows.Forms.TextFormatFlags.HorizontalCenter
        ENDIF
        IF _AND(liSSStyle, SS_RIGHT) != 0
            dwDrawStyle |= System.Windows.Forms.TextFormatFlags.Right
        ENDIF
        IF _AND(liSSStyle, SS_NOPREFIX) != 0
            dwDrawStyle |= System.Windows.Forms.TextFormatFlags.NoPrefix
        ENDIF
        // RvdH 050602 Prevent wrapping (issue [12944] )
        IF _AND(liSSStyle, SS_LEFTNOWORDWRAP) != 0
            dwDrawStyle |= System.Windows.Forms.TextFormatFlags.SingleLine
        ENDIF

        dwDrawStyle |= System.Windows.Forms.TextFormatFlags.NoPadding

        dwDrawStyle |= System.Windows.Forms.TextFormatFlags.PreserveGraphicsTranslateTransform | ;
            System.Windows.Forms.TextFormatFlags.PreserveGraphicsClipping

        LOCAL oCol AS System.Drawing.Color
        IF oCtrl:Enabled
            oCol := oCtrl:ForeColor
        ELSE
            oCol := System.Drawing.SystemColors.ControlDark
        ENDIF
        System.Windows.Forms.TextRenderer.DrawText(e:Graphics, cCaption, oCtrl:Font, oRect,oCol,dwDrawStyle)
        RETURN TRUE

    /// <include file="Gui.xml" path="doc/FixedText.Margin/*" />
    ASSIGN Margin (nNewValue AS LONG)
        _dwMargin := nNewValue
        SELF:oCtrl:Margin := System.Windows.Forms.Padding {nNewValue}


    /// <include file="Gui.xml" path="doc/FixedText.SetDrawStyle/*" />
    METHOD SetDrawStyle(dwDrawStyle AS DWORD, lEnable := NIL AS USUAL) AS DWORD

        IF IsLogic(lEnable)
            IF lEnable
                _dwDrawStyle :=  _OR(_dwDrawStyle, (DWORD) dwDrawStyle)
            ELSE
                _dwDrawStyle := _AND(_dwDrawStyle, _NOT(DWORD(_CAST, dwDrawStyle)))
            ENDIF
        ELSE
            _dwDrawStyle := dwDrawStyle
        ENDIF

        RETURN _dwDrawStyle

    /// <include file="Gui.xml" path="doc/FixedText.SetStandardStyle/*" />
    METHOD SetStandardStyle(kTextStyle AS LONG) AS LONG
        LOCAL liTempStyle, liStyle AS LONG
        LOCAL hHandle AS PTR

        SWITCH kTextStyle
        CASE FT_LEFTALIGN
            liTempStyle := SS_LEFT
        CASE FT_RIGHTALIGN
            liTempStyle := SS_RIGHT
        CASE FT_CENTERED
            liTempStyle := SS_CENTER
        END SWITCH

        hHandle := SELF:Handle()
        IF oCtrl != NULL_OBJECT .and. ! oCtrl:IsDisposed
            liStyle := GuiWin32.GetWindowStyle(hHandle)
            liStyle := _AND(dwStyle, _NOT(0X00000003U))
            liStyle := _OR(dwStyle, liTempStyle)
            GuiWin32.SetWindowStyle(hHandle, liStyle)
        ENDIF

        RETURN dwStyle

END CLASS

