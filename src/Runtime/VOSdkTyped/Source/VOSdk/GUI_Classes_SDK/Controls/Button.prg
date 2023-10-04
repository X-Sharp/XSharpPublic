//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.Windows.Forms
USING System.Drawing
/// <include file="Gui.xml" path="doc/Button/*" />
[XSharp.Internal.TypesChanged];
CLASS Button INHERIT TextControl
    PROTECT oImage AS VObject

    /// <exclude />
    PROPERTY ControlType AS Controltype GET ControlType.Button

    /// <include file="Gui.xml" path="doc/Button.ctor/*" />
    CONSTRUCTOR ( oOwner, xID, oPoint, oDimension, cText, kStyle, lDataAware)
        IF xID IS ResourceID
            SUPER(oOwner, xID, oPoint, oDimension, , kStyle,lDataAware)
        ELSE
            SUPER(oOwner, xID, oPoint, oDimension, "Button", kStyle, lDataAware)
            IF !IsNil(cText)
                SELF:Caption := cText
            ENDIF
        ENDIF

        RETURN



    /// <exclude />
    property __Button as ButtonBase get (ButtonBase) oCtrl

    /// <exclude />
    METHOD __GetImage()  AS Object
        if oImage is ButtonImageList var oBIL
            return oBIL:Image
        ENDIF
        RETURN oImage

    /// <exclude />
    METHOD __SetImage(oNewImage AS VObject)  AS LOGIC
        self:oImage := oNewImage
        if oNewImage is ButtonImageList var oBIL
            oImage := oNewImage
            self:__Button:ImageList := oBIL
            self:__Button:FlatStyle := FlatStyle.Standard
            self:__Button:Text := ""
            RETURN TRUE
        ELSEIF oNewImage IS Bitmap VAR oBM
            self:__Button:Image := oBM
            self:__Button:FlatStyle := FlatStyle.Standard
            self:__Button:Text := ""

            RETURN TRUE
        ENDIF

        RETURN FALSE


    /// <exclude />
    METHOD __Update() AS VOID STRICT
        //PP-030828 Strong typing
        LOCAL cText AS STRING
        LOCAL cNewText AS STRING
        LOCAL uOldValue

        IF SELF:Modified
            cText := SELF:TextValue
            uOldValue := AsString(uValue)
            IF oFieldSpec != NULL
                uValue := oFieldSpec:Val(cText)
                // If theres a picture clause we need to reformat the data at this point
                //RvdH 060608 optimized
                //IF ((!IsNil(oFieldSpec:Picture)) .AND. !Empty(oFieldSpec:Picture))
                IF SLen(oFieldSpec:Picture) > 0
                    cNewText := oFieldSpec:Transform(uValue)
                    IF !(cNewText == cText)
                        SELF:TextValue := cNewText
                    ENDIF
                ENDIF
            ELSE
                uValue := Unformat(cText, "", "L")
            ENDIF
            SELF:Modified := .F.
            SELF:ValueChanged := !(uOldValue == AsString(uValue))
        ENDIF
        RETURN

    /// <include file="Gui.xml" path="doc/Button.AsString/*" />
    METHOD AsString () as string strict
        RETURN "#"+Symbol2String(ClassName(SELF))+":"+SELF:Caption

    /// <include file="Gui.xml" path="doc/Button.Caption/*" />
    ASSIGN Caption(cNewText AS STRING)
        IF SELF:oImage == NULL
            SUPER:Caption := cNewText
        ENDIF

    /// <include file="Gui.xml" path="doc/Button.CurrentText/*" />
    PROPERTY CurrentText AS STRING GET NULL_STRING SET

    /// <include file="Gui.xml" path="doc/Button.Image/*" />
    PROPERTY Image AS VObject GET SELF:__GetImage() SET SELF:__SetImage(value)

    /// <include file="Gui.xml" path="doc/Button.ImageList/*" />
    PROPERTY ImageList AS ImageList GET oImage ASTYPE ImageList SET SELF:__SetImage(value)

END CLASS

/// <include file="Gui.xml" path="doc/ButtonImageList/*" />
CLASS ButtonImageList INHERIT ImageList
    protected _oImage as IResource

    /// <include file="Gui.xml" path="doc/ButtonImageList.Image/*" />
    property Image as IResource get _oImage

    /// <include file="Gui.xml" path="doc/ButtonImageList.ctor/*" />
    constructor(oImage as IResource)
        _oImage := oImage
        SUPER(1, oImage:Size, oImage, _OR(ILC_COLOR32, ILC_MASK))
        RETURN


END CLASS

