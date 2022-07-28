

USING System.Windows.Forms
USING System.Drawing
/// <include file="Gui.xml" path="doc/Button/*" />
[XSharp.Internal.TypesChanged];
CLASS Button INHERIT TextControl
	PROTECT oImage AS VObject

    PROPERTY ControlType AS Controltype GET ControlType.Button

/// <include file="Gui.xml" path="doc/Button.ctor/*" />
	CONSTRUCTOR ( oOwner, xID, oPoint, oDimension, cText, kStyle, lDataAware)
		IF IsInstanceOfUsual(xID,#ResourceID)
			SUPER(oOwner, xID, oPoint, oDimension, , kStyle,lDataAware)
		ELSE
			SUPER(oOwner, xID, oPoint, oDimension, "Button", kStyle, lDataAware)
			IF !IsNil(cText)
				SELF:Caption := cText
			ENDIF
		ENDIF

		RETURN

    ASSIGN Caption(cNewText AS STRING)
        IF SELF:oImage == NULL
            SUPER:Caption := cNewText
        ENDIF


	ACCESS __Button AS IVOButton
		RETURN (IVOButton) oCtrl

 /// <exclude />
	METHOD __GetImage()  AS VObject
		IF oImage is ButtonImageList
			RETURN IVarGet(oImage,#Image)
		ENDIF
		RETURN oImage

 /// <exclude />
	METHOD __SetImage(oNewImage AS VObject)  AS LOGIC
		IF oNewImage IS ButtonImageList VAR oBIL
			oImage := oNewImage
			SELF:__Button:Image := oBIL:Image:__Image
		    SELF:__Button:FlatStyle := FlatStyle.Standard
            SELF:__Button:Text := ""
			RETURN TRUE
		ELSEIF oNewImage IS Bitmap VAR oBM
			oImage := oNewImage
			SELF:__Button:Image := oBM:__Image
		    SELF:__Button:FlatStyle := FlatStyle.Standard
            SELF:__Button:Text := ""

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
			IF IsInstanceOfUsual(oFieldSpec, #FieldSpec)
				uValue := ((FieldSpec) oFieldSpec):Val(cText)
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
	METHOD AsString ()
		RETURN "#"+Symbol2String(ClassName(SELF))+":"+SELF:Caption

/// <include file="Gui.xml" path="doc/Button.CurrentText/*" />
	ACCESS CurrentText AS STRING
		RETURN NULL_STRING

/// <include file="Gui.xml" path="doc/Button.CurrentText/*" />
	ASSIGN CurrentText(cValue AS STRING)
		RETURN

/// <include file="Gui.xml" path="doc/Button.Image/*" />
	ACCESS Image AS VObject
		RETURN SELF:__GetImage()

/// <include file="Gui.xml" path="doc/Button.Image/*" />
	ASSIGN Image(oNewImage AS VObject)
		SELF:__SetImage(oNewImage)

		RETURN

/// <include file="Gui.xml" path="doc/Button.ImageList/*" />
	ACCESS ImageList
		IF oImage IS ImageList
			RETURN oImage
		ENDIF
		RETURN NULL_OBJECT

/// <include file="Gui.xml" path="doc/Button.ImageList/*" />
	ASSIGN ImageList(oImageList)
		SELF:__SetImage(oImageList)


/// <include file="Gui.xml" path="doc/Button.SetStyle/*" />
	METHOD SetStyle(kStyle AS LONG, lEnable := TRUE AS LOGIC)
		RETURN SUPER:SetStyle(kStyle, lEnable)

END CLASS

/// <include file="Gui.xml" path="doc/ButtonImageList/*" />
CLASS ButtonImageList INHERIT ImageList
	PROTECTED _oImage AS Object

/// <include file="Gui.xml" path="doc/ButtonImageList.Image/*" />
	ACCESS Image AS OBJECT
		RETURN _oImage

/// <include file="Gui.xml" path="doc/ButtonImageList.ctor/*" />
	CONSTRUCTOR(oImage AS OBJECT)
		_oImage := oImage
		SUPER(1, oImage:Size, oImage, _OR(ILC_COLOR32, ILC_MASK))
		RETURN


END CLASS

