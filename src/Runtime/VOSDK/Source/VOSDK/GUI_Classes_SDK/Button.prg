/// <include file="Gui.xml" path="doc/Button/*" />
CLASS Button INHERIT TextControl
	PROTECT oImage AS VObject


 /// <exclude />
METHOD __GetImage()
	//PP-030915
	IF oImage is ButtonImageList var oBMI
		RETURN oBMI:Image
	ENDIF
	RETURN oImage


 /// <exclude />
METHOD __SetImage(oNewImage)
	//PP-030915


	IF IsThemeEnabled()
		IF (oNewImage IS Icon) .OR. (oNewImage IS Bitmap)
			SELF:ImageList := ButtonImageList{oNewImage}
			RETURN TRUE
		ENDIF
	ENDIF


	RETURN FALSE


 /// <exclude />
METHOD __Update() AS Control STRICT
	//PP-030828 Strong typing
	LOCAL cText AS STRING
	LOCAL cNewText AS STRING
	LOCAL uOldValue


	IF SELF:Modified
		cText := SELF:TextValue
		uOldValue := AsString(uValue)
		IF oFieldSpec IS FieldSpec var oFs
			uValue := oFs:Val(cText)
			// If theres a picture clause we need to reformat the data at this point
			//RvdH 060608 optimized
			//IF ((!IsNil(oFieldSpec:Picture)) .AND. !Empty(oFieldSpec:Picture))
			IF SLen(oFs:Picture) > 0
				cNewText := oFs:Transform(uValue)
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
	RETURN SELF


/// <include file="Gui.xml" path="doc/Button.AsString/*" />
METHOD AsString ()




	RETURN "#"+Symbol2String(ClassName(SELF))+":"+SELF:Caption


/// <include file="Gui.xml" path="doc/Button.CurrentText/*" />
ACCESS CurrentText




	RETURN NULL_STRING


/// <include file="Gui.xml" path="doc/Button.CurrentText/*" />
ASSIGN CurrentText(cValue)




	RETURN
/// <inheritdoc />
METHOD Destroy()
    if self:oImage != null
        self:oImage:Destroy()
    endif
    SUPER:Destroy()
    return NULL

/// <include file="Gui.xml" path="doc/Button.Image/*" />
ACCESS Image
	//PP-030915
	RETURN SELF:__GetImage()


/// <include file="Gui.xml" path="doc/Button.Image/*" />
ASSIGN Image(oNewImage)
	LOCAL dwType 		AS DWORD
	LOCAL lStyle 		AS LONGINT
	LOCAL lSetStyle 	AS LONGINT
	LOCAL hImage 	 	AS PTR

	//PP-030915
	IF ! SELF:__SetImage(oNewImage)
		IF oNewImage IS Icon .OR. oNewImage IS Bitmap
			oImage := oNewImage
			lStyle := GetWindowLong(SELF:Handle(), GWL_STYLE)
			IF oNewImage IS Icon var oIcon
				dwType 		:= IMAGE_ICON
				lSetStyle 	:= BS_ICON
				oIcon			:= oNewImage
				hImage 		:= oIcon:Handle()
			ELSEIF oNewImage IS Bitmap var oBitMap
				dwType 		:= IMAGE_BITMAP
				lSetStyle 	:= BS_BITMAP
				hImage 		:= oBitMap:Handle()
			ENDIF


			lStyle := _AND(lStyle, _NOT(_OR(BS_ICON,BS_BITMAP)))
			lStyle := _OR(lStyle, lSetStyle)
			SetWindowLong(SELF:Handle(), GWL_STYLE, lStyle)


			SendMessage(SELF:Handle(), BM_SETIMAGE, dwType, LONGINT(_CAST, hImage))
		ENDIF
	ENDIF


	RETURN


/// <include file="Gui.xml" path="doc/Button.ImageList/*" />
ACCESS ImageList
	//PP-030915
	IF oImage is ImageList
		RETURN oImage
	ENDIF
	RETURN NULL_OBJECT


/// <include file="Gui.xml" path="doc/Button.ImageList/*" />
ASSIGN ImageList(oImageList)
	//PP-030915
	LOCAL sBImageList IS _winButton_ImageList
	LOCAL oImgList    AS ImageList


	IF oImageList IS ImageList
		oImgList := oImageList
		IF IsThemeEnabled()
			sBImageList:himl   := oImgList:Handle()
			sBImageList:uAlign := BUTTON_IMAGELIST_ALIGN_CENTER


			SendMessage(hWnd, BCM_SETIMAGELIST, 0, LONGINT(_CAST,@sBImageList))
		ENDIF
	ENDIF


	RETURN (oImage := oImgList)


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


/// <include file="Gui.xml" path="doc/Button.SetStyle/*" />
METHOD SetStyle(kStyle, lEnable)
	// local wTemp as word
	// local wSetStyle as word






	RETURN SUPER:setStyle(kStyle, lEnable)


END CLASS


/// <include file="Gui.xml" path="doc/ButtonImageList/*" />
CLASS ButtonImageList INHERIT ImageList
	//PP-030915 from S Ebert
	HIDDEN _oImage AS VObject


/// <include file="Gui.xml" path="doc/ButtonImageList.Image/*" />
ACCESS Image
	//PP-030915
	RETURN _oImage


/// <include file="Gui.xml" path="doc/ButtonImageList.ctor/*" />
CONSTRUCTOR(oImage)
	//PP-030915
	_oImage := oImage
	//PP-031129
	SUPER(1, oImage:Size, oImage, _OR(ILC_COLOR32, ILC_MASK))
	RETURN




END CLASS


