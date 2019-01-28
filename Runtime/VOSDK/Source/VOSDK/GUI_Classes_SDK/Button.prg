CLASS Button INHERIT TextControl
	PROTECT oImage AS VObject

METHOD __GetImage() 
	//PP-030915
	IF IsInstanceOf(oImage, #ButtonImageList)
		RETURN IVarGet(oImage,#Image)
	ENDIF
	RETURN oImage

METHOD __SetImage(oNewImage) 
	//PP-030915

	IF IsThemeEnabled()
		IF IsInstanceOfUsual(oNewImage, #Icon) .OR. IsInstanceOfUsual(oNewImage, #Bitmap)
			SELF:ImageList := ButtonImageList{oNewImage}
			RETURN TRUE
		ENDIF
	ENDIF

	RETURN FALSE

METHOD __Update() AS Control STRICT 
	//PP-030828 Strong typing
	LOCAL cText AS STRING
	LOCAL cNewText AS STRING
	LOCAL uOldValue

	IF SELF:Modified
		cText := SELF:TextValue
		uOldValue := AsString(uValue)
		IF IsInstanceOfUsual(oFieldSpec, #FieldSpec)
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
	RETURN SELF

METHOD AsString () 
	

	RETURN "#"+Symbol2String(ClassName(SELF))+":"+SELF:Caption

ACCESS CurrentText 
	

	RETURN NULL_STRING

ASSIGN CurrentText(cValue) 
	

	RETURN 

ACCESS Image 
	//PP-030915
	RETURN SELF:__GetImage()

ASSIGN Image(oNewImage) 
	LOCAL dwType 		AS DWORD
	LOCAL lStyle 		AS LONGINT
	LOCAL lSetStyle 	AS LONGINT
	LOCAL hImage 	 	AS PTR
	LOCAL oIcon 	  	AS Icon
	LOCAL oBitMap  	AS Bitmap 

	

	//PP-030915
	IF ! SELF:__SetImage(oNewImage)
		IF IsInstanceOfUsual(oNewImage, #Icon) .OR. IsInstanceOfUsual(oNewImage, #Bitmap)
			oImage := oNewImage

			lStyle := GetWindowLong(SELF:Handle(), GWL_STYLE)
			IF IsInstanceOf(oImage, #Icon)
				dwType 		:= IMAGE_ICON
				lSetStyle 	:= BS_ICON
				oIcon			:= oNewImage
				hImage 		:= oIcon:Handle()
			ELSE
				dwType 		:= IMAGE_BITMAP
				lSetStyle 	:= BS_BITMAP
				oBitMap   	:= oNewImage  
				hImage 		:= oBitMap:Handle()
			ENDIF

			lStyle := _AND(lStyle, _NOT(_OR(BS_ICON,BS_BITMAP)))
			lStyle := _OR(lStyle, lSetStyle)
			SetWindowLong(SELF:Handle(), GWL_STYLE, lStyle)
			
			SendMessage(SELF:Handle(), BM_SETIMAGE, dwType, LONGINT(_CAST, hImage))
		ENDIF
	ENDIF

	RETURN 

ACCESS ImageList 
	//PP-030915
	IF IsInstanceOf(oImage, #ImageList)
		RETURN oImage
	ENDIF
	RETURN NULL_OBJECT

ASSIGN ImageList(oImageList) 
	//PP-030915
	LOCAL sBImageList IS _winButton_ImageList
	LOCAL oImgList    AS ImageList

	IF IsInstanceOfUsual(oImageList, #ImageList)
		oImgList := oImageList
		IF IsThemeEnabled()
			sBImageList:himl   := oImgList:Handle()
			sBImageList:uAlign := BUTTON_IMAGELIST_ALIGN_CENTER

			SendMessage(hWnd, BCM_SETIMAGELIST, 0, LONGINT(_CAST,@sBImageList))
		ENDIF
	ENDIF

	RETURN (oImage := oImgList)

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

METHOD SetStyle(kStyle, lEnable) 
	// local wTemp as word
	// local wSetStyle as word

	

	RETURN SUPER:setStyle(kStyle, lEnable)

END CLASS

CLASS ButtonImageList INHERIT ImageList
	//PP-030915 from S Ebert
	HIDDEN _oImage AS VObject

ACCESS Image 
	//PP-030915
	RETURN _oImage

CONSTRUCTOR(oImage) 
	//PP-030915
	_oImage := oImage
	//PP-031129
	SUPER(1, oImage:Size, oImage, _OR(ILC_COLOR32, ILC_MASK))
	RETURN 


END CLASS

