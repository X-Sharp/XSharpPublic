

#USING System.Windows.Forms
using System.Drawing
CLASS Button INHERIT TextControl
	PROTECT oImage AS VObject

    PROPERTY ControlType AS Controltype GET ControlType.Button

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


	ACCESS __Button AS VOButton
		RETURN (VOButton) oCtrl
		

	METHOD __GetImage()  AS VObject
		IF oImage is ButtonImageList
			RETURN IVarGet(oImage,#Image)
		ENDIF
		RETURN oImage

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


    METHOD Dispatch (oEvent)
        
        RETURN SUPER:Dispatch(oEvent)


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

	METHOD AsString () 
		RETURN "#"+Symbol2String(ClassName(SELF))+":"+SELF:Caption

	ACCESS CurrentText AS STRING
		RETURN NULL_STRING

	ASSIGN CurrentText(cValue AS STRING) 
		RETURN 

	ACCESS Image AS VObject
		RETURN SELF:__GetImage()

	ASSIGN Image(oNewImage AS VObject) 
		SELF:__SetImage(oNewImage)				

		RETURN 

	ACCESS ImageList 
		IF oImage IS ImageList
			RETURN oImage
		ENDIF
		RETURN NULL_OBJECT

	ASSIGN ImageList(oImageList) 
		SELF:__SetImage(oImageList)


	METHOD SetStyle(kStyle AS LONG, lEnable := TRUE AS LOGIC) 
		RETURN SUPER:SetStyle(kStyle, lEnable)

END CLASS

CLASS ButtonImageList INHERIT ImageList
	PROTECTED _oImage AS Object

	ACCESS Image AS OBJECT
		RETURN _oImage

	CONSTRUCTOR(oImage AS OBJECT) 
		_oImage := oImage
		SUPER(1, oImage:Size, oImage, _OR(ILC_COLOR32, ILC_MASK))
		RETURN 


END CLASS

