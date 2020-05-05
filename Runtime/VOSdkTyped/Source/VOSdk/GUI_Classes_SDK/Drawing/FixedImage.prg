

PARTIAL ABSTRACT CLASS FixedImage INHERIT Control
	PROTECT oImage AS OBJECT

    PROPERTY ControlType AS ControlType GET ControlType.FixedImage

	CONSTRUCTOR(uOwner, uID, uPoint, uDimension, uResID) 
		IF  IsInstanceOfUsual(uID,#ResourceID)
			SUPER(uOwner,uID,,,,,FALSE)
		ELSEIF IsLong(uID)
			SUPER(uOwner, uID,	uPoint, uDimension, "Static", , FALSE)

			IF !IsNil(uResID)
				SELF:__SetImage(uResID)
				cWindowName:="#"+LTrim(AsString(uResID:ID))
			ENDIF
		ELSE
			WCError{#Init,#FixedImage,__WCSTypeError}:@@Throw()
		ENDIF

		RETURN 


	ACCESS __Label AS VOImageLabel
		RETURN (VOImageLabel) oCtrl

	ABSTRACT METHOD __SetImage(uResId AS USUAL) AS OBJECT STRICT 

	METHOD Destroy() AS USUAL CLIPPER

		IF (oImage != NULL_OBJECT)
			oImage:Destroy()
			oImage := NULL_OBJECT
		ENDIF
		SUPER:Destroy()

		RETURN SELF


END CLASS

