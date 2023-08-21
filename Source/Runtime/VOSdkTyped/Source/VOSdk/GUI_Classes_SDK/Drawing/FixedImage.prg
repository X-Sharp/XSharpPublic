/// <include file="Gui.xml" path="doc/FixedImage/*" />

PARTIAL ABSTRACT CLASS FixedImage INHERIT Control
	PROTECT oImage AS OBJECT

    /// <exclude />
    PROPERTY ControlType AS ControlType GET ControlType.FixedImage

/// <include file="Gui.xml" path="doc/FixedImage.ctor/*" />
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
			WCError{#Init,#FixedImage,__WCSTypeError}:Throw()
		ENDIF

		RETURN


    /// <exclude />
	ACCESS __Label AS VOLabel
		RETURN (VOLabel) oCtrl

    /// <exclude />
	ABSTRACT METHOD __SetImage(uResId AS USUAL) AS OBJECT STRICT

    /// <exclude />
	METHOD Destroy() AS USUAL

		IF (oImage != NULL_OBJECT)
			oImage:Destroy()
			oImage := NULL_OBJECT
		ENDIF
		SUPER:Destroy()

		RETURN SELF


END CLASS

