//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
/// <include file="Gui.xml" path="doc/FixedImage/*" />

PARTIAL ABSTRACT CLASS FixedImage INHERIT Control
	protect oImage as IResource

    /// <exclude />
    PROPERTY ControlType AS ControlType GET ControlType.FixedImage

/// <include file="Gui.xml" path="doc/FixedImage.ctor/*" />
	CONSTRUCTOR(uOwner, uID, uPoint, uDimension, uResID)
		if uID is ResourceID
			SUPER(uOwner,uID,,,,,FALSE)
		ELSEIF IsLong(uID)
			SUPER(uOwner, uID,	uPoint, uDimension, "Static", , FALSE)

			if uResID is ResourceId var oResID
				self:__SetImage(oResID)
				cWindowName:="#"+LTrim(AsString(oResID:ID))
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

