//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
/// <include file="Gui.xml" path="doc/FixedImage/*" />

PARTIAL ABSTRACT CLASS FixedImage INHERIT Control
    PROTECT oImage AS IResource

    /// <exclude />
    PROPERTY ControlType AS ControlType GET ControlType.FixedImage

    /// <include file="Gui.xml" path="doc/FixedImage.ctor/*" />
    CONSTRUCTOR(uOwner, uID, uPoint, uDimension, uResID)
        IF uID IS ResourceID
            SUPER(uOwner,uID,,,,,FALSE)
        ELSEIF IsLong(uID)
            SUPER(uOwner, uID,	uPoint, uDimension, "Static", , FALSE)

            IF uResID IS ResourceId VAR oResID
                SELF:__SetImage(oResID)
                cWindowName:="#"+LTrim(AsString(oResID:ID))
            ENDIF
        ELSE
            WCError{#Init,#FixedImage,__WCSTypeError}:Throw()
        ENDIF

        RETURN


    /// <exclude />
    PROPERTY __Label AS VOLabel GET (VOLabel) oCtrl

    /// <exclude />
    ABSTRACT METHOD __SetImage(uResId AS USUAL) AS OBJECT STRICT

    /// <exclude />
    METHOD Destroy() AS USUAL CLIPPER

        IF (oImage != NULL_OBJECT)
            oImage:Destroy()
            oImage := NULL_OBJECT
        ENDIF
        SUPER:Destroy()

        RETURN SELF


END CLASS

