//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
/// <include file="Gui.xml" path="doc/FixedIcon/*" />

class FixedIcon inherit FixedImage
    protect _oIcon as Icon

 /// <exclude />
	OVERRIDE METHOD __SetImage(uResID AS USUAL) AS OBJECT STRICT
		RETURN SELF:SetIcon(Icon{uResID})

    /// <inheritdoc />
	METHOD AsString () as string strict
		RETURN "#FixedImage Object"

/// <include file="Gui.xml" path="doc/FixedIcon.ctor/*" />
	CONSTRUCTOR(uOwner, uID, uPoint, uResID)
		LOCAL oSI as System.Drawing.Size
		oSI:= System.Windows.Forms.SystemInformation.IconSize

		SUPER(uOwner, uID, uPoint, Dimension{oSI:Width,oSI:Height}, uResID)

		RETURN

/// <include file="Gui.xml" path="doc/FixedIcon.SetIcon/*" />
	METHOD SetIcon(oIcon as Icon) as Icon
		SELF:SetStyle(SS_ICON)
        _oIcon := oIcon
        self:oImage := oIcon
        // Todo Lable Image
		//self:__Label:Image := oIcon:__Icon
        SELF:__Label:FlatStyle := System.Windows.Forms.FlatStyle.Flat
		RETURN oIcon


END CLASS

