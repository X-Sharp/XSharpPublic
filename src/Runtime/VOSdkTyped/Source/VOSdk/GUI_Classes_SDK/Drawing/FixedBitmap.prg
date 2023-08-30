//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
/// <include file="Gui.xml" path="doc/FixedBitmap/*" />
CLASS FixedBitmap INHERIT FixedImage
	PROTECT hInst AS IntPtr

 /// <exclude />
	OVERRIDE METHOD __SetImage(uResID AS USUAL) AS OBJECT STRICT
		RETURN SELF:SetBitmap(Bitmap{uResID})

/// <include file="Gui.xml" path="doc/FixedBitmap.AsString/*" />
	METHOD AsString() as string strict
		RETURN "#FixedBitmap Object"

/// <include file="Gui.xml" path="doc/FixedBitmap.ctor/*" />
	CONSTRUCTOR(uOwner, uID, uPoint, uDimension, uResID)
		SUPER(uOwner, uID, uPoint, uDimension, uResID)
		if uID  is ResourceID var oResID
			hInst := oResID:Handle()
		ENDIF
		if uResID is ResourceID var oResID2
			self:__SetImage(oResID2)
		ENDIF
		RETURN

    /// <inheritdoc />
	ASSIGN HyperLabel(oHL AS HyperLabel)
		SUPER:HyperLabel := oHL
		IF SELF:__Label == NULL_OBJECT .or. SELF:__Label:Image == NULL_OBJECT
			SELF:__SetImage(ResourceID{oHL:Caption, hInst})
		ENDIF
		RETURN

/// <include file="Gui.xml" path="doc/FixedBitmap.SetBitmap/*" />
	METHOD SetBitmap(oBitmap AS Bitmap)
		LOCAL oImg AS System.Drawing.Image
		oImg := oBitmap
		SELF:SetStyle(SS_BITMAP)
		IF SELF:oCtrl == NULL_OBJECT
			SELF:Create()
		ENDIF
		SELF:__Label:Image := oImg
		SELF:__Label:FlatStyle := System.Windows.Forms.FlatStyle.Flat

		RETURN oBitmap
END CLASS

