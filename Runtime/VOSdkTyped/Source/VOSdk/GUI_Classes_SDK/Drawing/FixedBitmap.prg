/// <include file="Gui.xml" path="doc/FixedBitmap/*" />
CLASS FixedBitmap INHERIT FixedImage
	PROTECT hInst AS IntPtr

 /// <exclude />
	METHOD __SetImage(uResID AS USUAL) AS OBJECT STRICT
		RETURN SELF:SetBitmap(Bitmap{uResID})

/// <include file="Gui.xml" path="doc/FixedBitmap.AsString/*" />
	METHOD AsString() as string strict
		RETURN "#FixedBitmap Object"

/// <include file="Gui.xml" path="doc/FixedBitmap.ctor/*" />
	CONSTRUCTOR(uOwner, uID, uPoint, uDimension, uResID)
		SUPER(uOwner, uID, uPoint, uDimension, uResID)
		IF IsInstanceOfUsual(uID , #ResourceID)
			LOCAL oResID AS ResourceID
			oResID := uID
			hInst := oResID:Handle()
		ENDIF
		IF IsInstanceOfUsual(uResID , #ResourceID)
			SELF:__SetImage(uResID)
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

