CLASS FixedBitmap INHERIT FixedImage

METHOD __SetImage(uResID AS USUAL) AS OBJECT STRICT 
	//PP-030828 Strong typing
	

	RETURN SELF:SetBitmap(Bitmap{uResID})

METHOD AsString() 
	

	RETURN "#FixedBitmap Object"

CONSTRUCTOR(uOwner, uID, uPoint, uDimension, uResID) 
    
    SUPER(uOwner, uID, uPoint, uDimension, uResID)


RETURN 

METHOD SetBitmap(oBitmap) 
	

	SELF:SetStyle(SS_BITMAP)
	oImage := oBitmap
	SendMessage(SELF:Handle(), STM_SETIMAGE, IMAGE_BITMAP, LONGINT(_CAST, oBitmap:Handle()))

	RETURN oBitmap
END CLASS

