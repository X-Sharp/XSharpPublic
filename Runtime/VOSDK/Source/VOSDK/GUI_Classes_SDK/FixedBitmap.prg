/// <include file="Gui.xml" path="doc/FixedBitmap/*" />
CLASS FixedBitmap INHERIT FixedImage


 /// <exclude />
METHOD __SetImage(uResID AS USUAL) AS OBJECT STRICT 
	//PP-030828 Strong typing
	
	


	RETURN SELF:SetBitmap(Bitmap{uResID})


/// <include file="Gui.xml" path="doc/FixedBitmap.AsString/*" />
METHOD AsString() 
	
	


	RETURN "#FixedBitmap Object"


/// <include file="Gui.xml" path="doc/FixedBitmap.ctor/*" />
CONSTRUCTOR(uOwner, uID, uPoint, uDimension, uResID) 
    
    
    SUPER(uOwner, uID, uPoint, uDimension, uResID)




RETURN 


/// <include file="Gui.xml" path="doc/FixedBitmap.SetBitmap/*" />
METHOD SetBitmap(oBitmap) 
	
	


	SELF:SetStyle(SS_BITMAP)
	oImage := oBitmap
	SendMessage(SELF:Handle(), STM_SETIMAGE, IMAGE_BITMAP, LONGINT(_CAST, oBitmap:Handle()))


	RETURN oBitmap
END CLASS


