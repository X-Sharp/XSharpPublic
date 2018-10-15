PARTIAL CLASS FixedIcon INHERIT FixedImage

METHOD __SetImage(uResID AS USUAL) AS OBJECT STRICT 
	//PP-030828 Strong typing
	

	RETURN SELF:SetIcon(Icon{uResID})

METHOD AsString () 
	

	RETURN "#FixedImage Object"

CONSTRUCTOR(uOwner, uID, uPoint, uResID) 
	

	SUPER(uOwner, uID, uPoint, Dimension{GetSystemMetrics(SM_CXICON),;
		GetSystemMetrics(SM_CYICON)}, uResID)

	RETURN 

METHOD SetIcon(oIcon) 
	

	SELF:SetStyle(SS_ICON)
	oImage := oIcon
	SendMessage(SELF:Handle(), STM_SETIMAGE, IMAGE_ICON, LONGINT(_CAST, oIcon:Handle()))

	RETURN oIcon


END CLASS

