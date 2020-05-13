

CLASS FixedIcon INHERIT FixedImage

	METHOD __SetImage(uResID AS USUAL) AS OBJECT STRICT 
		RETURN SELF:SetIcon(Icon{uResID})

	METHOD AsString () 
		RETURN "#FixedImage Object"

	CONSTRUCTOR(uOwner, uID, uPoint, uResID) 
		LOCAL oSI as System.Drawing.Size
		oSI:= System.Windows.Forms.SystemInformation.IconSize
		
		SUPER(uOwner, uID, uPoint, Dimension{oSI:Width,oSI:Height}, uResID)

		RETURN 

	METHOD SetIcon(oIcon as Icon) as Icon
		SELF:SetStyle(SS_ICON)
		oImage := oIcon
		SELF:__Label:Image := oImage
        SELF:__Label:FlatStyle := System.Windows.Forms.FlatStyle.Flat
		RETURN oIcon


END CLASS

