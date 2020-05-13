
CLASS HorizontalScrollBar INHERIT ScrollBar

    PROPERTY ControlType AS ControlType GET ControlType.HorizontalScrollBar

	CONSTRUCTOR(oOwner, xID, oPoint, oDimension) 
		SUPER(oOwner,xID,oPoint,oDimension)
		RETURN 

END CLASS

CLASS ScrollBar INHERIT Control

	ACCESS __ScrollBar as System.Windows.Forms.ScrollBar
		RETURN (System.Windows.Forms.ScrollBar) oCtrl

	[Obsolete];
	METHOD __SetColors(_hDc AS IntPtr) AS IntPtr STRICT 
		RETURN IntPtr.Zero

	ASSIGN __Value(nValue AS USUAL)  STRICT 
		IF IsNil(nValue)
			nValue := 0
		ELSEIF IsString(nValue)
			nValue := Val(nValue)
		ENDIF

		SELF:ThumbPosition := LONGINT(Round(nValue, 0))
		RETURN 

	PROPERTY BlockSize AS LONGINT GET __ScrollBar:LargeChange SET __ScrollBar:LargeChange := Value 

	//METHOD Create() 

	//	IF (SUPER:Create() != NULL_PTR)
	//		//SE-051114
	//		SELF:SetInfo(oRange)    

	//	ENDIF

	//	RETURN hWnd


	CONSTRUCTOR(oOwner, xID, oPoint, oDimension, lDataAware) 
		

		Default(@lDataAware, TRUE)
		IF !IsInstanceOfUsual(xID,#ResourceID)
			SUPER(oOwner,xID,oPoint,oDimension,"ScrollBar",,lDataAware)
		ELSE
			SUPER(oOwner,xID,oPoint,oDimension,,,lDataAware)
		ENDIF
		RETURN 

	PROPERTY PageSize AS LONGINT GET __ScrollBar:LargeChange SET __ScrollBar:LargeChange := Value 

	ACCESS Range as Range
		RETURN Range{__ScrollBar:Minimum, __ScrollBar:Maximum}

	ASSIGN Range(oScrollRange as Range) 
		__ScrollBar:Minimum := oScrollRange:Min
		__ScrollBar:Maximum := oScrollRange:Max
		RETURN 

	METHOD SetInfo(oScrollRange, nThumbPosition, nPageSize, lDisableNoScroll) 
		RETURN 0

	METHOD SetThumbPosition(nPosition, lNotifyOwner) 
		__ScrollBar:Value := nPosition
		IF lNotifyOwner
			//Todo Notify Owner
		ENDIF		
		RETURN NIL
	

	ACCESS TextValue AS STRING
		RETURN AllTrim(AsString(SELF:ThumbPosition))

	ASSIGN TextValue(cNewPos  AS STRING) 
		LOCAL wOldValue AS LONGINT
		wOldValue 			:= SELF:ThumbPosition
		SELF:ThumbPosition 	:= Val(cNewPos)
		SELF:Modified 		:= TRUE
		SELF:ValueChanged 	:= !wOldValue == SELF:ThumbPosition

		RETURN 

	PROPERTY ThumbPosition AS INT GET __ScrollBar:Value SET __ScrollBar:Value := Value
	PROPERTY UnitSize AS INT  GET __ScrollBar:SmallChange SET __ScrollBar:SmallChange :=Value

	ACCESS Value 
		RETURN SELF:ThumbPosition

	ASSIGN Value(nValue) 
		LOCAL iOldValue AS INT

		iOldValue := SELF:ThumbPosition
		SELF:__Value := nValue
		SELF:Modified := TRUE
		SELF:ValueChanged := iOldValue != SELF:ThumbPosition
		RETURN 

END CLASS

CLASS VerticalScrollBar INHERIT ScrollBar

    PROPERTY ControlType AS ControlType GET ControlType.VerticalScrollBar

	CONSTRUCTOR(oOwner, xID, oPoint, oDimension) 

		SUPER(oOwner,xID,oPoint,oDimension)
		RETURN 
	
END CLASS

