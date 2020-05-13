
//Todo: WIndowScrollBar classes
CLASS WindowHorizontalScrollBar INHERIT WindowScrollBar

CONSTRUCTOR(oOwner, xID, oPoint, oDimension, lDataAware) 
	SUPER(oOwner, xID, oPoint, oDimension, lDataAware)

END CLASS

CLASS WindowScrollBar INHERIT ScrollBar

CONSTRUCTOR(oOwner, xID, oPoint, oDimension, lDataAware) 
	SUPER(oOwner, xID, oPoint, oDimension, lDataAware)


RETURN 
END CLASS

CLASS WindowVerticalScrollBar INHERIT WindowScrollBar
CONSTRUCTOR(oOwner, xID, oPoint, oDimension, lDataAware) 
	SUPER(oOwner, xID, oPoint, oDimension, lDataAware)

	
END CLASS

