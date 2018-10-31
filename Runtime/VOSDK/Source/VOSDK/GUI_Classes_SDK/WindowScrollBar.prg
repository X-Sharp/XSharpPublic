PARTIAL CLASS WindowHorizontalScrollBar INHERIT WindowScrollBar

METHOD Destroy() 
	
	ShowScrollBar(hWnd, SB_HORZ, FALSE)
	SUPER:Destroy()

	RETURN SELF

CONSTRUCTOR(oOwner) 

	SUPER(oOwner, 0x0000C000L, Point{0,0}, Dimension{0,0})
	SELF:__ClassName:=""
	wType := SB_HORZ
	hWnd 	:= oOwner:Handle()

	RETURN 

END CLASS

PARTIAL CLASS WindowScrollBar INHERIT ScrollBar

CONSTRUCTOR(oOwner, xID, oPoint, oDimension, lDataAware) 
    
    SUPER(oOwner, xID, oPoint, oDimension, lDataAware)


RETURN 
END CLASS

PARTIAL CLASS WindowVerticalScrollBar INHERIT WindowScrollBar

METHOD Destroy() 
	
	ShowScrollBar(hWnd, SB_VERT, FALSE)
	SUPER:Destroy()

	RETURN SELF

CONSTRUCTOR(oOwner) 
	
	SUPER(oOwner, 0x0000C001L, Point{0,0}, Dimension{0,0})
	SELF:__ClassName:=""
	wType := SB_VERT
	hWnd 	:= oOwner:Handle()

	RETURN 
END CLASS

