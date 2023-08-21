/// <include file="Gui.xml" path="doc/WindowHorizontalScrollBar/*" />
CLASS WindowHorizontalScrollBar INHERIT WindowScrollBar


/// <include file="Gui.xml" path="doc/WindowHorizontalScrollBar.Destroy/*" />
METHOD Destroy()  AS USUAL CLIPPER
	
	
	ShowScrollBar(hWnd, SB_HORZ, FALSE)
	SUPER:Destroy()


	RETURN SELF


/// <include file="Gui.xml" path="doc/WindowHorizontalScrollBar.ctor/*" />
CONSTRUCTOR(oOwner) 


	SUPER(oOwner, 0x0000C000L, Point{0,0}, Dimension{0,0})
	SELF:__ClassName:=""
	wType := SB_HORZ
	hWnd 	:= oOwner:Handle()


	RETURN 


END CLASS


/// <include file="Gui.xml" path="doc/WindowScrollBar/*" />
CLASS WindowScrollBar INHERIT ScrollBar


/// <include file="Gui.xml" path="doc/WindowScrollBar.ctor/*" />
CONSTRUCTOR(oOwner, xID, oPoint, oDimension, lDataAware) 
    
    
    SUPER(oOwner, xID, oPoint, oDimension, lDataAware)




RETURN 
END CLASS


/// <include file="Gui.xml" path="doc/WindowVerticalScrollBar/*" />
CLASS WindowVerticalScrollBar INHERIT WindowScrollBar


/// <include file="Gui.xml" path="doc/WindowVerticalScrollBar.Destroy/*" />
METHOD Destroy()  AS USUAL CLIPPER
	
	
	ShowScrollBar(hWnd, SB_VERT, FALSE)
	SUPER:Destroy()


	RETURN SELF


/// <include file="Gui.xml" path="doc/WindowVerticalScrollBar.ctor/*" />
CONSTRUCTOR(oOwner) 
	
	
	SUPER(oOwner, 0x0000C001L, Point{0,0}, Dimension{0,0})
	SELF:__ClassName:=""
	wType := SB_VERT
	hWnd 	:= oOwner:Handle()


	RETURN 
END CLASS


