/// <include file="Gui.xml" path="doc/HorizontalScrollBar/*" />
CLASS HorizontalScrollBar INHERIT ScrollBar


/// <include file="Gui.xml" path="doc/HorizontalScrollBar.ctor/*" />
CONSTRUCTOR(oOwner, xID, oPoint, oDimension) 
	
	
	SUPER(oOwner,xID,oPoint,oDimension)
	IF hWnd==0 //if xID is not a resourceID
		SELF:SetStyle(SBS_HORZ)
	ENDIF


	RETURN 




END CLASS


/// <include file="Gui.xml" path="doc/ScrollBar/*" />
CLASS ScrollBar INHERIT Control
	PROTECT wType AS LONGINT			// RvdH 070205 Changed from WORD to LONG
	PROTECT wBlockSize AS LONGINT		// RvdH 070205 Changed from WORD to LONG
	PROTECT wUnitSize AS LONGINT		// RvdH 070205 Changed from WORD to LONG
	PROTECT oRange AS Range


	//PP-030828 Strong typing
 /// <exclude />
	METHOD __SetColors(_hDc AS PTR) AS PTR STRICT 
	IF oControlBackground = NULL_OBJECT
		RETURN NULL_PTR
	ENDIF
	RETURN SUPER:__SetColors(_hDc)


 /// <exclude />
ACCESS __Type AS LONGINT STRICT 
	//PP-030828 Strong typing


	RETURN wType


 /// <exclude />
ASSIGN __Value(nValue AS USUAL)  STRICT 
	//PP-030828 Strong typing
	
	


	// RvdH 050602 Bug 12 Handle NIL (NULL in SQL)
	IF IsNil(nValue)
		nValue := 0
	ELSEIF IsString(nValue)
		nValue := Val(nValue)
	ENDIF


	SELF:Thumbposition := LONGINT(Round(nValue, 0))
	RETURN 


/// <include file="Gui.xml" path="doc/ScrollBar.BlockSize/*" />
ACCESS BlockSize 


	RETURN wBlockSize


/// <include file="Gui.xml" path="doc/ScrollBar.BlockSize/*" />
ASSIGN BlockSize(nSize) 


	IF !IsLong(nSize) .OR. nSize<0
		WCError{#BlockSize,#ScrollBar,__WCSTypeError,nSize,1}:Throw()
	ENDIF


	wBlockSize := nSize




	RETURN 


/// <include file="Gui.xml" path="doc/ScrollBar.Create/*" />
METHOD Create() 


	IF (SUPER:Create() != NULL_PTR)
		//SE-051114
		SELF:SetInfo(oRange)    


	ENDIF


	RETURN hWnd


/// <include file="Gui.xml" path="doc/ScrollBar.Hide/*" />
METHOD Hide() 
	
	


	IF (hWnd == NULL_PTR)
		SELF:Create()
	ELSE
		//PP-040110 fix for Bug 12617
		ShowScrollBar(hWnd, wType, FALSE)
	ENDIF


	RETURN SELF


/// <include file="Gui.xml" path="doc/ScrollBar.ctor/*" />
CONSTRUCTOR(oOwner, xID, oPoint, oDimension, lDataAware) 
	
	


	Default(@lDataAware, TRUE)
	IF !IsInstanceOfUsual(xID,#ResourceID)
		SUPER(oOwner,xID,oPoint,oDimension,"ScrollBar",,lDataAware)
	ELSE
		SUPER(oOwner,xID,oPoint,oDimension,,,lDataAware)
	ENDIF


	wBlockSize 	:= 10
	wUnitSize 	:= 1
	wType 		:= SB_CTL
	oRange 		:= Range{0,99}


	RETURN 


/// <include file="Gui.xml" path="doc/ScrollBar.PageSize/*" />
ACCESS PageSize 
	//SE-051114
	LOCAL sScrollInfo IS _WINSCROLLINFO
	IF SELF:ValidateControl()
		sScrollInfo:cbSize := _SIZEOF(_winSCROLLINFO)
		sScrollInfo:fMask  := SIF_PAGE
		GetScrollInfo(hWnd, wType, @sScrollInfo)
		RETURN sScrollInfo:nPage
	ENDIF


	RETURN 0l


/// <include file="Gui.xml" path="doc/ScrollBar.PageSize/*" />
ASSIGN PageSize(nValue) 
	//SE-051114
	IF !IsLong(nValue)
		WCError{#PageSize, #ScrollBar, __WCSTypeError, nValue, 1}:Throw()
	ENDIF
   //RvdH 070320 This was setting the Position and not the size 
   //SELF:SetInfo(NIL, nValue)
	SELF:SetInfo(NIL, NIL, nValue)


	RETURN 


/// <include file="Gui.xml" path="doc/ScrollBar.Range/*" />
ACCESS Range 
	//SE-070421
	LOCAL sScrollInfo IS _WINSCROLLINFO
	
	
	IF hWnd != NULL_PTR
		sScrollInfo:cbSize := _SIZEOF(_winSCROLLINFO)
		sScrollInfo:fMask  := SIF_RANGE
		GetScrollInfo(hWnd, wType, @sScrollInfo) 
		oRange:Min := sScrollInfo:nMin
		oRange:Max := sScrollInfo:nMax
	ENDIF


	RETURN oRange


/// <include file="Gui.xml" path="doc/ScrollBar.Range/*" />
ASSIGN Range(oScrollRange) 
	
	
	IF ! IsInstanceOfUsual(oScrollRange, #Range)
		WCError{#Range,#ScrollBar,__WCSTypeError,oRange,1}:Throw()
	ENDIF
	
	
	oRange := oScrollRange 
	
	
   //SE-070421        
   IF hWnd != NULL_PTR
	   SELF:SetInfo(oRange)
   ENDIF   


	RETURN 


/// <include file="Gui.xml" path="doc/ScrollBar.SetInfo/*" />
METHOD SetInfo(oScrollRange, nThumbPosition, nPageSize, lDisableNoScroll) 
	//SE-051114
	//SE-070423
	LOCAL sScrollInfo IS _WINSCROLLINFO


	sScrollInfo:cbSize := _SIZEOF(_winSCROLLINFO)
	IF ! IsNil(oScrollRange)
		IF ! IsInstanceOfUsual(oRange,#Range)
			WCError{#SetInfo, #ScrollBar, __WCSTypeError, oRange, 1}:Throw()
		ENDIF
	   oRange := oScrollRange
	   sScrollInfo:fMask := SIF_RANGE
	   sScrollInfo:nMin  := oRange:Min
	   sScrollInfo:nMax  := oRange:Max
	ENDIF


   IF ! IsNil(nThumbPosition)
      IF ! IsLong(nThumbPosition)
		   WCError{#SetInfo, #ScrollBar, __WCSTypeError, nThumbPosition, 2}:Throw()
	   ENDIF
	   sScrollInfo:fMask := _OR(sScrollInfo:fMask, SIF_POS)
	   sScrollInfo:nPos  := nThumbPosition
	ENDIF


	IF ! IsNil(nPageSize)
      IF ! IsLong(nPageSize)
		   WCError{#SetInfo, #ScrollBar, __WCSTypeError, nPageSize, 3}:Throw()
	   ENDIF
	   sScrollInfo:fMask := _OR(sScrollInfo:fMask, SIF_PAGE)
	   sScrollInfo:nPage := nPageSize
	ENDIF


   IF IsLogic(lDisableNoScroll) .AND. lDisableNoScroll
	   sScrollInfo:fMask := _OR(sScrollInfo:fMask, SIF_DISABLENOSCROLL)
	ENDIF


   IF SELF:ValidateControl()
   	//RvdH 070320 Changed SB_CTL to wType to fix several reported problems
   	RETURN SetScrollInfo(hWnd, wType, @sScrollInfo, IsWindowVisible(hWnd))
   ENDIF


	RETURN 0l


/// <include file="Gui.xml" path="doc/ScrollBar.SetThumbPosition/*" />
METHOD SetThumbPosition(nPosition, lNotifyOwner) 
   LOCAL hOwner AS PTR
   LOCAL dwMsg  AS DWORD
   LOCAL lParam AS LONGINT
      
      
   SELF:ThumbPosition := nPosition 
   
   
   IF ! IsLogic(lNotifyOwner) .OR. lNotifyOwner
      hOwner := oParent:Handle() 
      IF hOwner != Null_Ptr
         IF wType = SB_CTL
            lParam := LONGINT(_CAST, hWnd)
            IF _And(DWORD(_CAST, GetWindowLong(hWnd, GWL_STYLE)), SBS_HORZ) > 0
               dwMsg := WM_HSCROLL
            ELSE 
               dwMsg := WM_VSCROLL
            ENDIF
         ELSEIF wType = SB_HORZ
            dwMsg := WM_HSCROLL
         ELSE 
            dwMsg := WM_VSCROLL
         ENDIF   
            
            
         SendMessage(hOwner, dwMsg, SB_THUMBTRACK, lParam)   
      ENDIF
   ENDIF   
   
   
   RETURN NIL
   
   




/// <include file="Gui.xml" path="doc/ScrollBar.Show/*" />
METHOD Show() 
	// 	LOCAL strucScrollInfo IS _winScrollInfo
	IF (hWnd == NULL_PTR)
		SELF:Create()
	ENDIF
	ShowScrollBar(hWnd, wType, TRUE)
	RETURN SELF


/// <include file="Gui.xml" path="doc/ScrollBar.TextValue/*" />
ACCESS TextValue 
	RETURN AllTrim(AsString(SELF:Thumbposition))


/// <include file="Gui.xml" path="doc/ScrollBar.TextValue/*" />
ASSIGN TextValue(cNewPos) 
	LOCAL wOldValue AS LONGINT
	IF !IsString(cNewPos)
		WCError{#TextValue,#ScrollBar,__WCSTypeError,cNewPos,1}:Throw()
	ENDIF
	wOldValue 				:= SELF:Thumbposition
	SELF:Thumbposition 	:= Val(cNewPos)
	SELF:Modified 			:= TRUE
	SELF:ValueChanged 	:= !wOldValue == SELF:Thumbposition


	RETURN 


/// <include file="Gui.xml" path="doc/ScrollBar.ThumbPosition/*" />
ACCESS ThumbPosition 
	//SE-051114
	LOCAL sScrollInfo IS _WINSCROLLINFO


	IF SELF:ValidateControl()
		sScrollInfo:cbSize := _SIZEOF(_winSCROLLINFO)
		sScrollInfo:fMask  := SIF_POS
		GetScrollInfo(hWnd, wType, @sScrollInfo)
		RETURN sScrollInfo:nPos
	ENDIF


	RETURN 0l


/// <include file="Gui.xml" path="doc/ScrollBar.ThumbPosition/*" />
ASSIGN ThumbPosition(nValue) 
	IF !IsLong(nValue)
		WCError{#Thumbposition,#ScrollBar,__WCSTypeError,nValue,1}:Throw()
	ENDIF
   //SE-051114
	SELF:SetInfo(NIL, nValue)


	RETURN 


/// <include file="Gui.xml" path="doc/ScrollBar.UnitSize/*" />
ACCESS UnitSize 
	
	
	RETURN wUnitSize


/// <include file="Gui.xml" path="doc/ScrollBar.UnitSize/*" />
ASSIGN UnitSize(nSize) 
	
	
	IF !IsLong(nSize) .OR. nSize<0
		WCError{#UnitSize,#ScrollBar,__WCSTypeError,nSize,1}:Throw()
	ENDIF
	wUnitSize:=nSize


	RETURN 


/// <include file="Gui.xml" path="doc/ScrollBar.Value/*" />
ACCESS Value 
	
	
	RETURN SELF:Thumbposition


/// <include file="Gui.xml" path="doc/ScrollBar.Value/*" />
ASSIGN Value(nValue) 
	LOCAL iOldValue AS INT


	iOldValue := SELF:Thumbposition
	SELF:__Value := nValue
	SELF:Modified := TRUE
	SELF:ValueChanged := iOldValue != SELF:Thumbposition
	RETURN 


END CLASS


/// <include file="Gui.xml" path="doc/VerticalScrollBar/*" />
CLASS VerticalScrollBar INHERIT ScrollBar


/// <include file="Gui.xml" path="doc/VerticalScrollBar.ctor/*" />
CONSTRUCTOR(oOwner, xID, oPoint, oDimension) 


	SUPER(oOwner,xID,oPoint,oDimension)
	IF hWnd==0 //if xID is not a resourceID
		SELF:SetStyle(SBS_VERT)
	ENDIF


	RETURN 
END CLASS


