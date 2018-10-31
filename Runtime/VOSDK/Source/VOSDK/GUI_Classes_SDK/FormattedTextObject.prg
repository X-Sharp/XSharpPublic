PARTIAL CLASS FormattedTextObject INHERIT TextObject
	PROTECT wTabSize:=8 AS LONGINT       //RvdH 070205 changed from WORD to LONG 
	

METHOD Draw() 
	//RvdH 070205 changed variables from WORD to LONG 
	//	and use typed byte pointer in stead of casting
	LOCAL hDC 			AS PTR
	LOCAL oOldPen 		AS Pen
	LOCAL oOldFont 	AS Font
	LOCAL strucTM 		IS _WinTextMetric
	LOCAL iTabStops, iLineHeight AS LONGINT
	LOCAL oOrigin 		AS Point
	LOCAL iX, iY, iLenSegment := 0 AS LONGINT
	LOCAL pText			AS BYTE PTR            
	LOCAL bChar 		AS BYTE
	LOCAL i  			AS LONGINT
	LOCAL lPrevCR 		AS LOGIC               
	
	
	
	hDC := SELF:Handle()
	
	IF (hDC != 0)
		oOldPen := oWnd:Pen
		oWnd:Pen := Pen{SELF:Color}
		oOldFont := oWnd:Font
		oWnd:Font := SELF:Font
		GetTextMetrics(hDC, @strucTM)
		
		iTabStops 	:= wTabSize * strucTM:tmAveCharWidth
		iLineHeight := strucTM:tmAscent + strucTM:tmDescent + strucTM:tmExternalLeading
		oOrigin := SELF:Origin
		iY := oOrigin:Y
		iX := oOrigin:X
		
		pText := StringAlloc( SELF:DisplayText)
		i		:= 1
		bChar := pText[i]
		//Loop to find the starting point of the text based on
		//the number of lines to output
		DO WHILE TRUE
			DO CASE
			CASE IsDBCSLeadByte(bChar)
				i++
				lPrevCR := FALSE
			CASE (bChar == 13) //LF
				iY += iLineHeight
				lPrevCR := TRUE
			CASE (bChar == 10) //CR
				IF !lPrevCR //treat CR LF pair as one new line
					iY += iLineHeight
				ENDIF
				lPrevCR := FALSE
			CASE (bChar == 0)
				EXIT
			OTHERWISE
				lPrevCR := FALSE
			ENDCASE      
			i++
			bChar := pText[i]
		ENDDO
		i:=1
		lPrevCR := FALSE
		bChar := pText[i]
		DO WHILE TRUE
			DO CASE
			CASE IsDBCSLeadByte(bChar)
				i++
				iLenSegment++
				lPrevCR := FALSE
				
			CASE (bChar == 13) //CR
				IF (iLenSegment != 0)    
					TabbedTextOut( hDC, iX, iY, pText+i-iLenSegment  , iLenSegment, 1, @iTabStops, iX)					
				ENDIF
				lPrevCR := TRUE
				iLenSegment := 0
				iY -= iLineHeight //Move to new line
				
			CASE (bChar == 10) //LF
				IF !lPrevCR
					IF (iLenSegment != 0)
						TabbedTextOut( hDC, iX, iY, pText+i-iLenSegment, iLenSegment, 1, @iTabStops, iX)
					ENDIF
					iLenSegment := 0
					iY -= iLineHeight //Move to new line
				ENDIF
				lPrevCR := FALSE
				
			CASE (bChar == 0)
				IF (iLenSegment != 0)
					TabbedTextOut( hDC, iX, iY, pText+i-iLenSegment, iLenSegment, 1, @iTabStops, iX)
				ENDIF
				EXIT
				
			OTHERWISE
				iLenSegment++
				lPrevCR := FALSE
			ENDCASE          
			i++
			bChar := pText[i]
		ENDDO
		MemFree(pText)
		oWnd:Pen := oOldPen
		oWnd:Font := oOldFont
	ENDIF
	
	RETURN NIL

CONSTRUCTOR(oPoint, cText, oFont, oColor) 
    
    SUPER(oPoint, cText, oFont, oColor)


RETURN 

ASSIGN TabSize(nNewTabSize) 
	
	
	IF !IsLong(nNewTabSize)
		WCError{#TabSize,#FormattedTextObject,__WCSTypeError,nNewTabSize,1}:@@Throw()
	ENDIF
	
	RETURN wTabSize:=nNewTabSize
	
	
END CLASS

