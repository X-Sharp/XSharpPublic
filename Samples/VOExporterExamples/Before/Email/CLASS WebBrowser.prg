CLASS WebBrowser INHERIT OleControl
	EXPORT Okay AS LOGIC
	


METHOD Display(cText AS STRING, lHtml AS LOGIC) AS VOID PASCAL 
	LOCAL cFileName AS STRING
	LOCAL ptrFile AS PTR
	LOCAL nSize AS DWORD

	// Turn an string into an HTM or TXT file for display in a browser control
	// Returns the filename is successful
	
	cFileName := GetTempFilePath() + "temp."+IF(lHtml, "htm", "txt")
	nSize := SLen(cText)
	
	ptrFile := FCreate(cFileName, FC_NORMAL)	// overwrite any file which is there
	IF ptrFile != NULL_PTR
		IF FWrite(ptrFile, cText, nSize) != nSize
			cFileName := ""	// failed the write
		ENDIF
		FClose(ptrFile)
	ELSE
		cFileName := ""	// failed to open the file
	ENDIF
	
	IF !Empty(cFileName)	
		Send(SELF, #Navigate, cFileName)
	ENDIF
	
	RETURN



METHOD GoEnd()	 
	LOCAL bError 	AS CODEBLOCK

	bError:=ErrorBlock({ |x| WebBrowserTrapError(x) })

 	BEGIN SEQUENCE			
		Send( SELF, #Navigate, "#end" )

		ErrorBlock(bError)
	RECOVER
		ErrorBlock( bError )
	END SEQUENCE

   RETURN SELF

METHOD GoTop()	 
	LOCAL bError AS CODEBLOCK

	bError:=ErrorBlock({ |x| WebBrowserTrapError(x) })

 	BEGIN SEQUENCE			
		Send( SELF, #Navigate, "#top" )
		
		ErrorBlock(bError)
	RECOVER
		ErrorBlock( bError )
	END SEQUENCE

   RETURN SELF

METHOD HTMLPageGoBack()	 
	LOCAL bError 	AS CODEBLOCK

	bError:=ErrorBlock({ |x| WebBrowserTrapError(x) })

 	BEGIN SEQUENCE			
		Send( SELF, #GoBack )
		ErrorBlock(bError)
	RECOVER
		ErrorBlock( bError )
	END SEQUENCE

   RETURN SELF

METHOD HTMLPageGoForward()	 
	LOCAL bError 	AS CODEBLOCK

	bError:=ErrorBlock({ |x| WebBrowserTrapError(x) })

 	BEGIN SEQUENCE			
		Send( SELF, #GoForward )
		ErrorBlock(bError)
	RECOVER
		ErrorBlock( bError )
	END SEQUENCE

   RETURN SELF

CONSTRUCTOR( oWindow, xID, oPoint, oDimension, lDataAware )	 

	Default( @xID, -1 )
	Default( @oPoint, Point{} )
	Default( @oDimension, Dimension{} )
	Default( @lDataAware, FALSE )

	SUPER( oWindow, xID, oPoint, oDimension, lDataAware )
	SELF:SetStyle(WS_BORDER, FALSE)
	SELF:SetExStyle(WS_EX_CLIENTEDGE, FALSE)

	SELF:Caption	 := "WebBrowser"
	SELF:HyperLabel := HyperLabel{ #WebBrowser, NULL_STRING, NULL_STRING, NULL_STRING }
	SELF:Okay		 := SELF:CreateEmbedding( "Shell.Explorer" )


	RETURN SELF

METHOD PageSetup() 
	LOCAL bError 	AS CODEBLOCK

	bError:=ErrorBlock({ |x| WebBrowserTrapError(x) })

 	BEGIN SEQUENCE			
     	Send( SELF, #ExecWB, OLECMDID_PAGESETUP, OLECMDEXECOPT_DODEFAULT, NIL, NIL )
		ErrorBlock(bError)
	RECOVER
		ErrorBlock( bError )
	END SEQUENCE

   RETURN SELF

METHOD Print()	 
	LOCAL bError 	AS CODEBLOCK

	bError:=ErrorBlock({ |x| WebBrowserTrapError(x) })

 	BEGIN SEQUENCE			
     	Send( SELF, #ExecWB, OLECMDID_PRINT, OLECMDEXECOPT_DODEFAULT, NIL, NIL )
		ErrorBlock(bError)
	RECOVER
		ErrorBlock( bError )
	END SEQUENCE

   RETURN SELF

METHOD PrintPreview() 
	// Only used by IE 5.5 which is a great feature
	LOCAL bError 	AS CODEBLOCK

	bError:=ErrorBlock({ |x| WebBrowserTrapError(x) })

 	BEGIN SEQUENCE			
     	Send( SELF, #ExecWB, OLECMDID_PRINTPREVIEW, OLECMDEXECOPT_DODEFAULT, NIL, NIL )
		ErrorBlock(bError)
	RECOVER
		ErrorBlock( bError )
	END SEQUENCE

   RETURN SELF

METHOD Quit(		) 
	// Exits application and closes the open document.

	LOCAL oMethod  	AS cOleMethod
	LOCAL uRetValue	AS USUAL

	oMethod		      	:= cOleMethod{}
	oMethod:symName	  	:= String2Symbol("Quit")
	oMethod:iMemberid  	:= 300 
	oMethod:wInvokeKind	:= INVOKE_METHOD  

	uRetValue := SELF:oAuto:__Invoke(oMethod, DWORD(_BP+16),PCount())

	RETURN (uRetValue)

END CLASS
STATIC FUNCTION WebBrowserTrapError(oError AS Error)
	BREAK oError
RETURN NIL



