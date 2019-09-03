using Email
using AxShDocVw
using ShDocVw

CLASS WebBrowser INHERIT MultiLineEdit
	EXPORT Okay AS LOGIC
    EXPORT oHost as webBrowserHost
    EXPORT oWebBrowser as AxWebBrowser


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
		SELF:oWebBrowser:Navigate(cFileName)
	ENDIF
	
	RETURN



METHOD GoEnd()	 
	LOCAL bError 	AS CODEBLOCK

	bError:=ErrorBlock({ |x| WebBrowserTrapError(x) })

 	BEGIN SEQUENCE			
		SELF:oWebBrowser:Navigate("#end" )

		ErrorBlock(bError)
	RECOVER
		ErrorBlock( bError )
	END SEQUENCE

   RETURN SELF

METHOD GoTop()	 
	LOCAL bError AS CODEBLOCK

	bError:=ErrorBlock({ |x| WebBrowserTrapError(x) })

 	BEGIN SEQUENCE			
		SELF:oWebBrowser:Navigate("#top" )
		
		ErrorBlock(bError)
	RECOVER
		ErrorBlock( bError )
	END SEQUENCE

   RETURN SELF

METHOD HTMLPageGoBack()	 
	LOCAL bError 	AS CODEBLOCK

	bError:=ErrorBlock({ |x| WebBrowserTrapError(x) })

 	BEGIN SEQUENCE			
		SELF:oWebBrowser:GoBack ()
		ErrorBlock(bError)
	RECOVER
		ErrorBlock( bError )
	END SEQUENCE

   RETURN SELF

METHOD HTMLPageGoForward()	 
	LOCAL bError 	AS CODEBLOCK

	bError:=ErrorBlock({ |x| WebBrowserTrapError(x) })

 	BEGIN SEQUENCE			
		SELF:oWebBrowser:GoForward ()
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
    SELF:oHost := webBrowserHost{}            // Create the host window, do not show !
    SELF:oWebBrowser := SELF:oHost:axWebBrowser1   // Get the ActiveX on the form
    SetParent(oWebBrowser:Handle, self:Handle())   // Using Windows API "steal" its handle and link to the MLE
    SELF:oWebBrowser:Visible := TRUE            // make the webbrowser visible
    SELF:oWebBrowser:NavigateComplete2 += NavigateComplete2
    SELF:Okay := TRUE

	RETURN SELF

METHOD PageSetup() 
	LOCAL bError 	AS CODEBLOCK

	bError:=ErrorBlock({ |x| WebBrowserTrapError(x) })

 	BEGIN SEQUENCE			
        SELF:oWebBrowser:ExecWB(OLECMDID.OLECMDID_PAGESETUP, OLECMDEXECOPT.OLECMDEXECOPT_DODEFAULT )
		ErrorBlock(bError)
	RECOVER
		ErrorBlock( bError )
	END SEQUENCE

   RETURN SELF

METHOD Print()	 
	LOCAL bError 	AS CODEBLOCK

	bError:=ErrorBlock({ |x| WebBrowserTrapError(x) })

 	BEGIN SEQUENCE			
        SELF:oWebBrowser:ExecWB(OLECMDID.OLECMDID_PRINT, OLECMDEXECOPT.OLECMDEXECOPT_DODEFAULT )
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

METHOD Resize(oEvent) 
 LOCAL oDim as Dimension
 SUPER:Resize(oEvent)
 oDim := SELF:Size
 IF oDim:Width > 0
    SELF:oWebBrowser:SuspendLayout()
    SELF:oWebBrowser:Location := System.Drawing.Point{0,0}
    SELF:oWebBrowser:Size := System.Drawing.Size{oDim:Width,oDim:Height}
    SELF:oWebBrowser:ResumeLayout()
 ENDIF
 RETURN NIL

METHOD Destroy()
  SUPER:Destroy()
  SELF:oWebBrowser:Dispose()
  SELF:oHost:Dispose()
  RETURN NIL
METHOD NavigateComplete2(sender AS OBJECT, e AS DWebBrowserEvents2_NavigateComplete2Event) AS VOID
       SELF:Owner:StatusBar:SetText("Showing file:" +e:uRL:ToString())
END CLASS
STATIC FUNCTION WebBrowserTrapError(oError AS Error)
	BREAK oError
RETURN NIL



