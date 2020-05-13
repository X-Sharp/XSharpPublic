

CLASS CurHand INHERIT Pointer

	CONSTRUCTOR () 
		SUPER(ResourceID{"CURHAND", _GetInst()})
		RETURN 

END CLASS

CLASS HyperLink INHERIT FixedText

    PROPERTY Controltype AS Controltype GET Controltype.Label

    METHOD OnControlCreated(oC AS System.Windows.Forms.Control) AS VOID
        VAR oLabel := (VOLabel) oC
        oLabel:Click += Clicked

    METHOD Clicked(sender AS OBJECT, e AS EventArgs) AS VOID
       SELF:OpenLink()
        RETURN

    CONSTRUCTOR(oOwner, xID, oPoint, oDimension, cText) 
		SUPER(oOwner, xID, oPoint, oDimension, cText )
        oCtrl:Cursor := System.Windows.Forms.Cursors.Hand
		RETURN 

	METHOD OpenLink() 
		ShellOpen(SELF:Owner, SELF:Caption)
		RETURN SELF
END CLASS

FUNCTION ShellOpen(oWindow AS Window, cFile AS STRING) AS VOID STRICT
	LOCAL hWnd AS PTR 

	cFile := AllTrim(Lower(cFile))
	IF At2("@",cFile)>1
	    IF ! cFile = "mailto:"
	        cFile := "mailto:" + cFile
	    ENDIF
	ENDIF
	IF ! Empty(cFile)
	    IF oWindow != NULL_OBJECT
	        hWnd := oWindow:Handle()
	    ELSE 
	        hWnd := NULL_PTR //Desktop
	    ENDIF 
	    ShellExecute(hWnd, "open", cFile, NULL, NULL, SW_SHOWNORMAL)
    ENDIF
	RETURN

_DLL FUNCTION ShellExecute( hWnd AS IntPtr, lpOperation AS STRING, lpFile AS STRING,;
	lpParameters AS STRING, lpDirectory AS STRING, nShowCmd AS INT) AS IntPtr PASCAL:SHELL32.ShellExecuteA ANSI


