// Application : XPorter
// dlgProgress.prg , Created : 28-9-2016   15:10
// User : robert

PARTIAL CLASS frmProgress INHERIT System.Windows.Forms.Form IMPLEMENTS IProgress

CONSTRUCTOR()

	SUPER()

	SELF:InitializeForm()

RETURN

VIRTUAL METHOD WriteLine(cText AS STRING) AS VOID
	SELF:oTbMessage:Text += cText + e"\r\n"   
	SELF:otbMessage:SelectionStart := SELF:otbMessage:Text:Length   
	SELF:otbMessage:ScrollToCaret()

VIRTUAL METHOD Stop() AS VOID
	SELF:obtnClose:Enabled := TRUE
VIRTUAL METHOD Start() AS VOID
	SELF:obtnClose:Enabled := FALSE

METHOD btnCloseClick(sender AS System.Object , e AS System.EventArgs) AS VOID
	SELF:Close()
RETURN


END CLASS                                                   
