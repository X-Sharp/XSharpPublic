#region DEFINES
STATIC DEFINE SUBMITDLG_DONEBUTTON := 101
STATIC DEFINE SUBMITDLG_PROGRESSBAR1 := 100
#endregion

class SubmitDlg inherit DIALOGWINDOW

	protect oDCProgressBar1 as PROGRESSBAR
	protect oCCDoneButton as PUSHBUTTON

METHOD DoneButton( )
	SELF:EndDialog()
	return nil

CONSTRUCTOR(oParent)

self:PreInit()

SUPER(oParent,ResourceID{"SubmitDlg",_GetInst()},TRUE)

oDCProgressBar1 := ProgressBar{self,ResourceID{SUBMITDLG_PROGRESSBAR1,_GetInst()}}
oDCProgressBar1:HyperLabel := HyperLabel{#ProgressBar1,NULL_STRING,NULL_STRING,NULL_STRING}

oCCDoneButton := PushButton{self,ResourceID{SUBMITDLG_DONEBUTTON,_GetInst()}}
oCCDoneButton:HyperLabel := HyperLabel{#DoneButton,"Done",NULL_STRING,NULL_STRING}

self:HyperLabel := HyperLabel{#SubmitDlg,"Submit, Prepare and Deliver ...",NULL_STRING,NULL_STRING}

self:PostInit()

return self



METHOD PostShowDialog() AS USUAL
  LOCAL i AS INT
  	FOR i:=1 TO 100
		oDCProgressBar1:Position := i
        DoEvents()
		SELF:Wait()
	NEXT






method Wait()
	LOCAL l := DateTime.Now:Ticks AS INT64

    WHILE ((DateTime.Now:Ticks - l) < 25)
        System.Threading.Thread.Sleep(10)
	end

	return nil

END CLASS
