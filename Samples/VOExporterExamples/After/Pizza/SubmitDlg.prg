#region DEFINES
STATIC DEFINE SUBMITDLG_PROGRESSBAR1 := 100
STATIC DEFINE SUBMITDLG_DONEBUTTON := 101 
#endregion

class SubmitDlg inherit DIALOGWINDOW 

	protect oDCProgressBar1 as PROGRESSBAR
	protect oCCDoneButton as PUSHBUTTON

	// {{%UC%}} User code starts here (DO NOT remove this line)  
CONSTRUCTOR(oParent,uExtra)

	SELF:PreInit(oParent,uExtra)

SUPER(oParent,ResourceID{"SubmitDlg",_GetInst()},TRUE)

	SELF:oDCProgressBar1 := PROGRESSBAR{SELF , ResourceID{ SUBMITDLG_PROGRESSBAR1  , _GetInst() } }
	SELF:oDCProgressBar1:HyperLabel := HyperLabel{#ProgressBar1 , NULL_STRING , NULL_STRING , NULL_STRING}

	SELF:oCCDoneButton := PUSHBUTTON{SELF , ResourceID{ SUBMITDLG_DONEBUTTON  , _GetInst() } }
	SELF:oCCDoneButton:HyperLabel := HyperLabel{#DoneButton , "Done" , NULL_STRING , NULL_STRING}

	SELF:Caption := "Submit, Prepare and Deliver ..."
self:HyperLabel := HyperLabel{#SubmitDlg,"Submit, Prepare and Deliver ...",NULL_STRING,NULL_STRING}

	SELF:PostInit(oParent,uExtra)

RETURN


METHOD DoneButton( ) 
	SELF:EndDialog()
	return nil

method Wait() 
	local l := GetTickCount() as DWORD
	
	while ((GetTickCount() - l) < 25)
	end	

	return nil

method ShowModal(lActive) 
  local i as int	

  if (lActive)
		for i:=1 to 100
			oDCProgressBar1:Position := i
			self:Wait()
		next
  endif

  return super:ShowModal(lActive)	

	


END CLASS
