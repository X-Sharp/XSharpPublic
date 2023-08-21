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


method ShowModal(lActive) 
  local i as int	

  if (lActive)
		for i:=1 to 100
			oDCProgressBar1:Position := i
			self:Wait()
		next
  endif

  return super:ShowModal(lActive)	

	

method Wait() 
	local l := GetTickCount() as DWORD
	
	while ((GetTickCount() - l) < 25)
	end	

	return nil

END CLASS
