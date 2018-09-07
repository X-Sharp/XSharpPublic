USING System.Drawing
USING System.IO
USING XSharp.VOEditors
USING System.Windows.Forms
USING XSharp.VODesigners
BEGIN NAMESPACE XSharp.VOEditors
CLASS XSharp_VOWEDControl INHERIT VOWEDControl IMPLEMENTS IVOWEDControl

	PROPERTY StatusMessage AS StatusMessageDelegate AUTO

    CONSTRUCTOR()
        SUPER()
    RETURN

    METHOD OpenWindow(cFileName AS STRING) AS LOGIC
        LOCAL oFileInfo AS FileInfo
        oFileInfo := FileInfo{cFileName}
        IF !VOWindowEditorTemplate.Load(oFileInfo:Directory:FullName)
            RETURN FALSE
        ENDIF
        VOWEDControl.InitializeGrid()
        VOWEDControl.InitializeToolbox()
        VOWEDControl.ToolBox:SelectPointer()
		SUPER:StatusBarMessage := ShowStatusBarMessage
        
        SELF:oOptions:oGridSize := Size{VOWindowEditorTemplate.GridX , VOWindowEditorTemplate.GridY}
        
    	SELF:oWed := XSharp_VOWindowEditor{SELF , SELF:oOptions , VOWEDControl.Grid , VOWEDControl.ToolBox}
    	SELF:oEditor := SELF:oWed
        IF .not. SELF:oWed:Open(cFileName)
        	RETURN FALSE
        ENDIF
        SELF:oWed:IsDirtyChanged := SELF:oIsDirtyChangedHandler
        SELF:oWed:TriggerSave := SELF:oTriggerSaveHandler
        SELF:oWed:StatusBarMessage := SELF:oStatusBarMessage
		SELF:oWed:ReadOnly := FALSE
    RETURN TRUE

    METHOD OpenMenu(cFileName AS STRING) AS LOGIC
		VOWEDControl.InitializeGrid()
		SELF:oMed := XSharp_VOMenuEditor{SELF , VOWEDControl.Grid }
    	SELF:oEditor := SELF:oMed
		IF ! SELF:oMed:Open(cFileName)
			RETURN false
		ENDIF
        SELF:oMed:IsDirtyChanged := SELF:oIsDirtyChangedHandler
        SELF:oMed:TriggerSave := SELF:oTriggerSaveHandler
		RETURN TRUE

    METHOD OpenFieldSpec(cFileName AS STRING) AS LOGIC
		VOWEDControl.InitializeGrid()
		SELF:oFed := XSharp_VOFieldSpecEditor{SELF , VOWEDControl.Grid }
    	SELF:oEditor := SELF:oFed
		IF ! SELF:oFed:Open(cFileName)
			RETURN false
		ENDIF
        SELF:oFed:IsDirtyChanged := SELF:oIsDirtyChangedHandler
        SELF:oFed:TriggerSave := SELF:oTriggerSaveHandler
		RETURN TRUE

    METHOD OpenDBServer(cFileName AS STRING) AS LOGIC
		VOWEDControl.InitializeGrid()
		SELF:oDed := XSharp_VODbServerEditor{SELF , VOWEDControl.Grid }
    	SELF:oEditor := SELF:oDed
		IF ! SELF:oDed:Open(cFileName)
			RETURN false
		ENDIF
        SELF:oDed:IsDirtyChanged := SELF:oIsDirtyChangedHandler
        SELF:oDed:TriggerSave := SELF:oTriggerSaveHandler
    RETURN TRUE

    METHOD SetStandalone() AS VOID
	    IF SELF:oWed != NULL
		    SELF:oWed:StandAlone := TRUE
	    ENDIF
    RETURN

	PROPERTY IsGridEnabled AS LOGIC GET oWed != NULL && oWed:IsGridEnabled


	PROPERTY ReadOnly AS LOGIC GET SUPER:oEditor:ReadOnly SET SUPER:ReadOnly := VALUE

	PROPERTY IWin32Window AS IWin32Window GET (IWin32Window) SELF

	METHOD ToggleGrid() AS VOID
		IF SELF:oWed != NULL
			SELF:oWed:ToggleGrid()
		ENDIF
	RETURN
	METHOD ShowTabOrder() AS VOID
		IF SELF:oWed != NULL
			SELF:oWed:ShowTabOrder()
		ENDIF
	RETURN
	METHOD TestForm() AS VOID
		IF SELF:oWed != NULL
			SELF:oWed:TestForm()
		ENDIF
	RETURN

	// Menu options
	METHOD CanDoAction(nType AS Actions) AS LOGIC
		VAR lOk := CanDoAction( (DesignerActionType) (INT) nType)
		RETURN lOk

	METHOD Action(nType AS Actions) AS VOID
		DoAction( (DesignerActionType) (INT) nType)
	
	METHOD ShowStatusBarMessage(cMessage AS STRING) AS VOID
		IF (SELF:StatusMessage != NULL)
			StatusMessage(cMessage)
		ENDIF

END CLASS
END NAMESPACE