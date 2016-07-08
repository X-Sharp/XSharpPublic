//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System.Windows.Forms
using System.Drawing
using System.IO
using XSharp.VOEditors
using XSharp.ProjectAPI

DELEGATE StatusBarMessageDelegate(cMessage AS STRING) AS VOID

CLASS VOWEDControl INHERIT Panel
    PROTECT oEditor AS DesignerBase
	PROTECT PROPERTY oWed as VOWindowEditor		GET IIF(oEditor IS VOWindowEditor, (VOWindowEditor) oEditor, NULL)
	PROTECT PROPERTY oMed as VOMenuEditor		GET IIF(oEditor IS VOMenuEditor, (VOMenuEditor) oEditor, NULL)
	PROTECT PROPERTY oFed as VOFieldSpecEditor	GET IIF(oEditor IS VOFieldSpecEditor, (VOFieldSpecEditor) oEditor, NULL)
	PROTECT PROPERTY oDed as VODBServerEditor	GET IIF(oEditor IS VODBServerEditor, (VODBServerEditor) oEditor, NULL)
    PROTECT oOptions AS WindowDesignerOptions
    PROTECT oIsDirtyChangedHandler AS EventHandler
    PROTECT oTriggerSaveHandler AS EventHandler
    PROTECT oStatusBarMessage AS StatusBarMessageDelegate

    CONSTRUCTOR()
        SUPER()
        SELF:oOptions := WindowDesignerOptions{}
        SELF:oOptions:lUseGrid := FALSE
        SELF:oOptions:lShowGrid := FALSE
        SELF:AutoScroll := TRUE
    RETURN

    METHOD OpenFile(cFileName as STRING) AS LOGIC
        LOCAL nType as VOBinaryType
        LOCAL lResult as LOGIC
        nType := VOBinary.DetermineType(cFileName)
        SWITCH nType
        CASE VOBinaryType.Window
            lResult := OpenWindow(cFileName)
        CASE VOBinaryType.Menu
            lResult := OpenMenu(cFileName)
        CASE VOBinaryType.DbServer
            lResult := OpenDBServer(cFileName)
        CASE VOBinaryType.FieldSpec
            lResult := OpenFieldSpec(cFileName)
        OTHERWISE
            Funcs.WarningBox("Unknown VO Binary type for file: "+cFileName)
            lResult := FALSE
        END SWITCH
        RETURN lResult

    METHOD OpenWindow(cFileName AS STRING) AS LOGIC
        LOCAL oFileInfo AS FileInfo
        oFileInfo := FileInfo{cFileName}
        IF !VOWindowEditorTemplate.Load(oFileInfo:Directory:FullName)
            RETURN FALSE
        ENDIF
        VOWEDControl.InitializeGrid()
        VOWEDControl.InitializeToolbox()
        VOWEDControl.ToolBox:SelectPointer()
        
        SELF:oOptions:oGridSize := Size{VOWindowEditorTemplate.GridX , VOWindowEditorTemplate.GridY}
        
    	SELF:oEditor := VOWindowEditor{SELF , SELF:oOptions , VOWEDControl.Grid , VOWEDControl.ToolBox}
        IF .not. SELF:oEditor:Open(cFileName)
        	RETURN FALSE
        ENDIF
        SELF:oEditor:IsDirtyChanged := SELF:oIsDirtyChangedHandler
        SELF:oEditor:TriggerSave := SELF:oTriggerSaveHandler
        SELF:oEditor:StatusBarMessage := SELF:oStatusBarMessage
    RETURN TRUE

    METHOD OpenMenu(cFileName AS STRING) AS LOGIC

		VOWEDControl.InitializeGrid()
		
		SELF:oEditor := VOMenuEditor{SELF , VOWEDControl.Grid}
		IF .not. SELF:oEditor:Open(cFileName)
			RETURN FALSE
		ENDIF
		SELF:oEditor:IsDirtyChanged := SELF:oIsDirtyChangedHandler
		SELF:oEditor:TriggerSave := SELF:oTriggerSaveHandler
    RETURN TRUE

    METHOD OpenFieldSpec(cFileName AS STRING) AS LOGIC
		VOWEDControl.InitializeGrid()
		
		SELF:oEditor := VOFieldSpecEditor{SELF , VOWEDControl.Grid}
		IF .not. SELF:oEditor:Open(cFileName)
			RETURN FALSE
		ENDIF
		SELF:oEditor:IsDirtyChanged := SELF:oIsDirtyChangedHandler
		SELF:oEditor:TriggerSave := SELF:oTriggerSaveHandler
    RETURN TRUE

    METHOD OpenDBServer(cFileName AS STRING) AS LOGIC

		VOWEDControl.InitializeGrid()
		
		SELF:oEditor := VODBServerEditor{SELF , VOWEDControl.Grid}
		IF .not. SELF:oEditor:Open(cFileName)
			RETURN FALSE
		ENDIF
		SELF:oEditor:IsDirtyChanged := SELF:oIsDirtyChangedHandler
		SELF:oEditor:TriggerSave := SELF:oTriggerSaveHandler
    RETURN TRUE

    METHOD Save(cFileName AS STRING) AS LOGIC
		RETURN SELF:oEditor:Save(cFileName , FALSE)
    
	METHOD Save(cFileName AS STRING , lVnfrmOnly AS LOGIC) AS LOGIC
		RETURN SELF:oEditor:Save(cFileName , lVnfrmOnly)

    ACCESS IsDirty AS LOGIC
		RETURN SELF:oEditor:IsDirty

    ASSIGN IsDirtyChanged(oHandler AS EventHandler)
        SELF:oIsDirtyChangedHandler := oHandler
    RETURN
    
	ASSIGN TriggerSave(oHandler AS EventHandler)
        SELF:oTriggerSaveHandler := oHandler
    RETURN
    
	ASSIGN StatusBarMessage(oHandler AS StatusBarMessageDelegate)
        SELF:oStatusBarMessage := oHandler
    RETURN

	METHOD CanDoAction(eAction AS DesignerActionType) AS LOGIC
		RETURN SELF:oEditor != NULL .and. SELF:oEditor:CanDoAction(eAction)

	METHOD DoAction(eAction AS DesignerActionType) AS VOID
        IF SELF:oEditor != NULL
    		SELF:oEditor:DoAction(eAction)
        ENDIF
	RETURN

   ACCESS IsGridEnabled AS LOGIC
      RETURN oWed != NULL && oWed:IsGridEnabled

	ASSIGN ReadOnly(_lReadOnly AS LOGIC)
        IF SELF:oEditor != NULL
			SELF:oEditor:ReadOnly := _lReadOnly
        ENDIF
	RETURN
      
    METHOD ToggleGrid() AS VOID
        IF SELF:oEditor IS VOWindowEditor
            SELF:oWed:ToggleGrid()
        ENDIF
    RETURN

    METHOD ShowTabOrder() AS VOID
        IF SELF:oEditor IS VOWindowEditor
            SELF:oWed:ShowTabOrder()
        ENDIF
    RETURN

	METHOD GiveFocus() AS VOID
        IF SELF:oEditor != NULL
            SELF:oEditor:GiveFocus()
        ENDIF
	RETURN

    METHOD ShowHideTools(lShow AS LOGIC) AS VOID
        IF SELF:oEditor != NULL
            SELF:oEditor:ShowHideTools(lShow)
        ENDIF
    RETURN

    METHOD TestForm() AS VOID
        SELF:oEditor:TestForm()
    RETURN
    
    METHOD RecordCommand(cCommand AS STRING) AS VOID
		RETURN
    
	METHOD GetIndexFromLineAndColumn(n AS INT, m AS INT) AS INT
		RETURN 0
    
	METHOD GetColumnFromIndex(n AS INT) AS INT
		RETURN 0
    
	ACCESS Overstrike AS LOGIC
		RETURN FALSE
    
	ASSIGN Overstrike (l AS LOGIC)
		RETURN
    
	METHOD StopRecorder() AS VOID
		RETURN

    STATIC PROTECT oGrid AS DesignerGrid

    STATIC ACCESS Grid AS DesignerGrid
		RETURN VOWEDControl.oGrid
    STATIC PROTECT oToolBox AS ToolBox

    STATIC ACCESS ToolBox AS ToolBox
		RETURN VOWEDControl.oToolBox

    STATIC CONSTRUCTOR()
		RETURN

    STATIC METHOD InitializeGrid() AS VOID
        IF VOWEDControl.oGrid == NULL
	        VOWEDControl.oGrid := DesignerGrid{}
	        VOWEDControl.CreateToolWindow( Resources.PropertiesCaption, Point{550 , 150} , Size{300 , 500} , oGrid)
	    ENDIF
		RETURN

    STATIC METHOD InitializeToolbox() AS VOID
        IF VOWEDControl.oToolBox == NULL
	        VOWEDControl.oToolBox := ToolBox{}
	        VOWEDControl.CreateToolWindow( Resources.ToolboxCaption, Point{950 , 150} , Size{200 , 600} , oToolBox)
	    ENDIF
    RETURN

    STATIC METHOD CreateToolWindow(cCaption AS STRING , oPos AS Point , oSize AS Size , oPanel AS Panel) AS VOID
	    LOCAL oForm AS Form
	    oForm := Form{}
	    oForm:Text := cCaption
    	oForm:ShowInTaskbar := FALSE
	    oForm:StartPosition := FormStartPosition.Manual
	    oForm:Location := oPos
	    oForm:Size := oSize
	    oForm:TopMost := TRUE
	    oForm:FormBorderStyle := FormBorderStyle.SizableToolWindow
	    oForm:Controls:Add(oPanel)
    	oForm:Closing += System.ComponentModel.CancelEventHandler{ NULL , @ToolWindowClosing() }
    RETURN 
    
	STATIC METHOD ToolWindowClosing(o AS OBJECT , e AS System.ComponentModel.CancelEventArgs) AS VOID
	    e:Cancel := TRUE
	    ((Form)o):Hide()
    RETURN

END CLASS
