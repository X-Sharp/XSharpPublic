#using System.Windows.Forms
#using System.Drawing
#using System.IO

DELEGATE StatusBarMessageDelegate(cMessage AS STRING) AS VOID


CLASS VOWEDControl INHERIT Panel
    PROTECT oWed AS VOWindowEditor
    PROTECT oMed AS VOMenuEditor
    PROTECT oFed AS VOFieldSpecEditor
    PROTECT oDed AS VODBServerEditor
    PROTECT oEditor AS DesignerBase
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

    METHOD OpenWindow(cFileName AS STRING) AS LOGIC
        LOCAL oFileInfo AS FileInfo
        oFileInfo := FileInfo{cFileName}
//        IF !VOWindowEditorTemplate.Load(oFileInfo:Directory:Parent:FullName)
        IF !VOWindowEditorTemplate.Load(oFileInfo:Directory:FullName)
            RETURN FALSE
        ENDIF
        VOWEDControl.InitializeGrid()
        VOWEDControl.InitializeToolbox()
        VOWEDControl.ToolBox:SelectPointer()
        
        SELF:oOptions:oGridSize := Size{VOWindowEditorTemplate.GridX , VOWindowEditorTemplate.GridY}
        
/*        IF TRUE .and. cFileName:ToUpper():Contains("MENU")
        	SELF:oMed := VOMenuEditor{SELF , VOWEDControl.Grid}
        	SELF:oEditor := SELF:oMed
        	SELF:oMed:OpenVNmnu(cFileName)
            RETURN TRUE
        ENDIF*/
        
    	SELF:oWed := VOWindowEditor{SELF , SELF:oOptions , VOWEDControl.Grid , VOWEDControl.ToolBox}
    	SELF:oEditor := SELF:oWed
        IF .not. SELF:oWed:Open(cFileName)
        	RETURN FALSE
        ENDIF
        SELF:oWed:IsDirtyChanged := SELF:oIsDirtyChangedHandler
        SELF:oWed:TriggerSave := SELF:oTriggerSaveHandler
        SELF:oWed:StatusBarMessage := SELF:oStatusBarMessage
    RETURN TRUE

    METHOD OpenMenu(cFileName AS STRING) AS LOGIC

/*		IF TRUE .and. cFileName:ToUpper():Contains("FIELDSPEC")
            RETURN SELF:OpenFieldSpec(cFileName)
        ENDIF*/
		
//		MessageBox.Show(cFileName:ToUpper())
/*		IF TRUE .and. cFileName:ToUpper():Contains("SERVER.CONTROLS")
            RETURN SELF:OpenDBServer(cFileName)
        ENDIF*/

/*		LOCAL oFileInfo AS FileInfo
		oFileInfo := FileInfo{cFileName}
		IF !VOWindowEditorTemplate.Load(oFileInfo:Directory:FullName)
		    RETURN FALSE
		ENDIF*/
		VOWEDControl.InitializeGrid()
//		VOWEDControl.ToolBox:SelectPointer()
		
//		SELF:oOptions:oGridSize := Size{VOWindowEditorTemplate.GridX , VOWindowEditorTemplate.GridY}
		
		SELF:oMed := VOMenuEditor{SELF , VOWEDControl.Grid}
		SELF:oEditor := SELF:oMed
		IF .not. SELF:oMed:Open(cFileName)
			RETURN FALSE
		ENDIF
		SELF:oMed:IsDirtyChanged := SELF:oIsDirtyChangedHandler
		SELF:oMed:TriggerSave := SELF:oTriggerSaveHandler
    RETURN TRUE

    METHOD OpenFieldSpec(cFileName AS STRING) AS LOGIC
/*		LOCAL oFileInfo AS FileInfo
		oFileInfo := FileInfo{cFileName}
		IF !VOWindowEditorTemplate.Load(oFileInfo:Directory:FullName)
		    RETURN FALSE
		ENDIF*/
		VOWEDControl.InitializeGrid()
//		VOWEDControl.ToolBox:SelectPointer()
		
//		SELF:oOptions:oGridSize := Size{VOWindowEditorTemplate.GridX , VOWindowEditorTemplate.GridY}
		
		SELF:oFed := VOFieldSpecEditor{SELF , VOWEDControl.Grid}
		SELF:oEditor := SELF:oFed
		IF .not. SELF:oFed:Open(cFileName)
			RETURN FALSE
		ENDIF
		SELF:oFed:IsDirtyChanged := SELF:oIsDirtyChangedHandler
		SELF:oFed:TriggerSave := SELF:oTriggerSaveHandler
    RETURN TRUE

    METHOD OpenDBServer(cFileName AS STRING) AS LOGIC

		VOWEDControl.InitializeGrid()
		
		SELF:oDed := VODBServerEditor{SELF , VOWEDControl.Grid}
		SELF:oEditor := SELF:oDed
		IF .not. SELF:oDed:Open(cFileName)
			RETURN FALSE
		ENDIF
		SELF:oDed:IsDirtyChanged := SELF:oIsDirtyChangedHandler
		SELF:oDed:TriggerSave := SELF:oTriggerSaveHandler
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
        IF SELF:oWed != NULL
            SELF:oWed:ToggleGrid()
        ENDIF
    RETURN

    METHOD ShowTabOrder() AS VOID
        IF SELF:oWed != NULL
            SELF:oWed:ShowTabOrder()
        ENDIF
    RETURN

	VIRTUAL METHOD GiveFocus() AS VOID
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
        IF SELF:oWed != NULL
            SELF:oWed:TestForm()
        ENDIF
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
	    //VOWEDControl.oGrid := DesignerGrid{}
	    //VOWEDControl.oToolBox := ToolBox{}
	    //VOWEDControl.CreateToolWindow( Resources.PropertiesCaption, Point{550 , 150} , Size{300 , 500} , oGrid)
	    //VOWEDControl.CreateToolWindow( Resources.ToolboxCaption, Point{950 , 150} , Size{200 , 600} , oToolBox)
    RETURN
    STATIC METHOD InitializeGrid() AS VOID
        IF VOWEDControl.oGrid == NULL
	        VOWEDControl.oGrid := DesignerGrid{}
	        VOWEDControl.CreateToolWindow( Resources.PropertiesCaption, Point{550 , 150} , Size{300 , 500} , oGrid,"Properties")
	    ENDIF
    RETURN
    STATIC METHOD InitializeToolbox() AS VOID
        IF VOWEDControl.oToolBox == NULL
	        VOWEDControl.oToolBox := ToolBox{}
	        VOWEDControl.CreateToolWindow( Resources.ToolboxCaption, Point{950 , 150} , Size{200 , 600} , oToolBox,"ToolBox")
	    ENDIF
    RETURN

    STATIC METHOD CreateToolWindow(cCaption AS STRING , oPos AS Point , oSize AS Size , oPanel AS Panel,cName as STRING) AS VOID
	    LOCAL oForm AS Form
	    oForm := Form{}
       oForm:Tag := cName
       RestoreLocation(cName, ref oSize, ref oPos)
	    oForm:Text := cCaption
    	 oForm:ShowInTaskbar := FALSE
	    oForm:StartPosition := FormStartPosition.Manual
	    oForm:Location := oPos
	    oForm:Size := oSize
	    oForm:TopMost := TRUE
	    oForm:FormBorderStyle := FormBorderStyle.SizableToolWindow
	    oForm:Controls:Add(oPanel)
    	 oForm:Closing += System.ComponentModel.CancelEventHandler{ NULL , @ToolWindowClosing() }
       oForm:Move    += ToolWindowmoving
       oForm:SizeChanged += ToolWindowSizeChanged
    RETURN 
    STATIC METHOD ToolWindowClosing(o AS OBJECT , e AS System.ComponentModel.CancelEventArgs) AS VOID
	    e:Cancel := TRUE
	    ((Form)o):Hide()
    RETURN
    STATIC METHOD ToolWindowmoving(o AS OBJECT , e AS EventArgs) AS VOID
         LOCAL form := (Form) o as Form
         var size := form:Size
         var pos  := form:Location
         SaveLocation((string) form:Tag, size, pos)
    RETURN
    STATIC METHOD ToolWindowSizeChanged(o AS OBJECT , e AS EventArgs) AS VOID
         var form := (Form) o
         var size := form:Size
         var pos  := form:Location
         SaveLocation((string) form:Tag, size, pos)
    RETURN
    STATIC METHOD SaveLocation(name as STRING, size as System.Drawing.Size, point as System.Drawing.Point) as void
         var keyName := "VOED_"+name
         var key     := Microsoft.Win32.Registry.CurrentUser
         var subkey  := key:OpenSubKey(Constants.RegistryKey, true)
         if (subkey == null)
            subkey := key:CreateSubKey(Constants.RegistryKey)
         endif
         subkey:SetValue(keyName+"_W", size:Width)
         subkey:SetValue(keyName+"_H", size:Height)
         subkey:SetValue(keyName+"_X", point:X)
         subkey:SetValue(keyName+"_Y", point:Y)
         subkey:Close()

    STATIC METHOD RestoreLocation(name as STRING, size REF System.Drawing.Size, point REF System.Drawing.Point) as void
         LOCAL key     := Microsoft.Win32.Registry.CurrentUser as Microsoft.Win32.RegistryKey
         LOCAL subkey  := key:OpenSubKey(Constants.RegistryKey, true) as Microsoft.Win32.RegistryKey
         var keyName := "VOED_"+name
         if (subkey == null)
            subkey := key:CreateSubKey(Constants.RegistryKey)
         endif
         var w := subkey:GetValue(keyName+"_W")
         var h := subkey:GetValue(keyName+"_H")
         var x := subkey:GetValue(keyName+"_X")
         var y := subkey:GetValue(keyName+"_Y")
         if w == null .or. h == null .or. x == null .or. y == null
            SaveLocation(name, size, point)
            RETURN
         endif
         TRY
            // ensure visible
            var iX := (int) x
            var iY := (int) y
            var iW := (int) w
            var iH := (int) h
            var screens := Screen.AllScreens
            var ok      := false
            foreach oScreen as Screen in screens
               if iX >= oScreen:Bounds:Left .and. iX+iW <= oScreen:Bounds:Right .and. ;
                  iY >= oScreen:Bounds:Top  .and. iY+iH <= oScreen:Bounds:Bottom
                  ok := true
               endif
            next
            if ok
               size  := System.Drawing.Size{ iW, iH}
               point := System.Drawing.Point{ iX, iY}
            endif            
            
         CATCH e as Exception
            subkey:DeleteValue(keyName+"_W")
            subkey:DeleteValue(keyName+"_H")
            subkey:DeleteValue(keyName+"_X")
            subkey:DeleteValue(keyName+"_X")
         END TRY
         subkey:Close()
         RETURN
END CLASS

