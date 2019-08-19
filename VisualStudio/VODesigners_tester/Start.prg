#using System.Windows.Forms
#using System.Drawing
#using XSharp.VODesigners
#using System.Collections.Generic


[STAThread];
FUNCTION Start() AS VOID
	LOCAL oOptions AS WindowDesignerOptions
	LOCAL oEditor AS VOWindowEditor
	LOCAL oMEditor AS VOMenuEditor
	LOCAL oFSEditor AS VOFieldSpecEditor
	LOCAL oDBEditor AS VODBServerEditor
	LOCAL oSurface AS Panel
	LOCAL oForm AS TestForm
	LOCAL oGrid AS DesignerGrid
	LOCAL oToolBox AS ToolBox
	
	Application.EnableVisualStyles()
	
	oOptions := WindowDesignerOptions{}
	oOptions:lUseGrid := FALSE
	oOptions:lShowGrid := FALSE
	
	oSurface := Panel{}
	oSurface:AutoScroll := TRUE
	oSurface:Dock := DockStyle.Fill
	oSurface:BackColor := Color.White

//	VOWindowEditorTemplate.Load("C:\")
//	VOWindowEditorTemplate.Load("C:\CAVO28\BIN")
//	VOWindowEditorTemplate.Load("C:\")
//	VOWindowEditorTemplate.Load("C:\Users\Cpc\Documents\Default Project1\DataAware\Properties")
//	VOWindowEditorTemplate.Load("C:\Users\Cpc\Documents\Default Project\MDIApp\")
	VOWindowEditorTemplate.Load("C:\")

	oGrid := DesignerGrid{}
	oToolBox := ToolBox{}

	oEditor := VOWindowEditor{oSurface , oOptions , oGrid , oToolBox}
	oEditor:StandAlone := TRUE
//	oEditor:CreateNewWindow("DIALOGWINDOW" , "Window1")
	
	oMEditor := VOMenuEditor{oSurface , oGrid}
	oMEditor:StandAlone := TRUE

	oFSEditor := VOFieldSpecEditor{oSurface , oGrid}
	oFSEditor:StandAlone := TRUE

	oDBEditor := VODBServerEditor{oSurface , oGrid}
	oDBEditor:StandAlone := TRUE

	oForm := TestForm{oEditor , oMEditor , oFSEditor , oDBEditor}
	oForm:StartPosition := FormStartPosition.Manual
	oForm:Location := Point{100 , 100}
	oForm:Size := Size{800 , 600}
	oForm:Controls:Add(oSurface)
	oForm:Text := "VOWED"
	oForm:Show()
	CreateToolWindow("Properties" , oForm , Point{550 , 150} , Size{300 , 500} , oGrid)
	CreateToolWindow("Toolbox" , oForm , Point{950 , 150} , Size{200 , 600} , oToolBox)
//	oEditor:CreateNewWindow("DATAWINDOW" , "Window1")
//	oEditor:CreateNewWindow("DIALOGWINDOW" , "Window1")
//	oEditor:CreateNewWindow("DATADIALOG" , "Window1")
	oForm:Focus()
	oToolBox:SelectPointer()

	Application.Run(oForm)
RETURN

FUNCTION CompareFiles(f1 AS STRING , f2 AS STRING) AS LOGIC
	IF System.IO.FileInfo{f1}:Length != System.IO.FileInfo{f2}:Length
		RETURN FALSE
	END IF
	LOCAL b1,b2 AS BYTE[]
	LOCAL n AS INT
	b1 := System.IO.File.ReadAllBytes(f1)
	b2 := System.IO.File.ReadAllBytes(f2)
	FOR n := 1 UPTO b1:Length
		IF b1[n] != b2[n]
			RETURN FALSE
		END IF
	NEXT
RETURN TRUE



CLASS TestForm INHERIT Form
	PROTECT oEditor AS VOWindowEditor
	PROTECT oMEditor AS VOMenuEditor
	PROTECT oFSEditor AS VOFieldSpecEditor
	PROTECT oDBEditor AS VODBServerEditor
	CONSTRUCTOR(_oEditor AS VOWindowEditor , _oMEditor AS VOMenuEditor , _oFSEditor AS VOFieldSpecEditor , _oDBEditor AS VODBServerEditor)
		SUPER()

		SELF:oEditor := _oEditor
		SELF:oMEditor := _oMEditor
		SELF:oFSEditor := _oFSEditor
		SELF:oDBEditor := _oDBEditor


		SELF:Menu := MainMenu{}
		SELF:Menu:MenuItems:Add("&Window")
		SELF:Menu:MenuItems[0]:MenuItems:Add("&Open" , EventHandler{ SELF , @MenuOpen() })
		SELF:Menu:MenuItems[0]:MenuItems:Add("&Save" , EventHandler{ SELF , @MenuSave() })
		SELF:Menu:MenuItems:Add("&Menu")
//		SELF:Menu:MenuItems[1]:MenuItems:Add("&New" , EventHandler{ SELF , @MenuNewMenu() })
		SELF:Menu:MenuItems[1]:MenuItems:Add("&Open" , EventHandler{ SELF , @MenuOpenMenu() })
		SELF:Menu:MenuItems[1]:MenuItems:Add("&Save" , EventHandler{ SELF , @MenuSaveMenu() })
		SELF:Menu:MenuItems[1]:MenuItems:Add("&Code" , EventHandler{ SELF , @MenuCodeMenu() })
		SELF:Menu:MenuItems:Add("&FieldSpec")
//		SELF:Menu:MenuItems[2]:MenuItems:Add("&New" , EventHandler{ SELF , @MenuNewMenu() })
		SELF:Menu:MenuItems[2]:MenuItems:Add("&Open" , EventHandler{ SELF , @MenuOpenFieldSpec() })
//		SELF:Menu:MenuItems[2]:MenuItems:Add("&Save" , EventHandler{ SELF , @MenuSaveMenu() })
		SELF:Menu:MenuItems[2]:MenuItems:Add("&Code" , EventHandler{ SELF , @MenuCodeFieldSpec() })
		SELF:Menu:MenuItems:Add("&DBServer")
//		SELF:Menu:MenuItems[3]:MenuItems:Add("&New" , EventHandler{ SELF , @MenuNewDBServer() })
		SELF:Menu:MenuItems[3]:MenuItems:Add("&Open" , EventHandler{ SELF , @MenuOpenDBServer() })
//		SELF:Menu:MenuItems[3]:MenuItems:Add("&Save" , EventHandler{ SELF , @MenuSaveDBServer() })
		SELF:Menu:MenuItems[3]:MenuItems:Add("&Code" , EventHandler{ SELF , @MenuCodeDBServer() })
		SELF:Menu:MenuItems:Add("&View")
		SELF:Menu:MenuItems[4]:MenuItems:Add("Toggle &Grid" , EventHandler{ SELF , @MenuToggleGrid() })
		SELF:Menu:MenuItems[4]:MenuItems:Add("&Tab Order" , EventHandler{ SELF , @MenuTabOrder() })
		SELF:Menu:MenuItems[4]:MenuItems:Add("-")
		SELF:Menu:MenuItems[4]:MenuItems:Add("&Code" , EventHandler{ SELF , @MenuCode() })
	RETURN
	METHOD MenuOpen(o AS OBJECT,e AS EventArgs) AS VOID
		SELF:oEditor:Open()
	RETURN
	METHOD MenuSave(o AS OBJECT,e AS EventArgs) AS VOID
		SELF:oEditor:Save()
	RETURN

	METHOD MenuNewMenu(o AS OBJECT,e AS EventArgs) AS VOID
//		SELF:oMEditor:New()
	RETURN
	METHOD MenuOpenMenu(o AS OBJECT,e AS EventArgs) AS VOID
//		SELF:oMEditor:OpenVNmnu("C:\Users\Cpc\Documents\Default Project_\MDIApp\Resources\Standard Menus.StandardShellMenu.xsmnu")
		LOCAL oDialog AS OpenFileDialog
		oDialog := OpenFileDialog{}
		oDialog:Filter := "Transported MED files (*.xsmnu)|*.xsmnu"
//		oDialog:InitialDirectory := "C:\Users\Cpc\Documents\Default Project_\MDIApp\Resources"
		IF oDialog:ShowDialog() == DialogResult.OK
//			SELF:oMEditor:OpenVNmnu(oDialog:FileName)
			SELF:oMEditor:Open(oDialog:FileName)
		ENDIF
	RETURN
	METHOD MenuSaveMenu(o AS OBJECT,e AS EventArgs) AS VOID
//		SELF:oMEditor:SaveVNmnu("C:\Users\Cpc\Documents\Default Project_\MDIApp\Resources\_Saved.xsmnu")
	RETURN
	METHOD MenuCodeMenu(o AS OBJECT,e AS EventArgs) AS VOID
		SELF:oMEditor:ShowCode()
	RETURN

	METHOD MenuCode(o AS OBJECT,e AS EventArgs) AS VOID
		SELF:oEditor:ShowCode()
	RETURN
	METHOD MenuTabOrder(o AS OBJECT,e AS EventArgs) AS VOID
		SELF:oEditor:ShowTabOrder()
	RETURN
	METHOD MenuToggleGrid(o AS OBJECT,e AS EventArgs) AS VOID
		SELF:oEditor:ToggleGrid()
	RETURN


	METHOD MenuCodeFieldSpec(o AS OBJECT,e AS EventArgs) AS VOID
		SELF:oFSEditor:ShowCode()
	RETURN
	METHOD MenuOpenFieldSpec(o AS OBJECT,e AS EventArgs) AS VOID
		LOCAL oDialog AS OpenFileDialog
		oDialog := OpenFileDialog{}
		oDialog:Filter := "Transported FED files (*.xsfs)|*.xsfs"
		oDialog:InitialDirectory := "C:\Users\Cpc\Documents\Default Project\MDIApp"
		IF oDialog:ShowDialog() == DialogResult.OK
			SELF:oFSEditor:Open(oDialog:FileName)
		ENDIF
	RETURN

	METHOD MenuNewDBServer(o AS OBJECT,e AS EventArgs) AS VOID
		SELF:oDBEditor:NEW()
	RETURN
	METHOD MenuOpenDBServer(o AS OBJECT,e AS EventArgs) AS VOID
		LOCAL oDialog AS OpenFileDialog
		oDialog := OpenFileDialog{}
		oDialog:Filter := "Transported DBS files (*.xsdbs)|*.xsdbs"
		oDialog:InitialDirectory := "C:\Users\Cpc\Documents\Default Project\DataAware1"
		IF oDialog:ShowDialog() == DialogResult.OK
			SELF:oDBEditor:Open(oDialog:FileName)
		ENDIF
	RETURN
	METHOD MenuCodeDBServer(o AS OBJECT,e AS EventArgs) AS VOID
		SELF:oDBEditor:ShowCode()
	RETURN
	
END CLASS

PROCEDURE CreateToolWindow(cCaption AS STRING , oParent AS Form , oPos AS Point , oSize AS Size , oPanel AS Panel)
	LOCAL oForm AS Form
	oForm := Form{}
	oForm:Text := cCaption
	oForm:ShowInTaskbar := FALSE
	oForm:Owner := oParent
	oForm:StartPosition := FormStartPosition.Manual
	oForm:Location := oPos
	oForm:Size := oSize
	oForm:FormBorderStyle := FormBorderStyle.SizableToolWindow
	oForm:Controls:Add(oPanel)
	oForm:Closing += System.ComponentModel.CancelEventHandler{ NULL , @ToolWindowClosing() }
	oForm:Show()
RETURN 

PROCEDURE ToolWindowClosing(o AS OBJECT , e AS System.ComponentModel.CancelEventArgs)
	e:Cancel := TRUE
RETURN

