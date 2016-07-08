#using System.Windows.Forms
#using System.Drawing
//#using Vulcan.VOEditors

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

/*		LOCAL oModule AS Vulcan.CodeModel.Module
		LOCAL oVEditor AS EntityParser
		LOCAL n AS INT
		LOCAL lSpeed AS LOGIC
		LOCAL cFileName AS STRING
		LOCAL aLines AS List<STRING>
	
	IF TRUE
		lSpeed := TRUE
		lSpeed := FALSE

		IF lSpeed
			cFileName := "C:\cpc\test\pide.prg"
		ELSE
			cFileName := "C:\cpc\test\test.prg"
		END IF
//			cFileName := "C:\GrafX\Dev\Source\VSIntegration\VODesigners\VulcanEditor.prg"
			cFileName := "C:\DotNet\VONet\VOEdit\Prg\test.prg"
//			cFileName := "C:\Users\Cpc\Documents\Default Project\MDIApp\dbs.prg"
//			cFileName := "C:\DotNet\VONet\VOEdit\VIDE_Project\VIRES\PIR_ControlTemplates.prg"
		oModule := Vulcan.CodeModel.Module{cFileName , Vulcan.CodeModel.TypeList{}}
		
		oVEditor := EntityParser{}
		oVEditor:ParseModule(oModule)
		LOCAL d AS DateTime
//		wait
		d := DateTime.Now
		oVEditor:ParseModule(oModule)
		? DateTime.Now - d
//		? oVEditor:GetFirstEntity():cName
		IF .not. lSpeed
//			oVEditor:DisplayEntities()
			aLines := GetCode(oModule)
			FOR n := 0 UPTO aLines:Count - 1
				? aLines[n]
			NEXT
		END IF
		wait

		cFileName := "C:\Cpc\_EntityParserTest\sample.prg"
		oModule := Vulcan.CodeModel.Module{cFileName , Vulcan.CodeModel.TypeList{}}
		oVEditor := EntityParser{}
		oVEditor:ParseModule(oModule)
		aLines := GetCode(oModule)
		LOCAL oWriter AS System.IO.StreamWriter
		oWriter := System.IO.StreamWriter{"C:\Cpc\_EntityParserTest\output.txt" , FALSE , System.Text.Encoding.Default}
		FOR n := 0 UPTO aLines:Count - 1
			oWriter:WriteLine(aLines[n])
		NEXT
		oWriter:Close()
		
		IF CompareFiles("C:\Cpc\_EntityParserTest\output.txt" , "C:\Cpc\_EntityParserTest\output_ok.txt")
			? "OK!"
		ELSE
			? "*****  PROBLEM  *****"
		ENDIF

		RETURN
	END IF
*/	
	
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
	VOWindowEditorTemplate.Load("C:\Users\Cpc\Documents\Default Project1\DataAware\Properties")
//	VOWindowEditorTemplate.Load("C:\Users\Cpc\Documents\Default Project\MDIApp\")
//	VOWindowEditorTemplate.Load("C:\DotNet\VONet\VOEdit\Prg\VOPSers\a")

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

FUNCTION GetCode(oModule AS Vulcan.CodeModel.Module) AS List<STRING>
	LOCAL aLines AS List<STRING>
	LOCAL n,m,k AS INT
	aLines := List<STRING>{}
	FOR n := 0 UPTO oModule:TypeList:Count - 1
		LOCAL cLine AS STRING
		LOCAL oType AS Vulcan.CodeModel.TypeInfo
		LOCAL oMember AS Vulcan.CodeModel.Member
		oType := oModule:TypeList[n]
		cLine := oType:AccessType:ToString() + " " + oType:ElementType:ToString() + " " + oType:Name
		IF oType:@@Partial
			cLine := "PARTIAL " + cLine
		END IF
		IF oType:ElementType == Vulcan.CodeModel.ElementType.Enum .or. oType:ElementType == Vulcan.CodeModel.ElementType.Delegate
			cLine += " AS " + oType:DisplayName
		END IF
		cLine += " INHERIT " + oType:ParentName
		aLines:Add(cLine)
		FOR m := 0 UPTO oType:Members:Count - 1
			oMember := oType:Members[m]
			cLine := "    " + oMember:AccessType:ToString() + " " + oMember:ElementType:ToString() + " " + oMember:Name + " "
			IF oMember:Parameters:Count != 0
				cLine += "("
				FOR k := 0 UPTO oMember:Parameters:Count - 1
					IF k > 0
						cLine += " , "
					END IF
					cLine += oMember:Parameters[k]:Name + " AS " + oMember:Parameters[k]:TypeName
				NEXT
				cLine += ")"
			ENDIF
			cLine += " AS " + oMember:ReturnType
			aLines:Add(cLine)
		NEXT
		
	NEXT
RETURN aLines

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
//		SELF:oMEditor:OpenVNmnu("C:\Users\Cpc\Documents\Default Project_\MDIApp\Resources\Standard Menus.StandardShellMenu.vnmnu")
		LOCAL oDialog AS OpenFileDialog
		oDialog := OpenFileDialog{}
		oDialog:Filter := "Transported MED files (*.vnmnu)|*.vnmnu"
//		oDialog:Filter := "Transported MED files (*.vnmnu)|*.vnfrm"
//		oDialog:InitialDirectory := "C:\Users\Cpc\Documents\Default Project_\MDIApp\Resources"
		IF oDialog:ShowDialog() == DialogResult.OK
//			SELF:oMEditor:OpenVNmnu(oDialog:FileName)
			SELF:oMEditor:Open(oDialog:FileName)
		ENDIF
	RETURN
	METHOD MenuSaveMenu(o AS OBJECT,e AS EventArgs) AS VOID
//		SELF:oMEditor:SaveVNmnu("C:\Users\Cpc\Documents\Default Project_\MDIApp\Resources\_Saved.vnmnu")
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
		oDialog:Filter := "Transported FED files (*.vnfs)|*.vnfs"
		oDialog:InitialDirectory := "C:\Users\Cpc\Documents\Default Project\MDIApp"
		IF oDialog:ShowDialog() == DialogResult.OK
			SELF:oFSEditor:Open(oDialog:FileName)
		ENDIF
	RETURN

	METHOD MenuNewDBServer(o AS OBJECT,e AS EventArgs) AS VOID
		SELF:oDBEditor:New()
	RETURN
	METHOD MenuOpenDBServer(o AS OBJECT,e AS EventArgs) AS VOID
		LOCAL oDialog AS OpenFileDialog
		oDialog := OpenFileDialog{}
		oDialog:Filter := "Transported DBS files (*.vndbs)|*.vndbs"
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

