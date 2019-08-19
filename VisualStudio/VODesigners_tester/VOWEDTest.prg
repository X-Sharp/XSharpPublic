#USING System.Collections.Generic
#USING System.Collections
#USING System.Windows.Forms
#USING System.Drawing
#USING System.Text
#USING System.IO
#using VODesigners
[STAThread];
FUNCTION Start() AS VOID
	LOCAL oOptions AS WindowDesignerOptions
	LOCAL oEditor AS XSharp_VOWindowEditor
	LOCAL oSurface AS Panel
	LOCAL oForm AS Form
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

	oGrid := DesignerGrid{}
	oToolBox := ToolBox{}

	VOWindowEditorTemplate.Load("C:\Users\Cpc\Documents\Default Project\DesignersOnly")
	oEditor := XSharp_VOWindowEditor{oSurface , oOptions , oGrid , oToolBox}
	oEditor:StandAlone := TRUE
	oEditor:Open("C:\Users\Cpc\Documents\Default Project\DesignersOnly\Help About.HELPABOUT.xsfrm")
/*	LOCAL oCode AS CodeContents
	oCode := oEditor:GetCodeContents()
	oCode*/
	
//	oEditor:CreateNewWindow("DIALOGWINDOW" , "Window1")
	
	oForm := Form{}
	oForm:StartPosition := FormStartPosition.Manual
	oForm:Location := Point{100 , 100}
	oForm:Size := Size{800 , 600}
	oForm:Controls:Add(oSurface)
	oForm:Text := "VOWED"
	oForm:Show()
	CreateToolWindow("Properties" , oForm , Point{550 , 150} , Size{300 , 500} , oGrid)
	CreateToolWindow("Toolbox" , oForm , Point{950 , 150} , Size{200 , 600} , oToolBox)
//	oEditor:CreateNewWindow("DATAWINDOW" , "Window1")
	oForm:Focus()
	oToolBox:SelectPointer()

	Application.Run(oForm)
RETURN

CLASS XSharp_VOWindowEditor INHERIT VOWindowEditor
	CONSTRUCTOR(_oSurface AS Control , _oOptions AS WindowDesignerOptions , _oGrid AS DesignerGrid , _oToolBox AS ToolBox)
		SUPER(_oSurface , _oOptions , _oGrid , _oToolBox)
	RETURN
	
	METHOD Save(cFileName AS STRING , lVnfrmOnly AS LOGIC) AS LOGIC
		LOCAL oPrgStream , oVhStream  , oRCStream AS EditorStream
		LOCAL oVNFrmStream AS FileStream
		LOCAL oCode AS CodeContents
		LOCAL lRcInSameFolder AS LOGIC
		LOCAL cVhName AS STRING
		LOCAL lSuccess AS LOGIC

		IF SELF:lReadOnly
			RETURN FALSE
		ENDIF
		
		lVnfrmOnly := lVnfrmOnly .or. SELF:lStandalone
		IF cFileName:ToUpper():Contains("~AUTORECOVER")
			lVnfrmOnly := TRUE // in case it isn't already
		END IF

		SELF:ArrangeColumnsOrder(TRUE)
		SELF:ArrangeControlOrder()
		
		oPrgStream := EditorStream{}
		oVhStream := EditorStream{}
		oRcStream := EditorStream{}
		
		oCode := SELF:GetCodeContents()
		IF SELF:GetSaveFileStreams(cFileName , oVNFrmStream , oRCStream , oPrgStream , oVhStream , cVhName , lVnfrmOnly , lRcInSameFolder)
//			SELF:SaveVNfrm(oVNFrmStream)
			SELF:SaveToXml(oVNFrmStream)
			IF !lVnfrmOnly
				SELF:SaveRC(oRCStream , oCode , cVhName , .not. oVhStream:IsValid , lRcInSameFolder)
				SELF:SavePrg(oPrgStream , oCode , .not. oVhStream:IsValid)
				IF oVhStream:IsValid
					SELF:SaveVh(oVhStream , oCode)
				END IF
			END IF
			lSuccess := TRUE
			IF !lVnfrmOnly
				SELF:nActionSaved := SELF:nAction
			END IF
		ENDIF
	RETURN lSuccess

END CLASS

/*CLASS XSharp_EditorStream INHERIT EditorStream
	PROTECT oXSharpVS_FileRepresentation AS OBJECT
	PROTECT cDiskFile AS STRING
	CONSTRUCTOR()
		SUPER()
	RETURN
	VIRTUAL METHOD Load(cFileName AS STRING) AS VOID
		LOCAL aLines AS List<STRING>

		SELF:oXSharpVS_FileRepresentation := NULL
		IF SELF:oXSharpVS_FileRepresentation != NULL
			aLines := List<STRING>{}
//			aLines := Funcs.BufferToLines(SELF:oXSharpVS_FileRepresentation:SourceCode)
			SELF:eType := EditorStreamType.Module
		ELSE
			SELF:cDiskFile := cFileName
			LOCAL oReader AS StreamReader
			oReader := StreamReader{SELF:cDiskFile , TRUE}
			SELF:oEncoding := oReader:CurrentEncoding
			aLines := List<STRING>{}
			DO WHILE oReader:Peek() != -1
				aLines:Add(oReader:ReadLine())
			END DO
			oReader:Close()
			oReader:Dispose()
			SELF:eType := EditorStreamType.File
		END IF

		SELF:oEditor := CodeEditor{aLines}
	RETURN

	METHOD Save() AS LOGIC
		IF SELF:eType == EditorStreamType.Module
			// save code contents to the open file buffer
			// contents can be obtained with:
			LOCAL oEnumerator AS IEnumerator
			oEnumerator := SELF:oEditor:GetEnumerator()
			DO WHILE oEnumerator:MoveNext()
				LOCAL cLine AS STRING
				cLine := (STRING)oEnumerator:Current
				// add line...
			END DO
		ELSE
//			File.WriteAllLines(SELF:cDiskFile , SELF:oEditor , SELF:oEncoding)
		END IF
	RETURN TRUE

END CLASS*/

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

