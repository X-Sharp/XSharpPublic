USING System.IO
USING System.Text
USING System.Drawing
USING System.Collections.Generic

STATIC CLASS WinFormsConverter
	STATIC METHOD Convert(oProject AS VOProjectDescriptor, cAppFolder AS STRING) AS VOID
		LOCAL aWeds AS STRING[]

		aWeds := Directory.GetFiles(cAppFolder + "\tmp" , "*.wed")
		Directory.CreateDirectory(cAppFolder)

		LOCAL oApp AS ApplicationDescriptor
		oApp := oProject:AddApplication("Windows.Forms")
		oApp:SetWinForms()
		oApp:GACReferences:Add("System.Drawing")
		oApp:GACReferences:Add("System.Windows.Forms")
		
		FOREACH cWed AS STRING IN aWeds
			LOCAL aLines AS STRING[]
			aLines := File.ReadAllLines(cWed , Encoding.Default)
			
			LOCAL oCurrentWed AS WedDescriptor
			LOCAL oMainWed AS WedDescriptor
			oCurrentWed := WedDescriptor{}
			oMainWed := oCurrentWed
					
			FOR LOCAL nLine := 1 AS INT UPTO aLines:Length
				LOCAL cLine AS STRING
				cLine := aLines[nLine]:Trim()
				IF cLine:StartsWith("CONTROL=")
					oCurrentWed := WedDescriptor{oMainWed}
					oMainWed:Controls:Add(oCurrentWed)
				ENDIF
				IF cLine:IndexOf('=') != -1
					oCurrentWed:ReadProperty(cLine)
				END IF
			NEXT
			
			// Put control inside GroupBoxes
			AdjustContainers(oMainWed)
			
			LOCAL cPrg AS STRING
			LOCAL cFileName AS STRING
			cFileName := FileInfo{cWed}:Name:Replace(".wed","")
			cPrg := cAppFolder + "\" + cFileName + ".prg"

			LOCAL aCode, aClassCode, aConstrCode AS List<STRING>
			aClassCode := List<STRING>{}
			aConstrCode := List<STRING>{}
			aCode := List<STRING>{}

			oApp:AddModule(cFileName, "")

			LOCAL oWriter AS StreamWriter
			oWriter := StreamWriter{cPrg + ".wed" , FALSE , Encoding.Default}
			oWriter:WriteLine(String.Format("DESIGNERSTART = Form,Form,{0}" , oMainWed:Name ) )
			oWriter:WriteLine(String.Format("GUID={0}" , Guid.NewGuid():ToString()) )
			oWriter:WriteLine(String.Format("Name={0}" , oMainWed:Name) )
			oWriter:WriteLine(String.Format("Text={0}" , oMainWed:Caption) )
			oWriter:WriteLine(String.Format("Location={0},{1}" , oMainWed:Location:X , oMainWed:Location:Y) )
			oWriter:WriteLine(String.Format("ClientSize={0},{1}" , oMainWed:Size:Width , oMainWed:Size:Height) )
			oWriter:WriteLine(String.Format("") )
			
			aClassCode:Add( String.Format("PUBLIC CLASS {0} INHERIT System.Windows.Forms.Form", oMainWed:Name) )

			aConstrCode:Add( String.Format(e"CONSTRUCTOR()" ) )
			aConstrCode:Add( String.Format(e"\tSELF:InitializeComponent()" ) )
			aConstrCode:Add( String.Format(e"RETURN" ) )
			aConstrCode:Add( String.Format(e"" ) )
			aConstrCode:Add( String.Format(e"PRIVATE METHOD InitializeComponent() AS VOID" ) )

			aCode:Add( String.Format(e"\tSELF:Name := {0}", Quoted(oMainWed:Name)) )
			aCode:Add( String.Format(e"\tSELF:Text := {0}", Quoted(oMainWed:Name)) )
			aCode:Add( String.Format(e"\tSELF:ClientSize := System.Drawing.Size{{{0},{1}}}", oMainWed:Size:Width , oMainWed:Size:Height) )
			aCode:Add( String.Format(e"") )

			oWriter:WriteLine(String.Format("SUBCONTROLSSTART") )

			FOREACH oControl AS WedDescriptor IN oMainWed:Controls
				WriteControl(oControl, "SELF", oWriter, aClassCode, aConstrCode, aCode)
			NEXT

			oWriter:WriteLine(String.Format("SUBCONTROLSEND") )

			oWriter:WriteLine(String.Format("") )
			oWriter:WriteLine(String.Format("DESIGNEREND={0}" , oMainWed:Name) )
			oWriter:Close()

			aCode:Add( String.Format(e"RETURN" ) )
			aCode:Add( String.Format(e"" ) )
			aCode:Add( String.Format("END CLASS") )


			oWriter := StreamWriter{cPrg , FALSE , Encoding.Default}
			FOREACH cLine AS STRING IN aClassCode
				oWriter:WriteLine(cLine)
			NEXT
			FOREACH cLine AS STRING IN aConstrCode
				oWriter:WriteLine(cLine)
			NEXT
			FOREACH cLine AS STRING IN aCode
				oWriter:WriteLine(cLine)
			NEXT
			oWriter:Close()
			
		NEXT
		
		oApp:CreateAppFile(cAppFolder , TRUE)
		oApp:CreateAppFile(cAppFolder , FALSE)
		
		SafeDirectoryDelete(cAppFolder + "\tmp")
	RETURN

	INTERNAL STATIC METHOD WriteControl(oControl AS WedDescriptor, cParent AS STRING, oWriter AS StreamWriter, aClassCode AS List<STRING>, aConstrCode AS List<STRING>, aCode AS List<STRING>) AS VOID
		oWriter:WriteLine(String.Format("CONTROL={0}" , oControl:Type) )
		oWriter:WriteLine(String.Format("GUID={0}" , Guid.NewGuid():ToString()) )
		oWriter:WriteLine(String.Format("Name={0}" , oControl:Name) )
		oWriter:WriteLine(String.Format("Text={0}" , oControl:Caption) )
		oWriter:WriteLine(String.Format("Location={0},{1}" , oControl:Location:X , oControl:Location:Y) )
		oWriter:WriteLine(String.Format("Size={0},{1}" , oControl:Size:Width , oControl:Size:Height) )
		oWriter:WriteLine(String.Format("TabIndex={0}" , oControl:Order) )

		aClassCode:Add(String.Format(e"\tPRIVATE {0} AS {1}" , oControl:Name, oControl:Type) )

		aConstrCode:Add(String.Format(e"\tSELF:{0} := {1}{{}}" , oControl:Name, oControl:Type) )

		aCode:Add(String.Format(e"\tSELF:{0}:Name := {1}" , oControl:Name, Quoted(oControl:Name)) )
		aCode:Add(String.Format(e"\tSELF:{0}:Text := {1}" , oControl:Name, Quoted(oControl:Caption)) )
		aCode:Add(String.Format(e"\tSELF:{0}:Location := System.Drawing.Point{{{1},{2}}}" , oControl:Name, oControl:Location:X, oControl:Location:Y) )
		aCode:Add(String.Format(e"\tSELF:{0}:Size := System.Drawing.Size{{{1},{2}}}" , oControl:Name, oControl:Size:Width, oControl:Size:Height) )
		aCode:Add(String.Format(e"\t{1}:Controls:Add(SELF:{0})" , oControl:Name, cParent) )
		aCode:Add(String.Format(e"") )
		
		IF oControl:Controls:Count != 0
			oWriter:WriteLine("SUBCONTROLSSTART")
			FOREACH oChild AS WedDescriptor IN oControl:Controls
				WriteControl(oChild, "SELF:" + oControl:Name, oWriter, aClassCode, aConstrCode, aCode)
			NEXT
			oWriter:WriteLine("SUBCONTROLSEND")
		END IF

		SWITCH oControl:VOType
			CASE "FIXEDTEXT"
				NOP
			CASE "RADIOBUTTON"
				oWriter:WriteLine(String.Format("AutoSize=TRUE") )
				aCode:Add(String.Format(e"\tSELF:{0}:AutoSize := true" , oControl:Name) )
			CASE "MULTILINEEDIT"
				oWriter:WriteLine(String.Format("Multiline=TRUE") )
				aCode:Add(String.Format(e"\tSELF:{0}:Multiline := true" , oControl:Name) )
		END SWITCH
	RETURN


	INTERNAL STATIC METHOD AdjustContainers(oWindow AS WedDescriptor) AS WedDescriptor
		LOCAL aContainers AS List<WedDescriptor>
		aContainers := List<WedDescriptor>{}
		FOREACH oControl AS WedDescriptor IN oWindow:Controls
			IF oControl:IsContainer
				aContainers:Add(oControl)
			END IF
		NEXT
		
		LOCAL nControl := 0 AS INT
		DO WHILE nControl < oWindow:Controls:Count
			LOCAL oControl AS WedDescriptor
			LOCAL oCurrent AS WedDescriptor
			oControl := oWindow:Controls[nControl]
			oCurrent := NULL
			FOREACH oContainer AS WedDescriptor IN aContainers
				IF oContainer == oControl
					LOOP
				END IF
				LOCAL oRectangle AS Rectangle
				oRectangle := Rectangle{oContainer:AbsoluteLocation , oContainer:Size}
				IF oRectangle:Contains(oControl:Location)
					IF oCurrent == NULL .or. ;
								(oContainer:AbsoluteLocation:X > oCurrent:AbsoluteLocation:X .and. ;
								oContainer:AbsoluteLocation:Y > oCurrent:AbsoluteLocation:Y)
						oCurrent := oContainer
					END IF
				END IF
			NEXT
			IF oCurrent != NULL
				oControl:SetParent(oCurrent)
			ELSE
				nControl ++
			END IF
		END DO
		
	RETURN NULL
	
//	INTERNAL STATIC METHOD GetAllContainers(oContainer AS WedDescriptor, aContainers as List<WedDescriptor>) AS VOID
	
	STATIC METHOD Quoted(cText AS STRING) AS STRING
	RETURN e"\"" + cText + e"\""
	
END CLASS

INTERNAL CLASS WedDescriptor
	PROTECT _cType AS STRING
	PROTECT _aControls AS List<WedDescriptor>
	PROTECT _oParent AS WedDescriptor
	PROTECT _lIsContainer AS LOGIC

	CONSTRUCTOR()
		SUPER()
		SELF:_cType := "System.Windows.Forms.Panel"
		SELF:_aControls := List<WedDescriptor>{}
		SELF:_lIsContainer := FALSE
	RETURN
	CONSTRUCTOR(oParent AS WedDescriptor)
		SELF()
		SELF:_oParent := oParent
	RETURN

	PROPERTY Name AS STRING AUTO
	PROPERTY Caption AS STRING AUTO
	PROPERTY Order AS INT AUTO
	PROPERTY Location AS Point AUTO
	PROPERTY Size AS Size AUTO
	PROPERTY VOType AS STRING AUTO
	PROPERTY Type AS STRING GET _cType
	PROPERTY IsContainer AS LOGIC GET SELF:_lIsContainer
	PROPERTY Controls AS List<WedDescriptor> GET SELF:_aControls
	PROPERTY AbsoluteLocation AS Point
		GET
			IF SELF:_oParent == NULL
				RETURN Point{0,0}
			ELSE
				RETURN Point{SELF:_oParent:AbsoluteLocation:X + SELF:Location:X , SELF:_oParent:AbsoluteLocation:Y + SELF:Location:Y}
			END IF
		END GET
	END PROPERTY
		
	METHOD SetParent(oParent AS WedDescriptor) AS VOID
		IF SELF:_oParent != NULL
			SELF:_oParent:Controls:Remove(SELF)
		END IF
		SELF:_oParent := oParent
		SELF:_oParent:Controls:Add(SELF)
		SELF:Location := Point{SELF:Location:X - SELF:_oParent:AbsoluteLocation:X , SELF:Location:Y - SELF:_oParent:AbsoluteLocation:Y}
	RETURN
	
	METHOD ReadProperty(cLine AS STRING) AS VOID
		LOCAL cProp AS STRING
		LOCAL cValue AS STRING
		LOCAL nAt AS INT
		nAt := cLine:IndexOf('=')
		cProp := cLine:Substring(0,nAt):Trim():ToUpperInvariant()
		cValue := cLine:Substring(nAt+1):Trim()
		SWITCH cProp
			CASE "NAME"
				SELF:Name := cValue
			CASE "CAPTION"
				SELF:Caption := cValue
			CASE "ORDER"
				SELF:Order := Int32.Parse(cValue)
			CASE "LEFT"
				SELF:Location := Point{Int32.Parse(cValue) , SELF:Location:Y}
			CASE "TOP"
				SELF:Location := Point{SELF:Location:X , Int32.Parse(cValue)}
			CASE "WIDTH"
				SELF:Size := Size{Int32.Parse(cValue) , SELF:Size:Height}
			CASE "HEIGHT"
				SELF:Size := Size{SELF:Size:Width , Int32.Parse(cValue)}
			CASE "CONTROL"
				SELF:VOType := cValue:ToUpperInvariant()
				SWITCH cValue:ToUpperInvariant()
					CASE "FIXEDTEXT"
						SELF:_cType := "System.Windows.Forms.Label"
					CASE "PUSHBUTTON"
						SELF:_cType := "System.Windows.Forms.Button"
					CASE "SINGLELINEEDIT"
					CASE "MULTILINEEDIT"
					CASE "IPADDRESS"
					CASE "RICHEDIT"
					CASE "HOTKEYEDIT"
						SELF:_cType := "System.Windows.Forms.TextBox"
					CASE "CHECKBOX"
						SELF:_cType := "System.Windows.Forms.CheckBox"
					CASE "RADIOBUTTON"
						SELF:_cType := "System.Windows.Forms.RadioButton"
					CASE "GROUPBOX"
					CASE "RADIOBUTTONGROUP"
						SELF:_cType := "System.Windows.Forms.GroupBox"
						SELF:_lIsContainer := TRUE
					CASE "COMBOBOX"
					CASE "COMBOBOXEX"
						SELF:_cType := "System.Windows.Forms.ComboBox"
					CASE "LISTBOX"
						SELF:_cType := "System.Windows.Forms.ListBox"
					CASE "LISTVIEW"
					CASE "DATALISTVIEW"
						SELF:_cType := "System.Windows.Forms.ListView"
					CASE "TREEVIEW"
						SELF:_cType := "System.Windows.Forms.TreeView"
					CASE "TABCONTROL"
						SELF:_cType := "System.Windows.Forms.TabControl"
					CASE "PROGRESSBAR"
						SELF:_cType := "System.Windows.Forms.ProgressBar"
					CASE "HORIZONTALSPINNER"
					CASE "VERTICALSPINNER"
					CASE "HORIZONTALSLIDER"
					CASE "VERTICALSLIDER"
						SELF:_cType := "System.Windows.Forms.TrackBar"
					CASE "VERTSCROLL"
						SELF:_cType := "System.Windows.Forms.HScrollBar"
					CASE "HORZSCROLL"
						SELF:_cType := "System.Windows.Forms.VScrollBar"
					CASE "SUBDATAWINDOW"
						SELF:_cType := "System.Windows.Forms.DataGridView"
				END SWITCH
		END SWITCH
	RETURN
END CLASS
