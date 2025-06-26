USING System.Collections.Generic
USING System.Drawing
USING System.Text
USING System.Xml
USING System.IO

PARTIAL CLASS VOWindowEditor INHERIT WindowDesignerBase
	STATIC METHOD ConvertXsfrmToPrg(cXsfrm AS STRING, cClassName as STRING, cFormPrg AS STRING, cNameSpace AS STRING) AS LOGIC
		LOCAL oNode , oWindowNode AS XmlNode
		LOCAL oCurrentWed AS WedDescriptor
		LOCAL oMainWed AS WedDescriptor
		LOCAL oXml AS XmlDocument
		LOCAL oItem AS VOWEDItem

		oXml := XmlDocument{}
		TRY
			oXml:Load(cXsfrm)
		CATCH e AS Exception
			System.Windows.Forms.MessageBox.Show("Problem loading window definition file:" + CRLF + CRLF + e:ToString())
		END TRY

		oNode := oXml:FirstChild
		DO WHILE oNode != NULL
			IF oNode:Name:ToUpper() == "DESIGNERS"
				EXIT
			END IF
			oNode := oNode:NextSibling
		END DO
		IF oNode == NULL
			RETURN FALSE
		END IF

		oWindowNode := oNode:FirstChild
		DO WHILE oWindowNode != NULL
			IF oWindowNode:Name:ToUpper() == "VOWINDOW"
				EXIT
			END IF
			oWindowNode := oWindowNode:NextSibling
		END DO
		IF oWindowNode == NULL
			RETURN FALSE
		END IF

        oNode := oWindowNode:FirstChild
		oItem := OpenXmlItem(oNode)
		oCurrentWed := WedDescriptor{oItem, NULL}
		oMainWed := oCurrentWed

		oNode := oNode:NextSibling
		DO WHILE oNode != NULL
			oItem := OpenXmlItem(oNode)
			oCurrentWed := WedDescriptor{oItem, oMainWed}
			oMainWed:Controls:Add(oCurrentWed)
			oNode := oNode:NextSibling
		END DO

		// Put control inside GroupBoxes
		AdjustContainers(oMainWed)

		LOCAL aCode, aClassCode, aInitCode, aConstrCode AS List<STRING>
		LOCAL cClassDeclCode AS STRING
		aClassCode := List<STRING>{}
		aConstrCode := List<STRING>{}
		aInitCode := List<STRING>{}
		aCode := List<STRING>{}

		LOCAL cFileName AS STRING
//		LOCAL cNameSpace AS STRING
//		LOCAL cPrg AS STRING
//		cFileName := FileInfo{cWed}:Name:Replace(".wed","")
/*		cNameSpace := cFileName:Substring(0, cFileName:IndexOf('.'))
		foreach cChar as char in e"!@#$%^&*()_+~`-=[]{};:',./<>?|\\\" "
			cNameSpace := cNameSpace:Replace(cChar:ToString(), "")
		next*/

//		cPrg := cFolder + "\" + cFileName + ".prg"

		LOCAL oWriter := NULL AS StreamWriter

		cClassDeclCode := String.Format("PARTIAL CLASS {0} INHERIT System.Windows.Forms.Form", cClassName)

		aConstrCode:Add( String.Format(e"" ) )
		aConstrCode:Add( String.Format(e"CONSTRUCTOR()" ) )
		aConstrCode:Add( String.Format(e"\tSELF:InitializeComponent()" ) )
		aConstrCode:Add( String.Format(e"RETURN" ) )

		aInitCode:Add( String.Format(e"" ) )
		aInitCode:Add( String.Format(e"PRIVATE METHOD InitializeComponent() AS VOID" ) )
		aInitCode:Add( String.Format(e"" ) )

		aCode:Add( String.Format(e"\t", Quoted(oMainWed:Name)) )
		aCode:Add( String.Format(e"\tSELF:SuspendLayout()", Quoted(oMainWed:Name)) )
		aCode:Add( String.Format(e"\t", Quoted(oMainWed:Name)) )
		aCode:Add( String.Format(e"\tSELF:Name := {0}", Quoted(oMainWed:Name)) )
		aCode:Add( String.Format(e"\tSELF:Text := {0}", Quoted(oMainWed:Name)) )
		aCode:Add( String.Format(e"\tSELF:ClientSize := System.Drawing.Size{{{0},{1}}}", oMainWed:Size:Width , oMainWed:Size:Height) )
		aCode:Add( String.Format(e"") )

		FOREACH oControl AS WedDescriptor IN oMainWed:Controls
			WriteControl(oControl, "SELF", oWriter, aClassCode, aInitCode, aCode)
		NEXT

		aCode:Add( String.Format(e"\tSELF:ResumeLayout(false)" ) )
		aCode:Add( String.Format(e"\tSELF:PerformLayout()" ) )
		aCode:Add( String.Format(e"" ) )
		aCode:Add( String.Format(e"RETURN" ) )
		aCode:Add( String.Format(e"" ) )

		oWriter := StreamWriter{cFormPrg , FALSE , Encoding.UTF8}
		oWriter:WriteLine("BEGIN NAMESPACE " + cNameSpace)
		oWriter:WriteLine("")
		oWriter:WriteLine(cClassDeclCode)
		FOREACH cLine AS STRING IN aConstrCode
			oWriter:WriteLine(cLine)
		NEXT
		oWriter:WriteLine("END CLASS")
		oWriter:WriteLine("")
		oWriter:WriteLine("END NAMESPACE")
		oWriter:Close()

		LOCAL oFileInfo AS FileInfo
		oFileInfo := FileInfo{cFormPrg}
		cFileName := System.IO.Path.ChangeExtension(oFileInfo:DirectoryName + "\" + oFileInfo:Name ,".designer.prg")
		oWriter := StreamWriter{cFileName , FALSE , Encoding.UTF8}
		oWriter:WriteLine("BEGIN NAMESPACE " + cNameSpace)
		oWriter:WriteLine("")
		oWriter:WriteLine(cClassDeclCode)
		FOREACH cLine AS STRING IN aClassCode
			oWriter:WriteLine(cLine)
		NEXT
		FOREACH cLine AS STRING IN aInitCode
			oWriter:WriteLine(cLine)
		NEXT
		FOREACH cLine AS STRING IN aCode
			oWriter:WriteLine(cLine)
		NEXT
		oWriter:WriteLine("END CLASS")
		oWriter:WriteLine("")
		oWriter:WriteLine("END NAMESPACE")
		oWriter:Close()

	RETURN TRUE

	INTERNAL STATIC METHOD WriteControl(oControl AS WedDescriptor, cParent AS STRING, oWriter AS StreamWriter, aClassCode AS List<STRING>, aInitCode AS List<STRING>, aCode AS List<STRING>) AS VOID
		IF oWriter != NULL
			oWriter:WriteLine(String.Format("") )
			oWriter:WriteLine(String.Format("CONTROL={0}" , oControl:Type) )
			oWriter:WriteLine(String.Format("GUID={0}" , Guid.NewGuid():ToString()) )
			oWriter:WriteLine(String.Format("Name={0}" , oControl:Name) )
			oWriter:WriteLine(String.Format("Text={0}" , oControl:Caption) )
			oWriter:WriteLine(String.Format("Location={0},{1}" , oControl:Location:X , oControl:Location:Y) )
			oWriter:WriteLine(String.Format("Size={0},{1}" , oControl:Size:Width , oControl:Size:Height) )
			oWriter:WriteLine(String.Format("TabIndex={0}" , oControl:Order) )
			IF oControl:ForeColor != NULL
			oWriter:WriteLine(String.Format("ForeColor={0}" , oControl:ForeColor) )
			ENDIF
			IF oControl:BackColor != NULL
			oWriter:WriteLine(String.Format("BackColor={0}" , oControl:BackColor) )
			ENDIF
			IF oControl:Font != NULL
				LOCAL cFont AS STRING
				cFont := String.Format("{0},{1}" , oControl:Font[2] , oControl:Font[1])
				FOR LOCAL n := 3 AS INT UPTO oControl:Font:Length
					cFont += "," + oControl:Font[n]
				NEXT
			oWriter:WriteLine(String.Format("Font={0}" , cFont) )
			ENDIF
		END IF

		LOCAL cControlIVar AS STRING
		cControlIVar := oControl:Name
// 		IF FALSE
// 			cControlIVar := "o" + cControlIVar
// 		END IF

		aClassCode:Add(String.Format(e"\tPRIVATE {0} AS {1}" , cControlIVar, oControl:Type) )

		aInitCode:Add(String.Format(e"\tSELF:{0} := {1}{{}}" , cControlIVar, oControl:Type) )

		aCode:Add(String.Format(e"\tSELF:{0}:Name := {1}" , cControlIVar, Quoted(oControl:Name)) )
		aCode:Add(String.Format(e"\tSELF:{0}:Text := {1}" , cControlIVar, Quoted(oControl:Caption)) )
		aCode:Add(String.Format(e"\tSELF:{0}:Location := System.Drawing.Point{{{1},{2}}}" , cControlIVar, oControl:Location:X, oControl:Location:Y) )
		aCode:Add(String.Format(e"\tSELF:{0}:Size := System.Drawing.Size{{{1},{2}}}" , cControlIVar, oControl:Size:Width, oControl:Size:Height) )
		aCode:Add(String.Format(e"\tSELF:{0}:TabIndex := {1}" , cControlIVar, iif(oControl:Order >= 0 , oControl:Order, 0) ) )
		IF oControl:ForeColor != NULL
		aCode:Add(String.Format(e"\tSELF:{0}:ForeColor := {1}", cControlIVar, ColorToNetColor(oControl:ForeColor)) )
		END IF
		IF oControl:BackColor != NULL
		aCode:Add(String.Format(e"\tSELF:{0}:BackColor := {1}", cControlIVar, ColorToNetColor(oControl:BackColor)) )
		END IF
		IF oControl:Font != NULL
		aCode:Add(String.Format(e"\tSELF:{0}:Font := {1}", cControlIVar, FontToNetFont(oControl:Font)) )
		END IF
		aCode:Add(String.Format(e"\t{1}:Controls:Add(SELF:{0})" , cControlIVar, cParent) )
		aCode:Add(String.Format(e"") )

		IF oControl:Controls:Count != 0
			FOREACH oChild AS WedDescriptor IN oControl:Controls
				WriteControl(oChild, "SELF:" + cControlIVar, oWriter, aClassCode, aInitCode, aCode)
			NEXT
		END IF

		SWITCH oControl:VOType
			CASE "FIXEDTEXT"
				NOP
			CASE "RADIOBUTTON"
				aCode:Add(String.Format(e"\tSELF:{0}:AutoSize := true" , cControlIVar) )
			CASE "MULTILINEEDIT"
				aCode:Add(String.Format(e"\tSELF:{0}:Multiline := true" , cControlIVar) )
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
				IF oRectangle:Contains(oControl:Location) .and. ;
					oRectangle:Contains(Point{oControl:Location:X + oControl:Size:Width - 1, oControl:Location:Y + oControl:Size:Height - 1}) .and. ;
					.not. oContainer:Parent == oControl
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

	STATIC METHOD Quoted(cText AS STRING) AS STRING
	RETURN e"\"" + cText + e"\""
	STATIC METHOD ColorToNetColor(cColor AS STRING) AS STRING
		LOCAL cRet AS STRING
		cRet := "System.Drawing."
		IF cColor:StartsWith("Color.")
			cRet += cColor
		ELSE
			cRet += String.Format("Color.FromArgb({0})" , cColor)
		END IF
	RETURN cRet
	STATIC METHOD FontToNetFont(aFont AS STRING[]) AS STRING
		LOCAL cRet AS STRING
		LOCAL cStyles AS STRING
		cStyles := ""
		FOR LOCAL n := 3 AS INT UPTO aFont:Length
			IF n == 3
				cStyles += ", "
			ELSE
				cStyles += " | "
			END IF
			cStyles += "System.Drawing.FontStyle." + aFont[n]:Replace("Strikethru" ,"Strikeout")
		NEXT
		cRet := String.Format("System.Drawing.Font{{{0}, {1}{2}}}" , Quoted(aFont[2]), aFont[1], cStyles)
	RETURN cRet

END CLASS

INTERNAL CLASS WedDescriptor
	PROTECT _cType AS STRING
	PROTECT _aControls AS List<WedDescriptor>
	PROTECT _oParent AS WedDescriptor
	PROTECT _lIsContainer AS LOGIC

	CONSTRUCTOR(oWedItem AS VOWEDItem, oParent AS WedDescriptor)
		SELF:_oParent := oParent
		SELF:_cType := "System.Windows.Forms.Panel"
		SELF:_aControls := List<WedDescriptor>{}
		SELF:_lIsContainer := FALSE

		SELF:ReadProperty("Name" ,  oWedItem:cName)
		SELF:ReadProperty("Control" ,  oWedItem:cClass)
		SELF:ReadProperty("Caption" ,  oWedItem:cCaption)
		SELF:ReadProperty("Order" ,  oWedItem:nOrder:ToString())
		SELF:ReadProperty("Left" ,  oWedItem:nLeft:ToString())
		SELF:ReadProperty("Top" ,  oWedItem:nTop:ToString())
		SELF:ReadProperty("Width" ,  oWedItem:nWidth:ToString())
		SELF:ReadProperty("Height" ,  oWedItem:nHeight:ToString())
		FOREACH oPair AS NameValue IN oWedItem:aProperties
			SELF:ReadProperty(oPAir:Name ,  oPair:Value:ToString())
		NEXT
	RETURN

	PROPERTY Name AS STRING AUTO
	PROPERTY Caption AS STRING AUTO
	PROPERTY Order AS INT AUTO
	PROPERTY Location AS Point AUTO
	PROPERTY Size AS Size AUTO
	PROPERTY VOType AS STRING AUTO
	PROPERTY Type AS STRING GET _cType
	PROPERTY Font AS STRING[] AUTO
	PROPERTY ForeColor AS STRING AUTO
	PROPERTY BackColor AS STRING AUTO
	PROPERTY Parent AS WedDescriptor GET SELF:_oParent
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

	METHOD ReadProperty(cProp AS STRING, cValue AS STRING) AS VOID
		cProp := cProp:Trim():ToUpperInvariant()
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
                // Get the part after the last Colon
                cValue := cValue:Substring(cValue:LastIndexOf(':') + 1)
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
					CASE "TEXTEDIT"
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
					CASE "VERTICALSCROLLBAR"
						SELF:_cType := "System.Windows.Forms.VScrollBar"
					CASE "HORZSCROLL"
					CASE "HORIZONTALSCROLLBAR"
						SELF:_cType := "System.Windows.Forms.HScrollBar"
					CASE "FIXEDICON"
					CASE "FIXEDBITMAP"
						SELF:_cType := "System.Windows.Forms.PictureBox"
					CASE "DATETIMEPICKER"
						SELF:_cType := "System.Windows.Forms.DateTimePicker"
					CASE "CUSTOMCONTROL"
						SELF:_cType := "System.Windows.Forms.Panel"
					CASE "SUBDATAWINDOW"
					CASE "BBROWSER"
						SELF:_cType := "System.Windows.Forms.DataGridView"
				END SWITCH
			CASE "TEXTCOLOR"
				SELF:ForeColor := TranslateColor(cValue)
			CASE "BACKGROUND"
				SELF:BackColor := TranslateColor(cValue)
			CASE "NEWFONT"
				TRY
					LOCAL aFont AS STRING[]
					aFont := cValue:Split(<char>{':'})
					IF aFont:Length >= 2
						SELF:Font := aFont
					END IF
				CATCH
					NOP
				END TRY
		END SWITCH
	RETURN

	STATIC METHOD TranslateColor(cColor AS STRING) AS STRING
		LOCAL cRet := NULL AS STRING
		TRY
			IF cColor:StartsWith("COLOR")
				cColor := cColor:Substring(5)
				cColor := cColor:Substring(0,1):ToUpper() + cColor:Substring(1):ToLower()
				cRet := "Color." + cColor
			ELSE
				LOCAL aColors AS STRING[]
				aColors := cColor:Split(<char>{' '})
				IF aColors:Length == 3
					cRet := String.Format("{0}, {1}, {2}" , aColors[1], aColors[2], aColors[3])
				END IF
			END IF
		CATCH
			NOP
		END TRY
	RETURN cRet

END CLASS
