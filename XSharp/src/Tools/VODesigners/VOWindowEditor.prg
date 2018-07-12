#using System.Windows.Forms
#using System.Drawing
#using System.Collections.Generic
#using System.Collections
#using System.IO

ENUM ViewMode
	MEMBER Auto
	MEMBER Form
	MEMBER Browse
END ENUM

PARTIAL CLASS VOWindowEditor INHERIT WindowDesignerBase
	PROTECT oWindowDesign AS DesignWindowItem
	PROTECT oWindow AS Control
	PROTECT aSelected AS ArrayList
	PROTECT aAffected AS List<STRING>
	
	PROTECT oActionStart AS Point
	PROTECT oActionEnd AS Point
	PROTECT lMoved AS LOGIC
	PROTECT nResizeGonia AS INT
	
	
	PROTECT oDummy AS Button

	PROTECT eCopyMode AS ViewMode
	PROTECT cDefaultFileName AS STRING
//	PROTECT oStatusBar AS StatusBar

	STATIC PROTECT oClipboard := DesignerClipboard{} AS DesignerClipboard
	STATIC ACCESS Clipboard AS DesignerClipboard
	RETURN oClipboard
	
	
	CONSTRUCTOR(_oSurface AS Control , _oOptions AS WindowDesignerOptions , _oGrid AS DesignerGrid , _oToolBox AS ToolBox)
		SUPER(_oSurface , _oOptions)
		
		SELF:oToolBox := _oToolBox
		SELF:oGrid := _oGrid
		SELF:oGrid:PropertyModified := PropertyUpdatedEventHandler{ SELF , @PropertyModifiedInGrid() }
		SELF:oGrid:ControlKeyPressed := ControlKeyPressedEventHandler{ SELF , @ControlKeyPressedInGrid() }
		SELF:oGrid:oActiveDesigner := SELF
		SELF:oGrid:UseHierarchy(TRUE)


		SELF:aSelected := ArrayList{}
		SELF:aAffected := List<STRING>{}
		SELF:aActions := ArrayList{}
		
		SELF:AddDummy()
		
		SELF:oTimer:Start()

	RETURN
		
	ACCESS IsDirty AS LOGIC
	RETURN SELF:nAction != SELF:nActionSaved

	ACCESS HasRichEdit AS LOGIC
		LOCAL aDesign AS ArrayList
		LOCAL oDesign AS DesignWindowItem
		LOCAL n AS INT
		aDesign := SELF:GetAllDesignItems()
		FOR n := 0 UPTO aDesign:Count - 1
			oDesign := (DesignWindowItem)aDesign[n]
			IF oDesign:cFullClass:IndexOf("CONTROL:TEXTCONTROL:EDIT:MULTILINEEDIT:RICHEDIT") == 0
				RETURN TRUE
			ENDIF
		NEXT
	RETURN FALSE

	METHOD CanDoAction(eAction AS DesignerActionType) AS LOGIC
		
		IF SELF:lReadOnly
			RETURN FALSE
		ENDIF
		
		SWITCH eAction
		case DesignerActionType.Cut 
        case DesignerActionType.Copy 
		case DesignerActionType.RemoveSelected
			RETURN !SELF:oWindowDesign:lSelected .and. SELF:aSelected:Count != 0
		CASE DesignerActionType.Paste
			RETURN Clipboard:Count != 0
		CASE DesignerActionType.Undo
			RETURN SELF:nAction >= 1
		CASE DesignerActionType.Redo
			RETURN SELF:nAction < SELF:aActions:Count
		CASE DesignerActionType.AlignLeft
			RETURN SELF:aSelected:Count >= 2
		CASE DesignerActionType.SpacingHorzEqual
			RETURN SELF:aSelected:Count >= 3
		END SWITCH
	RETURN FALSE
	
	PROTECTED METHOD AddDummy() AS VOID
		SELF:oDummy := Button{}
		SELF:oDummy:Size := Size{0,0}
		SELF:oDummy:KeyDown += KeyEventHandler{ SELF , @ControlKeyDown() }
		SELF:oDummy:PreviewKeyDown += PreviewKeyDownEventHandler{ SELF , @PreviewControlKeyDown() }
		SELF:oDummy:GotFocus += EventHandler{SELF , @DummyGotFocus() }
		SELF:oDummy:LostFocus += EventHandler{SELF , @DummyLostFocus() }
		SELF:oSurface:Controls:Add(SELF:oDummy)
	RETURN
    METHOD DummyGotFocus(o AS OBJECT , e AS EventArgs) AS VOID
    	SELF:ShowHideTools(TRUE)
    	IF SELF:oGrid:FindForm() != NULL
	    	SELF:oGrid:FindForm():Text := "Window Editor Properties" // TODO: need to use resource for that
    	END IF
		SELF:oGrid:PropertyModified := PropertyUpdatedEventHandler{ SELF , @PropertyModifiedInGrid() }
		SELF:oGrid:ControlKeyPressed := ControlKeyPressedEventHandler{ SELF , @ControlKeyPressedInGrid() }
		SELF:oGrid:oActiveDesigner := SELF
		SELF:oGrid:UseHierarchy(TRUE)
		SELF:oToolBox:SetViewMode(SELF:ViewMode)
    RETURN
    METHOD DummyLostFocus(o AS OBJECT , e AS EventArgs) AS VOID
    	SELF:ShowHideTools(FALSE)
    RETURN

	VIRTUAL METHOD GiveFocus() AS VOID
		SELF:oDummy:Focus()
	RETURN

	METHOD PrintStatus() AS VOID
		LOCAL oDesign AS DesignWindowItem
		LOCAL x,y,xx,yy,dx,dy AS INT
		LOCAL cText AS STRING
//		IF SELF:oStatusBar != NULL .and. .not. SELF:oStatusBar:IsDisposed
		IF SELF:StatusBarMessage != NULL
			TRY
				oDesign := (DesignWindowItem)SELF:aSelected[0]
				x := (INT)oDesign:GetProperty("_Left"):Value
				y := (INT)oDesign:GetProperty("_Top"):Value
				IF oDesign:IsForm
					x := 0
					y := 0
				END IF
				xx := (INT)oDesign:GetProperty("_Width"):Value
				yy := (INT)oDesign:GetProperty("_Height"):Value
				dx := SELF:oActionEnd:X - SELF:oActionStart:X
				dy := SELF:oActionEnd:Y - SELF:oActionStart:Y
				SWITCH eCurrent
				CASE WEDAction.Move
					x += dx
					y += dy
				CASE WEDAction.Resize
					LOCAL oRect AS Rectangle
					oRect := Rectangle{x,y,xx,yy}
					oRect := SELF:AdjustResizeRect(oRect , dx , dy , SELF:nResizeGonia)
					x := oRect:X
					y := oRect:Y
					xx := oRect:Width
					yy := oRect:Height
				END SWITCH
				cText := oDesign:Name
				cText += " : " + oDesign:cControl
				cText += "  (" + x:ToString() + " , " + y:ToString() + " ) "
				cText += ",  (" + xx:ToString() + " , " + yy:ToString() + " ) "
			CATCH
				cText := ""
			END TRY
//			SELF:oStatusBar:Text := ""
			SELF:StatusBarMessage:Invoke(cText)
		END IF
	RETURN
	
	METHOD CreateNewWindow(cType AS STRING , cName AS STRING) AS LOGIC
		LOCAL oTemplate AS VOControlTemplate
		LOCAL oDesign AS DesignWindowItem
		LOCAL cGuid AS STRING
		IF SELF:oWindowDesign != NULL
			RETURN FALSE
		ENDIF
		oTemplate := VOWindowEditorTemplate.Get(cType)
		IF oTemplate == NULL
			RETURN FALSE
		END IF
		
//		SELF:oStatusBar := StatusBar{}
//		SELF:Surface:Controls:Add(SELF:oStatusBar)
		
		cGuid := Guid.NewGuid():ToString()
		oDesign := (DesignWindowItem)SELF:StartAction(DesignerBasicActionType.Create , ActionData{cGuid , cType})
		oDesign:Control:AllowDrop := TRUE
		oDesign:Control:DragOver += DragEventHandler{ SELF , @WindowDragOver() }
		oDesign:Control:DragDrop += DragEventHandler{ SELF , @WindowDragDrop() }
//		oDesign:aProperties:Add(VODesignProperty{"_Left","Left","",PropertyType.Numeric})
//		oDesign:aProperties:Add(VODesignProperty{"_Top","Top","",PropertyType.Numeric})
		// now all properties are being added, even if not included in property pages
		SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "Name" , cName})
		SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "Caption" , oTemplate:cStartText})

		SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Left" , oDesign:Control:Left})
		SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Top" , oDesign:Control:Top})
		SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Width" , oDesign:Control:Width})
		SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Height" , oDesign:Control:Height})

		SELF:oWindow:Paint += PaintEventHandler{ SELF , @ControlPaint() }
		SELF:DoAction(DesignerActionType.Select , SELF:oWindowDesign:cGuid)
		SELF:ClearUndoBuffer()
	RETURN TRUE

	METHOD WindowDragOver(o AS OBJECT,e AS DragEventArgs) AS VOID
		IF SELF:lReadOnly
			e:Effect := DragDropEffects.None
			RETURN
		ENDIF
		IF e:Data != NULL .and. e:Data:GetDataPresent(TypeOf(VOControlTemplate))
			e:Effect := DragDropEffects.Move
		END IF
	RETURN
	METHOD WindowDragDrop(o AS OBJECT,e AS DragEventArgs) AS VOID
		LOCAL oTemplate AS VOControlTemplate
		IF SELF:lReadOnly
			RETURN
		ENDIF
		IF e:Data != NULL .and. e:Data:GetDataPresent(TypeOf(VOControlTemplate))
			oTemplate := (VOControlTemplate)e:Data:GetData(TypeOf(VOControlTemplate))
			IF oTemplate != NULL
				SELF:DoCreate(oTemplate , Point{e:X , e:Y} , Size{0,0})
			ENDIF
		END IF
	RETURN
	
	VIRTUAL METHOD TimerTicked(o AS OBJECT,e AS EventArgs) AS VOID
//	PRIVATE METHOD TimerTicked(o AS OBJECT,e AS EventArgs) AS VOID // TODO: should be allowed?
/*		IF SELF:eCurrent == WEDAction.None
			SELF:oSurface:Invalidate(TRUE)
		ENDIF*/
		// Check periodically if the WED is active and if not, hide the toolwindows
		IF SELF:oGrid != NULL .and. SELF:oGrid:oActiveDesigner == SELF
			IF SELF:eCurrent == WEDAction.None .and. !SELF:oSurface:ContainsFocus
				SELF:ShowHideTools(FALSE)
			ENDIF
		ENDIF
	RETURN
	
	PROTECTED VIRTUAL METHOD ControlPaint(o AS OBJECT , e AS PaintEventArgs) AS VOID
		SELF:DrawGrid((Control)o , e)
	RETURN

	VIRTUAL METHOD DrawGrid(oControl AS Control , e AS PaintEventArgs) AS VOID
		IF SELF:oOptions:lShowGrid
			ControlPaint.DrawGrid(e:Graphics , e:ClipRectangle , SELF:oOptions:oGridSize , oControl:BackColor)
		END IF
	RETURN
	
	METHOD ShowCode() AS VOID
		LOCAL oCode AS CodeContents
		LOCAL t AS TextBox
		LOCAL f AS Form
		LOCAL n AS INT

		oCode := SELF:GetCodeContents()
		
		f := Form{}
		f:Size := Size{800 , 500}
		f:ShowInTaskbar := FALSE
		f:Text := "Generated code"
		t := TextBox{}
		t:Multiline := TRUE
		t:ScrollBars := ScrollBars.Vertical
		t:Dock := DockStyle.Fill
		f:Controls:Add(t)

		t:Text += e"Defines : \r\n"
		FOR n := 0 UPTO oCode:aDefines:Count - 1
			t:Text += oCode:aDefines[n] + " " + oCode:aDefineValues[n] + e"\r\n"
		NEXT
		t:Text += e"\r\n"
		t:Text += e"-----------------------------------"
		t:Text += e"\r\n"
		t:Text += e"Resource : \r\n"
		FOR n := 0 UPTO oCode:aResource:Count - 1
			t:Text += oCode:aResource[n] + e"\r\n"
		NEXT
		t:Text += e"\r\n"
		t:Text += e"-----------------------------------"
		t:Text += e"\r\n"
		t:Text += e"Code : \r\n"
		FOR n := 0 UPTO oCode:aClass:Count - 1
			t:Text += oCode:aClass[n] + e"\r\n"
		NEXT
		t:Text += e"\r\n"
		FOR n := 0 UPTO oCode:aConstructor:Count - 1
			t:Text += oCode:aConstructor[n] + e"\r\n"
		NEXT

		f:ShowDialog()
RETURN

	METHOD GetCodeContents() AS CodeContents
		LOCAL oCode AS CodeContents
		LOCAL cLine AS STRING
		LOCAL aDesign AS ArrayList
		LOCAL aColumns AS ArrayList
		LOCAL oDesign AS DesignWindowItem
		LOCAL cTemp AS STRING
		LOCAL lPixPos AS LOGIC
		LOCAL lExpCtls AS LOGIC
		LOCAL oRect AS Rectangle
		LOCAL oWinRect AS Rectangle
		LOCAL oProp AS VODesignProperty
		LOCAL lHasServer AS LOGIC
		LOCAL oTabControl AS DesignTabControl
		LOCAL oPageOptions AS VOTabPageOptions
		LOCAL n,m AS INT

		LOCAL aDefines AS List<STRING>
		LOCAL aDefineValues AS List<STRING>
		LOCAL aResource AS List<STRING>
		LOCAL aClass AS List<STRING>
		LOCAL aConstructor AS List<STRING>
		LOCAL aTabPages AS List<STRING>
		LOCAL aRadioGroups AS List<STRING>
		
//		LOCAL rDivX , rDivY AS REAL8
		LOCAL oFont AS Font
		LOCAL oGraphics AS Graphics
		LOCAL nBaseUnits AS INT
		LOCAL hDC , hOldFont AS IntPtr
		LOCAL sTextMetric AS _winTEXTMETRIC
		LOCAL sSize AS _winSIZE
		LOCAL oInfo AS UnitTranslateInfo
		LOCAL cStyle AS STRING
		LOCAL lFontUsed AS LOGIC
		
		LOCAL eViewMode AS ViewMode
		LOCAL cColumnsInherit AS STRING
		LOCAL cBrowserInherit AS STRING
		eViewMode := SELF:ViewMode
		cColumnsInherit := SELF:oWindowDesign:ColumnsInheritClassName
		cBrowserInherit := SELF:oWindowDesign:BrowserInheritClassName

		oProp := SELF:oWindowDesign:GetProperty("Font")
		IF oProp != NULL .and. !oProp:IsAuto .and. oProp:Value != NULL .and. oProp:Value:GetType() == TypeOf(Font)
			oFont := (Font)oProp:Value
		ELSE
			oFont := SELF:oWindow:Font
		ENDIF
		nBaseUnits := GetDialogBaseUnits()
		oInfo:nBaseUnitX := nBaseUnits & (INT)0xFFFF
		oInfo:nBaseUnitY := nBaseUnits >> 16
		oGraphics := SELF:oWindow:CreateGraphics()
		hDC := oGraphics:GetHdc()
		hOldFont := SelectObject(hDC, oFont:ToHfont())
		GetTextMetrics(hDC, sTextMetric)
		cLine := "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
		GetTextExtentPoint32(hDC, cLine, cLine:Length, sSize)
		oInfo:tmWidth := (sSize:cx / 26 + 1) / 2
		oInfo:tmHeight := sTextMetric:tmHeight
		SelectObject(hDC, hOldFont)
		//ReleaseDC(IntPtr.Zero, hdc)
		oGraphics:ReleaseHdc(hDC)
		oGraphics:Dispose()

/*		rDivY := (REAL8)oFont:Height / (REAL8)8.0
		cLine := "ABCDabcdefghiklmnopqrstuvwxyz" // TODO
		cLine := cLine + cLine + cLine + cLine
		rDivX := (REAL8)oGraphics:MeasureString(cLine , oFont):Width / (REAL8)cLine:Length  / (REAL4)4.0*/


		oCode := CodeContents{}

		aDefines := oCode:aDefines
		aDefineValues := oCode:aDefineValues
		aResource := oCode:aResource
		aClass := oCode:aClass
		aConstructor := oCode:aConstructor
		aTabPages := List<STRING>{}
		aRadioGroups := List<STRING>{}

		SELF:ArrangeControlOrder()
		aDesign := SELF:GetAllDesignItemsByCreationOrder()
		FOR n := 0 UPTO aDesign:Count - 1
			oDesign := (DesignWindowItem)aDesign[n]
//			aDefines:Add("#define " + oDesign:GetVODefine(SELF:oWindowDesign:Name) + " " + (100 + n):ToString())
			aDefines:Add(oDesign:GetVODefine(SELF:oWindowDesign:Name))
			aDefineValues:Add((100 + n):ToString())
		NEXT
		
		IF SELF:ViewMode != ViewMode.Auto
			aColumns := SELF:GetAllColumnDesignItemsByIndex(TRUE)
		END IF

// resource
		oDesign := SELF:oWindowDesign
		IF oDesign:lCreateResource
			oRect := SELF:oWindowDesign:GetRect()
			cLine := e"RESOURCE "
			cLine := e""
			cLine += SELF:oWindowDesign:Name + " DIALOGEX "
/*			cLine += ((INT)(oRect:Left / rDivX)):ToString() + ", "
			cLine += ((INT)(oRect:Top / rDivY)):ToString() + ", "
			cLine += ((INT)(oRect:Width / rDivX)):ToString() + ", "
			cLine += ((INT)(oRect:Height / rDivY)):ToString()*/
			oRect:Width -= 2
			oRect:Height -= 2
			oRect := Funcs.PixelsToUnits(oRect , oInfo)
			cLine += oRect:Left:ToString() + ", "
			cLine += oRect:Top:ToString() + ", "
			cLine += oRect:Width:ToString() + ", "
			cLine += oRect:Height:ToString()
			aResource:Add(cLine)
			
//			aResource:Add(e"STYLE WS_CHILD")
			cStyle := oDesign:GetVOStylesString(VOStyle.Style)
			aResource:Add( "STYLE " + cStyle)
			IF cStyle:ToUpper():IndexOf("WS_CAPTION") != -1 .and. SELF:oWindowDesign:GetProperty("Caption") != NULL
				aResource:Add( e"CAPTION \"" + Funcs.TranslateCaption(SELF:oWindowDesign:Caption , FALSE) + e"\"")
			ENDIF
			oProp := oDesign:GetProperty("Font")
			IF oProp != NULL .and. !oProp:IsAuto
				aResource:Add("FONT " + oProp:CodeValue:Substring(3)) // TODO ugly hack
			ELSE
				aResource:Add(e"FONT 8, \"MS Shell Dlg\"")
			ENDIF
			aResource:Add(e"BEGIN")
			FOR n := 0 UPTO aDesign:Count - 1
				oDesign := (DesignWindowItem)aDesign[n]
				IF oDesign:Deleted != 0
					LOOP
				END IF
				oRect := oDesign:GetRect()
				cLine := e"\t"
				cLine += e"CONTROL "
				cLine += e"\"" + Funcs.TranslateCaption(oDesign:Caption , FALSE) + e"\", "
				cLine += oDesign:GetVODefine(SELF:oWindowDesign:Name) + ", "
				cLine += e"\"" + oDesign:cWinClass:ToUpper() + e"\", "
				cLine += oDesign:GetVOStylesString(VOStyle.Style)
				oProp := oDesign:GetProperty("_Visible")
				IF oProp != NULL .and. oProp:TextValue:ToUpper() == "NO"
					cLine += "|NOT WS_VISIBLE"
				ENDIF
				cLine += ", "
/*				cLine += ((INT)(oRect:Left / rDivX)):ToString() + ", "
				cLine += ((INT)(oRect:Top / rDivY)):ToString() + ", "
				cLine += ((INT)(oRect:Width / rDivX)):ToString() + ", "
				cLine += ((INT)(oRect:Height / rDivY)):ToString()*/
				oRect := Funcs.PixelsToUnits(oRect , oInfo)
				cLine += oRect:Left:ToString() + ", "
				cLine += oRect:Top:ToString() + ", "
				cLine += oRect:Width:ToString() + ", "
				cLine += oRect:Height:ToString()
				cTemp := oDesign:GetVOStylesString(VOStyle.ExStyle)
				IF cTemp:Length != 0
					cLine += ", " + cTemp
				ENDIF
				aResource:Add(cLine)
			NEXT
			aResource:Add("END")
		END IF
		
		oProp := SELF:oWindowDesign:GetPropertyByMember("ExpCtls")
		IF oProp != NULL .and. oProp:TextValue:ToUpper() == "YES"
			lExpCtls := TRUE
		ENDIF
		

// class declaration
		cLine := "CLASS " + SELF:oWindowDesign:Name + " INHERIT " + SELF:oWindowDesign:InheritClassName
		aClass:Add(cLine)
		IF eViewMode != ViewMode.Auto
//			FOR n := 0 UPTO aDesign:Count - 1
			FOR n := 0 UPTO aColumns:Count - 1
//				oDesign := (DesignWindowItem)aDesign[n]
				oDesign := (DesignWindowItem)aColumns[n]
				IF oDesign:Column != NULL .and. oDesign:BrowseIndex != -1
					cLine := e"\t"
					IF lExpCtls
						cLine += "EXPORT "
					ELSE
						cLine += "PROTECT "
					END IF
					cLine += "oDB" + oDesign:Name + " AS "
					cLine += cColumnsInherit
					aClass:Add(cLine)
				END IF
			NEXT
		END IF
		FOR n := 0 UPTO aDesign:Count - 1
			oDesign := (DesignWindowItem)aDesign[n]
			IF oDesign:Deleted != 0
				LOOP
			END IF
			IF oDesign:GetProperty("_GenCode"):TextValue:ToUpper() == "NO"
				LOOP
			ENDIF
			cLine := e"\t"
			IF lExpCtls
				cLine += "EXPORT "
			ELSE
				cLine += "PROTECT "
			END IF
			cLine += oDesign:cPrefix + oDesign:Name + " AS "
			IF oDesign:IsSubForm
				cLine += oDesign:Name
			ELSE
				cLine += oDesign:InheritClassName
			END IF
			aClass:Add(cLine)
			IF oDesign:IsTabControl
				oTabControl := (DesignTabControl)oDesign:Control
				LOCAL cSelectedTab := "" AS STRING
				FOR m := 1 UPTO oTabControl:TabPages:Count
					oPageOptions := oTabControl:GetTabPageOptions(m)
					IF oPageOptions:cCaption == "Page" // TODO Should check if page window exists
						LOOP
					ENDIF
					cLine := e"\t"
					IF lExpCtls
						cLine += "EXPORT "
					ELSE
						cLine += "PROTECT "
					END IF
					cLine += "oTP" + oPageOptions:cName + " AS " + oPageOptions:cName
					aClass:Add(cLine)
					
					cLine := e"\t"
					cLine += "SELF:oTP" + oPageOptions:cName + " := " + oPageOptions:cName
					cLine += "{SELF , " + (m - 1):ToString() + "}"
					aTabPages:Add(cLine)
					cLine := e"\t"
					cLine += "SELF:" + oDesign:cPrefix + oDesign:Name + ":AppendTab(#" + oPageOptions:cName
//					cLine += e" , \""  + oPageOptions:cCaption + e"\" , SELF:oTP" + oPageOptions:cName
					cLine += " , "  + Funcs.TranslateCaption(oPageOptions:cCaption , TRUE) + " , SELF:oTP" + oPageOptions:cName
					cLine += " , " + (m - 1):ToString() + ")"
					aTabPages:Add(cLine)
					IF oTabControl:SelectedIndex == m - 1
						cLine := e"\t"
						cLine += "SELF:" + oDesign:cPrefix + oDesign:Name + ":SelectTab(#" + oPageOptions:cName
						cLine += ")"
						cSelectedTab := cLine
					END IF
				NEXT
				aTabPages:Add(cSelectedTab)
			ENDIF
		NEXT
		aClass:Add("")
//		aClass:Add(e"\t//{{%UC%}} USER CODE STARTS HERE (do NOT remove this line)")
//		aClass:Add("")
		


// constructor
		oProp := SELF:oWindowDesign:GetProperty("Pixel Positions")
		IF oProp != NULL .and. oProp:TextValue:ToUpper() == "YES"
			lPixPos := TRUE
		ENDIF
		
		cLine := "CONSTRUCTOR(" + SELF:oWindowDesign:cInitMethod + e")"
		aConstructor:Add(cLine)
		aConstructor:Add("")
		cLine := e"\tSELF:PreInit(" + SELF:oWindowDesign:cInitMethod + e")"
		aConstructor:Add(cLine)
		aConstructor:Add("")
		
		IF SELF:HasRichEdit
			aConstructor:Add(e"\tLoadLibrary(String2Psz(\"RICHED32.DLL\"))")
			aConstructor:Add("")
		ENDIF

		cLine := ""
		DO CASE
		CASE SELF:oWindowDesign:cFullClass:IndexOf("FORM:DATAWINDOW") == 0
			cLine += String.Format(e"\tSUPER(oWindow , ResourceID{{\"{0}\" , _GetInst()}},iCtlID)" , SELF:oWindowDesign:Name)
		CASE SELF:oWindowDesign:cFullClass:IndexOf("FORM:DATADIALOG") == 0
			cLine += String.Format(e"\tSUPER(oWindow , ResourceID{{\"{0}\" , _GetInst()}},iCtlID)" , SELF:oWindowDesign:Name)
		CASE SELF:oWindowDesign:cFullClass:IndexOf("FORM:DIALOGWINDOW") == 0
			LOCAL cModal AS STRING
			oProp := SELF:oWindowDesign:GetProperty("Modeless")
			IF oProp != NULL .and. oProp:TextValue:ToUpper() == "YES" // null in classmate
				cModal := "FALSE"
			ELSE
				cModal := "TRUE"
			ENDIF
			cLine += String.Format(e"\tSUPER(oParent , ResourceID{{\"{0}\" , _GetInst()}} , {1})" , SELF:oWindowDesign:Name , cModal)
		END CASE
		aConstructor:Add(cLine)

		aConstructor:Add("")
		FOR n := 0 UPTO aDesign:Count - 1
			oDesign := (DesignWindowItem)aDesign[n]
			IF oDesign:GetProperty("_GenCode"):TextValue:ToUpper() == "NO"
				LOOP
			ENDIF
			IF oDesign:Deleted != 0
				LOOP
			END IF
			IF oDesign:IsSubForm
				LOOP
			ENDIF

//			IF oDesign:cPrefix == "oDC"
			IF oDesign:lAccessAssign
				oCode:aAccessAssign:Add(oDesign:Name)
			ENDIF

			cLine := e"\t"
			cLine += "SELF:" + oDesign:cPrefix
			cLine += oDesign:Name
			cLine += " := "
			cLine += oDesign:InheritClassName
			cLine += "{"
			cLine += "SELF , ResourceID{ " + oDesign:GetVODefine(SELF:oWindowDesign:Name) + "  , _GetInst() } "
			IF lPixPos
				oWinRect := SELF:oWindowDesign:GetRect()
				oRect := oDesign:GetRect()
				cLine += String.Format(", Point{{{0} , {1}}} " , oRect:Left , oWinRect:Height - oRect:Bottom)
				cLine += String.Format(", Dimension{{{0} , {1}}} " , oRect:Width , oRect:Height)
			ENDIF
			cLine += "}"
			aConstructor:Add(cLine)
			SELF:GetCode(oDesign , "SELF:" + oDesign:cPrefix + oDesign:Name + ":" , aConstructor , lFontUsed)
			IF oDesign:cFullClass:ToUpper():StartsWith("CONTROL:TEXTCONTROL:RADIOBUTTONGROUP")
				SELF:GetRadioGroupCode(oDesign , "SELF:" + oDesign:cPrefix + oDesign:Name , aRadioGroups)
			ENDIF
			aConstructor:Add("")
		NEXT

		IF lFontUsed
			aConstructor:Insert(1 , e"\tLOCAL oFont AS Font")
		ENDIF
		
		FOR n := 0 UPTO aRadioGroups:Count - 1
			aConstructor:Add(aRadioGroups[n])
		NEXT

		// Now all properties are being used, so no need to do that
		oProp := SELF:oWindowDesign:GetProperty("Caption")
//		IF oProp != NULL .or. oProp:lMultiple // not in classmate
		IF oProp != NULL .and. oProp:lMultiple // not in classmate
//			cLine := String.Format(e"\tSELF:Caption := \"{0}\"" , oProp:TextValue)
			cLine := String.Format(e"\tSELF:Caption := {0}" , Funcs.TranslateCaption(oProp:TextValue , TRUE))
			aConstructor:Add(cLine)
		END IF

		SELF:GetCode(SELF:oWindowDesign , "SELF:" , aConstructor , lFontUsed)

		oProp := SELF:oWindowDesign:GetProperty("Data Server")
		IF oProp != NULL .and. (SELF:oWindowDesign:cFullClass:IndexOf("FORM:DATAWINDOW") == 0 .or. SELF:oWindowDesign:cFullClass:IndexOf("FORM:DATADIALOG") == 0)
			aConstructor:Add(e"\tIF !IsNil(oServer)")
			aConstructor:Add(e"\t\tSELF:Use(oServer)")
			IF !oProp:IsAuto
				lHasServer := TRUE
				aConstructor:Add(e"\tELSE")
				IF oProp:TextValue:ToUpper():Contains("<SHARE")
					aConstructor:Add(String.Format(e"\t\tSELF:Use(SELF:Owner:Server)"))
				ELSE
					aConstructor:Add(String.Format(e"\t\tSELF:Use({0}{{}})" , oProp:TextValue))
				END IF
			END IF
			aConstructor:Add(e"\tENDIF")
			aConstructor:Add("")
		END IF


// databrowser - columns
		IF eViewMode != ViewMode.Auto
			cLine := e"\tSELF:Browser := " + cBrowserInherit + "{SELF}"
			aConstructor:Add(cLine)
			aConstructor:Add("")
			
//			FOR n := 0 UPTO aDesign:Count - 1
//				oDesign := (DesignWindowItem)aDesign[n]
			FOR n := 0 UPTO aColumns:Count - 1
				oDesign := (DesignWindowItem)aColumns[n]
				IF oDesign:Column == NULL .or. oDesign:BrowseIndex == -1
					LOOP
				ENDIF
				LOCAL cColumn AS STRING
				
				cColumn := e"\tSELF:oDB" + oDesign:Name
	
				cLine := cColumn
				cLine += " := "
				cLine += cColumnsInherit
				cLine += "{"
				oProp := oDesign:GetPropertyByMember("FieldSpec")
				IF oProp == NULL .or. oProp:IsAuto
					cLine += oDesign:BrowseSize:ToString()
				ELSE
					cLine += oProp:TextValue + "{}"
				END IF
				cLine += "}"
				aConstructor:Add(cLine)
				aConstructor:Add(cColumn + ":Width := " + oDesign:BrowseSize:ToString())
				cLine := cColumn + ":HyperLabel := "
				IF oDesign:Deleted != 0
					cLine += "HyperLabel{"
					cLine += "#" + oDesign:Name + ","
					oProp := oDesign:GetProperty("Caption")
					IF oProp == NULL .or. oProp:TextValue:Length == 0
						cLine += "NULL_STRING, "
					ELSE
						cLine += e"\"" + oProp:TextValue + e"\", "
					END IF
					oProp := oDesign:GetProperty("Description")
					IF oProp == NULL .or. oProp:TextValue:Length == 0
						cLine += "NULL_STRING, "
					ELSE
						cLine += e"\"" + oProp:TextValue + e"\", "
					END IF
					oProp := oDesign:GetProperty("Help Context")
					IF oProp == NULL .or. oProp:TextValue:Length == 0
						cLine += "NULL_STRING"
					ELSE
						cLine += e"\"" + oProp:TextValue + e"\""
					END IF
					cLine += "}"
				ELSE
					cLine += "SELF:" + oDesign:cPrefix + oDesign:Name + ":HyperLabel"
				END IF
				aConstructor:Add(cLine)

				oProp := oDesign:GetProperty("Caption")
				IF oProp != NULL
					cLine := cColumn + e":Caption := \"" + oProp:TextValue + e"\""
					aConstructor:Add(cLine)
				END IF
				
				oProp := oDesign:GetProperty("Block")
				IF oProp != NULL .and. !oProp:TextValue:Trim():Length == 0
					cLine := cColumn + ":Block := " + oProp:TextValue:Trim()
					aConstructor:Add(cLine)
				END IF
				oProp := oDesign:GetProperty("Block Owner")
				IF oProp != NULL .and. !oProp:TextValue:Trim():Length == 0
					cLine := cColumn + ":BlockOwner := " + oProp:TextValue:Trim()
					aConstructor:Add(cLine)
				END IF
				
				cLine := e"\tSELF:Browser:AddColumn(SELF:oDB" + oDesign:Name + ")"
				aConstructor:Add(cLine)
				
				aConstructor:Add("")
			NEXT
			
			cLine := e"\tSELF:ViewAs(" + SELF:oWindowDesign:GetProperty("View As"):TextValue + ")"
			aConstructor:Add(cLine)
			aConstructor:Add("")
		END IF
		

// subforms
		FOR n := 0 UPTO aDesign:Count - 1
			oDesign := (DesignWindowItem)aDesign[n]
			IF !oDesign:IsSubForm
				LOOP
			ENDIF
			IF oDesign:GetProperty("_GenCode"):TextValue:ToUpper() == "NO"
				LOOP
			ENDIF
			cLine := e"\t"
			cLine += "SELF:oSF"
			cLine += oDesign:Name
			cLine += " := "
			cLine += oDesign:Name
			cLine += "{"
			cLine += "SELF , " + oDesign:GetVODefine(SELF:oWindowDesign:Name) + " "
			cLine += "}"
			aConstructor:Add(cLine)
			cLine := e"\tSELF:oSF" + oDesign:Name + ":Show()"
			aConstructor:Add(cLine)
			IF lHasServer
				oProp := oDesign:GetProperty("Order")
				IF !oProp:IsAuto
					aConstructor:Add(String.Format(e"\tIF SELF:oSF{0}:Server != NIL" , oDesign:Name))
					aConstructor:Add(String.Format(e"\t\tSELF:oSF{0}:Server:SetOrder(\"{1}\")" , oDesign:Name , oProp:TextValue))
					aConstructor:Add(e"\tENDIF")
				ENDIF
				oProp := oDesign:GetProperty("Relation String")
				IF !oProp:IsAuto
					aConstructor:Add(String.Format(e"\tSELF:SetSelectiveRelation(SELF:oSF{0} , {1})" , oDesign:Name , oProp:TextValue))
				ENDIF
			ENDIF
		NEXT


		FOR n := 0 UPTO aTabPages:Count - 1
			aConstructor:Add(aTabPages[n])
		NEXT
		

		aConstructor:Add("")
		cLine := e"\tSELF:PostInit(" + SELF:oWindowDesign:cInitMethod + e")"
		aConstructor:Add(cLine)
		aConstructor:Add("")
		aConstructor:Add(e"RETURN")

		aConstructor:Add("")
//		aConstructor:Add("END CLASS")
		
	RETURN oCode

	METHOD GetCode(oDesign AS DesignWindowItem , cVar AS STRING , aCode AS List<STRING> , lFontUsed REF LOGIC) AS VOID
		LOCAL oProp AS VODesignProperty
		LOCAL oTemp AS VODesignProperty
		LOCAL cValue AS STRING
//		LOCAL aMultiple AS Dictionary<STRING,ArrayList>
		LOCAL aMultiple AS SortedList<STRING,ArrayList>
		LOCAL aList AS ArrayList
		LOCAL cMap AS STRING
		LOCAL n AS INT

		IF SELF:oWindow == NULL .or. SELF:oWindowDesign == NULL
			RETURN
		ENDIF
		
//		aMultiple := Dictionary<STRING,ArrayList>{}
		aMultiple := SortedList<STRING,ArrayList>{}
		FOR n := 0 UPTO oDesign:aProperties:Count - 1
			oProp := (VODesignProperty)oDesign:aProperties[n]
//			IF oProp:eVOStyle == VOStyle.None  .and. !oProp:lNoCode .and. !oProp:IsAuto .and. oProp:Name[0] != '_'
			IF oProp:eVOStyle == VOStyle.None  .and. !oProp:lNoCode .and. oProp:Name[0] != '_'

				IF oProp:lMultiple 
					IF oProp:nMultiPos != 0
						IF aMultiple:ContainsKey(oProp:cMember)
							aList := aMultiple[oProp:cMember]
						ELSE
							aList := ArrayList{}
							aMultiple:Add(oProp:cMember , aList)
						ENDIF
						DO WHILE aList:Count < oProp:nMultiPos
							aList:Add("")
						ENDDO
						cValue := ""
						DO CASE
						CASE oProp:cSymbolProp != NULL .and. oProp:cSymbolProp != ""
							oTemp := oDesign:GetProperty(oProp:cSymbolProp)
							cValue := "#" + oTemp:TextValue
						CASE oProp:Type == PropertyType.Text
							IF oProp:IsAuto
								cValue := "NULL_STRING"
							ELSE
//								cValue := e"\"" + oProp:TextValue + e"\""
								cValue := Funcs.TranslateCaption(oProp:TextValue , TRUE)
							ENDIF
						CASE oProp:Type == PropertyType.Numeric
							IF oProp:IsAuto
								cValue := NULL
							ELSE
								cValue := oProp:TextValue
							ENDIF
						END CASE
						aList[oProp:nMultiPos - 1] := cValue
					ENDIF

				ELSEIF !oProp:IsAuto .or. (oProp:cSymbolProp != NULL .and. oProp:cSymbolProp != "")

					cValue := NULL
					DO CASE
					CASE oProp:cSymbolProp != NULL .and. oProp:cSymbolProp != ""
						cValue := "#" + oDesign:GetProperty(oProp:cSymbolProp):TextValue
					CASE oProp:Type == PropertyType.Text
//						cValue := e"\"" + oProp:TextValue + e"\""
//						cValue := Funcs.TranslateCaption(oProp:TextValue , TRUE)
						cValue := oProp:TextValue
						cValue := Funcs.TranslateCaption(cValue , TRUE)
						cMap := VOWindowEditorTemplate.GetAssignMap(oProp:cMember)
						DO CASE
						CASE oProp:lMethod
						CASE cMap == NULL
							cValue := oProp:cMember + "{ " + cValue + " }"
						CASE cMap:Length != 0
							cValue := cMap + "{ " + cValue + " }"
						END CASE
					CASE oProp:Type == PropertyType.Numeric
						cValue := oProp:TextValue
					CASE oProp:Type == PropertyType.Enumerated
						cValue := oProp:CodeValue
						cMap := VOWindowEditorTemplate.GetAssignMap(oProp:cMember)
						IF cMap != NULL .and. cMap:ToUpper() == "BRUSH"
							cValue := "Color{ " + cValue + " }"
						ENDIF
						DO CASE
						CASE oProp:lMethod .and. (oProp:cMember == NULL .or. oProp:cMember:ToUpper() != "FONT")
						CASE cMap == NULL
							cValue := oProp:cMember + "{ " + cValue + " }"
						CASE cMap:Length != 0
							cValue := cMap + "{ " + cValue + " }"
						END CASE
/*						IF oProp:cSpecialClass != NULL
							DO CASE
							CASE oProp:cSpecialClass == "Color"
								cValue := "Color{ " + cValue + " }"
							CASE oProp:cSpecialClass == "Brush"
								cValue := "Brush{ Color{ " + cValue + " } }"
							CASE oProp:cSpecialClass == "Font"
								cValue := "Font{ " + cValue + " }"
							END CASE
						END IF*/
					CASE oProp:Type == PropertyType.Type
						cValue := oProp:TextValue + "{}"
					END CASE
					IF cValue != NULL
						LOCAL oFont AS Font
						IF oProp:cMember != NULL .and. oProp:cMember:ToUpper() == "FONT" .and. oProp:Value:GetType() == TypeOf(Font)
							lFontUsed := TRUE
							oFont := (Font)oProp:Value
							aCode:Add(e"\toFont := " + cValue)
							IF oFont:Bold
								aCode:Add(e"\toFont:Bold := TRUE")
							ENDIF
							IF oFont:Italic
								aCode:Add(e"\toFont:Italic := TRUE")
							ENDIF
							IF oFont:Underline
								aCode:Add(e"\toFont:Underline := TRUE")
							ENDIF
							IF oFont:Strikeout
								aCode:Add(e"\toFont:Strikethru := TRUE")
							ENDIF
							cValue := "oFont"
						END IF
						IF oProp:lMethod
							aCode:Add(e"\t" + cVar + oProp:cMember + "( " + cValue + e" )")
						ELSE
							aCode:Add(e"\t" + cVar + oProp:cMember + e" := " + cValue + e"")
						ENDIF
					ENDIF

				ENDIF
			ENDIF
		NEXT
		
		LOCAL m AS INT
		LOCAL lAuto AS LOGIC
		FOR n := 0 UPTO aMultiple:Count - 1
			aList := aMultiple:Values[n]
			cValue := ""
			lAuto := TRUE
			FOR m := 0 UPTO aList:Count - 1
				IF m != 0
					cValue += " , "
				ENDIF
				IF aList[m] != NULL
					cValue += aList[m]:ToString()
					lAuto := FALSE
				ENDIF
			NEXT
			IF !lAuto
				cValue := aMultiple:Keys[n] + "{" + cValue + "}"
				aCode:Add(e"\t" + cVar + aMultiple:Keys[n] + " := " + cValue + e"")
			END IF
		NEXT
		
	RETURN

	METHOD GetRadioGroupCode(oGroup AS DesignWindowItem , cGroup AS STRING , aRadioGroups AS List<STRING>) AS VOID
		LOCAL oDesign AS DesignWindowItem 
		LOCAL oProp AS VODesignProperty
		LOCAL aRadios AS List<STRING>
		LOCAL aDesign AS ArrayList
		LOCAL oRect AS Rectangle
		LOCAL cValue AS STRING
		LOCAL lAdded AS LOGIC
		LOCAL cTabs AS STRING
		LOCAL n AS INT
		
		aRadios := List<STRING>{}
		
		oRect := Rectangle{(INT)oGroup:GetProperty("_Left"):Value , (INT)oGroup:GetProperty("_Top"):Value , (INT)oGroup:GetProperty("_Width"):Value , (INT)oGroup:GetProperty("_Height"):Value}
		aDesign := SELF:GetAllDesignItemsByCreationOrder()
		FOR n := 0 UPTO aDesign:Count - 1
			oDesign := (DesignWindowItem)aDesign[n]
			IF oDesign:cFullClass:ToUpper():StartsWith("CONTROL:TEXTCONTROL:BUTTON:RADIOBUTTON")
				IF oRect:Contains(Point{(INT)oDesign:GetProperty("_Left"):Value , (INT)oDesign:GetProperty("_Top"):Value})
					lAdded := TRUE
					oProp := (VODesignProperty)oDesign:GetProperty("Group Value")
					IF oProp != NULL .and. .not. oProp:IsAuto
						cValue := oProp:TextValue
					ELSE
						cValue := oDesign:Name
					END IF
					cValue := e"\"" + cValue + e"\""
					aRadios:Add("{SELF:oCC" + oDesign:Name + ", " + cValue + "}")
				END IF
			END IF
		NEXT

		IF lAdded
			cTabs := e"\t\t\t\t\t\t\t\t\t\t"
			aRadioGroups:Add(e"\t" + cGroup + ":FillUsing({ ;")
			FOR n := 0 UPTO aRadios:Count - 1
				IF n == aRadios:Count - 1
					aRadioGroups:Add(cTabs + aRadios[n] + " ;")
				ELSE
					aRadioGroups:Add(cTabs + aRadios[n] + ", ;")
				END IF
			NEXT
			aRadioGroups:Add(cTabs + "})")
			aRadioGroups:Add("")
		END IF
		
	RETURN

	PROTECTED VIRTUAL METHOD ControlKeyDown(o AS OBJECT,e AS KeyEventArgs) AS VOID

		IF SELF:oWindow == NULL .or. SELF:oWindowDesign == NULL
			RETURN
		ENDIF
		IF SELF:lReadOnly
			RETURN
		ENDIF

		DO CASE
		CASE e:KeyCode == Keys.Escape
			SELF:CancelAction()

		CASE e:KeyData == Keys.G + Keys.Control
			SELF:ToggleGrid()

		CASE e:KeyData == Keys.K + Keys.Control
			SELF:ShowTabOrder()

		CASE e:KeyData == Keys.T + Keys.Control
			SELF:TestForm()

		CASE e:KeyData == Keys.D + Keys.Control
			SELF:ShowCode()

		END CASE

		IF SELF:lStandalone
			DO CASE
			CASE e:KeyCode == Keys.A .and. e:Control
				SELF:DoAction(DesignerActionType.SelectAll)
			CASE e:KeyCode == Keys.Delete
				IF !SELF:oWindowDesign:lSelected
					SELF:DoAction(DesignerActionType.RemoveSelected)
				END IF

			CASE e:KeyData == Keys.Z + Keys.Control .or. e:KeyData == Keys.Back + Keys.Alt
				SELF:Undo()
			CASE e:KeyData == Keys.Z + Keys.Control + Keys.Shift .or. e:KeyData == Keys.Back + Keys.Alt + Keys.Shift
				SELF:Redo()
	
			CASE e:KeyData == Keys.C + Keys.Control .or. e:KeyData == Keys.Insert + Keys.Control
				SELF:Copy()
			CASE e:KeyData == Keys.V + Keys.Control .or. e:KeyData == Keys.Insert + Keys.Shift
				SELF:Paste()
			CASE e:KeyData == Keys.X + Keys.Control .or. e:KeyData == Keys.Delete + Keys.Shift
				SELF:Cut()

			CASE e:KeyData == Keys.O + Keys.Control
				SELF:Open()
			CASE e:KeyData == Keys.S + Keys.Control
				SELF:Save()
			CASE e:KeyData == Keys.N + Keys.Control
				SELF:CreateNewWindow("DATAWINDOW" , "Window1")
			END CASE
		ENDIF

	RETURN

	METHOD GetDesignerUnderPoint(_oPoint AS Point) AS DesignWindowItem
		LOCAL oDesign AS DesignWindowItem
		LOCAL oRet AS DesignWindowItem
		LOCAL aDesign AS ArrayList
		LOCAL oPoint AS Point
		LOCAL n AS INT
		aDesign := SELF:GetAllDesignItems()
		FOR n := 0 UPTO aDesign:Count - 1
			oDesign := (DesignWindowItem)aDesign[n]
			oPoint := oDesign:Control:Parent:PointToClient(_oPoint)
			IF oDesign:Control:Bounds:Contains(oPoint)
				oRet := oDesign // no EXIT, can be a control inside it
				EXIT
			END IF
		NEXT
	RETURN oRet

	METHOD ShowTabOrder() AS VOID
		LOCAL oDlg AS VOControlCreationOrderDlg
		LOCAL oDesign AS DesignWindowItem
		LOCAL n AS INT
		IF SELF:oWindow == NULL .or. SELF:oWindowDesign == NULL
			RETURN
		ENDIF
		IF SELF:lReadOnly
			RETURN
		ENDIF
		SELF:SelectMainItem()
		oDlg := VOControlCreationOrderDlg{SELF , SELF:GetAllDesignItemsByCreationOrder()}
		IF oDlg:ShowDialog() == DialogResult.OK
			SELF:BeginAction()
			FOR n := 0 UPTO oDlg:aNewOrder:Count - 1
				oDesign := (DesignWindowItem)oDlg:aNewOrder[n]
				SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{oDesign:cGuid , "__Order" , n + 1})
			NEXT
			SELF:EndAction()
		ENDIF
		SELF:SelectMainItem()
	RETURN

	METHOD SelectMainItem() AS VOID
		SELF:DoAction(DesignerActionType.Select , SELF:oWindowDesign:cGuid)
	RETURN
	
	ACCESS IsGridEnabled AS LOGIC
	   RETURN oWindow != NULL && oOptions:lShowGrid
	   
	ACCESS ViewMode AS ViewMode
		LOCAL oProp AS DesignProperty
		LOCAL eMode AS ViewMode
		
		IF SELF:oWindowDesign == NULL
			RETURN ViewMode.Auto
		ENDIF
		
		oProp := SELF:oWindowDesign:GetProperty("View As")
		DO CASE
		CASE oProp == NULL
			eMode := ViewMode.Auto
		CASE oProp:TextValue:ToUpper() == "#FORMVIEW"
			eMode := ViewMode.Form
		CASE oProp:TextValue:ToUpper() == "#BROWSEVIEW"
			eMode := ViewMode.Browse
		OTHERWISE
			eMode := ViewMode.Auto
		END CASE
	RETURN eMode
	   
	METHOD ToggleGrid() AS VOID
		IF SELF:oWindow == NULL .or. SELF:oWindowDesign == NULL
			RETURN
		ENDIF
		SELF:oOptions:lShowGrid := ! SELF:oOptions:lUseGrid
		SELF:oOptions:lUseGrid := ! SELF:oOptions:lUseGrid
		SELF:oSurface:Invalidate(TRUE)
	RETURN

	METHOD Open() AS VOID
		LOCAL oDialog AS OpenFileDialog
		oDialog := OpenFileDialog{}
		oDialog:Filter := "Transported WED files (*.xsfrm)|*.xsfrm"
		IF oDialog:ShowDialog() == DialogResult.OK
			SELF:Open(oDialog:FileName)
		ENDIF
//		SELF:OpenVNfrm("C:\Documents and Settings\Cpc\My Documents\Visual Studio Projects\Default Project\VODCApp\Resources\Customer.CustomerWin.xsfrm")
//		SELF:OpenVNfrm("C:\Documents and Settings\Cpc\My Documents\Visual Studio Projects\Default Project1\DS_Test\Resources\TestForm1.Test_Form1.xsfrm")
//		SELF:OpenVNfrm("C:\Documents and Settings\Cpc\My Documents\Visual Studio Projects\TestProject\TestWin\Resources\DialogWin.DialogWin.xsfrm")
//		SELF:OpenVNfrm("C:\Documents and Settings\Cpc\My Documents\Visual Studio Projects\TestProject\TestWin\Resources\DataWin.DataWin.xsfrm")

//		SELF:OpenVNfrm("C:\Documents and Settings\Cpc\My Documents\Visual Studio Projects\TestProject\TestWin\Resources\DataWin.DataWinName.xsfrm")
//		SELF:OpenVNfrm("C:\Documents and Settings\Cpc\My Documents\Visual Studio Projects\TestProject\TestWin\Resources\TabWin.TabWin.xsfrm")
	RETURN
	METHOD Save() AS VOID
		LOCAL oDlg AS SaveFileDialog
		IF SELF:lReadOnly
			RETURN
		ENDIF
		oDlg := SaveFileDialog{}
		oDlg:Filter := "Transported WED files (*.xsfrm)|*.xsfrm"
		IF oDlg:ShowDialog() == DialogResult.OK
			SELF:Save(oDlg:FileName , FALSE)
		ENDIF
	RETURN


	METHOD Open(cFileName AS STRING) AS LOGIC
		LOCAL lSuccess AS LOGIC
		SELF:cDefaultFileName := cFileName
		lSuccess := SELF:OpenVNfrm(cFileName)
		IF lSuccess
			SELF:GiveFocus()
		ENDIF
	RETURN lSuccess
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

	PROTECTED METHOD GetSaveFileStreams(cVNFrmFileName AS STRING , oVNFrmStream REF FileStream , ;
								oRCStream AS EditorStream , oPrgStream AS EditorStream , oVhStream AS EditorStream , ;
								cVhName REF STRING , lVnfrmOnly AS LOGIC , lRcInSameFolder REF LOGIC) AS LOGIC
		LOCAL cRCFileName AS STRING
		LOCAL cPrgFileName AS STRING
		LOCAL cVhFileName AS STRING
		LOCAL cFileName AS STRING
		LOCAL cBaseName AS STRING
		LOCAL oFileInfo AS FileInfo
		LOCAL oBaseDir AS DirectoryInfo
		LOCAL cBaseDir AS STRING
		LOCAL cAlternative AS STRING
		LOCAL lSuccess AS LOGIC
		LOCAL lError AS LOGIC
		LOCAL nAt AS INT
		
		TRY

			oFileInfo := FileInfo{cVNFrmFileName}
			oBaseDir := oFileInfo:Directory
			cBaseDir := oBaseDir:FullName
			cBaseName := oFileInfo:Name
			nAt := cBaseName:ToLower():IndexOf(".xsfrm")
			IF nAt != -1
				cBaseName := cBaseName:Substring(0 , nAt)
			ENDIF
			cRCFileName := cBaseDir + "\Resources\" + cBaseName + ".rc"
			cAlternative := cBaseDir + "\" + cBaseName + ".rc"
			IF !File.Exists(cRCFileName) .and. File.Exists(cAlternative)
				cRCFileName := cAlternative
				lRcInSameFolder := TRUE
			ENDIF
						
			cFileName := cBaseName
			nAt := cFileName:LastIndexOf('.')
			IF nAt != -1 // strip out window name
				cFileName := cFileName:Substring(0 , nAt)
			ENDIF
			cPrgFileName := cBaseDir + "\" + cFileName + ".prg"
			cVhFileName := cBaseDir + "\" + cFileName + ".vh"
			cAlternative := oBaseDir:Parent:FullName + "\" + cFileName + ".prg"
			IF !File.Exists(cPrgFileName) .and. File.Exists(cAlternative)
				cPrgFileName := cAlternative
				cVhFileName := oBaseDir:Parent:FullName + "\" + cFileName + ".vh"
			ENDIF
			cVhName := cFileName + ".vh"
//			MessageBox.Show(cVNFrmFileName + Chr(13)+ Chr(13) + cRCFileName + Chr(13)+ Chr(13) + cPrgFileName , (File.Exists(cVNFrmFileName) .and. File.Exists(cRCFileName) .and. File.Exists(cPrgFileName)):ToString())

			lError := FALSE
			IF !lVnfrmOnly
				IF !File.Exists(cRCFileName)
					Funcs.ErrorBox("File was not found : " + cRCFileName)
					lError := TRUE
				END IF
				IF !File.Exists(cPrgFileName)
					Funcs.ErrorBox("File was not found : " + cPrgFileName)
					lError := TRUE
				END IF
			END IF
			
			lSuccess := FALSE
			IF !lError
				IF !lVnfrmOnly
					oRCStream:Load(cRCFileName)
					oPrgStream:Load(cPrgFileName)
					IF File.Exists(cVhFileName) // else old transporter version
						oVhStream:Load(cVhFileName)
					ENDIF
					lSuccess := oRCStream:IsValid .and. oPrgStream:IsValid
				ELSE
					lSuccess := TRUE
				END IF
			END IF
			IF lSuccess .and. !lError
				lSuccess := FALSE
				oVNFrmStream := File.Open(cVNFrmFileName , FileMode.Create , FileAccess.Write , FileShare.None)
				lSuccess := TRUE
			ENDIF
			
		CATCH e AS Exception

			MessageBox.Show(e:Message , Resources.EditorName , MessageBoxButtons.OK , MessageBoxIcon.Exclamation)
			lSuccess := FALSE

		END TRY

		IF !lSuccess
			IF oVNFrmStream != NULL
				oVNFrmStream:Close()
			ENDIF
			IF oRCStream:IsValid
				oRCStream:Close()
			ENDIF
			IF oPrgStream:IsValid
				oPrgStream:Close()
			ENDIF
			IF oVhStream:IsValid
				oVhStream:Close()
			ENDIF
		END IF

	RETURN lSuccess
	
	PROTECTED VIRTUAL METHOD PreviewControlKeyDown(o AS OBJECT,e AS PreviewKeyDownEventArgs) AS VOID

		IF SELF:oWindow == NULL .or. SELF:oWindowDesign == NULL
			RETURN
		ENDIF
		IF SELF:lReadOnly
			RETURN
		ENDIF

		DO CASE
		CASE e:KeyCode == Keys.Up
			SELF:DoMoveSelected(Direction.Up , e:Control .or. e:Shift)
		CASE e:KeyCode == Keys.Down
			SELF:DoMoveSelected(Direction.Down , e:Control .or. e:Shift)
		CASE e:KeyCode == Keys.Left
			SELF:DoMoveSelected(Direction.Left , e:Control .or. e:Shift)
		CASE e:KeyCode == Keys.Right
			SELF:DoMoveSelected(Direction.Right , e:Control .or. e:Shift)
		END CASE
	RETURN
	
	PROTECTED VIRTUAL METHOD CreateControl(cControl AS STRING, oParent AS Control , cGuid AS STRING) AS DesignWindowItem
		LOCAL oItem AS DesignWindowItem
		oItem := DesignWindowItem{SELF , VOWindowEditorTemplate.Get(cControl)}
		IF cGuid == ""
			cGuid := Guid.NewGuid():ToString()
		ENDIF
		oItem:cGuid := cGuid
		oParent:Controls:Add(oItem:Control)

		IF oItem:IsForm
			SELF:oWindowDesign := oItem
			SELF:oWindow := oItem:Control
		ENDIF

		IF oItem:Column != NULL
			oParent:Controls:Add(oItem:Column)
		END IF
		IF SELF:ViewMode == ViewMode.Browse
			oItem:lBrowseView := TRUE
			oItem:Control:Hide()
		ELSE
			IF oItem:Column != NULL
				oItem:Column:Hide()
			END IF
		ENDIF
//		oItem:Control:BringToFront()
	RETURN oItem
	
	
	METHOD SetText(cText AS STRING) AS VOID
//		SELF:oSurface:Parent:Text := cText
	RETURN

	VIRTUAL METHOD GetDesignItemFromControl(oControl AS Control) AS DesignWindowItem
		IF oControl == NULL
			RETURN NULL
		ENDIF
		IF oControl:GetType() == TypeOf(SelectorBitmap)
			RETURN NULL
		ENDIF
		IF oControl:GetType():GetField("oItem") == NULL
			RETURN NULL
		ENDIF
	RETURN (DesignWindowItem)oControl:GetType():GetField("oItem"):GetValue(oControl)

	METHOD GetHierarchyItems() AS ArrayList
		LOCAL aItems AS ArrayList
		aItems := SELF:GetAllDesignItems()
		aItems:Insert(0 , SELF:oWindowDesign)
	RETURN aItems

	VIRTUAL METHOD GetAllDesignItems() AS ArrayList
	RETURN SELF:GetAllDesignItems(FALSE)
	VIRTUAL METHOD GetAllDesignItems(lIncludeColumns AS LOGIC) AS ArrayList
		LOCAL oItem AS DesignWindowItem
		LOCAL aItems AS ArrayList
		LOCAL n AS INT
		aItems := ArrayList{}
		FOR n := 0 UPTO SELF:oWindow:Controls:Count - 1
			oItem := SELF:GetDesignItemFromControl(SELF:oWindow:Controls[n])
			IF oItem != NULL
				IF aItems:Contains(oItem)
					LOOP
				END IF
				aItems:Add(oItem)
				IF oItem:IsBrowser .and. lIncludeColumns
					LOCAL oColumn AS DesignWindowItem
					LOCAL m AS INT
					FOR m := 0 UPTO oItem:Control:Controls:Count - 1
						oColumn := SELF:GetDesignItemFromControl(oItem:Control:Controls[m])
						IF oColumn != NULL
							aItems:Add(oColumn)
						ENDIF                 
					NEXT
				ENDIF
			ENDIF
		NEXT
	RETURN aItems

	VIRTUAL METHOD GetAllColumnDesignItemsByIndex() AS ArrayList
	RETURN SELF:GetAllColumnDesignItemsByIndex(FALSE)
	VIRTUAL METHOD GetAllColumnDesignItemsByIndex(lIncludeHidden AS LOGIC) AS ArrayList
		LOCAL oItem AS DesignWindowItem
		LOCAL aSorted AS SortedList
		LOCAL oColumn AS Control
		LOCAL aItems AS ArrayList
		LOCAL nIndex AS INT
		LOCAL n AS INT
		aItems := ArrayList{}
		aSorted := SortedList{}
		FOR n := 0 UPTO SELF:oWindow:Controls:Count - 1
			oColumn := SELF:oWindow:Controls[n]
			IF oColumn:GetType() != TypeOf(DesignDataColumn)
				LOOP
			END IF
			IF .not. lIncludeHidden .and. .not. oColumn:Visible
				LOOP
			END IF
			oItem := SELF:GetDesignItemFromControl(oColumn)
			IF oItem != NULL
				nIndex := oItem:BrowseIndex
				IF nIndex == -1
					LOOP
				END IF
				DO WHILE aSorted:ContainsKey(nIndex)
					nIndex ++
				END DO
				aSorted:Add(nIndex , oItem)
			ENDIF
		NEXT
		FOR n := 0 UPTO aSorted:Count - 1
			aItems:Add(aSorted:GetByIndex(n))
		NEXT
	RETURN aItems

	VIRTUAL METHOD GetAllColumnDesignItems() AS ArrayList
		LOCAL oItem AS DesignWindowItem
		LOCAL oColumn AS Control
		LOCAL aItems AS ArrayList
		LOCAL n AS INT
		aItems := ArrayList{}
		FOR n := 0 UPTO SELF:oWindow:Controls:Count - 1
			oColumn := SELF:oWindow:Controls[n]
			IF oColumn:GetType() != TypeOf(DesignDataColumn) .or. !oColumn:Visible
				LOOP
			END IF
			oItem := SELF:GetDesignItemFromControl(oColumn)
			IF oItem != NULL
				aItems:Add(oItem)
			ENDIF
		NEXT
	RETURN aItems
		
	VIRTUAL METHOD GoniaClicked(oSelector AS SelectorBitmap , e AS MouseEventArgs) AS VOID
		IF SELF:lReadOnly
			RETURN
		ENDIF
//		SELF:oDummy:Focus()
		
		IF SELF:ViewMode == ViewMode.Browse
			IF oSelector:nGonia != 4 //.and. oSelector:nGonia != 8
				RETURN
			END IF
		END IF
	
		SELF:oActionStart := Point{e:X , e:Y}
		SELF:oActionEnd := SELF:oActionStart

		SELF:nResizeGonia := oSelector:nGonia
		SELF:lMoved := FALSE
		SELF:eCurrent := WEDAction.Resize
		SELF:oSurface:Capture := TRUE
		
	RETURN

	VIRTUAL METHOD ControlMouseDoubleClick(oItem AS DesignWindowItem , eButton AS MouseButtons , oPoint AS Point) AS VOID
		IF oItem != NULL .and. oItem:cFullClass:ToUpper():StartsWith("CONTROL:TEXTCONTROL:BUTTON:PUSHBUTTON")
			SELF:WriteCallback(oItem , "Click")
		END IF
	RETURN
	
	VIRTUAL METHOD ControlMouseDown(oItem AS DesignWindowItem , eButton AS MouseButtons , oPoint AS Point) AS VOID
		LOCAL oTemp AS Point

		IF SELF:oWindow == NULL .or. SELF:oWindowDesign == NULL
			RETURN
		ENDIF

		SELF:oDummy:Focus()
		IF oItem:lSelected
			IF !oItem:IsForm
				IF oItem:IsTabControl .and. SELF:aSelected:Count == 1 .and. eButton == MouseButtons.Left
					LOCAL oTabControl AS DesignTabControl
					oTabControl := (DesignTabControl)oItem:Control
//					oPoint := oTabControl:PointToClient(oPoint)
//					MessageBox.Show(oPoint:ToString() , oPoint:ToString())
//					IF oPoint:Y < 20 
//					IF oTabControl:GetChildAtPoint(oPoint) != NULL
					IF oTabControl:GetPageUnderPos(oTabControl:PointToClient(oPoint)) != -1
						LOCAL nPage AS INT
//						nPage := oPoint:X / 60
						nPage := oTabControl:GetPageUnderPos(oTabControl:PointToClient(oPoint))
						IF nPage < oTabControl:TabPages:Count
							oTabControl:SelectedIndex := nPage
							SELF:BeginAction()
							SELF:EndAction()
							SELF:oDummy:Focus()
						ENDIF
						RETURN
					END IF
				ELSEIF Control.ModifierKeys == Keys.Control .or. Control.ModifierKeys == Keys.Shift .or. eButton == MouseButtons.Middle
					SELF:DoAction(DesignerActionType.DeSelect, oItem:cGuid )
				ELSE
					SELF:DoAction(DesignerActionType.SelectDefault, oItem:cGuid )
				ENDIF
				Application.DoEvents()
			END IF
		ELSEIF SELF:oToolBox:PointerSelected .or. oItem:IsForm
			IF (Control.ModifierKeys == Keys.Control .or. Control.ModifierKeys == Keys.Shift .or. eButton == MouseButtons.Middle) .and. !oItem:IsForm .and. !SELF:oWindowDesign:lSelected
				IF (oItem:IsColumn .and. SELF:SelectionHasNonColumns()) .or. ;
					(!oItem:IsColumn .and. SELF:SelectionHasColumns())
					SELF:DoAction(DesignerActionType.Select, oItem:cGuid )
				ELSE
					SELF:DoAction(DesignerActionType.SelectAdd, oItem:cGuid )
				ENDIF
			ELSE
				SELF:DoAction(DesignerActionType.Select, oItem:cGuid )
			ENDIF
			Application.DoEvents()
		ENDIF

		IF SELF:lReadOnly
			RETURN
		ENDIF

		DO CASE
		CASE eButton == MouseButtons.Right
			SELF:ShowContextMenu(oItem , oPoint)
		CASE eButton == MouseButtons.Left .and. SELF:oToolBox:PointerSelected
			oTemp := oItem:Control:PointToClient(oPoint)
			DO CASE
			CASE oItem:IsForm .or. (oItem:IsGroupBox .and. oTemp:Y > 12)
				SELF:oActionStart := oPoint
				SELF:oActionEnd := oPoint
				SELF:lMoved := FALSE
				SELF:eCurrent := WEDAction.Select
				SELF:oSurface:Capture := TRUE
			CASE !oItem:IsForm
				SELF:oActionStart := oPoint
				SELF:oActionEnd := oPoint
				SELF:lMoved := FALSE
				SELF:eCurrent := WEDAction.Move
				SELF:oSurface:Capture := TRUE
			END CASE
		CASE eButton == MouseButtons.Left .and. !SELF:oToolBox:PointerSelected
			SELF:oActionStart := oPoint
			SELF:oActionEnd := oPoint
			SELF:lMoved := FALSE
			SELF:eCurrent := WEDAction.Create
			SELF:oSurface:Capture := TRUE
		END CASE
	RETURN
	VIRTUAL METHOD ControlMouseMove(oItem AS DesignWindowItem , eButton AS MouseButtons , oPoint AS Point) AS VOID
		LOCAL oTemp AS Point
		
		IF SELF:oWindow == NULL .or. SELF:oWindowDesign == NULL
			RETURN
		ENDIF
		
		IF SELF:eCurrent != WEDAction.None .and. Control.MouseButtons == MouseButtons.None
			SELF:eCurrent := WEDAction.None
			RETURN
		ENDIF

		IF SELF:eCurrent != WEDAction.None
			IF SELF:lMoved
				SELF:DrawFrame()
			END IF
			SELF:oActionEnd := oPoint
			IF SELF:oActionEnd != SELF:oActionStart .or. SELF:lMoved
				SELF:lMoved := TRUE
				SELF:DrawFrame()
			END IF
		ENDIF
		
		oTemp := SELF:oWindow:PointToClient(oPoint)
		IF oItem != NULL
			SELF:SetText(oTemp:X:ToString() + " , " + oTemp:Y:ToString())
		ENDIF

		IF SELF:eCurrent == WEDAction.Move .or. SELF:eCurrent == WEDAction.Resize
			SELF:PrintStatus()
		END IF

		DO CASE
		CASE SELF:eCurrent == WEDAction.None .and. oItem != NULL
			IF SELF:oToolBox:PointerSelected
				oTemp := oItem:Control:PointToClient(oPoint)
				IF oItem:lSelected .and. !oItem:IsForm .and. (!oItem:IsGroupBox .or. oTemp:Y <= 12)
					oItem:Control:Cursor := Cursors.SizeAll
					Cursor.Current := Cursors.SizeAll
				ELSE
					oItem:Control:Cursor := Cursors.Default
					Cursor.Current := Cursors.Default
				ENDIF
			ELSE
				Cursor.Current := Cursors.Cross
				oItem:Control:Cursor := Cursors.Cross
			ENDIF
		CASE SELF:eCurrent == WEDAction.Create .and. oItem != NULL
			Cursor.Current := Cursors.Cross
			oItem:Control:Cursor := Cursors.Cross
		CASE SELF:eCurrent == WEDAction.Move .and. oItem != NULL
			Cursor.Current := Cursors.SizeAll
			oItem:Control:Cursor := Cursors.SizeAll

/*		CASE SELF:eCurrent == WEDAction.Resize
			DO CASE
			CASE SELF:nResizeGonia == 4 .or. SELF:nResizeGonia == 8
				Cursor.Current := Cursors.SizeWE
			CASE SELF:nResizeGonia == 2 .or. SELF:nResizeGonia == 6
				Cursor.Current := Cursors.SizeNS
			CASE SELF:nResizeGonia == 1 .or. SELF:nResizeGonia == 5
				Cursor.Current := Cursors.SizeNWSE
			CASE SELF:nResizeGonia == 3 .or. SELF:nResizeGonia == 7
				Cursor.Current := Cursors.SizeNESW
			END CASE*/

		END CASE

	RETURN
	VIRTUAL METHOD ControlMouseUp(oItem AS DesignWindowItem , e AS MouseButtons , oPoint AS Point) AS VOID
		LOCAL oRect AS Rectangle
		LOCAL n,m AS INT

		IF SELF:oWindow == NULL .or. SELF:oWindowDesign == NULL
			RETURN
		ENDIF
		IF SELF:lReadOnly
			RETURN
		ENDIF
		
		DO CASE

		CASE SELF:eCurrent == WEDAction.Move
//			SELF:oActionEnd := oPoint
//			IF SELF:oActionEnd != SELF:oActionStart
			IF SELF:lMoved
				SELF:DrawFrame()
				IF SELF:ViewMode == ViewMode.Browse
					LOCAL oTargetColumn AS DesignWindowItem
					LOCAL nTargetIndex AS INT
					LOCAL aNewOrder AS ArrayList
					LOCAL aSorted AS ArrayList
					LOCAL lLeft AS LOGIC
					oPoint := Point{SELF:oActionEnd:X , 0}
					oPoint := SELF:oWindow:PointToClient(oPoint)
					aSorted := SELF:GetAllColumnDesignItemsByIndex()
					FOR n := 0 UPTO aSorted:Count - 1
						oItem := (DesignWindowItem)aSorted[n]
						IF oItem:Column:Left < oPoint:X .and. oItem:Column:Right > oPoint:X
							oTargetColumn := oItem
							nTargetIndex := n
							lLeft := (oPoint:X - oItem:Column:Left) < (oItem:Column:Right - oPoint:X)
						END IF
					NEXT
					IF oTargetColumn != NULL .and. !SELF:aSelected:Contains(oTargetColumn)
						aNewOrder := ArrayList{}
						SELF:BeginAction()
						FOR n := 0 UPTO aSorted:Count - 1
							oItem := (DesignWindowItem)aSorted[n]
							DO CASE
							CASE SELF:aSelected:Contains(oItem)
							CASE n == nTargetIndex
								IF !lLeft
									aNewOrder:Add(oItem)
								END IF
								FOR m := 0 UPTO SELF:aSelected:Count - 1
									aNewOrder:Add(SELF:aSelected[m])
								NEXT
								IF lLeft
									aNewOrder:Add(oItem)
								END IF
							OTHERWISE
								aNewOrder:Add(oItem)
							END CASE
						NEXT
						FOR n := 0 UPTO aNewOrder:Count - 1
							oItem := (DesignWindowItem)aNewOrder[n]
							SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{oItem:cGuid , "_BrowseIndex" , n})
						NEXT
						SELF:EndAction()
					END IF
					
				ELSE
					SELF:BeginAction()
/*					IF SELF:SelectionHasColumns()
						FOR n := 0 UPTO SELF:aSelected:Count - 1
							oItem := (DesignWindowItem)SELF:aSelected[n]
							SELF:StartAction(DesignerBasicActionType.SetIndex , ActionData{oItem:cGuid , NULL , 0})
						NEXT
					ELSE*/
						FOR n := 0 UPTO SELF:aSelected:Count - 1
							oItem := (DesignWindowItem)SELF:aSelected[n]
							SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{oItem:cGuid , "_Left" , SELF:IntToGridX(oItem:Control:Left + SELF:oActionEnd:X - SELF:oActionStart:X)})
							SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{oItem:cGuid , "_Top" , SELF:IntToGridY(oItem:Control:Top + SELF:oActionEnd:Y - SELF:oActionStart:Y)})
						NEXT
//					ENDIF
					SELF:EndAction()
				END IF
			ENDIF
			SELF:eCurrent := WEDAction.None

		CASE SELF:eCurrent == WEDAction.Resize
			IF SELF:lMoved
				SELF:DrawFrame()
				SELF:BeginAction()
				IF SELF:ViewMode == ViewMode.Browse .and. !SELF:oWindowDesign:lSelected
					LOCAL nSize AS INT
					FOR n := 0 UPTO SELF:aSelected:Count - 1
						oItem := (DesignWindowItem)SELF:aSelected[n]
						nSize := oItem:BrowseSize
						nSize += (SELF:oActionEnd:X - SELF:oActionStart:X) / 6
						IF nSize < 0
							nSize := 0
						END IF
						SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{oItem:cGuid , "_BrowseSize" , nSize })
					NEXT
				ELSE
					FOR n := 0 UPTO SELF:aSelected:Count - 1
						oItem := (DesignWindowItem)SELF:aSelected[n]
						oRect := Rectangle{oItem:Control:Left , oItem:Control:Top , oItem:Control:Width , oItem:Control:Height}
						IF oItem:IsComboBox
							oRect:Height := (INT)oItem:GetProperty("_Height"):Value
						ENDIF
						oRect := SELF:AdjustResizeRect(oRect , SELF:oActionEnd:X - SELF:oActionStart:X , SELF:oActionEnd:Y - SELF:oActionStart:Y , SELF:nResizeGonia)
						SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{oItem:cGuid , "_Left" , oRect:Left })
						SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{oItem:cGuid , "_Top" , oRect:Top })
						SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{oItem:cGuid , "_Width" , oRect:Width })
						SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{oItem:cGuid , "_Height" , oRect:Height })
					NEXT
				END IF
				SELF:EndAction()
			ENDIF
			SELF:eCurrent := WEDAction.None

		CASE SELF:eCurrent == WEDAction.Create
			IF SELF:lMoved
				SELF:DrawFrame()
			ENDIF
			SELF:DoCreate()
			SELF:eCurrent := WEDAction.None
			SELF:oToolBox:SelectPointer()

		CASE SELF:eCurrent == WEDAction.Select
			IF SELF:lMoved
				SELF:DrawFrame()
			ENDIF
			SELF:eCurrent := WEDAction.None
			SELF:BeginAction()

			LOCAL aItems AS ArrayList
			LOCAL lInclude AS LOGIC
			LOCAL x,y,xx,yy AS INT
			LOCAL oDesign AS DesignWindowItem
			LOCAL lAdded AS LOGIC
			LOCAL oControl AS Control
			
			aItems := SELF:GetAllDesignItems()
			FOR n := 0 UPTO aItems:Count - 1
				oDesign := (DesignWindowItem)aItems[n]
				x := Math.Min(SELF:oActionStart:X , SELF:oActionEnd:X)
				y := Math.Min(SELF:oActionStart:Y , SELF:oActionEnd:Y)
				oPoint := Point{x,y}
				oPoint := SELF:oWindow:PointToClient(oPoint)
				x := oPoint:X
				y := oPoint:Y
				xx := x + Math.Abs(SELF:oActionStart:X - SELF:oActionEnd:X)
				yy := y + Math.Abs(SELF:oActionStart:Y - SELF:oActionEnd:Y)
				
				IF oDesign:lBrowseView
					oControl := oDesign:Column
				ELSE
					oControl := oDesign:Control
				END IF
				IF oControl == NULL .or. !oControl:Visible
					LOOP
				ENDIF
				
/*				IF (Glo.glPartialLasso .and. Control.ModifierKeys != Keys.Shift) .or. ;
					(!Glo.glPartialLasso .and. Control.ModifierKeys == Keys.Shift)
					lInclude := oDesign:x <= xx .and. oDesign:y <= yy .and. oDesign:x + oDesign:xs >= x .and. oDesign:y + oDesign:ys >= y
				ELSE
					lInclude := oDesign:x >= x .and. oDesign:y >= y .and. oDesign:x + oDesign:xs <= xx .and. oDesign:y + oDesign:ys <= yy
				ENDIF*/
				lInclude := oDesign:Control:Left >= x .and. oDesign:Control:Top >= y .and. oDesign:Control:Right <= xx .and. oDesign:Control:Bottom <= yy
	
				IF lInclude
					IF lAdded
						SELF:DoAction(DesignerActionType.SelectAdd , oDesign:cGuid)
					ELSE
						SELF:DoAction(DesignerActionType.Select , oDesign:cGuid)
						lAdded := TRUE
					ENDIF
				ENDIF
			NEXT
			SELF:EndAction()

		END CASE
		
		SELF:PrintStatus()
	RETURN

	VIRTUAL METHOD CancelAction() AS VOID
		IF SELF:eCurrent != WEDAction.None
			IF SELF:lMoved
				SELF:DrawFrame()
			ENDIF
			SELF:eCurrent := WEDAction.None
			SELF:PrintStatus()
		ENDIF
	RETURN

	VIRTUAL METHOD ControlGotFocus() AS VOID
		SELF:oDummy:Focus()
	RETURN

	PROTECTED VIRTUAL METHOD DrawFrame() AS VOID
		LOCAL oDesign AS DesignWindowItem
		LOCAL n,x,xx,y,yy AS INT
		LOCAL dx,dy AS INT
		LOCAL oRect AS Rectangle
		LOCAL oPoint AS Point
		LOCAL oSize AS Size
		LOCAL oControl AS Control

		dx := SELF:oActionEnd:X - SELF:oActionStart:X
		dy := SELF:oActionEnd:Y - SELF:oActionStart:Y

		DO CASE

		CASE SELF:eCurrent == WEDAction.Create

			IF SELF:oOptions:lUseGrid
				oPoint := Point{SELF:oActionStart:X , SELF:oActionStart:Y}
				oPoint := SELF:oWindow:PointToClient(oPoint)
				oPoint := SELF:PointToGrid(oPoint)
				oPoint := SELF:oWindow:PointToScreen(oPoint)
				oSize := Size{dx , dy}
				oSize := SELF:SizeToGrid(oSize)
				oRect := Rectangle{oPoint , oSize}
			ELSE
				oRect := Rectangle{SELF:oActionStart:X , SELF:oActionStart:Y , dx , dy}
			ENDIF
			ControlPaint.DrawReversibleFrame(oRect , Color.White , FrameStyle.Thick)

		CASE SELF:eCurrent == WEDAction.Select
			oRect := Rectangle{SELF:oActionStart:X , SELF:oActionStart:Y , dx , dy}
			ControlPaint.DrawReversibleFrame(oRect , Color.White , FrameStyle.Thick)

		CASE SELF:eCurrent == WEDAction.Move .or. SELF:eCurrent == WEDAction.Resize

			FOR n := 0 UPTO SELF:aSelected:Count - 1
				oDesign := (DesignWindowItem)SELF:aSelected[n]
				IF oDesign:lBrowseView
					oControl := oDesign:Column
				ELSE
					oControl := oDesign:Control
				END IF
				x := oControl:Left
				y := oControl:Top
				xx := oControl:Width
				yy := oControl:Height
				IF oDesign:IsComboBox .and. SELF:eCurrent == WEDAction.Resize
					yy := (INT)oDesign:GetProperty("_Height"):Value
				ENDIF
				DO CASE
				CASE SELF:eCurrent == WEDAction.Move
					x += dx
					y += dy
					oPoint := Point{x,y}
					oPoint := SELF:PointToGrid(oPoint)
//					oPoint := SELF:oWindow:PointToScreen(oPoint)
					oPoint := oDesign:Control:Parent:PointToScreen(oPoint)
					oRect := Rectangle{oPoint , Size{xx ,yy}}
					ControlPaint.DrawReversibleFrame(oRect , Color.White , FrameStyle.Thick)
				CASE SELF:eCurrent == WEDAction.Resize
					oRect := Rectangle{x,y,xx,yy}
					oRect := SELF:AdjustResizeRect(oRect , dx , dy , SELF:nResizeGonia)
					oPoint := Point{oRect:X , oRect:Y}
//					IF SELF:oWindowDesign:lSelected;oPoint := Point{0 , 0};ENDIF // TODO Big, ugly, dirty hack!
//					oPoint := SELF:oWindow:PointToScreen(oPoint)
					IF SELF:oWindowDesign:lSelected;oPoint := Point{5 , 5};ENDIF // TODO Big, ugly, dirty hack!
					oPoint := oDesign:Control:Parent:PointToScreen(oPoint)
					oRect := Rectangle{oPoint , oRect:Size}
					ControlPaint.DrawReversibleFrame(oRect , Color.White , FrameStyle.Thick)
				END CASE
			NEXT

		END CASE

	RETURN

	PROTECTED VIRTUAL METHOD DoCreate() AS VOID
		LOCAL x,y,xx,yy AS INT
		x := Math.Min(SELF:oActionStart:X , SELF:oActionEnd:X)
		y := Math.Min(SELF:oActionStart:Y , SELF:oActionEnd:Y)
		xx := Math.Abs(SELF:oActionStart:X - SELF:oActionEnd:X)
		yy := Math.Abs(SELF:oActionStart:Y - SELF:oActionEnd:Y)
		
		SELF:DoCreate(SELF:oToolBox:Selected , Point{x,y} , Size{xx,yy})
	RETURN
	PROTECTED VIRTUAL METHOD DoCreate(oTemplate AS VOControlTemplate , oPoint AS Point , oSize AS Size) AS VOID
		LOCAL oDesign AS DesignWindowItem
		LOCAL x,y,xx,yy AS INT
		LOCAL cGuid AS STRING
		
		IF SELF:ViewMode == ViewMode.Browse
			oTemplate := VOWindowEditorTemplate.Get("SINGLELINEEDIT")
			IF oTemplate == NULL
				RETURN
			END IF
			SELF:BeginAction()
			cGuid := Guid.NewGuid():ToString()
			oDesign := (DesignWindowItem)SELF:StartAction(DesignerBasicActionType.Create , ActionData{cGuid , oTemplate:cControl , ""})
			SELF:DoAction(DesignerActionType.Select , cGuid)
			SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "Name" , SELF:GetNextName(oTemplate:cName)})
			SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Left" , 2})
			SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Top" , 2})
			SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Width" , 80})
			SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Height" , 20})
//			oDesign:Deleted := 1
//			oDesign:BrowseIndex := SELF:GetAllColumnDesignItems():Count + 1
			SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Deleted" , 1})
			SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_BrowseIndex" , SELF:GetAllColumnDesignItems():Count + 1})
//			SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_BrowseIndex" , 1000})
			SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_BrowseSize" , 17})
			SELF:EndAction()
			SELF:oToolBox:SelectPointer()
			RETURN
		END IF
		
		IF oTemplate == NULL
			RETURN
		ENDIF
		
		oPoint := SELF:oWindow:PointToClient(oPoint)
		oPoint := SELF:PointToGrid(oPoint)
		x := oPoint:X
		y := oPoint:Y
		IF SELF:oOptions:lUseGrid
			x --
			y --
		ENDIF
		xx := oSize:Width
		yy := oSize:Height
		
		SELF:BeginAction()

		TRY

			cGuid := Guid.NewGuid():ToString()
			oDesign := (DesignWindowItem)SELF:StartAction(DesignerBasicActionType.Create , ActionData{cGuid , oTemplate:cControl , ""})
			SELF:DoAction(DesignerActionType.Select , cGuid)
			SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "Name" , SELF:GetNextName(oTemplate:cName)})
			IF oDesign:GetProperty("Caption") != NULL .and. !SELF:lLoading
				SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "Caption" , oTemplate:cStartText})
			END IF
			SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Left" , x})
			SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Top" , y})
			IF SELF:ViewMode == ViewMode.Auto .and. oDesign:Column != NULL
				SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_BrowseIndex" , 0})
				SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_BrowseSize" , 17})
			ELSE
				SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_BrowseIndex" , -1})
			ENDIF
			IF xx < 10 .or. yy < 10
				xx := oDesign:Control:Width
				yy := oDesign:Control:Height
			ENDIF
			xx := SELF:IntToGridX(xx)
			yy := SELF:IntToGridX(yy)
			IF SELF:oOptions:lUseGrid
				xx ++
				yy ++
			ENDIF
			SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Width" , xx})
			SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Height" , yy})
			
			IF oDesign:IsTabControl
				((DesignTabControl)oDesign:Control):AddPage()
			ENDIF

		CATCH

			Funcs.ErrorBox("Error creating control.")
			
		END TRY

		SELF:EndAction()
		
		SELF:oToolBox:SelectPointer()
		
/*		SELF:UpdateFieldSpecProperties()
		SELF:UpdateMenuProperties()
		SELF:UpdateDBServerProperties()
		
		SELF:ArrangeCreationOrder()*/
		
	RETURN

	PROTECTED METHOD GetNextName(cName AS STRING) AS STRING
		LOCAL cNext AS STRING
		LOCAL n AS INT
		DO WHILE TRUE
			n ++
			cNext := cName + n:ToString()
			IF !SELF:NameExists(cNext)
				EXIT
			ENDIF
		END DO
	RETURN cNext

	PROTECTED METHOD NameExists(cName AS STRING) AS LOGIC
		LOCAL oDesign AS DesignWindowItem
		LOCAL aDesign AS ArrayList
		LOCAL n AS INT
		cName := cName:ToUpper():Trim()
		aDesign := SELF:GetAllDesignItems()
		FOR n := 0 UPTO aDesign:Count - 1
			oDesign := (DesignWindowItem)aDesign[n]
			IF oDesign:GetProperty("Name"):TextValue:ToUpper():Trim() == cName
				RETURN TRUE
			ENDIF
		NEXT
	RETURN FALSE

	PROTECTED VIRTUAL METHOD AdjustResizeRect(oRect AS Rectangle , nDifX AS INT,nDifY AS INT , nGonia AS INT) AS Rectangle
		LOCAL x1,x2,y1,y2 AS INT
/*		LOCAL lGrid AS LOGIC
	
		lGrid := SELF:UseGrid*/
	
		x1 := oRect:Left
		x2 := oRect:Right
		y1 := oRect:Top
		y2 := oRect:Bottom
		
		IF nGonia == 1 .or. nGonia == 7 .or. nGonia == 8
			x1 += nDifX
			IF x1 > x2
				x1 := x2
			ENDIF
			IF SELF:oOptions:lUseGrid
				x1 := SELF:IntToGridX(x1)
			ENDIF
		ENDIF
		IF nGonia == 3 .or. nGonia == 4 .or. nGonia == 5
			x2 += nDifX
			IF x2 < x1
				x2 := x1
			ENDIF
			IF SELF:oOptions:lUseGrid
				x2 := SELF:IntToGridX(x2)
			ENDIF
		ENDIF
		IF nGonia == 1 .or. nGonia == 2 .or. nGonia == 3
			y1 += nDifY
			IF y1 > y2
				y1 := y2
			ENDIF
			IF SELF:oOptions:lUseGrid
				y1 := SELF:IntToGridY(y1)
			ENDIF
		ENDIF
		IF nGonia == 5 .or. nGonia == 6 .or. nGonia == 7
			y2 += nDifY
			IF y2 < y1
				y2 := y1
			ENDIF
			IF SELF:oOptions:lUseGrid
				y2 := SELF:IntToGridY(y2)
			ENDIF
		ENDIF

	RETURN Rectangle{ x1 , y1 , x2 - x1 , y2 - y1}



	VIRTUAL METHOD BeginAction() AS VOID
		IF SELF:nActionDepth == 0
			SELF:aAffected:Clear()
			SELF:lDidAction := FALSE
			SELF:lClearUndoBuffer := FALSE
		ENDIF
		SELF:nActionDepth ++
	RETURN
	VIRTUAL METHOD EndAction() AS VOID
		LOCAL oDesign AS DesignWindowItem
		LOCAL oAction AS DesignerBasicAction
		LOCAL lDefault AS LOGIC
//		LOCAL lUpdateGuids AS LOGIC
		LOCAL n AS INT

		IF SELF:aSelected:Count == 0 .and. SELF:nActionDepth == 1
//			SELF:StartAction(DesignerActionType.Select , SELF:oWindowDesign:cGuid)
			SELF:DoAction(DesignerActionType.Select , SELF:oWindowDesign:cGuid)
		ENDIF
	
		SELF:nActionDepth --
	
		IF SELF:nActionDepth == 0
	
			IF SELF:lClearUndoBuffer
				SELF:nAction := 0
				SELF:aActions:Clear()
			ENDIF
			IF SELF:nAction != 0
				oAction := (DesignerBasicAction)SELF:aActions[SELF:nAction - 1]
				oAction:lGroup := TRUE
			ENDIF
			FOR n := 0 UPTO SELF:aSelected:Count - 1
				oDesign  := (DesignWindowItem)SELF:aSelected[n]
				lDefault := n == SELF:aSelected:Count - 1
				IF oDesign:lDefault != lDefault
					SELF:AddAffected(oDesign)
					oDesign:lDefault := lDefault
				ENDIF
			NEXT
			
			IF SELF:ViewMode == ViewMode.Browse
				SELF:ArrangeColumnsOrder()
			END IF
			
			FOR n := 0 UPTO SELF:aAffected:Count - 1
				oDesign := SELF:GetDesignItemFromGuid(SELF:aAffected[n])
				IF oDesign != NULL
/*					oDesign:AdjustSizePos()*/
					oDesign:AdjustSelectors()
/*					IF oDesign:nType == 3 .or. oDesign:nType == 51
						lUpdateGuids := TRUE
					ENDIF*/
				ENDIF
			NEXT
	
			IF oAction != NULL .and. oAction:aSelected:Count == 0
				FOR n := 0 UPTO SELF:aSelected:Count - 1
					oDesign := (DesignWindowItem)SELF:aSelected[n]
					IF !oDesign:IsForm
						oAction:aSelected:Add(oDesign:cGuid)
					ENDIF
				NEXT
			ENDIF
	
			
			FOR n := 0 UPTO SELF:aSelected:Count - 1
				oDesign  := (DesignWindowItem)SELF:aSelected[n]
				IF oDesign:IsTabControl
					LOCAL oTabControl AS DesignTabControl
					LOCAL oPage AS VOTabPageOptions
					oTabControl := (DesignTabControl)oDesign:Control
					oPage := oTabControl:GetTabPageOptions()
					IF oDesign:GetProperty("_PageName") != NULL
						oDesign:GetProperty("_PageName"):Value := oPage:cName
					ENDIF
					IF oDesign:GetProperty("_PageCaption") != NULL
						oDesign:GetProperty("_PageCaption"):Value := oPage:cCaption
					ENDIF
					IF oDesign:GetProperty("_IsDataPage") != NULL
						oDesign:GetProperty("_IsDataPage"):Value := iif(oPage:lDataAware , 0 , 1)
					ENDIF
				ENDIF
				
				IF oDesign:Control:GetType() == TypeOf(DesignEmpty)
					oDesign:Control:Invalidate()
				ENDIF
				
			NEXT
			
			
			SELF:oGrid:Fill(SELF:aSelected)
			SELF:UpdateControlOrders()
//			((DesignWindowItem)SELF:aSelected[ self:aSelected:Count - 1 ]):Control:BringToFront()
//			((DesignWindowItem)SELF:aSelected[0]):AdjustSelectors()

	
/*			Application.DoEvents()
			System.Threading.Thread.Sleep(20)
			FOR n := 0 UPTO SELF:oWindow:Controls:Count - 1
				SELF:oWindow:Controls[n]:Invalidate()
			NEXT
			SELF:oWindow:Invalidate()
			SELF:oSurface:Invalidate()*/
//			SELF:oSurface:Invalidate(TRUE)

            IF SELF:IsDirtyChanged != NULL
                SELF:IsDirtyChanged:Invoke(SELF , EventArgs{})
            ENDIF
            
            SELF:PrintStatus()
	
		ENDIF
	
	RETURN

	PROTECTED VIRTUAL METHOD UpdateControlOrders() AS VOID
		LOCAL oDesign AS DesignWindowItem
		oDesign := ((DesignWindowItem)SELF:aSelected[ SELF:aSelected:Count - 1 ])
		IF !oDesign:IsForm .and. !oDesign:IsGroupBox .and. !oDesign:IsColumn
			oDesign:Control:BringToFront()
		ENDIF
		
		AdjustGroupBoxesInForm(SELF:oWindow)
	RETURN
		
	PROTECTED STATIC METHOD AdjustGroupBoxesInForm(oForm AS Control) AS VOID
		LOCAL oBoxes AS SortedList<INT,Control>
		LOCAL oControl AS Control
		LOCAL oType AS Type
		LOCAL n,nKey AS INT

		oBoxes := SortedList<Int32,Control>{}
		FOR n := 0 UPTO oForm:Controls:Count - 1
			oControl := oForm:Controls[n]
			oType := oControl:GetType()
			IF oType == TypeOf(GroupBox) .or. oType:BaseType == TypeOf(GroupBox)
				nKey := oControl:Width * oControl:Height
				DO WHILE oBoxes:IndexOfKey(nKey) != -1
					nKey ++
				END DO
				oBoxes:Add(nKey , oControl)
			ENDIF
		NEXT
		FOR n := 0 UPTO oBoxes:Count - 1
//			oBoxes:GetByIndex(n):SendToBack()
			oBoxes:Values[n]:SendToBack()
		NEXT

	RETURN

	VIRTUAL METHOD DoBasicAction(oAction AS DesignerBasicAction , eAction AS DesignerBasicActionType , uData AS ActionData) AS OBJECT
//		LOCAL oDesign,oParent,oOldParent,oChild AS DesignWindowItem
		LOCAL oDesign AS DesignWindowItem
		LOCAL oUndo,oRedo AS DesignerBasicAction
		LOCAL oProp AS VODesignProperty
		LOCAL cGuid , cParent AS STRING
		LOCAL aProperties AS NameValueCollection
		LOCAL oEnumerator AS IEnumerator
		LOCAL oNameValue AS NameValue
		LOCAL oParent AS Control
		LOCAL oRet AS OBJECT
		LOCAL n AS INT

		DO CASE
		CASE eAction == DesignerBasicActionType.Create
			cGuid := uData:cGuid
			cParent := (STRING)uData:oData
			IF cParent == NULL
				oParent := SELF:oSurface
			ELSE
				oParent := SELF:oWindow
				IF cParent != ""
					oParent := ((DesignWindowItem)SELF:aSelected[0]):Control
				ENDIF
			ENDIF
			oDesign := SELF:CreateControl(uData:cData , oParent , cGuid)
			oRet := oDesign
			SELF:lDidAction := TRUE
			IF !oAction:lExecuted
				oUndo := DesignerBasicAction{TRUE}
				oUndo:eAction := DesignerBasicActionType.Remove
				oUndo:uData := ActionData{oDesign:cGuid}
				oAction:aUndo:Add(oUndo)
				
				oRedo := DesignerBasicAction{TRUE}
				oRedo:eAction := DesignerBasicActionType.Create
				oRedo:uData := ActionData{cGuid , oDesign:cControl , cParent}
				oAction:aRedo:Add(oRedo)
			ENDIF
			IF uData:aData != NULL
				oEnumerator := uData:aData:GetEnumerator()
				DO WHILE oEnumerator:MoveNext()
					oNameValue := (NameValue)oEnumerator:Current
					SELF:DoBasicAction(oAction , DesignerBasicActionType.SetProperty , ActionData{cGuid , oNameValue:Name , oNameValue:Value})
				END DO
			ENDIF
			SELF:AddAffected(oDesign)
			
		CASE eAction == DesignerBasicActionType.Remove
			cGuid := uData:cGuid
			oDesign := SELF:GetDesignItemFromGuid(cGuid)
			IF oDesign:lSelected
				SELF:DoAction(DesignerActionType.DeSelect , cGuid)
			ENDIF
			oDesign:@@Delete()
			SELF:lDidAction := TRUE
			IF !oAction:lExecuted
				aProperties := NameValueCollection{}
				FOR n := 0 UPTO oDesign:aProperties:Count - 1
					oProp := (VODesignProperty)oDesign:aProperties[n]
					IF !oProp:IsAuto
						aProperties:Add(oProp:Name , oProp:Value)
					ENDIF
				NEXT

				oUndo := DesignerBasicAction{TRUE}
				oUndo:eAction := DesignerBasicActionType.Create
				oUndo:uData := ActionData{oDesign:cGuid , oDesign:cControl , "" , aProperties}
				oAction:aUndo:Add(oUndo)
				
				oRedo := DesignerBasicAction{TRUE}
				oRedo:eAction := DesignerBasicActionType.Remove
				oRedo:uData := ActionData{oDesign:cGuid}
				oAction:aRedo:Add(oRedo)
			ENDIF

		CASE eAction == DesignerBasicActionType.SetProperty
			oDesign := SELF:GetDesignItemFromGuid(uData:cGuid)
			oProp := oDesign:GetProperty(uData:cData)
			IF !oProp:TextValue == oProp:GetTextValue(uData:oData)
				SELF:lDidAction := TRUE
				IF !oAction:lExecuted
					oUndo := DesignerBasicAction{TRUE}
					oUndo:eAction := DesignerBasicActionType.SetProperty
					oUndo:uData := ActionData{oDesign:cGuid , uData:cData , oProp:Value}
					oAction:aUndo:Add(oUndo)
					
					oRedo := DesignerBasicAction{TRUE}
					oRedo:eAction := DesignerBasicActionType.SetProperty
					oRedo:uData := ActionData{oDesign:cGuid , uData:cData , uData:oData}
					oAction:aRedo:Add(oRedo)
				ENDIF
				oProp:Value := uData:oData
				SELF:PropertyGotUpdated(oDesign , oProp)
				SELF:AddAffected(oDesign)
			ENDIF
	

		CASE eAction == DesignerBasicActionType.SetIndex
			oDesign := SELF:GetDesignItemFromGuid(uData:cGuid)
			n := INT(uData:oData)
			IF oDesign:Control:Parent:Controls:GetChildIndex(oDesign:Control) != n
				oDesign:Control:Parent:Controls:SetChildIndex(oDesign:Control , n)
				SELF:lDidAction := TRUE
/*				IF !oAction:lExecuted
					oUndo := DesignerBasicAction{TRUE}
					oUndo:eAction := DesignerBasicActionType.SetProperty
					oUndo:uData := ActionData{oDesign:cGuid , uData:cData , oProp:Value}
					oAction:aUndo:Add(oUndo)
					
					oRedo := DesignerBasicAction{TRUE}
					oRedo:eAction := DesignerBasicActionType.SetProperty
					oRedo:uData := ActionData{oDesign:cGuid , uData:cData , uData:oData}
					oAction:aRedo:Add(oRedo)
				ENDIF*/
				SELF:AddAffected(oDesign)
			ENDIF
	
		END CASE

		oAction:lExecuted := TRUE

	RETURN oRet

	VIRTUAL METHOD DoAction(eAction AS DesignerActionType) AS VOID
		SELF:DoAction(eAction , NULL)
	RETURN
	VIRTUAL METHOD DoAction(eAction AS DesignerActionType , cGuid AS STRING) AS VOID
//		LOCAL oDesign,oParent,oOldParent,oChild AS DesignWindowItem
		LOCAL oDesign AS DesignWindowItem
		LOCAL aItems AS ArrayList
		LOCAL n AS INT

		SELF:BeginAction()

		DO CASE
		CASE eAction == DesignerActionType.Select
			oDesign := SELF:GetDesignItemFromGuid(cGuid)
			IF !oDesign:lSelected
				SELF:DoAction(DesignerActionType.DeSelectAll)
				oDesign:lSelected := TRUE
				SELF:aSelected:Add(oDesign)
				SELF:AddAffected(oDesign)
			ENDIF
		CASE eAction == DesignerActionType.SelectAdd
			oDesign := SELF:GetDesignItemFromGuid(cGuid)
			IF !oDesign:lSelected
				oDesign:lSelected := TRUE
				SELF:aSelected:Add(oDesign)
				SELF:AddAffected(oDesign)
			ENDIF
		CASE eAction == DesignerActionType.SelectDefault
			IF SELF:aSelected:Count >= 2
				oDesign := SELF:GetDesignItemFromGuid(cGuid)
				IF !oDesign:IsForm
					FOR n := 0 UPTO SELF:aSelected:Count - 1
						IF (DesignWindowItem)SELF:aSelected[n] == oDesign
							SELF:aSelected:RemoveAt(n)
							SELF:aSelected:Add(oDesign)
							SELF:AddAffected(oDesign)
							EXIT
						ENDIF
					NEXT
				ENDIF
			ENDIF
		CASE eAction == DesignerActionType.SelectAll
			LOCAL lBrowseView AS LOGIC
			lBrowseView := SELF:ViewMode == ViewMode.Browse
			SELF:DoAction(DesignerActionType.DeSelectAll)
			aItems := SELF:GetAllDesignItems()
			FOR n := 0 UPTO aItems:Count - 1
				oDesign := (DesignWindowItem)aItems[n]
				IF lBrowseView
					IF oDesign:Column == NULL .or. oDesign:BrowseIndex == -1
						LOOP
					END IF
				ELSE
					IF !oDesign:Control:Visible
						LOOP
					END IF
				END IF
				SELF:DoAction(DesignerActionType.SelectAdd , oDesign:cGuid)
			NEXT
			
			
		CASE eAction == DesignerActionType.DeSelect
//			oDesign := SELF:GetDesignItemFromGuid(cData)
//			oDesign:lSelected := FALSE
//			SELF:aSelected:Remove(oDesign)
			FOR n := 0 UPTO SELF:aSelected:Count - 1
				oDesign := (DesignWindowItem)SELF:aSelected[n]
				IF oDesign:cGuid == cGuid
					SELF:aSelected:RemoveAt(n)
					oDesign:lSelected := FALSE
					EXIT
				ENDIF
			NEXT
			
			SELF:AddAffected(oDesign)

		CASE eAction == DesignerActionType.RemoveSelected
			SELF:BeginAction()
//			SELF:MakeSelectionMovable()
			DO WHILE SELF:aSelected:Count != 0
				oDesign := (DesignWindowItem)SELF:aSelected[0]
				IF !oDesign:IsForm
					DO CASE
					CASE SELF:ViewMode == ViewMode.Auto
						SELF:StartAction(DesignerBasicActionType.Remove , ActionData{oDesign:cGuid})
					CASE SELF:ViewMode == ViewMode.Form
						IF oDesign:BrowseIndex == -1
							SELF:StartAction(DesignerBasicActionType.Remove , ActionData{oDesign:cGuid})
						ELSE
							SELF:DoAction(DesignerActionType.DeSelect , oDesign:cGuid)
							SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{oDesign:cGuid , "_Deleted" , 1} )
						END IF
					CASE SELF:ViewMode == ViewMode.Browse
						IF oDesign:Deleted == 1
							SELF:StartAction(DesignerBasicActionType.Remove , ActionData{oDesign:cGuid})
						ELSE
							SELF:DoAction(DesignerActionType.DeSelect , oDesign:cGuid)
							SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{oDesign:cGuid , "_BrowseIndex" , -1} )
						END IF
					END CASE
				ENDIF
			ENDDO
			SELF:EndAction()

		CASE eAction == DesignerActionType.DeSelectAll
			DO WHILE SELF:aSelected:Count != 0
				oDesign := (DesignWindowItem)SELF:aSelected[0]
				SELF:DoAction(DesignerActionType.DeSelect , oDesign:cGuid)
			ENDDO
		CASE eAction == DesignerActionType.Cut
			SELF:Cut()
		CASE eAction == DesignerActionType.Copy
			SELF:Copy()
		CASE eAction == DesignerActionType.Paste
			SELF:Paste()

		CASE eAction == DesignerActionType.Undo
			SELF:Undo()
		CASE eAction == DesignerActionType.Redo
			SELF:Redo()

		CASE eAction == DesignerActionType.AddColumn
			IF SELF:aSelected:Count == 1
				oDesign := (DesignWindowItem)SELF:aSelected[0]
				IF oDesign:IsBrowser
					SELF:AddColumn(oDesign)
				ENDIF
			ENDIF

		CASE eAction == DesignerActionType.RemoveSelected
			SELF:DoAction(DesignerActionType.RemoveSelected)
		CASE eAction >= DesignerActionType.AlignLeft .and. eAction <= DesignerActionType.SameVerSize
			SELF:DoAlignAction(eAction)
		CASE eAction >= DesignerActionType.CenterHorz .and. eAction <= DesignerActionType.SpacingHorzEqual
			SELF:DoSpacingAction(eAction)
		END CASE

		SELF:EndAction()

	RETURN

	PRIVATE METHOD DoAlignAction(eAction AS DesignerActionType) AS VOID
		LOCAL oDesign AS DesignWindowItem
		LOCAL oControl AS Control
		LOCAL x,y,xx,yy,xs,ys AS INT
		LOCAL cGuid AS STRING
		LOCAL n AS INT
		
		oDesign := (DesignWindowItem) SELF:aSelected[SELF:aSelected:Count - 1]
		oControl := oDesign:Control
		x := oControl:Left
		y := oControl:Top
		xs := oControl:Width
		ys := oControl:Height
		xx := x + xs
		yy := y + ys
		SELF:BeginAction()
		FOR n := 0 UPTO SELF:aSelected:Count - 2
			oDesign := (DesignWindowItem) SELF:aSelected[n]
			oControl := oDesign:Control
			cGuid := oDesign:cGuid
			DO CASE
			CASE eAction == DesignerActionType.AlignLeft
				SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{oDesign:cGuid , "_Left" , x} )
			CASE eAction == DesignerActionType.AlignRight
				SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Left" , xx - oControl:Width} )
			CASE eAction == DesignerActionType.AlignTop
				SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Top" , y} )
			CASE eAction == DesignerActionType.AlignBottom
				SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Top" , yy - oControl:Height} )
			CASE eAction == DesignerActionType.AlignCenterHorz
				SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Left" , x + xs / 2 - oControl:Width / 2} )
			CASE eAction == DesignerActionType.AlignCenterVert
				SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Top" , y + ys / 2 - oControl:Height / 2} )
				
			CASE eAction == DesignerActionType.ResizeAlignLeft
				SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Width" , oControl:Left + oControl:Width - x} )
				SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Left" , x} )
			CASE eAction == DesignerActionType.ResizeAlignRight
				SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Width" , xx - oControl:Left} )
			CASE eAction == DesignerActionType.ResizeAlignTop
				SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Height" , oControl:Top + oControl:Height - y} )
				SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Top" , y} )
			CASE eAction == DesignerActionType.ResizeAlignBottom
				SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Height" , yy - oControl:Top} )
				
			CASE eAction == DesignerActionType.SameHorSize
				SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Width" , xs} )
			CASE eAction == DesignerActionType.SameVerSize
				SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Height" , ys} )
			CASE eAction == DesignerActionType.SameSize
				SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Width" , xs} )
				SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Height" , ys} )
				
			END CASE
		NEXT
		SELF:EndAction()
	RETURN
	
	PRIVATE METHOD DoSpacingAction(eAction AS DesignerActionType) AS VOID
		LOCAL oDesign AS DesignWindowItem
		LOCAL oControl AS Control 
		LOCAL nX,nY,nXS,nYS AS INT
		LOCAL nMaxX,nMinX,nMaxY,nMinY AS INT
		LOCAL nTotalSize AS INT
		LOCAL nSpacing AS INT
		LOCAL n,nn AS INT
		LOCAL oSorted AS SortedList
		LOCAL lHorz AS LOGIC
		
		nMinX := 78686
		nMinY := 78686
		nMaxX := - 76757
		nMaxY := - 76757
		
		lHorz := eAction == DesignerActionType.SpacingHorzEqual .or. eAction == DesignerActionType.SpacingHorzRem .or. ;
				eAction == DesignerActionType.SpacingHorzInc .or. eAction == DesignerActionType.SpacingHorzDec .or. ;
				eAction == DesignerActionType.CenterHorz
		
		oSorted := System.Collections.SortedList{SELF:aSelected:Count}
	
		FOR n := 0 UPTO SELF:aSelected:Count - 1
			oDesign := (DesignWindowItem) SELF:aSelected[n]
			oControl := oDesign:Control
			nX := oControl:Left
			nY := oControl:Top
			nXS := oControl:Width
			nYS := oControl:Height
			
			IF nX < nMinX
				nMinX := nX
			ENDIF
			IF (nX + nXS) > nMaxX
				nMaxX := nX + nXS
			ENDIF
			IF nY < nMinY
				nMinY := nY
			ENDIF
			IF (nY + nYS) > nMaxY
				nMaxY := nY + nYS
			ENDIF
			
//			IF eAction == DesignerActionType.SpacingHorzEqual
			IF lHorz
				nTotalSize += nXS
		        DO WHILE oSorted:ContainsKey(nX)
		        	nX ++
		        END DO
				oSorted:Add(nX , oDesign)
//			ELSEIF eAction == DesignerActionType.SpacingVertEqual
			ELSE
				nTotalSize += nYS
		        DO WHILE oSorted:ContainsKey(nY)
		        	nY ++
		        END DO
				oSorted:Add(nY , oDesign)
			ENDIF
			
		NEXT
		
		IF eAction == DesignerActionType.CenterHorz .or. eAction == DesignerActionType.CenterVert
			SELF:BeginAction()
			FOR n := 0 UPTO SELF:aSelected:Count - 1
				oDesign := (DesignWindowItem) SELF:aSelected[n]
				oControl := oDesign:Control
				nX := oControl:Left
				nY := oControl:Top
				nXS := oControl:Width
				nYS := oControl:Height
				
				DO CASE
				CASE eAction == DesignerActionType.CenterHorz
					nX := nX + (SELF:oWindow:ClientSize:Width / 2 - (nMinX + nMaxX) / 2)
				CASE eAction == DesignerActionType.CenterVert
					nY := nY + (SELF:oWindow:ClientSize:Height / 2 - (nMinY + nMaxY) / 2)
				ENDCASE
		
				IF nX != oControl:Left
					SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{oDesign:cGuid , "_Left" , nX} )
				ENDIF
				IF nY != oControl:Top
					SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{oDesign:cGuid , "_Top" , nY} )
				ENDIF
			NEXT	
			SELF:EndAction()
	
		ELSE
			
			IF SELF:aSelected:Count < 3
				RETURN
			ENDIF
			
			oDesign := (DesignWindowItem)oSorted:GetByIndex(0)
			oControl := oDesign:Control
			IF lHorz
				nSpacing := ((nMaxX - nMinX) - nTotalSize) / (SELF:aSelected:Count - 1)
				DO CASE
				CASE eAction == DesignerActionType.SpacingHorzInc
					nSpacing += 5
				CASE eAction == DesignerActionType.SpacingHorzDec
					nSpacing -= 5
				CASE eAction == DesignerActionType.SpacingHorzRem
					nSpacing := 0
				END CASE
				nn := oControl:Left + oControl:Width + nSpacing 
			ELSE
				nSpacing := ((nMaxY - nMinY) - nTotalSize) / (SELF:aSelected:Count - 1)
				DO CASE
				CASE eAction == DesignerActionType.SpacingVertInc
					nSpacing += 5
				CASE eAction == DesignerActionType.SpacingVertDec
					nSpacing -= 5
				CASE eAction == DesignerActionType.SpacingVertRem
					nSpacing := 0
				END CASE
				nn := oControl:Top + oControl:Height + nSpacing
			END IF
			
			IF nSpacing < 0
				RETURN
			ENDIF
	
			SELF:BeginAction()
			FOR n := 1 UPTO oSorted:Count - 1
				oDesign := (DesignWindowItem)oSorted:GetByIndex(n)
				oControl := oDesign:Control
				IF lHorz
					SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{oDesign:cGuid , "_Left" , nn} )
					nn += oControl:Width + nSpacing
				ELSE
					SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{oDesign:cGuid , "_Top" , nn} )
					nn += oControl:Height + nSpacing
				ENDIF
			NEXT
			SELF:EndAction()
	
		END IF
	
	RETURN

	PRIVATE METHOD AddColumn(oParent AS DesignWindowItem) AS VOID
		LOCAL oDesign AS DesignWindowItem
		LOCAL oTemplate AS VOControlTemplate
		LOCAL cGuid AS STRING
		
		oTemplate := VOWindowEditorTemplate.Get("BDATACOLUMN")
		
		SELF:BeginAction()
		cGuid := Guid.NewGuid():ToString()
		oDesign := (DesignWindowItem)SELF:StartAction(DesignerBasicActionType.Create , ActionData{cGuid , oTemplate:cControl , oParent:cGuid})
		oDesign:Control:BringToFront()
		SELF:DoAction(DesignerActionType.Select , cGuid)
		SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "Name" , SELF:GetNextName("bDataColumn")})
		SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Width" , 100})
		SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "Caption" , "bDataColumn"})
/*		
		IF oDesign:GetProperty("Caption") != NULL
			SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "Caption" , oTemplate:cStartText})
		END IF
		SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Left" , x})
		SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Top" , y})
		IF xx < 10 .or. yy < 10
			xx := oDesign:Control:Width
			yy := oDesign:Control:Height
		ENDIF
		xx := SELF:IntToGridX(xx)
		yy := SELF:IntToGridX(yy)
		IF SELF:oOptions:lUseGrid
			xx ++
			yy ++
		ENDIF
		SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Width" , xx})
		SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Height" , yy})
		*/
		SELF:EndAction()

	RETURN
	
	VIRTUAL METHOD DoMoveSelected(eDirection AS Direction , lAlternate AS LOGIC) AS VOID
		LOCAL oDesign AS DesignWindowItem
		LOCAL x,y AS Int32
		LOCAL nOffsetX AS INT
		LOCAL nOffsetY AS INT
		LOCAL lGrid AS LOGIC
		LOCAL n AS INT
		
		IF SELF:oWindowDesign:lSelected
			RETURN
		END IF
		
		FOR n := 0 UPTO SELF:aSelected:Count - 1
			oDesign := (DesignWindowItem)SELF:aSelected[n]
			IF oDesign:lLocked
				RETURN
			ENDIF
		NEXT
		
		lGrid := SELF:oOptions:lUseGrid .xor. lAlternate
		IF lGrid
			nOffsetX := SELF:oOptions:oGridSize:Width
			nOffsetY := SELF:oOptions:oGridSize:Height
		ELSE
			nOffsetX := 1
			nOffsetY := 1
		ENDIF
		
		SELF:BeginAction()
		FOR n := 0 UPTO SELF:aSelected:Count - 1
			oDesign := (DesignWindowItem)SELF:aSelected[n]
			x := oDesign:Control:Left
			y := oDesign:Control:Top
			DO CASE
			CASE eDirection == Direction.Left
				x -= nOffsetX
				IF lGrid
					x := SELF:IntToGridX(x)
				ENDIF
			CASE eDirection == Direction.Right
				x += nOffsetX
				IF lGrid
					x := SELF:IntToGridX(x)
				ENDIF
			CASE eDirection == Direction.Up
				y -= nOffsetY
				IF lGrid
					y := SELF:IntToGridY(y)
				ENDIF
			CASE eDirection == Direction.Down
				y += nOffsetY
				IF lGrid
					y := SELF:IntToGridY(y)
				ENDIF
			END CASE
			SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{oDesign:cGuid , "_Left" , x })
			SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{oDesign:cGuid , "_Top" , y })
		NEXT
		SELF:EndAction()
	RETURN


	#region Clipboard Actions

	VIRTUAL METHOD Cut() AS VOID
		IF SELF:lReadOnly
			RETURN
		ENDIF
		SELF:Copy()
		SELF:DoAction(DesignerActionType.RemoveSelected)
	RETURN
	VIRTUAL METHOD Copy() AS VOID
		LOCAL oDesign AS DesignWindowItem
//		LOCAL lInclude AS LOGIC
		LOCAL n AS INT

		IF SELF:oWindowDesign:lSelected .or. SELF:aSelected:Count == 0
			RETURN
		ENDIF
	
		Clipboard:Clear()
		FOR n := 0 UPTO SELF:aSelected:Count - 1
			oDesign := (DesignWindowItem) SELF:aSelected[n]
			SELF:CopyDesigner(oDesign , "")
		NEXT
		SELF:eCopyMode := SELF:ViewMode
	
	RETURN
	PROTECTED VIRTUAL METHOD CopyDesigner(oDesign AS DesignWindowItem , cParent AS STRING) AS VOID
		LOCAL oEntry AS DesignerClipboardEntry
		LOCAL oProp AS VODesignProperty
		LOCAL n AS INT
	
		oEntry := DesignerClipboardEntry{}
		oEntry:cGuid := oDesign:cGuid
		oEntry:cClass := oDesign:cControl
		oEntry:cName := oDesign:Name
		oEntry:x := oDesign:Control:Left
		oEntry:y := oDesign:Control:Top
		oEntry:cParent := cParent
		FOR n := 0 UPTO oDesign:aProperties:Count - 1
			oProp := (VODesignProperty)oDesign:aProperties[n]
			IF !oProp:IsAuto
				oEntry:aProperties:Add(oProp:Name , oProp:Value)
			ENDIF
		NEXT
		Clipboard:AddEntry(oEntry)
		
	RETURN
	
	VIRTUAL METHOD Paste() AS VOID
		LOCAL oEntry AS DesignerClipboardEntry
		LOCAL lNameConflict AS LOGIC
		LOCAL cGuid AS STRING
		LOCAL n,m AS INT
	
		IF SELF:lReadOnly
			RETURN
		ENDIF
		
		IF SELF:ViewMode == ViewMode.Browse .and. SELF:eCopyMode != ViewMode.Browse
			RETURN
		END IF
		IF SELF:ViewMode != ViewMode.Browse .and. SELF:eCopyMode == ViewMode.Browse
			RETURN
		END IF

		SELF:BeginAction()
		SELF:DoAction(DesignerActionType.DeSelectAll)
		FOR n := 0 UPTO Clipboard:Count - 1
			oEntry := Clipboard:GetEntry(n)
			lNameConflict := SELF:NameExists(oEntry:cName)
			oEntry:nPasted ++
			cGuid := Guid.NewGuid():ToString()
			SELF:StartAction(DesignerBasicActionType.Create , ActionData{cGuid , oEntry:cClass , ""})
			FOR m := 0 UPTO oEntry:aProperties:Count - 1
				SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , oEntry:aProperties:GetName(m) , oEntry:aProperties:GetValue(m)})
			NEXT
			IF lNameConflict
				SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "Name" , SELF:GetNextName(oEntry:cName)})
			END IF
			SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Left" , oEntry:x + 8 * oEntry:nPasted})
			SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Top" , oEntry:y + 8 * oEntry:nPasted})
			IF SELF:ViewMode == ViewMode.Browse
				SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{cGuid , "_Deleted" , 1})
			END IF
			SELF:DoAction(DesignerActionType.SelectAdd , cGuid)
		NEXT
		SELF:EndAction()
	RETURN

	#endregion


	PROTECTED VIRTUAL METHOD ShowContextMenu(oDesign AS DesignWindowItem , oPoint AS Point) AS VOID
		LOCAL oConMenu AS ContextMenu
		LOCAL oResizeMenu AS MenuItem
//		LOCAL oItem AS MenuItem

		IF SELF:lReadOnly
			RETURN
		ENDIF
		
		oConMenu:=ContextMenu{}
		
		IF SELF:aSelected:Count > 1
			oConMenu:MenuItems:Add(SELF:MakeMenuItem("Align Left",DesignerActionType.AlignLeft))
			oConMenu:MenuItems:Add(SELF:MakeMenuItem("Align Right",DesignerActionType.AlignRight))
			oConMenu:MenuItems:Add(SELF:MakeMenuItem("Align Top",DesignerActionType.AlignTop))
			oConMenu:MenuItems:Add(SELF:MakeMenuItem("Align Bottom",DesignerActionType.AlignBottom))
			oConMenu:MenuItems:Add("-")
			oConMenu:MenuItems:Add(SELF:MakeMenuItem("Align Horz. Center",DesignerActionType.AlignCenterHorz))
			oConMenu:MenuItems:Add(SELF:MakeMenuItem("Align Vert. Center",DesignerActionType.AlignCenterVert))
			oConMenu:MenuItems:Add("-")
			oResizeMenu := MenuItem{"Resize To"}
			oResizeMenu:MenuItems:Add(SELF:MakeMenuItem("Align Left",DesignerActionType.ResizeAlignLeft))
			oResizeMenu:MenuItems:Add(SELF:MakeMenuItem("Align Right",DesignerActionType.ResizeAlignRight))
			oResizeMenu:MenuItems:Add(SELF:MakeMenuItem("Align Top",DesignerActionType.ResizeAlignTop))
			oResizeMenu:MenuItems:Add(SELF:MakeMenuItem("Align Bottom",DesignerActionType.ResizeAlignBottom))
			oConMenu:MenuItems:Add(oResizeMenu)
			oConMenu:MenuItems:Add("-")
			oConMenu:MenuItems:Add(SELF:MakeMenuItem("Same Size",DesignerActionType.SameSize))
			oConMenu:MenuItems:Add(SELF:MakeMenuItem("Same Horizontal Size",DesignerActionType.SameHorSize))
			oConMenu:MenuItems:Add(SELF:MakeMenuItem("Same Vertical Size",DesignerActionType.SameVerSize))
			
/*			oConMenu:MenuItems:Add("-")
			oItem := SELF:MakeMenuItem("Lock Controls",DesignerActionType.LockUnlock)
			oItem:Checked := ((DesignControlForms)ATail(SELF:aSelected)):lLocked
			oConMenu:MenuItems:Add(oItem)*/
			
			oConMenu:MenuItems:Add("-")
			oConMenu:MenuItems:Add(SELF:MakeMenuItem("Cut",DesignerActionType.Cut))
			oConMenu:MenuItems:Add(SELF:MakeMenuItem("Copy",DesignerActionType.Copy))
			IF Clipboard:Count != 0
				oConMenu:MenuItems:Add(SELF:MakeMenuItem("Paste",DesignerActionType.Paste))
			END IF
			oConMenu:MenuItems:Add("-")
			oConMenu:MenuItems:Add(SELF:MakeMenuItem("Delete",DesignerActionType.RemoveSelected))
	
		ELSEIF !oDesign:IsForm
	
/*			IF oDesign:IsBrowser
				oConMenu:MenuItems:Add(SELF:MakeMenuItem("Add Column",DesignerActionType.AddColumn))
				oConMenu:MenuItems:Add("-")
			ENDIF*/
			IF oDesign:IsTabControl
				oConMenu:MenuItems:Add(SELF:MakeMenuItem("Add Page",DesignerActionType.AddPage))
				IF ((DesignTabControl)oDesign:Control):TabPages:Count != 0
					oConMenu:MenuItems:Add(SELF:MakeMenuItem("Delete Page",DesignerActionType.DeletePage))
				END IF
				oConMenu:MenuItems:Add("-")
			ENDIF

			oConMenu:MenuItems:Add(SELF:MakeMenuItem("Cut",DesignerActionType.Cut))
			oConMenu:MenuItems:Add(SELF:MakeMenuItem("Copy",DesignerActionType.Copy))
			IF Clipboard:Count != 0
				oConMenu:MenuItems:Add(SELF:MakeMenuItem("Paste",DesignerActionType.Paste))
			END IF
			oConMenu:MenuItems:Add("-")
			oConMenu:MenuItems:Add(SELF:MakeMenuItem("Delete",DesignerActionType.RemoveSelected))

		ELSEIF oDesign:IsForm
			
			IF Clipboard:Count != 0
				IF oConMenu:MenuItems:Count != 0
					oConMenu:MenuItems:Add("-")
				ENDIF
				oConMenu:MenuItems:Add(SELF:MakeMenuItem("Paste",DesignerActionType.Paste))
			END IF
			
		END IF		
	
		IF oConMenu:MenuItems:Count != 0
			oPoint := SELF:oSurface:PointToClient(oPoint)
			oConMenu:Show(SELF:oSurface , oPoint)
		ENDIF
	RETURN

	PROTECTED VIRTUAL METHOD DoContextAction(eAction AS DesignerActionType) AS VOID
		LOCAL oTabControl AS DesignTabControl
		DO CASE
		CASE eAction == DesignerActionType.AddPage
			oTabControl := (DesignTabControl) ((DesignWindowItem)SELF:aSelected[0]):Control
			oTabControl:AddPage()
			SELF:BeginAction()
			SELF:EndAction()
		CASE eAction == DesignerActionType.DeletePage
			oTabControl := (DesignTabControl) ((DesignWindowItem)SELF:aSelected[0]):Control
			IF oTabControl:SelectedTab != NULL
				oTabControl:TabPages:Remove(oTabControl:SelectedTab)
				SELF:BeginAction()
				SELF:EndAction()
				SELF:oDummy:Focus()
			ENDIF
		OTHERWISE
			SUPER:DoContextAction(eAction)
		END CASE
	RETURN

	VIRTUAL METHOD PropertyGotUpdated(oDesign AS DesignWindowItem , oProp AS VODesignProperty) AS VOID
		SELF:ApplyProperty(oDesign , oProp)
	RETURN
	PROTECTED VIRTUAL METHOD ApplyProperty(oDesign AS DesignWindowItem , oProp AS VODesignProperty) AS VOID
		LOCAL oColor AS Color
		DO CASE
		CASE oProp:Name == "Caption"
			IF oDesign:Control:GetType() == TypeOf(DesignPushButton)
				oDesign:Control:Text := ""
				oDesign:Control:Invalidate()
				RETURN
			END IF
			oDesign:Control:Text := Funcs.TranslateCaption(oProp:TextValue , FALSE)
			IF oDesign:IsColumn
				oDesign:Control:Invalidate()
			ENDIF
			IF oDesign:Column != NULL
				oDesign:Column:Invalidate()
			ENDIF
		CASE oProp:Name == "_Left"
			IF !oDesign:IsForm
				oDesign:Control:Left := (INT)oProp:Value
			ENDIF
		CASE oProp:Name == "_Top"
			IF !oDesign:IsForm
				oDesign:Control:Top := (INT)oProp:Value
			ENDIF
		CASE oProp:Name == "_Width"
			oDesign:Control:Width := (INT)oProp:Value
		CASE oProp:Name == "_Height"
			oDesign:Control:Height := (INT)oProp:Value

		CASE oProp:Name == "_PageName" .or. oProp:Name == "_PageCaption" .or. oProp:Name == "_IsDataPage"
			LOCAL oTabControl AS DesignTabControl
			LOCAL oOptions AS VOTabPageOptions
			oTabControl := (DesignTabControl)oDesign:Control
			oOptions := oTabControl:GetTabPageOptions()
			oOptions:cName := (STRING)oDesign:GetProperty("_PageName"):Value
			oOptions:cCaption := (STRING)oDesign:GetProperty("_PageCaption"):Value
//			oOptions:lDataAware := (INT)oDesign:GetProperty("_IsDataPage"):Value == 0
			oOptions:lDataAware := Convert.ToInt32(oDesign:GetProperty("_IsDataPage"):Value) == 0
			oTabControl:SetTabPageOptions(oOptions)

		CASE oProp:cMember == "Background" .or. oProp:cMember == "BackgroundColor"
			oColor := Color.Empty
			IF oProp:Value != NULL .and. oProp:Value:GetType() == TypeOf(Color)
				oColor := (Color)oProp:Value
			ELSEIF oProp:Value != NULL .and. oProp:TextValue:Length != 0
				oColor := Funcs.StringToColor(oProp:TextValue)
			ENDIF
			IF oColor != Color.Empty
				oDesign:Control:BackColor := oColor
			ELSE
				oDesign:Control:ResetBackColor()
			ENDIF
			
		CASE oProp:cMember == "TextColor"
			oColor := Color.Empty
			IF oProp:Value != NULL .and. oProp:Value:GetType() == TypeOf(Color)
				oColor := (Color)oProp:Value
			ELSEIF oProp:Value != NULL .and. oProp:TextValue:Length != 0
				oColor := Funcs.StringToColor(oProp:TextValue)
			ENDIF
			IF oColor != Color.Empty
				oDesign:Control:ForeColor := oColor
			ELSE
				oDesign:Control:ResetForeColor()
			ENDIF
		CASE oProp:cMember == "Font"
			IF oProp:Value != NULL .and. oProp:Value:GetType() == TypeOf(Font)
				oDesign:Control:Font := (Font)oProp:Value
			ELSE
				oDesign:Control:ResetFont()
			ENDIF

		CASE oProp:cMember == "_BrowseSize"
			IF oDesign:Column != NULL
				oDesign:Column:Width := (INT)oProp:Value * 6
				IF oDesign:Column:Width < 1
					oDesign:Column:Width := 1
				END IF
			END IF
		CASE oProp:cMember == "_BrowseIndex"
			IF oDesign:Column != NULL
				IF (INT)oProp:Value == -1
					oDesign:Column:Hide()
				ELSE
					oDesign:Column:Visible := SELF:ViewMode == ViewMode.Browse
				END IF
			ENDIF
		CASE oProp:cMember == "_Deleted"
			oDesign:Control:Visible := SELF:ViewMode != ViewMode.Browse .and. oDesign:Deleted == 0
			
		CASE oDesign:IsForm .and. oProp:Name:ToUpper() == "VIEW AS"
			IF !SELF:lLoading
				SELF:HandleViewModeChange()
				SELF:oToolBox:SetViewMode(SELF:ViewMode)
			ENDIF

		END CASE
		
		IF oProp:eVOStyle != VOStyle.None
			IF oDesign:Control:GetType():GetMethod("ApplyVOStyleProperty") != NULL
				oDesign:Control:GetType():GetMethod("ApplyVOStyleProperty"):Invoke(oDesign:Control,<OBJECT>{oProp})
			ENDIF
		ENDIF
		
	RETURN

	PROTECTED VIRTUAL METHOD AddAffected(oDesign AS DesignWindowItem) AS VOID
		IF !SELF:aAffected:Contains(oDesign:cGuid)
			SELF:aAffected:Add(oDesign:cGuid)
		ENDIF
	RETURN
	
	VIRTUAL METHOD GetDesignItemFromGuid(cGuid AS STRING) AS DesignWindowItem
		LOCAL oDesign AS DesignWindowItem
		LOCAL aItems AS ArrayList
		LOCAL n AS INT
		IF SELF:oWindowDesign:cGuid == cGuid
			RETURN SELF:oWindowDesign
		ENDIF
		aItems := SELF:GetAllDesignItems(TRUE)
		FOR n := 0 UPTO aItems:Count - 1
			oDesign := (DesignWindowItem)aItems[n]
			IF oDesign:cGuid == cGuid
				RETURN oDesign
			ENDIF
		NEXT
	RETURN NULL

	PROTECTED METHOD WriteCallback(oDesign AS DesignWindowItem , cName AS STRING) AS VOID
	RETURN

	PROTECTED STATIC METHOD SubstituteTpl(cLine AS STRING , cClass AS STRING , cInitMethod AS STRING) AS STRING
		LOCAL nAt AS INT
		nAt := cLine:ToUpper():IndexOf("CLASS %FORM:NAME%")
		IF nAt != -1
			cLine := cLine:Substring(0, nAt - 1)
		END IF
		cLine := cLine:Replace("%FORM:NAME%" , cClass)
		cLine := cLine:Replace("%INITPARAMS%" , cInitMethod)
		cLine := cLine:Replace("\t" , e"\t")
		cLine := cLine:Replace("\r" , "")
		cLine := cLine:Replace("\n" , "")
	RETURN cLine
	
	METHOD PropertyModifiedInGrid(cProp AS STRING , oValue AS OBJECT) AS VOID
		LOCAL oDesign AS DesignWindowItem
		LOCAL n AS INT
		
		IF SELF:lReadOnly
			RETURN
		ENDIF
		
		IF cProp == NULL
			IF SELF:aSelected:Count != 1
				RETURN
			END IF
			oDesign := (DesignWindowItem)SELF:aSelected[n]
			SELF:WriteCallback(oDesign , (STRING)oValue)
			RETURN
		END IF
		
		SELF:BeginAction()
		FOR n := 0 UPTO SELF:aSelected:Count - 1
			oDesign := (DesignWindowItem)SELF:aSelected[n]
			IF cProp == "Name" .and. (SELF:NameExists((STRING)oValue) .or. ((STRING)oValue):Trim() == "")
				LOOP
			ENDIF
			IF cProp == "Name" .and. oDesign:IsForm
				Funcs.WarningBox("Modifying window name is not allowed yet." , "VSWED")
				LOOP
			ENDIF
			IF cProp == "Name"
				oDesign:Control:Invalidate()
			ENDIF
//			oProp := oDesign:GetProperty(cProp)
			SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{oDesign:cGuid , cProp , oValue})
		NEXT
		SELF:EndAction()
		
	RETURN

	METHOD HandleViewModeChange() AS VOID
		LOCAL oDesign AS DesignWindowItem
		LOCAL aDesign AS ArrayList
		LOCAL eMode AS ViewMode
		LOCAL n AS INT
		eMode := SELF:ViewMode
		aDesign := SELF:GetAllDesignItems()
		FOR n := 0 UPTO aDesign:Count - 1
			oDesign := (DesignWindowItem)aDesign[n]
			IF eMode == ViewMode.Browse
				oDesign:lBrowseView := TRUE
				oDesign:Control:Hide()
				IF oDesign:BrowseIndex != -1 .and. oDesign:Column != NULL
					oDesign:Column:Show()
				ENDIF
			ELSE
				oDesign:lBrowseView := FALSE
				IF oDesign:Column != NULL
					oDesign:Column:Hide()
				ENDIF
				oDesign:Control:Visible := oDesign:Deleted == 0
			END IF
		NEXT

		IF SELF:ViewMode == ViewMode.Auto
			LOCAL nIndex AS INT
			SELF:BeginAction()
			FOR n := 0 UPTO aDesign:Count - 1
				oDesign := (DesignWindowItem)aDesign[n]
				IF oDesign:Column != NULL
					IF oDesign:Deleted != 0
						SELF:StartAction(DesignerBasicActionType.Remove , ActionData{oDesign:cGuid})
					ELSE
						SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{oDesign:cGuid , "_BrowseIndex" , nIndex})
						SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{oDesign:cGuid , "_BrowseSize" , 17})
						nIndex ++
					END IF
				END IF
			NEXT
			SELF:EndAction()
		END IF

	RETURN
	
	METHOD Reset() AS VOID
		LOCAL oDesign AS DesignWindowItem
		IF SELF:oWindow == NULL .or. SELF:oWindowDesign == NULL
			DO WHILE SELF:oSurface:Controls:Count != 0
				SELF:oSurface:Controls:RemoveAt(0)
			END DO
			SELF:oWindow := NULL
			SELF:oWindowDesign := NULL
			SELF:AddDummy()
			SELF:aSelected:Clear()
		ELSE
			DO WHILE SELF:oWindow:Controls:Count != 0
				oDesign := SELF:GetDesignItemFromControl(SELF:oWindow:Controls[0])
				SELF:StartAction(DesignerBasicActionType.Remove , ActionData{oDesign:cGuid})
			END DO
			SELF:oWindowDesign:Delete()
			SELF:oWindowDesign := NULL
			DO WHILE SELF:oSurface:Controls:Count != 0
				SELF:oSurface:Controls:RemoveAt(0)
			END DO
			SELF:AddDummy()
		ENDIF

		SELF:ClearUndoBuffer()
		
	RETURN
	METHOD ClearUndoBuffer() AS VOID
		SELF:aActions:Clear()
		SELF:nAction := 0
	RETURN

	METHOD ArrangeColumnsOrder() AS VOID
		SELF:ArrangeColumnsOrder(FALSE)
	RETURN
	METHOD ArrangeColumnsOrder(lForceUpdate AS LOGIC) AS VOID
		LOCAL oDesign AS DesignWindowItem
		LOCAL aDesign AS ArrayList
		LOCAL aSorted AS ArrayList
		LOCAL lUpdate AS LOGIC
		LOCAL n AS INT

		aDesign := SELF:GetAllColumnDesignItems()
		aSorted := SELF:GetAllColumnDesignItemsByIndex()
		
		IF !lForceUpdate
			FOR n := 0 UPTO aSorted:Count - 1
				oDesign := (DesignWindowItem)aSorted[n]
				IF oDesign != (DesignWindowItem)aDesign[aSorted:Count - 1 - n]
					lUpdate := TRUE
					EXIT
				END IF
			NEXT		
			IF !lUpdate
				RETURN
			END IF
		END IF
		
		FOR n := 0 UPTO aSorted:Count - 1
			oDesign := (DesignWindowItem)aSorted[n]
//			IF lForceUpdate
			oDesign:BrowseIndex := n
//			END IF
			IF oDesign:Column:Parent:Controls:GetChildIndex(oDesign:Column) != aSorted:Count - 1 - n
				oDesign:Column:Parent:Controls:SetChildIndex(oDesign:Column , aSorted:Count - 1 - n)
			END IF
		NEXT
	RETURN

	METHOD ArrangeControlOrder() AS SortedList
		LOCAL aDesign AS ArrayList
		LOCAL aSorted AS SortedList
		LOCAL oDesign AS DesignWindowItem
		LOCAL n AS INT
		SELF:oWindowDesign:GetProperty("__Order"):Value := 0
		aDesign := SELF:GetAllDesignItems()
		aSorted := SortedList{}
		FOR n := 0 UPTO aDesign:Count - 1
			oDesign := (DesignWindowItem)aDesign[n]
			DO WHILE aSorted:ContainsKey(oDesign:Order)
				oDesign:GetProperty("__Order"):Value := oDesign:Order + 1
			ENDDO
			aSorted:Add(oDesign:Order , oDesign)
		NEXT
		FOR n := 0 UPTO aSorted:Count - 1
			oDesign := (DesignWindowItem)aSorted:GetByIndex(n)
			oDesign:GetProperty("__Order"):Value := n + 1
		NEXT
	RETURN aSorted
	METHOD GetAllDesignItemsByCreationOrder() AS ArrayList
		LOCAL aSorted AS SortedList
		LOCAL aDesign AS ArrayList
		LOCAL n AS INT
		aSorted := SELF:ArrangeControlOrder()
		aDesign := ArrayList{}
		FOR n := 0 UPTO aSorted:Count - 1
			aDesign:Add(aSorted:GetByIndex(n))
		NEXT
	RETURN aDesign

	METHOD SelectionHasColumns() AS LOGIC
		LOCAL n AS INT
		FOR n := 0 UPTO SELF:aSelected:Count - 1
			IF ((DesignWindowItem)SELF:aSelected[n]):IsColumn
				RETURN TRUE
			ENDIF
		NEXT
	RETURN FALSE
	METHOD SelectionHasNonColumns() AS LOGIC
		LOCAL n AS INT
		FOR n := 0 UPTO SELF:aSelected:Count - 1
			IF !((DesignWindowItem)SELF:aSelected[n]):IsColumn
				RETURN TRUE
			ENDIF
		NEXT
	RETURN FALSE
	
	METHOD TestForm() AS VOID
		LOCAL aDesign AS ArrayList
		LOCAL oDesign AS DesignWindowItem
		LOCAL oControl AS Control
		LOCAL oTest AS Form
		LOCAL oType AS Type
		LOCAL n AS INT

		IF SELF:oWindow == NULL .or. SELF:oWindowDesign == NULL
			RETURN
		ENDIF
		
		oTest := Form{}
		oTest:ClientSize := Size{ (INT)SELF:oWindowDesign:GetProperty("_Width"):Value , (INT)SELF:oWindowDesign:GetProperty("_Height"):Value}
		oTest:Text := SELF:oWindowDesign:GetProperty("Caption"):TextValue
		oTest:ShowInTaskbar := FALSE
		oTest:MinimizeBox := FALSE
		oTest:KeyPreview := TRUE
		oTest:KeyDown += KeyEventHandler{ SELF , @TestFormKeyDown() }
		aDesign := SELF:GetAllDesignItemsByCreationOrder()
		FOR n := 0 UPTO aDesign:Count - 1
			oDesign := (DesignWindowItem)aDesign[n]
            IF oDesign:Deleted != 0
                LOOP
            ENDIF
			TRY
				oControl := (Control)Activator.CreateInstance(oDesign:Control:GetType() , <OBJECT>{oDesign})
				oControl:GetType():GetField("lTestMode" , System.Reflection.BindingFlags.Instance + System.Reflection.BindingFlags.Public):SetValue(oControl , TRUE)
//				oControl := (Control)Activator.CreateInstance(oDesign:Control:GetType():BaseType , <OBJECT>{})
				oType := oControl:GetType()
				IF oType == TypeOf(Panel) .or. oType == TypeOf(DesignbBrowser) .or. oType == TypeOf(DesignEmpty)
					((Panel)oControl):BorderStyle := BorderStyle.FixedSingle
				ENDIF
				oControl:Left := (INT)oDesign:GetProperty("_Left"):Value
				oControl:Top := (INT)oDesign:GetProperty("_Top"):Value
				oControl:Width := (INT)oDesign:GetProperty("_Width"):Value
				oControl:Height := (INT)oDesign:GetProperty("_Height"):Value
				IF oDesign:GetProperty("Caption") != NULL
					oControl:Text := oDesign:GetProperty("Caption"):TextValue
				ENDIF
				oTest:Controls:Add(oControl)
				DO CASE
				CASE oType == TypeOf(DesignCheckBox)
					((CheckBox)oControl):CheckAlign := ((CheckBox)oDesign:Control):CheckAlign
					((CheckBox)oControl):TextAlign := ((CheckBox)oDesign:Control):TextAlign
					((CheckBox)oControl):Appearance := ((CheckBox)oDesign:Control):Appearance
				CASE oType == TypeOf(DesignRadioButton)
					((RadioButton)oControl):CheckAlign := ((RadioButton)oDesign:Control):CheckAlign
					((RadioButton)oControl):TextAlign := ((RadioButton)oDesign:Control):TextAlign
					((RadioButton)oControl):Appearance := ((RadioButton)oDesign:Control):Appearance
				CASE oType == TypeOf(DesignPushButton)
					((Button)oControl):FlatStyle := ((Button)oDesign:Control):FlatStyle
					oControl:Text := ""
				CASE oType == TypeOf(DesignFixedText)
					((Label)oControl):FlatStyle := ((Label)oDesign:Control):FlatStyle
					((Label)oControl):TextAlign := ((Label)oDesign:Control):TextAlign
				CASE oType == TypeOf(DesignEdit)
					((TextBox)oControl):Multiline := TRUE
				CASE oType == TypeOf(DesignComboBox)
					((ComboBox)oControl):DropDownStyle := ((ComboBox)oDesign:Control):DropDownStyle
				CASE oType == TypeOf(DesignHorizontalSlider) .or. oType == TypeOf(DesignVerticalSlider)
					((Trackbar)oControl):Orientation := ((Trackbar)oDesign:Control):Orientation
				END CASE
				oControl:Font := oDesign:Control:Font
				oControl:ForeColor := oDesign:Control:ForeColor
				IF oType != TypeOf(DesignPushButton)
					oControl:BackColor := oDesign:Control:BackColor
				END IF
			END TRY
		NEXT
		AdjustGroupBoxesInForm(oTest)
		oTest:ShowDialog()
		
	RETURN
	
    PROTECTED METHOD TestFormKeyDown(o AS OBJECT,e AS KeyEventArgs) AS VOID
    	IF e:KeyCode == Keys.Escape
	    	((Form)o):Close()
    	END IF
    RETURN
	
	STATIC PROTECT aTpl AS ArrayList

	METHOD GetTemplate(cName AS STRING) AS List<STRING>
		LOCAL oTemplate AS TemplateCode
		LOCAL n AS INT
		cName := cName:ToUpper()
		IF .not. LoadTpl(FileInfo{SELF:cDefaultFileName}:Directory:FullName)
			RETURN NULL
		END IF
		FOR n := 0 UPTO aTpl:Count - 1
			oTemplate := (TemplateCode)aTpl[n]
			IF oTemplate:cName:ToUpper() == cName
				RETURN oTemplate:aLines
			END IF
		NEXT
	RETURN NULL
	
	STATIC METHOD LoadTpl(cDirectory AS STRING) AS LOGIC
		LOCAL oTemplate AS TemplateCode
		LOCAL oStream AS StreamReader
		LOCAL aLines AS List<STRING>
		LOCAL cCavoWed AS STRING
      LOCAL cOrigDir AS STRING
		LOCAL cLine AS STRING
		
		IF aTpl != NULL
			RETURN TRUE
		ENDIF
		
      cOrigDir := cDirectory
        TRY
        	cCavoWed := cDirectory + "\Properties\CAVOWED.TPL"
	        IF !System.IO.File.Exists(cCavoWed)
		        cCavoWed := cDirectory + "\CAVOWED.TPL"
				IF !System.IO.File.Exists(cCavoWed)
					cDirectory := Directory.GetParent(cDirectory):FullName
					cCavoWed := cDirectory + "\CAVOWED.TPL"
			        IF !System.IO.File.Exists(cCavoWed)
			        	cCavoWed := cDirectory + "\Properties\CAVOWED.TPL"
				        IF !System.IO.File.Exists(cCavoWed) .and. Funcs.InstallTemplatesFolder != ""
				        	cCavoWed := Funcs.InstallTemplatesFolder  + "\CAVOWED.TPL"
				        ENDIF
			        ENDIF
				ENDIF
			END IF
        END TRY

		IF !System.IO.File.Exists(cCavoWed)
			MessageBox.Show("File Cavowed.tpl was not found, please locate it on disk." , Resources.EditorName)
		ENDIF
		DO WHILE !System.IO.File.Exists(cCavoWed)
			LOCAL oDlg AS OpenFileDialog
			oDlg := OpenFileDialog{}
			oDlg:Filter := "CavoWED files (*.tpl)|*.tpl"
			oDlg:Title := "Open cavowed.tpl file"
			IF oDlg:ShowDialog() == DialogResult.OK
				cCavoWed := oDlg:FileName:ToLower()
            TRY
               IF cCavoWed:Contains("cavowed") .and. cCavoWed:EndsWith(".tpl")
                  File.Copy(cCavoWed , cOrigDir + "\cavowed.tpl" , FALSE)
               ENDIF
            END TRY
			ELSE
				RETURN FALSE
			ENDIF
		END DO
		IF !System.IO.File.Exists(cCavoWed)
			RETURN FALSE
		ENDIF
		
		aTpl := ArrayList{}
		
		oStream := StreamReader{cCavoWed , System.Text.Encoding.Default}
		DO WHILE oStream:Peek() != - 1
			cLine := oStream:ReadLine():Trim()
			DO CASE
			CASE cLine:StartsWith(";")
			CASE cLine:StartsWith("[") .and. cLine:EndsWith("]")
				oTemplate := TemplateCode{cLine:Substring(1, cLine:Length - 2)}
				aTpl:Add(oTemplate)
				aLines := oTemplate:aLines
			OTHERWISE
				IF aLines != NULL
					aLines:Add(cLine)
				END IF
			END CASE
		END DO
		oStream:Close()
	RETURN TRUE
	
END CLASS

CLASS DesignWindowItem INHERIT DesignItem
	EXPORT cControl AS STRING
	EXPORT cFullClass AS STRING
	EXPORT cWinClass AS STRING
	EXPORT cInitMethod AS STRING
	EXPORT lCreateResource AS LOGIC
	PROTECT oControl AS Control
	PROTECT aSelectors AS SelectorBitmap[]
	PROTECT lForm AS LOGIC
	EXPORT cPrefix AS STRING
	EXPORT lAccessAssign AS LOGIC
	INTERNAL oColumn AS DesignDataColumn
	EXPORT lBrowseView AS LOGIC
	PROTECT oBIProp,oBSProp,oDelProp AS DesignProperty
	
	CONSTRUCTOR(_oDesigner AS VOWindowEditor , oTemplate AS VOControlTemplate)

		SUPER(_oDesigner)
		
		LOCAL oProp,oTest AS VODesignProperty
		LOCAL cProp AS STRING
		LOCAL n,m,k AS INT
		
		IF oTemplate == NULL
			oTemplate := VOWindowEditorTemplate.Get("PUSHBUTTON")
		ENDIF
		
		SELF:cControl := oTemplate:cControl:ToUpper()
		SELF:cWinClass := oTemplate:cWinClass
		SELF:cFullClass := oTemplate:cFullClass
		SELF:cInitMethod := oTemplate:cInitMethod
		SELF:lCreateResource := oTemplate:lCreateResource
		SELF:lForm := oTemplate:lForm
		
		SELF:cPrefix := "oDC"
		
		DO CASE
//		CASE SELF:cControl == "DATAWINDOW"
		CASE SELF:lForm
			SELF:oControl := DesignWindow{SELF}

		CASE SELF:cFullClass:IndexOf("CONTROL:TEXTCONTROL:FIXEDTEXT") == 0
			SELF:oControl := DesignFixedText{SELF}
		CASE SELF:cFullClass:IndexOf("CONTROL:TEXTCONTROL:BUTTON:PUSHBUTTON") == 0
			SELF:oControl := DesignPushButton{SELF}
			SELF:cPrefix := "oCC"
		CASE SELF:cFullClass:IndexOf("CONTROL:TEXTCONTROL:BUTTON:CHECKBOX") == 0
			SELF:oControl := DesignCheckBox{SELF}
			SELF:oColumn := DesignDataColumn{SELF}
		CASE SELF:cFullClass:IndexOf("CONTROL:TEXTCONTROL:BUTTON:RADIOBUTTON") == 0
			SELF:oControl := DesignRadioButton{SELF}
			SELF:cPrefix := "oCC"
		CASE SELF:cFullClass:IndexOf("CONTROL:TEXTCONTROL:EDIT") == 0 .or. ;
					SELF:cFullClass:IndexOf("CONTROL:TEXTCONTROL:IPADDRESS") == 0 .or. ;
					SELF:cFullClass:IndexOf("CONTROL:TEXTCONTROL:DATETIMEPICKER") == 0 .or. ;
					SELF:cFullClass:IndexOf("CONTROL:TEXTCONTROL:HOTKEYEDIT") == 0
			SELF:oControl := DesignEdit{SELF}
			SELF:oColumn := DesignDataColumn{SELF}
		CASE SELF:cFullClass:IndexOf("CONTROL:TEXTCONTROL:GROUPBOX") == 0 .or. ;
					SELF:cFullClass:IndexOf("CONTROL:TEXTCONTROL:RADIOBUTTONGROUP") == 0
			SELF:oControl := DesignGroupBox{SELF}
		CASE SELF:cFullClass:IndexOf("CONTROL:TEXTCONTROL:BASELISTBOX:COMBOBOX") == 0
			SELF:oControl := DesignComboBox{SELF}
		CASE SELF:cFullClass:IndexOf("CONTROL:TEXTCONTROL:BASELISTBOX:LISTBOX") == 0
			SELF:oControl := DesignListBox{SELF}
		CASE SELF:cFullClass:IndexOf("CONTROL:COMMONCONTROL:LISTVIEW") == 0
			SELF:oControl := DesignListView{SELF}
		CASE SELF:cFullClass:IndexOf("CONTROL:COMMONCONTROL:TREEVIEW") == 0
			SELF:oControl := DesignTreeView{SELF}
		CASE SELF:cFullClass:IndexOf("CONTROL:COMMONCONTROL:PROGRESSBAR") == 0
			SELF:oControl := DesignProgressBar{SELF}
		CASE SELF:cFullClass:IndexOf("CONTROL:TEXTCONTROL:MONTHCALENDAR") == 0 .and. FALSE
			// MonthCalendar control in .Net has standard size
			SELF:oControl := DesignMonthCalendar{SELF}
		CASE SELF:cFullClass:IndexOf("CONTROL:SCROLLBAR:HORIZONTALSCROLLBAR") == 0
			SELF:oControl := DesignHorizontalScrollBar{SELF}
		CASE SELF:cFullClass:IndexOf("CONTROL:SCROLLBAR:VERTICALSCROLLBAR") == 0
			SELF:oControl := DesignVerticalScrollBar{SELF}
		CASE SELF:cFullClass:IndexOf("CONTROL:SCROLLBAR:SLIDER:HORIZONTALSLIDER") == 0
			SELF:oControl := DesignHorizontalSlider{SELF}
		CASE SELF:cFullClass:IndexOf("CONTROL:SCROLLBAR:SLIDER:VERTICALSLIDER") == 0
			SELF:oControl := DesignVerticalSlider{SELF}
		CASE SELF:cFullClass:IndexOf("CONTROL:SCROLLBAR:SPINNER:HORIZONTALSPINNER") == 0
			SELF:oControl := DesignHorizontalSpinner{SELF}
		CASE SELF:cFullClass:IndexOf("CONTROL:SCROLLBAR:SPINNER:VERTICALSPINNER") == 0
			SELF:oControl := DesignVerticalSpinner{SELF}
		CASE SELF:cFullClass:IndexOf("CONTROL:COMMONCONTROL:TABCONTROL") == 0
			SELF:oControl := DesignTabControl{SELF}

/*		CASE SELF:cFullClass:IndexOf("CONTROL:CUSTOMCONTROL:BBROWSER") == 0
			SELF:oControl := DesignbBrowser{SELF}*/
/*		CASE SELF:cFullClass:IndexOf("CONTROL:BDATACOLUMN") == 0
			SELF:oControl := DesignbDataColumn{SELF}*/

		CASE SELF:cFullClass:IndexOf("CONTROL:SUBDATAWINDOW") == 0
			SELF:oControl := DesignEmpty{SELF}
			SELF:cWinClass := "Static"
			SELF:cPrefix := "oSF"
		OTHERWISE
			SELF:oControl := DesignEmpty{SELF}
		END CASE
		
		SELF:lAccessAssign := SELF:cFullClass:IndexOf("CONTROL:TEXTCONTROL:BUTTON:CHECKBOX") == 0 .or. ;
						SELF:cFullClass:IndexOf("CONTROL:TEXTCONTROL:EDIT:SINGLELINEEDIT") == 0 .or. ;
						SELF:cFullClass:IndexOf("CONTROL:TEXTCONTROL:EDIT:MULTILINEEDIT") == 0 .or. ;
						SELF:cFullClass:IndexOf("CONTROL:TEXTCONTROL:BASELISTBOX") == 0 .or. ;
						SELF:cFullClass:IndexOf("CONTROL:TEXTCONTROL:RADIOBUTTONGROUP") == 0

		IF SELF:cFullClass:IndexOf("CONTROL:TEXTCONTROL:EDIT:MULTILINEEDIT:RICHEDIT") == 0
			SELF:lAccessAssign := FALSE
		END IF
				
		SELF:aPages := oTemplate:aPages
		
		IF oTemplate:oSize:Width > 5 .and. oTemplate:oSize:Height > 5
			SELF:oControl:Size := oTemplate:oSize
		ENDIF
		
		SELF:oControl:GotFocus += EventHandler{ SELF , @ControlGotFocus() }

		SELF:aSelectors := SelectorBitmap[]{8}	
		FOR n := 1 UPTO 8
			SELF:aSelectors[n] := SelectorBitmap{SELF , n}
		NEXT
		SELF:ResetSelectors(FALSE)

//		SELF:AddProperty(VODesignProperty{"Name" , "Name" , PropertyType.Text , PropertyStyles.NoAuto + PropertyStyles.NoCode} , "")
/*		SELF:AddProperty(VODesignProperty{"_Left","Left",PropertyType.Numeric,PropertyStyles.NoCode + PropertyStyles.NoNULL} , 0 )
		SELF:AddProperty(VODesignProperty{"_Top","Top",PropertyType.Numeric,PropertyStyles.NoCode + PropertyStyles.NoNULL} , 0 )
		SELF:AddProperty(VODesignProperty{"_Width","Width",PropertyType.Numeric,PropertyStyles.NoCode + PropertyStyles.NoNULL} , 0 )
		SELF:AddProperty(VODesignProperty{"_Height","Height",PropertyType.Numeric,PropertyStyles.NoCode + PropertyStyles.NoNULL} , 0 )*/
/*
		FOR n := 0 UPTO oTemplate:aProperties:Count - 1
			oProp := (VODesignProperty)oTemplate:aProperties[n]
			DO CASE
			CASE oProp:eVOStyle != VOStyle.None
				SELF:AddProperty(VODesignProperty{oProp:Name , oProp:Caption , oProp:eVOStyle})
			CASE oProp:Type == PropertyType.Enumerated
				SELF:AddProperty(VODesignProperty{oProp:Name , oProp:Caption , oProp:cEnum})
			OTHERWISE
				SELF:AddProperty(VODesignProperty{oProp:Name , oProp:Caption , oProp:Type})
			ENDCASE
		NEXT
*/
		IF .not. SELF:IsForm
			SELF:AddHiddenStyle("WS_CHILD" , oTemplate)
		ENDIF
		DO CASE
		CASE SELF:cFullClass:StartsWith("FORM:DIALOGWINDOW")
			SELF:AddHiddenStyle("DS_3DLOOK" , oTemplate)
		CASE SELF:cFullClass:StartsWith("CONTROL:SCROLLBAR:VERTICALSCROLLBAR")
			SELF:AddHiddenStyle("SBS_VERT" , oTemplate)
		CASE SELF:cFullClass:StartsWith("CONTROL:SCROLLBAR:SLIDER:VERTICALSLIDER")
			SELF:AddHiddenStyle("TBS_VERT" , oTemplate)
		CASE SELF:cFullClass:StartsWith("CONTROL:SCROLLBAR:SPINNER:HORIZONTALSPINNER")
			SELF:AddHiddenStyle("UDS_HORZ" , oTemplate)
		CASE SELF:cFullClass:StartsWith("CONTROL:TEXTCONTROL")
			SELF:AddHiddenStyle("BS_GROUPBOX" , oTemplate)
			IF SELF:cFullClass:StartsWith("CONTROL:TEXTCONTROL:EDIT:MULTILINEEDIT")
				SELF:AddHiddenStyle("ES_MULTILINE" , oTemplate)
			ENDIF
		CASE SELF:cFullClass:StartsWith("CONTROL:FIXEDBITMAP")
			SELF:AddHiddenStyle("SS_BITMAP" , oTemplate)
		CASE SELF:cFullClass:StartsWith("CONTROL:FIXEDICON")
			SELF:AddHiddenStyle("SS_ICON" , oTemplate)
		CASE SELF:cFullClass:StartsWith("CONTROL:COMMONCONTROL:LISTVIEW")
			SELF:AddHiddenStyle("LVS_OWNERDATA" , oTemplate)
		END CASE
			
		LOCAL aPages AS List<STRING>
		LOCAL cPage AS STRING
		LOCAL cCaption AS STRING
		LOCAL cFirstPage AS STRING
		LOCAL nAt AS INT
		cFirstPage := ""
		FOR n := 0 UPTO SELF:aPages:Count - 1
			cPage := SELF:aPages[n]
			nAt := cPage:IndexOf(':')
			IF nAt == -1
				cCaption := cPage
			ELSE
				cCaption := cPage:Substring(nAt + 1)
				cPage := cPage:Substring(0 , nAt)
//				SELF:aPages[n] := cCaption
			ENDIF
			IF n == 0
				cFirstPage := cCaption
			END IF
//			TRY
				aPages := (List<STRING>)VOWindowEditorTemplate.aPages[cPage:ToUpper()]
//				MessageBox.Show((aPages == NULL):ToString() , cPage)
				FOR m := 0 UPTO aPages:Count - 1
					LOCAL lBasic AS LOGIC
					cProp := aPages[m]:ToUpper()
					lBasic := cProp[0] == '_'
/*					oProp := SELF:GetPropertyByCaption(cProp)
					IF oProp != NULL
						oProp:cPage := cCaption
					ENDIF*/
//					lFound := FALSE
					FOR k := 0 UPTO oTemplate:aProperties:Count - 1
						oTest := (VODesignProperty)oTemplate:aProperties[k]
						IF (!lBasic .and. oTest:Caption:ToUpper() == cProp) .or. (lBasic .and. oTest:Name:ToUpper() == cProp)
//							lFound := TRUE
//							IF oTest:Name:ToUpper() == "TAB KEY" .and. SELF:cFullClass:IndexOf("FORM:DIALOGWINDOW") == 0
								// TODO : in the VO WED, the Tab Key property has an empty option
//								LOOP
//							ENDIF
							DO CASE
							CASE oTest:eVOStyle != VOStyle.None
								oProp := VODesignProperty{oTest:Name , oTest:cEnumType , oTest:cEnumValues , oTest:eVOStyle}
								IF !SELF:oDesigner:Loading
									IF oTest:eVOStyle == VOStyle.Style
										oProp:InitVOStyle(oTemplate:aStyles)
									ELSE
										oProp:InitVOStyle(oTemplate:aExStyles)
									ENDIF
								END IF
							CASE oTest:Type == PropertyType.Enumerated
								oProp := VODesignProperty{oTest:Name , oTest:Caption , oTest:cMember , oTest:cEnumType}
							OTHERWISE
								oProp := VODesignProperty{oTest:Name , oTest:Caption , oTest:cMember , oTest:Type}
							ENDCASE
							oProp:lMultiple := oTest:lMultiple
							oProp:lMethod := oTest:lMethod
							oProp:cType := oTest:cType
							oProp:lNoAuto := oTest:lNoAuto
							oProp:nMultiPos := oTest:nMultiPos
							oProp:cSymbolProp := oTest:cSymbolProp
//							oProp:cSpecialClass := oTest:cSpecialClass
							oProp:cPage := cCaption
//							IF "ExpCtls|PixPos|NoAcc|Modeless":ToUpper():IndexOf(oProp:Name:ToUpper()) != -1
							IF oProp:cMember != NULL .and. "|ExpCtls|PixPos|NoAcc|Modeless|Use|InheritClassName|Block|BlockOwner|ViewAs|":ToUpper():IndexOf("|" + oProp:cMember:ToUpper() + "|") != -1
								oProp:lNoCode := TRUE
							ENDIF
							IF oProp:Caption == "Group Value" .and. oProp:cMember == "Value"
								oProp:lNoCode := TRUE
							END IF
							IF SELF:IsForm .and. oProp:cMember != NULL .and. "|Font|":ToUpper():IndexOf("|" + oProp:cMember:ToUpper() + "|") != -1
								oProp:lNoCode := TRUE
							ENDIF
							SELF:AddProperty(oProp)
							EXIT
						ENDIF
					NEXT
				
/*					IF !lFound
						DO CASE
						CASE cProp:ToUpper() == "NAME"
							oProp := VODesignProperty{cProp , cProp , cProp , PropertyType.Text}
							oProp:lNoCode := TRUE
							SELF:AddProperty(oProp)
						END CASE
					ENDIF*/


				NEXT
//			END
		NEXT


		FOR n := 0 UPTO oTemplate:aProperties:Count - 1
			oTest := (VODesignProperty)oTemplate:aProperties[n]
			cProp := oTest:Name
			IF SELF:GetProperty(cProp) == NULL // TODO code duplication
//				IF cProp:ToUpper() == "TAB KEY" .and. SELF:cFullClass:IndexOf("FORM:DIALOGWINDOW") == 0
					// TODO : in the VO WED, the Tab Key property has an empty option
//					LOOP
//				ENDIF
//				MessageBox.Show(oTest:Name)
				DO CASE
				CASE oTest:eVOStyle != VOStyle.None
					oProp := VODesignProperty{oTest:Name , oTest:cEnumType , oTest:cEnumValues , oTest:eVOStyle}
					IF !SELF:oDesigner:Loading
						IF oTest:eVOStyle == VOStyle.Style
							oProp:InitVOStyle(oTemplate:aStyles)
						ELSE
							oProp:InitVOStyle(oTemplate:aExStyles)
						ENDIF
					END IF
				CASE oTest:Type == PropertyType.Enumerated
					oProp := VODesignProperty{oTest:Name , oTest:Caption , oTest:cMember , oTest:cEnumType}
				OTHERWISE
					oProp := VODesignProperty{oTest:Name , oTest:Caption , oTest:cMember , oTest:Type}
				ENDCASE
				oProp:lMultiple := oTest:lMultiple
				oProp:lMethod := oTest:lMethod
				oProp:cType := oTest:cType
				oProp:lNoAuto := oTest:lNoAuto
				oProp:nMultiPos := oTest:nMultiPos
				oProp:cSymbolProp := oTest:cSymbolProp
//				oProp:cSpecialClass := oTest:cSpecialClass
				oProp:cPage := "_Hidden"
				IF oProp:cMember != NULL .and. "|ExpCtls|PixPos|NoAcc|Modeless|Use|InheritClassName|":ToUpper():IndexOf("|" + oProp:cMember:ToUpper() + "|") != -1
					oProp:lNoCode := TRUE
				ENDIF
				IF oProp:Caption == "Group Value" .and. oProp:cMember == "Value"
					oProp:lNoCode := TRUE
				END IF
				IF SELF:IsForm .and. oProp:cMember != NULL .and. "|Font|":ToUpper():IndexOf("|" + oProp:cMember:ToUpper() + "|") != -1
					oProp:lNoCode := TRUE
				ENDIF
				SELF:AddProperty(oProp)
			ENDIF
		NEXT

		IF SELF:GetProperty("Name") == NULL
			cProp := "Name"
			oProp := VODesignProperty{cProp , cProp , cProp , PropertyType.Text}
			oProp:lNoCode := TRUE
			oProp:cPage := cFirstPage
//			SELF:AddProperty(oProp)
			SELF:aProperties:Insert(0 , oProp)
		ENDIF

		IF SELF:GetProperty("Click Event") == NULL .and. SELF:cFullClass:StartsWith("CONTROL:TEXTCONTROL:BUTTON:PUSHBUTTON")
			cProp := "Click Event"
			oProp := VODesignProperty{cProp , cProp , "Click" , PropertyType.Callback}
			oProp:lNoCode := TRUE
			oProp:cPage := "General"
			SELF:aProperties:Insert(0 , oProp)
		ENDIF
		
		oProp := VODesignProperty{"__Order" , "__Order" , "__Order" , PropertyType.Numeric}
		oProp:cPage := "_Hidden"
		SELF:AddProperty(oProp)
		IF SELF:IsForm
			oProp:Value := 0
		ELSE
			oProp:Value := 1000
		ENDIF

//		IF SELF:Column != NULL
		SELF:oBIProp := VODesignProperty{"_BrowseIndex" , "_BrowseIndex" , "_BrowseIndex" , PropertyType.Numeric}
		SELF:oBIProp:cPage := "_Hidden"
		SELF:AddProperty(SELF:oBIProp)
		SELF:oBSProp := VODesignProperty{"_BrowseSize" , "_BrowseSize" , "_BrowseSize" , PropertyType.Numeric}
		SELF:oBSProp:cPage := "_Hidden"
		SELF:AddProperty(SELF:oBSProp)
		SELF:oDelProp := VODesignProperty{"_Deleted" , "_Deleted" , "_Deleted" , PropertyType.Numeric}
		SELF:oDelProp:cPage := "_Hidden"
		SELF:AddProperty(SELF:oDelProp)
//		END IF
		
		IF SELF:IsGroupBox
			SELF:oControl:Paint += PaintEventHandler{ SELF , @ControlPaint() }
		ENDIF
		
	RETURN

	PROTECTED METHOD AddHiddenStyle(cName AS STRING , oTemplate AS VOControlTemplate) AS VOID
		LOCAL oProp AS VODesignProperty
//		oProp := VODesignProperty{"_WSChild" , "BOOL" , "WS_CHILD" , VOStyle.Style}
		IF SELF:GetProperty("_" + cName) != NULL
			RETURN
		ENDIF
		oProp := VODesignProperty{"_" + cName , "BOOL" , cName , VOStyle.Style}
		oProp:cPage := "_Hidden"
		IF !SELF:oDesigner:Loading
			oProp:InitVOStyle(oTemplate:aStyles)
		END IF
		SELF:AddProperty(oProp)
	RETURN

	METHOD ResetSelectors(lColumn AS LOGIC) AS VOID
		IF lColumn
			SELF:aSelectors[1]:Cursor := Cursors.No
			SELF:aSelectors[2]:Cursor := Cursors.No
			SELF:aSelectors[3]:Cursor := Cursors.No
			SELF:aSelectors[4]:Cursor := Cursors.SizeWE
			SELF:aSelectors[5]:Cursor := Cursors.No
			SELF:aSelectors[6]:Cursor := Cursors.No
			SELF:aSelectors[7]:Cursor := Cursors.No
			SELF:aSelectors[8]:Cursor := Cursors.No
		ELSE
			SELF:aSelectors[1]:Cursor := Cursors.SizeNWSE
			SELF:aSelectors[2]:Cursor := Cursors.SizeNS
			SELF:aSelectors[3]:Cursor := Cursors.SizeNESW
			SELF:aSelectors[4]:Cursor := Cursors.SizeWE
			SELF:aSelectors[5]:Cursor := Cursors.SizeNWSE
			SELF:aSelectors[6]:Cursor := Cursors.SizeNS
			SELF:aSelectors[7]:Cursor := Cursors.SizeNESW
			SELF:aSelectors[8]:Cursor := Cursors.SizeWE
		END IF
	RETURN
	
	ACCESS Name() AS STRING
	RETURN SELF:GetProperty("Name"):Value:ToString()
	ACCESS Caption() AS STRING
		LOCAL oProp AS VODesignProperty
		oProp := SELF:GetProperty("Caption")
		IF oProp == NULL
			RETURN ""
		ENDIF
	RETURN oProp:TextValue
	
	ASSIGN BrowseIndex(nValue AS INT)
		IF SELF:oBIProp != NULL
			SELF:oBIProp:Value := nValue
		ENDIF
	RETURN
	ACCESS BrowseIndex AS INT
		IF SELF:oBIProp != NULL
			RETURN (INT)SELF:oBIProp:Value
		ENDIF
	RETURN 0
	
/*	ASSIGN BrowseSize(nValue AS INT)
		IF SELF:oBSProp != NULL
			SELF:oBSProp:Value := nValue
		ENDIF
	RETURN*/
	ACCESS BrowseSize AS INT
		IF SELF:oBSProp != NULL
			RETURN (INT)SELF:oBSProp:Value
		ENDIF
	RETURN 0
	
/*	ASSIGN Deleted(nValue AS INT)
		IF SELF:oDelProp != NULL
			SELF:oDelProp:Value := nValue
		ENDIF
	RETURN*/
	ACCESS Deleted AS INT
		IF SELF:oDelProp != NULL
			RETURN (INT)SELF:oDelProp:Value
		ENDIF
	RETURN 0
	
	ACCESS InheritClassName AS STRING
		LOCAL oProp AS VODesignProperty
		oProp := SELF:GetPropertyByMember("InheritClassName")
//		IF oProp == NULL .or. oProp:TextValue:Trim():Length == 0
		IF oProp == NULL .or. oProp:IsAuto
			RETURN SELF:cControl
		ENDIF
	RETURN oProp:TextValue
	ACCESS BrowserInheritClassName AS STRING
		LOCAL oProp AS VODesignProperty
		oProp := SELF:GetProperty("_DBInhFrom")
		IF oProp == NULL .or. oProp:IsAuto
			RETURN "DataBrowser"
		ENDIF
	RETURN oProp:TextValue:Trim()
	ACCESS ColumnsInheritClassName AS STRING
		LOCAL oProp AS VODesignProperty
		oProp := SELF:GetProperty("_DCInhFrom")
		IF oProp == NULL .or. oProp:IsAuto
			RETURN "DataColumn"
		ENDIF
	RETURN oProp:TextValue:Trim()

	METHOD GetVODefine(cBase AS STRING) AS STRING
	RETURN cBase:ToUpper() + "_" + SELF:Name:ToUpper()

	PROTECTED VIRTUAL METHOD ControlPaint(o AS OBJECT , e AS PaintEventArgs) AS VOID
		((VOWindowEditor)SELF:oDesigner):DrawGrid((Control)o , e)
	RETURN

	VIRTUAL METHOD ControlGotFocus(o AS OBJECT , e AS EventArgs) AS VOID
		((VOWindowEditor)SELF:oDesigner):ControlGotFocus()
	RETURN
	
	VIRTUAL ACCESS IsForm() AS LOGIC
	RETURN SELF:lForm
	ACCESS IsGroupBox() AS LOGIC
	RETURN SELF:cFullClass:IndexOf("CONTROL:TEXTCONTROL:GROUPBOX") == 0 .or. ;
			SELF:cFullClass:IndexOf("CONTROL:TEXTCONTROL:RADIOGROUPBOX") == 0
	ACCESS IsComboBox() AS LOGIC
	RETURN SELF:cFullClass:IndexOf("CONTROL:TEXTCONTROL:BASELISTBOX:COMBOBOX") == 0
	ACCESS IsSubForm() AS LOGIC
	RETURN SELF:cFullClass:IndexOf("CONTROL:SUBDATAWINDOW") == 0
	ACCESS IsTabControl() AS LOGIC
	RETURN SELF:cFullClass:IndexOf("CONTROL:COMMONCONTROL:TABCONTROL") == 0
	ACCESS IsBrowser() AS LOGIC
	RETURN SELF:cFullClass:IndexOf("CONTROL:CUSTOMCONTROL:BBROWSER") == 0
	ACCESS IsColumn() AS LOGIC
	RETURN SELF:cFullClass:IndexOf("CONTROL:BDATACOLUMN") == 0
	
/*	VIRTUAL METHOD AddProperty(oProp AS VODesignProperty , oValue AS OBJECT) AS VOID
		oProp:Value := oValue
		SELF:AddProperty(oProp)
	RETURN*/
	
	VIRTUAL METHOD PropertyValueSelected(cProp AS STRING , oValue AS OBJECT) AS VOID
		LOCAL oProp AS VODesignProperty
	    oProp := SELF:GetProperty(cProp)
	    IF oProp != NULL
//	    	IF SELF:AllowPropertyUpdate_(oProp , uValue)
				((VOWindowEditor)SELF:oDesigner):StartAction(DesignerBasicActionType.SetProperty , ActionData{SELF:cGuid , oProp:Name , oValue})
/*		    	oProp:AssignValue_ := uValue
		    	SELF:PropertyGotUpdated_(oProp)*/
//	    	ENDIF
	    ENDIF
	RETURN
	
	VIRTUAL METHOD AdjustSelectors() AS VOID
		LOCAL oControl AS Control
		LOCAL x,y,xx,yy,xy AS INT
		LOCAL nOffset AS INT
		LOCAL oPoint AS Point
		LOCAL n AS INT

		IF SELF:lBrowseView .and. !SELF:IsForm
			oControl := SELF:Column
		ELSE
			oControl := SELF:Control
		END IF
		
		IF oControl == NULL .or. ( ((VOWindowEditor)SELF:Designer):ViewMode != ViewMode.Browse .and. SELF:Deleted == 1 )
			FOR n := 1 UPTO 8
				SELF:aSelectors[n]:Hide()
			NEXT
			RETURN
		END IF
		
		SELF:ResetSelectors(SELF:lBrowseView)
		
		x := oControl:Location:X
		y := oControl:Location:Y
		xx := oControl:Width
		yy := oControl:Height
		xy := SELF:aSelectors[1]:Width
		IF SELF:IsForm
			nOffset := 3
		ELSE
			nOffset := 0
		ENDIF
		IF SELF:IsComboBox
			yy := (INT) SELF:GetProperty("_Height"):Value
		ENDIF
		
		IF SELF:aSelectors[1]:Parent != SELF:oDesigner:Surface
			FOR n := 1 UPTO 8
				SELF:oDesigner:Surface:Controls:Add(SELF:aSelectors[n])
				SELF:aSelectors[n]:BringToFront()
			NEXT
		ENDIF
		FOR n := 1 UPTO 8
			SELF:aSelectors[n]:Visible := SELF:lSelected
			IF SELF:lLocked
				SELF:aSelectors[n]:BackColor := Color.Gray
			ELSEIF SELF:lSelected .and. SELF:lDefault
				SELF:aSelectors[n]:BackColor := Color.Blue
			ELSE
				SELF:aSelectors[n]:BackColor := Color.DarkBlue
			ENDIF
		NEXT
		
		oPoint := Point{x , y}
		oPoint := oControl:Parent:PointToScreen(oPoint)
		oPoint := SELF:oDesigner:Surface:PointToClient(oPoint)
		x := oPoint:X
		y := oPoint:Y
		
		SELF:aSelectors[1]:Location := Point{x - nOffset , y - nOffset}
		SELF:aSelectors[2]:Location := Point{x + (xx - xy) / 2 , y - nOffset}
		SELF:aSelectors[3]:Location := Point{x + xx - xy + nOffset, y - nOffset}
		SELF:aSelectors[4]:Location := Point{x + xx - xy + nOffset, y + (yy - xy) / 2}
		SELF:aSelectors[5]:Location := Point{x + xx - xy + nOffset , y + yy - xy + nOffset}
		SELF:aSelectors[6]:Location := Point{x + (xx - xy) / 2 , y + yy - xy  + nOffset}
		SELF:aSelectors[7]:Location := Point{x - nOffset , y + yy - xy + nOffset}
		SELF:aSelectors[8]:Location := Point{x - nOffset , y + (yy - xy) / 2}

		IF SELF:lBrowseView .and. SELF:lSelected
			SELF:aSelectors[4]:BringToFront()
		END IF
	RETURN
	
	VIRTUAL ACCESS Control AS Control
	RETURN SELF:oControl
	VIRTUAL ACCESS Column AS DesignDataColumn
	RETURN SELF:oColumn

	VIRTUAL METHOD GoniaClicked(oSelector AS SelectorBitmap , e AS MouseEventArgs) AS VOID
		((VOWindowEditor)SELF:oDesigner):GoniaClicked(oSelector , e)
	RETURN
	VIRTUAL METHOD Delete() AS VOID
		LOCAL n AS INT
		SELF:oControl:Parent:Controls:Remove(SELF:Control)
		SELF:oControl:Dispose()
		IF SELF:oColumn != NULL
			SELF:oColumn:Parent:Controls:Remove(SELF:oColumn)
			SELF:oColumn:Dispose()
		END IF
		FOR n := 1 UPTO 8
			IF SELF:aSelectors[n] != NULL
				IF SELF:aSelectors[n]:Parent != NULL
					SELF:aSelectors[n]:Parent:Controls:Remove(SELF:aSelectors[n])
				END IF
				SELF:aSelectors[n]:Dispose()
				SELF:aSelectors[n] := NULL
			ENDIF
		NEXT
//		SELF:lDeleted := TRUE
	RETURN

	METHOD GetVOStylesString(eVOStyle AS VOStyle) AS STRING
		LOCAL oProp AS VODesignProperty
		LOCAL aStyles AS List<STRING>
		LOCAL aDefines AS STRING[]
		LOCAL cDefine AS STRING
		LOCAL cRet AS STRING
		LOCAL n,m AS INT

		aStyles := List<STRING>{}
		cRet := ""
		FOR n := 0 UPTO SELF:aProperties:Count - 1
			oProp := (VODesignProperty)SELF:aProperties[n]
			IF oProp:eVOStyle == eVOStyle
//				IF !oProp:IsAuto .and. VODefines.GetValue(oProp:aEnumValues[(INT)oProp:Value]) != 0
//				MessageBox.Show(oProp:aEnumValues[(INT)oProp:Value]:ToString())
				cDefine := oProp:aEnumValues[(INT)oProp:Value]
				cDefine := cDefine:ToUpper()
				aDefines := cDefine:Split('|')
				IF SELF:cFullClass:StartsWith("FORM:DIALOGWINDOW")
					IF oProp:Name:ToUpper() == "TYPE"
						IF cDefine:ToUpper() != "WS_POPUP" .and. SELF:GetPropertyByCaption("Tab Key") != NULL
							LOOP // Let the Tab Key property decide on WS_CHILD and DS_CONTROL styles
						END IF
					ENDIF
					IF oProp:Name:ToUpper() == "TAB KEY"
						LOCAL oTypeProp AS VODesignProperty
						oTypeProp := SELF:GetPropertyByCaption("Type")
						IF oTypeProp != NULL .and. oTypeProp:Value != NULL .and. oTypeProp:Value:GetType() == TypeOf(INT) .and. (INT)oTypeProp:Value == 0
							LOOP
						END IF
					ENDIF
				END IF
				FOR m := 1 UPTO aDefines:Length
					cDefine := aDefines[m]
					IF cDefine == "DS_MODALFRAME"
						IF SELF:GetPropertyByCaption("Modeless") != NULL .and. !SELF:GetPropertyByCaption("Modeless"):ValueLogic
							LOOP
						ENDIF
					ENDIF
					IF !aStyles:Contains(cDefine) // don't duplicate defines
						IF VODefines.GetDefineValue(cDefine) != 0
							IF cRet:Length != 0
								cRet += "|"
							ENDIF
//							cRet += oProp:aEnumValues[(INT)oProp:Value]
							cRet += cDefine
							aStyles:Add(cDefine)
						ENDIF
					END IF
				NEXT
			ENDIF
		NEXT
		
	RETURN cRet
	METHOD GetVOStylesValue(eVOStyle AS VOStyle) AS DWORD
		LOCAL oProp AS VODesignProperty
		LOCAL aDefines AS STRING[]
		LOCAL cDefine AS STRING
		LOCAL dValue AS DWORD
		LOCAL n,m AS INT
		FOR n := 0 UPTO SELF:aProperties:Count - 1
			oProp := (VODesignProperty)SELF:aProperties[n]
			IF oProp:eVOStyle == eVOStyle
				cDefine := oProp:aEnumValues[(INT)oProp:Value]
				cDefine := cDefine:ToUpper()
				aDefines := cDefine:Split('|')
				FOR m := 1 UPTO aDefines:Length
					cDefine := aDefines[m]
					dValue := dValue | VODefines.GetDefineValue(cDefine)
				NEXT
			ENDIF
		NEXT
		IF eVOStyle == VOStyle.Style
			oProp := SELF:GetProperty("_Visible")
			IF oProp != NULL .and. oProp:TextValue:ToUpper() == "NO"
				dValue := dValue | VODefines.GetDefineValue("WS_VISIBLE")
			END IF
		END IF
	RETURN dValue

	METHOD GetRect() AS Rectangle
		LOCAL oRect AS Rectangle
		oRect:X := (INT)SELF:GetProperty("_Left"):Value
		oRect:Y := (INT)SELF:GetProperty("_Top"):Value
		oRect:Width := (INT)SELF:GetProperty("_Width"):Value
		oRect:Height := (INT)SELF:GetProperty("_Height"):Value
	RETURN oRect

	METHOD GetWedItem() AS VOWEDItem
		LOCAL oItem AS VOWEDItem
		LOCAL oProp AS VODesignProperty
		LOCAL n,m AS INT
		
		oItem := VOWEDItem{}
		oItem:cName := SELF:Name
		oItem:nOrder := SELF:Order
//		oItem:cControl := SELF:cControl
		oItem:cControl := SELF:cFullClass
		oItem:cCaption := SELF:Caption
	
		oItem:nLeft := (INT)SELF:GetProperty("_Left"):Value
		oItem:nTop := (INT)SELF:GetProperty("_Top"):Value
		oItem:nWidth := (INT)SELF:GetProperty("_Width"):Value
		oItem:nHeight := (INT)SELF:GetProperty("_Height"):Value

		oItem:nDeleted := (INT)SELF:GetProperty("_Deleted"):Value
		
		oItem:nBrowseIndex := SELF:BrowseIndex
		oItem:nBrowseSize := SELF:BrowseSize

		oItem:nNoSave := iif(SELF:GetProperty("_GenCode"):TextValue:ToUpper() == "NO" , 1 , 0)
		
		oItem:dStyles := SELF:GetVOStylesValue(VOStyle.Style)
		oItem:dExStyles := SELF:GetVOStylesValue(VOStyle.ExStyle)
		
		LOCAL cName , cValue AS STRING
		FOR n := 0 UPTO SELF:aProperties:Count - 1
			oProp := (VODesignProperty)SELF:aProperties[n]
			IF oProp:eVOStyle == VOStyle.None
				IF .not. oProp:IsAuto .and. oProp:Type != PropertyType.Callback .and. oProp:Name[0] != '_'
					IF oProp:cSpecialClass == "Font"
						cName := "NewFont"
					ELSE
						cName := oProp:cMember
					ENDIF
					cValue := ""
					IF oProp:lMultiple
						FOR m := 1 UPTO oProp:nMultiPos - 1
							cValue += ","
						NEXT
						cValue += oProp:SaveValue
					ELSE
						cValue += oProp:SaveValue
					ENDIF
					oItem:aProperties:Add(cName , cValue)
				ENDIF
			END IF
		NEXT

		IF oItem:cControl:Contains(":TABCONTROL")
			LOCAL oTabControl AS DesignTabControl
			oTabControl := (DesignTabControl)SELF:Control
			FOR n := 1 UPTO oTabControl:TabPages:Count
				oItem:aTabPages:Add(oTabControl:GetTabPageOptions(n))
			NEXT
		ENDIF
		
		oProp := (VODesignProperty)SELF:GetProperty("_DBInhFrom")
		IF oProp != NULL .and. !oProp:IsAuto
			oItem:aProperties:Add("DBInhFrom" , oProp:TextValue)
		END IF
		oProp := (VODesignProperty)SELF:GetProperty("_DCInhFrom")
		IF oProp != NULL .and. !oProp:IsAuto
			oItem:aProperties:Add("DCInhFrom" , oProp:TextValue)
		END IF
		
	RETURN oItem

	ACCESS Order AS INT
	RETURN (INT)SELF:GetProperty("__Order"):Value

	METHOD ToString() AS STRING
		LOCAL cRet AS STRING
		cRet := SELF:Name + "  :  " + SELF:cControl
		IF SELF:IsForm
			cRet := "<" + cRet + ">"
		END IF
	RETURN cRet
		
END CLASS


CLASS TemplateCode
	EXPORT cName AS STRING
	EXPORT aLines AS List<STRING>
	CONSTRUCTOR(_cName AS STRING)
		SELF:cName := _cName
		SELF:aLines := List<STRING>{}
	RETURN
END CLASS


INTERNAL CLASS WindowTypeSelectDlg INHERIT Form
	PROTECT oCancelButton AS System.Windows.Forms.Button
	PROTECT oOKButton AS System.Windows.Forms.Button
	PROTECT oGroupBox1 AS System.Windows.Forms.GroupBox
	PROTECT oTypeListBox AS System.Windows.Forms.ListBox
	
	EXPORT cName AS STRING
	
	CONSTRUCTOR()

		SUPER()

		LOCAL oTemplate AS VOControlTemplate
		LOCAL n AS INT


		SELF:InitializeComponent()
	
		FOR n := 0 UPTO VOWindowEditorTemplate.Count - 1
			oTemplate := VOWindowEditorTemplate.Get(n)
			IF oTemplate:lForm .and. oTemplate:cFullClass:Contains(":")
				SELF:oTypeListBox:Items:Add(oTemplate:cControl)
			END IF
		NEXT
		
		IF SELF:oTypeListBox:Items:Count != 0
			SELF:oTypeListBox:SelectedIndex := 0
		END IF

	RETURN

	PROTECTED METHOD InitializeComponent() AS VOID
	
	SELF:oCancelButton := System.Windows.Forms.Button{}
	SELF:oOKButton := System.Windows.Forms.Button{}
	SELF:oGroupBox1 := System.Windows.Forms.GroupBox{}
	SELF:oTypeListBox := System.Windows.Forms.ListBox{}

	SELF:SuspendLayout()

	SELF:AutoScaleDimensions := System.Drawing.SizeF{ 6 , 13 }
	SELF:AutoScaleMode := System.Windows.Forms.AutoScaleMode.Font
	SELF:ClientSize := System.Drawing.Size{174 , 203}
	SELF:FormBorderStyle := FormBorderStyle.FixedDialog
	SELF:Location := System.Drawing.Point{100 , 100}
	SELF:MaximizeBox := FALSE
	SELF:MinimizeBox := FALSE
	SELF:Name := "WindowTypeSelectDlg"
	SELF:ShowIcon := FALSE
	SELF:ShowInTaskbar := FALSE
	SELF:StartPosition := FormStartPosition.CenterParent
	SELF:Text := "Select new window type:"

	SELF:AcceptButton := SELF:oOKButton
	SELF:CancelButton := SELF:oCancelButton
	SELF:oCancelButton:Location := System.Drawing.Point{91 , 172}
	SELF:oCancelButton:Name := "CancelButton"
	SELF:oCancelButton:Size := System.Drawing.Size{75 , 23}
	SELF:oCancelButton:TabIndex := 2
	SELF:oCancelButton:Text := "&Cancel"
	SELF:Controls:Add(SELF:oCancelButton)
	
	SELF:oOKButton:Click += System.EventHandler{ SELF , @OKButtonClick() }
	SELF:oOKButton:Location := System.Drawing.Point{8 , 172}
	SELF:oOKButton:Name := "OKButton"
	SELF:oOKButton:Size := System.Drawing.Size{75 , 23}
	SELF:oOKButton:TabIndex := 1
	SELF:oOKButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOKButton)
	
	SELF:oGroupBox1:SuspendLayout()
	SELF:oGroupBox1:Location := System.Drawing.Point{8 , 3}
	SELF:oGroupBox1:Name := "GroupBox1"
	SELF:oGroupBox1:Size := System.Drawing.Size{158 , 164}
	SELF:oGroupBox1:TabIndex := 0
	SELF:Controls:Add(SELF:oGroupBox1)
	

	SELF:oTypeListBox:DoubleClick += System.EventHandler{ SELF , @TypeListBoxDoubleClick() }
	SELF:oTypeListBox:Location := System.Drawing.Point{8 , 16}
	SELF:oTypeListBox:Name := "TypeListBox"
	SELF:oTypeListBox:Size := System.Drawing.Size{139 , 137}
	SELF:oTypeListBox:Sorted := TRUE
	SELF:oTypeListBox:TabIndex := 0
	SELF:oGroupBox1:Controls:Add(SELF:oTypeListBox)
	
	SELF:oGroupBox1:ResumeLayout()
	SELF:ResumeLayout()

RETURN


METHOD TypeListBoxDoubleClick(sender AS System.Object , e AS System.EventArgs) AS System.Void
	SELF:SelectTemplate()
RETURN

METHOD OKButtonClick(sender AS System.Object , e AS System.EventArgs) AS System.Void
	SELF:SelectTemplate()
RETURN

METHOD SelectTemplate() AS VOID
	IF SELF:oTypeListBox:SelectedIndex == -1
		RETURN
	END IF
	SELF:cName := SELF:oTypeListBox:Text
	SELF:DialogResult := DialogResult.OK
RETURN

END CLASS
