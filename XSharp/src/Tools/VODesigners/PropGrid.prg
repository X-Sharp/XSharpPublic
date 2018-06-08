#using System.Windows.Forms
#using System.Drawing
#using System.Collections.Generic
#using System.Collections

DELEGATE PropertyUpdatedEventHandler(cProperty AS STRING , oValue AS OBJECT) AS VOID
DELEGATE ControlKeyPressedEventHandler(eKey AS Keys) AS VOID
DELEGATE RetrieveClassNamesEventHandler(cClass AS STRING) AS STRING[]

CLASS DesignerGrid INHERIT Panel
	PROTECT oTabControl AS TABCONTROL
	PROTECT oPanel AS PropertyPanel
	PROTECT oDesign AS DesignItem
	PROTECT aSelected AS ArrayList
	PROTECT oHierarchy AS ComboBox
	PROTECT cPage AS STRING

	EXPORT oActiveDesigner AS DesignerBase

	EXPORT PropertyModified AS PropertyUpdatedEventHandler
	EXPORT ControlKeyPressed AS ControlKeyPressedEventHandler
	EXPORT RetrieveClassNames AS RetrieveClassNamesEventHandler

	CONSTRUCTOR()
		SUPER()

		SELF:cPage := ""		
		SELF:oTabControl := TABCONTROL{}
		SELF:oTabControl:Dock := DockStyle.Top
		
		SELF:oTabControl:Height := 25
		SELF:oTabControl:SelectedIndexChanged += EventHandler{ SELF , @GridTabPageChanged() }
		
		SELF:oPanel := PropertyPanel{SELF}
		SELF:oPanel:Dock := DockStyle.Fill
		SELF:oPanel:oDummy:KeyDown += KeyEventHandler{ SELF , @PanelKeyDown() }
		
		LOCAL oContainer AS Panel
		oContainer := Panel{}
		oContainer:Dock := DockStyle.Fill
		oContainer:AutoScroll := TRUE
		oContainer:Controls:Add(SELF:oPanel)
		oContainer:Controls:Add(SELF:oPanel:oDummy)
		
		SELF:Controls:Add(oContainer)
		SELF:Controls:Add(SELF:oTabControl)
		
		SELF:oHierarchy := ComboBox{}
		SELF:oHierarchy:DropDownStyle := ComboBoxStyle.DropDownList
		SELF:oHierarchy:Dock := DockStyle.Top
		SELF:oHierarchy:DropDown += EventHandler{ SELF , @HierarchyDropDown() }
		SELF:oHierarchy:SelectionChangeCommitted += EventHandler{ SELF , @HierarchySelected() }
		SELF:oHierarchy:Sorted := TRUE
		SELF:Controls:Add(SELF:oHierarchy)

		SELF:Dock := DockStyle.Fill
	RETURN

	METHOD SetPages(aPages AS List<STRING>) AS VOID
		LOCAL nPage , n AS INT
		LOCAL oPage AS TabPage
		LOCAL lFound AS LOGIC
		
		LOCAL aTemp AS List<STRING> // todo BIG, BAD, UGLY HACK!
		LOCAL cPage AS STRING
		LOCAL nAt AS INT
		aTemp := List<STRING>{}
		FOR n := 0 UPTO aPages:Count - 1
			cPage := aPages[n]
			nAt := cPage:IndexOf(':')
			IF nAt != -1
				cPage := cPage:Substring(nAt + 1)
			ENDIF
			aTemp:Add(cPage)
		NEXT
		aPages := aTemp
		
		
		nPage := 0
		DO WHILE nPage < SELF:oTabControl:TabPages:Count
			oPage := SELF:oTabControl:TabPages[nPage]
			IF aPages:IndexOf(oPage:Text) == -1
				TRY
					SELF:oTabControl:TabPages:Remove(oPage)
				CATCH // probably infamous .Net bug
					nPage ++
				END TRY
			ELSE
				nPage ++
			ENDIF
		END DO
		FOR n := 0 UPTO aPages:Count - 1
			lFound := FALSE
			FOR nPage := 0 UPTO SELF:oTabControl:TabPages:Count - 1
				oPage := SELF:oTabControl:TabPages[nPage]
				IF oPage:Text == aPages[n]
					lFound := TRUE
					EXIT
				ENDIF
			NEXT
			IF !lFound
//				SELF:oTabControl:TabPages:Add(aPages[n])
				SELF:oTabControl:TabPages:Insert(n , aPages[n])
			END IF
		NEXT
		
		IF SELF:oTabControl:TabPages:Count == 0
			SELF:cPage := ""
		ELSE
			SELF:cPage := SELF:oTabControl:SelectedTab:Text
		ENDIF
		
	RETURN

	METHOD GridTabPageChanged(o AS OBJECT,e AS EventArgs) AS VOID
		SELF:cPage := SELF:oTabControl:SelectedTab:Text
		SELF:Fill(SELF:aSelected)
		SELF:oPanel:ShowButton()
	RETURN

	METHOD UseHierarchy(lUse AS LOGIC) AS VOID
		SELF:oHierarchy:Visible := lUse
	RETURN
	
	METHOD HierarchyDropDown(o AS OBJECT,e AS EventArgs) AS VOID
		LOCAL aDesign AS ArrayList
		LOCAL cSelection AS STRING
		LOCAL n AS INT
		cSelection := SELF:oHierarchy:Text
		SELF:oHierarchy:Items:Clear()
		IF SELF:oActiveDesigner != NULL .and. .not. SELF:oActiveDesigner:Surface:IsDisposed
			aDesign := SELF:oActiveDesigner:GetHierarchyItems()
			IF aDesign == NULL
				RETURN
			END IF
			FOR n := 0 UPTO aDesign:Count - 1
				SELF:oHierarchy:Items:Add(aDesign[n])
			NEXT
			SELF:oHierarchy:Text := cSelection
		END IF
	RETURN

	METHOD HierarchySelected(o AS OBJECT,e AS EventArgs) AS VOID
		LOCAL oDesign AS DesignItem
		IF SELF:oActiveDesigner != NULL .and. .not. SELF:oActiveDesigner:Surface:IsDisposed .and. SELF:oHierarchy:SelectedItem != NULL
			oDesign := (DesignItem)SELF:oHierarchy:SelectedItem
			SELF:oActiveDesigner:DoAction(DesignerActionType.Select , oDesign:cGuid)
		END IF
	RETURN

	METHOD Fill(_aSelected AS ArrayList) AS VOID
		LOCAL oDesign AS DesignItem
		LOCAL aProperties AS ArrayList
		LOCAL lSkip AS LOGIC
		LOCAL n,m AS INT
		
		SELF:oHierarchy:Items:Clear()

		aProperties := ArrayList{}
		SELF:aSelected := _aSelected
		IF SELF:aSelected:Count == 0
			SELF:oPanel:Fill(aProperties , FALSE)
			SELF:oHierarchy:Items:Clear()
			RETURN
		END IF

		oDesign := (DesignItem)SELF:aSelected[0]

		SELF:SetPages(oDesign:aPages)
		
		IF SELF:aSelected:Count == 1
			SELF:oHierarchy:Items:Add(oDesign:ToString())
			SELF:oHierarchy:SelectedIndex := 0
		END IF

		FOREACH oProp AS DesignProperty IN oDesign:aProperties
			lSkip := FALSE
			IF oProp:cPage:ToUpper() == SELF:cPage:ToUpper()
				IF SELF:aSelected:Count > 1
					FOR m := 1 UPTO SELF:aSelected:Count - 1
						IF ((DesignItem)SELF:aSelected[m]):GetPropertyByCaption(oProp:Caption) == NULL
							lSkip := TRUE
							EXIT
						ENDIF
					NEXT
				END IF
				IF !lSkip
					aProperties:Add(oProp)
				END IF
			ENDIF
		NEXT
		SELF:oPanel:Fill(aProperties , SELF:aSelected:Count >= 2)
	RETURN

	METHOD PanelKeyDown(o AS OBJECT , e AS KeyEventArgs) AS VOID
		IF e:KeyData == Keys.Control + Keys.S
			IF SELF:ControlKeyPressed != NULL
				SELF:ControlKeyPressed:Invoke(e:KeyData)
			ENDIF
		END IF
	RETURN

	METHOD SetProperty(cProp AS STRING , oValue AS OBJECT) AS VOID
		IF SELF:PropertyModified != NULL
			SELF:PropertyModified:Invoke(cProp , oValue)
		ENDIF
	RETURN
	
END CLASS


CLASS PropertyPanel INHERIT PictureBox
	PROTECT nItemHeight AS Int32
	PROTECT aProperties AS ArrayList
	PROTECT nSplit AS Int32
	PROTECT nCurX,nCurY AS Int32
	INTERNAL oEdit AS PropertyTextBox
	INTERNAL oCombo AS PropertyComboBox
	INTERNAL oButton AS PropertyButton
	PROTECT oBrushBlack AS SolidBrush
	PROTECT oBrushWhite AS SolidBrush
	PROTECT oBrushBlue AS SolidBrush
	PROTECT oBrushDarkBlue AS SolidBrush
	PROTECT oBrushRed AS SolidBrush
	PROTECT oBrushGray AS SolidBrush
	PROTECT lMultiple AS LOGIC
	PROTECT lMovingSplitter AS LOGIC
	PROTECT cMultipleName AS STRING
	PROTECT oToolTip AS Tooltip
	PROTECT oBoldFont AS Font
	PROTECT oGrid AS DesignerGrid
	PROTECT oProperty AS VODesignProperty
	EXPORT oDummy AS Button
	
	CONSTRUCTOR(_oGrid AS DesignerGrid)
		SUPER()
		SELF:oGrid := _oGrid
		SELF:oBrushWhite := SolidBrush{Color.White}
		SELF:oBrushBlack := SolidBrush{Color.Black}
		SELF:oBrushBlue := SolidBrush{Color.FromKnownColor(KnownColor.Highlight)}
		SELF:oBrushDarkBlue := SolidBrush{Color.DarkBlue}
		SELF:oBrushRed := SolidBrush{Color.Red}
		SELF:oBrushGray := SolidBrush{Color.FromArgb(80,80,80)}
		SELF:aProperties := ArrayList{}
		SELF:nCurX := 1
		SELF:nCurY := 0
		SELF:oEdit := PropertyTextBox{SELF}
		SELF:oCombo := PropertyComboBox{SELF}
		SELF:oCombo:DropDownStyle := ComboBoxStyle.DropDownList
		SELF:oButton := PropertyButton{}
		SELF:oButton:Text := "..."
		SELF:oButton:FlatStyle := FlatStyle.Standard
		SELF:oButton:BackColor := Color.LightGray
		SELF:oButton:Click += EventHandler{SELF,@EventButtonClicked()}
		SELF:Controls:Add(SELF:oEdit)
		SELF:Controls:Add(SELF:oCombo)
		SELF:Controls:Add(SELF:oButton)
		SELF:oEdit:Visible := FALSE
		SELF:oCombo:Visible := FALSE
		SELF:oButton:Visible := FALSE
		SELF:nSplit := 120
		SELF:nItemHeight := SELF:oEdit:Height
		SELF:Dock := DockStyle.Top
	
		SELF:oDummy := Button{}
		SELF:oDummy:Size := Size{0 , SELF:nItemHeight - 2}
		SELF:oDummy:PreviewKeyDown += PreviewKeyDownEventHandler{ SELF , @DummyPreviewKeyDown() }
		
		SELF:oToolTip := Tooltip{}
		SELF:oBoldFont := Font{SELF:Font , FontStyle.Bold}
		
	RETURN

	METHOD EventButtonClicked(o AS OBJECT,e AS EventArgs) AS VOID
		SELF:oProperty := (VODesignProperty)SELF:aProperties[SELF:nCurY]
		IF SELF:oProperty:cSpecialClass != NULL
			SWITCH  SELF:oProperty:cSpecialClass 
			CASE "Color" //.or. SELF:oProperty:cSpecialClass == "Brush"
				LOCAL oColorDlg AS ColorDialog
				oColorDlg := ColorDialog{}
				IF SELF:oProperty:Value:GetType() == TypeOf(Color)
					oColorDlg:Color := (Color)SELF:oProperty:Value
				ENDIF
				IF oColorDlg:ShowDialog() == DialogResult.OK
					SELF:SetProperty(oColorDlg:Color)
				ENDIF
			CASE "Font"
				LOCAL oFontDlg AS FontDialog
				oFontDlg := FontDialog{}
				IF SELF:oProperty:Value:GetType() == TypeOf(Font)
					oFontDlg:Font := (Font)SELF:oProperty:Value
				ENDIF
				IF oFontDlg:ShowDialog() == DialogResult.OK
					SELF:SetProperty(oFontDlg:Font)
				ENDIF

			CASE "FillUsing"
				LOCAL oUsingDlg AS FillUsingPickerDlg
				IF SELF:oProperty:Value:GetType() == TypeOf(FillUsingClass)
					oUsingDlg := FillUsingPickerDlg{"" , (FillUsingClass)SELF:oProperty:Value}
				ELSE
					oUsingDlg := FillUsingPickerDlg{"" , NULL}
				ENDIF
				IF oUsingDlg:ShowDialog() == DialogResult.OK
					IF oUsingDlg:oValue == NULL
						SELF:SetProperty("")
					ELSE
						SELF:SetProperty(oUsingDlg:oValue)
					ENDIF
				ENDIF

			CASE "__MenuAccelerator"
				LOCAL oAccelDlg AS AccelSelectDlg
				oAccelDlg := AccelSelectDlg{(MenuAccelerator)SELF:oProperty:Value}
				IF oAccelDlg:ShowDialog() == DialogResult.OK
					SELF:SetProperty(oAccelDlg:oAccelerator)
					SELF:Invalidate()
				ENDIF
			END SWITCH
			
		ELSEIF SELF:oProperty:Type == PropertyType.Callback
			
			SELF:oGrid:SetProperty(NULL , SELF:oProperty:cMember)
			
		ENDIF
	RETURN

	METHOD Fill(_aProperies AS ArrayList , _lMultiple AS LOGIC) AS VOID
		SELF:aProperties := _aProperies
		SELF:lMultiple := _lMultiple
		SELF:Height := SELF:aProperties:Count * SELF:nItemHeight + 5
		IF SELF:nCurY >= SELF:aProperties:Count
			SELF:nCurY := SELF:aProperties:Count - 1
		END IF
//		SELF:oDummy:Top := SELF:nItemHeight * (SELF:aProperties:Count - 1) + 2

		SELF:oDummy:Size := Size{0 , SELF:nItemHeight * SELF:aProperties:Count}
		SELF:oDummy:Top := 0

		SELF:Invalidate()
		SELF:ShowButton()
	RETURN
	
	PROTECTED METHOD OnResize(e AS EventArgs) AS VOID
		SUPER:OnResize(e)
		IF SELF:oEdit:Visible
			SELF:oEdit:Width := SELF:Width - SELF:nSplit
		ENDIF
	RETURN

	PROTECTED METHOD OnPaint(e AS PaintEventArgs) AS VOID
		LOCAL oProp AS DesignProperty
		LOCAL cText AS STRING
		LOCAL oBrush AS SolidBrush
		LOCAL oGraphics AS Graphics
		LOCAL oBlackPen AS Pen
		LOCAL n AS INT
		LOCAL x,y AS INT
		LOCAL oFont AS Font
		
		SUPER:OnPaint(e)
		
		IF SELF:aProperties:Count == 0
			RETURN
		END IF
		oGraphics := e:Graphics
		oFont := SELF:Font
		oBlackPen := Pen{Color.Black}
		FOR  n := 0 UPTO SELF:aProperties:Count - 1
			oProp := (DesignProperty)SELF:aProperties[n]
			cText := oProp:Caption
			IF SELF:nCurX == 1 .and. SELF:nCurY == n
				oGraphics:FillRectangle(SELF:oBrushBlue , 0 , n * SELF:nItemHeight + 1 , SELF:nSplit , SELF:nItemHeight - 1)
				oBrush := SELF:oBrushWhite
			ELSE
				oBrush := SELF:oBrushBlack
			ENDIF
			oGraphics:DrawString(cText , oFont , oBrush , Rectangle{ 2 , n * SELF:nItemHeight + 5 , SELF:nSplit , SELF:nItemHeight -4 })
			
			cText := oProp:TextValue
			IF cText == "" .and. !oProp:lNoAuto
				cText := "<Auto>"
			ENDIF
			IF oProp:Type == PropertyType.Callback
				cText := ""
			END IF
			x := SELF:nSplit + 2
			y := n * SELF:nItemHeight + 5
			IF oProp:lReadOnly
				oBrush := SELF:oBrushGray
			ELSEIF SELF:lMultiple
				oBrush := SELF:oBrushDarkBlue
			ELSE
				oBrush := SELF:oBrushBlack
			ENDIF
			IF SELF:nCurX == 2 .and. SELF:nCurY == n
				oGraphics:FillRectangle(SELF:oBrushBlue , SELF:nSplit + 1 , SELF:nCurY * SELF:nItemHeight + 1 , SELF:Width - SELF:nSplit , SELF:nItemHeight - 1)
				oBrush := SELF:oBrushWhite
			ENDIF
			oGraphics:DrawString(cText,oFont,oBrush, x, y)
			
			oGraphics:DrawLine(oBlackPen , 0 , (n + 1) * SELF:nItemHeight , SELF:Width , (n + 1) * SELF:nItemHeight)
		NEXT
		oGraphics:DrawLine(oBlackPen , SELF:nSplit , 0 , SELF:nSplit , n * SELF:nItemHeight)
	RETURN

	PROTECTED METHOD DummyPreviewKeyDown(o AS OBJECT , e AS PreviewKeyDownEventArgs) AS VOID

		SWITCH e:KeyData 
		CASE Keys.Left
			IF SELF:nCurX == 2
				SELF:nCurX := 1
			END IF
		CASE Keys.Right
			IF SELF:nCurX == 1
				SELF:nCurX := 2
			END IF
		CASE Keys.Up
			IF SELF:nCurY > 0
				SELF:nCurY -= 1
			END IF
		CASE Keys.Down
			IF SELF:nCurY < SELF:aProperties:Count - 1
				SELF:nCurY += 1
			END IF
		CASE Keys.Home
			SELF:nCurY := 0
		CASE Keys.End
			SELF:nCurY := SELF:aProperties:Count - 1
		CASE Keys.Prior
			SELF:nCurY := 0
		CASE Keys.Next
			SELF:nCurY := SELF:aProperties:Count - 1
		case Keys.Enter 
        case Keys.F2
			IF SELF:nCurX == 2
				SELF:ShowControl()
			ENDIF
		END SWITCH
		SELF:Invalidate()
		SELF:ShowButton()
		
	RETURN

	PROTECTED METHOD OnMouseDown(e AS MouseEventArgs) AS VOID
		SUPER:OnMouseDown(e)
	
		SELF:oDummy:Focus()
	
		IF Math.Abs(e:X - SELF:nSplit) < 3
			SELF:lMovingSplitter := TRUE
			RETURN
		END IF
	
		IF e:Y / SELF:nItemHeight > SELF:aProperties:Count
			RETURN
		END IF
		
		SELF:nCurX := iif(e:X < SELF:nSplit , 1 , 2)
		SELF:nCurY := e:Y / SELF:nItemHeight
		SELF:Invalidate()
	
		IF SELF:nCurX == 2 .or. e:Clicks == 2
			SELF:ShowControl()
		ENDIF
		SELF:ShowButton()

	RETURN

	PROTECTED METHOD OnMouseMove(e AS MouseEventArgs) AS VOID
		IF SELF:lMovingSplitter
			IF e:X > 10 .and. e:X < SELF:Width - 10
				SELF:nSplit:=e:X
				SELF:Invalidate()
			END IF
		ENDIF
		IF Math.Abs(e:X - SELF:nSplit) < 3 .or. SELF:lMovingSplitter
			SELF:Cursor := Cursors.VSplit
		ELSE
			SELF:Cursor := Cursors.Default
		END IF
	RETURN

	PROTECTED METHOD OnMouseUp(e AS MouseEventArgs) AS VOID
		SUPER:OnMouseUp(e)
		IF SELF:lMovingSplitter
			SELF:lMovingSplitter := FALSE
		ENDIF
	RETURN
	
	METHOD ShowControl() AS VOID
		LOCAL n AS INT
		IF SELF:nCurY >= SELF:aProperties:Count
			RETURN
		ENDIF
		SELF:oProperty := (VODesignProperty)SELF:aProperties[SELF:nCurY]
		
		IF SELF:oProperty:cSpecialClass == "FillUsing" .or. SELF:oProperty:cSpecialClass == "__MenuAccelerator"
			RETURN
		ENDIF
		
		IF SELF:oProperty:lReadOnly
			RETURN
		END IF
		
		SWITCH SELF:oProperty:Type 
		case PropertyType.Numeric 
        CASE PropertyType.Text
			SELF:oEdit:Text := SELF:oProperty:TextValue
			SELF:oEdit:oProperty := SELF:oProperty
/*			SELF:oEdit:lOnlyNumbers := SELF:oProperty:Type == PropertyType.Number
			SELF:oEdit:lAllowDecimal := SELF:oProperty:lAllowDecimal*/
			SELF:oEdit:Location := Point{SELF:nSplit + 1 , SELF:nCurY * SELF:nItemHeight + 1}
			SELF:oEdit:Width := SELF:Width - SELF:nSplit
			SELF:oEdit:Show(SELF:lMultiple , SELF:oProperty:Name != "validation") // HACK
//		CASE SELF:oProperty:Type == PropertyType.Boolean .or. ;
		CASE PropertyType.Enumerated 
	    CASE PropertyType.Type
//			SELF:oCombo:DropDownStyle := ComboBoxStyle.DropDownList
			SELF:oCombo:Location := Point{SELF:nSplit + 1 , SELF:nCurY * SELF:nItemHeight + 1}
			SELF:oCombo:Width := SELF:Width - SELF:nSplit
			IF SELF:oProperty:cSpecialClass != NULL
				SELF:oCombo:Width -= SELF:nItemHeight
			ENDIF
			SELF:oCombo:Items:Clear()
			SELF:oCombo:Sorted := FALSE
			IF !SELF:oProperty:lNoAuto
				SELF:oCombo:Items:Add("<Auto>")
			ENDIF
			IF SELF:oProperty:Caption:ToUpper() == "DATA SERVER"
				SELF:oCombo:Items:Add("<Share Owner's>")
			ENDIF
			IF SELF:oProperty:cMember != NULL .and. SELF:oProperty:cMember:ToUpper() == "FONT"
				SELF:oCombo:DropDownStyle := ComboBoxStyle.DropDownList
			ELSEIF SELF:oProperty:Type == PropertyType.Type
				SELF:oCombo:DropDownStyle := ComboBoxStyle.DropDown
				IF SELF:oGrid:RetrieveClassNames != NULL // temp hack, for now only DBServer returns FieldSpecs
					LOCAL aValues AS STRING[]
					aValues := SELF:oGrid:RetrieveClassNames:Invoke("FieldSpec")
					IF aValues != NULL
						SELF:oCombo:Sorted := TRUE
						FOR n := 1 UPTO aValues:Length
							SELF:oCombo:Items:Add(aValues[n])
						NEXT
					END IF
				ENDIF
//				SELF:oCombo:SelectedIndex := 0
				SELF:oCombo:Text := (STRING)SELF:oProperty:Value
//				SELF:oCombo:SelectedText := (STRING)SELF:oProperty:Value
			ELSE
				SELF:oCombo:DropDownStyle := ComboBoxStyle.DropDownList
				FOR n := 0 UPTO SELF:oProperty:aEnumTextValues:Count - 1
					SELF:oCombo:Items:Add(SELF:oProperty:aEnumTextValues[n])
				NEXT
				IF SELF:oProperty:cSpecialClass != NULL .and. SELF:oProperty:Value:GetType() != TypeOf(INT)
					SELF:oCombo:SelectedIndex := -1
				ELSE
					SELF:oCombo:SelectedIndex := INT(SELF:oProperty:Value)
				ENDIF
			END IF
			SELF:oCombo:nProperty := SELF:nCurY
			SELF:oCombo:oProperty := SELF:oProperty
			SELF:oCombo:Show(SELF:lMultiple)
			SELF:oCombo:DroppedDown := TRUE
		END SWITCH
	RETURN
	
	METHOD ShowButton() AS VOID
		LOCAL oProp AS DesignProperty
	
		IF SELF:nCurY < 0 .or. SELF:nCurY >= SELF:aProperties:Count
			SELF:oButton:Hide()
			RETURN
		ENDIF
		oProp := (DesignProperty)SELF:aProperties[SELF:nCurY]
	
		IF oProp:Type == PropertyType.Callback .or. oProp:cSpecialClass != NULL
			SELF:oButton:Size := Size{SELF:nItemHeight + 1 , SELF:nItemHeight + 1}
			SELF:oButton:Location := Point{SELF:Width - SELF:oButton:Width + 1 , SELF:nCurY * SELF:nItemHeight}
			SELF:oButton:Show()
		ELSE
			SELF:oButton:Hide()
		ENDIF
	RETURN

	METHOD SetProperty(oValue AS OBJECT) AS VOID
		SELF:oGrid:SetProperty(SELF:oProperty:Name , oValue)
	RETURN

END CLASS






INTERNAL CLASS PropertyTextBox INHERIT TextBox
	EXPORT oPropertyPanel AS PropertyPanel
	EXPORT oProperty AS DesignProperty
	PROTECT cOldText AS STRING
	PROTECT lMultiple AS LOGIC
	PROTECT lCanceled AS LOGIC
	PROTECT lEntered AS LOGIC
	PROTECT lConvertQuotes AS LOGIC
	CONSTRUCTOR(oPanel AS PropertyPanel)
		SUPER()
		SELF:oPropertyPanel := oPanel
	RETURN
	METHOD Show(_lMultiple AS LOGIC, _lConvertQuotes AS LOGIC) AS VOID
		SELF:lMultiple := _lMultiple
		IF SELF:lMultiple
			SELF:ForeColor := Color.Red
		ELSE
			SELF:ForeColor := Color.Black
		END IF
		SELF:lConvertQuotes := _lConvertQuotes
		SELF:lCanceled := FALSE
		SELF:lEntered := FALSE
		SELF:cOldText := SELF:Text
		SELF:Select(0 , SELF:Text:Length)
		SELF:Show()
		SELF:Focus()
	RETURN
	
	PROTECTED METHOD OnKeyPress(e AS KeyPressEventArgs) AS VOID
		IF e:KeyChar == (Char)13 .or. e:KeyChar == (Char)27
			e:Handled := TRUE
		END IF
		SUPER:OnKeyPress(e)
	RETURN
	PROTECTED METHOD ProcessCmdKey(Msg REF Message,KeyData AS Keys) AS LOGIC
		IF KeyData == Keys.Escape
			SELF:lCanceled := TRUE
			SELF:Hide()
			RETURN TRUE
		END IF
	RETURN FALSE
	PROTECTED METHOD OnKeyDown(e AS KeyEventArgs) AS VOID
		SUPER:OnKeyDown(e)
		DO CASE
		CASE e:KeyData==Keys.Enter
			SELF:lEntered := TRUE
			SELF:Hide()
		END CASE
	RETURN
	PROTECTED METHOD OnLostFocus(e AS EventArgs) AS VOID
		SUPER:OnLostFocus(e)
		IF !SELF:lCanceled .and. (SELF:Text != SELF:cOldText .or. (SELF:lEntered .and. SELF:lMultiple))
//			IF SELF:oProperty:lAllowNULL .or. !AllTrim(SELF:Text)==""
				SELF:SetProperty()
//			END IF
		ENDIF
		SELF:Hide()
	RETURN
	PROTECTED METHOD OnTextChanged(e AS EventArgs) AS VOID
		SUPER:OnTextChanged(e)
/*		IF SELF:Visible .and. SELF:oProperty:lTrack
//			SELF:SetProperty()
		END IF*/
	RETURN
	PROTECTED METHOD SetProperty() AS VOID
		LOCAL cValue AS STRING
		cValue := SELF:Text
		IF SELF:lConvertQuotes
			cValue := cValue:Replace((Char)34 , (Char)39)
		END IF
		SELF:oPropertyPanel:SetProperty(cValue)
	RETURN

END CLASS



INTERNAL CLASS PropertyComboBox INHERIT ComboBox
	EXPORT oPropertyPanel AS PropertyPanel
	EXPORT nProperty AS Int32
	EXPORT oProperty AS DesignProperty
	PROTECT lMultiple AS LOGIC
	PROTECT lCanceled AS LOGIC
	PROTECT lCombined AS LOGIC
	EXPORT cInitValue AS STRING
CONSTRUCTOR(oPanel AS PropertyPanel)
	SUPER()
	SELF:oPropertyPanel:=oPanel
RETURN
METHOD Show(_lMultiple AS LOGIC) AS VOID
	SELF:lMultiple:=_lMultiple
	IF SELF:lMultiple
		SELF:ForeColor := Color.Red
	ELSE
		SELF:ForeColor := Color.Black
	END IF
	SELF:DropDownHeight := 150
	SELF:lCanceled := FALSE
	SELF:lCombined := SELF:DropDownStyle == ComboBoxStyle.DropDown
	SELF:Show()
	SELF:Focus()
RETURN
PROTECTED METHOD OnKeyDown(e AS KeyEventArgs) AS VOID
	SUPER:OnKeyDown(e)
	SWITCH e:KeyData
	CASE Keys.Escape
		SELF:lCanceled := TRUE
		SELF:Hide()
	CASE Keys.Enter
//		IF SELF:oProperty:lAllowNULL .or. !AllTrim(SELF:Text) == ""
			SELF:SetProperty()
			SELF:Hide()
//		END IF
	END SWITCH
RETURN
PROTECTED METHOD OnLostFocus(e AS EventArgs) AS VOID
	SUPER:OnLostFocus(e)
	IF !SELF:lCombined
		SELF:Hide()
	ENDIF
RETURN
PROTECTED METHOD OnSelectionChangeCommitted (e AS EventArgs) AS VOID
	SUPER:OnSelectionChangeCommitted(e)
	IF SELF:lCombined
		SELF:Text := SELF:SelectedItem:ToString()
		RETURN
	ENDIF
	IF SELF:oProperty:lAllowNULL .or. !SELF:Text:Trim() == ""
		SELF:SetProperty()
	END IF
	SELF:Hide()
RETURN
PROTECTED METHOD OnDropDownClosed(e AS EventArgs) AS VOID
	SUPER:OnDropDownClosed(e)
	IF !SELF:lCombined
		SELF:Hide()
		RETURN
	ENDIF
	IF SELF:oProperty:lAllowNULL .or. !SELF:Text:Trim() == ""
		SELF:SetProperty()
	END IF
	SELF:Hide()
RETURN

PROTECTED METHOD SetProperty() AS VOID
	LOCAL cValue AS STRING
	IF SELF:lCanceled
		RETURN
	ENDIF
	IF SELF:lCombined
		IF SELF:Text == "<Auto>"
			cValue := ""
		ELSE
			cValue := SELF:Text
		ENDIF
		IF cValue == SELF:cInitValue
			SELF:lCombined := TRUE
			RETURN
		ENDIF
		cValue := cValue:Replace((Char)34 , (Char)39)
		SELF:oPropertyPanel:SetProperty(cValue)
	ELSE
		SELF:oPropertyPanel:SetProperty(SELF:SelectedIndex)
	ENDIF
RETURN
END CLASS




INTERNAL CLASS PropertyButton INHERIT Button
CONSTRUCTOR()
	SUPER()
	SELF:SetStyle(ControlStyles.Selectable,FALSE)
RETURN
END CLASS




CLASS AccelSelectDlg INHERIT System.Windows.Forms.Form

	PROTECT oAutoButton AS System.Windows.Forms.Button
	PROTECT oOkButton AS System.Windows.Forms.Button
	PROTECT oCancelButton AS System.Windows.Forms.Button
	PROTECT oGroupBox1 AS System.Windows.Forms.GroupBox
	PROTECT oKeyComboBox AS System.Windows.Forms.ComboBox
	PROTECT oLabel1 AS System.Windows.Forms.Label
	PROTECT oControlCheckBox AS System.Windows.Forms.CheckBox
	PROTECT oShiftCheckBox AS System.Windows.Forms.CheckBox
	PROTECT oAltCheckBox AS System.Windows.Forms.CheckBox

// User code starts here (DO NOT remove this line)  ##USER##
	EXPORT oAccelerator AS MenuAccelerator

CONSTRUCTOR(_oAccelerator AS MenuAccelerator)

	SUPER()

	SELF:InitializeForm()
	
	LOCAL n AS INT
	FOR n := 0 UPTO VOMenuEditor.AccelKeys:Count - 1
		SELF:oKeyComboBox:Items:Add(VOMenuEditor.AccelKeys:GetName(n))
	NEXT
	
	IF !_oAccelerator:IsEmpty
		SELF:oKeyComboBox:Text := _oAccelerator:Key
		SELF:oControlCheckBox:Checked := _oAccelerator:Control
		SELF:oShiftCheckBox:Checked := _oAccelerator:Shift
		SELF:oAltCheckBox:Checked := _oAccelerator:Alt
	END IF

RETURN

METHOD InitializeForm() AS VOID

// IDE generated code (please DO NOT modify)

	SELF:Name := "AccelSelectDlg"
	SELF:SuspendLayout()
	SELF:FormBorderStyle := System.Windows.Forms.FormBorderStyle.FixedDialog
	SELF:Location := System.Drawing.Point{ 100 , 100 }
	SELF:MaximizeBox := FALSE
	SELF:MinimizeBox := FALSE
	SELF:ClientSize := System.Drawing.Size{ 220 , 144 }
	SELF:Text := "Accelerator"

	SELF:oAutoButton := System.Windows.Forms.Button{}
	SELF:oAutoButton:Name := "AutoButton"
	SELF:oAutoButton:Click += System.EventHandler{ SELF , @AutoButtonClick() }
	SELF:oAutoButton:Location := System.Drawing.Point{ 77 , 112 }
	SELF:oAutoButton:Size := System.Drawing.Size{ 64 , 23 }
	SELF:oAutoButton:TabIndex := 2
	SELF:oAutoButton:Text := "<&Auto>"
	SELF:Controls:Add(SELF:oAutoButton)
	
	SELF:oOkButton := System.Windows.Forms.Button{}
	SELF:oOkButton:Name := "OkButton"
	SELF:oOkButton:Click += System.EventHandler{ SELF , @OKButtonClick() }
	SELF:oOkButton:Location := System.Drawing.Point{ 8 , 112 }
	SELF:oOkButton:Size := System.Drawing.Size{ 64 , 23 }
	SELF:oOkButton:TabIndex := 1
	SELF:oOkButton:Text := "&OK"
	SELF:Controls:Add(SELF:oOkButton)
	
	SELF:oCancelButton := System.Windows.Forms.Button{}
	SELF:oCancelButton:Name := "CancelButton"
	SELF:oCancelButton:Location := System.Drawing.Point{ 144 , 112 }
	SELF:oCancelButton:Size := System.Drawing.Size{ 64 , 23 }
	SELF:oCancelButton:TabIndex := 3
	SELF:oCancelButton:Text := "&Cancel"
	SELF:Controls:Add(SELF:oCancelButton)
	
	SELF:oGroupBox1 := System.Windows.Forms.GroupBox{}
	SELF:oGroupBox1:Name := "GroupBox1"
	SELF:oGroupBox1:SuspendLayout()
	SELF:oGroupBox1:Location := System.Drawing.Point{ 8 , 8 }
	SELF:oGroupBox1:Size := System.Drawing.Size{ 200 , 95 }
	SELF:oGroupBox1:TabIndex := 0
	SELF:Controls:Add(SELF:oGroupBox1)
	

	SELF:oKeyComboBox := System.Windows.Forms.ComboBox{}
	SELF:oKeyComboBox:Name := "KeyComboBox"
	SELF:oKeyComboBox:DropDownStyle := System.Windows.Forms.ComboBoxStyle.DropDownList
	SELF:oKeyComboBox:Location := System.Drawing.Point{ 88 , 24 }
	SELF:oKeyComboBox:Size := System.Drawing.Size{ 96 , 21 }
	SELF:oKeyComboBox:TabIndex := 0
	SELF:oGroupBox1:Controls:Add(SELF:oKeyComboBox)
	
	SELF:oLabel1 := System.Windows.Forms.Label{}
	SELF:oLabel1:Name := "Label1"
	SELF:oLabel1:Location := System.Drawing.Point{ 16 , 24 }
	SELF:oLabel1:Size := System.Drawing.Size{ 64 , 23 }
	SELF:oLabel1:TabIndex := 1
	SELF:oLabel1:Text := "Accel. Key :"
	SELF:oGroupBox1:Controls:Add(SELF:oLabel1)
	
	SELF:oControlCheckBox := System.Windows.Forms.CheckBox{}
	SELF:oControlCheckBox:Name := "ControlCheckBox"
	SELF:oControlCheckBox:Location := System.Drawing.Point{ 16 , 56 }
	SELF:oControlCheckBox:Size := System.Drawing.Size{ 64 , 24 }
	SELF:oControlCheckBox:TabIndex := 2
	SELF:oControlCheckBox:Text := "Control"
	SELF:oGroupBox1:Controls:Add(SELF:oControlCheckBox)
	
	SELF:oShiftCheckBox := System.Windows.Forms.CheckBox{}
	SELF:oShiftCheckBox:Name := "ShiftCheckBox"
	SELF:oShiftCheckBox:Location := System.Drawing.Point{ 88 , 56 }
	SELF:oShiftCheckBox:Size := System.Drawing.Size{ 56 , 24 }
	SELF:oShiftCheckBox:TabIndex := 3
	SELF:oShiftCheckBox:Text := "Shift"
	SELF:oGroupBox1:Controls:Add(SELF:oShiftCheckBox)
	
	SELF:oAltCheckBox := System.Windows.Forms.CheckBox{}
	SELF:oAltCheckBox:Name := "AltCheckBox"
	SELF:oAltCheckBox:Location := System.Drawing.Point{ 152 , 56 }
	SELF:oAltCheckBox:Size := System.Drawing.Size{ 40 , 24 }
	SELF:oAltCheckBox:TabIndex := 4
	SELF:oAltCheckBox:Text := "Alt"
	SELF:oGroupBox1:Controls:Add(SELF:oAltCheckBox)
	
	SELF:oGroupBox1:ResumeLayout()
	SELF:ResumeLayout()

	SELF:AcceptButton := SELF:oOkButton

	SELF:CancelButton := SELF:oCancelButton

RETURN

METHOD AutoButtonClick(o AS OBJECT , e AS System.EventArgs) AS VOID
	SELF:oAccelerator := MenuAccelerator{"" , FALSE , FALSE , FALSE}
	SELF:DialogResult := DialogResult.OK
RETURN

METHOD OKButtonClick(o AS OBJECT , e AS System.EventArgs) AS VOID
	IF SELF:oKeyComboBox:SelectedIndex != - 1
		SELF:oAccelerator := MenuAccelerator{SELF:oKeyComboBox:Text , SELF:oControlCheckBox:Checked , SELF:oShiftCheckBox:Checked , SELF:oAltCheckBox:Checked}
		SELF:DialogResult := DialogResult.OK
	END IF
RETURN

END CLASS

