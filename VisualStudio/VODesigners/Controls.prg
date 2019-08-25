#using System.Windows.Forms
#using System.Drawing

INTERNAL CLASS DesignWindow INHERIT Panel
	EXPORT oItem AS DesignWindowItem
	CONSTRUCTOR(_oItem AS DesignWindowItem)
		SUPER()
		SELF:oItem := _oItem
		SELF:Location := Point{5,5}
		SELF:Size := Size{600,400}
		SELF:BorderStyle := BorderStyle.FixedSingle
		SELF:BackColor := Color.FromKnownColor(KnownColor.Control)
	RETURN
	PROTECTED METHOD WndProc(m REF Message) AS VOID
		DO CASE
		CASE WindowDesignerBase.HandleWndProc(SELF:oItem , REF m)
			RETURN
		CASE m:Msg == 132  //HITTEST
			m:Result := 1
			RETURN
		END CASE
		SUPER:WndProc(REF m)
	RETURN
END CLASS



INTERNAL CLASS DesignPushButton INHERIT Button
	EXPORT oItem AS DesignWindowItem
	PROTECT oBrush AS SolidBrush
	PROTECT oSF AS StringFormat
	EXPORT lTestMode AS LOGIC
	CONSTRUCTOR(_oItem AS DesignWindowItem)
		SUPER()
		SELF:oItem := _oItem
		SELF:oSF := StringFormat{}
	RETURN
	PROTECTED METHOD WndProc(m REF Message) AS VOID
		IF SELF:lTestMode .or. .not. WindowDesignerBase.HandleWndProc(SELF:oItem , REF m)
			SUPER:WndProc(REF m)
		END IF		
	RETURN
	METHOD ApplyVOStyleProperty(oProp AS VODesignProperty) AS VOID
		LOCAL cUpper AS STRING
		cUpper := oProp:Name:ToUpperInvariant()
		DO CASE
		CASE cUpper == "FLAT"
			IF (INT)oProp:Value == 0
				SELF:FlatStyle := FlatStyle.Flat
			ELSE
				SELF:FlatStyle := FlatStyle.Standard
			ENDIF
		CASE cUpper == "VERTICAL ALIGNMENT" .or. cUpper == "HORIZONTAL ALIGNMENT" .or. cUpper == "EXALIGNMENT" .or. cUpper == "MULTILINE"
			SELF:Invalidate()
		CASE IsVisibleStyle(oProp:Name)
			SELF:UpdateStyles()
		END CASE
	RETURN
	PROTECTED ACCESS CreateParams() AS CreateParams
		LOCAL oParams AS CreateParams
		oParams := SUPER:CreateParams
		Funcs.SetCreateParams(oParams , SELF:oItem)
	RETURN oParams
	PROTECTED METHOD OnPaint(e AS PaintEventArgs) AS VOID
		SUPER:OnPaint(e)
		
		IF SELF:oBrush == NULL .or. SELF:oBrush:Color != SELF:ForeColor
			SELF:oBrush := SolidBrush{SELF:ForeColor}
		END IF

//		SELF:oSF:Trimming := StringTrimming.Character

		TRY
			IF SELF:oItem:GetProperty("Multiline"):ValueLogic
				IF _And(SELF:oSF:FormatFlags , StringFormatFlags.NoWrap) != 0
					SELF:oSF:FormatFlags := (StringFormatFlags)_Xor(SELF:oSF:FormatFlags , StringFormatFlags.NoWrap)
				END IF
			ELSE
				SELF:oSF:FormatFlags := StringFormatFlags.NoWrap
			END IF
		CATCH
			SELF:oSF:FormatFlags := StringFormatFlags.NoWrap
		END TRY

		LOCAL cValue AS STRING
		TRY
			cValue := SELF:oItem:GetProperty("Horizontal Alignment"):TextValue:ToUpper()
		CATCH
			cValue := "AUTO"
		END TRY
		SWITCH cValue
		CASE "LEFT"
			SELF:oSF:Alignment := StringAlignment.Near
		case "CENTER" 
        case "AUTO"
			SELF:oSF:Alignment := StringAlignment.Center
		CASE "RIGHT"
			SELF:oSF:Alignment := StringAlignment.Far
		END SWITCH
		TRY
			cValue := SELF:oItem:GetProperty("Vertical Alignment"):TextValue:ToUpper()
		CATCH
			cValue := "AUTO"
		END TRY
		SWITCH cValue
		CASE "TOP"
			SELF:oSF:LineAlignment := StringAlignment.Near
		case "CENTER" 
        case "AUTO"
			SELF:oSF:LineAlignment := StringAlignment.Center
		CASE "BOTTOM"
			SELF:oSF:LineAlignment := StringAlignment.Far
		END SWITCH
		TRY
			IF SELF:oItem:GetProperty("ExAlignment"):TextValue:ToUpper() == "RIGHT"
				SELF:oSF:Alignment := StringAlignment.Far
			END IF
		END TRY
		TRY
			e:Graphics:DrawString(SELF:oItem:GetProperty("Caption"):TextValue , SELF:Font , SELF:oBrush , SELF:ClientRectangle , SELF:oSF)
		END TRY
		
	RETURN
	
END CLASS

INTERNAL CLASS DesignFixedText INHERIT Label
	EXPORT oItem AS DesignWindowItem
	EXPORT lTestMode AS LOGIC
	CONSTRUCTOR(_oItem AS DesignWindowItem)
		SUPER()
		SELF:oItem := _oItem
		SELF:TextAlign := ContentAlignment.MiddleLeft
		SELF:FlatStyle := FlatStyle.System // fixes text overflow on small labels
	RETURN
	PROTECTED METHOD WndProc(m REF Message) AS VOID
		IF SELF:lTestMode .or. .not. WindowDesignerBase.HandleWndProc(SELF:oItem , REF m)
			SUPER:WndProc(REF m)
		END IF		
	RETURN
	METHOD ApplyVOStyleProperty(oProp AS VODesignProperty) AS VOID
		DO CASE
		CASE oProp:Name == "Center vertically" .or. oProp:Name == "Alignment" .or. oProp:Name == "ExAlignment"
			LOCAL x,y AS INT
			LOCAL cValue AS STRING
			LOCAL lExLeft AS LOGIC
			TRY
				lExLeft := SELF:oItem:GetProperty("ExAlignment"):TextValue:ToUpper() == "RIGHT"
			END TRY
			TRY
				IF SELF:oItem:GetProperty("Center vertically"):ValueLogic
					y := 0x10
				ELSE
					y := 0x1
				END IF
			CATCH
				y := 0x1
			END TRY
			TRY
				cValue := SELF:oItem:GetProperty("Alignment"):TextValue:ToUpper()
			CATCH
				cValue := "LEFT"
			END TRY
			DO CASE
			CASE cValue:StartsWith("LEFT")
				x := 0x1
			CASE cValue:StartsWith("CENTER")
				x := 0x2
			CASE cValue:StartsWith("RIGHT")
				x := 0x4
			CASE cValue:StartsWith("SIMPLE")
				x := 0x1
				IF .not. lExLeft
					y := 0x1
				END IF
			END CASE
			IF lExLeft
				x := 0x4
			END IF
			IF y == 0x10
				SELF:FlatStyle := FlatStyle.Standard
			ELSE
				SELF:FlatStyle := FlatStyle.System
			END IF
			SELF:TextAlign := (ContentAlignment)(x*y)
			
		CASE IsVisibleStyle(oProp:Name)
			SELF:UpdateStyles()
		END CASE
	RETURN
	PROTECTED ACCESS CreateParams() AS CreateParams
		LOCAL oParams AS CreateParams
		oParams := SUPER:CreateParams
		Funcs.SetCreateParams(oParams , SELF:oItem)
	RETURN oParams
END CLASS

INTERNAL CLASS DesignCheckBox INHERIT CheckBox
	EXPORT oItem AS DesignWindowItem
	EXPORT lTestMode AS LOGIC
	CONSTRUCTOR(_oItem AS DesignWindowItem)
		SUPER()
		SELF:oItem := _oItem
	RETURN
	PROTECTED METHOD WndProc(m REF Message) AS VOID
		IF SELF:lTestMode .or. .not. WindowDesignerBase.HandleWndProc(SELF:oItem , REF m)
			SUPER:WndProc(REF m)
		END IF		
	RETURN
	METHOD ApplyVOStyleProperty(oProp AS VODesignProperty) AS VOID
		LOCAL cUpper AS STRING
		cUpper := oProp:Name:ToUpper()
		DO CASE
		CASE cUpper == "VERTICAL ALIGNMENT" .or. cUpper == "HORIZONTAL ALIGNMENT" .or. cUpper == "EXALIGNMENT" .or. cUpper == "TEXT LEFT"
			TRY
				SELF:TextAlign := GetButtonAlignment(SELF:oItem , TRUE)
				SELF:CheckAlign := GetButtonAlignment(SELF:oItem , FALSE)
			END TRY
		CASE cUpper == "PUSH LIKE"
			SELF:Appearance := iif(oProp:ValueLogic , Appearance.Button , Appearance.Normal)
		CASE IsVisibleStyle(oProp:Name)
			SELF:UpdateStyles()
		END CASE
	RETURN
	PROTECTED ACCESS CreateParams() AS CreateParams
		LOCAL oParams AS CreateParams
		oParams := SUPER:CreateParams
		Funcs.SetCreateParams(oParams , SELF:oItem)
	RETURN oParams
END CLASS

INTERNAL CLASS DesignRadioButton INHERIT RadioButton
	EXPORT oItem AS DesignWindowItem
	EXPORT lTestMode AS LOGIC
	CONSTRUCTOR(_oItem AS DesignWindowItem)
		SUPER()
		SELF:oItem := _oItem
	RETURN
	PROTECTED METHOD WndProc(m REF Message) AS VOID
		IF SELF:lTestMode .or. .not. WindowDesignerBase.HandleWndProc(SELF:oItem , REF m)
			SUPER:WndProc(REF m)
		END IF		
	RETURN
	METHOD ApplyVOStyleProperty(oProp AS VODesignProperty) AS VOID
		LOCAL cUpper AS STRING
		cUpper := oProp:Name:ToUpper()
		DO CASE
		CASE cUpper == "VERTICAL ALIGNMENT" .or. cUpper == "HORIZONTAL ALIGNMENT" .or. cUpper == "EXALIGNMENT" .or. cUpper == "TEXT LEFT"
			TRY
				SELF:TextAlign := GetButtonAlignment(SELF:oItem , TRUE)
				SELF:CheckAlign := GetButtonAlignment(SELF:oItem , FALSE)
			END TRY
		CASE cUpper == "PUSH LIKE"
			SELF:Appearance := iif(oProp:ValueLogic , Appearance.Button , Appearance.Normal)
		CASE IsVisibleStyle(oProp:Name)
			SELF:UpdateStyles()
		END CASE
	RETURN
	PROTECTED ACCESS CreateParams() AS CreateParams
		LOCAL oParams AS CreateParams
		oParams := SUPER:CreateParams
		Funcs.SetCreateParams(oParams , SELF:oItem)
	RETURN oParams
END CLASS

INTERNAL CLASS DesignEdit INHERIT TextBox
	EXPORT oItem AS DesignWindowItem
	EXPORT lTestMode AS LOGIC
	CONSTRUCTOR(_oItem AS DesignWindowItem)
		SUPER()
		SELF:oItem := _oItem
		SELF:Multiline := TRUE
	RETURN
	PROTECTED METHOD WndProc(m REF Message) AS VOID
		IF SELF:lTestMode .or. .not. WindowDesignerBase.HandleWndProc(SELF:oItem , REF m)
			SUPER:WndProc(REF m)
		END IF		
	RETURN
	METHOD ApplyVOStyleProperty(oProp AS VODesignProperty) AS VOID
		DO CASE
		CASE IsVisibleStyle(oProp:Name)
			SELF:UpdateStyles()
		END CASE
	RETURN
	PROTECTED ACCESS CreateParams() AS CreateParams
		LOCAL oParams AS CreateParams
		oParams := SUPER:CreateParams
		Funcs.SetCreateParams(oParams , SELF:oItem)
	RETURN oParams
END CLASS

INTERNAL CLASS DesignComboBox INHERIT ComboBox
	EXPORT oItem AS DesignWindowItem
	EXPORT lTestMode AS LOGIC
	CONSTRUCTOR(_oItem AS DesignWindowItem)
		SUPER()
		SELF:oItem := _oItem
		SELF:DropDownStyle := ComboBoxStyle.DropDownList
	RETURN
	PROTECTED METHOD WndProc(m REF Message) AS VOID
		IF SELF:lTestMode .or. .not. WindowDesignerBase.HandleWndProc(SELF:oItem , REF m)
			SUPER:WndProc(REF m)
		END IF		
	RETURN
	METHOD ApplyVOStyleProperty(oProp AS VODesignProperty) AS VOID
		DO CASE
		CASE oProp:Name == "ComboBox Type"
			IF (INT)oProp:Value == 0
				SELF:DropDownStyle := ComboBoxStyle.Simple
			ELSE
				SELF:DropDownStyle := ComboBoxStyle.DropDownList
			ENDIF
		CASE IsVisibleStyle(oProp:Name)
			SELF:UpdateStyles()
		END CASE
	RETURN
	PROTECTED ACCESS CreateParams() AS CreateParams
		LOCAL oParams AS CreateParams
		oParams := SUPER:CreateParams
		Funcs.SetCreateParams(oParams , SELF:oItem)
	RETURN oParams
END CLASS

INTERNAL CLASS DesignListBox INHERIT ListBox
	EXPORT oItem AS DesignWindowItem
	EXPORT lTestMode AS LOGIC
	CONSTRUCTOR(_oItem AS DesignWindowItem)
		SUPER()
		SELF:oItem := _oItem
	RETURN
	PROTECTED METHOD WndProc(m REF Message) AS VOID
		IF SELF:lTestMode .or. .not. WindowDesignerBase.HandleWndProc(SELF:oItem , REF m)
			SUPER:WndProc(REF m)
		END IF		
	RETURN
	METHOD ApplyVOStyleProperty(oProp AS VODesignProperty) AS VOID
		DO CASE
		CASE IsVisibleStyle(oProp:Name)
			SELF:UpdateStyles()
		END CASE
	RETURN
	PROTECTED ACCESS CreateParams() AS CreateParams
		LOCAL oParams AS CreateParams
		oParams := SUPER:CreateParams
		Funcs.SetCreateParams(oParams , SELF:oItem)
	RETURN oParams
END CLASS

INTERNAL CLASS DesignListView INHERIT ListView
	EXPORT oItem AS DesignWindowItem
	EXPORT lTestMode AS LOGIC
	CONSTRUCTOR(_oItem AS DesignWindowItem)
		SUPER()
		SELF:oItem := _oItem
	RETURN
	PROTECTED METHOD WndProc(m REF Message) AS VOID
		IF SELF:lTestMode .or. .not. WindowDesignerBase.HandleWndProc(SELF:oItem , REF m)
			SUPER:WndProc(REF m)
		END IF		
	RETURN
	METHOD ApplyVOStyleProperty(oProp AS VODesignProperty) AS VOID
		DO CASE
		CASE IsVisibleStyle(oProp:Name)
			SELF:UpdateStyles()
		END CASE
	RETURN
	PROTECTED ACCESS CreateParams() AS CreateParams
		LOCAL oParams AS CreateParams
		oParams := SUPER:CreateParams
		Funcs.SetCreateParams(oParams , SELF:oItem)
	RETURN oParams
END CLASS

INTERNAL CLASS DesignTreeView INHERIT TreeView
	EXPORT oItem AS DesignWindowItem
	EXPORT lTestMode AS LOGIC
	CONSTRUCTOR(_oItem AS DesignWindowItem)
		SUPER()
		SELF:oItem := _oItem
		SELF:Nodes:Add("TreeNode")
		SELF:Nodes[0]:Nodes:Add("SubNode 1")
		SELF:Nodes[0]:Nodes:Add("SubNode 2")
		SELF:Nodes[0]:Nodes:Add("SubNode 3")
		SELF:ExpandAll()
	RETURN
	PROTECTED METHOD WndProc(m REF Message) AS VOID
		IF SELF:lTestMode .or. .not. WindowDesignerBase.HandleWndProc(SELF:oItem , REF m)
			SUPER:WndProc(REF m)
		END IF		
	RETURN
	METHOD ApplyVOStyleProperty(oProp AS VODesignProperty) AS VOID
		DO CASE
		CASE IsVisibleStyle(oProp:Name)
			SELF:UpdateStyles()
		END CASE
	RETURN
	PROTECTED ACCESS CreateParams() AS CreateParams
		LOCAL oParams AS CreateParams
		oParams := SUPER:CreateParams
		Funcs.SetCreateParams(oParams , SELF:oItem)
	RETURN oParams
END CLASS

INTERNAL CLASS DesignProgressBar INHERIT ProgressBar
	EXPORT oItem AS DesignWindowItem
	EXPORT lTestMode AS LOGIC
	CONSTRUCTOR(_oItem AS DesignWindowItem)
		SUPER()
		SELF:oItem := _oItem
	RETURN
	PROTECTED METHOD WndProc(m REF Message) AS VOID
		IF SELF:lTestMode .or. .not. WindowDesignerBase.HandleWndProc(SELF:oItem , REF m)
			SUPER:WndProc(REF m)
		END IF		
	RETURN
	METHOD ApplyVOStyleProperty(oProp AS VODesignProperty) AS VOID
		DO CASE
		CASE IsVisibleStyle(oProp:Name)
			SELF:UpdateStyles()
		END CASE
	RETURN
	PROTECTED ACCESS CreateParams() AS CreateParams
		LOCAL oParams AS CreateParams
		oParams := SUPER:CreateParams
		Funcs.SetCreateParams(oParams , SELF:oItem)
	RETURN oParams
END CLASS

INTERNAL CLASS DesignGroupBox INHERIT GroupBox
	EXPORT oItem AS DesignWindowItem
	EXPORT lTestMode AS LOGIC
	CONSTRUCTOR(_oItem AS DesignWindowItem)
		SUPER()
		SELF:oItem := _oItem
	RETURN
	PROTECTED METHOD WndProc(m REF Message) AS VOID
		IF SELF:lTestMode .or. .not. WindowDesignerBase.HandleWndProc(SELF:oItem , REF m)
			SUPER:WndProc(REF m)
		END IF		
	RETURN
	METHOD ApplyVOStyleProperty(oProp AS VODesignProperty) AS VOID
		DO CASE
		CASE IsVisibleStyle(oProp:Name)
			SELF:UpdateStyles()
		END CASE
	RETURN
	PROTECTED ACCESS CreateParams() AS CreateParams
		LOCAL oParams AS CreateParams
		oParams := SUPER:CreateParams
		Funcs.SetCreateParams(oParams , SELF:oItem)
	RETURN oParams
END CLASS


INTERNAL CLASS DesignMonthCalendar INHERIT MonthCalendar
	EXPORT oItem AS DesignWindowItem
	EXPORT lTestMode AS LOGIC
	CONSTRUCTOR(_oItem AS DesignWindowItem)
		SUPER()
		SELF:oItem := _oItem
	RETURN
	PROTECTED METHOD WndProc(m REF Message) AS VOID
		IF SELF:lTestMode .or. .not. WindowDesignerBase.HandleWndProc(SELF:oItem , REF m)
			SUPER:WndProc(REF m)
		END IF		
	RETURN
	METHOD ApplyVOStyleProperty(oProp AS VODesignProperty) AS VOID
		DO CASE
		CASE IsVisibleStyle(oProp:Name)
			SELF:UpdateStyles()
		END CASE
	RETURN
	PROTECTED ACCESS CreateParams() AS CreateParams
		LOCAL oParams AS CreateParams
		oParams := SUPER:CreateParams
		Funcs.SetCreateParams(oParams , SELF:oItem)
	RETURN oParams
END CLASS


INTERNAL CLASS DesignHorizontalScrollBar INHERIT HScrollBar
	EXPORT oItem AS DesignWindowItem
	EXPORT lTestMode AS LOGIC
	CONSTRUCTOR(_oItem AS DesignWindowItem)
		SUPER()
		SELF:oItem := _oItem
	RETURN
	PROTECTED METHOD WndProc(m REF Message) AS VOID
		IF SELF:lTestMode .or. .not. WindowDesignerBase.HandleWndProc(SELF:oItem , REF m)
			SUPER:WndProc(REF m)
		END IF		
	RETURN
	METHOD ApplyVOStyleProperty(oProp AS VODesignProperty) AS VOID
		DO CASE
		CASE IsVisibleStyle(oProp:Name)
			SELF:UpdateStyles()
		END CASE
	RETURN
	PROTECTED ACCESS CreateParams() AS CreateParams
		LOCAL oParams AS CreateParams
		oParams := SUPER:CreateParams
		Funcs.SetCreateParams(oParams , SELF:oItem)
	RETURN oParams
END CLASS
INTERNAL CLASS DesignVerticalScrollBar INHERIT VScrollBar
	EXPORT oItem AS DesignWindowItem
	EXPORT lTestMode AS LOGIC
	CONSTRUCTOR(_oItem AS DesignWindowItem)
		SUPER()
		SELF:oItem := _oItem
	RETURN
	PROTECTED METHOD WndProc(m REF Message) AS VOID
		IF SELF:lTestMode .or. .not. WindowDesignerBase.HandleWndProc(SELF:oItem , REF m)
			SUPER:WndProc(REF m)
		END IF		
	RETURN
	METHOD ApplyVOStyleProperty(oProp AS VODesignProperty) AS VOID
		DO CASE
		CASE IsVisibleStyle(oProp:Name)
			SELF:UpdateStyles()
		END CASE
	RETURN
	PROTECTED ACCESS CreateParams() AS CreateParams
		LOCAL oParams AS CreateParams
		oParams := SUPER:CreateParams
		Funcs.SetCreateParams(oParams , SELF:oItem)
	RETURN oParams
END CLASS


INTERNAL CLASS DesignHorizontalSlider INHERIT TrackBar
	EXPORT oItem AS DesignWindowItem
	EXPORT lTestMode AS LOGIC
	CONSTRUCTOR(_oItem AS DesignWindowItem)
		SUPER()
		SELF:oItem := _oItem
		SELF:Orientation := Orientation.Horizontal
	RETURN
	PROTECTED METHOD WndProc(m REF Message) AS VOID
		IF SELF:lTestMode .or. .not. WindowDesignerBase.HandleWndProc(SELF:oItem , REF m)
			SUPER:WndProc(REF m)
		END IF		
	RETURN
	METHOD ApplyVOStyleProperty(oProp AS VODesignProperty) AS VOID
		DO CASE
		CASE IsVisibleStyle(oProp:Name)
			SELF:UpdateStyles()
		END CASE
	RETURN
	PROTECTED ACCESS CreateParams() AS CreateParams
		LOCAL oParams AS CreateParams
		oParams := SUPER:CreateParams
		Funcs.SetCreateParams(oParams , SELF:oItem)
	RETURN oParams
END CLASS
INTERNAL CLASS DesignVerticalSlider INHERIT TrackBar
	EXPORT oItem AS DesignWindowItem
	EXPORT lTestMode AS LOGIC
	CONSTRUCTOR(_oItem AS DesignWindowItem)
		SUPER()
		SELF:oItem := _oItem
		SELF:Orientation := Orientation.Vertical
	RETURN
	PROTECTED METHOD WndProc(m REF Message) AS VOID
		IF SELF:lTestMode .or. .not. WindowDesignerBase.HandleWndProc(SELF:oItem , REF m)
			SUPER:WndProc(REF m)
		END IF		
	RETURN
	METHOD ApplyVOStyleProperty(oProp AS VODesignProperty) AS VOID
		DO CASE
		CASE IsVisibleStyle(oProp:Name)
			SELF:UpdateStyles()
		END CASE
	RETURN
	PROTECTED ACCESS CreateParams() AS CreateParams
		LOCAL oParams AS CreateParams
		oParams := SUPER:CreateParams
		Funcs.SetCreateParams(oParams , SELF:oItem)
	RETURN oParams
END CLASS


INTERNAL CLASS DesignHorizontalSpinner INHERIT HScrollBar
	EXPORT oItem AS DesignWindowItem
	EXPORT lTestMode AS LOGIC
	CONSTRUCTOR(_oItem AS DesignWindowItem)
		SUPER()
		SELF:oItem := _oItem
	RETURN
	PROTECTED METHOD WndProc(m REF Message) AS VOID
		IF SELF:lTestMode .or. .not. WindowDesignerBase.HandleWndProc(SELF:oItem , REF m)
			SUPER:WndProc(REF m)
		END IF		
	RETURN
	METHOD ApplyVOStyleProperty(oProp AS VODesignProperty) AS VOID
		DO CASE
		CASE IsVisibleStyle(oProp:Name)
			SELF:UpdateStyles()
		END CASE
	RETURN
	PROTECTED ACCESS CreateParams() AS CreateParams
		LOCAL oParams AS CreateParams
		oParams := SUPER:CreateParams
		Funcs.SetCreateParams(oParams , SELF:oItem)
	RETURN oParams
END CLASS
INTERNAL CLASS DesignVerticalSpinner INHERIT VScrollBar
	EXPORT oItem AS DesignWindowItem
	EXPORT lTestMode AS LOGIC
	CONSTRUCTOR(_oItem AS DesignWindowItem)
		SUPER()
		SELF:oItem := _oItem
	RETURN
	PROTECTED METHOD WndProc(m REF Message) AS VOID
		IF SELF:lTestMode .or. .not. WindowDesignerBase.HandleWndProc(SELF:oItem , REF m)
			SUPER:WndProc(REF m)
		END IF		
	RETURN
	METHOD ApplyVOStyleProperty(oProp AS VODesignProperty) AS VOID
		DO CASE
		CASE IsVisibleStyle(oProp:Name)
			SELF:UpdateStyles()
		END CASE
	RETURN
	PROTECTED ACCESS CreateParams() AS CreateParams
		LOCAL oParams AS CreateParams
		oParams := SUPER:CreateParams
		Funcs.SetCreateParams(oParams , SELF:oItem)
	RETURN oParams
END CLASS




INTERNAL CLASS VOTabPageOptions
	EXPORT cName AS STRING
	EXPORT cCaption AS STRING
	EXPORT lDataAware AS LOGIC
	CONSTRUCTOR()
		SELF:cName := ""
		SELF:cCaption := ""
	RETURN
	METHOD Set(oOptions AS VOTabPageOptions) AS VOID
		SELF:cName := oOptions:cName
		SELF:cCaption := oOptions:cCaption
		SELF:lDataAware := oOptions:lDataAware
	RETURN
END CLASS

INTERNAL CLASS DesignTabControl INHERIT TabControl
	EXPORT oItem AS DesignWindowItem
	EXPORT lTestMode AS LOGIC
	CONSTRUCTOR(_oItem AS DesignWindowItem)
		SUPER()
		SELF:oItem := _oItem
	RETURN
	METHOD GetPageUnderPos(oPoint AS Point) AS INT
		LOCAL oRect AS Rectangle
		LOCAL n AS INT
		FOR n := 0 UPTO SELF:TabCount-1
			oRect := SELF:GetTabRect(n)
			IF oRect:Contains(oPoint)
				RETURN n
			END IF
		NEXT
	RETURN -1
	METHOD AddPage() AS VOID
		LOCAL oPage AS DesignTabPage
		oPage := DesignTabPage{SELF:oItem}
		oPage:Text := "Page" + (SELF:TabPages:Count + 1):ToString()
		oPage:Name := SELF:oItem:Name + "_PAGE" + (SELF:TabPages:Count + 1):ToString()
		oPage:Tag := FALSE
		SELF:TabPages:Add(oPage)
	RETURN
	METHOD SetTabPageOptions(oOptions AS VOTabPageOptions) AS VOID
		SELF:SetTabPageOptions(SELF:SelectedIndex + 1 , oOptions)
	RETURN
	METHOD SetTabPageOptions(nPage AS INT, oOptions AS VOTabPageOptions) AS VOID
		LOCAL oPage AS DesignTabPage
		IF SELF:TabPages:Count < nPage .or. nPage <= 0
			RETURN
		ENDIF
		oPage := (DesignTabPage)SELF:TabPages[nPage - 1]
		oPage:oOptions:Set(oOptions)
		oPage:Text := Funcs.TranslateCaption( oOptions:cCaption , FALSE )
	RETURN
	METHOD GetTabPageOptions() AS VOTabPageOptions
	RETURN SELF:GetTabPageOptions(SELF:SelectedIndex + 1)
	METHOD GetTabPageOptions(nPage AS INT) AS VOTabPageOptions
		LOCAL oOptions AS VOTabPageOptions
		IF SELF:TabPages:Count == 0
			RETURN VOTabPageOptions{}
		ENDIF
		oOptions := ((DesignTabPage)SELF:TabPages[nPage - 1]):oOptions
	RETURN oOptions
	PROTECTED METHOD WndProc(m REF Message) AS VOID
		IF SELF:lTestMode .or. .not. WindowDesignerBase.HandleWndProc(SELF:oItem , REF m)
			SUPER:WndProc(REF m)
		END IF		
	RETURN
	METHOD ApplyVOStyleProperty(oProp AS VODesignProperty) AS VOID
		DO CASE
		CASE IsVisibleStyle(oProp:Name)
			SELF:UpdateStyles()
		END CASE
	RETURN
	PROTECTED ACCESS CreateParams() AS CreateParams
		LOCAL oParams AS CreateParams
		oParams := SUPER:CreateParams
		Funcs.SetCreateParams(oParams , SELF:oItem)
	RETURN oParams
END CLASS
INTERNAL CLASS DesignTabPage INHERIT TabPage
	EXPORT oItem AS DesignWindowItem
	EXPORT oOptions AS VOTabPageOptions
	EXPORT lTestMode AS LOGIC
	CONSTRUCTOR(_oItem AS DesignWindowItem)
		SUPER()
		SELF:oItem := _oItem
		SELF:Text := "Page"
		SELF:oOptions := VOTabPageOptions{}
		SELF:SetStyle(ControlStyles.Selectable , FALSE)
	RETURN
	PROTECTED METHOD WndProc(m REF Message) AS VOID
		IF SELF:lTestMode .or. .not. WindowDesignerBase.HandleWndProc(SELF:oItem , REF m)
			SUPER:WndProc(REF m)
		END IF		
	RETURN
END CLASS

INTERNAL CLASS DesignbBrowser INHERIT Panel
//INTERNAL CLASS DesignbBrowser INHERIT DataBrowserControl
	EXPORT oItem AS DesignWindowItem
	EXPORT nHeaderHeight AS INT
	EXPORT lTestMode AS LOGIC
	CONSTRUCTOR(_oItem AS DesignWindowItem)
		SUPER()
		SELF:oItem := _oItem
//		SELF:BorderStyle := BorderStyle.Fixed3D
		SELF:BorderStyle := BorderStyle.FixedSingle
		SELF:nHeaderHeight := 20
	RETURN
	PROTECTED METHOD WndProc(m REF Message) AS VOID
		IF SELF:lTestMode .or. .not. WindowDesignerBase.HandleWndProc(SELF:oItem , REF m)
			SUPER:WndProc(REF m)
		END IF		
	RETURN
END CLASS

CLASS DesignDataColumn INHERIT PictureBox
//INTERNAL CLASS DesignbDataColumn INHERIT PictureBox
//INTERNAL CLASS DesignbBrowser INHERIT DataBrowserControl
	EXPORT oItem AS DesignWindowItem
	EXPORT lTestMode AS LOGIC

	PROTECT nHeaderHeight AS INT
	PROTECT nRowHeight AS INT
	PROTECT oBrushBlack AS SolidBrush
	PROTECT oBrushWhite AS SolidBrush
	PROTECT oPenGray AS Pen
	PROTECT nCurX,nCurY AS DWORD
	PROTECT lMovingSplitter AS LOGIC
	PROTECT nMovingSplitter AS INT
	PROTECT nMovingColumn AS INT
	PROTECT nMovingStartLength AS INT
	PROTECT oSF_Right,oSF_Left,oSF_Center AS StringFormat


	CONSTRUCTOR(_oItem AS DesignWindowItem)
		SUPER()
		SELF:oItem := _oItem
		SELF:Dock := DockStyle.Left
		SELF:BackColor := Color.Aqua

		SELF:BackColor:=Color.White
		SELF:BorderStyle:=BorderStyle.Fixed3D
		
		SELF:oSF_Right:=StringFormat{}
		SELF:oSF_Right:Trimming:=StringTrimming.None
		SELF:oSF_Right:FormatFlags:=StringFormatFlags.NoWrap + StringFormatFlags.DirectionRightToLeft
		
		SELF:oSF_Left:=StringFormat{}
		SELF:oSF_Left:Trimming:=StringTrimming.None
		SELF:oSF_Left:FormatFlags:=StringFormatFlags.NoWrap
		
		SELF:oSF_Center:=StringFormat{}
		SELF:oSF_Center:Trimming:=StringTrimming.None
		SELF:oSF_Center:FormatFlags:=StringFormatFlags.LineLimit
		SELF:oSF_Center:Alignment:=StringAlignment.Center
			
		SELF:nHeaderHeight:=20
		SELF:nRowHeight:=20
		SELF:oBrushBlack:=SolidBrush{Color.Black}
		SELF:oBrushWhite:=SolidBrush{Color.White}
		SELF:oPenGray:=Pen{Color.FromArgb(152,152,152)}
		
		SELF:Width := 50
		
	RETURN
	PROTECTED METHOD OnResize(e AS EventArgs) AS VOID
		SUPER:OnResize(e)
		SELF:Invalidate()
	RETURN
	
	PROTECTED METHOD WndProc(m REF Message) AS VOID
		IF SELF:lTestMode .or. .not. WindowDesignerBase.HandleWndProc(SELF:oItem , REF m)
			SUPER:WndProc(REF m)
		END IF		
	RETURN
	PROTECTED METHOD OnPaint(e AS PaintEventArgs) AS VOID
		LOCAL x,y AS REAL4
		LOCAL rLength AS REAL4
		
		SUPER:OnPaint(e)
		
		y:=REAL4(2.0)
		e:Graphics:FillRectangle(SolidBrush{Color.FromKnownColor(KnownColor.Control)},0,0,SELF:Width,SELF:nHeaderHeight)
		rLength := REAL4(SELF:Width)
		
		
		e:Graphics:DrawLine(Pen{Color.White},REAL4(x+1.0),REAL4(1.0),REAL4(x+rLength-2.0),REAL4(1.0))
		e:Graphics:DrawLine(Pen{Color.White},REAL4(x+2.0),REAL4(1.0),REAL4(x+2.0),REAL4(SELF:Height))
		
		e:Graphics:DrawLine(SELF:oPenGray,REAL4(x+rLength-1.0),REAL4(0),REAL4(x+rLength-1.0),REAL4(SELF:nHeaderHeight))
		e:Graphics:DrawLine(SELF:oPenGray,REAL4(x+rLength-2.0),REAL4(1),REAL4(x+rLength-2.0),REAL4(SELF:nHeaderHeight))
		
		e:Graphics:DrawLine(SELF:oPenGray,REAL4(x+1.0),REAL4(SELF:nHeaderHeight-1),REAL4(x+rLength-2.0),REAL4(SELF:nHeaderHeight-1))
		e:Graphics:DrawLine(SELF:oPenGray,REAL4(x+2.0),REAL4(SELF:nHeaderHeight-2),REAL4(x+rLength-2.0),REAL4(SELF:nHeaderHeight-2))
		
		e:Graphics:DrawString(SELF:oItem:Caption,;
											SELF:Font,;
											SELF:oBrushBlack,;
											RectangleF{x,REAL4(4),rLength,REAL4(20)},;
											SELF:oSF_Center)
		
		x+=rLength
		e:Graphics:DrawLine(Pen{Color.Black},x,REAL4(0),x,REAL4(SELF:Height))
		y:=REAL4(SELF:nHeaderHeight)
		e:Graphics:DrawLine(Pen{Color.Black},REAL4(0),y,REAL4(SELF:Width),y)
	RETURN
	
END CLASS

INTERNAL CLASS DesignEmpty INHERIT Panel
	EXPORT oItem AS DesignWindowItem
	EXPORT lTestMode AS LOGIC
	PROTECT oBrush AS SolidBrush
	PROTECT oSF AS StringFormat
	CONSTRUCTOR(_oItem AS DesignWindowItem)
		SUPER()
		SELF:oItem := _oItem
		SELF:BorderStyle := BorderStyle.FixedSingle
		SELF:oBrush := SolidBrush{Color.Black}
		SELF:oSF := StringFormat{}
		SELF:oSF:Alignment := StringAlignment.Center
		SELF:oSF:LineAlignment := StringAlignment.Center
	RETURN
	PROTECTED METHOD WndProc(m REF Message) AS VOID
		IF SELF:lTestMode .or. .not. WindowDesignerBase.HandleWndProc(SELF:oItem , REF m)
			SUPER:WndProc(REF m)
		END IF		
	RETURN
	METHOD ApplyVOStyleProperty(oProp AS VODesignProperty) AS VOID
		DO CASE
		CASE IsVisibleStyle(oProp:Name)
			SELF:UpdateStyles()
		END CASE
	RETURN
	PROTECTED ACCESS CreateParams() AS CreateParams
		LOCAL oParams AS CreateParams
		oParams := SUPER:CreateParams
		Funcs.SetCreateParams(oParams , SELF:oItem)
	RETURN oParams
	PROTECTED METHOD OnPaint(e AS PaintEventArgs) AS VOID
		SUPER:OnPaint(e)
		e:Graphics:DrawString(SELF:oItem:Name , SELF:Font , SELF:oBrush , Rectangle{0,0,SELF:Width,SELF:Height} , SELF:oSF)
	RETURN
END CLASS


INTERNAL CLASS DesignNativeWindow INHERIT NativeWindow
	EXPORT oItem AS DesignWindowItem
	EXPORT lTestMode AS LOGIC
	CONSTRUCTOR(_oItem AS DesignWindowItem)
		SUPER()
		SELF:oItem := _oItem
	RETURN
	PROTECTED METHOD WndProc(m REF Message) AS VOID
		SUPER:WndProc(REF m)
	RETURN
END CLASS


INTERNAL FUNCTION GetButtonAlignment(oItem AS DesignWindowItem , lText AS LOGIC) AS ContentAlignment
	LOCAL eAlignment AS ContentAlignment
	LOCAL cValue AS STRING
	LOCAL lExLeft AS LOGIC
	LOCAL lTextLeft AS LOGIC
	LOCAL x,y AS INT

	TRY
		lExLeft := oItem:GetProperty("ExAlignment"):TextValue:ToUpper() == "RIGHT"
		lTextLeft := oItem:GetProperty("Text Left"):ValueLogic
	END TRY

	TRY
		cValue := oItem:GetProperty("Vertical Alignment"):TextValue:ToUpper()
	CATCH
		cValue := "CENTER"
	END TRY
	SWITCH cValue
	CASE "TOP"
		y := 0x1
	CASE "CENTER"
		y := 0x10
	CASE "BOTTOM"
		y := 0x100
	OTHERWISE // "AUTO"
		y := 0x10
	END SWITCH

	TRY
		cValue := oItem:GetProperty("Horizontal Alignment"):TextValue:ToUpper()
	CATCH
		cValue := "LEFT"
	END TRY
	SWITCH cValue
	CASE "LEFT"
		IF lText
			IF lExLeft
				x := 0x2 // kai omws, center!
			ELSE
				x := 0x1
			ENDIF
		ELSE
			IF lExLeft
				x := 0x4
			ELSE
				x := 0x1
			ENDIF
		END IF
	CASE "CENTER"
		IF lText
			x := 0x2
		ELSE
			IF lExLeft .or. lTextLeft
				x := 0x4
			ELSE
				x := 0x1
			END IF
		END IF
	CASE "RIGHT"
		IF lText
			x := 0x4
		ELSE
			IF lExLeft .or. lTextLeft
				x := 0x4
			ELSE
				x := 0x1
			END IF
		END IF
	OTHERWISE // "AUTO"
		IF lExLeft
			x := 0x4
		ELSE
			x := 0x1
		ENDIF
	END SWITCH
	IF .not. lText // check/radio
		IF lTextLeft
			x := 0x4
		END IF
	END IF
/*	IF y == 0x10
		SELF:FlatStyle := FlatStyle.Standard
	ELSE
		SELF:FlatStyle := FlatStyle.System
	END IF*/
	eAlignment := (ContentAlignment)(x*y)
	
RETURN eAlignment

INTERNAL FUNCTION IsVisibleStyle(cProp AS STRING) AS LOGIC
	switch cProp:ToUpper()
    case "BORDER" 
    case "CLIENT EDGE" 
    case "STATIC EDGE" 
    case "MODAL FRAME"
        return true
    OTHERWISE
        return false
    end switch

