#using System.Windows.Forms
#using System.Drawing

ENUM FillUsingType
	MEMBER UseArray
	MEMBER UseMethod
	MEMBER UseServer
END ENUM


CLASS FillUsingClass
	EXPORT eType AS FillUsingType
	EXPORT cValue AS STRING
	EXPORT cField1 AS STRING
	EXPORT cField2 AS STRING
	CONSTRUCTOR()
		SELF:cValue := ""
		SELF:cField1 := ""
		SELF:cField2 := ""
	RETURN
	METHOD ToString() AS STRING
		LOCAL cRet AS STRING
		SWITCH SELF:eType 
		CASE FillUsingType.UseArray
			cRet := SELF:cValue
		CASE FillUsingType.UseMethod
			cRet := "SELF:" + SELF:cValue + "()"
		CASE FillUsingType.UseServer
			cRet := SELF:cValue + "{} , #"
			cRet += SELF:cField1 + " , #"
			cRet += SELF:cField2
		END SWITCH
	RETURN cRet
	
END CLASS


CLASS FillUsingPickerDlg INHERIT Form

	PROTECT oOkButton AS Button
	PROTECT oAutoButton AS Button
	PROTECT oCancelButton AS Button
	PROTECT oGroupBox2 AS GroupBox
	PROTECT oField2Combo AS ComboBox
	PROTECT oLabel3 AS Label
	PROTECT oField1Combo AS ComboBox
	PROTECT oLabel2 AS Label
	PROTECT oServerCombo AS ComboBox
	PROTECT oLabel1 AS Label
	PROTECT oValueTextBox AS TextBox
	PROTECT oGroupBox1 AS GroupBox
	PROTECT oUseServerRadio AS RADIOBUTTON
	PROTECT oUseMethodRadio AS RADIOBUTTON
	PROTECT oUseArrayRadio AS RADIOBUTTON

// User code starts here (DO NOT remove this line)  ##USER##
	EXPORT oValue AS FillUsingClass

CONSTRUCTOR(cControl AS STRING , _oValue AS FillUsingClass)

	SUPER()

	SELF:InitializeForm()
	SELF:TopMost := TRUE
	
	SELF:Text := cControl + " Fill Using :"
	
	SELF:oValue := _oValue
	IF SELF:oValue == NULL
		SELF:oValue := FillUsingClass{}
	ENDIF
	
	SWITCH SELF:oValue:eType 
	CASE FillUsingType.UseArray
		SELF:oUseArrayRadio:Checked := TRUE
		SELF:oValueTextBox:Text := SELF:oValue:cValue
	CASE FillUsingType.UseMethod
		SELF:oUseMethodRadio:Checked := TRUE
		SELF:oValueTextBox:Text := SELF:oValue:cValue
	CASE FillUsingType.UseServer
		SELF:oServerCombo:Text := SELF:oValue:cValue
		SELF:oField1Combo:Text := SELF:oValue:cField1
		SELF:oField2Combo:Text := SELF:oValue:cField2
		SELF:oUseServerRadio:Checked := TRUE
	END SWITCH
	
	SELF:ShowControls()

RETURN

METHOD InitializeForm() AS VOID

// IDE generated code (please DO NOT modify)

	SELF:Name := "FillUsingPickerDlg"
	SELF:SuspendLayout()
	SELF:Location := Point{100,100}
	SELF:ClientSize := Size{344,230}
	SELF:AutoScaleDimensions := SizeF{ 96 , 96 }
	SELF:AutoScaleMode := AutoScaleMode.Dpi
	SELF:FormBorderStyle := FormBorderStyle.Sizable
	SELF:MaximizeBox := FALSE
	SELF:MinimizeBox := FALSE
	SELF:ShowIcon := FALSE
	SELF:ShowInTaskbar := FALSE
	SELF:StartPosition := FormStartPosition.CenterParent
	SELF:Text := "Fill Using"

	SELF:oOkButton := Button{}
	SELF:oOkButton:Name := "OKButton"
	SELF:oOkButton:Location := Point{ 101 , 204 }
	SELF:oOkButton:Size := Size{ 75 , 23 }
	SELF:oOkButton:Text := "&OK"
	SELF:oOkButton:Click += EventHandler{ SELF , @OKButtonClick() }
	SELF:oOkButton:TabIndex := 2
	SELF:oOkButton:Anchor := AnchorStyles.Bottom + AnchorStyles.Right
	SELF:Controls:Add(SELF:oOkButton)
	
	SELF:oAutoButton := Button{}
	SELF:oAutoButton:Name := "AutoButton"
	SELF:oAutoButton:Location := Point{ 182 , 204 }
	SELF:oAutoButton:Size := Size{ 75 , 23 }
	SELF:oAutoButton:Text := "&Auto"
	SELF:oAutoButton:Click += EventHandler{ SELF , @AutoButtonClick() }
	SELF:oAutoButton:TabIndex := 3
	SELF:oAutoButton:Anchor := AnchorStyles.Bottom + AnchorStyles.Right
	SELF:Controls:Add(SELF:oAutoButton)
	
	SELF:oCancelButton := Button{}
	SELF:oCancelButton:Name := "CancelButton"
	SELF:oCancelButton:Location := Point{ 264 , 204 }
	SELF:oCancelButton:Size := Size{ 75 , 23 }
	SELF:oCancelButton:Text := "&Cancel"
	SELF:oCancelButton:Click += EventHandler{ SELF , @CancelButtonClick() }
	SELF:oCancelButton:TabIndex := 4
	SELF:oCancelButton:Anchor := AnchorStyles.Bottom + AnchorStyles.Right
	SELF:Controls:Add(SELF:oCancelButton)
	
	SELF:oGroupBox2 := GroupBox{}
	SELF:oGroupBox2:Name := "GroupBox2"
	SELF:oGroupBox2:SuspendLayout()
	SELF:oGroupBox2:Location := Point{ 7 , 74 }
	SELF:oGroupBox2:Size := Size{ 331 , 124 }
	SELF:oGroupBox2:Text := "Options"
	SELF:oGroupBox2:TabIndex := 1
	SELF:oGroupBox2:Anchor := AnchorStyles.Top + AnchorStyles.Bottom + AnchorStyles.Left + AnchorStyles.Right
	SELF:Controls:Add(SELF:oGroupBox2)
	

	SELF:oField2Combo := ComboBox{}
	SELF:oField2Combo:Name := "Field2Combo"
	SELF:oField2Combo:Location := Point{ 96 , 83 }
	SELF:oField2Combo:Size := Size{ 220 , 21 }
	SELF:oField2Combo:DropDownStyle := ComboBoxStyle.DropDown
	SELF:oField2Combo:TabIndex := 5
	SELF:oField2Combo:Anchor := AnchorStyles.Top + AnchorStyles.Left + AnchorStyles.Right
	SELF:oGroupBox2:Controls:Add(SELF:oField2Combo)
	
	SELF:oLabel3 := Label{}
	SELF:oLabel3:Name := "Label3"
	SELF:oLabel3:Location := Point{ 14 , 85 }
	SELF:oLabel3:Size := Size{ 72 , 19 }
	SELF:oLabel3:Text := "Return field"
	SELF:oLabel3:TabIndex := 4
	SELF:oGroupBox2:Controls:Add(SELF:oLabel3)
	
	SELF:oField1Combo := ComboBox{}
	SELF:oField1Combo:Name := "Field1Combo"
	SELF:oField1Combo:Location := Point{ 96 , 53 }
	SELF:oField1Combo:Size := Size{ 220 , 21 }
	SELF:oField1Combo:DropDownStyle := ComboBoxStyle.DropDown
	SELF:oField1Combo:TabIndex := 3
	SELF:oField1Combo:Anchor := AnchorStyles.Top + AnchorStyles.Left + AnchorStyles.Right
	SELF:oGroupBox2:Controls:Add(SELF:oField1Combo)
	
	SELF:oLabel2 := Label{}
	SELF:oLabel2:Name := "Label2"
	SELF:oLabel2:Location := Point{ 14 , 55 }
	SELF:oLabel2:Size := Size{ 72 , 19 }
	SELF:oLabel2:Text := "Display field"
	SELF:oLabel2:TabIndex := 2
	SELF:oGroupBox2:Controls:Add(SELF:oLabel2)
	
	SELF:oServerCombo := ComboBox{}
	SELF:oServerCombo:Name := "ServerCombo"
	SELF:oServerCombo:Location := Point{ 96 , 23 }
	SELF:oServerCombo:Size := Size{ 220 , 21 }
	SELF:oServerCombo:DropDownStyle := ComboBoxStyle.DropDown
	SELF:oServerCombo:SelectedIndexChanged += EventHandler{ SELF , @ServerComboSelectedIndexChanged() }
	SELF:oServerCombo:TabIndex := 1
	SELF:oServerCombo:Anchor := AnchorStyles.Top + AnchorStyles.Left + AnchorStyles.Right
	SELF:oGroupBox2:Controls:Add(SELF:oServerCombo)
	
	SELF:oLabel1 := Label{}
	SELF:oLabel1:Name := "Label1"
	SELF:oLabel1:Location := Point{ 14 , 25 }
	SELF:oLabel1:Size := Size{ 72 , 19 }
	SELF:oLabel1:Text := "Label"
	SELF:oLabel1:TabIndex := 0
	SELF:oGroupBox2:Controls:Add(SELF:oLabel1)
	
	SELF:oValueTextBox := TextBox{}
	SELF:oValueTextBox:Name := "ValueTextBox"
	SELF:oValueTextBox:Location := Point{ 96 , 23 }
	SELF:oValueTextBox:Size := Size{ 220 , 81 }
	SELF:oValueTextBox:Multiline := TRUE
	SELF:oValueTextBox:TabIndex := 6
	SELF:oValueTextBox:Anchor := AnchorStyles.Top + AnchorStyles.Bottom + AnchorStyles.Left + AnchorStyles.Right
	SELF:oGroupBox2:Controls:Add(SELF:oValueTextBox)
	
	SELF:oGroupBox1 := GroupBox{}
	SELF:oGroupBox1:Name := "GroupBox1"
	SELF:oGroupBox1:SuspendLayout()
	SELF:oGroupBox1:Location := Point{ 7 , 5 }
	SELF:oGroupBox1:Size := Size{ 331 , 63 }
	SELF:oGroupBox1:Text := "DataSource"
	SELF:oGroupBox1:TabIndex := 0
	SELF:oGroupBox1:Anchor := AnchorStyles.Top + AnchorStyles.Left + AnchorStyles.Right
	SELF:Controls:Add(SELF:oGroupBox1)
	

	SELF:oUseServerRadio := RADIOBUTTON{}
	SELF:oUseServerRadio:Name := "UseServerRadio"
	SELF:oUseServerRadio:Location := Point{ 119 , 26 }
	SELF:oUseServerRadio:Size := Size{ 89 , 24 }
	SELF:oUseServerRadio:Text := "Use Server"
	SELF:oUseServerRadio:Click += EventHandler{ SELF , @UseRadioClick() }
	SELF:oUseServerRadio:TabIndex := 1
	SELF:oGroupBox1:Controls:Add(SELF:oUseServerRadio)
	
	SELF:oUseMethodRadio := RADIOBUTTON{}
	SELF:oUseMethodRadio:Name := "UseMethodRadio"
	SELF:oUseMethodRadio:Location := Point{ 225 , 26 }
	SELF:oUseMethodRadio:Size := Size{ 91 , 24 }
	SELF:oUseMethodRadio:Text := "Use Method"
	SELF:oUseMethodRadio:Click += EventHandler{ SELF , @UseRadioClick() }
	SELF:oUseMethodRadio:TabIndex := 2
	SELF:oGroupBox1:Controls:Add(SELF:oUseMethodRadio)
	
	SELF:oUseArrayRadio := RADIOBUTTON{}
	SELF:oUseArrayRadio:Name := "UseArrayRadio"
	SELF:oUseArrayRadio:Location := Point{ 17 , 26 }
	SELF:oUseArrayRadio:Size := Size{ 81 , 24 }
	SELF:oUseArrayRadio:Text := "Use Array"
	SELF:oUseArrayRadio:Click += EventHandler{ SELF , @UseRadioClick() }
	SELF:oUseArrayRadio:TabIndex := 0
	SELF:oGroupBox1:Controls:Add(SELF:oUseArrayRadio)
	
	SELF:oGroupBox1:ResumeLayout()
	SELF:oGroupBox2:ResumeLayout()
	SELF:ResumeLayout()

	SELF:AcceptButton := SELF:oOkButton

	SELF:CancelButton := SELF:oCancelButton

RETURN


METHOD ServerComboSelectedIndexChanged(o AS OBJECT , e AS EventArgs) AS VOID
RETURN

METHOD CancelButtonClick(o AS OBJECT , e AS EventArgs) AS VOID
	SELF:DialogResult := DialogResult.Cancel
RETURN

METHOD AutoButtonClick(o AS OBJECT , e AS EventArgs) AS VOID
	SELF:oValue := NULL
	SELF:DialogResult := DialogResult.OK
RETURN

METHOD UseRadioClick(o AS OBJECT , e AS EventArgs) AS VOID
	SELF:ShowControls()
RETURN

METHOD ShowControls() AS VOID
	IF SELF:oUseArrayRadio:Checked .or. SELF:oUseMethodRadio:Checked
		SELF:oLabel2:Visible := FALSE
		SELF:oLabel3:Visible := FALSE
		SELF:oValueTextBox:Visible := TRUE
		SELF:oServerCombo:Visible := FALSE
		SELF:oField1Combo:Visible := FALSE
		SELF:oField2Combo:Visible := FALSE
	ELSE
		SELF:oLabel2:Visible := TRUE
		SELF:oLabel3:Visible := TRUE
		SELF:oValueTextBox:Visible := FALSE
		SELF:oServerCombo:Visible := TRUE
		SELF:oField1Combo:Visible := TRUE
		SELF:oField2Combo:Visible := TRUE
	ENDIF
	
	DO CASE
	CASE SELF:oUseArrayRadio:Checked
		SELF:oLabel1:Text := "Array :"
		SELF:oValueTextBox:Size := Size{SELF:oValueTextBox:Width , 82}
	CASE SELF:oUseMethodRadio:Checked
		SELF:oLabel1:Text := "Name :"
		SELF:oValueTextBox:Size := Size{SELF:oValueTextBox:Width , 20}
	CASE SELF:oUseServerRadio:Checked
		SELF:oLabel1:Text := "Server :"
	END CASE
	
RETURN

METHOD OKButtonClick(o AS OBJECT , e AS EventArgs) AS VOID
	DO CASE
	CASE SELF:oUseArrayRadio:Checked
		IF SELF:oValueTextBox:Text:Trim() == ""
			SELF:oValueTextBox:Focus()
			RETURN
		ENDIF
	CASE SELF:oUseMethodRadio:Checked
		IF SELF:oValueTextBox:Text:Trim() == ""
			SELF:oValueTextBox:Focus()
			RETURN
		ENDIF
	CASE SELF:oUseServerRadio:Checked
//		IF SELF:oServerCombo:SelectedIndex == - 1 .or. SELF:oField1Combo:SelectedIndex == - 1 .or. SELF:oField2Combo:SelectedIndex == - 1
		IF SELF:oServerCombo:Text:Trim() == "" .or. SELF:oField1Combo:Text:Trim() == "" .or. SELF:oField2Combo:Text:Trim() == ""
			SELF:oServerCombo:Focus()
			RETURN
		ENDIF
	END CASE

	DO CASE
	CASE SELF:oUseArrayRadio:Checked
		SELF:oValue:eType := FillUsingType.UseArray
		SELF:oValue:cValue := SELF:oValueTextBox:Text
	CASE SELF:oUseMethodRadio:Checked
		SELF:oValue:eType := FillUsingType.UseMethod
		SELF:oValue:cValue := SELF:oValueTextBox:Text
	CASE SELF:oUseServerRadio:Checked
		SELF:oValue:eType := FillUsingType.UseServer
		SELF:oValue:cValue := SELF:oServerCombo:Text
	END CASE
	SELF:oValue:cField1 := SELF:oField1Combo:Text
	SELF:oValue:cField2 := SELF:oField2Combo:Text
	
	SELF:DialogResult := DialogResult.OK
RETURN


END CLASS

