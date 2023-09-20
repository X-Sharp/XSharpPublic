//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Windows.Forms
USING System.Drawing

internal _dll func DeleteObject (hObject as IntPtr) as logic pascal:GDI32.DeleteObject
internal _dll function SetROP2(hDC as IntPtr, fnDrawMode as Int32) as Int32 pascal:GDI32.SetROP2
internal _dll function SetBkMode(hDC as IntPtr, iBkMOde as int) as Int32 pascal:GDI32.SetBkMode
//INTERNAL _Dll Function SetBkMode(hDC As Ptr, iBkMOde As Int32) As Int32 Pascal:GDI32.SetBkMode
internal _dll function SetTextColor(hDC as IntPtr, crColor as dword) as dword pascal:GDI32.SetTextColor
internal _dll function SetBkColor(hDC as IntPtr, clrref as dword) as dword pascal:GDI32.SetBkColor
internal _dll function TextOut(hDC as IntPtr,x as Int32,y as Int32,lpString as string,nLen as Int32) as logic pascal:GDI32.TextOutW
internal _dll function DrawText(hDC as IntPtr, lpString as string, nCount as int, lpRect ref _winRECT,;
    uFormat as dword) as int pascal:USER32.DrawTextW
[System.Runtime.InteropServices.StructLayout(System.Runtime.InteropServices.LayoutKind.Sequential)];
internal struct _winRECT
	export Left as long
	export Top as long
	export Right as long
	export Bottom as long
end structure

internal define DT_TOP              := 0x00000000
internal define DT_LEFT             := 0x00000000
internal define DT_CENTER           := 0x00000001
internal define DT_RIGHT            := 0x00000002
internal define DT_VCENTER          := 0x00000004
internal define DT_BOTTOM           := 0x00000008
internal define DT_WORDBREAK        := 0x00000010
internal define DT_SINGLELINE       := 0x00000020
internal define DT_CALCRECT         := 0x00000400

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
        switch cUpper
        case "FLAT"
            IF (INT)oProp:Value == 0
                SELF:FlatStyle := FlatStyle.Flat
            ELSE
                SELF:FlatStyle := FlatStyle.Standard
            ENDIF
        case "VERTICAL ALIGNMENT"
        case "HORIZONTAL ALIGNMENT"
        case "EXALIGNMENT"
        case "MULTILINE"
            self:Invalidate()
        otherwise
            if IsVisibleStyle(oProp:Name)
                self:UpdateStyles()
            endif
        end switch
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
                if self:oSF:FormatFlags:HasFlag(StringFormatFlags.NoWrap)
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
        CASE "CENTER"
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
        CASE "CENTER"
        case "AUTO"
            SELF:oSF:LineAlignment := StringAlignment.Center
        CASE "BOTTOM"
            SELF:oSF:LineAlignment := StringAlignment.Far
        END SWITCH
        TRY
            IF SELF:oItem:GetProperty("ExAlignment"):TextValue:ToUpper() == "RIGHT"
                SELF:oSF:Alignment := StringAlignment.Far
            END IF
        CATCH
            NOP
        END TRY
        TRY
            e:Graphics:DrawString(Funcs.TranslateCaption(SELF:oItem:GetProperty("Caption"):TextValue , FALSE) , SELF:Font , SELF:oBrush , SELF:ClientRectangle , SELF:oSF)
        CATCH
            NOP
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
            CATCH
                NOP
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
        cUpper := oProp:Name:ToUpper()
        switch cUpper
        case "VERTICAL ALIGNMENT"
        case "HORIZONTAL ALIGNMENT"
        case "EXALIGNMENT"
        case "TEXT LEFT"
        case "MULTILINE"
            try
                SELF:TextAlign := GetButtonAlignment(SELF:oItem , TRUE)
                SELF:CheckAlign := GetButtonAlignment(SELF:oItem , FALSE)
                SELF:Invalidate()
            CATCH
                NOP
            END TRY
        case "PUSH LIKE"
            SELF:Appearance := iif(oProp:ValueLogic , Appearance.Button , Appearance.Normal)
        otherwise
            if IsVisibleStyle(oProp:Name)
                self:UpdateStyles()
            endif
        end switch
        RETURN
    PROTECTED ACCESS CreateParams() AS CreateParams
        LOCAL oParams AS CreateParams
        oParams := SUPER:CreateParams
        Funcs.SetCreateParams(oParams , SELF:oItem)
        RETURN oParams
	override protected method OnPaint(e as PaintEventArgs) as void
		super:OnPaint(e)

		if self:lTestMode
//			SELF:Text := ""
			return
		end if

		if self:oBrush == null .or. self:oBrush:Color != self:ForeColor
			self:oBrush := SolidBrush{self:ForeColor}
		end if

		DesignCheckBox.DoPaint(self, oItem, oSF, oBrush, e:Graphics)
	return

	static method DoPaint(oControl as Control, oItem as DesignWindowItem, oSF as StringFormat, oBrush as SolidBrush, oGraphics as Graphics) as void
		local dwFlags as dword
		local lMultiline := false as logic

//		SELF:oSF:Trimming := StringTrimming.Character

		try
            if oItem:GetProperty("Multiline"):ValueLogic
                lMultiline := true
				dwFlags := DT_WORDBREAK

				if oSF:FormatFlags:HasFlag(StringFormatFlags.NoWrap)
					oSF:FormatFlags := (StringFormatFlags)_xor(oSF:FormatFlags , StringFormatFlags.NoWrap)
				end if
			else
				dwFlags := DT_SINGLELINE + DT_VCENTER

				oSF:FormatFlags := StringFormatFlags.NoWrap
			end if
		catch
			oSF:FormatFlags := StringFormatFlags.NoWrap
		end try

		local cValue as string
		try
			cValue := oItem:GetProperty("Horizontal Alignment"):TextValue:ToUpper()
		catch
			cValue := "AUTO"
		end try
		switch cValue
        case "LEFT"
        case "AUTO"
			oSF:Alignment := StringAlignment.Near
		case "CENTER"
			oSF:Alignment := StringAlignment.Center
			dwFlags += DT_CENTER
		case "RIGHT"
			oSF:Alignment := StringAlignment.Far
			dwFlags += DT_RIGHT
		end switch

		try
			cValue := oItem:GetProperty("Vertical Alignment"):TextValue:ToUpper()
		catch e3 as Exception
			cValue := "AUTO"
		end try
		switch cValue
        case "TOP"
            if .not. lMultiline
                dwFlags += DT_BOTTOM // for some bizarre reason...
            endif
			oSF:LineAlignment := StringAlignment.Near
        case "CENTER"
        case "AUTO"
			oSF:LineAlignment := StringAlignment.Center
		case "BOTTOM"
            if .not. lMultiline
                dwFlags += DT_VCENTER // see above...
            endif
			oSF:LineAlignment := StringAlignment.Far
		end switch
		try
			if oItem:GetProperty("ExAlignment"):TextValue:ToUpper() == "RIGHT"
				oSF:Alignment := StringAlignment.Far
            end if
        catch
            nop
		end try

		local lExLeft, lTextLeft, lCheckRight as logic
		try
			lExLeft := oItem:GetProperty("ExAlignment"):TextValue:ToUpper() == "RIGHT"
			lTextLeft := oItem:GetProperty("Text Left"):ValueLogic
        catch
            nop
		end try
		lCheckRight := lExLeft .or. lTextLeft

		local oRect as Rectangle
		local const nCheckSize := 16 as int
		oRect := oControl:ClientRectangle
		if lCheckRight
			oRect := Rectangle{0,0,oRect:Width - nCheckSize, oRect:Height}
		else
			oRect := Rectangle{nCheckSize,0,oRect:Width - nCheckSize, oRect:Height}
		end if

		try
			local hDC as IntPtr
			local hFont as IntPtr
			hDC := oGraphics:GetHdc()
			SetBkMode(hDC,1 /*bkMode.Transparent*/)
			hFont := oControl:Font:ToHfont()
			SelectObject(hDC, hFont )
			local cText as string
			cText := Funcs.TranslateCaption(oItem:GetProperty("Caption"):TextValue , false)
			local r as _winRECT
			r:Left := oRect:Left
			r:Top := oRect:Top
			r:Right := oRect:Right
			r:Bottom := oRect:Bottom

//			SetTextColor(hDC, (DWORD)oControl:ForeColor:ToArgb()  << 8 )
			SetTextColor(hDC, ((dword)oControl:ForeColor:B  << 16) + ((dword)oControl:ForeColor:G  << 8) + ((dword)oControl:ForeColor:R))

   			if lMultiline
				// becuase DT_VCENTER and DT_BOTTOM do not work with multiline text, we need to calculate the height of the text and print it manually in the required position..
				local testRect as _winRECT
				testRect:Left := oRect:Left
				testRect:Top := oRect:Top
				testRect:Right := oRect:Right
				testRect:Bottom := oRect:Bottom

				DrawText(hDC, cText , cText:Length , ref testRect,  dwFlags + DT_CALCRECT)
				switch oSF:LineAlignment
				case StringAlignment.Near
					r:Top := 0
				case StringAlignment.Center
					r:Top := r:Bottom / 2 - testRect:Bottom / 2
				case StringAlignment.Far
					r:Top := r:Bottom - testRect:Bottom
				end switch
			end if

			DrawText(hDC, cText , cText:Length , ref r, dwFlags)
//			TextOut(hDC, 0,0, cText , cText:Length)
//			e:Graphics:DrawString(cText , oControl:Font , oBrush , oRect , oSF)

			oGraphics:ReleaseHdc(hDC)
			DeleteObject(hFont)
        catch
            nop
		end try

	return

END CLASS

INTERNAL CLASS DesignRadioButton INHERIT RadioButton
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
        cUpper := oProp:Name:ToUpper()
        DO CASE
        CASE cUpper == "VERTICAL ALIGNMENT" .OR. cUpper == "HORIZONTAL ALIGNMENT" .OR. cUpper == "EXALIGNMENT" .OR. cUpper == "TEXT LEFT" .OR. cUpper == "MULTILINE"
            TRY
                SELF:TextAlign := GetButtonAlignment(SELF:oItem , TRUE)
                SELF:CheckAlign := GetButtonAlignment(SELF:oItem , FALSE)
                SELF:Invalidate()
            CATCH
                NOP
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
    VIRTUAL PROTECTED METHOD OnPaint(e AS PaintEventArgs) AS VOID
        SUPER:OnPaint(e)

   		if self:lTestMode
//			SELF:Text := ""
			return
		end if

        IF SELF:oBrush == NULL .OR. SELF:oBrush:Color != SELF:ForeColor
            SELF:oBrush := SolidBrush{SELF:ForeColor}
        END IF

		DesignCheckBox.DoPaint(self, oItem, oSF, oBrush, e:Graphics)
        return
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
        SELF:AddOrInsertPage(TRUE)
    END METHOD
    METHOD AddOrInsertPage(lAdd AS LOGIC) AS VOID
        LOCAL oPage AS DesignTabPage
        oPage := DesignTabPage{SELF:oItem}
        oPage:Text := "Page" + (SELF:TabPages:Count + 1):ToString()
        oPage:Name := SELF:oItem:Name + "_PAGE" + (SELF:TabPages:Count + 1):ToString()
        oPage:Tag := FALSE
        IF lAdd
            SELF:TabPages:Add(oPage)
        ELSE
            LOCAL nIndex AS INT
            nIndex := SELF:SelectedIndex
            IF nIndex = -1
                nIndex := 0
            END IF
            SELF:TabPages:Insert(nIndex,oPage)
        END IF
        RETURN
    METHOD MovePage(lLeft AS LOGIC) AS VOID
        LOCAL oPage AS DesignTabPage
        oPage := (DesignTabPage)SELF:SelectedTab
        IF oPage == NULL
            RETURN
        END IF
        LOCAL nIndex AS INT
        nIndex := SELF:SelectedIndex
        IF lLeft
            nIndex --
            IF nIndex >= 0
                SELF:TabPages:Remove(oPage)
                SELF:TabPages:Insert(nIndex, oPage)
                SELF:SelectedTab := oPage
            END IF
        ELSE
            nIndex ++
            IF nIndex < SELF:TabPages:Count
                SELF:TabPages:Remove(oPage)
                SELF:TabPages:Insert(nIndex, oPage)
                SELF:SelectedTab := oPage
            END IF
        END IF
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
    CATCH
        NOP
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
    CASE "BORDER"
    CASE "CLIENT EDGE"
    CASE "STATIC EDGE"
    case "MODAL FRAME"
        return true
    OTHERWISE
        return false
    end switch


