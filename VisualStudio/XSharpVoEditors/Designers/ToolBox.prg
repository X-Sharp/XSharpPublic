//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Windows.Forms
USING System.Drawing
USING System.Collections.Generic
USING System.Reflection
USING System.IO



INTERNAL CLASS ToolBoxButton INHERIT Button
    PROTECT oTemplate AS VOControlTemplate
    PROTECT oDragPoint AS Point
    CONSTRUCTOR()
        SUPER()
        SELF:Text := "Pointer"
        SELF:Dock := DockStyle.Top
        SELF:TextAlign := ContentAlignment.MiddleLeft
        SELF:FlatStyle := FlatStyle.Flat
        RETURN
    CONSTRUCTOR(_oTemplate AS VOControlTemplate)
        SUPER()
        SELF:oTemplate := _oTemplate
        SELF:Text := SELF:oTemplate:ToolboxTitle
        SELF:Dock := DockStyle.Top
        SELF:TextAlign := ContentAlignment.MiddleLeft
        SELF:FlatStyle := FlatStyle.Flat
        SELF:ImageAlign := ContentAlignment.MiddleLeft
        SELF:TextImageRelation := TextImageRelation.ImageBeforeText
        RETURN
    ACCESS Template AS VOControlTemplate
        RETURN SELF:oTemplate
    PROTECTED METHOD OnMouseDown(e AS MouseEventArgs) AS VOID
        SUPER:OnMouseDown(e)
        SELF:oDragPoint := e:Location
        RETURN
    PROTECTED METHOD OnMouseUp(e AS MouseEventArgs) AS VOID
        SUPER:OnMouseUp(e)
        SELF:oDragPoint := Point.Empty
        RETURN
    PROTECTED METHOD OnMouseMove(e AS MouseEventArgs) AS VOID
        SUPER:OnMouseMove(e)
        IF SELF:oTemplate == NULL .or. SELF:oDragPoint == Point.Empty
            RETURN
        ENDIF
        IF Math.Abs(e:X - SELF:oDragPoint:X) > 2 .or. Math.Abs(e:Y - SELF:oDragPoint:Y) > 2
            SELF:DoDragDrop(SELF:oTemplate , DragDropEffects.Move)
            SELF:oDragPoint := Point.Empty
        ENDIF
        RETURN


END CLASS

CLASS ToolBox INHERIT Panel
    INTERNAL oCurrent AS ToolBoxButton
    PROTECT eCurrentMode AS ViewMode
    CONSTRUCTOR()

        SUPER()

        LOCAL oTemplate AS VOControlTemplate
        LOCAL oButton AS ToolBoxButton
        LOCAL oImageList AS ImageList
        LOCAL n AS INT

        SELF:AutoScroll := TRUE
        SELF:Dock := DockStyle.Fill

        IF !VOWindowEditorTemplate.Loaded
            RETURN
        ENDIF

        LOCAL oAssembly AS Assembly
        oAssembly := TypeOf(System.Windows.Forms.Form):Assembly
        oImageList := ImageList{}
        oImageList:TransparentColor := Color.Fuchsia

        FOR n := VOWindowEditorTemplate.Count - 1 DOWNTO 0
            oTemplate := VOWindowEditorTemplate.Get(n)
            IF (!oTemplate:lUse .or. oTemplate:lForm) //.and. n != 0
                LOOP
            ENDIF
            oButton := ToolBoxButton{oTemplate}
            oImageList:Images:Add((Bitmap)Image.FromStream(oAssembly:GetManifestResourceStream(SELF:ImageNameFromTemplate(oTemplate))))
            oButton:ImageList := oImageList
            oButton:ImageIndex := oImageList:Images:Count - 1
            oButton:Text := "   " + oButton:Text


            oButton:Click += EventHandler{ SELF , @ButtonClicked() }
            SELF:Controls:Add(oButton)
        NEXT

        LOCAL aStandard AS STRING[]
        aStandard := <STRING>{"FIXEDTEXT","PUSHBUTTON","CHECKBOX","RADIOBUTTON","SINGLELINEEDIT","MULTILINEEDIT","COMBOBOX","LISTBOX","LISTVIEW","TREEVIEW","GROUPBOX","RADIOBUTTONGROUP"}
        n := 0
        DO WHILE n < SELF:Controls:Count
            //		FOR n := 0 UPTO SELF:Controls:Count - 1
            LOCAL m AS INT
            oButton := (ToolBoxButton)SELF:Controls[n]
            IF oButton:Template != NULL
                oTemplate := oButton:Template
                FOR m := 1 UPTO aStandard:Length
                    IF aStandard[m] == oTemplate:cControl:ToUpper()
                        IF SELF:Controls:GetChildIndex(oButton) != SELF:Controls:Count - m
                            SELF:Controls:SetChildIndex(oButton , SELF:Controls:Count - m)
                            n --
                            EXIT
                        END IF
                    ENDIF
                NEXT
            ENDIF
            n ++
            //		NEXT
        ENDDO

        TRY
            oButton := ToolBoxButton{VOWindowEditorTemplate.Get("SINGLELINEEDIT")}
        CATCH
            oButton := ToolBoxButton{}
        END TRY
        oButton:Text := "DataColumn"
        oButton:Click += EventHandler{ SELF , @ButtonClicked() }
        SELF:Controls:Add(oButton)

        oButton := ToolBoxButton{}
        oButton:Click += EventHandler{ SELF , @ButtonClicked() }
        SELF:Controls:Add(oButton)

        SELF:SetViewMode(ViewMode.Form)
        SELF:SelectPointer()
        RETURN
    PROTECTED METHOD ImageNameFromTemplate(oTemplate AS VOControlTemplate) AS STRING
        LOCAL cRet AS STRING
        LOCAL cClass AS STRING
        cClass := oTemplate:cFullClass:ToUpperInvariant()
        DO CASE
        CASE cClass:StartsWith("CONTROL:CUSTOMCONTROL:BBROWSER")
            cRet := "System.Windows.Forms.DataGridView.bmp"
        CASE cClass:StartsWith("CONTROL:TEXTCONTROL:BASELISTBOX:COMBOBOX")
            cRet := "System.Windows.Forms.ComboBox.bmp"
        CASE cClass:StartsWith("CONTROL:TEXTCONTROL:BASELISTBOX:LISTBOX")
            cRet := "System.Windows.Forms.ListBox.bmp"
        CASE cClass:StartsWith("CONTROL:COMMONCONTROL:LISTVIEW")
            cRet := "System.Windows.Forms.ListView.bmp"
        CASE cClass:StartsWith("CONTROL:COMMONCONTROL:TREEVIEW")
            cRet := "System.Windows.Forms.TreeView.bmp"
        CASE cClass:StartsWith("CONTROL:COMMONCONTROL:TABCONTROL")
            cRet := "System.Windows.Forms.TabControl.bmp"
        CASE cClass:StartsWith("CONTROL:TEXTCONTROL:MONTHCALENDAR")
            cRet := "System.Windows.Forms.MonthCalendar.bmp"
        CASE cClass:StartsWith("CONTROL:TEXTCONTROL:DATETIMEPICKER")
            cRet := "System.Windows.Forms.DateTimePicker.bmp"
        CASE cClass:StartsWith("CONTROL:COMMONCONTROL:PROGRESSBAR")
            cRet := "System.Windows.Forms.ProgressBar.bmp"
        CASE cClass:StartsWith("CONTROL:TEXTCONTROL:GROUPBOX") .OR. ;
                cClass:StartsWith("CONTROL:TEXTCONTROL:RADIOBUTTONGROUP")
            cRet := "System.Windows.Forms.GroupBox.bmp"
        CASE cClass:StartsWith("CONTROL:TEXTCONTROL:FIXEDTEXT")
            cRet := "System.Windows.Forms.Label.bmp"
        CASE cClass:StartsWith("CONTROL:TEXTCONTROL:BUTTON:RADIOBUTTON")
            cRet := "System.Windows.Forms.RadioButton.bmp"
        CASE cClass:StartsWith("CONTROL:TEXTCONTROL:BUTTON:PUSHBUTTON")
            cRet := "System.Windows.Forms.Button.bmp"
        CASE cClass:StartsWith("CONTROL:COMMONCONTROL:ANIMATIONCONTROL")
            cRet := "System.Windows.Forms.NotifyIcon.bmp"
        CASE cClass:StartsWith("CONTROL:TEXTCONTROL:BUTTON:CHECKBOX")
            cRet := "System.Windows.Forms.CheckBox.bmp"
        CASE cClass:StartsWith("CONTROL:SCROLLBAR:VERTICALSCROLLBAR")
            cRet := "System.Windows.Forms.VScrollBar.bmp"
        CASE cClass:StartsWith("CONTROL:SCROLLBAR:HORIZONTALSCROLLBAR")
            cRet := "System.Windows.Forms.HScrollBar.bmp"
        CASE cClass:StartsWith("CONTROL:FIXEDBITMAP") .or. ;
                cClass:StartsWith("CONTROL:FIXEDICON")
            cRet := "System.Windows.Forms.PictureBox.bmp"
        CASE cClass:StartsWith("CONTROL:TEXTCONTROL:EDIT:MULTILINEEDIT:RICHEDIT")
            cRet := "System.Windows.Forms.RichTextBox.bmp"
        CASE cClass:StartsWith("CONTROL:SCROLLBAR:SPINNER:HORIZONTALSPINNER") .or. ;
                cClass:StartsWith("CONTROL:SCROLLBAR:SPINNER:VERTICALSPINNER")
            cRet := "System.Windows.Forms.NumericUpDown.bmp"
        CASE cClass:StartsWith("CONTROL:SCROLLBAR:SLIDER:VERTICALSLIDER") .or. ;
                cClass:StartsWith("CONTROL:SCROLLBAR:SLIDER:HORIZONTALSLIDER")
            cRet := "System.Windows.Forms.TrackBar.bmp"
        CASE cClass:StartsWith("CONTROL:TEXTCONTROL:IPADDRESS") .or. ;
                cClass:StartsWith("CONTROL:TEXTCONTROL:HOTKEYEDIT") .or. ;
                cClass:StartsWith("CONTROL:TEXTCONTROL:EDIT:MULTILINEEDIT") .or. ;
                cClass:StartsWith("CONTROL:TEXTCONTROL:EDIT:SINGLELINEEDIT") .or. ;
                cClass:StartsWith("CONTROL:TEXTCONTROL:IPADDRESS")
            cRet := "System.Windows.Forms.TextBox.bmp"
        OTHERWISE
            cRet := "System.Windows.Forms.Panel.bmp"
        END CASE
        RETURN cRet

    METHOD SetViewMode(eViewMode AS ViewMode) AS VOID
        LOCAL n AS INT
        IF eViewMode == ViewMode.Auto
            eViewMode := ViewMode.Form
        END IF
        IF SELF:eCurrentMode != eViewMode
            SELF:eCurrentMode := eViewMode
            FOR n := 0 UPTO SELF:Controls:Count - 2
                SELF:Controls[n]:Visible := SELF:eCurrentMode == ViewMode.Form
            NEXT
            SELF:Controls[SELF:Controls:Count - 2]:Visible := SELF:eCurrentMode == ViewMode.Browse
        END IF
        RETURN

    ACCESS Selected() AS VOControlTemplate
        IF SELF:oCurrent == NULL
            RETURN NULL
        ENDIF
        RETURN SELF:oCurrent:Template
    ACCESS PointerSelected() AS LOGIC
        RETURN SELF:oCurrent == NULL .or. SELF:oCurrent:Template == NULL
    METHOD SelectPointer() AS VOID
        LOCAL oPointer AS ToolBoxButton
        IF SELF:Controls:Count == 0
            RETURN
        ENDIF
        oPointer := (ToolBoxButton)SELF:Controls[SELF:Controls:Count - 1]
        SELF:SelectButton(oPointer)
        SELF:ScrollControlIntoView(oPointer)
        oPointer:Select()
        RETURN
    METHOD ButtonClicked(o AS OBJECT , e AS EventArgs) AS VOID
        SELF:SelectButton((ToolBoxButton)o)
        RETURN
    INTERNAL METHOD SelectButton(oButton AS ToolBoxButton) AS VOID
        IF SELF:oCurrent != NULL
            SELF:oCurrent:BackColor := Color.FromKnownColor(KnownColor.Control)
            SELF:oCurrent:ForeColor := Color.Black
        ENDIF
        oButton:BackColor := Color.White
        oButton:ForeColor := Color.Red
        SELF:oCurrent := oButton
        RETURN

END CLASS
