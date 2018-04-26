#using System.Windows.Forms
#using System.Drawing
#using System.Collections.Generic

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
		SELF:Text := SELF:oTemplate:cName
		SELF:Dock := DockStyle.Top
		SELF:TextAlign := ContentAlignment.MiddleLeft
		SELF:FlatStyle := FlatStyle.Flat
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
		LOCAL n AS INT

		SELF:AutoScroll := TRUE
		SELF:Dock := DockStyle.Fill
		
		IF !VOWindowEditorTemplate.Loaded
			RETURN
		ENDIF
		
		FOR n := VOWindowEditorTemplate.Count - 1 DOWNTO 0
			oTemplate := VOWindowEditorTemplate.Get(n)
			IF (!oTemplate:lUse .or. oTemplate:lForm) //.and. n != 0
				LOOP
			ENDIF
			oButton := ToolBoxButton{oTemplate}
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
