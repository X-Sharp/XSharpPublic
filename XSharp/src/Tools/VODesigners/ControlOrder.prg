#using System.Windows.Forms
#using System.Drawing
#using System.Collections

CLASS VOControlCreationOrderDlg INHERIT System.Windows.Forms.Form

	PROTECT oDownButton AS System.Windows.Forms.Button
	PROTECT oUpButton AS System.Windows.Forms.Button
	PROTECT oMouseButton AS System.Windows.Forms.Button
	PROTECT oOkButton AS System.Windows.Forms.Button
	PROTECT oCancelButton AS System.Windows.Forms.Button
	PROTECT oControlsList AS System.Windows.Forms.ListView

// User code starts here (DO NOT remove this line)  ##USER##
	PROTECT oEditor AS VOWindowEditor
	EXPORT aNewOrder AS ArrayList
	
	PROTECT oDragItem AS ListViewItem
	PROTECT oDragPoint AS Point

	PROTECT lUsingMouse AS LOGIC
	PROTECT oTimer AS Timer
	PROTECT nMouseIndex AS INT

	CONSTRUCTOR(_oEditor AS VOWindowEditor , aDesign AS ArrayList)
		
		SUPER()

		LOCAL oDesign AS DesignWindowItem
		LOCAL oItem AS ListViewItem
		LOCAL n AS INT
		
		SELF:InitializeForm()
		
		SELF:oEditor := _oEditor
		
		SELF:oControlsList:Columns:Add("Name" , 120)
		SELF:oControlsList:Columns:Add("Type" , 120)
		SELF:oControlsList:Columns:Add("Caption" , 120)
		
		SELF:oUpButton:Text := "Up"
		SELF:oDownButton:Text := "Dn"
		
		FOR n := 0 UPTO aDesign:Count - 1
			oDesign := (DesignWindowItem)aDesign[n]
			IF oDesign:Deleted == 1
				LOOP
			END IF
			oItem := ListViewItem{}
			oItem:Tag := oDesign
			oItem:Text := oDesign:Name
			oItem:SubItems:Add(oDesign:cControl)
			IF oDesign:GetProperty("Caption") != NULL
				oItem:SubItems:Add(oDesign:GetProperty("Caption"):TextValue)
			ENDIF
			SELF:oControlsList:Items:Add(oItem)
		NEXT
		
		IF SELF:oControlsList:Items:Count != 0
			SELF:oControlsList:Items[0]:Selected :=  TRUE
		ENDIF

		SELF:oTimer := Timer{}
		SELF:oTimer:Interval := 100
		SELF:oTimer:Tick += EventHandler{ SELF , @TimerTicked() }
		
	RETURN
	
	PROTECTED METHOD InitializeForm() AS VOID
	
//	 IDE generated code (please DO NOT modify)
	
		SELF:Name := "VOControlCreationOrderDlg"
		SELF:SuspendLayout()
		SELF:Location := System.Drawing.Point{100,100}
		SELF:ClientSize := System.Drawing.Size{450,422}
		SELF:AutoScaleDimensions := System.Drawing.SizeF{ 96 , 96 }
		SELF:AutoScaleMode := System.Windows.Forms.AutoScaleMode.Dpi
		SELF:FormBorderStyle := System.Windows.Forms.FormBorderStyle.Sizable
		SELF:MaximizeBox := FALSE
		SELF:MinimizeBox := FALSE
		SELF:ShowIcon := FALSE
		SELF:ShowInTaskbar := FALSE
		SELF:StartPosition := System.Windows.Forms.FormStartPosition.CenterParent
		SELF:Text := "Control Creation Order"
	
		SELF:oDownButton := System.Windows.Forms.Button{}
		SELF:oDownButton:Name := "DownButton"
		SELF:oDownButton:Location := System.Drawing.Point{ 420 , 359 }
		SELF:oDownButton:Size := System.Drawing.Size{ 24 , 23 }
		SELF:oDownButton:Click += System.EventHandler{ SELF , @DownButtonClick() }
		SELF:oDownButton:TabIndex := 2
		SELF:oDownButton:Anchor := System.Windows.Forms.AnchorStyles.Bottom + System.Windows.Forms.AnchorStyles.Right
		SELF:Controls:Add(SELF:oDownButton)
		
		SELF:oUpButton := System.Windows.Forms.Button{}
		SELF:oUpButton:Name := "UpButton"
		SELF:oUpButton:Location := System.Drawing.Point{ 420 , 29 }
		SELF:oUpButton:Size := System.Drawing.Size{ 24 , 23 }
		SELF:oUpButton:Click += System.EventHandler{ SELF , @UpButtonClick() }
		SELF:oUpButton:TabIndex := 1
		SELF:oUpButton:Anchor := System.Windows.Forms.AnchorStyles.Top + System.Windows.Forms.AnchorStyles.Right
		SELF:Controls:Add(SELF:oUpButton)
		
		SELF:oMouseButton := System.Windows.Forms.Button{}
		SELF:oMouseButton:Name := "MouseButton"
		SELF:oMouseButton:Location := System.Drawing.Point{ 172 , 390 }
		SELF:oMouseButton:Size := System.Drawing.Size{ 75 , 23 }
		SELF:oMouseButton:Text := "Use Mouse"
		SELF:oMouseButton:TabIndex := 3
		SELF:oMouseButton:Anchor := System.Windows.Forms.AnchorStyles.Bottom + System.Windows.Forms.AnchorStyles.Right
		SELF:oMouseButton:Click += System.EventHandler{ SELF , @UseMouseButtonClick() }
		SELF:Controls:Add(SELF:oMouseButton)
		
		SELF:oOkButton := System.Windows.Forms.Button{}
		SELF:oOkButton:Name := "OKButton"
		SELF:oOkButton:Location := System.Drawing.Point{ 255 , 390 }
		SELF:oOkButton:Size := System.Drawing.Size{ 75 , 23 }
		SELF:oOkButton:Text := "&OK"
		SELF:oOkButton:Click += System.EventHandler{ SELF , @OKButtonClick() }
		SELF:oOkButton:TabIndex := 4
		SELF:oOkButton:Anchor := System.Windows.Forms.AnchorStyles.Bottom + System.Windows.Forms.AnchorStyles.Right
		SELF:Controls:Add(SELF:oOkButton)
		
		SELF:oCancelButton := System.Windows.Forms.Button{}
		SELF:oCancelButton:Name := "CancelButton"
		SELF:oCancelButton:Location := System.Drawing.Point{ 338 , 390 }
		SELF:oCancelButton:Size := System.Drawing.Size{ 75 , 23 }
		SELF:oCancelButton:Text := "&Cancel"
		SELF:oCancelButton:TabIndex := 5
		SELF:oCancelButton:Anchor := System.Windows.Forms.AnchorStyles.Bottom + System.Windows.Forms.AnchorStyles.Right
		SELF:Controls:Add(SELF:oCancelButton)
		
		SELF:oControlsList := System.Windows.Forms.ListView{}
		SELF:oControlsList:Name := "ControlsList"
		SELF:oControlsList:Location := System.Drawing.Point{ 12 , 9 }
		SELF:oControlsList:Size := System.Drawing.Size{ 401 , 373 }
		SELF:oControlsList:FullRowSelect := TRUE
		SELF:oControlsList:HideSelection := FALSE
		SELF:oControlsList:MultiSelect := FALSE
		SELF:oControlsList:View := System.Windows.Forms.View.Details
		SELF:oControlsList:SelectedIndexChanged += System.EventHandler{ SELF , @ControlsListSelectedIndexChanged() }
		SELF:oControlsList:TabIndex := 0
		SELF:oControlsList:Anchor := System.Windows.Forms.AnchorStyles.Top + System.Windows.Forms.AnchorStyles.Bottom + System.Windows.Forms.AnchorStyles.Left + System.Windows.Forms.AnchorStyles.Right
		SELF:oControlsList:MouseDown += System.Windows.Forms.MouseEventHandler{ SELF , @ControlsListMouseDown() }
		SELF:oControlsList:MouseMove += System.Windows.Forms.MouseEventHandler{ SELF , @ControlsListMouseMove() }
		SELF:oControlsList:MouseUp += System.Windows.Forms.MouseEventHandler{ SELF , @ControlsListMouseUp() }
		SELF:oControlsList:DragDrop += System.Windows.Forms.DragEventHandler{ SELF , @ControlsListDragDrop() }
		SELF:oControlsList:DragOver += System.Windows.Forms.DragEventHandler{ SELF , @ControlsListDragOver() }
		SELF:Controls:Add(SELF:oControlsList)
		
		SELF:ResumeLayout()
	
		SELF:AcceptButton := SELF:oOkButton
	
		SELF:CancelButton := SELF:oCancelButton
	
	RETURN

	METHOD ControlsListMouseDown(sender AS System.Object , e AS System.Windows.Forms.MouseEventArgs) AS System.Void
		SELF:oDragItem := SELF:oControlsList:GetItemAt(e:X , e:Y)
		IF SELF:oDragItem != NULL
			SELF:oDragPoint := e:Location
		END IF
	RETURN
	METHOD ControlsListMouseUp(sender AS System.Object , e AS System.Windows.Forms.MouseEventArgs) AS System.Void
		SELF:oDragItem := NULL
	RETURN
	METHOD ControlsListMouseMove(sender AS System.Object , e AS System.Windows.Forms.MouseEventArgs) AS System.Void
		IF SELF:oDragItem != NULL
			IF Math.Abs(SELF:oDragPoint:X - e:X) > 2 .or. Math.Abs(SELF:oDragPoint:Y - e:Y) > 2
				SELF:oControlsList:SelectedIndices:Add(SELF:oDragItem:Index)
				SELF:oControlsList:AllowDrop := TRUE
				SELF:oControlsList:DoDragDrop(SELF:oDragItem , DragDropEffects.Move)
				SELF:oControlsList:AllowDrop := FALSE
				SELF:oDragItem := NULL
			END IF
		END IF
	RETURN
	METHOD ControlsListDragOver(sender AS System.Object , e AS System.Windows.Forms.DragEventArgs) AS System.Void
		LOCAL oItem AS ListViewItem
		LOCAL oPoint AS Point
		
		IF .not. e:Data:GetDataPresent(TypeOf(ListViewItem)) .or. SELF:oDragItem == NULL
			e:Effect := DragDropEffects.None
			RETURN
		ENDIF
		
		oPoint := Point{e:X , e:Y}
		oPoint := SELF:oControlsList:PointToClient(oPoint)
		IF SELF:oControlsList:GetItemAt(oPoint:X , oPoint:Y) != NULL
			oItem := SELF:oControlsList:GetItemAt(oPoint:X , oPoint:Y)
			IF oItem != SELF:oDragItem
				e:Effect:= DragDropEffects.Move
				RETURN
			END IF
		END IF
		e:Effect := DragDropEffects.None
	RETURN
	METHOD ControlsListDragDrop(sender AS System.Object , e AS System.Windows.Forms.DragEventArgs) AS System.Void
		LOCAL oItem AS ListViewItem
		LOCAL oPoint AS Point
		LOCAL nIndex AS INT
		
		IF .not. e:Data:GetDataPresent(TypeOf(ListViewItem)) .or. SELF:oDragItem == NULL
			e:Effect := DragDropEffects.None
			RETURN
		ENDIF
		
		oPoint := Point{e:X , e:Y}
		oPoint := SELF:oControlsList:PointToClient(oPoint)
		IF SELF:oControlsList:GetItemAt(oPoint:X , oPoint:Y) != NULL
			oItem := SELF:oControlsList:GetItemAt(oPoint:X , oPoint:Y)
			IF oItem != SELF:oDragItem
				nIndex := SELF:oDragItem:Index
				IF nIndex > oItem:Index
					SELF:oControlsList:Items:Remove(SELF:oDragItem)
					SELF:oControlsList:Items:Insert(oItem:Index , SELF:oDragItem)
				ELSE
					SELF:oControlsList:Items:Remove(SELF:oDragItem)
					SELF:oControlsList:Items:Insert(oItem:Index + 1, SELF:oDragItem)
				END IF
				SELF:oControlsList:SelectedIndices:Add(SELF:oDragItem:Index)
				RETURN
			END IF
		END IF
	RETURN
	
	PROTECTED METHOD UseMouseButtonClick(o AS OBJECT , e AS EventArgs) AS VOID
		SELF:oEditor:SelectMainItem()
		SELF:lUsingMouse := TRUE
		SELF:nMouseIndex := 0
		SELF:oTimer:Start()
	RETURN
	METHOD TimerTicked(o AS OBJECT , e AS EventArgs) AS VOID
		IF SELF:lUsingMouse
			SELF:Capture := TRUE
			Cursor.Current := Cursors.Cross
		END IF
	RETURN
	PROTECTED METHOD OnMouseDown(e AS MouseEventArgs) AS VOID
		LOCAL oItem AS ListViewItem
		LOCAL oDesign AS DesignItem

		SUPER:OnMouseDown(e)

		IF SELF:lUsingMouse
			SELF:lUsingMouse := FALSE
			oDesign := SELF:oEditor:GetDesignerUnderPoint(SELF:PointToScreen(e:Location))
			IF oDesign == NULL
				SELF:oTimer:Stop()
			ELSE
				SELF:lUsingMouse := TRUE
				IF oDesign:lSelected
					RETURN
				END IF
				IF SELF:nMouseIndex == 0
					SELF:oEditor:DoAction(DesignerActionType.Select , oDesign:cGuid)
				ELSE
					SELF:oEditor:DoAction(DesignerActionType.SelectAdd , oDesign:cGuid)
				END IF
				oItem := SELF:GetItem(oDesign)
				IF oItem != NULL
					IF oItem:Index != SELF:nMouseIndex
						IF SELF:nMouseIndex < SELF:oControlsList:Items:Count
							SELF:oControlsList:Items:Remove(oItem)
							SELF:oControlsList:Items:Insert(SELF:nMouseIndex , oItem)
							SELF:nMouseIndex ++
						ELSE
							SELF:nMouseIndex ++
							RETURN
						END IF
					ELSE
						SELF:nMouseIndex ++
					END IF
					IF SELF:nMouseIndex == SELF:oControlsList:Items:Count
						SELF:lUsingMouse := FALSE
					END IF
					SELF:oControlsList:SelectedIndices:Add(oItem:Index)
				END IF
			END IF
		END IF
	RETURN
	PROTECTED METHOD GetItem(oDesign AS DesignItem) AS ListViewItem
		FOREACH oItem AS ListViewItem in SELF:oControlsList:Items
			IF oItem:Tag == oDesign
				RETURN oItem
			END IF
		NEXT
	RETURN NULL
	
	PROTECTED METHOD ControlsListSelectedIndexChanged(o AS OBJECT , e AS System.EventArgs) AS VOID
		LOCAL oDesign AS DesignItem
		LOCAL nIndex AS INT
		
		IF SELF:oControlsList:SelectedIndices:Count == 0
			SELF:oUpButton:Enabled := FALSE
			SELF:oDownButton:Enabled := FALSE
			RETURN
		ENDIF
		nIndex := SELF:oControlsList:SelectedIndices[0]
		SELF:oUpButton:Enabled := nIndex != 0
		SELF:oDownButton:Enabled := nIndex != SELF:oControlsList:Items:Count - 1
		oDesign := (DesignWindowItem)SELF:oControlsList:Items[nIndex]:Tag
		SELF:oEditor:DoAction(DesignerActionType.Select , oDesign:cGuid)
	RETURN
	
	PROTECTED METHOD DownButtonClick(o AS OBJECT , e AS System.EventArgs) AS VOID
		SELF:UpDown(FALSE)
	RETURN
	PROTECTED METHOD UpButtonClick(o AS OBJECT , e AS System.EventArgs) AS VOID
		SELF:UpDown(TRUE)
	RETURN
	
	PROTECTED METHOD UpDown(lUp AS LOGIC) AS VOID
		LOCAL oItem AS ListViewItem
		LOCAL nIndex AS INT
		
		IF SELF:oControlsList:SelectedIndices:Count == 0
			RETURN
		ENDIF
		nIndex := SELF:oControlsList:SelectedIndices[0]
		IF lUp .and. nIndex == 0
			RETURN
		ENDIF
		IF !lUp .and. nIndex == SELF:oControlsList:Items:Count - 1
			RETURN
		ENDIF
		oItem := SELF:oControlsList:Items[nIndex]
		SELF:oControlsList:Items:RemoveAt(nIndex)
		IF lUp
			nIndex --
		ELSE
			nIndex ++
		ENDIF
		SELF:oControlsList:Items:Insert(nIndex , oItem)
	RETURN
	
	PROTECTED METHOD OKButtonClick(o AS OBJECT , e AS System.EventArgs) AS VOID
		SELF:aNewOrder := ArrayList{SELF:oControlsList:Items:Count}
		FOREACH oDesign AS DesignWindowItem IN SELF:oControlsList:Items
			SELF:aNewOrder:Add(oDesign)
		NEXT
		SELF:DialogResult := DialogResult.OK
	RETURN

END CLASS

