// Panel.prg

#USING System.Windows.Forms
#USING System.ComponentModel
#using System.Diagnostics
#using System.Drawing
#using System.Collections.Generic


[DebuggerDisplay("{Text}, {Size}")];
CLASS VOPanel INHERIT System.Windows.Forms.Panel 
	PROPERTY Window AS XSharp.VO.Window AUTO
	PROPERTY SuppressMovingControls AS LOGIC AUTO
	PROPERTY ReturnAllKeys	AS LOGIC AUTO
	PROTECT oToolTip AS VOToolTip
	PROTECT lToolTipsEnabled AS LOGIC
	PROTECT oLastControl AS OBJECT
	PROTECT _lNoUpdateUIState:=FALSE AS LOGIC
	PROTECT _aGroups AS System.Collections.Generic.SortedList<INT,VOGroupBox>
	PROTECT _aRBGroups AS System.Collections.Generic.SortedList<INT,VOGroupBox>
	PROTECT _aFramepanels AS System.Collections.Generic.SortedList<INT,VOFramepanel>


	METHOD CleanUp() AS VOID STRICT
		// Tooltip wurde im original Dispose nicht sauber entfernt und führte zu Abstürzen
		IF SELF:oToolTip != NULL_OBJECT
			SELF:oToolTip:Stop()
			SELF:oToolTip  := NULL_OBJECT
		ENDIF
		oLastControl := NULL_OBJECT
	
	CONSTRUCTOR(oWindow AS Window) STRICT
		SUPER()
		SELF:Window := oWindow
		lToolTipsEnabled := TRUE
		SELF:Initialize()
		RegisterEventHandlers()
		SELF:AllowDrop := TRUE
		SELF:DragEnter += Panel_DragEnter
		SELF:DragDrop += Panel_DragDrop

	CONSTRUCTOR(Owner AS XSharp.VO.Control, dwStyle AS LONG, dwExStyle AS LONG)
		SUPER()
		SELF:Window := Owner:Owner
		SELF:Initialize()
		RegisterEventHandlers()
		SELF:AllowDrop := TRUE
		SELF:DragEnter += Panel_DragEnter
		SELF:DragDrop += Panel_DragDrop

	METHOD Panel_DragEnter(Sender AS OBJECT, e AS DragEventArgs) AS VOID
		IF (e:Data:GetDataPresent(DataFormats.FileDrop))
			e:Effect := DragDropEffects.Copy
		ENDIF
	RETURN

	METHOD Panel_DragDrop(Sender AS OBJECT, e AS DragEventArgs) AS VOID
	IF IsMethod(SELF:Window,"Drop")
		LOCAL oDE AS DragEvent
		oDE := DragEvent{e, SELF}
		SELF:Window:Drop(oDE,FALSE)
	ENDIF
	RETURN
	
	METHOD Initialize() AS VOID STRICT
		SELF:Cursor         := System.Windows.Forms.Cursors.Arrow
		SELF:Margin         := Padding{0}
		SELF:BorderStyle    := BorderStyle.None	
		SELF:SuppressMovingControls := TRUE
		SELF:BackColor      := System.Drawing.Color.White


	PROTECTED METHOD RegisterEventHandlers() AS VOID STRICT
		oToolTip := VOToolTip{}
		SELF:KeyDown	+= OnKeyDown
		SELF:KeyUp		+= OnKeyUp
		SELF:MouseDown	+= OnMouseDown
		SELF:MouseUp	+= OnMouseUp
		SELF:MouseMove  += OnMouseMove


	PROTECTED METHOD IsInputKey(keyData AS Keys) AS LOGIC
		IF SELF:ReturnAllKeys
			RETURN TRUE
		ENDIF
		RETURN SUPER:IsInputKey(keyData)
				
	METHOD OnKeyDown(s AS OBJECT, e AS KeyEventArgs) AS VOID	
		IF SELF:Window != NULL_OBJECT
			SELF:Window:KeyDown(KeyEvent{e})
		ENDIF
		RETURN		
		
	METHOD OnKeyUp(s AS OBJECT, e AS KeyEventArgs) AS VOID	
		IF SELF:Window != NULL_OBJECT
			SELF:Window:KeyUp(KeyEvent{e})
		ENDIF
		RETURN

	METHOD AdjustMouseEventPosition(e AS MouseEventArgs) AS MouseEventArgs
		LOCAL nDeltaX, nDeltaY AS LONG
		LOCAL oParent AS System.Windows.Forms.Control
		oParent := SELF:Parent
		DO WHILE oParent != NULL
			IF oParent is System.Windows.Forms.Form
				// We are at the form level, so exit
				EXIT
			ENDIF
			nDeltaX += oParent:Location:X
			nDeltaY += oParent:Location:Y
			oParent := oParent:Parent
		ENDDO
		RETURN MouseEventArgs{e:Button, e:clicks, e:X+nDeltaX, e:y+nDeltaY, e:delta}	

	METHOD OnMouseDown(s AS OBJECT, e AS MouseEventArgs) AS VOID	
		IF SELF:Window != NULL_OBJECT
			// Make sure the position is relative to the form
			e := SELF:AdjustMouseEventPosition(e)
			IF e:Clicks == 2
				 
				SELF:Window:MouseButtonDoubleClick(MouseEvent{(MouseEventArgs) e, System.Windows.Forms.Control.ModifierKeys})
			ELSE
				SELF:Window:MouseButtonDown(MouseEvent{e, System.Windows.Forms.Control.ModifierKeys})
			ENDIF
		ENDIF
		RETURN

	METHOD OnMouseUp(s AS OBJECT, e AS MouseEventArgs) AS VOID	
		IF SELF:Window != NULL_OBJECT
			e := SELF:AdjustMouseEventPosition(e)
			SELF:Window:MouseButtonUp(MouseEvent{e, System.Windows.Forms.Control.ModifierKeys})
		ENDIF
		RETURN


	METHOD OnMouseMove(s AS OBJECT, e AS MouseEventArgs) AS VOID	
		IF SELF:Window != NULL_OBJECT
			LOCAL m AS MouseEvent
			e := SELF:AdjustMouseEventPosition(e)
			m := MouseEvent{e, System.Windows.Forms.Control.ModifierKeys}
			IF e:Button == System.Windows.Forms.MouseButtons.None
				SELF:Window:MouseMove(m)
			ELSE
				SELF:Window:MouseDrag(m)
			ENDIF
		ENDIF
		RETURN

	PROTECTED METHOD OnSizeChanged(e AS EventArgs) AS VOID
		SUPER:OnSizeChanged(e)


	METHOD Prepare() AS VOID STRICT
		LOCAL aControls AS System.Windows.Forms.Control[]
        // Was in lSortgroups-Blöcken passiert steuert, dass die Reihenfolge der Gruppen in der Controls-Collection des Panels
        // nach der Position der unteren Kante der Gruppe sortiert ist
		LOCAL lSortGroups := SELF:_aGroups == NULL AS LOGIC 
		LOCAL nKey AS INT
		IF lSortGroups
			SELF:_aGroups   := System.Collections.Generic.SortedList<INT,VOGroupBox>{}
			SELF:_aRBGroups := System.Collections.Generic.SortedList<INT,VOGroupBox>{}
		ENDIF
		IF SELF:Controls:Count > 0 
			SELF:SuspendLayout()
			aControls := System.Windows.Forms.Control[]{SELF:Controls:Count}
			SELF:Controls:CopyTo(aControls,0)
			FOREACH oC AS System.Windows.Forms.Control IN aControls
				IF oC IS VOGroupBox VAR oGroup
					IF ! SELF:SuppressMovingControls
						oGroup:FindChildren()
					ENDIF
					IF lSortGroups
                        // sort key is based on location
						nKey := (oGroup:Location:Y + oGroup:Height)*10000 + oGroup:Location:X 
						DO WHILE SELF:_aGroups:ContainsKey(nKey)
							nKey++
						ENDDO
			    		SELF:_aGroups:Add(nKey,oGroup)
					ENDIF
				ELSEIF oc IS VOPanel VAR  opanel
					opanel:Prepare()
				ELSEIF oc is System.Windows.Forms.TabControl
					LOCAL oTab AS System.Windows.Forms.TabControl
					oTab := (System.Windows.Forms.TabControl) oC
					FOREACH oPage AS TabPage IN oTab:TabPages
						FOREACH IMPLIED oC2 IN oPage:Controls
							IF oC2 IS VOPanel VAR panel
								panel:Prepare()
							ENDIF						
						NEXT
					NEXT
				ELSEIF oc is VODataGridView
					oC:Size := SELF:Size
				ENDIF
				oC:PerformLayout() 
			NEXT
			IF lSortGroups // Controlreihenfolge der Gruppe anpassen
				SELF:SortGroups()
			ENDIF
			SELF:ResumeLayout(TRUE)
		ENDIF
		RETURN

	METHOD SortGroups() AS VOID STRICT
		LOCAL nKey AS INT
		SELF:_aGroups       := System.Collections.Generic.SortedList<INT,VOGroupBox>{}
		SELF:_aRBGroups     := System.Collections.Generic.SortedList<INT,VOGroupBox>{}
		SELF:_aFramepanels  := System.Collections.Generic.SortedList<INT,VOFramepanel>{}
		FOREACH oC AS System.Windows.Forms.Control IN SELF:Controls
			IF oc IS VOGroupBox VAR oGroup
				nKey := ((oGroup:Location:Y + oGroup:Height)*10000 + oGroup:Location:X)*(-1) // untere kante berechnen (oben>unten , links > rechts)
				DO WHILE SELF:_aGroups:ContainsKey(nKey)
					nKey++
				ENDDO
				SELF:_aGroups:Add(nKey,oGroup)
			ENDIF
			IF oc IS VOFramePanel VAR oFrame
				nKey := ((oFrame:Location:Y + oFrame:Height)*10000 + oFrame:Location:X)*(-1) // untere kante berechnen (oben>unten , links > rechts)
				DO WHILE SELF:_aFramepanels:ContainsKey(nKey)
					nKey++
				ENDDO
				SELF:_aFramepanels:Add(nKey,oFrame)
			ENDIF
		NEXT
		nKey := SELF:Controls:Count-1
		FOREACH VAR kvp IN SELF:_aGroups
			SELF:Controls:SetChildIndex(kvp:Value,nKey)
			nKey--
		NEXT
		FOREACH VAR kvp IN SELF:_aRBGroups
			SELF:Controls:SetChildIndex(kvp:Value,nKey)
			nKey--
		NEXT
		FOREACH VAR kvp IN SELF:_aFramepanels
			SELF:Controls:SetChildIndex(kvp:Value,nKey)
			nKey--
		NEXT
		RETURN
		
	PROTECTED METHOD OnVisibleChanged(e AS EventArgs ) AS VOID
		SUPER:OnVisibleChanged(e)
		IF SELF:Visible
			Win32.SendMessage(SELF:Handle, WM_UPDATEUISTATE, MakeWParam(UIS_CLEAR,UISF_HIDEFOCUS) , 0) // always show focus rectangles, switching the ui mode flickers
			SELF:_lNoUpdateUIState := TRUE
			IF SELF:Parent != NULL_OBJECT
				IF self:Parent is System.Windows.Forms.Form
					SELF:Prepare()
				ENDIF
			ENDIF
		ELSE
			SELF:HideToolTip()
			SELF:_lNoUpdateUIState := FALSE
		ENDIF
		RETURN		

	VIRTUAL PROTECT METHOD WndProc(msg REF Message) AS VOID
		IF SELF:_lNoUpdateUIState .AND. (msg:Msg == WM_UPDATEUISTATE .OR. msg:Msg == WM_CHANGEUISTATE)
			RETURN
		ENDIF
		SUPER:WndProc(REF msg)
		RETURN

	METHOD HideToolTip() AS VOID
		IF oToolTip != NULL_OBJECT
			SELF:oToolTip:Active := FALSE
		ENDIF
		RETURN
	
	METHOD RemoveToolTip(oC AS 	System.WIndows.Forms.Control) AS VOID STRICT
		IF SELF:oToolTip != NULL_OBJECT
			SELF:oToolTip:SetToolTip(oC, Null)
		ENDIF
		RETURN
		
	METHOD ShowToolTip(oC AS System.WIndows.Forms.Control, cMessage AS STRING) AS VOID STRICT
		IF !STRING.IsNullOrEmpty(cMessage)
			IF oC != oLastControl
				SELF:HideToolTip()
				IF SELF:lToolTipsEnabled
					SELF:oToolTip:Active := TRUE
					SELF:oToolTip:SetToolTip(oC, cMessage)
					oLastControl := oC
				ENDIF
			ENDIF
		ELSE
			SELF:HideToolTip()
		ENDIF
		RETURN
		
	METHOD EnableToolTips(lEnable AS LOGIC) AS VOID STRICT
		lToolTipsEnabled := lEnable	
		RETURN

	METHOD IsParentOf(oC AS System.Windows.Forms.Control)
		IF oC == SELF
			RETURN FALSE
		ENDIF
		DO WHILE oC != NULL_OBJECT
			IF oC == SELF
				RETURN TRUE
			ENDIF
			oC := oC:Parent
		ENDDO
		RETURN FALSE

END CLASS


CLASS VOSurfacePanel INHERIT VOPanel
	PROTECT lShown	AS LOGIC
	
	CONSTRUCTOR(oWindow AS Window) STRICT
		SUPER(oWindow)

	CONSTRUCTOR(Owner AS XSharp.VO.Control, dwStyle AS LONG, dwExStyle AS LONG)
		SUPER(Owner, dwStyle, dwExStyle)
		RETURN 

	METHOD Initialize() AS VOID STRICT
		SUPER:Initialize()
		SELF:TabIndex	:= 0
		SELF:lShown := FALSE
		SELF:AutoSizeMode := AutoSizeMode.GrowAndShrink
		SELF:AutoSize     := TRUE
#ifdef DEBUG
        SELF:BackColor := System.Drawing.Color.Bisque
        SELF:Text        := "SurfacePanel"
#endif        
		RETURN

	PROTECTED METHOD OnVisibleChanged(e AS EventArgs) AS VOID
		SUPER:OnVisibleChanged(e)
		IF SELF:Visible .and. ! lShown
			IF SELF:MinimumSize == System.Drawing.Size.Empty
				SELF:MinimumSize := SELF:Size
			ENDIF
			SELF:lShown := TRUE
		ENDIF

END CLASS

CLASS VOFramePanel INHERIT VOPanel
	PROTECT oSurfacePanel	AS VOSurfacePanel
	PROTECT oDwForm		AS VODataForm
	PROTECT lShown		AS LOGIC
	PROPERTY IsSubForm AS LOGIC AUTO
	CONSTRUCTOR(oOwner AS VODataForm, oWindow AS Window) STRICT
		SUPER(oWindow)
		oDwForm := oOwner

	CONSTRUCTOR(Owner AS XSharp.VO.Control, dwStyle AS LONG, dwExStyle AS LONG)
		SUPER(Owner, dwStyle, dwExStyle)
		RETURN 

	METHOD Initialize() AS VOID STRICT
		SUPER:Initialize()
		SELF:TabIndex := 1
		SELF:AutoScroll := TRUE						// Show scrollbars when surface is bigger than the backpanel
		SELF:lShown := FALSE
#ifdef DEBUG
        SELF:BackColor   := System.Drawing.Color.Beige
        SELF:Text        := "FramePanel"
#endif        
		RETURN

	VIRTUAL PROTECTED PROPERTY CreateParams AS System.Windows.Forms.CreateParams 
		GET
			LOCAL IMPLIED result := SUPER:CreateParams
			result:Style := 0X56000000 // kommt in 0x56010000 -> WS_TABSTOP entfernt sonst gibts unschöne scrollbars
			RETURN result
		END GET
	END PROPERTY

	PROPERTY DefinedSize AS System.Drawing.Size AUTO	
	PROPERTY SurfacePanel AS VOSurfacePanel
	GET
		IF oSurfacePanel == NULL_OBJECT
			FOREACH IMPLIED oC IN Controls
				IF oc is VOSurfacePanel
					oSurfacePanel := (VOSurfacePanel) oC
					EXIT
				ENDIF
			NEXT
		ENDIF
		RETURN oSurfacePanel
	END GET
	END PROPERTY

	PROTECTED METHOD OnSizeChanged(e AS EventArgs) AS VOID
		// When we come here as a result of SkinManager.ApplySkin
		// Then the Frame has been resized by COdeJock
		// In that case we resize back to the DefinedSize
		SUPER:OnSizeChanged(e)
		IF SELF:IsSubForm .and. ! SELF:DefinedSize:IsEmpty .and. ! lShown
			lShown := true // Applyskin should only trigger this one time. But this is also triggered when the window is opened Maximized, and should not resize then -> #7857
			IF SELF:DefinedSize:Width != SELF:Size:Width .OR. SELF:DefinedSize:Height != SELF:Size:Height
				SELF:Size := SELF:DefinedSize
			ENDIF
		ENDIF
		IF SELF:SurfacePanel != NULL_OBJECT .and. SELF:SurfacePanel:Visible
			LOCAL oSize AS System.Drawing.Size
			oSize := System.Drawing.Size{}
			IF SELF:SurfacePanel:MinimumSize:Width > SELF:SurfacePanel:Width //SELF:HorizontalScroll:Visible .AND. 
				oSize:Width := SELF:SurfacePanel:MinimumSize:Width
			ELSE
				oSize:Width := SELF:Width
			ENDIF
			IF SELF:SurfacePanel:MinimumSize:Height > SELF:SurfacePanel:Height //SELF:VerticalScroll:Visible .AND.
				oSize:Height := SELF:SurfacePanel:MinimumSize:Height
			ELSE
				oSize:Height := SELF:Height
			ENDIF
			IF oSize:Width != SELF:SurfacePanel:Width .OR. oSize:Height != SELF:SurfacePanel:Height
				SELF:SurfacePanel:Size := oSize
			ENDIF
		ENDIF
		IF SELF:Parent != SELF:oDwForm .and. SELF:oDwForm != NULL_OBJECT .and. SELF:oDwForm:Window != NULL_OBJECT
			// When we are on a subform then we must call the resize of the oDwform to make sure controls are aligned
			SELF:oDwForm:Window:Resize(ResizeEvent{})
		ENDIF		
		RETURN 
		
	PROTECTED METHOD OnVisibleChanged(e AS EventArgs) AS VOID
		SUPER:OnVisibleChanged(e)
		IF SELF:Visible
			lShown := TRUE
		ENDIF	
		RETURN 
END CLASS
