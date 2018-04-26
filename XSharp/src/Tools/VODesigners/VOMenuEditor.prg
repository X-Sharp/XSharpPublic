#using System.Collections
#using System.Collections.Generic
#using System.Windows.Forms
#using System.Drawing
#using System.IO
#using System.Text
#using System.Globalization

PARTIAL CLASS VOMenuEditor INHERIT DesignerBase
//	PROTECT aAffected AS List<STRING>

	EXPORT oTree AS MEditorTreeView
	EXPORT oMainNode AS DesignTreeNode

	PROTECT cDefaultFileName AS STRING
	
	STATIC EXPORT Clipboard AS DesignerClipboard
	
	CONSTRUCTOR(_oSurface AS Control , _oGrid AS DesignerGrid)
		SUPER(_oSurface)

		IF VOMenuEditor.Clipboard == NULL
			VOMenuEditor.Clipboard := DesignerClipboard{}
		END IF
		
		SELF:oGrid := _oGrid
		SELF:oGrid:PropertyModified := PropertyUpdatedEventHandler{ SELF , @PropertyModifiedInGrid() }
		SELF:oGrid:ControlKeyPressed := ControlKeyPressedEventHandler{ SELF , @ControlKeyPressedInGrid() }
		SELF:oGrid:oActiveDesigner := SELF
		SELF:oGrid:UseHierarchy(FALSE)

//		SELF:aAffected := List<STRING>{}
		SELF:aActions := ArrayList{}

		SELF:oTree := MEditorTreeView{SELF}
		SELF:oTree:GotFocus += EventHandler{ SELF , @TreeGotFocus() }
		SELF:oTree:LostFocus += EventHandler{ SELF , @TreeLostFocus() }
	
		SELF:oMainNode := DesignTreeNode{999 , SELF}
		SELF:oMainNode:ForeColor := Color.Red
		SELF:oMainNode:NodeFont := Font{SELF:oTree:Font , FontStyle.Bold}
		SELF:oTree:Nodes:Add(SELF:oMainNode)

		SELF:oTimer:Start()
		
	RETURN

	ACCESS IsDirty AS LOGIC
	RETURN SELF:nAction != SELF:nActionSaved

	VIRTUAL METHOD GiveFocus() AS VOID
		SELF:oTree:Focus()
	RETURN
    METHOD TreeGotFocus(o AS OBJECT , e AS EventArgs) AS VOID
    	SELF:ShowHideTools(TRUE)
    	IF SELF:oGrid:FindForm() != NULL
	    	SELF:oGrid:FindForm():Text := "Menu Editor Properties" // TODO: need to use resource for that
    	END IF
		SELF:oGrid:PropertyModified := PropertyUpdatedEventHandler{ SELF , @PropertyModifiedInGrid() }
		SELF:oGrid:ControlKeyPressed := ControlKeyPressedEventHandler{ SELF , @ControlKeyPressedInGrid() }
		SELF:oGrid:oActiveDesigner := SELF
		SELF:oGrid:UseHierarchy(FALSE)
//		SELF:oToolBox:SetViewMode(SELF:ViewMode)
    RETURN
    METHOD TreeLostFocus(o AS OBJECT , e AS EventArgs) AS VOID
    	SELF:ShowHideTools(FALSE)
    RETURN

    METHOD ShowHideTools(lShow AS LOGIC) AS VOID
    	LOCAL oGridForm AS Form
    	IF SELF:oGrid != NULL
    		oGridForm := SELF:oGrid:FindForm()
    	ENDIF
    	IF oGridForm != NULL
    		IF Form.ActiveForm == oGridForm
    			RETURN
    		ENDIF
    		IF lShow
    			oGridForm:Show()
    			SELF:GiveFocus()
    		ELSE
    			IF !SELF:oSurface:ContainsFocus
    				oGridForm:Hide()
    			END IF
    		END IF
    	ENDIF
    RETURN

	ACCESS HasAccelerators AS LOGIC
		LOCAL aDesign AS ArrayList
		LOCAL oDesign AS DesignMenuItem
		LOCAL oAccel AS MenuAccelerator
		LOCAL n AS INT
		aDesign := SELF:GetAllDesignItems()
		FOR n := 0 UPTO aDesign:Count - 1
			oDesign := (DesignMenuItem)aDesign[n]
			oAccel := (MenuAccelerator)oDesign:GetProperty("Accelerator"):Value
			IF !oAccel:IsEmpty
				RETURN TRUE
			END IF
		NEXT
	RETURN FALSE

	ACCESS AutoUpdateItem AS INT
		LOCAL oNode AS TreeNode
		LOCAL cText AS STRING
		LOCAL n AS INT
		FOR n := 0 UPTO SELF:oMainNode:Nodes:Count - 1
			oNode := SELF:oMainNode:Nodes[n]
			cText := oNode:Text:Trim():ToUpper()
			cText := cText:Replace("&" , "")
			IF cText == "WINDOW"
				RETURN n
			END IF
		NEXT
	RETURN -1

	METHOD SetCaption(oDesign AS DesignMenuItem , cText AS STRING) AS VOID
		SELF:BeginAction()
		SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{oDesign:cGuid , "Caption" , cText})
		SELF:EndAction()
	RETURN

	METHOD PropertyModifiedInGrid(cProp AS STRING , oValue AS OBJECT) AS VOID
		LOCAL aSelected AS ArrayList
		LOCAL oDesign AS DesignMenuItem
		LOCAL n AS INT
		aSelected := SELF:GetSelected()
		SELF:BeginAction()
		FOR n := 0 UPTO aSelected:Count - 1
			oDesign := (DesignMenuItem)aSelected[n]
			SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{oDesign:cGuid , cProp , oValue})
		NEXT
		SELF:EndAction()
	RETURN

	METHOD CanDoAction(eAction AS DesignerActionType) AS LOGIC
		
		IF SELF:lReadOnly
			RETURN FALSE
		ENDIF
		
		SWITCH eAction
		CASE DesignerActionType.Undo
			RETURN SELF:nAction >= 1
		CASE DesignerActionType.Redo
			RETURN SELF:nAction < SELF:aActions:Count

		CASE DesignerActionType.Cut
			RETURN SELF:oTree:SelectedNode:Parent != NULL
		CASE DesignerActionType.Copy
			RETURN SELF:oTree:Nodes[0]:Nodes:Count != 0
		CASE DesignerActionType.Paste
			RETURN VOMenuEditor.Clipboard:Count != 0
		END SWITCH
	RETURN FALSE

	VIRTUAL METHOD BeginAction() AS VOID
		IF SELF:nActionDepth == 0
			SELF:lDidAction := FALSE
			SELF:lClearUndoBuffer := FALSE
		ENDIF
		SELF:nActionDepth ++
	RETURN
	VIRTUAL METHOD EndAction() AS VOID
		LOCAL oAction AS DesignerBasicAction
		LOCAL aSelected AS ArrayList
		LOCAL oDesign AS DesignMenuItem
		LOCAL n AS INT

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
	
			aSelected := SELF:GetSelected()
			IF oAction != NULL .and. oAction:aSelected:Count == 0
				FOR n := 0 UPTO aSelected:Count - 1
					oDesign := (DesignMenuItem)aSelected[n]
					oAction:aSelected:Add(oDesign:cGuid)
				NEXT
			ENDIF
	
			SELF:DisplayProperties()

            IF SELF:IsDirtyChanged != NULL
                SELF:IsDirtyChanged:Invoke(SELF , EventArgs{})
            ENDIF
            
		ENDIF
	
	RETURN

	VIRTUAL METHOD DoBasicAction(oAction AS DesignerBasicAction , eAction AS DesignerBasicActionType , uData AS ActionData) AS OBJECT
		LOCAL aProperties AS NameValueCollection
		LOCAL oUndo,oRedo AS DesignerBasicAction
		LOCAL oNode, oParent AS DesignTreeNode
		LOCAL oDesign AS DesignMenuItem
		LOCAL oProp AS VODesignProperty
		LOCAL cGuid , cParent AS STRING
		LOCAL oEnumerator AS IEnumerator
		LOCAL oNameValue AS NameValue
		LOCAL n,nIndex AS INT
		LOCAL oRet AS OBJECT

		SWITCH eAction
		CASE DesignerBasicActionType.Create
			cGuid := uData:cGuid
			IF cGuid == NULL
				cGuid := Guid.NewGuid():ToString()
			END IF
			cParent := uData:cData
			nIndex := (INT)uData:oData
			oParent := SELF:GetDesignItemFromGuid(cParent):oNode
			oNode := SELF:oTree:CreateNewNode()
			oParent:Nodes:Insert(nIndex , oNode)
			oParent:Expand()
			oDesign := oNode:oDesign
			oDesign:cGuid := cGuid
			oRet := oDesign
			SELF:lDidAction := TRUE
			IF !oAction:lExecuted
				oUndo := DesignerBasicAction{TRUE}
				oUndo:eAction := DesignerBasicActionType.Remove
				oUndo:uData := ActionData{oDesign:cGuid}
				oAction:aUndo:Add(oUndo)
				
				oRedo := DesignerBasicAction{TRUE}
				oRedo:eAction := DesignerBasicActionType.Create
				oRedo:uData := ActionData{cGuid , cParent , nIndex}
				oAction:aRedo:Add(oRedo)
			ENDIF
			IF uData:aData != NULL
				oEnumerator := uData:aData:GetEnumerator()
				DO WHILE oEnumerator:MoveNext()
					oNameValue := (NameValue)oEnumerator:Current
					SELF:DoBasicAction(oAction , DesignerBasicActionType.SetProperty , ActionData{cGuid , oNameValue:Name , oNameValue:Value})
				END DO
			ENDIF
//			SELF:AddAffected(oDesign)
			
		CASE DesignerBasicActionType.Remove
			cGuid := uData:cGuid
			oDesign := SELF:GetDesignItemFromGuid(cGuid)
			nIndex := oDesign:oNode:Index
			oParent := (DesignTreeNode)oDesign:oNode:Parent
			cParent := oParent:oDesign:cGuid
			
			DO WHILE oDesign:oNode:Nodes:Count != 0
				SELF:StartAction(DesignerBasicActionType.Remove , ActionData{((DesignTreeNode)oDesign:oNode:Nodes[0]):oDesign:cGuid})
			END DO
			
			oDesign:oNode:Remove()
			oDesign:lDeleted := TRUE
			
			SELF:lDidAction := TRUE
			IF !oAction:lExecuted
				aProperties := NameValueCollection{}
				FOR n := 0 UPTO oDesign:aProperties:Count - 1
					oProp := (VODesignProperty)oDesign:aProperties[n]
					IF TRUE .or. !oProp:IsAuto // problem with accelerator property
						aProperties:Add(oProp:Name , oProp:Value)
					ENDIF
				NEXT

				oUndo := DesignerBasicAction{TRUE}
				oUndo:eAction := DesignerBasicActionType.Create
				oUndo:uData := ActionData{oDesign:cGuid , cParent , nIndex , aProperties}
				oAction:aUndo:Add(oUndo)
				
				oRedo := DesignerBasicAction{TRUE}
				oRedo:eAction := DesignerBasicActionType.Remove
				oRedo:uData := ActionData{oDesign:cGuid}
				oAction:aRedo:Add(oRedo)
			ENDIF

		CASE DesignerBasicActionType.SetParent
			LOCAL nOldIndex AS INT
			LOCAL cOldParent AS STRING
			cGuid := uData:cGuid
			cParent := uData:cData
			nIndex := (INT)uData:oData

			oDesign := SELF:GetDesignItemFromGuid(cGuid)
			oParent := SELF:GetDesignItemFromGuid(cParent):oNode

			nOldIndex := oDesign:oNode:Index
			cOldParent := ((DesignTreeNode)oDesign:oNode:Parent):oDesign:cGuid
			
			oDesign:oNode:Remove()
			oParent:Nodes:Insert(nIndex , oDesign:oNode)
			SELF:lDidAction := TRUE
			IF !oAction:lExecuted
				oUndo := DesignerBasicAction{TRUE}
				oUndo:eAction := DesignerBasicActionType.SetParent
				oUndo:uData := ActionData{cGuid , cOldParent , nOldIndex}
				oAction:aUndo:Add(oUndo)
				
				oRedo := DesignerBasicAction{TRUE}
				oRedo:eAction := DesignerBasicActionType.SetParent
				oRedo:uData := ActionData{cGuid , cParent , nIndex}
				oAction:aRedo:Add(oRedo)
			ENDIF

		CASE DesignerBasicActionType.SetProperty
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
//				SELF:AddAffected(oDesign)
			ENDIF
	
		END SWITCH

		oAction:lExecuted := TRUE

	RETURN oRet

	VIRTUAL METHOD DoAction(eAction AS DesignerActionType) AS VOID
		SELF:DoAction(eAction , NULL)
	RETURN
	VIRTUAL METHOD DoAction(eAction AS DesignerActionType , cGuid AS STRING) AS VOID
//		LOCAL oDesign,oParent,oOldParent,oChild AS DesignWindowItem
		LOCAL oDesign AS DesignMenuItem
		LOCAL aSelected AS ArrayList
		LOCAL n AS INT
		
		IF eAction == DesignerActionType.RemoveSelected
			IF SELF:oTree:SelectedNode != NULL .and. SELF:oTree:SelectedNode:IsEditing
				RETURN
			END IF
		END IF

		SELF:BeginAction()

		SWITCH eAction
		CASE DesignerActionType.SelectAll
		CASE DesignerActionType.SelectAdd
			oDesign := SELF:GetDesignItemFromGuid(cGuid)
			SELF:oTree:SelectedNode := oDesign:oNode
/*			oDesign:oNode:lSelected := TRUE
			SELF:oTree:SetNodeColor(oDesign:oNode)*/
			
		CASE DesignerActionType.RemoveSelected
			aSelected := SELF:GetSelected()
			FOR n := 0 UPTO aSelected:Count - 1
				oDesign := (DesignMenuItem)aSelected[n]
				IF !oDesign:lDeleted
					SELF:StartAction(DesignerBasicActionType.Remove , ActionData{oDesign:cGuid})
				END IF
			NEXT
			
//			oDesign := SELF:GetDesignItemFromGuid(cGuid)
/*			SELF:BeginAction()
			DO WHILE oDesign:oNode:Nodes:Count != 0
				SELF:DoAction(DesignerActionType.RemoveSelected , ((DesignTreeNode)oDesign:oNode:Nodes[0]):oDesign:cGuid)
			ENDDO*/
//			SELF:StartAction(DesignerBasicActionType.Remove , ActionData{oDesign:cGuid})
//			SELF:EndAction()

		CASE DesignerActionType.DeSelectAll
			SELF:oTree:DeSelectAll()

		CASE DesignerActionType.Cut
			SELF:Cut()
		CASE DesignerActionType.Copy
			SELF:Copy()
		CASE DesignerActionType.Paste
			SELF:Paste()

		CASE DesignerActionType.Undo
			SELF:Undo()
		CASE DesignerActionType.Redo
			SELF:Redo()

		END SWITCH

		SELF:EndAction()

	RETURN

	METHOD DoCreate(oParent AS DesignTreeNode , nIndex AS INT) AS DesignTreeNode
		LOCAL oNode AS DesignTreeNode
		LOCAL oDesign AS DesignMenuItem
		IF oParent == NULL
			RETURN NULL
		END IF
		SELF:BeginAction()
		oDesign := (DesignMenuItem)SELF:StartAction(DesignerBasicActionType.Create , ActionData{NULL , oParent:oDesign:cGuid , nIndex})
		SELF:SetCaption(oDesign , "MenuItemName")
		oNode := oDesign:oNode
		SELF:EndAction()
		SELF:oTree:SelectedNode := oNode
		SELF:oTree:LabelEdit := TRUE
		oNode:BeginEdit()
	RETURN oNode

	METHOD DoRemove() AS VOID
		IF SELF:oTree:SelectedNode:Parent == NULL
			RETURN
		ENDIF
		SELF:DoAction(DesignerActionType.RemoveSelected , ((DesignTreeNode)SELF:oTree:SelectedNode):oDesign:cGuid)
/*		SELF:BeginAction()
		SELF:StartAction(DesignerBasicActionType.Remove , ActionData{((DesignTreeNode)SELF:oTree:SelectedNode):oDesign:cGuid})
		SELF:EndAction()*/
	RETURN
	
	METHOD CalculateFirstId() AS INT
		LOCAL cName AS STRING
		LOCAL nId AS INT
		LOCAL n AS INT
		cName := SELF:oMainNode:oDesign:Name
		FOR n := 0 UPTO cName:Length - 1
			nId += ((INT)cName[n]) * (n + 1)
			nId %= 50
		NEXT
		nId := 10000 + (nId * 500) % 22000 // should not get IDs over 32767
	RETURN nId

/*	METHOD GetNewMenuID() AS INT
		LOCAL aDesign AS ArrayList
		LOCAL oDesign AS DesignMenuItem
		LOCAL nMax , nID AS INT
		LOCAL n AS INT
		
		nMax := (INT)SELF:oMainNode:oDesign:GetProperty("BaseID"):Value
		aDesign := SELF:GetAllDesignItems()
		FOR n := 0 UPTO aDesign:Count - 1
			oDesign := (DesignMenuItem)aDesign[n]
			nID := (INT)oDesign:GetProperty("MenuID"):Value
			IF nID > nMax
				nMax := nID
			END IF
		NEXT
	RETURN nMax + 1*/


	METHOD Cut() AS VOID
		SELF:Copy()
		SELF:DoRemove()
	RETURN
	
	METHOD Copy() AS VOID
		LOCAL oEntry AS DesignerClipboardEntry
		LOCAL n AS INT
		VOMenuEditor.Clipboard:Clear()
		IF SELF:oTree:SelectedNode:Parent == NULL
			FOR n := 0 UPTO SELF:oTree:Nodes[0]:Nodes:Count - 1
				oEntry := DesignerClipboardEntry{}
				SELF:CopyNode((DesignTreeNode)SELF:oTree:Nodes[0]:Nodes[n] , oEntry)
				VOMenuEditor.Clipboard:AddEntry(oEntry)
			NEXT
		ELSE
			oEntry := DesignerClipboardEntry{}
			SELF:CopyNode((DesignTreeNode)SELF:oTree:SelectedNode , oEntry)
			VOMenuEditor.Clipboard:AddEntry(oEntry)
		END IF
	RETURN
	METHOD CopyNode(oNode AS DesignTreeNode , oEntry AS DesignerClipboardEntry) AS VOID
		LOCAL oChildEntry AS DesignerClipboardEntry
		LOCAL oDesign AS DesignMenuItem
		LOCAL oProp AS VODesignProperty
		LOCAL n AS INT
	
		oDesign := oNode:oDesign
		FOR n := 0 UPTO oDesign:aProperties:Count - 1
			oProp := (VODesignProperty)oDesign:aProperties[n]
//			IF !oProp:IsAuto // problem with accelerator prop
				oEntry:aProperties:Add(oProp:Name , oProp:Value)
//			ENDIF
		NEXT
		FOR n := 0 UPTO oNode:Nodes:Count - 1
			oChildEntry := DesignerClipboardEntry{}
			SELF:CopyNode((DesignTreeNode)oNode:Nodes[n] , oChildEntry)
			oEntry:aSubEntries:Add(oChildEntry)
		NEXT
		
	RETURN
	
	METHOD Paste() AS VOID
		LOCAL oEntry AS DesignerClipboardEntry
		LOCAL oParent AS DesignTreeNode
		LOCAL n AS INT
		oParent := (DesignTreeNode)SELF:oTree:SelectedNode
		SELF:BeginAction()
		FOR n := 0 UPTO VOMenuEditor.Clipboard:Count - 1
			oEntry := VOMenuEditor.Clipboard:GetEntry(n)
			SELF:PasteNode(oParent , oEntry)
		NEXT
		SELF:EndAction()
	RETURN
	METHOD PasteNode(oParent AS DesignTreeNode , oEntry AS DesignerClipboardEntry) AS VOID
		LOCAL oDesign AS DesignMenuItem
		LOCAL n AS INT
		oDesign := (DesignMenuItem)SELF:StartAction(DesignerBasicActionType.Create , ActionData{NULL , oParent:oDesign:cGuid , oParent:Nodes:Count})
		FOR n := 0 UPTO oEntry:aProperties:Count - 1
			SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{oDesign:cGuid , oEntry:aProperties:GetName(n) , oEntry:aProperties:GetValue(n)})
		NEXT
		FOR n := 0 UPTO oEntry:aSubEntries:Count - 1
			SELF:PasteNode(oDesign:oNode , (DesignerClipboardEntry)oEntry:aSubEntries[n])
		NEXT
	RETURN

	METHOD PropertyGotUpdated(oDesign AS DesignMenuItem , oProp AS VODesignProperty) AS VOID
		SELF:ApplyProperty(oDesign , oProp)
	RETURN
	
	METHOD ApplyProperty(_oDesign AS DesignMenuItem , oProp AS VODesignProperty) AS VOID
		LOCAL oDesign AS DesignMenuItem
		oDesign :=(DesignMenuItem)_oDesign
		
		SWITCH oProp:Name
		CASE "Name"
			IF oDesign:nType == 999
				oDesign:oNode:Text := oProp:TextValue
			END IF
		CASE "Caption"
			IF oDesign:nType != 999
				oDesign:oNode:Text := Funcs.TranslateCaption(oProp:TextValue , FALSE)
			END IF
		CASE "Enabled"
			SELF:oTree:SetNodeColor(oDesign:oNode)
		CASE "ButtonBmp"
/*			LOCAL cValue AS STRING
			cValue := oProp:TextValue_
			IF !cValue == ""
				oDesign:PropertyValueSelected_("ButtonCaption" , cValue)
				oDesign:PropertyValueSelected_("ButtonToolTip" , cValue)
			ENDIF*/
            NOP
		END SWITCH
		
		
	RETURN

	METHOD DisplayProperties() AS VOID
		SELF:oGrid:Fill(SELF:GetSelected())
	RETURN
	
	METHOD GetAllDesignItems() AS ArrayList
		LOCAL oNode AS TreeNode
		LOCAL aDesign := ArrayList{} AS ArrayList
		oNode := SELF:oTree:Nodes[0]
//		oNode := oNode:NextVisibleNode
		oNode := VOMenuEditor.GetNextNode(oNode , TRUE)
		DO WHILE oNode != NULL
			aDesign:Add(((DesignTreeNode)oNode):oDesign)
//			oNode := oNode:NextVisibleNode
			oNode := VOMenuEditor.GetNextNode(oNode , TRUE)
		ENDDO
	RETURN aDesign

	VIRTUAL METHOD GetDesignItemFromGuid(cGuid AS STRING) AS DesignMenuItem
		LOCAL oDesign AS DesignMenuItem
		LOCAL aItems AS ArrayList
		LOCAL n AS INT
		IF SELF:oMainNode:oDesign:cGuid == cGuid
			RETURN SELF:oMainNode:oDesign
		ENDIF
		aItems := SELF:GetAllDesignItems()
		FOR n := 0 UPTO aItems:Count - 1
			oDesign := (DesignMenuItem)aItems[n]
			IF oDesign:cGuid == cGuid
				RETURN oDesign
			ENDIF
		NEXT
	RETURN NULL

	METHOD GetSelected() AS ArrayList
//		LOCAL oNode AS TreeNode
		LOCAL aSelected := ArrayList{} AS ArrayList
/*		oNode := SELF:oTree:Nodes[0]
		oNode := oNode:NextVisibleNode
		DO WHILE oNode != NULL
			IF ((DesignTreeNode)oNode):lSelected
				aSelected:Add(((DesignTreeNode)oNode):oDesign)
			ENDIF
			oNode := oNode:NextVisibleNode
		ENDDO*/
		IF aSelected:Count == 0
			aSelected:Add(((DesignTreeNode)SELF:oTree:SelectedNode):oDesign)
		END IF
	RETURN aSelected

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
		LOCAL oDesign AS DesignMenuItem
		LOCAL oMainDesign AS DesignMenuItem
		LOCAL cTemp AS STRING
		LOCAL oNode AS DesignTreeNode
		LOCAL cMenuName AS STRING
		LOCAL lFound AS LOGIC
		LOCAL nLevel AS INT
		LOCAL n,m AS INT
		LOCAL nDesign AS INT

		LOCAL aDefines AS List<STRING>
		LOCAL aDefineValues AS List<STRING>
		LOCAL aResource AS List<STRING>
		LOCAL aClass AS List<STRING>
		LOCAL aConstructor AS List<STRING>
		
		LOCAL nMinPos , nMaxPos AS INT
		LOCAL nPos AS INT
		LOCAL lUseBands AS LOGIC
		LOCAL lToolBar AS LOGIC
		LOCAL cInherit AS STRING
		LOCAL cToolbarInherit AS STRING
		LOCAL cEventName AS STRING
		
		LOCAL oAccelerator AS MenuAccelerator
		LOCAL cAccelerator AS STRING
		LOCAL lAccelerators AS LOGIC
		
    	SELF:UpdateNames()
		oCode := CodeContents{}
		lAccelerators := SELF:HasAccelerators

// defines

		aDefines := oCode:aDefines
		aDefineValues := oCode:aDefineValues
		
		oDesign := SELF:oMainNode:oDesign
		aDefines:Add("IDM_" + oDesign:Name)
		aDefineValues:Add(e"\"" + oDesign:Name + e"\"")
		IF lAccelerators
			aDefines:Add("IDA_" + oDesign:Name)
			aDefineValues:Add(e"\"" + oDesign:Name + e"\"")
		END IF

		aDesign := SELF:GetAllDesignItems()
		FOR n := 0 UPTO aDesign:Count - 1
			oDesign := (DesignMenuItem)aDesign[n]
			IF !oDesign:IsSeparator
				lFound := FALSE
				FOR m := 0 UPTO aDefines:Count - 1
					IF aDefines[m]:ToUpper() == oDesign:GetVODefine():ToUpper()
						lFound := TRUE
						EXIT
					END IF
				NEXT
				IF !lFound
					aDefines:Add(oDesign:GetVODefine())
					aDefineValues:Add(oDesign:GetProperty("MenuID"):TextValue)
				END IF
			END IF
		NEXT
		
		

// resource

		IF lAccelerators
			aResource := oCode:aAccelResource
	
			oDesign := SELF:oMainNode:oDesign
			aResource:Add(oDesign:Name + "_Accelerator ACCELERATORS")
			aResource:Add("BEGIN")
			FOR n := 0 UPTO aDesign:Count - 1
				oDesign := (DesignMenuItem)aDesign[n]
				oAccelerator := (MenuAccelerator)oDesign:GetProperty("Accelerator"):Value
				IF !oAccelerator:IsEmpty
					cLine := e"\t"
					cLine += oAccelerator:KeyValue:ToString() + ", "
					cLine += oDesign:GetVODefine() + ", "
//					cLine += oDesign:GetProperty("MenuID"):TextValue + ", "
					IF oAccelerator:Control
						cLine += "CONTROL ,"
					END IF
					IF oAccelerator:Shift
						cLine += "SHIFT ,"
					END IF
					IF oAccelerator:Alt
						cLine += "ALT ,"
					END IF
					cLine += "VIRTKEY"
					aResource:Add(cLine)
				END IF
			NEXT
			aResource:Add("END")
		END IF
		


		aResource := oCode:aResource

		oDesign := SELF:oMainNode:oDesign
		oMainDesign := oDesign
		cMenuName := oDesign:Name
		aResource:Add(SELF:oMainNode:Text + " MENU")
		aResource:Add("BEGIN")
		
		FOR n := 0 UPTO aDesign:Count - 1
			oDesign := (DesignMenuItem)aDesign[n]
			oAccelerator := (MenuAccelerator)oDesign:GetProperty("Accelerator"):Value
			IF oAccelerator:IsEmpty
				cAccelerator := ""
			ELSE
				cAccelerator := "\t" + oAccelerator:ToString()
			END IF
			oNode := oDesign:oNode
			IF oDesign:GetNestLevel() < nLevel
				FOR m := nLevel - 1 DOWNTO oDesign:GetNestLevel()
					cLine := System.String{'\t' , m} + "END"
					aResource:Add(cLine)
				NEXT
			ENDIF
			cLine := System.String{'\t' , oDesign:GetNestLevel()}
			IF oDesign:IsSeparator
				cLine += "MENUITEM SEPARATOR"
			ELSEIF oNode:Nodes:Count == 0
				cLine += e"MENUITEM \"" + Funcs.TranslateCaption( oDesign:GetProperty("Caption"):TextValue , FALSE ) + cAccelerator + e"\" , " + oDesign:GetVODefine()
//				cLine += e"MENUITEM \"" + oDesign:GetProperty("Caption"):TextValue + cAccelerator + e"\" , " + oDesign:GetProperty("MenuID"):TextValue
			ELSE
				cLine += e"POPUP \"" + Funcs.TranslateCaption( oDesign:GetProperty("Caption"):TextValue , FALSE ) + e"\""
			ENDIF
			IF !oDesign:GetProperty("Enabled"):ValueLogic
				cLine += ", GRAYED"
			END IF
			IF oDesign:GetProperty("Checked"):ValueLogic
				cLine += ", CHECKED"
			END IF
			aResource:Add(cLine)

			nLevel := oDesign:GetNestLevel()

			IF oNode:Nodes:Count != 0
				cLine := System.String{'\t' , nLevel} + "BEGIN"
				aResource:Add(cLine)
			ENDIF
		NEXT

		IF nLevel > 0
			FOR n := nLevel - 1 DOWNTO 0
				cLine := System.String{'\t' , n} + "END"
				aResource:Add(cLine)
			NEXT
		ELSE
			cLine := System.String{'\t' , n} + "END"
			aResource:Add(cLine)
		ENDIF


// class declaration
		
		aClass := oCode:aClass

		oDesign := SELF:oMainNode:oDesign

		cInherit := oDesign:GetProperty("Inherit"):TextValue:Trim()
		IF cInherit == System.String.Empty
			cInherit := "Menu"
		END IF
		cToolbarInherit := oDesign:GetProperty("ToolbarInherit"):TextValue:Trim()
		IF cToolbarInherit == System.String.Empty
			cToolbarInherit := "Toolbar"
		END IF

		aClass:Add("CLASS " + oDesign:Name + " INHERIT " + cInherit)
		aClass:Add("")


		IF lAccelerators
			aClass := oCode:aAccelClass
			aClass:Add("CLASS " + oDesign:Name + "_Accelerator INHERIT Accelerator")
			aClass:Add("")
		END IF


// constructor

		IF lAccelerators
			aConstructor := oCode:aAccelConstructor
	
			oDesign := SELF:oMainNode:oDesign
			aConstructor:Add("CONSTRUCTOR()")
			aConstructor:Add(e"\tSUPER( ResourceID { \"" + oDesign:Name + "_Accelerator" + e"\" , _GetInst( ) } )")
			aConstructor:Add("RETURN")
			aConstructor:Add("")
		END IF



		aConstructor := oCode:aConstructor

		oDesign := SELF:oMainNode:oDesign
		lUseBands := oDesign:GetProperty("UseBands"):ValueLogic
		nMaxPos := -99999
		nMinPos := 999999
		FOR n := 0 UPTO aDesign:Count - 1
			oDesign := (DesignMenuItem)aDesign[n]
			IF !oDesign:GetProperty("ButtonBmp"):TextValue == ""
				lToolBar := TRUE
				nPos := (INT)oDesign:GetProperty("ButtonPos"):Value
				IF nPos > nMaxPos
					nMaxPos := nPos
				ENDIF
				IF nPos < nMinPos .and. nPos >= 0
					nMinPos := nPos
				ENDIF
			ENDIF
		NEXT
		IF (INT)oMainDesign:GetProperty("Toolbar"):Value == 0
			lToolBar := FALSE
		ENDIF

		aConstructor:Add("CONSTRUCTOR( oOwner )")
		aConstructor:Add("")
		IF lToolBar
			aConstructor:Add(e"\tLOCAL oTB AS " + cToolbarInherit)
			aConstructor:Add("")
		ENDIF
		aConstructor:Add(e"\tSELF:PreInit()")
		aConstructor:Add("")
		aConstructor:Add(e"\tSUPER( ResourceID { \"" + cMenuName + e"\" , _GetInst( ) } )")
		aConstructor:Add("")


		FOR nDesign := 0 UPTO aDesign:Count - 1
			
			oDesign := (DesignMenuItem)aDesign[nDesign]
			oNode := oDesign:oNode
			
			IF oDesign:IsSeparator
				LOOP
			ENDIF
			
			cLine := e"\tSELF:RegisterItem(" + oDesign:GetVODefine() + ", ;"
			aConstructor:Add(cLine)
			cEventName := oDesign:GetProperty("EventName"):TextValue:Trim()
			IF cEventName == ""
//				cEventName := oDesign:Name
				cEventName := oDesign:cNameID
			ENDIF
			oAccelerator := (MenuAccelerator)oDesign:GetProperty("Accelerator"):Value
			IF oAccelerator:IsEmpty
				cAccelerator := ""
			ELSE
				cAccelerator := e"\t" + oAccelerator:ToString()
			END IF
			
			cLine := e"\t\t"

			IF cEventName:Trim():Length == 0
				cLine += "HyperLabel{ #NoEvent , "
			ELSE
				cLine += "HyperLabel{ #" + cEventName + " , "
			END IF

			cTemp := oDesign:GetProperty("Caption"):TextValue
			cTemp := iif(cTemp == "" , "" , Funcs.TranslateCaption(cTemp , TRUE) )
			IF .not. cTemp:StartsWith("LoadResString") .and. cTemp:Length > 2 .and. cAccelerator != ""
				// put it inside the quotes
				cTemp := cTemp:Substring(0 , cTemp:Length - 1) + cAccelerator + cTemp:Substring(cTemp:Length - 1)
			END IF
			cLine += cTemp + " , "

			cTemp := oDesign:GetProperty("Description"):TextValue
			cTemp := iif(cTemp == "" , "" , Funcs.TranslateCaption(cTemp , TRUE) )
			cLine += cTemp + " , "

			cTemp := oDesign:GetProperty("HelpContext"):TextValue
			cTemp := iif(cTemp == "" , "" , Funcs.TranslateCaption(cTemp , TRUE) )
			cLine += cTemp + " }"
		
			IF oNode:Nodes:Count != 0
	
				LOCAL cNest AS STRING
				LOCAL nNest AS INT
				LOCAL oParent AS TreeNode
				LOCAL aNest AS List<INT>
				
				aNest := List<INT>{}
	
				oParent := oNode
				DO WHILE oParent:Parent != NULL
					aNest:Add(oParent:Index)
					oParent := oParent:Parent
				END DO
				
				cNest := ""
				FOR n := aNest:Count - 1 DOWNTO 0
					nNest := aNest[n]
					IF n == aNest:Count - 1
						cNest := "SELF:Handle() , " + nNest:ToString()
					ELSE
						cNest := "GetSubMenu( " + cNest + " ) , " + nNest:ToString()
					ENDIF
				NEXT
				cLine := cLine + " , " + cNest
	
			ENDIF
	
			cLine += ")"
			aConstructor:Add(cLine)
			aConstructor:Add("")
	
		NEXT

		#region AutoUpdate
	
		LOCAL nAutoUpdate AS INT
		nAutoUpdate := SELF:AutoUpdateItem
		IF nAutoUpdate != -1
			cLine := e"\tSELF:SetAutoUpdate( " + nAutoUpdate:ToString() + " )"
			aConstructor:Add(cLine)
			aConstructor:Add("")
		ENDIF

		#endregion

		#region Toolbar

		LOCAL cFlat AS STRING
		LOCAL nToolBarShow AS INT
		LOCAL nBand AS INT

		IF lToolBar
			aConstructor:Add(e"\toTB := " + cToolbarInherit + "{}")
			aConstructor:Add("")
	
			cLine := e"\toTB:ButtonStyle := "
			nToolBarShow := (INT)oMainDesign:GetProperty("Show"):Value
			DO CASE
			CASE nToolBarShow == 0
				cLine += "TB_TEXTONLY"
			CASE nToolBarShow == 1
				cLine += "TB_ICONONLY"
			CASE nToolBarShow == 2
				cLine += "TB_TEXTANDICON"
			END CASE
			aConstructor:Add(cLine)
	
			IF (INT)oMainDesign:GetProperty("Toolbar"):Value == 1
				cFlat := "TRUE"
				aConstructor:Add(e"\toTB:Flat := TRUE")
			ELSE
				cFlat := "FALSE"
			ENDIF
			cLine := e"\toTB:EnableBands(" + iif(lUseBands , "TRUE" , "FALSE") + ")"
			aConstructor:Add(cLine)
			aConstructor:Add("")
	
			LOCAL cIdt , cCaption , cToolTip AS STRING
			LOCAL lLastSeparator AS LOGIC
			LOCAL oButton AS VOTBButton
			LOCAL cButtonBmp AS STRING
			LOCAL aButtons AS ArrayList
			LOCAL nIcons AS INT
			aButtons := ArrayList{}
			
			FOR n := nMinPos UPTO nMaxPos
	
				lFound := FALSE
				FOR nDesign := 0 UPTO aDesign:Count - 1
					oDesign := (DesignMenuItem)aDesign[nDesign]
					cButtonBmp := oDesign:GetProperty("ButtonBmp"):TextValue:ToUpper()
					IF cButtonBmp != ""
						nPos := (INT)oDesign:GetProperty("ButtonPos"):Value
						IF nPos == n
							lFound := TRUE

//							m := Int32.Parse(cButtonBmp)
							m := VOMenuEditor.VOMenuToolBar:GetNameIndex(cButtonBmp)
                            IF m != -1
//	                            cIdt := (STRING)VOMenuEditor.VOMenuToolBar:GetValue(m-1) + " , " + oDesign:GetVODefine()
	                            cIdt := (STRING)VOMenuEditor.VOMenuToolBar:GetValue(m) + " , " + oDesign:GetVODefine()
								cCaption := oDesign:GetProperty("ButtonCaption"):TextValue
								cToolTip := oDesign:GetProperty("ButtonToolTip"):TextValue
								aButtons:Add(VOTBButton{cIdt , cCaption , cToolTip})
							END IF

						ENDIF
					ENDIF
				NEXT
				IF !lFound
					aButtons:Add(VOTBButton{})
				ENDIF
	
			NEXT
	
			FOR n := aButtons:Count DOWNTO 1
				oButton := (VOTBButton)aButtons[n - 1]
				IF oButton:lSeparator .and. !lLastSeparator
					oButton:lLastSeparator := TRUE
					lLastSeparator := TRUE
				ENDIF
				IF !oButton:lSeparator
					nIcons ++
				ENDIF
				IF oButton:lSeparator .or. n == 1
					oButton:nIcons := nIcons
					nIcons := 0
				ENDIF
			NEXT
			
			lLastSeparator := FALSE
			FOR n := 1 UPTO aButtons:Count
				oButton := (VOTBButton)aButtons[n-1]
				IF lUseBands .and. (n == 1 .or. (oButton:lSeparator .and. !oButton:lLastSeparator) )
					nBand ++
					cLine := e"\toTB:AddSubToolBarBand(#__SUBBAND" + nBand:ToString() + " , , " + (oButton:nIcons * 24):ToString() + " , "
					cLine += cFlat + ")"
					aConstructor:Add(cLine)
				ENDIF
				IF oButton:lLastSeparator
					lLastSeparator := TRUE
				ENDIF
				IF oButton:lSeparator
					IF !lUseBands
						aConstructor:Add(e"\toTB:AppendItem(IDT_SEPARATOR)")
					ENDIF
				ELSE
					IF lUseBands .and. !lLastSeparator
						cLine := e"\toTB:AppendSubItem(#__SUBBAND" + nBand:ToString() + " , "
					ELSE
						cLine := e"\toTB:AppendItem("
					ENDIF
					cLine += oButton:cIdt
					IF nToolBarShow != 1
						IF oButton:cCaption == ""
							cLine += " , , , " + e"\" \""
						ELSE
//							cLine += " , , , " + e"\"" + oButton:cCaption + e"\""
							cLine += " , , , " + Funcs.TranslateCaption(oButton:cCaption , TRUE)
						ENDIF
					ENDIF
					cLine += ")"
					aConstructor:Add(cLine)
					IF oButton:cToolTip != ""
//						cLine := e"\toTB:AddTipText(" + oButton:cIdt + " , " + e"\"" + oButton:cToolTip + e"\")"
						cLine := e"\toTB:AddTipText(" + oButton:cIdt + " , " + Funcs.TranslateCaption(oButton:cToolTip , TRUE) + ")"
						aConstructor:Add(cLine)
					ENDIF
				ENDIF
				
				aConstructor:Add("")
	
			NEXT
			
	
			aConstructor:Add("")
			aConstructor:Add(e"\tSELF:ToolBar := oTB")
		ENDIF

		#endregion Toolbar

		IF SELF:HasAccelerators
			aConstructor:Add(e"\tSELF:Accelerator := " + SELF:oMainNode:oDesign:Name + "_Accelerator{ }")
			aConstructor:Add("")
		END IF        

		FOR n := 0 UPTO aDesign:Count - 1
			oDesign := (DesignMenuItem)aDesign[n]
			IF !oDesign:GetProperty("Enabled"):ValueLogic
				aConstructor:Add(e"\tSELF:DisableItem(" + oDesign:GetVODefine() + ")")
			END IF
			IF oDesign:GetProperty("Checked"):ValueLogic
				aConstructor:Add(e"\tSELF:CheckItem(" + oDesign:GetVODefine() + ")")
			END IF
		NEXT


		aConstructor:Add(e"\tSELF:PostInit()")
		aConstructor:Add("")
		aConstructor:Add(e"\tRETURN")
		
	RETURN oCode


	METHOD Open(cFileName AS STRING) AS LOGIC
		LOCAL lSuccess AS LOGIC
		SELF:cDefaultFileName := cFileName
		lSuccess := SELF:OpenVNmnu(cFileName)
		IF lSuccess
			IF SELF:oMainNode:oDesign:GetProperty("BaseID"):IsAuto
				SELF:oMainNode:oDesign:GetProperty("BaseID"):Value := SELF:CalculateFirstId()
			END IF
			SELF:GiveFocus()
		ENDIF
	RETURN lSuccess

	METHOD Save(cFileName AS STRING , lVnfrmOnly AS LOGIC) AS LOGIC
		LOCAL oRCStream , oRCAccelStream , oPrgStream , oVhStream AS EditorStream
		LOCAL oVNFrmStream AS FileStream
		LOCAL oCode AS CodeContents
		LOCAL cPathToVh AS STRING
		LOCAL lSuccess AS LOGIC

		IF SELF:lReadOnly
			RETURN FALSE
		ENDIF
		
		lVnfrmOnly := lVnfrmOnly .or. SELF:lStandalone
		IF cFileName:ToUpper():Contains("~AUTORECOVER")
			lVnfrmOnly := TRUE // in case it isn't already
		END IF

		oPrgStream := EditorStream{}
		oVhStream := EditorStream{}
		oRcStream := EditorStream{}
		oRcAccelStream := EditorStream{}
		
		oCode := SELF:GetCodeContents()
		IF SELF:GetSaveFileStreams(cFileName , oVNFrmStream , oRCStream , oPrgStream , oVhStream , lVnfrmOnly , oRCAccelStream , cPathToVh)
//			SELF:SaveVNmnu(oVNFrmStream)
			IF .not. SELF:SaveToXml(oVNFrmStream)
				RETURN FALSE
			ENDIF
			IF !lVnfrmOnly
				SELF:SaveRC(oRCStream , oRCAccelStream , oCode , cPathToVh)
				SELF:SavePrg(oPrgStream , oCode)
				SELF:SaveVh(oVhStream , oCode)
			END IF
			lSuccess := TRUE
			IF !lVnfrmOnly
				SELF:nActionSaved := SELF:nAction
			END IF
		ENDIF
	RETURN lSuccess

	PROTECTED METHOD GetSaveFileStreams(cVNFrmFileName AS STRING , oVNFrmStream REF FileStream , ;
								oRCStream AS EditorStream , oPrgStream AS EditorStream , oVhStream AS EditorStream , ;
								lVnfrmOnly AS LOGIC , oRCAccelStream AS EditorStream , cPathToVh REF STRING) AS LOGIC
		LOCAL cRCFileName AS STRING
		LOCAL cRCAccelFileName AS STRING
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
		LOCAL lAccelerators AS LOGIC
		LOCAL lFound AS LOGIC
		
		lAccelerators := SELF:HasAccelerators
		
		TRY

			oFileInfo := FileInfo{cVNFrmFileName}
			oBaseDir := oFileInfo:Directory
			cBaseDir := oBaseDir:FullName
			cBaseName := oFileInfo:Name
			nAt := cBaseName:ToLower():IndexOf(".xsmnu")
			IF nAt != -1
				cBaseName := cBaseName:Substring(0 , nAt)
			ENDIF
			cRCFileName := cBaseDir + "\Resources\" + cBaseName + ".rc"
			cRCAccelFileName := cBaseDir + "\Resources\" + cBaseName + "_Accelerator.rc"
			cAlternative := cBaseDir + "\" + cBaseName + ".rc"
			IF !File.Exists(cRCFileName) .and. File.Exists(cAlternative)
				cRCFileName := cAlternative
				cRCAccelFileName := cBaseDir + "\" + cBaseName + "_Accelerator.rc"
			ENDIF
			
			cVhFileName := cBaseDir + "\GlobalDefines.vh"
			cPathToVh := ""
			TRY
				LOCAL oDirInfo AS DirectoryInfo
				oFileInfo := FileInfo{cRCFileName}
				oDirInfo := oFileInfo:Directory
				DO WHILE oDirInfo != NULL
					IF oDirInfo:GetFiles("GlobalDefines.vh"):Length != 0
						cVhFileName := oDirInfo:FullName + "\GlobalDefines.vh"
						lFound := TRUE
						EXIT
					END IF
					cPathToVh += "..\"
					oDirInfo := oDirInfo:Parent
				END DO
			END TRY
			IF .not. lFound
				cPathToVh := ""
			END IF
						
			cFileName := cBaseName
			nAt := cFileName:LastIndexOf('.')
			IF nAt != -1 // strip out window name
				cFileName := cFileName:Substring(0 , nAt)
			ENDIF
			cPrgFileName := cBaseDir + "\" + cFileName + ".prg"
			cAlternative := oBaseDir:Parent:FullName + "\" + cFileName + ".prg"
			IF !File.Exists(cPrgFileName) .and. File.Exists(cAlternative)
				cPrgFileName := cAlternative
			ENDIF

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
				IF !File.Exists(cVhFileName)
					Funcs.ErrorBox("GlobalDefines .vh File was not found")
					lError := TRUE
				END IF
			END IF
			
			lSuccess := FALSE
			IF !lError
				IF !lVnfrmOnly
					oRCStream:Load(cRCFileName)
					IF lAccelerators
						oRCAccelStream:Load(cRCAccelFileName)
					END IF
					oPrgStream:Load(cPrgFileName)
					oVhStream:Load(cVhFileName)
					lSuccess := oRCStream:IsValid .and. oPrgStream:IsValid .and. oRCStream:IsValid
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
			IF oRCAccelStream:IsValid
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



METHOD UpdateNames() AS VOID
	LOCAL oDesign AS DesignMenuItem
	LOCAL aControls AS ArrayList
//	LOCAL cOrigName AS STRING
	LOCAL cName AS STRING
	LOCAL lFound AS LOGIC
//	LOCAL n,m AS INT
	LOCAL n AS INT
	LOCAL nID AS INT
	
	nId := (INT)SELF:oMainNode:oDesign:GetProperty("BaseID"):Value
	
	aControls := SELF:GetAllDesignItems()
	FOR n := 0 UPTO aControls:Count - 1
		oDesign := (DesignMenuItem)aControls[n]
//		cOrigName := oDesign:Name
		cName := SELF:GetNameFromTree(oDesign:oNode)
		cName := SELF:AdjustName(cName)
		lFound := TRUE
		DO WHILE lFound
			lFound := FALSE
/*			FOR m := 1 UPTO n - 1
				IF ((DesignMenuItem)aControls[m]):Name:ToUpper() == cName:ToUpper()
					lFound := TRUE
					EXIT
				ENDIF
			NEXT*/
			IF lFound
				cName := SELF:GetNextNumName(cName)
			ENDIF
		END DO
		oDesign:cNameID := cName
		
/*		IF (INT)oDesign:GetProperty("MenuID"):Value == 0
			oDesign:GetProperty("MenuID"):Value := SELF:GetNewMenuID()
		END IF*/
		oDesign:GetProperty("MenuID"):Value := nId
		nId ++
		
/*		IF !cName == cOrigName
			oDesign:PropertyValueSelected("Name" , cName)
		ENDIF*/
	NEXT
	
RETURN

INTERNAL METHOD AdjustName(cName AS STRING) AS STRING
	LOCAL cChar AS STRING
	LOCAL cRet AS STRING
	LOCAL nAsc AS DWORD
	LOCAL n AS INT
	cName := cName:Replace("&" , "")
	cName := cName:Replace("." , "")
//	cName := cName:Replace('&' , '')
//	cName := cName:Replace('.' , '')

	cRet := ""
	FOR n := 1 UPTO cName:Length
		cChar := CharPos(cName , (DWORD)n)
		nAsc := DWORD(cChar:ToUpper()[0])
		DO CASE
		CASE (nAsc >= 48 .and. nAsc <= 57)
			IF n == 1
				cRet += "_" + cChar
			ELSE
				cRet += cChar
			ENDIF
		CASE (nAsc >= 65 .and. nAsc <= 90) .or. cChar == "_" .or. nAsc > 128
			cRet += cChar
		OTHERWISE
			cRet += "_"
		END CASE
	NEXT
RETURN SELF:RemoveDiacritics(cRet)

INTERNAL METHOD RemoveDiacritics (s AS STRING) AS STRING
	LOCAL sNormalized AS STRING
	LOCAL sb AS StringBuilder
	LOCAL n  AS INT
	sNormalized := s:Normalize(NormalizationForm.FormD)
	sb := StringBuilder{}
	FOR n := 0 TO sNormalized:Length -1
	   LOCAL cChar := sNormalized[n] AS Char
       IF (CharUnicodeInfo.GetUnicodeCategory(cChar) != UnicodeCategory.NonSpacingMark )
          sb:Append(cChar)
       ENDIF	   
	NEXT
    RETURN sb:ToString()



INTERNAL METHOD GetNextNumName(cName AS STRING) AS STRING
	LOCAL lHasNum AS LOGIC
	LOCAL cRet AS STRING
	LOCAL nValue AS INT
	LOCAL nAsc AS INT
	LOCAL n AS INT
	cRet := ""
	IF cName:Length > 4
		lHasNum := TRUE
		FOR n := cName:Length - 3 UPTO cName:Length
			nAsc := CharPos(cName , (DWORD)n)[0]
			IF !(nAsc >= 48 .and. nAsc <= 57)
				lHasNum := FALSE
				EXIT
			END IF
		NEXT
	ENDIF
	IF !lHasNum
		cRet := cName + "0001"
	ELSE
		cRet := Left(cName , (DWORD)(cName:Length - 4))
//		nValue := Int32.Parse(SubStr(cName , cName:Length - 3))
		nValue := Funcs.Val(SubStr(cName , cName:Length - 3))
		nValue ++
		cRet += nValue:ToString():PadLeft(4 , '0')
	ENDIF
	
RETURN cRet

METHOD GetNameFromTree(oNode AS TreeNode) AS STRING
RETURN SELF:GetNameFromTree(oNode , oNode:Text)
METHOD GetNameFromTree(oNode AS TreeNode , cName AS STRING) AS STRING
	DO WHILE !oNode:Parent == NULL
		oNode := oNode:Parent
		cName := oNode:Text + "_" + cName
//		cName := StrTran(cName , " " , "_")
		cName := cName:Replace(' ' , '_')
	ENDDO
RETURN cName

	STATIC METHOD GetNextNode(oNode AS TreeNode , lIncludeChildren AS LOGIC) AS TreeNode
		LOCAL oParent AS TreeNode
		LOCAL oTree AS TreeView
	
		IF lIncludeChildren .and. oNode:Nodes:Count != 0
			RETURN oNode:Nodes[0]
		ENDIF
	
		oParent := oNode:Parent
		IF oParent == NULL
			oTree := oNode:TreeView
			IF oNode:Index == oTree:Nodes:Count - 1
				RETURN NULL
			ELSE
				RETURN oNode:NextNode
			ENDIF
		ENDIF
	
		IF oNode:Index != oParent:Nodes:Count - 1
			RETURN oNode:NextNode
		ENDIF
	
	RETURN GetNextNode(oParent , FALSE)

END CLASS



CLASS MEditorTreeView INHERIT TreeView
	PROTECT oDragNode AS DesignTreeNode
	PROTECT nDragX,nDragY AS INT

	EXPORT oEditor AS VOMenuEditor
	EXPORT oMEditor AS VOMenuEditor

	CONSTRUCTOR(_oEditor AS VOMenuEditor)
		SUPER()
		SELF:oEditor := _oEditor
		SELF:oMEditor := _oEditor
		SELF:Dock := DockStyle.Fill
		SELF:LabelEdit := FALSE
		SELF:HideSelection := FALSE
		SELF:ContextMenu := ContextMenu{}
		SELF:ContextMenu:Popup += EventHandler{SELF , @TreeViewContextMenuPopUp()}
	RETURN

	METHOD SelectAll() AS VOID
/*		LOCAL oNode AS TreeNode
		SELF:ExpandAll()
		oNode := SELF:Nodes[0]
		IF oNode:Nodes:Count == 0
			RETURN
		ENDIF
		IF SELF:SelectedNode == oNode
			SELF:SelectedNode := oNode:NextVisibleNode
		ENDIF
		oNode := oNode:NextVisibleNode
		DO WHILE oNode != NULL
			((DesignTreeNode)oNode):lSelected := TRUE
			SELF:SetNodeColor((DesignTreeNode)oNode)
			oNode := oNode:NextVisibleNode
		ENDDO
		SELF:oEditor:DisplayProperties()*/
	RETURN
	METHOD DeSelectAll() AS VOID
/*		LOCAL oNode AS TreeNode
		oNode := SELF:Nodes[0]
		oNode := oNode:NextVisibleNode
		DO WHILE oNode != NULL
			IF ((DesignTreeNode)oNode):lSelected
				((DesignTreeNode)oNode):lSelected := FALSE
				SELF:SetNodeColor((DesignTreeNode)oNode)
			ENDIF
			oNode := oNode:NextVisibleNode
		ENDDO*/
	RETURN

	PROTECTED METHOD OnMouseDown(e AS MouseEventArgs) AS VOID
		LOCAL oNode AS DesignTreeNode
		SELF:LabelEdit:=FALSE
		SUPER:OnMouseDown(e)
		SELF:nDragX:=0
		SELF:nDragY:=0
		oNode:=(DesignTreeNode)SELF:GetNodeAt(e:X,e:Y)
		DO CASE
		CASE e:Button==MouseButtons.Right .and. oNode!=NULL
			SELF:SelectedNode := oNode
		CASE e:Clicks==1 .and. e:Button==MouseButtons.Left .and. oNode!=NULL
			SELF:SelectedNode:=oNode
			IF SELF:SelectedNode!=NULL .and. SELF:SelectedNode:Parent!=NULL
				SELF:nDragX:=e:X
				SELF:nDragY:=e:Y
				SELF:oDragNode := (DesignTreeNode)SELF:SelectedNode
			END IF
			
		END CASE
	RETURN
	PROTECTED METHOD OnMouseMove(e AS MouseEventArgs) AS VOID
		SUPER:OnMouseMove(e)
		IF Control.MouseButtons==MouseButtons.Left
			IF SELF:nDragX!=0
				IF Math.Abs(SELF:nDragX - e:X ) > 2 .or. Math.Abs(SELF:nDragY - e:Y) > 2
					SELF:AllowDrop:=TRUE
					SELF:DoDragDrop(0 , DragDropEffects.Move)
				END IF
			END IF
		END IF
	RETURN
	PROTECTED METHOD OnMouseUp(e AS MouseEventArgs) AS VOID
		SUPER:OnMouseUp(e)
		SELF:nDragX := 0
		SELF:nDragY := 0
		SELF:oDragNode := NULL
		SELF:AllowDrop := FALSE
	RETURN
	PROTECTED METHOD OnDragOver(e AS DragEventArgs) AS VOID
		LOCAL oNode AS DesignTreeNode
		LOCAL oTest AS TreeNode
		
		SUPER:OnDragOver(e)
		e:Effect:=DragDropEffects.None
		oNode:=(DesignTreeNode)SELF:GetNodeAt ( SELF:PointToClient(Point{e:X,e:Y}) )
		IF oNode != NULL .and. oNode:Parent != NULL .and. SELF:oDragNode != NULL
			SELF:SelectedNode := oNode
			e:Effect:=DragDropEffects.Move
			
			oTest := oNode
			DO WHILE oTest:Parent != NULL
				oTest := oTest:Parent
				IF oTest == SELF:oDragNode
					e:Effect := DragDropEffects.None
					RETURN
				ENDIF
			ENDDO
			
		END IF
	RETURN
	PROTECTED METHOD OnDragDrop(e AS DragEventArgs) AS VOID
		LOCAL oNode AS DesignTreeNode
		LOCAL oTest AS TreeNode
		LOCAL lUp AS LOGIC
		
		SUPER:OnDragDrop(e)
		oNode:=(DesignTreeNode)SELF:GetNodeAt ( SELF:PointToClient(Point{e:X,e:Y}) )
		IF SELF:oDragNode!=NULL .and. oNode!=NULL .and. SELF:oDragNode!=oNode
			
			oTest := oNode
			DO WHILE oTest:Parent != NULL
				oTest := oTest:Parent
				IF oTest == SELF:oDragNode
					SELF:oDragNode:=NULL
					SELF:AllowDrop:=FALSE
					RETURN
				ENDIF
			ENDDO
			
			lUp := SELF:oDragNode:Bounds:Y < oNode:Bounds:Y
/*			SELF:oDragNode:Parent:Nodes:Remove(SELF:oDragNode)
			IF lUp
				oNode:Parent:Nodes:Insert(oNode:Index+1,SELF:oDragNode)
			ELSE
				oNode:Parent:Nodes:Insert(oNode:Index,SELF:oDragNode)
			ENDIF*/
			IF lUp
				SELF:oEditor:StartAction(DesignerBasicActionType.SetParent , ActionData{SELF:oDragNode:oDesign:cGuid , ((DesignTreeNode)oNode:Parent):oDesign:cGuid , oNode:Index + 1})
			ELSE
				SELF:oEditor:StartAction(DesignerBasicActionType.SetParent , ActionData{SELF:oDragNode:oDesign:cGuid , ((DesignTreeNode)oNode:Parent):oDesign:cGuid , oNode:Index})
			ENDIF
			SELF:DeSelectAll()
			SELF:SelectedNode := SELF:oDragNode
			SELF:oDragNode := NULL
			SELF:AllowDrop := FALSE
		END IF
	RETURN
	
	PROTECTED METHOD OnKeyPress(e AS KeyPressEventArgs) AS VOID
		IF e:KeyChar == '\r'
			e:Handled := TRUE
		END IF
	RETURN

	PROTECTED METHOD ProcessDialogKey(KeyData AS Keys) AS LOGIC
		SWITCH KeyData
		CASE Keys.Insert
			SELF:InsertNode()
			RETURN TRUE
		CASE Keys.Tab
			SELF:TabIt()
			RETURN TRUE
		CASE Keys.Tab + Keys.Shift
			SELF:UnTabIt()
			RETURN TRUE
		END SWITCH
	RETURN FALSE
	
	PROTECTED METHOD OnKeyDown(e AS KeyEventArgs) AS VOID
		SUPER:OnKeyDown(e)
		
		IF SELF:oEditor:StandAlone
			DO CASE
			CASE e:KeyCode==Keys.Z .and. e:Modifiers==Keys.Control
				SELF:oEditor:Undo()
			CASE e:KeyCode==Keys.Z .and. e:Modifiers==Keys.Control + Keys.Shift
				SELF:oEditor:Redo()
				
			CASE e:KeyCode==Keys.X .and. e:Modifiers==Keys.Control
				SELF:oEditor:Cut()
			CASE e:KeyCode==Keys.C .and. e:Modifiers==Keys.Control
				SELF:oEditor:Copy()
			CASE e:KeyCode==Keys.V .and. e:Modifiers==Keys.Control
				SELF:oEditor:Paste()
			END CASE
		END IF
		
		DO CASE
		CASE e:KeyCode==Keys.Enter .and. e:Modifiers==Keys.None
			SELF:AddNode()
		CASE e:KeyCode==Keys.Enter .and. e:Modifiers==Keys.Shift
			SELF:AddSubNode()
		CASE e:KeyCode==Keys.Delete
			SELF:DoRemove()
		CASE e:KeyCode==Keys.F2 .and. SELF:SelectedNode:Parent!=NULL
			SELF:DoEdit()
/*		CASE e:KeyCode==Keys.A .and. e:Modifiers==Keys.Control
			SELF:SelectAll()*/
		END CASE
	RETURN

	METHOD DoEdit() AS VOID
		SELF:DeSelectAll()
		IF SELF:SelectedNode:Parent != NULL
			SELF:LabelEdit := TRUE
			SELF:SelectedNode:BeginEdit()
		ENDIF
	RETURN
	METHOD DoRemove() AS VOID
		IF SELF:SelectedNode:Parent != NULL
			SELF:oEditor:DoRemove()
		ENDIF
	RETURN
	
/*	METHOD DoMove(lUp AS LOGIC) AS VOID
		LOCAL oNode,oParent AS TreeNode
		LOCAL nIndex AS INT
		
		oNode := SELF:SelectedNode
		IF oNode == NULL .or. oNode:Parent == NULL
			RETURN
		ENDIF
		oParent := oNode:Parent
		nIndex := oNode:Index
		IF (lUp .and. nIndex == 0) .or. (!lUp .and. nIndex == oParent:Nodes:Count - 1)
			RETURN
		ENDIF
		
		oParent:Nodes:Remove(oNode)
		IF lUp
			oParent:Nodes:Insert(nIndex - 1 , oNode)
		ELSE
			oParent:Nodes:Insert(nIndex + 1 , oNode)
		ENDIF
		SELF:SelectedNode := oNode
		
	RETURN*/
/*
	METHOD CanDoAction(eAction AS VOWEDActionType) AS LOGIC
		LOCAL oParent,oNode AS DesignTreeNode
		LOCAL nIndex AS INT
		oNode := (DesignTreeNode)SELF:SelectedNode
		nIndex := oNode:Index
		IF oNode != NULL
			oParent := (DesignTreeNode)oNode:Parent
		ENDIF
	
		DO CASE
		CASE eAction == VOWEDActionType.Promote
			RETURN nIndex != 0 .and. oParent != NULL
		CASE eAction == VOWEDActionType.Demote
			RETURN oParent != NULL .and. oParent:Parent != NULL
		CASE eAction == VOWEDActionType.Add
			RETURN TRUE
		CASE eAction == VOWEDActionType.AddSub
			RETURN TRUE
		CASE eAction == VOWEDActionType.Insert
			RETURN oParent != NULL
		CASE eAction == VOWEDActionType.Edit
			RETURN oNode != NULL .and. oParent != NULL
		CASE eAction == VOWEDActionType.Remove
			RETURN oNode != NULL .and. oParent != NULL
		CASE eAction == VOWEDActionType.MoveUp
			RETURN oNode != NULL .and. oParent != NULL .and. oNode:Index != 0
		CASE eAction == VOWEDActionType.MoveDown
			RETURN oNode != NULL .and. oParent != NULL .and. oNode:Index != oParent:Nodes:Count - 1
		END CASE
	RETURN TRUE
	
*/
	METHOD CreateNewNode() AS DesignTreeNode
		LOCAL oNode AS DesignTreeNode
		oNode := DesignTreeNode{0 , SELF:oMEditor}
	RETURN oNode
			
	METHOD AddNode() AS VOID
		LOCAL oParent AS DesignTreeNode
		LOCAL nIndex AS INT
		oParent := (DesignTreeNode)SELF:SelectedNode:Parent
		nIndex := SELF:SelectedNode:Index
		SELF:DeSelectAll()
		IF oParent == NULL
			oParent := (DesignTreeNode)SELF:Nodes[0]
			nIndex := oParent:Nodes:Count - 1
		END IF
		SELF:oEditor:DoCreate(oParent , nIndex + 1)
	RETURN

	METHOD AddSubNode() AS VOID
		LOCAL oParent AS DesignTreeNode
		oParent := (DesignTreeNode)SELF:SelectedNode
		SELF:DeSelectAll()
		IF oParent == NULL
			RETURN
		END IF
		SELF:oEditor:DoCreate(oParent , 0)
	RETURN

	METHOD InsertNode() AS VOID
		LOCAL oParent AS DesignTreeNode
		oParent := (DesignTreeNode)SELF:SelectedNode:Parent
		SELF:DeSelectAll()
		IF oParent == NULL
			RETURN
		END IF
		SELF:oEditor:DoCreate(oParent , SELF:SelectedNode:Index)
	RETURN

	METHOD TabIt() AS VOID
		LOCAL oParent,oNode AS DesignTreeNode
		LOCAL nIndex AS INT
		oNode := (DesignTreeNode)SELF:SelectedNode
		nIndex := oNode:Index
		IF nIndex == 0
			RETURN
		END IF
		oParent := (DesignTreeNode)oNode:Parent
		IF oParent == NULL
			RETURN
		END IF
//		oParent:Nodes:Remove(oNode)
		oParent := (DesignTreeNode)oParent:Nodes[nIndex-1]
//		oParent:Nodes:Add(oNode)
		SELF:oEditor:StartAction(DesignerBasicActionType.SetParent , ActionData{oNode:oDesign:cGuid , oParent:oDesign:cGuid , oParent:Nodes:Count})
		oNode:EnsureVisible()
		SELF:SelectedNode := oNode
		SELF:DeSelectAll()
	RETURN
	
	METHOD UnTabIt() AS VOID
		LOCAL oParent , oNode AS DesignTreeNode
		LOCAL nIndex AS INT
		oNode := (DesignTreeNode)SELF:SelectedNode
		oParent := (DesignTreeNode)oNode:Parent
		IF oParent == NULL
			RETURN
		END IF
		IF oParent:Parent == NULL
			RETURN
		END IF
		nIndex := oParent:Index
//		oParent:Nodes:Remove(oNode)
		oParent := (DesignTreeNode)oParent:Parent
//		oParent:Nodes:Insert(nIndex + 1 , oNode)
		SELF:oEditor:StartAction(DesignerBasicActionType.SetParent , ActionData{oNode:oDesign:cGuid , oParent:oDesign:cGuid , nIndex + 1})
		oNode:EnsureVisible()
		SELF:SelectedNode:=oNode
		SELF:DeSelectAll()
	RETURN

	PROTECTED METHOD OnAfterLabelEdit(e AS NodeLabelEditEventArgs) AS VOID
		SUPER:OnAfterLabelEdit(e)
		IF e:Label == NULL .or. e:Label:Trim():Length == 0
			e:CancelEdit := TRUE
		ELSE
			SELF:oEditor:SetCaption(((DesignTreeNode)e:Node):oDesign , e:Label)
		END IF
		SELF:LabelEdit := FALSE
	RETURN

	PROTECTED METHOD OnAfterSelect(e AS TreeViewEventArgs) AS VOID
		LOCAL oNode AS DesignTreeNode
		SUPER:OnAfterSelect(e)
		oNode := (DesignTreeNode)SELF:SelectedNode
		IF !(Control.ModifierKeys == Keys.Shift) .or. oNode:Parent == NULL
			SELF:DeSelectAll()
		ENDIF
/*		IF oNode:Parent != NULL
			oNode:lSelected := TRUE
			SELF:SetNodeColor(oNode)
		ENDIF*/
		SELF:oMEditor:DisplayProperties()
	RETURN

	METHOD SetNodeColor(oNode AS DesignTreeNode) AS VOID
/*	IF oNode:lSelected
		oNode:BackColor := SystemColors.Highlight
		oNode:ForeColor := Color.LightGray
	ELSE*/
		IF oNode:oDesign:GetProperty("Enabled"):ValueLogic
			oNode:BackColor := Color.White
			oNode:ForeColor := Color.Black
		ELSE
			oNode:BackColor := Color.White
			oNode:ForeColor := Color.Gray
		ENDIF
//	ENDIF
	RETURN
	
	METHOD TreeViewContextMenuPopUp(o AS OBJECT,e AS EventArgs) AS VOID
		LOCAL oItem AS MenuItem
		LOCAL oNode AS DesignTreeNode
		LOCAL lMultiple AS LOGIC
		
		SELF:ContextMenu:MenuItems:Clear()
		
		oNode := (DesignTreeNode)SELF:SelectedNode
		IF oNode == NULL
			RETURN
		END IF
		
//		lMultiple := SELF:oMEditor:GetSelected():Count > 1
		lMultiple := FALSE
		
		IF !lMultiple .and. oNode:Parent != NULL
			oItem := MenuItem{"Cut" + ChrW(9) + "CTRL+X"}
			oItem:Click += EventHandler{SELF , @ContextCut()}
			SELF:ContextMenu:MenuItems:Add(oItem)
		END IF
		IF !lMultiple
			oItem := MenuItem{"Copy" + ChrW(9) + "CTRL+C"}
			oItem:Click += EventHandler{SELF , @ContextCopy()}
			SELF:ContextMenu:MenuItems:Add(oItem)
		END IF
		IF VOMenuEditor.Clipboard:Count != 0
			oItem := MenuItem{"Paste" + ChrW(9) + "CTRL+V"}
			oItem:Click += EventHandler{SELF , @ContextPaste()}
			SELF:ContextMenu:MenuItems:Add(oItem)
		END IF
		IF SELF:ContextMenu:MenuItems:Count != 0
			SELF:ContextMenu:MenuItems:Add("-")
		END IF
		
		IF oNode:Parent == NULL
			oItem:=MenuItem{"Add Subitem" + ChrW(9) + "Shift+Enter"}
			oItem:Click+=EventHandler{SELF,@ContextAddSub()}
			SELF:ContextMenu:MenuItems:Add(oItem)
			RETURN
		ENDIF
		
		oItem:=MenuItem{e"Promote\tTab"}
		oItem:Click+=EventHandler{SELF,@ContextPromote()}
		IF oNode:Index == 0
			oItem:Enabled := FALSE
		ENDIF
		SELF:ContextMenu:MenuItems:Add(oItem)
		oItem:=MenuItem{e"Demote\tShift+Tab"}
		oItem:Click+=EventHandler{SELF,@ContextDemote()}
		IF oNode:Parent == NULL .or. oNode:Parent:Parent == NULL
			oItem:Enabled := FALSE
		ENDIF
		SELF:ContextMenu:MenuItems:Add(oItem)
		
		SELF:ContextMenu:MenuItems:Add("-")
		
		oItem:=MenuItem{e"Remove\tDel"}
		oItem:Click+=EventHandler{SELF,@ContextRemove()}
		SELF:ContextMenu:MenuItems:Add(oItem)
		
		SELF:ContextMenu:MenuItems:Add("-")
		oItem:=MenuItem{e"Add Subitem\tShift+Enter"}
		oItem:Click+=EventHandler{SELF,@ContextAddSub()}
		SELF:ContextMenu:MenuItems:Add(oItem)
		oItem:=MenuItem{e"Add Item\tEnter"}
		oItem:Click+=EventHandler{SELF,@ContextAdd()}
		SELF:ContextMenu:MenuItems:Add(oItem)
		
		oItem:=MenuItem{e"Insert Item\tInsert"}
		oItem:Click+=EventHandler{SELF,@ContextInsert()}
		SELF:ContextMenu:MenuItems:Add(oItem)
		
	RETURN
	
	METHOD ContextPromote(o AS OBJECT,e AS EventArgs) AS VOID
		SELF:TabIt()
	RETURN
	METHOD ContextDemote(o AS OBJECT,e AS EventArgs) AS VOID
		SELF:UnTabIt()
	RETURN
	METHOD ContextRemove(o AS OBJECT,e AS EventArgs) AS VOID
		SELF:DoRemove()
	RETURN
	METHOD ContextAdd(o AS OBJECT,e AS EventArgs) AS VOID
		SELF:AddNode()
	RETURN
	METHOD ContextAddSub(o AS OBJECT,e AS EventArgs) AS VOID
		SELF:AddSubNode()
	RETURN
	METHOD ContextInsert(o AS OBJECT,e AS EventArgs) AS VOID
		SELF:InsertNode()
	RETURN
	
	METHOD ContextCut(o AS OBJECT,e AS EventArgs) AS VOID
		SELF:oEditor:Cut()
	RETURN
	METHOD ContextCopy(o AS OBJECT,e AS EventArgs) AS VOID
		SELF:oEditor:Copy()
	RETURN
	METHOD ContextPaste(o AS OBJECT,e AS EventArgs) AS VOID
		SELF:oEditor:Paste()
	RETURN

END CLASS



CLASS DesignTreeNode INHERIT TreeNode
	EXPORT oDesign AS DesignMenuItem
//	EXPORT lSelected AS LOGIC
	EXPORT nType AS INT
	CONSTRUCTOR(_nType AS INT , _oMEditor AS VOMenuEditor)
		SUPER()
		SELF:nType := _nType
		SELF:oDesign := DesignMenuItem{SELF:nType , SELF , _oMEditor}
	RETURN
END CLASS



CLASS DesignMenuItem INHERIT DesignItem
	EXPORT oNode AS DesignTreeNode
	EXPORT nType AS INT
	EXPORT lDeleted AS LOGIC
	PROTECT oEditor AS VOMenuEditor
	
	EXPORT cNameID AS STRING
	CONSTRUCTOR(_nType AS INT , _oNode AS DesignTreeNode , _oEditor AS VOMenuEditor)

		SUPER(_oEditor)

		LOCAL oProp AS VODesignProperty
		
		SELF:oEditor := _oEditor
		SELF:oNode := _oNode
		SELF:nType := _nType
		
		SELF:aPages := List<STRING>{}
		SELF:aPages:Add("General")

		IF nType==999

			SELF:AddProperty(VODesignProperty{"Name","Name","Name",PropertyType.Text})
			SELF:AddProperty(VODesignProperty{"Inherit","Inherit from","Inherit",PropertyType.Text})
			SELF:AddProperty(VODesignProperty{"ToolbarInherit","Toolbar inherit from","ToolbarInherit",PropertyType.Text})
			oProp := VODesignProperty{"Toolbar","Toolbar style","Toolbar","__menuTOOLBARSTYLE"}
			oProp:lNoAuto := TRUE
			SELF:AddProperty(oProp)
			oProp := VODesignProperty{"Show","Show","Show","__menuSHOW"}
			oProp:lNoAuto := TRUE
			SELF:AddProperty(oProp)
			SELF:AddProperty(VODesignProperty{"UseBands","Use Bands","UseBands","YESNO"})
			SELF:AddProperty(VODesignProperty{"BaseID","BaseID","BaseID",PropertyType.Numeric})
			
		ELSE
			
			SELF:AddProperty(VODesignProperty{"EventName","Event Name","EventName",PropertyType.Text,PropertyStyles.NoAuto})
			SELF:AddProperty(VODesignProperty{"Caption","Caption","Caption",PropertyType.Text})
			SELF:AddProperty(VODesignProperty{"Description","Description","Description",PropertyType.Text , PropertyStyles.NoAuto})
			SELF:AddProperty(VODesignProperty{"HelpContext","HelpContext","HelpContext",PropertyType.Text , PropertyStyles.NoAuto})
			oProp := VODesignProperty{"Accelerator","Accelerator","Accelerator",PropertyType.Text}
			oProp:cSpecialClass := "__MenuAccelerator"
			oProp:Value := MenuAccelerator{"" , FALSE , FALSE , FALSE}
			SELF:AddProperty(oProp)
			SELF:AddProperty(VODesignProperty{"Enabled","Init. Enabled","Enabled","YESNO"})
			oProp := VODesignProperty{"Checked","Init. Checked","Checked","YESNO"}
			oProp:Value := 1
			SELF:AddProperty(oProp)
			SELF:AddProperty(VODesignProperty{"ButtonBmp","Button Bmp","ButtonBmp","__menuToolBarButtons" , PropertyStyles.NoAuto})
			SELF:AddProperty(VODesignProperty{"ButtonCaption","Button Caption","ButtonCaption",PropertyType.Text , PropertyStyles.NoAuto})
			SELF:AddProperty(VODesignProperty{"ButtonToolTip","Button ToolTip","ButtonToolTip",PropertyType.Text , PropertyStyles.NoAuto})
			SELF:AddProperty(VODesignProperty{"ButtonPos","Button Pos","ButtonPos",PropertyType.Numeric , PropertyStyles.NoAuto})
			SELF:AddProperty(VODesignProperty{"ID","ID","ID",PropertyType.Text})
			oProp := VODesignProperty{"MenuID","MenuID","MenuID",PropertyType.Numeric}
			IF !SELF:oDesigner:StandAlone
				oProp:cPage := "__Hidden"
			END IF
			SELF:AddProperty(oProp)
			
		END IF
		
	RETURN
	VIRTUAL METHOD GetThumb() AS Bitmap
	RETURN NULL
	VIRTUAL METHOD GetBitmap() AS Bitmap
	RETURN NULL

	METHOD AllowPropertyUpdate(oProp AS VODesignProperty , oValue AS OBJECT) AS LOGIC
	RETURN TRUE

	METHOD PropertyValueSelected(cProp AS STRING , uValue AS OBJECT) AS VOID
		LOCAL oProp AS VODesignProperty
	    oProp := SELF:GetProperty(cProp)
	    IF oProp != NULL
	    	IF SELF:AllowPropertyUpdate(oProp , uValue)
		    	oProp:Value := uValue
		    	SELF:PropertyGotUpdated(oProp)
	    	ENDIF
	    ENDIF
	RETURN
	METHOD PropertyGotUpdated(oProp AS VODesignProperty) AS VOID
		SELF:oEditor:PropertyGotUpdated(SELF , oProp)
	RETURN
	ACCESS Name() AS STRING
	RETURN SELF:GetProperty("Name"):Value:ToString()

	ACCESS IsSeparator() AS LOGIC
		LOCAL cCaption AS STRING
		LOCAL n AS INT
		cCaption := SELF:GetProperty("Caption"):TextValue
		FOR n := 0 UPTO cCaption:Length - 1
			IF cCaption[n] != '-'
				RETURN FALSE
			ENDIF
		NEXT
	RETURN TRUE
	
	METHOD GetMenuID() AS STRING
	RETURN SELF:GetProperty("MenuID"):TextValue
	METHOD GetVODefine() AS STRING
		LOCAL cDefine AS STRING
		cDefine := "IDM_"
//		cDefine += SELF:Name
		cDefine += SELF:cNameID
		IF SELF:oNode:Parent != NULL
			cDefine += "_ID"
		ENDIF
//		cDefine := StrTran(cDefine , " " , "_")
//		cDefine := cDefine:Replace(' ' , '_')
/*		IF SELF:GetProperty("MenuID"):TextValue:Trim():Length == 0
			SELF:GetProperty("MenuID"):Value := "30001"
		END IF*/
		
		LOCAL n AS INT
		LOCAL cTemp AS STRING
		LOCAL cChar AS Char
		cTemp := ""
		FOR n := 0 UPTO cDefine:Length - 1
			cChar := cDefine[n]
			IF cChar > 127 //.and. cChar:ToString():ToUpper() == cChar:ToString()
				cTemp += "_" + ((INT)cChar):ToString() + "_"
			ELSE
				cTemp += cChar:ToString()
			END IF
		NEXT
		cDefine := cTemp
		
	RETURN cDefine
	
	METHOD GetNestLevel() AS INT
		LOCAL oNode AS TreeNode
		LOCAL nLevel AS INT
		oNode := SELF:oNode
		DO WHILE !oNode:Parent == NULL
			oNode := oNode:Parent
			nLevel ++
		END DO
	RETURN nLevel
	
END CLASS



INTERNAL CLASS VOTBButton
	EXPORT cIdt AS STRING
	EXPORT cCaption AS STRING
	EXPORT cToolTip AS STRING
	EXPORT lSeparator AS LOGIC
	EXPORT lLastSeparator AS LOGIC
	EXPORT nIcons AS INT
	CONSTRUCTOR()
		SELF:lSeparator := TRUE
	RETURN
	CONSTRUCTOR(_cIdt AS STRING , _cCaption AS STRING , _cToolTip AS STRING)
		SELF:cIdt := _cIdt    
		SELF:cCaption := _cCaption
		SELF:cToolTip := _cToolTip
	RETURN
END CLASS

