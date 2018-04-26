#using System.Windows.Forms
#using System.Drawing
#using System.Collections
#using System.Collections.Generic

#using System.IO
#using System.Text


PARTIAL CLASS VOFieldSpecEditor INHERIT DesignerBase
	PROTECT cLoadedDir AS STRING
	PROTECT oListView AS FSEListView
	EXPORT lLoadedAsXml AS LOGIC

	PROTECT aFilesToDelete AS List<STRING>

	PROTECT aUsedFieldSpecNames AS List<STRING>

	STATIC PROTECT lEditorCloseWarningShown AS LOGIC
	STATIC PROTECT _Template AS FieldSpecCode
	STATIC ACCESS Template AS FieldSpecCode
	RETURN VOFieldSpecEditor._Template
	STATIC METHOD LoadTemplate(cLoadedDir AS STRING) AS LOGIC
		IF VOFieldSpecEditor._Template == NULL
			VOFieldSpecEditor._Template := FieldSpecCode{}
		ENDIF
	RETURN VOFieldSpecEditor._Template:Read(cLoadedDir)

	CONSTRUCTOR(_oSurface AS Control , _oGrid AS DesignerGrid)
		SUPER(_oSurface)
		
		SELF:oGrid := _oGrid
		SELF:oGrid:PropertyModified := PropertyUpdatedEventHandler{ SELF , @PropertyModifiedInGrid() }
		SELF:oGrid:ControlKeyPressed := ControlKeyPressedEventHandler{ SELF , @ControlKeyPressedInGrid() }
		SELF:oGrid:oActiveDesigner := SELF
		SELF:oGrid:UseHierarchy(FALSE)

		SELF:aActions := ArrayList{}
		SELF:aUsedFieldSpecNames := List<STRING>{}

		SELF:oListView := FSEListView{SELF}
		SELF:oListView:Dock := DockStyle.Fill
		SELF:oListView:GridLines := TRUE
		SELF:oListView:HideSelection := FALSE
//		SELF:oListView:MultiSelect := TRUE
		SELF:oListView:MultiSelect := FALSE
		SELF:oListView:FullRowSelect := TRUE
		SELF:oListView:HeaderStyle := ColumnHeaderStyle.Nonclickable
		SELF:oListView:Columns:Add("FieldSpec",240,HorizontalAlignment.Left)
		SELF:oListView:Columns:Add("Type",100,HorizontalAlignment.Left)
		SELF:oListView:Columns:Add("Len",80,HorizontalAlignment.Right)
		SELF:oListView:Columns:Add("Dec",50,HorizontalAlignment.Right)
		SELF:oListView:Columns:Add("Picture",160,HorizontalAlignment.Right)
		SELF:oListView:Columns:Add("MinLen",60,HorizontalAlignment.Right)
		SELF:oListView:Columns:Add("Required",60,HorizontalAlignment.Right)
		SELF:oListView:Columns:Add("Validation",160,HorizontalAlignment.Left)
		SELF:oListView:View := View.Details
		SELF:oListView:Sorting := SortOrder.Ascending
//		SELF:oListView:GotFocus += EventHandler{SELF , @GotFocusHandler()}
		SELF:oListView:Show()

		SELF:oListView:GotFocus += EventHandler{ SELF , @ListGotFocus() }
		SELF:oListView:LostFocus += EventHandler{ SELF , @ListLostFocus() }
		
		SELF:aFilesToDelete := List<STRING>{}

		SELF:oTimer:Start()

	RETURN

	VIRTUAL METHOD Cut() AS VOID
	VIRTUAL METHOD Copy() AS VOID
	VIRTUAL METHOD Paste() AS VOID

	ACCESS IsDirty AS LOGIC
	RETURN SELF:nAction != SELF:nActionSaved
		
	VIRTUAL METHOD GiveFocus() AS VOID
		SELF:oListView:Focus()
	RETURN
    METHOD ListGotFocus(o AS OBJECT , e AS EventArgs) AS VOID
    	SELF:ShowHideTools(TRUE)
    	IF SELF:oGrid:FindForm() != NULL
	    	SELF:oGrid:FindForm():Text := "FieldSpec Editor Properties" // TODO: need to use resource for that
    	END IF
		SELF:oGrid:PropertyModified := PropertyUpdatedEventHandler{ SELF , @PropertyModifiedInGrid() }
		SELF:oGrid:ControlKeyPressed := ControlKeyPressedEventHandler{ SELF , @ControlKeyPressedInGrid() }
		SELF:oGrid:oActiveDesigner := SELF
		SELF:oGrid:UseHierarchy(FALSE)
		SELF:DisplayProperties()
    RETURN
    METHOD ListLostFocus(o AS OBJECT , e AS EventArgs) AS VOID
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

	METHOD LoadUsedFieldSpecNames() AS VOID
		LOCAL aFieldSpecs AS FSEDesignFieldSpec[]
		LOCAL n AS INT
		
		SELF:aUsedFieldSpecNames:Clear()
		aFieldSpecs := SELF:GetFieldSpecs()
		FOR n := 1 UPTO aFieldSpecs:Length
			SELF:aUsedFieldSpecNames:Add(aFieldSpecs[n]:Name:ToUpper())
		NEXT
	RETURN

	METHOD GetFieldSpecs() AS FSEDesignFieldSpec[]
		LOCAL oItem AS FSEDesignListViewItem
		LOCAL aRet AS FSEDesignFieldSpec[]
		LOCAL n AS INT
		aRet := FSEDesignFieldSpec[]{SELF:oListView:Items:Count}
		FOR n := 0 UPTO SELF:oListView:Items:Count - 1
			oItem := (FSEDesignListViewItem)SELF:oListView:Items[n]
			aRet[n+1] := oItem:oDesign
		NEXT
	RETURN aRet
	
	METHOD GetSelected() AS ArrayList
		LOCAL oItem AS FSEDesignListViewItem
		LOCAL aRet := ArrayList{} AS ArrayList
		LOCAL n AS INT
		FOR n := 0 UPTO SELF:oListView:Items:Count - 1
			oItem := (FSEDesignListViewItem)SELF:oListView:Items[n]
			IF oItem:Selected
				aRet:Add(oItem:oDesign)
			ENDIF
		NEXT
	RETURN aRet

	METHOD GetAllDesignItems() AS ArrayList
		LOCAL oItem AS FSEDesignListViewItem
		LOCAL aRet := ArrayList{} AS ArrayList
		LOCAL n AS INT
		FOR n := 0 UPTO SELF:oListView:Items:Count - 1
			oItem := (FSEDesignListViewItem)SELF:oListView:Items[n]
			aRet:Add(oItem:oDesign)
		NEXT
	RETURN aRet

	METHOD PropertyModifiedInGrid(cProp AS STRING , oValue AS OBJECT) AS VOID
		LOCAL aSelected AS ArrayList
		LOCAL oDesign AS FSEDesignFieldSpec
		LOCAL n AS INT
		aSelected := SELF:GetSelected()
		SELF:BeginAction()
		FOR n := 0 UPTO aSelected:Count - 1
			oDesign := (FSEDesignFieldSpec)aSelected[n]
//			oDesign:PropertyValueSelected(cProp , oValue)
			SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{oDesign:cGuid , cProp , oValue})
			IF cProp == "type"
				LOCAL nLength,nDecimal AS INT
				SWITCH  (INT)oValue 
				CASE 0
					nLength := 10
					nDecimal := 0
				CASE 1
					nLength := 12
					nDecimal := 2
				CASE 2
					nLength := 8
					nDecimal := 0
				CASE 3
					nLength := 1
					nDecimal := 0
				CASE 4
					nLength := 10
					nDecimal := 0
				CASE 5
					nLength := 10
					nDecimal := 0
				CASE 6
					nLength := 8
					nDecimal := 0
				END SWITCH
				SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{oDesign:cGuid , "Len" , nLength})
				SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{oDesign:cGuid , "Dec" , nDecimal})
				oDesign:GetProperty("Len"):lReadOnly := (INT)oValue > 1
				oDesign:GetProperty("Dec"):lReadOnly := (INT)oValue != 1
			END IF
		NEXT
		SELF:EndAction()
	RETURN

	METHOD DisplayProperties() AS VOID
		SELF:oGrid:Fill(SELF:GetSelected())
	RETURN

	METHOD PropertyGotUpdated(oDesign AS FSEDesignFieldSpec , oProp AS VODesignProperty) AS VOID
		SELF:ApplyProperty(oDesign , oProp)
		oDesign:lModified := TRUE
		oDesign:oItem:ForeColor := Color.Red
	RETURN
	
	METHOD ApplyProperty(_oDesign AS FSEDesignFieldSpec , oProp AS VODesignProperty) AS VOID
		LOCAL oDesign AS FSEDesignFieldSpec
		oDesign :=(FSEDesignFieldSpec)_oDesign
		
		switch oProp:Name 
        case "classname" 
        case "type" 
        case "dec" 
        case "len" 
	    case "picture" 
        case "required" 
        case "minlen" 
        case "validation"
			oDesign:oItem:SetValues()
		end switch
		
	RETURN
	
	METHOD ResetModified() AS VOID
		LOCAL oDesign AS FSEDesignFieldSpec
		LOCAL n AS INT
		FOR n := 0 UPTO SELF:oListView:Items:Count - 1
			oDesign := ((FSEDesignListViewItem)SELF:oListView:Items[n]):oDesign
			oDesign:lModified := FALSE
			oDesign:oItem:ForeColor := Color.Black
		NEXT
	RETURN

	METHOD CanDoAction(eAction AS DesignerActionType) AS LOGIC
		
		IF SELF:lReadOnly
			RETURN FALSE
		ENDIF
		
		DO CASE
/*		CASE eAction == DesignerActionType.Cut .or. eAction == DesignerActionType.Copy .or. ;
			eAction == DesignerActionType.RemoveSelected
			RETURN !SELF:oWindowDesign:lSelected .and. SELF:aSelected:Count != 0
		CASE eAction == DesignerActionType.Paste
			RETURN SELF:oClipboard:Count != 0*/
		CASE eAction == DesignerActionType.Undo
			RETURN SELF:nAction >= 1
		CASE eAction == DesignerActionType.Redo
			RETURN SELF:nAction < SELF:aActions:Count

		END CASE
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
//		LOCAL n AS INT

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
	
/*			IF oAction != NULL .and. oAction:aSelected:Count == 0
				FOR n := 0 UPTO SELF:aSelected:Count - 1
					oDesign := (DesignWindowItem)SELF:aSelected[n]
					IF !oDesign:IsForm
						oAction:aSelected:Add(oDesign:cGuid)
					ENDIF
				NEXT
			ENDIF*/
	
			SELF:oGrid:Fill(SELF:GetSelected())

            IF SELF:IsDirtyChanged != NULL
                SELF:IsDirtyChanged:Invoke(SELF , EventArgs{})
            ENDIF
            
            IF !SELF:IsDirty
            	SELF:ResetModified()
            END IF
	
		ENDIF
	
	RETURN

	VIRTUAL METHOD DoBasicAction(oAction AS DesignerBasicAction , eAction AS DesignerBasicActionType , uData AS ActionData) AS OBJECT
		LOCAL aProperties AS NameValueCollection
		LOCAL oUndo,oRedo AS DesignerBasicAction
		LOCAL oItem AS FSEDesignListViewItem
		LOCAL oDesign AS FSEDesignFieldSpec
		LOCAL oEnumerator AS IEnumerator
		LOCAL oProp AS VODesignProperty
		LOCAL oNameValue AS NameValue
		LOCAL cGuid AS STRING
		LOCAL oRet AS OBJECT
		LOCAL n AS INT

		SWITCH eAction
		CASE DesignerBasicActionType.Create
			cGuid := uData:cGuid
			IF cGuid == NULL
				cGuid := Guid.NewGuid():ToString()
			END IF
			IF uData:oData == NULL
				oItem := FSEDesignListViewItem{SELF}
				SELF:oListView:Items:Add(oItem)
			ELSE
				oItem := (FSEDesignListViewItem)uData:oData
			END IF
			oDesign := oItem:oDesign
			oItem:SetValues()
			SELF:oListView:SelectedIndices:Add(oItem:Index)
			oItem:Selected := TRUE
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
				oRedo:uData := ActionData{cGuid}
				oAction:aRedo:Add(oRedo)
			ENDIF
			IF uData:aData != NULL
				oEnumerator := uData:aData:GetEnumerator()
				DO WHILE oEnumerator:MoveNext()
					oNameValue := (NameValue)oEnumerator:Current
					SELF:DoBasicAction(oAction , DesignerBasicActionType.SetProperty , ActionData{cGuid , oNameValue:Name , oNameValue:Value})
				END DO
			ENDIF
			
		CASE DesignerBasicActionType.Remove
			cGuid := uData:cGuid
			oDesign := SELF:GetDesignItemFromGuid(cGuid)
			LOCAL nIndex AS INT
			nIndex := oDesign:oItem:Index
			oDesign:oItem:Remove()
			IF SELF:oListView:Items:Count > nIndex
				SELF:oListView:SelectedIndices:Add(nIndex)
				SELF:oListView:SelectedItems[0]:Selected := TRUE
			ELSEIF SELF:oListView:Items:Count != 0
				SELF:oListView:SelectedIndices:Add(SELF:oListView:Items:Count - 1)
				SELF:oListView:SelectedItems[0]:Selected := TRUE
			END IF
			
			SELF:lDidAction := TRUE
			IF !oAction:lExecuted
				aProperties := NameValueCollection{}
				FOR n := 0 UPTO oDesign:aProperties:Count - 1
					oProp := (VODesignProperty)oDesign:aProperties[n]
					IF TRUE .or. !oProp:IsAuto // problem with some properties
						aProperties:Add(oProp:Name , oProp:Value)
					ENDIF
				NEXT

				oUndo := DesignerBasicAction{TRUE}
				oUndo:eAction := DesignerBasicActionType.Create
				oUndo:uData := ActionData{oDesign:cGuid , NULL , NULL , aProperties}
				oAction:aUndo:Add(oUndo)
				
				oRedo := DesignerBasicAction{TRUE}
				oRedo:eAction := DesignerBasicActionType.Remove
				oRedo:uData := ActionData{oDesign:cGuid}
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

		SELF:BeginAction()

		SWITCH eAction
		CASE DesignerActionType.SelectAll
		CASE DesignerActionType.RemoveSelected
		CASE DesignerActionType.DeSelectAll
		CASE DesignerActionType.Cut
		CASE DesignerActionType.Copy
		CASE DesignerActionType.Paste
            nop
		CASE DesignerActionType.Undo
			SELF:Undo()
		CASE DesignerActionType.Redo
			SELF:Redo()

		END SWITCH

		SELF:EndAction()

	RETURN

	VIRTUAL METHOD GetDesignItemFromGuid(cGuid AS STRING) AS FSEDesignFieldSpec
		LOCAL oDesign AS DesignItem
		LOCAL aItems AS ArrayList
		LOCAL n AS INT
		aItems := SELF:GetAllDesignItems()
		FOR n := 0 UPTO aItems:Count - 1
			oDesign := (DesignItem)aItems[n]
			IF oDesign:cGuid == cGuid
				RETURN (FSEDesignFieldSpec)oDesign
			ENDIF
		NEXT
	RETURN NULL

	// overriden in x# code
	VIRTUAL METHOD Save(cFileName AS STRING , lVnfrmOnly AS LOGIC) AS LOGIC
	RETURN TRUE
	PROTECTED METHOD GetSaveFileStreams(cVNfsFileName REF STRING , cPrgFileName REF STRING , ;
								oPrgStream AS EditorStream, lVnfrmOnly AS LOGIC) AS LOGIC
		LOCAL lSuccess AS LOGIC
		LOCAL lError AS LOGIC
		
		IF .not. SELF:lLoadedAsXml
			cVNfsFileName := Funcs.GetModuleFilenameFromBinary(cVNfsFileName) + ".Fieldspecs.xsfs"
			IF .not. VOFieldSpecEditor.lEditorCloseWarningShown
				IF .not. Funcs.QuestionBox(e"By saving the editor contents, FieldSpec definitions will be written to file: \n\n" + cVNfsFileName + ;
										 e"\n\nwhich will be added as a new item to the Project. " + ;
										 e"The current editor will be closed and all selected .xsfs files will be removed from the project." + ;
										 e"\n\nDo you wish to continue ?" , "FieldSpec Editor")
						RETURN FALSE
				ENDIF
				VOFieldSpecEditor.lEditorCloseWarningShown := TRUE
			END IF
		END IF

		TRY
			cPrgFileName := Funcs.GetModuleFilenameFromBinary(cVNfsFileName) + ".prg"

			lError := FALSE
			IF !lVnfrmOnly
				IF !File.Exists(cPrgFileName)
					Funcs.ErrorBox("File was not found : " + cPrgFileName)
					lError := TRUE
				END IF
			END IF
			
			lSuccess := FALSE
			IF !lError
				IF !lVnfrmOnly
					oPrgStream:Load(cPrgFileName)
					lSuccess := oPrgStream:IsValid
				ELSE
					lSuccess := TRUE
				END IF
			END IF
			
		CATCH e AS Exception

			MessageBox.Show(e:Message , Resources.EditorName , MessageBoxButtons.OK , MessageBoxIcon.Exclamation)
			lSuccess := FALSE

		END TRY

		IF !lSuccess
			IF oPrgStream:IsValid
				oPrgStream:Close()
			ENDIF
		END IF

	RETURN lSuccess

	PROTECTED METHOD GetSaveFileStreams_unused(cVNFrmFileName AS STRING , aVNFrmStream AS FileStream[] , ;
										oPrgStream REF FileStream, lVnfrmOnly AS LOGIC) AS LOGIC
/*		LOCAL cPrgFileName AS STRING
		LOCAL cFileName AS STRING
		LOCAL cBaseName AS STRING
		LOCAL oFileInfo AS FileInfo
		LOCAL oBaseDir AS DirectoryInfo
		LOCAL oDesign AS FSEDesignFieldSpec
		LOCAL cAlternative AS STRING
		LOCAL cBaseDir AS STRING
		LOCAL lSuccess AS LOGIC
		LOCAL lError AS LOGIC
		LOCAL n,nAt AS INT
		
		TRY

			oFileInfo := FileInfo{cVNFrmFileName}
			oBaseDir := oFileInfo:Directory
			cBaseDir := oBaseDir:FullName
			cBaseName := oFileInfo:Name
			nAt := cBaseName:ToLower():IndexOf(".xsfs")
			IF nAt != -1
				cBaseName := cBaseName:Substring(0 , nAt)
			ENDIF
						
			cFileName := cBaseName
			nAt := cFileName:IndexOf('.')
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
				IF !File.Exists(cPrgFileName)
					Funcs.ErrorBox("File was not found : " + cPrgFileName)
					lError := TRUE
				END IF
			END IF
			
			lSuccess := FALSE
			IF !lError
				IF !lVnfrmOnly
					oPrgStream := File.Open(cPrgFileName, FileMode.OpenOrCreate , FileAccess.ReadWrite , FileShare.None)
					lSuccess := oPrgStream != NULL
				ELSE
					lSuccess := TRUE
				END IF
			END IF
			IF lSuccess .and. !lError
				lSuccess := FALSE
				FOR n := 1 UPTO aVNFrmStream:Length
					oDesign := ((FSEDesignListViewItem)SELF:oListView:Items[n-1]):oDesign
					IF oDesign:lModified
						aVNFrmStream[n] := File.Open(oDesign:cVNfsFileName , FileMode.Create , FileAccess.Write , FileShare.None)
					END IF
				NEXT
				lSuccess := TRUE
			ENDIF
			
		CATCH e AS Exception

			MessageBox.Show(e:Message , Resources.EditorName , MessageBoxButtons.OK , MessageBoxIcon.Exclamation)
			lSuccess := FALSE

		END TRY

		IF !lSuccess
			FOR n := 1 UPTO aVNFrmStream:Length
				IF aVNFrmStream[n] != NULL
					aVNFrmStream[n]:Close()
				END IF
			NEXT
			IF oPrgStream != NULL
				oPrgStream:Close()
			ENDIF
		END IF
	RETURN lSuccess
*/
	RETURN FALSE

	VIRTUAL METHOD Open(cFileName AS STRING) AS LOGIC
	RETURN TRUE
	
	METHOD InitReadOnlyProperties() AS VOID
		LOCAL oDesign AS FSEDesignFieldSpec
		LOCAL aDesign AS ArrayList
		LOCAL nType AS INT
		LOCAL n AS INT
		aDesign := SELF:GetAllDesignItems()
		FOR n := 0 UPTO aDesign:Count - 1
			oDesign := (FSEDesignFieldSpec)aDesign[n]
			nType := (INT)oDesign:GetProperty("type"):Value
			IF nType >= 2
				oDesign:GetProperty("len"):lReadOnly := TRUE
			END IF
			IF nType != 1
				oDesign:GetProperty("dec"):lReadOnly := TRUE
			END IF
		NEXT
	RETURN
	
	METHOD ShowCode() AS VOID
		LOCAL oCode AS CodeContents
		LOCAL aDesign AS ArrayList
		LOCAL t AS TextBox
		LOCAL n,m AS INT
		LOCAL f AS Form

		aDesign := SELF:GetAllDesignItems()
		
		f := Form{}
		f:Size := Size{800 , 500}
		f:ShowInTaskbar := FALSE
		f:Text := "Generated code"
		t := TextBox{}
		t:Multiline := TRUE
		t:ScrollBars := ScrollBars.Vertical
		t:Dock := DockStyle.Fill
		f:Controls:Add(t)

//		VOFieldSpecEditor.Template:Read(SELF:cLoadedDir)
		VOFieldSpecEditor.LoadTemplate(SELF:cLoadedDir)

		FOR m := 0 UPTO aDesign:Count - 1
			oCode := GetCodeContents((FSEDesignFieldSpec)aDesign[m])
			FOR n := 0 UPTO oCode:aClass:Count - 1
				t:Text += oCode:aClass[n] + e"\r\n"
			NEXT
			t:Text += e"\r\n"
			FOR n := 0 UPTO oCode:aConstructor:Count - 1
				t:Text += oCode:aConstructor[n] + e"\r\n"
			NEXT
		NEXT

		f:ShowDialog()
	RETURN

	STATIC METHOD GetCodeContents(oDesign AS FSEDesignFieldSpec) AS CodeContents
		LOCAL oCode AS CodeContents
		LOCAL aClass AS List<STRING>
		LOCAL aConstructor AS List<STRING>
		LOCAL cLine AS STRING
		LOCAL n AS INT
		
		oCode := CodeContents{}
		
		aClass := oCode:aClass
		FOR n := 0 UPTO VOFieldSpecEditor.Template:aClass:Count - 1
			cLine := VOFieldSpecEditor.Template:aClass[n]
			cLine := TranslateLine(cLine , oDesign)
			aClass:Add(cLine)
		NEXT
		
		aConstructor := oCode:aConstructor
		FOR n := 0 UPTO VOFieldSpecEditor.Template:aInit:Count - 1
			cLine := VOFieldSpecEditor.Template:aInit[n]
			cLine := TranslateLine(cLine , oDesign)
			aConstructor:Add(cLine)
		NEXT
	RETURN oCode
	
	INTERNAL STATIC METHOD TranslateLine(cLine AS STRING , oDesign AS FSEDesignFieldSpec) AS STRING
		LOCAL oProp AS DesignProperty
		LOCAL n , nAt AS INT
		LOCAL cUpper AS STRING
		LOCAL cRet AS STRING
		LOCAL nLength AS INT
		LOCAL cValue AS STRING

		LOCAL aSorted AS SortedList
		LOCAL rKey AS REAL8
		
		aSorted := SortedList{}
		FOR n := 0 UPTO oDesign:aProperties:Count - 1
			oProp := (DesignProperty)oDesign:aProperties[n]
			rKey := (REAL8)oProp:Name:Length
			DO WHILE aSorted:ContainsKey(rKey)
				rKey += 0.001
			END DO
			aSorted:Add(rKey , oProp)
		NEXT
		
		cUpper := cLine:ToUpper()
		cRet := cLine
//		FOR n := 0 UPTO oDesign:aProperties:Count - 1
		FOR n := aSorted:Count - 1 DOWNTO 0
//			oProp := oDesign:GetProperty(n)
			oProp := (DesignProperty)aSorted:GetByIndex(n)
			nAt := cUpper:IndexOf("%" + oProp:Name:ToUpper() + "%")
			IF nAt == - 1
				nAt := cUpper:IndexOf("%" + oProp:Name:ToUpper())
				IF nAt != -1
					nLength := oProp:Name:Length + 1
				END IF
			ELSE
				nLength := oProp:Name:Length + 2
			END IF
			IF nAt != -1
				cRet := cLine:Substring(0 , nAt)
				DO CASE
				CASE oProp:cEnumType == "YESNO"
					cValue := iif(oProp:ValueLogic , "TRUE" , "FALSE")
				CASE oProp:Name == "type"
					IF (INT)oProp:Value == 6
						cValue := "X"
					ELSE
						cValue := oProp:TextValue:Substring(0,1)
					END IF
				CASE oProp:Name == "superclass"
					cValue := oProp:TextValue:Trim()
					IF cValue:Length == 0
						cValue := "FieldSpec"
					END IF
					
				CASE oProp:IsAuto .and. (oProp:Name == "minrange" .or. oProp:Name == "maxrange" .or. oProp:Name == "validation")
					cValue := "NIL"

				CASE oProp:Name == "validation"
					cValue := oProp:TextValue
					IF .not. cValue:StartsWith("{")
						cValue := e"\"" + cValue + e"\""
					END IF
					
				OTHERWISE

					cValue := Funcs.TranslateCaption(oProp:TextValue , TRUE , FALSE)
					
					// Ultra hack, template probably contains quotes that we need to remove
					IF cValue:StartsWith("LoadResString") .and. cRet:EndsWith(e"\"")
						cRet := cRet:Substring(0 , cRet:Length - 1)
						IF nAt + nLength < cLine:Length
							IF cLine[nAt + nLength] == '"'
								cLine := cLine:Substring(0 , nAt + nLength) + cLine:Substring(nAt + nLength + 1)
							END IF
						END IF
					END IF

				END CASE
				cRet += cValue
				IF nAt + nLength < cLine:Length
					cRet += cLine:Substring(nAt + nLength)
				END IF
				cLine := cRet
				cUpper := cLine:ToUpper()
			END IF
		NEXT
		
	RETURN cRet

	PROTECTED METHOD NameExists(cName AS STRING) AS LOGIC
		LOCAL oDesign AS FSEDesignFieldSpec
		LOCAL aDesign AS ArrayList
		LOCAL n AS INT
		cName := cName:ToUpper():Trim()
		aDesign := SELF:GetAllDesignItems()
		FOR n := 0 UPTO aDesign:Count - 1
			oDesign := (FSEDesignFieldSpec)aDesign[n]
			IF oDesign:Name:ToUpper():Trim() == cName
				RETURN TRUE
			ENDIF
		NEXT
	RETURN FALSE

	METHOD CheckIfValid() AS LOGIC
		LOCAL oDesign AS FSEDesignFieldSpec
		LOCAL aDesign AS ArrayList
		LOCAL n AS INT
		aDesign := SELF:GetAllDesignItems()
		IF aDesign:Count == 0
			Funcs.WarningBox("No FieldSpecs included.")
			RETURN FALSE
		ENDIF
		FOR n := 0 UPTO aDesign:Count - 1
			oDesign := (FSEDesignFieldSpec)aDesign[n]
			IF oDesign:GetProperty("classname"):TextValue:Trim() == "" .or. oDesign:GetProperty("hlname"):TextValue:Trim() == "" .or. ;
					(INT)oDesign:GetProperty("len"):Value == 0 .or. .not. IsNameValid(oDesign:Name)
				Funcs.WarningBox("Fieldspec " + oDesign:Name + " has invalid values.")
				RETURN FALSE
			END IF
		NEXT
	RETURN TRUE

	STATIC METHOD IsNameValid(cName AS STRING) AS LOGIC
		LOCAL n AS INT
		cName := cName:ToUpper()
		IF cName:Trim() == String.Empty
			RETURN FALSE
		END IF
		FOR n := 0 UPTO cName:Length - 1
			IF "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_":IndexOf(cName[n]) == -1
				RETURN FALSE
			END IF
		NEXT
	RETURN TRUE

END CLASS




CLASS FSEListView INHERIT ListView
	PROTECT oItemEdit AS FSEDesignListViewItem
	PROTECT lAppending AS LOGIC
	PROTECT oFSEditor AS VOFieldSpecEditor
	PROTECT oEdit AS FSETextBox
	CONSTRUCTOR(_oFSEditor AS VOFieldSpecEditor)
		SUPER()
		SELF:oFSEditor := _oFSEditor
		SELF:oEdit := FSETextBox{}
		SELF:oEdit:Visible := FALSE
		SELF:oEdit:AutoSize := FALSE
		SELF:Controls:Add(SELF:oEdit)

		SELF:ContextMenu := ContextMenu{}
		SELF:ContextMenu:Popup += EventHandler{ SELF , @ContextMenuPopUp() }
		SELF:ContextMenu:MenuItems:Add("Add FieldSpec" , EventHandler{ SELF , @ContextAdd() })
		SELF:ContextMenu:MenuItems:Add("Delete FieldSpec" , EventHandler{ SELF , @ContextRemove() })

	RETURN
	
	PROTECTED METHOD ProcessCmdKey(Msg REF Message,KeyData AS Keys) AS LOGIC
		IF KeyData==Keys.Escape
			RETURN FALSE
		END IF
	RETURN SUPER:ProcessCmdKey(REF Msg,KeyData)
	
	PROTECTED METHOD OnMouseUp(e AS MouseEventArgs) AS VOID
		DO CASE
		CASE e:Clicks==1 .and. e:Button==MouseButtons.Left 
			IF SELF:oFSEditor:lLoadedAsXml
				IF SELF:Items:Count == 0
					SELF:Append()
				ELSEIF SELF:GetItemAt(e:X , e:Y) == NULL
					SELF:Append()
				END IF
			END IF
		END CASE
	RETURN
	
	PROTECTED METHOD OnGotFocus(e AS EventArgs) AS VOID
		SUPER:OnGotFocus(e)
/*		IF SELF:Items:Count == 0 .and. SELF:oItemEdit == NULL
			SELF:Append()
		END IF*/
	RETURN
	
	PROTECTED METHOD OnSelectedIndexChanged(e AS EventArgs) AS VOID
		SUPER:OnSelectedIndexChanged(e)
//		IF SELF:SelectedItems:Count != 0
			SELF:oFSEditor:DisplayProperties()
//		END IF
	RETURN

	METHOD ContextMenuPopUp(o AS OBJECT,e AS EventArgs) AS VOID
		LOCAL oDesign AS FSEDesignFieldSpec
		IF SELF:SelectedItems:Count != 0
			oDesign := ((FSEDesignListViewItem)SELF:SelectedItems[0]):oDesign
		END IF
		SELF:ContextMenu:MenuItems[1]:Enabled := oDesign != NULL
	RETURN
	METHOD ContextAdd(o AS OBJECT , e AS EventArgs) AS VOID
		SELF:Append()
	RETURN
	METHOD ContextRemove(o AS OBJECT , e AS EventArgs) AS VOID
		SELF:Delete()
	RETURN
	
	PROTECTED METHOD OnKeyDown(e AS KeyEventArgs) AS VOID
		SUPER:OnKeyDown(e)
		DO CASE
		CASE e:KeyData == Keys.Z + Keys.Control
			SELF:oFSEditor:Undo()
		CASE e:KeyData == Keys.Z + Keys.Control + Keys.Shift
			SELF:oFSEditor:Redo()
		CASE e:KeyData==Keys.Delete
			SELF:Delete()
		CASE e:KeyData==Keys.Enter
			IF SELF:oFSEditor:lLoadedAsXml
				SELF:Append()
			ENDIF
		CASE e:KeyCode == Keys.Down
/*			IF SELF:SelectedIndices:Count == 1 .and. SELF:SelectedIndices[0] == SELF:Items:Count - 1
				IF SELF:oFSEditor:lLoadedAsXml
					SELF:Append()
				END IF
			END IF*/
/*		CASE e:KeyData==Keys.F2
			SELF:ShowEdit(FALSE)
		CASE e:KeyData==Keys.Insert
			SELF:Insert()
		CASE e:KeyCode==Keys.S .and. e:Modifiers==Keys.Control
			SELF:oIde:MenuFileSave()
		CASE e:KeyCode==Keys.Enter .and. e:Modifiers==Keys.Control
			SELF:oIde:FocusEditor()*/
		END CASE
	RETURN
	
	METHOD Delete() AS VOID
		LOCAL oItem AS FSEDesignListViewItem
//		LOCAL nIndex AS INT
		IF SELF:SelectedItems:Count==0
			RETURN
		END IF
		oItem := (FSEDesignListViewItem)SELF:SelectedItems[0]
//		nIndex := oItem:Index
/*		IF !Funcs.QuestionBox("Are you sure you want to delete the FieldSpec "+oItem:Text+ " ?" , "FieldSpec Editor")
			RETURN
		END IF*/
		SELF:oFSEditor:StartAction(DesignerBasicActionType.Remove , ActionData{oItem:oDesign:cGuid})
/*		SELF:Items:Remove(oItem)
		IF SELF:Items:Count > nIndex
			SELF:Items[nIndex]:Selected:=TRUE
		ELSEIF SELF:Items:Count!=0
			SELF:Items[SELF:Items:Count-1]:Selected:=TRUE
		END IF*/
	RETURN
	
	METHOD Append() AS VOID
		IF SELF:lAppending .or. SELF:oItemEdit != NULL
			RETURN
		END IF
		IF .not. SELF:oFSEditor:lLoadedAsXml
			Funcs.WarningBox("FieldSpec definitions need to be saved in xml format, before adding new items." , "FieldSpec Editor")
			RETURN
		END IF
		SELF:Sorting := SortOrder.None
		SELF:Items:Add(FSEDesignListViewItem{SELF:oFSEditor})
		SELF:SelectedItems:Clear()
		SELF:Items[SELF:Items:Count-1]:Selected := TRUE
		SELF:ShowEdit(TRUE)
	RETURN
	
	METHOD ShowEdit(_lAppending AS LOGIC) AS VOID
		LOCAL oItem AS FSEDesignListViewItem
		LOCAL oRect AS Rectangle

		IF .not. _lAppending
			RETURN
		END IF
		IF SELF:lAppending .or. SELF:oItemEdit != NULL
			RETURN
		END IF
		IF SELF:SelectedItems:Count == 0
			IF SELF:FocusedItem == NULL
				RETURN
			ELSE
				SELF:FocusedItem:Selected := TRUE
			END IF
		END IF
		oItem := (FSEDesignListViewItem)SELF:SelectedItems[0]
		SELF:EnsureVisible(oItem:Index)
		SELF:lAppending := _lAppending
		SELF:oItemEdit := oItem
		oRect := SELF:GetItemRect(SELF:oItemEdit:Index , ItemBoundsPortion.Label)
		SELF:oEdit:Location := Point{oRect:Left-1,oRect:Top-2}
		SELF:oEdit:Width := oRect:Width+1
		SELF:oEdit:Height := oRect:Height + 2
		SELF:oEdit:Text := oItem:Text
		SELF:oEdit:Show(SELF)
		SELF:oEdit:BringToFront()
		SELF:oEdit:Focus()
	RETURN
		
	METHOD AcceptEdit() AS VOID
		LOCAL oDesign AS FSEDesignFieldSpec
		LOCAL n AS INT

		IF SELF:oItemEdit == NULL
			RETURN
		END IF
		FOR n := 0 UPTO SELF:Items:Count - 2
			IF SELF:Items[n]:Text:ToUpper() == SELF:oEdit:Text:Trim():ToUpper()
				SELF:CancelEdit()
				RETURN
			END IF
		NEXT
		IF .not. VOFieldSpecEditor.IsNameValid(SELF:oEdit:Text:Trim())
			SELF:CancelEdit()
			RETURN
		END IF
		
		IF SELF:lAppending
			SELF:oFSEditor:BeginAction()
			oDesign := (FSEDesignFieldSpec)SELF:oFSEditor:StartAction(DesignerBasicActionType.Create , ActionData{NULL , NULL , SELF:oItemEdit})
			SELF:oFSEditor:StartAction(DesignerBasicActionType.SetProperty , ActionData{oDesign:cGuid , "classname" , SELF:oEdit:Text:Trim()})
			SELF:oFSEditor:StartAction(DesignerBasicActionType.SetProperty , ActionData{oDesign:cGuid , "hlname" , SELF:oEdit:Text:Trim()})
			SELF:oFSEditor:StartAction(DesignerBasicActionType.SetProperty , ActionData{oDesign:cGuid , "len" , 10})
			SELF:oFSEditor:StartAction(DesignerBasicActionType.SetProperty , ActionData{oDesign:cGuid , "minlen" , -1})
			SELF:oFSEditor:StartAction(DesignerBasicActionType.SetProperty , ActionData{oDesign:cGuid , "required" , 1})
			SELF:oFSEditor:EndAction()
		END IF

		SELF:oItemEdit:SetValues()
		SELF:oItemEdit:Focused := TRUE
		SELF:oItemEdit := NULL
		SELF:Focus()
		SELF:lAppending := FALSE

		SELF:Sorting := SortOrder.Ascending
		SELF:Sort()

	RETURN

	METHOD CancelEdit() AS VOID
		LOCAL nIndex AS INT
		IF SELF:oItemEdit == NULL
			RETURN
		END IF
		IF SELF:lAppending
			nIndex := SELF:oItemEdit:Index
			SELF:Items:Remove(SELF:oItemEdit)
			SELF:lAppending := FALSE
			IF SELF:Items:Count > nIndex
				SELF:Items[nIndex]:Selected := TRUE
				SELF:Items[nIndex]:Focused := TRUE
			ELSEIF SELF:Items:Count!=0
				SELF:Items[SELF:Items:Count-1]:Selected := TRUE
				SELF:Items[SELF:Items:Count-1]:Focused := TRUE
			END IF
		ELSE
			SELF:oItemEdit:Selected := TRUE
			SELF:oItemEdit:Focused := TRUE
		END IF
		SELF:Focus()
		Application.DoEvents()
		SELF:oItemEdit := NULL
		SELF:Sorting := SortOrder.Ascending
	RETURN
	
END CLASS





CLASS FSEDesignListViewItem INHERIT ListViewItem
	EXPORT oDesign AS FSEDesignFieldSpec
	EXPORT oFSEditor AS VOFieldSpecEditor
	CONSTRUCTOR(_oFSEditor AS VOFieldSpecEditor)
		SUPER()
		SELF:oFSEditor:=_oFSEditor
		SELF:SubItems:Add("")
		SELF:SubItems:Add("")
		SELF:SubItems:Add("")
		SELF:SubItems:Add("")
		SELF:SubItems:Add("")
		SELF:SubItems:Add("")
		SELF:SubItems:Add("")
		SELF:oDesign := FSEDesignFieldSpec{SELF,SELF:oFSEditor}
	RETURN
	METHOD SetValues() AS VOID
//		SELF:Text := SELF:oDesign:GetProperty("HLName"):TextValue
		SELF:Text := SELF:oDesign:GetProperty("classname"):TextValue
		SELF:SubItems[1]:Text:= SELF:oDesign:GetProperty("Type"):TextValue
		SELF:SubItems[2]:Text := SELF:oDesign:GetProperty("Len"):TextValue
		SELF:SubItems[3]:Text := SELF:oDesign:GetProperty("Dec"):TextValue
		SELF:SubItems[4]:Text := SELF:oDesign:GetProperty("Picture"):TextValue
		TRY
			LOCAL cValue AS STRING
			cValue := SELF:oDesign:GetProperty("MinLen"):TextValue
			SELF:SubItems[5]:Text := iif(cValue == "-1" , "" , cValue)
			SELF:SubItems[6]:Text := SELF:oDesign:GetProperty("Required"):TextValue
			SELF:SubItems[7]:Text := SELF:oDesign:GetProperty("Validation"):TextValue
		END TRY
	RETURN
END CLASS






CLASS FSEDesignFieldSpec INHERIT DesignItem
	EXPORT oEditor AS VOFieldSpecEditor
	EXPORT oItem AS FSEDesignListViewItem
	EXPORT cVNfsFileName AS STRING
	EXPORT lModified AS LOGIC

	CONSTRUCTOR(_oItem AS FSEDesignListViewItem , _oEditor AS VOFieldSpecEditor)

		SUPER(_oEditor)
		
		LOCAL oProp AS VODesignProperty
		
		SELF:oItem := _oItem
		SELF:oEditor := _oEditor

		SELF:aPages := List<STRING>{}
		SELF:aPages:Add("General")
		
//		SELF:AddProperty(VODesignProperty{"classname" , "FieldSpec" , "" , PropertyType.Text , PropertyStyles.ReadOnly})
		SELF:AddProperty(VODesignProperty{"classname" , "FieldSpec" , "" , PropertyType.Text , PropertyStyles.None})
		SELF:AddProperty(VODesignProperty{"superclass" , "Inherit from" , "" , PropertyType.Text , PropertyStyles.NoAuto})
//		SELF:AddProperty(VODesignProperty{"hlname" , "Name" , "" , PropertyType.Text , PropertyStyles.ReadOnly})
		SELF:AddProperty(VODesignProperty{"hlname" , "Name" , "" , PropertyType.Text})

		SELF:AddProperty(VODesignProperty{"hlcaption" , "Caption" , "" , PropertyType.Text , PropertyStyles.NoAuto})
		SELF:AddProperty(VODesignProperty{"hlDescription" , "Description" , "" , PropertyType.Text , PropertyStyles.NoAuto})
		SELF:AddProperty(VODesignProperty{"hlhelpcontext" , "Help Context" , "" , PropertyType.Text , PropertyStyles.NoAuto})

//		SELF:AddProperty(VODesignProperty{"Type" , "Type" , "Type" , PropertyType.Text})
//		SELF:AddProperty(VODesignProperty{"Type","Type","Type","__fieldspecDataTypes",PropertyStyles.NoAuto})
		oProp := VODesignProperty{"type","Type","Type","__fieldspecDataTypes"}
		oProp:lNoAuto := TRUE
		SELF:AddProperty(oProp)
		SELF:AddProperty(VODesignProperty{"typediag","Type Diagnostic","Type Diagnostic",PropertyType.Text , PropertyStyles.NoAuto})
		SELF:AddProperty(VODesignProperty{"typehelp","Type Help","",PropertyType.Text , PropertyStyles.NoAuto})


		SELF:AddProperty(VODesignProperty{"len" , "Length" , "Length" , PropertyType.Numeric , PropertyStyles.NoAuto})
		SELF:AddProperty(VODesignProperty{"lendiag","Length Diagnostic","",PropertyType.Text , PropertyStyles.NoAuto})
		SELF:AddProperty(VODesignProperty{"lenhelp","Length Help","",PropertyType.Text , PropertyStyles.NoAuto})

		SELF:AddProperty(VODesignProperty{"dec" , "Decimal" , "Decimal" , PropertyType.Numeric , PropertyStyles.NoAuto})
		SELF:AddProperty(VODesignProperty{"picture" , "Picture" , "Picture" , PropertyType.Text , PropertyStyles.NoAuto})
	
		SELF:AddProperty(VODesignProperty{"minlen","Min Length","",PropertyType.Numeric , PropertyStyles.NoAuto})
		SELF:AddProperty(VODesignProperty{"minlendiag","Min Length Diagnostic","",PropertyType.Text , PropertyStyles.NoAuto})
		SELF:AddProperty(VODesignProperty{"minlenhelp","Min Length Help","",PropertyType.Text , PropertyStyles.NoAuto})
		SELF:AddProperty(VODesignProperty{"required","Required","","YESNO" , PropertyStyles.NoAuto})
		SELF:AddProperty(VODesignProperty{"reqdiag","Required Diagnostic","",PropertyType.Text , PropertyStyles.NoAuto})
		SELF:AddProperty(VODesignProperty{"reqhelp","Required Help","",PropertyType.Text , PropertyStyles.NoAuto})
	
		SELF:AddProperty(VODesignProperty{"minrange","Minimum","",PropertyType.Text , PropertyStyles.NoAuto})
		SELF:AddProperty(VODesignProperty{"maxrange","Maximum","",PropertyType.Text , PropertyStyles.NoAuto})
		SELF:AddProperty(VODesignProperty{"rangediag","Range Diagnostic","",PropertyType.Text , PropertyStyles.NoAuto})
		SELF:AddProperty(VODesignProperty{"rangehelp","Range Help","",PropertyType.Text , PropertyStyles.NoAuto})
		SELF:AddProperty(VODesignProperty{"validation","Validation","",PropertyType.Text , PropertyStyles.NoAuto})
		SELF:AddProperty(VODesignProperty{"validdiag","Validation Diagnostic","",PropertyType.Text , PropertyStyles.NoAuto})
		SELF:AddProperty(VODesignProperty{"validhelp","Validation Help","",PropertyType.Text , PropertyStyles.NoAuto})
	
	RETURN 

	METHOD AllowPropertyUpdate(oProp AS VODesignProperty , oValue AS OBJECT) AS LOGIC
		IF oProp:Name == "hlname" .or. oProp:Name == "classname"
			RETURN FALSE
		END IF
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
	ACCESS Name AS STRING
	RETURN SELF:GetProperty("classname"):Value:ToString()

END CLASS




CLASS FieldSpecCode
	EXPORT aClass,aInit AS List<STRING>

	CONSTRUCTOR()
		SELF:Reset()
	RETURN
	
	INTERNAL METHOD Reset() AS VOID
		SELF:aClass := List<STRING>{}
		SELF:aInit := List<STRING>{}
	RETURN
	
	METHOD Read(cDirectory AS STRING) AS LOGIC
		LOCAL oStream AS System.IO.StreamReader
		LOCAL cLine,cUpper AS STRING
		LOCAL aRead AS List<STRING>
		LOCAL cOrigDir AS STRING
		LOCAL cCavoWed AS STRING
		
//		cFileName := Application.StartupPath + "\cavofed.tpl"
//		cFileName := "C:\cavo28\Bin\cavofed.tpl"
//		cFileName := "C:\cavo28\Bin\cavofed_26.tpl"

      cOrigDir := cDirectory
        TRY
        	cCavoWed := cDirectory + "\Properties\CAVOFED.TPL"
	        IF !System.IO.File.Exists(cCavoWed)
		        cCavoWed := cDirectory + "\CAVOFED.TPL"
				IF !System.IO.File.Exists(cCavoWed)
					cDirectory := Directory.GetParent(cDirectory):FullName
					cCavoWed := cDirectory + "\CAVOFED.TPL"
			        IF !System.IO.File.Exists(cCavoWed)
			        	cCavoWed := cDirectory + "\Properties\CAVOFED.TPL"
				        IF !System.IO.File.Exists(cCavoWed) .and. Funcs.InstallTemplatesFolder != ""
				        	cCavoWed := Funcs.InstallTemplatesFolder  + "\CAVOFED.TPL"
				        ENDIF
			        ENDIF
				ENDIF
			END IF
        END TRY
		IF !System.IO.File.Exists(cCavoWed)
			MessageBox.Show("File Cavofed.tpl was not found, please locate it on disk." , Resources.EditorName)
		ENDIF
		DO WHILE !System.IO.File.Exists(cCavoWed)
			LOCAL oDlg AS OpenFileDialog
			oDlg := OpenFileDialog{}
			oDlg:Filter := "CavoFED files (*.tpl)|*.tpl"
			oDlg:Title := "Open cavofed.tpl file"
			IF oDlg:ShowDialog() == DialogResult.OK
				cCavoWed := oDlg:FileName:ToLower()
            TRY
               IF cCavoWed:Contains("cavofed") .and. cCavoWed:EndsWith(".tpl")
                  File.Copy(cCavoWed , cOrigDir + "\cavofed.tpl" , FALSE)
               ENDIF
            END TRY
			ELSE
				RETURN FALSE
			ENDIF
		END DO
	
		IF System.IO.File.Exists(cCavoWed)
			
			SELF:Reset()
		    oStream := System.IO.StreamReader{cCavoWed , System.Text.Encoding.GetEncoding(0)}
			DO WHILE oStream:Peek()!=-1
			    cLine := oStream:ReadLine()
			    IF cLine:Trim():StartsWith(";")
			    	LOOP
			    ENDIF
			    cUpper := cLine:Trim():ToUpper()
			    SWITCH cUpper
			    CASE "[CLASS]"
			    	aRead := SELF:aClass
			    CASE "[INIT]"
			    	aRead := SELF:aInit
			    CASE "[DBSERVER]"
			    	aRead := NULL
				OTHERWISE
					IF aRead != NULL
						IF cLine:ToUpper():StartsWith("METHOD") .and. cLine:ToUpper():Contains("INIT")
							aRead:Add("CONSTRUCTOR()")
						ELSE
							cLine := cLine:Replace("SUPER:Init" , "SUPER")
							cLine := cLine:Replace("RETURN SELF" , "RETURN")
							aRead:Add(cLine)
						END IF
					ENDIF
			    END SWITCH
			END DO
			oStream:Close()
			RETURN TRUE
		ENDIF
	
	RETURN FALSE
	
END CLASS


CLASS FSETextBox INHERIT TextBox
	PROTECT oList AS FSEListView
	PROTECTED METHOD OnLostFocus(e AS EventArgs) AS VOID
		SUPER:OnLostFocus(e)
		IF SELF:Text:Trim() == ""
			SELF:Hide()
			SELF:oList:CancelEdit()
		ELSE
			SELF:oList:AcceptEdit()
			SELF:Hide()
		ENDIF
	RETURN
	METHOD Show(_oList AS FSEListView) AS VOID
		SUPER:Show()
		SELF:oList := _oList
	RETURN
	PROTECTED METHOD OnKeyDown(e AS KeyEventArgs) AS VOID
		SUPER:OnKeyDown(e)
		SWITCH e:KeyData
		CASE Keys.Enter
			IF SELF:Text:Trim()==""
				RETURN
			END IF
			SELF:oList:AcceptEdit()
			SELF:Hide()
		CASE Keys.Escape
			SELF:Text := ""
			SELF:oList:CancelEdit()
			SELF:Hide()
		END SWITCH
	RETURN
	PROTECTED METHOD OnKeyPress(e AS KeyPressEventArgs) AS VOID
		SUPER:OnKeyPress(e)
		IF (INT)e:KeyChar == 27 .or. (INT)e:KeyChar == 13
			e:Handled := TRUE
		END IF
	RETURN
END CLASS


