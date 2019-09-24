#using System.Collections
#using System.Collections.Generic
#using System.Windows.Forms
#using System.Drawing
#using System.IO


PARTIAL CLASS VODBServerEditor INHERIT DesignerBase
	
	EXPORT oNameEdit AS TextBox
	EXPORT oFileNameEdit AS TextBox
	EXPORT oExportButton AS Button
	EXPORT oImportButton AS Button

	INTERNAL oIndexList AS DBEListView
	INTERNAL oOrderList AS DBEListView
	INTERNAL oFieldList AS DBEListView


	INTERNAL oEdit AS DBETextBox
	EXPORT oMainDesign AS DBEDesignDBServer
	EXPORT oLastDesign AS DBEDesignDBServer
	
	PROTECT oPanel AS DBServerSurface
	PROTECT cDefaultFileName AS STRING
	PROTECT cLoadedDir AS STRING
	
	PROTECT oPropertyUpdatedHandler AS PropertyUpdatedEventHandler
	PROTECT oControlKeyPressedHandler AS ControlKeyPressedEventHandler
	PROTECT oRetrieveClassNamesHandler AS RetrieveClassNamesEventHandler
	
	PROTECT aFilesToDelete AS List<STRING>
	
	STATIC PROTECT _Template AS DBServerCode

	PROTECT aAvailableFieldSpecs AS ArrayList
	PROTECT aFieldSpecsInModule AS ArrayList
	
	EXPORT lFillingOrders AS LOGIC

	STATIC ACCESS Template AS DBServerCode
	RETURN VODBServerEditor._Template
	STATIC METHOD LoadTemplate(cLoadedDir AS STRING) AS LOGIC
		IF VODBServerEditor._Template == NULL
			VODBServerEditor._Template := DBServerCode{}
		ENDIF
	RETURN VODBServerEditor._Template:Read(cLoadedDir)

	CONSTRUCTOR(_oSurface AS Control , _oGrid AS DesignerGrid)
		SUPER(_oSurface)

		SELF:aAvailableFieldSpecs := ArrayList{}
		SELF:aFieldSpecsInModule := ArrayList{}
		SELF:aFilesToDelete := List<STRING>{}

		SELF:oPropertyUpdatedHandler := PropertyUpdatedEventHandler{ SELF , @PropertyModifiedInGrid() }
		SELF:oControlKeyPressedHandler := ControlKeyPressedEventHandler{ SELF , @ControlKeyPressedInGrid() }
		SELF:oRetrieveClassNamesHandler := RetrieveClassNamesEventHandler{ SELF , @RetrieveClassNamesHandler() }

		SELF:oGrid := _oGrid
		SELF:oGrid:PropertyModified := SELF:oPropertyUpdatedHandler
		SELF:oGrid:ControlKeyPressed := SELF:oControlKeyPressedHandler
		SELF:oGrid:oActiveDesigner := SELF
		SELF:oGrid:UseHierarchy(FALSE)

		SELF:aActions := ArrayList{}

		SELF:oPanel := DBServerSurface{SELF}
		SELF:oNameEdit := SELF:oPanel:oNameEdit
		SELF:oFileNameEdit := SELF:oPanel:oFileNameEdit
		SELF:oExportButton := SELF:oPanel:oExportButton
		SELF:oImportButton := SELF:oPanel:oImportButton
		SELF:oIndexList := SELF:oPanel:oIndexList
		SELF:oOrderList := SELF:oPanel:oOrderList
		SELF:oFieldList := SELF:oPanel:oFieldList

		SELF:oMainDesign := DBEDesignDBServer{ DBServerItemType.DBServer , NULL , SELF}
		
		SELF:oNameEdit:GotFocus += EventHandler{SELF , @ItemGotFocus()}
		SELF:oFileNameEdit:GotFocus += EventHandler{SELF , @ItemGotFocus()}
		SELF:oExportButton:GotFocus += EventHandler{SELF , @ItemGotFocus()}
		SELF:oImportButton:GotFocus += EventHandler{SELF , @ItemGotFocus()}
		SELF:oFieldList:GotFocus += EventHandler{SELF , @ItemGotFocus()}
		SELF:oIndexList:GotFocus += EventHandler{SELF , @ItemGotFocus()}
		SELF:oOrderList:GotFocus += EventHandler{SELF , @ItemGotFocus()}
		
		SELF:oNameEdit:LostFocus += EventHandler{SELF , @ItemLostFocus()}
		SELF:oFileNameEdit:LostFocus += EventHandler{SELF , @ItemLostFocus()}
		SELF:oExportButton:LostFocus += EventHandler{SELF , @ItemLostFocus()}
		SELF:oImportButton:LostFocus += EventHandler{SELF , @ItemLostFocus()}
		SELF:oFieldList:LostFocus += EventHandler{SELF , @ItemLostFocus()}
		SELF:oIndexList:LostFocus += EventHandler{SELF , @ItemLostFocus()}
		SELF:oOrderList:LostFocus += EventHandler{SELF , @ItemLostFocus()}
		
		SELF:oNameEdit:KeyDown += KeyEventHandler{SELF , @EditKeyDown()}
		SELF:oFileNameEdit:KeyDown += KeyEventHandler{SELF , @EditKeyDown()}
		
		SELF:oExportButton:Click += EventHandler{SELF , @ExportDbfClicked()}
		SELF:oImportButton:Click += EventHandler{SELF , @ImportDbfClicked()}
//		SELF:oExportButton:Enabled := FALSE
		SELF:oNameEdit:ReadOnly := TRUE

		SELF:oTimer:Start()
		
	RETURN

	ACCESS Name AS STRING
	RETURN SELF:oMainDesign:Name
	ACCESS IsDirty AS LOGIC
	RETURN SELF:nAction != SELF:nActionSaved

	ACCESS MultipleOrdersSupported() AS LOGIC
		LOCAL cRdd AS STRING
		cRdd := SELF:oMainDesign:GetProperty("rdd"):TextValue:Trim():ToUpper()
	RETURN cRdd != String.Empty .and. cRdd != "DBFNTX"

	METHOD CanDoAction(eAction AS DesignerActionType) AS LOGIC
		
		IF SELF:lReadOnly
			RETURN FALSE
		ENDIF
		
		DO CASE
		CASE eAction == DesignerActionType.Undo
			RETURN SELF:nAction >= 1
		CASE eAction == DesignerActionType.Redo
			RETURN SELF:nAction < SELF:aActions:Count

		END CASE
	RETURN FALSE
		
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
    			IF !oGridForm:Visible
	    			oGridForm:Show()
	    			SELF:GiveFocus()
    			END IF
    		ELSE
    			IF !SELF:oSurface:ContainsFocus
    				oGridForm:Hide()
    			END IF
    		END IF
    	ENDIF
    RETURN

	METHOD ClearUndoBuffer() AS VOID
		SELF:aActions:Clear()
		SELF:nAction := 0
	RETURN

	METHOD CheckIfValid() AS LOGIC
		LOCAL oDesign AS DBEDesignDBServer
		LOCAL aDesign AS ArrayList
		LOCAL n AS INT

		IF .not. IsNameValid(SELF:oMainDesign)
			Funcs.WarningBox("DBServer name is not valid." , "DBServer Editor")
			RETURN FALSE
		END IF
		IF .not. IsFileNameValid(SELF:oMainDesign:GetProperty("filename"):TextValue)
			Funcs.WarningBox("Filename is not valid." , "DBServer Editor")
			RETURN FALSE
		END IF

		aDesign := SELF:GetAllDesignItems(DBServerItemType.Field)
		IF aDesign:Count == 0
			Funcs.WarningBox("No fields included in the DBServer" , "DBServer Editor")
			RETURN FALSE
		ENDIF
		FOR n := 0 UPTO aDesign:Count - 1
			oDesign := (DBEDesignDBServer)aDesign[n]
			IF oDesign:GetProperty("fldname"):TextValue:Trim() == "" .or. oDesign:GetProperty("classname"):TextValue:Trim() == "" .or. ;
					oDesign:GetProperty("hlname"):TextValue:Trim() == "" .or. oDesign:GetProperty("type"):IsAuto .or. ;
					oDesign:GetProperty("len"):IsAuto .or. .not. IsNameValid(oDesign)
				Funcs.WarningBox("Field " + oDesign:Name + " has invalid values." , "DBServer Editor")
				RETURN FALSE
			END IF
		NEXT
		
		aDesign := SELF:GetAllDesignItems(DBServerItemType.Index)
		FOR n := 0 UPTO aDesign:Count - 1
			oDesign := (DBEDesignDBServer)aDesign[n]
			IF .not. IsFileNameValid(oDesign:GetProperty("filename"):TextValue)
				Funcs.WarningBox("Index Filename " + oDesign:GetProperty("filename"):TextValue + " is not valid." , "DBServer Editor")
				RETURN FALSE
			END IF
			IF .not. SELF:MultipleOrdersSupported .and. oDesign:aOrders:Count > 1
				Funcs.WarningBox("Index " + oDesign:GetProperty("filename"):TextValue + " has multiple orders." , "DBServer Editor")
				RETURN FALSE
			END IF
		NEXT
		
		
	RETURN TRUE
	

#define DBOI_EXPRESSION       2
#define DBOI_NAME             5
#define DBOI_INDEXNAME        7
#define DBOI_FULLPATH         20
#define DBOI_ISDESC           22
#define DBOI_ORDERCOUNT       44
	METHOD ImportDbfClicked(o AS OBJECT , ee AS EventArgs) AS VOID
		TRY
			SELF:ImportDbf()
		CATCH e AS Exception
			Funcs.ErrorBox("Error importing from dbf: " + e:Message , "DBServer Editor")
		END TRY
	RETURN
	METHOD ImportDbf() AS LOGIC
		LOCAL oDesign AS DBEDesignDBServer
		LOCAL oOrder AS DBEDesignDBServer
		LOCAL eResult AS DialogResult
		LOCAL oDlg AS OpenFileDialog
		LOCAL lDeleteOld AS LOGIC
		LOCAL cFileName AS STRING
		LOCAL cDriver AS STRING
//		LOCAL aIndexes AS ARRAY
		LOCAL aStruct AS List<OBJECT>
		LOCAL cType AS STRING
		LOCAL nType AS INT
		LOCAL d AS DWORD
		LOCAL n AS INT
		
		oDlg := OpenFileDialog{}
		oDlg:Filter := "Dbf files (*.dbf)|*.dbf"
		oDlg:Title := "Select dbf file to import"
		oDlg:Multiselect := FALSE
		IF oDlg:ShowDialog() != DialogResult.OK
			RETURN FALSE
		END IF
		cFileName := oDlg:FileName
		
		cDriver := SELF:oMainDesign:GetProperty("rdd"):TextValue:Trim():ToUpper()
		IF cDriver == String.Empty
			cDriver := "DBFNTX"
		END IF
		DBHelpers.DBH_DBUseArea(cDriver , cFileName)
		aStruct := DBHelpers.DBH_DBStruct()
//		aStruct := {}
//		FOR d := 1 UPTO FCount()
//			AAdd(aStruct , {FieldName(d), DBFieldInfo(2,d), DBFieldInfo(3,d), DBFieldInfo(4,d)})
//		NEXT
		
/*		aIndexes := {}
		TRY
			LOCAL nOrders AS INT
			IF DBOrderInfo(DBOI_NAME , 1) != NIL .and. .not. String.IsNullOrEmpty(DBOrderInfo(DBOI_INDEXNAME , 1))
				AAdd(aIndexes , {DBOrderInfo(DBOI_INDEXNAME , 1) , DBOrderInfo(DBOI_FULLPATH , 1) , {} })
				nOrders := DBOrderInfo(DBOI_ORDERCOUNT , 1)
				FOR n := 1 UPTO nOrders
					AAdd(aIndexes[1,3] , {DBOrderInfo(DBOI_NAME , NIL , n) , DBOrderInfo(DBOI_EXPRESSION , NIL , n)})
				NEXT
			END IF
		CATCH
			ASize(aIndexes , 0)
		END TRY*/
		
		DBHelpers.DBH_DBCloseArea()

		IF SELF:oFieldList:Items:Count != 0
			eResult := MessageBox.Show("Remove existing fields" , "Import dbf" , MessageBoxButtons.YesNoCancel , MessageBoxIcon.Question)
			DO CASE
			CASE eResult == DialogResult.Cancel
				RETURN FALSE
			CASE eResult == DialogResult.Yes
				lDeleteOld := TRUE
			END CASE
		ENDIF

		SELF:BeginAction()
		
		IF lDeleteOld
			DO WHILE SELF:oFieldList:Items:Count != 0
				SELF:StartAction(DesignerBasicActionType.Remove , ActionData{((DBEDesignListViewItem)SELF:oFieldList:Items[0]):oDesign:cGuid})
			END DO
		END IF
		
		FOREACH oRecord AS OBJECT[] IN aStruct
			oDesign := (DBEDesignDBServer)SELF:StartAction(DesignerBasicActionType.Create , ActionData{NULL , SELF:oFieldList:Items:Count:ToString() , DBServerItemType.Field})
			oDesign:InitValues((STRING)oRecord[1])
			SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{oDesign:cGuid , "fldName" , (STRING)oRecord[1]})
			cType := Left((STRING)oRecord[2],1)
			DO CASE
			CASE cType == "C"
				nType := 0
			CASE cType == "N"
				nType := 1
			CASE cType == "D"
				nType := 2
			CASE cType == "L"
				nType := 3
			CASE cType == "M"
				nType := 4
			OTHERWISE
				nType := 0
			END CASE
			SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{oDesign:cGuid , "type" , nType})
			SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{oDesign:cGuid , "len" , (INT)oRecord[3]})
			SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{oDesign:cGuid , "dec" , (INT)oRecord[4]})
		NEXT

		SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{SELF:oMainDesign:cGuid , "filename" , cFileName})
		
/*		IF ALen(aIndexes) != 0
			DO WHILE SELF:oIndexList:Items:Count != 0
				SELF:StartAction(DesignerBasicActionType.Remove , ActionData{((DBEDesignListViewItem)SELF:oIndexList:Items[0]):oDesign:cGuid})
			END DO
			TRY
				oDesign := (DBEDesignDBServer)SELF:StartAction(DesignerBasicActionType.Create , ActionData{NULL , "0" , DBServerItemType.Index})
				oDesign:InitValues((STRING)aIndexes[1,1])
				SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{oDesign:cGuid , "filename" , (STRING)aIndexes[1,2]})
				FOR n := 1 UPTO ALen(aIndexes[1,3])
					oOrder := (DBEDesignDBServer)SELF:StartAction(DesignerBasicActionType.Create , ActionData{NULL , oDesign:aOrders:Count:ToString() , oDesign:cGuid})
					oOrder:InitValues((STRING)aIndexes[1,3,n,1])
					SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{oOrder:cGuid , "keyexp" , (STRING)aIndexes[1,3,n,2]})
				NEXT
			END TRY
		END IF*/

		SELF:EndAction()
		
	RETURN TRUE

	METHOD ExportDbfClicked(o AS OBJECT , ee AS EventArgs) AS VOID
		LOCAL lOldAnsi AS LOGIC
		lOldAnsi := DBHelpers.DBH_SetAnsi()
		TRY
			SELF:ExportDbf()
		CATCH e AS Exception
			Funcs.ErrorBox(e:Message , "Error Exporting to dbf")
		END TRY
		DBHelpers.DBH_SetAnsi(lOldAnsi)
	RETURN
	METHOD ExportDbf() AS LOGIC
		LOCAL oDesign AS DBEDesignDBServer
		LOCAL eResult AS DialogResult
		LOCAL oDlg AS SaveFileDialog
		LOCAL nLen,nDec,nType AS INT
		LOCAL aDesign AS ArrayList
		LOCAL cFileName AS STRING
		LOCAL cFolder AS STRING
		LOCAL lSuccess AS LOGIC
		LOCAL cDriver AS STRING
		LOCAL cBackup AS STRING
		LOCAL aFields AS List<OBJECT>
		LOCAL lImport AS LOGIC
		LOCAL cName AS STRING
		LOCAL cType AS STRING
		LOCAL n AS INT
		
		IF .not. SELF:CheckIfValid()
			RETURN FALSE
		END IF
		
		eResult := MessageBox.Show("Export dbf in ANSI format? (Select no for OEM)" , "Export dbf" , MessageBoxButtons.YesNoCancel , MessageBoxIcon.Question)
		DO CASE
		CASE eResult == DialogResult.Cancel
			RETURN FALSE
		CASE eResult == DialogResult.Yes
			DBHelpers.DBH_SetAnsi(TRUE)
		CASE eResult == DialogResult.No
			DBHelpers.DBH_SetAnsi(FALSE)
		END CASE
		
		aDesign := SELF:GetAllIncludedFields()
		cFileName := SELF:oMainDesign:GetProperty("FileName"):TextValue:Trim()
		IF cFileName == ""
			cFileName := SELF:oMainDesign:Name
		ENDIF
		
		IF cFileName:Contains(":")
			cFolder := FileInfo{cFileName}:DirectoryName
		ELSE
			cFolder := ""
		END IF
		IF cFolder == "" .or. ! System.IO.Directory.Exists(cFolder)
			oDlg := SaveFileDialog{}
			oDlg:FileName := FileInfo{cFileName}:Name
			oDlg:OverwritePrompt := FALSE
			oDlg:Filter := "Dbf files (*.dbf)|*.dbf"
			IF oDlg:ShowDialog() == DialogResult.OK
				cFileName := oDlg:FileName
				cFolder := FileInfo{cFileName}:DirectoryName
			ELSE
				RETURN FALSE
			END IF
		END IF
		
		IF File.Exists(cFileName)
			IF .not. Funcs.QuestionBox("File " + cFileName + " already exists. Overwrite ?" , "DBServer Editor")
				RETURN FALSE
			END IF
//			File.Delete(cFileName)
			TRY
				LOCAL oInfo AS FileInfo
				oInfo := FileInfo{cFileName}
				cBackup := oInfo:DirectoryName + "\$" + oInfo:Name
				File.Delete(cBackup)
				File.Move(cFileName , cBackup)
				lImport := TRUE
			END TRY
		END IF
		
		aFields := List<OBJECT>{}
		FOR n := 0 UPTO aDesign:Count - 1
			oDesign := (DBEDesignDBServer)aDesign[n]
			cName := oDesign:Name
			nLen := (INT)oDesign:GetProperty("Len"):Value
			nDec := (INT)oDesign:GetProperty("Dec"):Value
			nType := (INT)oDesign:GetProperty("Type"):Value
			DO CASE
			CASE nType==0
				cType := "C"
			CASE nType==1
				cType := "N"
			CASE nType==2
				cType := "D"
			CASE nType==3
				cType := "L"
			CASE nType==4
				cType := "M"
			END CASE
//			AAdd(aFields , {cName , cType , nLen , nDec})
			aFields:Add(<OBJECT>{cName , cType , nLen , nDec})
		NEXT
		
		cDriver := SELF:oMainDesign:GetProperty("rdd"):TextValue:Trim():ToUpper()
		IF cDriver == String.Empty
			cDriver := "DBFNTX"
		END IF

		DBHelpers.DBH_RDDInfo(104,FALSE) // _SET_AUTOOPEN
		TRY
			lSuccess := DBHelpers.DBH_DBCreate(cFileName , aFields , cDriver)
			TRY
				IF DBHelpers.DBH_Used()
					DBHelpers.DBH_DBCloseArea()
				END IF
			END TRY
		CATCH e AS Exception
			Funcs.ErrorBox(e:Message , "Export to dbf failed.")
		END TRY
		
		IF lSuccess .and. lImport
			IF Funcs.QuestionBox("Import old data ?" , "Export To Dbf")
				LOCAL oProgressBar AS ProgressBarForm
				LOCAL aFieldMap AS List<DWORD>
				LOCAL cField AS STRING
				LOCAL dFCount AS DWORD
				LOCAL dRCount AS DWORD
				LOCAL d,dPos AS DWORD
				LOCAL nCount AS INT
				LOCAL lFirst AS LOGIC
				aFieldMap := List<DWORD>{}
				TRY
					DBHelpers.DBH_DBUseArea(cDriver , cFileName , "new")
					DBHelpers.DBH_DBUseArea(cDriver , cBackup , "old")
					DBHelpers.DBH_GoTop("old")
					lFirst := TRUE
					dFCount := DBHelpers.DBH_FCount("old")
					dRCount := DBHelpers.DBH_LastRec("old")
					oProgressBar := ProgressBarForm{"Importing old dbf data" , (INT)dRCount}
					nCount := 0
					DO WHILE .not. DBHelpers.DBH_EOF("old")
						DBHelpers.DBH_DBAppend("new", TRUE)
						FOR d := 1 UPTO dFCount
							IF lFirst
								cField := DBHelpers.DBH_FieldName("old", d)
								dPos := DBHelpers.DBH_FieldPos("new", cField)
								IF dPos == 0
									dPos := d
								END IF
							ELSE
								dPos := aFieldMap[(INT)d - 1]
							END IF
							IF dPos != 0
								TRY
									DBHelpers.DBH_FieldPut("new", dPos , DBHelpers.DBH_FieldGet("old", d))
								CATCH
									dPos := 0
								END TRY
							END IF
							IF lFirst
								aFieldMap:Add(dPos)
							END IF
						NEXT
						lFirst := FALSE
						DBHelpers.DBH_DBSkip("old")
						nCount ++
						IF nCount % 100 == 0
							oProgressBar:Show()
							Application.DoEvents()
							oProgressBar:Position(nCount)
							IF oProgressBar:Canceled
								EXIT
							END IF
						END IF
					END DO
				FINALLY
					DBHelpers.DBH_DBCloseAll()
					oProgressBar:DoClose()
				END TRY
			END IF
		END IF
		
		IF lSuccess
			SELF:BeginAction()
			IF SELF:oFileNameEdit:Text != cFileName
				SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{SELF:oMainDesign:cGuid , "filename" , cFileName})
			END IF
			IF SELF:GetAllDesignItems(DBServerItemType.Index):Count == 0
				Funcs.MessageBox("DBServer exported to file " + cFileName , "DBServer Editor")
			ELSE
				IF Funcs.QuestionBox("DBServer exported to file " + cFileName + e".\r\n\r\nExport index files as well ?" , "DBServer Editor")
					lSuccess := SELF:ExportIndexes(cFileName)
					IF lSuccess
						Funcs.MessageBox("Index files exported successfully." , "DBServer Editor")
					END IF
				END IF
			END IF
			SELF:EndAction()
		END IF
	RETURN TRUE

	METHOD ExportIndexes(cDbfFileName AS STRING) AS LOGIC
		LOCAL oDesign AS DBEDesignDBServer
		LOCAL aDesign AS ArrayList
		LOCAL lSuccess AS LOGIC
		LOCAL cDriver AS STRING
		LOCAL n AS INT

		cDriver := SELF:oMainDesign:GetProperty("rdd"):TextValue:Trim():ToUpper()
		IF cDriver == String.Empty
			cDriver := "DBFNTX"
		END IF

		aDesign := SELF:GetAllDesignItems(DBServerItemType.Index)
		lSuccess := TRUE
		FOR n := 0 UPTO aDesign:Count - 1
			oDesign := (DBEDesignDBServer)aDesign[n]
			TRY
				lSuccess := lSuccess .and. SELF:ExportIndex(oDesign , cDbfFileName , cDriver)
			CATCH e AS Exception
				Funcs.ErrorBox("An error occured while exporting index file " + oDesign:GetProperty("filename"):TextValue + e"\r\n\r\n" + e:Message , "DBServer Editor")
				lSuccess := FALSE
			END TRY
		NEXT
	RETURN lSuccess

	METHOD ExportIndex(oDesign AS DBEDesignDBServer) AS LOGIC
		LOCAL cDbfFileName AS STRING
		LOCAL cDriver AS STRING
		LOCAL lSuccess AS LOGIC
		cDriver := SELF:oMainDesign:GetProperty("rdd"):TextValue:Trim():ToUpper()
		IF cDriver == String.Empty
			cDriver := "DBFNTX"
		END IF
		cDbfFileName := SELF:oMainDesign:GetProperty("FileName"):TextValue:Trim()
		IF .not. (IsFileNameValid(cDbfFileName) .and. cDbfFileName:Contains(":") .and. File.Exists(cDbfFileName) )
			Funcs.WarningBox("No database found to create index." , "DBServer Editor")
			RETURN FALSE
		END IF
		TRY
			lSuccess := SELF:ExportIndex(oDesign , cDbfFileName , cDriver)
		CATCH e AS Exception
			Funcs.ErrorBox("An error occured while exporting index file " + oDesign:GetProperty("filename"):TextValue + e"\r\n\r\n" + e:Message , "DBServer Editor")
			lSuccess := FALSE
		END TRY
		IF lSuccess
			Funcs.MessageBox("Index file exported successfully." , "DBServer Editor")
		ELSE
			Funcs.MessageBox("Index file was not created." , "DBServer Editor")
		END IF
	RETURN lSuccess

	METHOD ExportIndex(oDesign AS DBEDesignDBServer , cDbfFileName AS STRING , cDriver AS STRING) AS LOGIC
		LOCAL oOrder AS DBEDesignDBServer
		LOCAL cFileName AS STRING
		LOCAL lSuccess AS LOGIC
		LOCAL cName AS STRING
		LOCAL cKey AS STRING
		LOCAL n AS INT
		
		DBHelpers.DBH_RDDInfo(104,FALSE) // _SET_AUTOOPEN
		TRY
//			DBUseArea( , cDriver , cDbfFileName , , FALSE , FALSE)
			DBHelpers.DBH_DBUseArea( cDriver , cDbfFileName )
			
			cFileName := oDesign:GetProperty("FileName"):TextValue
			IF ! cFileName:Contains(":")
				LOCAL cDirectory AS STRING
				cDirectory := FileInfo{cDbfFileName}:DirectoryName
				IF .not. cDirectory:EndsWith("\")
					cDirectory += "\"
				ENDIF
				cFileName := cDirectory + cFileName
			END IF
			IF File.Exists(cFileName)
				File.Delete(cFileName)
			END IF
			FOR n := 0 UPTO oDesign:aOrders:Count - 1
				oOrder := (DBEDesignDBServer)oDesign:aOrders[n]
				cName := oOrder:Name
				cKey := oOrder:GetProperty("KeyExp"):TextValue
	//			cFor := oOrder:GetProperty("ForExpression")
				TRY
					lSuccess := DBHelpers.DBH_DBCreateOrder(cName , cFileName , cKey)
				CATCH e1 AS Exception
					Funcs.ErrorBox("An error occured while creating order " + oOrder:Name + " for index file " + oDesign:GetProperty("filename"):TextValue + e"\r\n\r\n" + e1:Message , "DBServer Editor")
				END TRY
				IF .not. lSuccess
					EXIT
				END IF
			NEXT
			IF lSuccess
				SELF:BeginAction()
				SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{oDesign:cGuid , "filename" , cFileName})
				SELF:EndAction()
			END IF
		
		CATCH e AS Exception
			
			Funcs.ErrorBox("An error occured while exporting index file " + oDesign:GetProperty("filename"):TextValue + e"\r\n\r\n" + e:Message , "DBServer Editor")
	
		FINALLY
	
			DBHelpers.DBH_DBCloseArea()
	
		END TRY
	RETURN lSuccess
	
	METHOD FillOrders() AS VOID
		LOCAL oDesign AS DBEDesignDBServer
		LOCAL n AS INT
		SELF:lFillingOrders := TRUE
		SELF:oOrderList:Items:Clear()
		IF SELF:oIndexList:SelectedIndices:Count == 0
			SELF:lFillingOrders := FALSE
			RETURN
		ENDIF
		oDesign := ((DBEDesignListViewItem)SELF:oIndexList:SelectedItems[0]):oDesign
		FOR n := 0 UPTO oDesign:aOrders:Count - 1
			SELF:oOrderList:Items:Add(((DBEDesignDBServer)oDesign:aOrders[n]):oItem)
		NEXT
		SELF:lFillingOrders := FALSE
	RETURN

	METHOD PropertyGotUpdated(oDesign AS DBEDesignDBServer , oProp AS VODesignProperty) AS VOID
		SELF:ApplyProperty(oDesign , oProp)
	RETURN
	
	METHOD ApplyProperty(_oDesign AS DBEDesignDBServer , oProp AS VODesignProperty) AS VOID
		LOCAL oDesign AS DBEDesignDBServer
		LOCAL n AS INT

		oDesign :=(DBEDesignDBServer)_oDesign
		DO CASE
		CASE oDesign:eType == DBServerItemType.DBServer
			DO CASE
			CASE oProp:Name == "classname"
				SELF:oNameEdit:Text := oProp:TextValue
			CASE oProp:Name == "filename"
				SELF:oFileNameEdit:Text := oProp:TextValue
			CASE oProp:Name == "rdd"
				SELF:BeginAction()
				DO WHILE SELF:oIndexList:Items:Count != 0
					SELF:DeleteItem(((DBEDesignListViewItem)SELF:oIndexList:Items[SELF:oIndexList:Items:Count - 1]):oDesign)
				END DO
				SELF:EndAction()
			END CASE

		CASE oDesign:eType == DBServerItemType.Field
			DO CASE
			CASE oProp:Name == "classname"
				oDesign:oItem:SubItems[4]:Text := oProp:TextValue

				LOCAL oFieldSpec AS FSEDesignFieldSpec
				LOCAL lReadOnly AS LOGIC
				lReadOnly := ! SELF:IsFieldSpecInModule(oProp:TextValue) .and. SELF:IsAvailableFieldSpec(oProp:TextValue)
				FOR n := 6 UPTO oDesign:aProperties:Count - 1
					oDesign:GetProperty(n):lReadOnly := lReadOnly
				NEXT
				oFieldSpec := SELF:GetAvailableFieldSpec(oProp:TextValue)
				IF oFieldSpec != NULL
					SELF:CopyPropertyValues(oFieldSpec , oDesign)
					oDesign:oItem:SetValues()
				END IF

			CASE oProp:Name == "fldname"
				oDesign:oItem:Text := oProp:TextValue
			CASE oProp:Name == "type"
				oDesign:oItem:SubItems[1]:Text := oProp:TextValue
			CASE oProp:Name == "len"
				oDesign:oItem:SubItems[2]:Text := oProp:TextValue
			CASE oProp:Name == "dec"
				oDesign:oItem:SubItems[3]:Text := oProp:TextValue
			CASE oProp:Name == "included"
				IF (INT)oProp:Value == 0
					oDesign:oItem:ForeColor := Color.Red
				ELSE
					oDesign:oItem:ForeColor := Color.Black
				END IF

			CASE oProp:Name == "pos"
				IF (INT)oProp:Value != oDesign:oItem:Index + 1
					LOCAL oListView AS ListView
					oListView := oDesign:oItem:ListView
					oListView:Items:Remove(oDesign:oItem)
					IF (INT)oProp:Value - 1 > oListView:Items:Count .or. (INT)oProp:Value <= 0
						oListView:Items:Add(oDesign:oItem)
					ELSE
						oListView:Items:Insert((INT)oProp:Value - 1 , oDesign:oItem)
					END IF
				END IF

			END CASE
			
			IF System.Array.IndexOf(<STRING>{"fldname","pos","included","caption","description","classname"} , oProp:Name) == -1
				LOCAL oTestField AS DBEDesignDBServer
				LOCAL aDesign AS ArrayList
				aDesign := SELF:GetAllDesignItems(DBServerItemType.Field)
				FOR n := 0 UPTO aDesign:Count - 1
					oTestField := (DBEDesignDBServer)aDesign[n]
					IF oTestField != oDesign .and. oTestField:GetProperty("classname"):TextValue:ToUpper() == oDesign:GetProperty("classname"):TextValue:ToUpper()
						IF oProp:TextValue != oTestField:GetProperty(oProp:Name):TextValue
							SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{oTestField:cGuid , oProp:Name , oProp:Value})
						END IF
					END IF
				NEXT
				
			END IF

		CASE oDesign:eType == DBServerItemType.Index
			DO CASE
			CASE oProp:Name == "filename"
				oDesign:oItem:Text := oProp:TextValue
				IF ! SELF:MultipleOrdersSupported
					IF oDesign:aOrders:Count == 1
						oDesign := (DBEDesignDBServer)oDesign:aOrders[0]
						SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{oDesign:cGuid , "tag" , NameFromFilename(oProp:TextValue)})
					END IF
				END IF
			END CASE

		CASE oDesign:eType == DBServerItemType.Order
			DO CASE
			CASE oProp:Name == "tag"
				oDesign:oItem:Text := oProp:TextValue
			CASE oProp:Name == "keyexp"
				oDesign:oItem:SubItems[1]:Text := oProp:TextValue
			END CASE

		END CASE
		
	RETURN

	METHOD GetAllIncludedFields() AS ArrayList
		LOCAL aDesign := ArrayList{} AS ArrayList
		LOCAL oDesign AS DBEDesignDBServer
		LOCAL n AS INT
		FOR n := 0 UPTO SELF:oFieldList:Items:Count - 1
			oDesign := ((DBEDesignListViewItem)SELF:oFieldList:Items[n]):oDesign
			IF oDesign:GetProperty("included"):TextValue != "0"
				aDesign:Add(oDesign)
			END IF
		NEXT
	RETURN aDesign		

	METHOD GetAllDesignItems(eType AS DBServerItemType) AS ArrayList
		LOCAL aDesign := ArrayList{} AS ArrayList
		LOCAL oDesign AS DBEDesignDBServer
		LOCAL n,m AS INT
		SWITCH eType
		CASE DBServerItemType.DBServer
			aDesign:Add(SELF:oMainDesign)
		CASE DBServerItemType.Field
			FOR n := 0 UPTO SELF:oFieldList:Items:Count - 1
				aDesign:Add(((DBEDesignListViewItem)SELF:oFieldList:Items[n]):oDesign)
			NEXT
		CASE DBServerItemType.Index
			FOR n := 0 UPTO SELF:oIndexList:Items:Count - 1
				oDesign := ((DBEDesignListViewItem)SELF:oIndexList:Items[n]):oDesign
				aDesign:Add(oDesign)
			NEXT
		CASE DBServerItemType.Order
			FOR n := 0 UPTO SELF:oIndexList:Items:Count - 1
				oDesign := ((DBEDesignListViewItem)SELF:oIndexList:Items[n]):oDesign
				FOR m := 0 UPTO oDesign:aOrders:Count - 1
					aDesign:Add((DBEDesignDBServer)oDesign:aOrders[m])
				NEXT
			NEXT
		END SWITCH
	RETURN aDesign		

	METHOD GetAllDesignItems() AS ArrayList
		LOCAL aDesign := ArrayList{} AS ArrayList
		LOCAL oDesign AS DBEDesignDBServer
		LOCAL n,m AS INT
		
		FOR n := 0 UPTO SELF:oFieldList:Items:Count - 1
			aDesign:Add(((DBEDesignListViewItem)SELF:oFieldList:Items[n]):oDesign)
		NEXT
		FOR n := 0 UPTO SELF:oIndexList:Items:Count - 1
			oDesign := ((DBEDesignListViewItem)SELF:oIndexList:Items[n]):oDesign
			aDesign:Add(oDesign)
			FOR m := 0 UPTO oDesign:aOrders:Count - 1
				aDesign:Add((DBEDesignDBServer)oDesign:aOrders[m])
			NEXT
		NEXT
	RETURN aDesign

	VIRTUAL METHOD GetDesignItemFromGuid(cGuid AS STRING) AS DBEDesignDBServer
		LOCAL oDesign AS DBEDesignDBServer
		LOCAL aItems AS ArrayList
		LOCAL n AS INT
		IF SELF:oMainDesign:cGuid == cGuid
			RETURN SELF:oMainDesign
		ENDIF
		aItems := SELF:GetAllDesignItems()
		FOR n := 0 UPTO aItems:Count - 1
			oDesign := (DBEDesignDBServer)aItems[n]
			IF oDesign:cGuid == cGuid
				RETURN oDesign
			ENDIF
		NEXT
	RETURN NULL

	VIRTUAL METHOD GetIndexFromOrder(cGuid AS STRING) AS DBEDesignDBServer
		LOCAL oIndex AS DBEDesignDBServer
		LOCAL oOrder AS DBEDesignDBServer
		LOCAL n,m AS INT
		FOR n := 0 UPTO SELF:oIndexList:Items:Count - 1
			oIndex := ((DBEDesignListViewItem)SELF:oIndexList:Items[n]):oDesign
			FOR m := 0 UPTO oIndex:aOrders:Count - 1
				oOrder := (DBEDesignDBServer)oIndex:aOrders[m]
				IF oOrder:cGuid == cGuid
					RETURN oIndex
				ENDIF
			NEXT
		NEXT
	RETURN NULL

	METHOD GetSelected() AS ArrayList
		LOCAL aSelected := ArrayList{} AS ArrayList
		IF SELF:oLastDesign == NULL
			aSelected:Add(SELF:oMainDesign)
		ELSE
			aSelected:Add(SELF:oLastDesign)
		END IF
	RETURN aSelected

	METHOD DisplayProperties() AS VOID
		SELF:oGrid:Fill(SELF:GetSelected())
	RETURN

	METHOD EditKeyDown(o AS OBJECT,e AS KeyEventArgs) AS VOID
		DO CASE
		CASE SELF:StandAlone .and. e:KeyCode==Keys.Z .and. e:Modifiers==Keys.Control
			SELF:Undo()
		CASE SELF:StandAlone .and. e:KeyCode==Keys.Z .and. e:Modifiers==Keys.Control + Keys.Shift
			SELF:Redo()
		END CASE
	RETURN

	METHOD ItemLostFocus(o AS OBJECT,e AS EventArgs) AS VOID

		IF SELF:lLoading
			RETURN
		END IF
		
		IF !SELF:oSurface:ContainsFocus
			SELF:ShowHideTools(FALSE)
		ENDIF
		
		DO CASE
		CASE o == SELF:oNameEdit
			IF SELF:oNameEdit:Text:Trim() != SELF:oMainDesign:GetProperty("classname"):TextValue
				SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{SELF:oMainDesign:cGuid , "classname" , SELF:oNameEdit:Text:Trim()})
			END IF
		CASE o == SELF:oFileNameEdit
			IF SELF:oFileNameEdit:Text:Trim() != SELF:oMainDesign:GetProperty("filename"):TextValue
				SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{SELF:oMainDesign:cGuid , "filename" , SELF:oFileNameEdit:Text:Trim()})
			ENDIF
		END CASE
	RETURN
	
	METHOD ItemGotFocus(o AS OBJECT,e AS EventArgs) AS VOID
		
		IF SELF:lLoading
			RETURN
		END IF

    	SELF:ShowHideTools(TRUE)
    	IF SELF:oGrid:FindForm() != NULL
	    	SELF:oGrid:FindForm():Text := "DBServer Editor Properties" // TODO: need to use resource for that
    	END IF
		SELF:oGrid:PropertyModified := SELF:oPropertyUpdatedHandler
		SELF:oGrid:ControlKeyPressed := SELF:oControlKeyPressedHandler
		SELF:oGrid:RetrieveClassNames := SELF:oRetrieveClassNamesHandler
		SELF:oGrid:oActiveDesigner := SELF
		SELF:oGrid:UseHierarchy(FALSE)

		DO CASE
		CASE o == SELF:oNameEdit .or. o==SELF:oFileNameEdit .or. o==SELF:oExportButton .or. o==SELF:oImportButton
			SELF:oLastDesign:=SELF:oMainDesign
		CASE o == SELF:oFieldList .or. o == SELF:oOrderList .or. o == SELF:oIndexList
			IF ((ListView)o):SelectedItems:Count==0
				SELF:oLastDesign:=NULL
			ELSE
				SELF:oLastDesign:=((DBEDesignListViewItem)((ListView)o):SelectedItems[0]):oDesign
			END IF
		END CASE
		SELF:DisplayProperties()
	RETURN

	METHOD RetrieveClassNamesHandler(cClass AS STRING) AS STRING[]
		LOCAL aValues AS STRING[]
		LOCAL n AS INT
		IF SELF:oGrid:oActiveDesigner != SELF
			RETURN NULL
		END IF
		aValues := STRING[]{SELF:aAvailableFieldSpecs:Count}
		FOR n := 0 UPTO SELF:aAvailableFieldSpecs:Count - 1
			aValues[n + 1] := ((FSEDesignFieldSpec)SELF:aAvailableFieldSpecs[n]):Name
		NEXT
	RETURN aValues

	METHOD PropertyModifiedInGrid(cProp AS STRING , oValue AS OBJECT) AS VOID
		LOCAL aSelected AS ArrayList
		LOCAL oDesign AS DBEDesignDBServer
		LOCAL n AS INT
		
		IF cProp:ToUpper() == "TAG" .and. !SELF:MultipleOrdersSupported
			RETURN
		END IF
/*		IF cProp == "rdd"
			IF MessageBox.Show("Changing the driver will invalidate your indexes. Do you wish to proceed?" , "DBServer Editor" , MessageBoxButtons.YesNo , MessageBoxIcon.Question) != DialogResult.Yes
				RETURN
			END IF
		END IF*/
		
		aSelected := SELF:GetSelected()
		SELF:BeginAction()
		FOR n := 0 UPTO aSelected:Count - 1
			oDesign := (DBEDesignDBServer)aSelected[n]
			SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{oDesign:cGuid , cProp , oValue})
			IF cProp == "type"
				LOCAL nLength,nDecimal AS INT
				SWITCH (INT)oValue 
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

	VIRTUAL METHOD BeginAction() AS VOID
		IF SELF:nActionDepth == 0
			SELF:lDidAction := FALSE
			SELF:lClearUndoBuffer := FALSE
		ENDIF
		SELF:nActionDepth ++
	RETURN
	VIRTUAL METHOD EndAction() AS VOID
		LOCAL oAction AS DesignerBasicAction

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
	
			IF SELF:oIndexList:SelectedItems:Count != 0
				IF ((DBEDesignListViewItem)SELF:oIndexList:SelectedItems[0]):oDesign:aOrders:Count != SELF:oOrderList:Items:Count
					SELF:FillOrders()
				END IF
			END IF
	
			SELF:DisplayProperties()

            IF SELF:IsDirtyChanged != NULL
                SELF:IsDirtyChanged:Invoke(SELF , EventArgs{})
            ENDIF
            
		ENDIF
	
	RETURN

	VIRTUAL METHOD DoBasicAction(oAction AS DesignerBasicAction , eAction AS DesignerBasicActionType , uData AS ActionData) AS OBJECT
		LOCAL aProperties AS NameValueCollection
		LOCAL oUndo,oRedo AS DesignerBasicAction
		LOCAL oDesign AS DBEDesignDBServer
		LOCAL oParent AS DBEDesignDBServer
		LOCAL oItem AS DBEDesignListViewItem
		LOCAL oProp AS VODesignProperty
		LOCAL oListView AS DBEListView
		LOCAL cGuid AS STRING
		LOCAL oEnumerator AS IEnumerator
		LOCAL oNameValue AS NameValue
		LOCAL n,nIndex AS INT
		LOCAL oRet AS OBJECT
		LOCAL eType AS DBServerItemType

		SWITCH eAction

		CASE DesignerBasicActionType.Create
			cGuid := uData:cGuid
			IF cGuid == NULL
				cGuid := Guid.NewGuid():ToString()
			END IF
			DO CASE
			CASE uData:oData:GetType() == TypeOf(DBEDesignListViewItem)
				oItem := (DBEDesignListViewItem)uData:oData
				eType := oItem:eType
			CASE uData:oData:GetType() == TypeOf(STRING)
				eType := DBServerItemType.Order
				oItem := DBEDesignListViewItem{eType , SELF}
				oParent := SELF:GetDesignItemFromGuid((STRING)uData:oData)
			OTHERWISE
				eType := (DBServerItemType)uData:oData
				oItem := DBEDesignListViewItem{eType , SELF}
			END CASE
			IF uData:cData != NULL
				nIndex := Int32.Parse(uData:cData)
				DO CASE
				CASE eType == DBServerItemType.Field
					oListView := SELF:oFieldList
				CASE eType == DBServerItemType.Index
					oListView := SELF:oIndexList
				CASE eType == DBServerItemType.Order
					oListView := SELF:oOrderList
				END CASE
				IF eType != DBServerItemType.Order
					IF nIndex == - 1
						oListView:Items:Add(oItem)
					ELSE
						oListView:Items:Insert(nIndex , oItem)
					END IF
				END IF
			ELSE
				nIndex := oItem:Index
			END IF
			oDesign := oItem:oDesign
			oDesign:cGuid := cGuid
			oRet := oDesign
			IF oParent != NULL
				IF nIndex == - 1 .or. nIndex >= oParent:aOrders:Count
					oParent:aOrders:Add(oDesign)
				ELSE
					oParent:aOrders:Insert(nIndex , oDesign)
				END IF
			END IF			
			SELF:lDidAction := TRUE
			IF !oAction:lExecuted
				oUndo := DesignerBasicAction{TRUE}
				oUndo:eAction := DesignerBasicActionType.Remove
				oUndo:uData := ActionData{oDesign:cGuid}
				oAction:aUndo:Add(oUndo)
				
				oRedo := DesignerBasicAction{TRUE}
				oRedo:eAction := DesignerBasicActionType.Create
				IF eType == DBServerItemType.Order .and. oParent != NULL
					oRedo:uData := ActionData{cGuid , nIndex:ToString() , oParent:cGuid}
				ELSE
					oRedo:uData := ActionData{cGuid , nIndex:ToString() , eType}
				END IF
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
			nIndex := oDesign:oItem:Index
			oListView := (DBEListView)oDesign:oItem:ListView
			IF oListView != NULL
				IF ! oListView:SelectedItems:Contains(oDesign:oItem)
					oListView := NULL
				END IF
			END IF
			oDesign:oItem:Remove()
			IF oDesign:eType == DBServerItemType.Order
				oParent := SELF:GetIndexFromOrder(cGuid)
				IF oParent != NULL
					oParent:aOrders:Remove(oDesign)
				END IF
			END IF

			IF ! oAction:lExecuted
				IF oListView != NULL
					IF oListView:Items:Count > nIndex
						oListView:SelectedIndices:Add(nIndex)
					ELSEIF oListView:Items:Count > 0
						oListView:SelectedIndices:Add(oListView:Items:Count - 1)
					END IF
				END IF
			END IF
			
			SELF:lDidAction := TRUE
			IF !oAction:lExecuted
				aProperties := NameValueCollection{}
				FOR n := 0 UPTO oDesign:aProperties:Count - 1
					oProp := (VODesignProperty)oDesign:aProperties[n]
					IF TRUE //!oProp:IsAuto
						aProperties:Add(oProp:Name , oProp:Value)
					ENDIF
				NEXT

				oUndo := DesignerBasicAction{TRUE}
				oUndo:eAction := DesignerBasicActionType.Create
				IF oDesign:eType == DBServerItemType.Order .and. oParent != NULL
					oUndo:uData := ActionData{oDesign:cGuid , nIndex:ToString() , oParent:cGuid , aProperties}
				ELSE
					oUndo:uData := ActionData{oDesign:cGuid , nIndex:ToString() , oDesign:eType , aProperties}
				END IF
				oAction:aUndo:Add(oUndo)
				
				oRedo := DesignerBasicAction{TRUE}
				oRedo:eAction := DesignerBasicActionType.Remove
				oRedo:uData := ActionData{oDesign:cGuid}
				oAction:aRedo:Add(oRedo)
			ENDIF

		CASE DesignerBasicActionType.SetIndex
			LOCAL lModified AS LOGIC
			lModified := FALSE
			nIndex := (INT)uData:oData
			oDesign := SELF:GetDesignItemFromGuid(uData:cGuid)
			eType := oDesign:eType
			DO CASE
			CASE eType == DBServerItemType.Field
			CASE eType == DBServerItemType.Index
				oListView := SELF:oIndexList
				oItem := oDesign:oItem
				IF oItem:Index != nIndex
					n := oItem:Index
					oListView:Items:Remove(oItem)
					oListView:Items:Insert(nIndex , oItem)
					lModified := TRUE
				END IF
			CASE eType == DBServerItemType.Order
				oListView := SELF:oOrderList
				oItem := oDesign:oItem
				IF oItem:Index != nIndex
					n := oItem:Index
					oListView:Items:Remove(oItem)
					oListView:Items:Insert(nIndex , oItem)
					oParent := SELF:GetIndexFromOrder(oDesign:cGuid)
					oParent:aOrders:Remove(oDesign)
					oParent:aOrders:Insert(nIndex , oDesign)
					lModified := TRUE
				END IF
			END CASE

			IF lModified
				SELF:lDidAction := TRUE
				IF .not. oAction:lExecuted
					oUndo := DesignerBasicAction{TRUE}
					oUndo:eAction := DesignerBasicActionType.SetIndex
					oUndo:uData := ActionData{oDesign:cGuid , uData:cData , n}
					oAction:aUndo:Add(oUndo)
					
					oRedo := DesignerBasicAction{TRUE}
					oRedo:eAction := DesignerBasicActionType.SetIndex
					oRedo:uData := ActionData{oDesign:cGuid , uData:cData , uData:oData}
					oAction:aRedo:Add(oRedo)
				ENDIF
//				SELF:AddAffected(oDesign)
			ENDIF
	
		CASE DesignerBasicActionType.SetProperty
			oDesign := SELF:GetDesignItemFromGuid(uData:cGuid)
			oProp := oDesign:GetProperty(uData:cData)
			IF !oProp:TextValue == oProp:GetTextValue(uData:oData) .or. oProp:Name == "type"
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
            NOP
		CASE DesignerActionType.Undo
			SELF:Undo()
		CASE DesignerActionType.Redo
			SELF:Redo()

		END SWITCH

		SELF:EndAction()

	RETURN

	METHOD @@New() AS VOID
/*		SELF:oSurface:Controls:Add(SELF:oPanel)
		SELF:oGrid:PropertyModified := SELF:oPropertyUpdatedHandler
		SELF:oGrid:ControlKeyPressed := NULL*/
	RETURN
	
	VIRTUAL METHOD Cut() AS VOID
	RETURN
	VIRTUAL METHOD Copy() AS VOID
	RETURN
	VIRTUAL METHOD Paste() AS VOID
	RETURN
	VIRTUAL METHOD GiveFocus() AS VOID
		SELF:oNameEdit:Focus()
	RETURN

	METHOD ToggleInclude(oDesign AS DBEDesignDBServer) AS VOID
		IF oDesign:eType == DBServerItemType.Field
			SELF:BeginAction()
			SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{oDesign:cGuid , "included" , 1 - (INT)oDesign:GetProperty("included"):Value})
			SELF:EndAction()
		END IF
	RETURN

	METHOD DeleteItem(oDesign AS DBEDesignDBServer) AS VOID
		SELF:BeginAction()
		IF oDesign:eType == DBServerItemType.Index
			DO WHILE oDesign:aOrders:Count != 0
				SELF:StartAction(DesignerBasicActionType.Remove , ActionData{((DBEDesignDBServer)oDesign:aOrders[oDesign:aOrders:Count - 1]):cGuid})
			END DO
			IF SELF:GetAllDesignItems(DBServerItemType.Index):Count == 1
				IF (INT)SELF:oMainDesign:GetProperty("order"):Value == 1
					SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{SELF:oMainDesign:cGuid , "order" , 0})
				END IF
			END IF
		END IF
		SELF:StartAction(DesignerBasicActionType.Remove , ActionData{oDesign:cGuid})
		SELF:EndAction()
	RETURN

	METHOD ShowCode() AS VOID
		LOCAL oCode AS CodeContents
		LOCAL t AS TextBox
		LOCAL n AS INT
		LOCAL f AS Form

		f := Form{}
		f:Size := Size{800 , 500}
		f:ShowInTaskbar := FALSE
		f:Text := "Generated code"
		t := TextBox{}
		t:Multiline := TRUE
		t:ScrollBars := ScrollBars.Vertical
		t:Dock := DockStyle.Fill
		f:Controls:Add(t)

		VODBServerEditor.LoadTemplate(SELF:cLoadedDir)

		oCode := SELF:GetCodeContents()
		FOR n := 0 UPTO oCode:aClass:Count - 1
			t:Text += oCode:aClass[n] + e"\r\n"
		NEXT
		t:Text += e"\r\n"
		FOR n := 0 UPTO oCode:aConstructor:Count - 1
			t:Text += oCode:aConstructor[n] + e"\r\n"
		NEXT
		t:Text += e"\r\n"
		FOR n := 0 UPTO oCode:aFieldDesc:Count - 1
			t:Text += oCode:aFieldDesc[n] + e"\r\n"
		NEXT
		t:Text += e"\r\n"
		FOR n := 0 UPTO oCode:aIndexList:Count - 1
			t:Text += oCode:aIndexList[n] + e"\r\n"
		NEXT

		f:ShowDialog()
	RETURN

	METHOD GetCodeContents() AS CodeContents
		LOCAL oCode AS CodeContents
		LOCAL dbfpath,dbfname AS STRING
		LOCAL cLine AS STRING
		LOCAL n,m AS INT
		LOCAL aValues AS NameValueCollection
		LOCAL oProp AS DesignProperty
		LOCAL cValue AS STRING
		
		aValues := NameValueCollection{}
		FOR n := 0 UPTO SELF:oMainDesign:aProperties:Count - 1
			oProp := (DesignProperty)SELF:oMainDesign:aProperties[n]
			DO CASE
/*			CASE oProp:cEnumType == "YESNO"
				cValue := iif(oProp:ValueLogic , "TRUE" , "FALSE")*/
			CASE oProp:Name == "ro"
				cValue := iif((INT)oProp:Value == 1 , "TRUE" , "FALSE")
			CASE oProp:Name == "share"
				DO CASE
				CASE (INT)oProp:Value == 0
					cValue := "NIL"
				CASE (INT)oProp:Value == 1
					cValue := "TRUE"
				OTHERWISE
					cValue := "FALSE"
				END CASE
			CASE oProp:Name == "rdd"
				IF oProp:TextValue:Trim() == String.Empty
					cValue := "NIL"
				ELSE
					cValue := e"\"" + oProp:TextValue:Trim() + e"\""
				END IF
			CASE oProp:Name == "superclass"
				cValue := oProp:TextValue:Trim()
				IF cValue:Length == 0
					cValue := "DBServer"
				END IF
				
			OTHERWISE

				cValue := Funcs.TranslateCaption(oProp:TextValue , TRUE , FALSE)

			END CASE
			aValues:Add(oProp:Name , cValue)
		NEXT
		
		SplitFilename(SELF:oMainDesign:GetProperty("filename"):TextValue:Trim() , dbfpath , dbfname)

		aValues:Add("dbfpath" , dbfpath)
		aValues:Add("dbfname" , dbfname)
		
		aValues:Add("fieldcount" , SELF:GetAllIncludedFields():Count:ToString())
		aValues:Add("indexcount" , SELF:GetAllDesignItems(DBServerItemType.Index):Count:ToString())

		aValues:Add("hlname" , SELF:oMainDesign:GetProperty("classname"):TextValue)
		aValues:Add("declarations" , "")
		
		oCode := CodeContents{}
		
		FOR n := 0 UPTO VODBServerEditor.Template:aClass:Count - 1
			cLine := VODBServerEditor.Template:aClass[n]
			cLine := TranslateLine(cLine , aValues)
			oCode:aClass:Add(cLine)
		NEXT
		
		FOR n := 0 UPTO VODBServerEditor.Template:aInit:Count - 1
			cLine := VODBServerEditor.Template:aInit[n]
			cLine := TranslateLine(cLine , aValues)
			oCode:aConstructor:Add(cLine)
		NEXT

		FOR n := 0 UPTO VODBServerEditor.Template:aFieldDesc:Count - 1
			cLine := VODBServerEditor.Template:aFieldDesc[n]
			IF cLine:ToUpper():Contains("%FIELDDESC%")
				cLine := SELF:TranslateMacro(DBServerItemType.Field , NULL)
				TranslateLineToLines(cLine , oCode:aFieldDesc)
			ELSE
				cLine := TranslateLine(cLine , aValues)
				oCode:aFieldDesc:Add(cLine)
			END IF
		NEXT
		FOR n := 0 UPTO VODBServerEditor.Template:aIndexList:Count - 1
			cLine := VODBServerEditor.Template:aIndexList[n]
			IF cLine:ToUpper():Contains("%INDEXLIST%")
				cLine := SELF:TranslateMacro(DBServerItemType.Index , NULL)
				TranslateLineToLines(cLine , oCode:aIndexList)
			ELSE
				cLine := TranslateLine(cLine , aValues)
				oCode:aIndexList:Add(cLine)
			END IF
		NEXT

		LOCAL aAdditional AS List<STRING>
		FOR m := 0 UPTO VODBServerEditor.Template:aAdditional:Count - 1
			aAdditional := VODBServerEditor.Template:aAdditional[m]
			oCode:aAdditional:Add(List<STRING>{})
			FOR n := 0 UPTO aAdditional:Count - 1
				cLine := aAdditional[n]
				cLine := TranslateLine(cLine , aValues)
				oCode:aAdditional[m]:Add(cLine)
			NEXT
		NEXT
		
	RETURN oCode
	
	INTERNAL METHOD TranslateMacro(eType AS DBServerItemType , oIndex AS DBEDesignDBServer) AS STRING
		LOCAL oDesign AS DBEDesignDBServer
		LOCAL aDesign AS ArrayList
		LOCAL cRet := "" AS STRING
		LOCAL cMacro AS STRING
		LOCAL n,m AS INT
		LOCAL aValues AS NameValueCollection
		LOCAL oProp AS DesignProperty
		LOCAL cValue AS STRING
		LOCAL dbfpath,dbfname AS STRING

		SWITCH eType
		CASE DBServerItemType.Field
			aDesign := SELF:GetAllIncludedFields()
			cMacro := VODBServerEditor.Template:cFieldDesc
		CASE DBServerItemType.Index
			aDesign := SELF:GetAllDesignItems(DBServerItemType.Index)
			cMacro := VODBServerEditor.Template:cIndexList
		CASE DBServerItemType.Order
			aDesign := oIndex:aOrders
			cMacro := VODBServerEditor.Template:cOrderList
		END SWITCH

		FOR m := 0 UPTO aDesign:Count - 1
			oDesign := (DBEDesignDBServer)aDesign[m]
			aValues := NameValueCollection{}
			FOR n := 0 UPTO oDesign:aProperties:Count - 1
				oProp := (DesignProperty)oDesign:aProperties[n]
				DO CASE
				CASE oProp:Name == "hlname" .and. eType == DBServerItemType.Field
					// it appears that %hlname% tag is translated to %fldname% in VO
					cValue := oDesign:GetProperty("fldname"):TextValue // BIG BAD UGLY HACK
				CASE oProp:cEnumType == "YESNO"
					cValue := iif(oProp:ValueLogic , "TRUE" , "FALSE")
				CASE oProp:Name == "type"
					IF (INT)oProp:Value == 6
						cValue := "X"
					ELSE
						cValue := oProp:TextValue:Substring(0,1)
					END IF
					
				CASE oProp:Name == "pos"
					LOOP

				OTHERWISE
					cValue := oProp:TextValue
				END CASE
				aValues:Add(oProp:Name , cValue)
			NEXT
			
			aValues:Add("pos" , (m+1):ToString())

			DO CASE
			CASE eType == DBServerItemType.Index
				aValues:Add("orderlist" , SELF:TranslateMacro(DBServerItemType.Order , oDesign))
				SplitFilename(oDesign:GetProperty("filename"):TextValue:Trim() , dbfpath , dbfname)
				IF dbfpath == ""
					LOCAL temp AS STRING
					SplitFilename(SELF:oMainDesign:GetProperty("filename"):TextValue:Trim() , dbfpath , temp)
				END IF
				aValues:Add("indexname" , dbfname)
				aValues:Add("dbfpath" , dbfpath)
			END CASE
			
			cRet += TranslateLine(cMacro , aValues)

			DO CASE
			CASE eType == DBServerItemType.Order
				IF m < aDesign:Count - 1
//					cRet += " , ;\r\n"
					cRet += " , ;" + ChrW(0)
				END IF
			END CASE
			
		NEXT
		
	RETURN cRet

	INTERNAL STATIC METHOD TranslateLine(cLine AS STRING , aValues AS NameValueCollection) AS STRING
		LOCAL n , nAt AS INT
		LOCAL cUpper AS STRING
		LOCAL cRet AS STRING
		LOCAL nLength AS INT
		LOCAL cValue AS STRING
		LOCAL oNameValue AS NameValue
		LOCAL lDidAction AS LOGIC

		IF !cLine:Contains("%")
			RETURN cLine
		END IF
		
		cUpper := cLine:ToUpper()
		cRet := cLine
		REPEAT
			lDidAction := FALSE
			FOR n := 0 UPTO aValues:Count - 1
				oNameValue := aValues:Get(n)
				nAt := cUpper:IndexOf("%" + oNameValue:Name:ToUpper() + "%")
				IF nAt != - 1
					nLength := oNameValue:Name:Length + 2
				END IF
				IF nAt != -1
					cRet := cLine:Substring(0 , nAt)
					cValue := (STRING)oNameValue:Value
					
					// Ultra hack, template probably contains quotes that we need to remove
					IF cValue:StartsWith("LoadResString") .and. cRet:EndsWith(e"\"")
						cRet := cRet:Substring(0 , cRet:Length - 1)
						IF nAt + nLength < cLine:Length
							IF cLine[nAt + nLength] == '"'
								cLine := cLine:Substring(0 , nAt + nLength) + cLine:Substring(nAt + nLength + 1)
							END IF
						END IF
					END IF
					
					cRet += cValue
					IF nAt + nLength < cLine:Length
						cRet += cLine:Substring(nAt + nLength)
					END IF
					cLine := cRet
					cUpper := cLine:ToUpper()
					lDidAction := TRUE
				END IF
			NEXT
		UNTIL ! lDidAction // in case there are multiple occurences of the same tag
		
	RETURN cRet
	
	INTERNAL STATIC METHOD TranslateLineToLines(cLine AS STRING , aLines AS List<STRING>) AS VOID
		LOCAL cTemp AS STRING
		LOCAL nAt AS INT
/*		cLine := cLine:Replace("\r\n" , ChrW(0))
		cLine := cLine:Replace("\r" , ChrW(0))
		cLine := cLine:Replace("\n" , ChrW(0))
		cLine := cLine:Replace("\t" , Chr(9))
		cLine := cLine:Replace(e"\\\"" , e"\"")*/
		DO WHILE TRUE
			nAt := cLine:IndexOf(ChrW(0))
			IF nAt == -1
				EXIT
			END IF
			IF nAt > 0
				cTemp := cLine:Substring(0, nAt)
			ELSE
				cTemp := ""
			END IF
			aLines:Add(cTemp)
			IF nAt < cLine:Length - 1
				cLine := cLine:Substring(nAt + 1)
			ELSE
				cLine := ""
			END IF
		END DO
		aLines:Add(cLine)
	RETURN
	
	INTERNAL STATIC METHOD SplitFilename(cFullPath AS STRING , cFolder REF STRING , cFileName REF STRING) AS VOID
		LOCAL nAt AS INT
		nAt := cFullPath:LastIndexOf('\\')
		IF nAt == -1
			cFolder := ""
			cFileName := cFullPath
		ELSE
			cFolder := cFullPath:Substring(0 , nAt + 1)
			cFileName := cFullPath:Substring(nAt + 1)
		END IF
	RETURN

	METHOD InitReadOnlyProperties() AS VOID
		LOCAL oDesign AS DesignItem
		LOCAL aDesign AS ArrayList
		LOCAL nType AS INT
		LOCAL n AS INT
		aDesign := SELF:GetAllDesignItems(DBServerItemType.Field)
		FOR n := 0 UPTO aDesign:Count - 1
			oDesign := (DesignItem)aDesign[n]
			nType := (INT)oDesign:GetProperty("type"):Value
			IF nType >= 2
				oDesign:GetProperty("len"):lReadOnly := TRUE
			END IF
			IF nType != 1
				oDesign:GetProperty("dec"):lReadOnly := TRUE
			END IF
		NEXT
	RETURN

	METHOD Open(cFileName AS STRING) AS LOGIC
/*		LOCAL lSuccess AS LOGIC
		
		SELF:ReadAllAvailableFieldspecs(Funcs.GetModuleFilenameFromBinary(cFileName))
		SELF:aFilesToDelete:Clear()

		SELF:cLoadedDir := FileInfo{cFileName}:DirectoryName
		SELF:cDefaultFileName := cFileName
		SELF:lLoading := TRUE
		lSuccess := SELF:OpenXml(cFileName)
		IF .not. lSuccess
			TRY
				lSuccess := SELF:OpenVNdbs(cFileName)
			END TRY
		END IF
		SELF:lLoading := FALSE
		IF lSuccess
			SELF:oSurface:Controls:Add(SELF:oPanel)
			SELF:oGrid:PropertyModified := SELF:oPropertyUpdatedHandler
			SELF:oGrid:ControlKeyPressed := SELF:oControlKeyPressedHandler
			SELF:InitReadOnlyProperties()
			SELF:GiveFocus()
		ENDIF
	RETURN lSuccess*/
	RETURN TRUE

	METHOD Save(cFileName AS STRING , lVnfrmOnly AS LOGIC) AS LOGIC
/*		LOCAL oFieldSpec AS FSEDesignFieldSpec
		LOCAL oDesign AS DBEDesignDBServer
		LOCAL aFieldSpecs AS ArrayList
		LOCAL oPrgStream AS EditorStream
		LOCAL lSaveFieldSpecs AS LOGIC
		LOCAL oCode AS CodeContents
		LOCAL cFieldSpec AS STRING
		LOCAL aDesign AS ArrayList
		LOCAL aCode AS ArrayList
		LOCAL cModule AS STRING
		LOCAL lSuccess AS LOGIC
		LOCAL lAutoRecover AS LOGIC
		LOCAL n,m AS INT
		
		lAutoRecover := lVnfrmOnly .or. cFileName:ToUpper():Contains("~AUTORECOVER")

		IF SELF:oFileNameEdit:Focused
		   IF SELF:oFileNameEdit:Text:Trim() != SELF:oMainDesign:GetProperty("filename"):TextValue
				SELF:StartAction(DesignerBasicActionType.SetProperty , ActionData{SELF:oMainDesign:cGuid , "filename" , SELF:oFileNameEdit:Text:Trim()})
			ENDIF
		ENDIF
		
		IF lAutoRecover
			TRY
				SELF:SaveToXml(cFileName)
				lSuccess := TRUE
			END TRY
			RETURN lSuccess
		END IF

		IF .not. SELF:CheckIfValid()
			RETURN FALSE
		END IF

		IF !VODBServerEditor.LoadTemplate(SELF:cLoadedDir)
			RETURN FALSE
		ENDIF
		IF !VOFieldSpecEditor.LoadTemplate(SELF:cLoadedDir)
			RETURN FALSE
		ENDIF

		oCode := SELF:GetCodeContents()
		
        cModule := Funcs.GetModuleFilenameFromBinary(cFileName)
		SELF:ReadAllAvailableFieldspecs(cModule)
//		aFieldSpecs := VOFieldSpecEditor.ReadFieldSpecsInModule(cModule)
//		IF aFieldSpecs == NULL
//			aFieldSpecs := ArrayList{}
//		END IF
		aFieldSpecs := SELF:aFieldSpecsInModule

		aCode := ArrayList{}
		aDesign := SELF:GetAllDesignItems(DBServerItemType.Field)
		FOR n := 0 UPTO aDesign:Count - 1
			oDesign := (DBEDesignDBServer)aDesign[n]
			cFieldSpec := oDesign:GetProperty("classname"):TextValue
			cFieldSpec := cFieldSpec:Trim():ToUpper()
			IF SELF:IsFieldSpecInModule(cFieldSpec)
				lSaveFieldSpecs := TRUE
				FOR m := 0 UPTO aFieldSpecs:Count - 1
					oFieldSpec := (FSEDesignFieldSpec)aFieldSpecs[m]
					IF oFieldSpec:Name:ToUpper() == cFieldSpec
						aCode:Add(oFieldSpec)
						SELF:CopyPropertyValues(oDesign , oFieldSpec)
						EXIT
					END IF
				NEXT
			ELSEIF .not. SELF:IsAvailableFieldSpec(cFieldSpec) // New FieldSpec
				lSaveFieldSpecs := TRUE
				oFieldSpec := FSEDesignFieldSpec{NULL , NULL}
				aCode:Add(oFieldSpec)
				SELF:CopyPropertyValues(oDesign , oFieldSpec)
				aFieldSpecs:Add(oFieldSpec)
			END IF
		NEXT

		IF aFieldSpecs:Count != 0 .and. lSaveFieldSpecs
			VOFieldSpecEditor.SaveToXml(cModule + ".FieldSpecs.xsfs" , aFieldSpecs)
			SELF:oProject:AddItem(cModule + ".FieldSpecs.xsfs")
			FOR n := 0 UPTO aFieldSpecs:Count - 1
				oFieldSpec := (FSEDesignFieldSpec)aFieldSpecs[n]
				IF .not. String.IsNullOrEmpty(oFieldSpec:cVNfsFileName) .and. .not. oFieldSpec:cVNfsFileName:ToUpper():Contains(".FIELDSPECS.XSFSS")
					SELF:oProject:RemoveItem(oFieldSpec:cVNfsFileName)
					IF File.Exists(oFieldSpec:cVNfsFileName)
						TRY
							File.Delete(oFieldSpec:cVNfsFileName)
						END TRY
					END IF
				END IF
			NEXT
		END IF

		SELF:ReadAllAvailableFieldspecs(cModule)

		lVnfrmOnly := lVnfrmOnly .or. SELF:lStandalone
		oPrgStream := EditorStream{}
		IF SELF:GetSaveFileStreams(cFileName , oPrgStream , lVnfrmOnly)
			TRY
				SELF:SaveToXml(cFileName)
				IF !lVnfrmOnly
					SELF:SavePrg(oPrgStream , oCode , aCode)
				END IF
				lSuccess := TRUE
	
				IF !lVnfrmOnly
					SELF:nActionSaved := SELF:nAction
				END IF
			END TRY
		END IF
		
		IF lSuccess
			FOR n := 0 UPTO SELF:aFilesToDelete:Count - 1
				cFileName := SELF:aFilesToDelete[n]
				IF oProject:ItemExists(cFileName)
//					oProject:RemoveVOBinary(cFileName)
					oProject:RemoveItem(cFileName)
				END IF
				IF File.Exists(cFileName)
					TRY
						File.Delete(cFileName)
					END TRY
				END IF
			NEXT
			SELF:aFilesToDelete:Clear()
		END IF
		
	RETURN lSuccess
*/
	RETURN TRUE
	PROTECTED METHOD GetSaveFileStreams(cVNFrmFileName AS STRING , oPrgStream AS EditorStream, lVnfrmOnly AS LOGIC) AS LOGIC
		LOCAL cPrgFileName AS STRING
		LOCAL lSuccess AS LOGIC
		LOCAL lError AS LOGIC
		
		TRY

			cPrgFileName := Funcs.GetModuleFilenameFromBinary(cVNFrmFileName) + ".prg"
			lError := FALSE
			IF !lVnfrmOnly
				IF !File.Exists(cPrgFileName)
					Funcs.ErrorBox("File was not found : " + cPrgFileName , "DBServer Editor")
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

			MessageBox.Show(e:Message , "Error saving DBServer" , MessageBoxButtons.OK , MessageBoxIcon.Exclamation)
			lSuccess := FALSE

		END TRY

		IF !lSuccess
			IF oPrgStream:IsValid
				oPrgStream:Close()
			ENDIF
		END IF

	RETURN lSuccess
	
	STATIC METHOD IsNameValid(oDesign AS DBEDesignDBServer) AS LOGIC
	RETURN IsNameValid(oDesign:Name , oDesign:eType)
	STATIC METHOD IsNameValid(cName AS STRING , eType AS DBServerItemType) AS LOGIC
		LOCAL n AS INT
		cName := cName:ToUpper()
		DO CASE
		CASE eType == DBServerItemType.Field .or. eType == DBServerItemType.Order .or. eType == DBServerItemType.DBServer
			IF cName:Trim() == String.Empty
				RETURN FALSE
			END IF
			FOR n := 0 UPTO cName:Length - 1
				IF "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_":IndexOf(cName[n]) == -1
					RETURN FALSE
				END IF
			NEXT
		CASE eType == DBServerItemType.Index
			RETURN IsFileNameValid(cName)
		END CASE
	RETURN TRUE
	STATIC METHOD IsFileNameValid(cFileName AS STRING) AS LOGIC
		LOCAL lSuccess AS LOGIC
		TRY
			FileInfo{cFileName}
			lSuccess := TRUE
		END TRY
	RETURN lSuccess
	STATIC METHOD NameFromFilename(cFileName AS STRING) AS STRING
		LOCAL nAt AS INT
		nAt := cFileName:LastIndexOf('\\')
		IF nAt != -1
			cFileName := cFileName:Substring(nAt + 1)
		END IF
		nAt := cFileName:LastIndexOf('.')
		IF nAt != -1
			cFileName := cFileName:Substring(0 , nAt)
		END IF
	RETURN cFileName

	VIRTUAL METHOD ReadAllAvailableFieldspecs(cModule AS STRING) AS VOID
	RETURN
	METHOD IsFieldSpecInModule(cFieldSpec AS STRING) AS LOGIC
		LOCAL oDesign AS FSEDesignFieldSpec
		LOCAL n AS INT
		cFieldSpec := cFieldSpec:ToUpper()
		FOR n := 0 UPTO SELF:aFieldSpecsInModule:Count - 1
			oDesign := (FSEDesignFieldSpec)SELF:aFieldSpecsInModule[n]
			IF oDesign:Name:ToUpper() == cFieldSpec
				RETURN TRUE
			END IF
		NEXT
	RETURN FALSE
	METHOD IsAvailableFieldSpec(cFieldSpec AS STRING) AS LOGIC
	RETURN SELF:GetAvailableFieldSpec(cFieldSpec) != NULL
	METHOD GetAvailableFieldSpec(cFieldSpec AS STRING) AS FSEDesignFieldSpec
		LOCAL oDesign AS FSEDesignFieldSpec
		LOCAL n AS INT
		cFieldSpec := cFieldSpec:ToUpper()
		FOR n := 0 UPTO SELF:aAvailableFieldSpecs:Count - 1
			oDesign := (FSEDesignFieldSpec)SELF:aAvailableFieldSpecs[n]
			IF oDesign:Name:ToUpper() == cFieldSpec
				RETURN oDesign
			END IF
		NEXT
	RETURN NULL
	METHOD CopyPropertyValues(oSource AS DesignItem , oDest AS DesignItem) AS VOID
		LOCAL oProp AS DesignProperty
		LOCAL n AS INT
		FOR n := 0 UPTO oSource:aProperties:Count - 1
			oProp := oDest:GetProperty(((DesignProperty)oSource:aProperties[n]):Name)
			IF oProp != NULL
				oProp:Value := ((DesignProperty)oSource:aProperties[n]):Value
			END IF
		NEXT
	RETURN

END CLASS




CLASS DBServerSurface INHERIT Panel
	PROTECT oGroupBox2 AS GroupBox
	INTERNAL oIndexList AS DBEListView
	INTERNAL oOrderList AS DBEListView
	PROTECT oGroupBox3 AS GroupBox
	INTERNAL oFieldList AS DBEListView
	PROTECT oGroupBox1 AS GroupBox
	EXPORT oFileNameEdit AS TextBox
	EXPORT oNameEdit AS TextBox
	PROTECT oLabel1 AS Label
	PROTECT oLabel2 AS Label
	EXPORT oExportButton AS Button
	EXPORT oImportButton AS Button
	CONSTRUCTOR(oDBServerEditor AS VODBServerEditor)
		SUPER()

		SELF:Initialize(oDBServerEditor)

		SELF:oFieldList:Columns:Add("Field",120,HorizontalAlignment.Left)
		SELF:oFieldList:Columns:Add("Type",80,HorizontalAlignment.Left)
		SELF:oFieldList:Columns:Add("Len",50,HorizontalAlignment.Right)
		SELF:oFieldList:Columns:Add("Dec",50,HorizontalAlignment.Right)
		SELF:oFieldList:Columns:Add("FieldSpec",180,HorizontalAlignment.Left)
		
		SELF:oIndexList:Columns:Add("Index",160,HorizontalAlignment.Left)
		
		SELF:oOrderList:Columns:Add("Order",110,HorizontalAlignment.Left)
		SELF:oOrderList:Columns:Add("Key Expression",170,HorizontalAlignment.Left)
		
		SELF:oFieldList:FullRowSelect := TRUE
		SELF:oIndexList:FullRowSelect := TRUE
		SELF:oOrderList:FullRowSelect := TRUE

		SELF:Dock := DockStyle.Fill

	RETURN

	METHOD Initialize(oDBServerEditor AS VODBServerEditor) AS VOID
		SELF:Name := "VODBServerEditor"
		SELF:Location := Point{100,100}
		SELF:ClientSize := Size{456,373}
		SELF:Text := "Form"
		
		SELF:oGroupBox1:=GroupBox{}
		SELF:oGroupBox1:Name := "GroupBox1"
		SELF:oGroupBox1:Location := Point{8,8}
		SELF:oGroupBox1:Size := Size{540,72}
		SELF:oGroupBox1:Text := "&General"
		SELF:oGroupBox1:TabIndex := 0
		SELF:oGroupBox1:FlatStyle := FlatStyle.System
		SELF:Controls:Add(SELF:oGroupBox1)
		
		SELF:oExportButton:=Button{}
		SELF:oExportButton:Name := "ExportButton"
		SELF:oExportButton:Location := Point{452,40}
		SELF:oExportButton:Size := Size{72,20}
		SELF:oExportButton:Text := "&Export"
		SELF:oExportButton:TabIndex := 4
		SELF:oExportButton:FlatStyle := FlatStyle.System
		SELF:oGroupBox1:Controls:Add(SELF:oExportButton)
		
		SELF:oImportButton:=Button{}
		SELF:oImportButton:Name := "ImportButton"
		SELF:oImportButton:Location := Point{452,16}
		SELF:oImportButton:Size := Size{72,20}
		SELF:oImportButton:Text := "&Import"
		SELF:oImportButton:TabIndex := 3
		SELF:oImportButton:FlatStyle := FlatStyle.System
		SELF:oGroupBox1:Controls:Add(SELF:oImportButton)
		
		
		
		SELF:oNameEdit := TextBox{}
		SELF:oNameEdit:Name := "NameEdit"
		SELF:oNameEdit:Location := Point{88,16}
		SELF:oNameEdit:Size := Size{356,20}
		SELF:oNameEdit:TabIndex := 1
		SELF:oGroupBox1:Controls:Add(SELF:oNameEdit)
		SELF:oFileNameEdit := TextBox{}
		SELF:oFileNameEdit:Name := "FileNameEdit"
		SELF:oFileNameEdit:Location := Point{88,40}
		SELF:oFileNameEdit:Size := Size{356,20}
		SELF:oFileNameEdit:TabIndex := 2
		SELF:oGroupBox1:Controls:Add(SELF:oFileNameEdit)
		SELF:oLabel1:=Label{}
		SELF:oLabel1:Name := "Label1"
		SELF:oLabel1:Location := Point{16,16}
		SELF:oLabel1:Size := Size{56,16}
		SELF:oLabel1:Text := "&Name :"
		SELF:oLabel1:TabIndex := 0
		SELF:oLabel1:TextAlign := ContentAlignment.BottomLeft
		SELF:oGroupBox1:Controls:Add(SELF:oLabel1)
		SELF:oLabel2:=Label{}
		SELF:oLabel2:Name := "Label2"
		SELF:oLabel2:Location := Point{16,40}
		SELF:oLabel2:Size := Size{64,16}
		SELF:oLabel2:Text := "&File Name"
		SELF:oLabel2:TabIndex := 2
		SELF:oLabel2:TextAlign := ContentAlignment.BottomLeft
		SELF:oGroupBox1:Controls:Add(SELF:oLabel2)
		
		SELF:oGroupBox3:=GroupBox{}
		SELF:oGroupBox3:Name := "GroupBox3"
		SELF:oGroupBox3:Location := Point{8,236}
		SELF:oGroupBox3:Size := Size{540,132}
		SELF:oGroupBox3:Text := "Fields"
		SELF:oGroupBox3:TabIndex := 2
		SELF:oGroupBox3:Anchor := AnchorStyles.Left + AnchorStyles.Top + AnchorStyles.Bottom
		SELF:oGroupBox3:FlatStyle := FlatStyle.System
		SELF:Controls:Add(SELF:oGroupBox3)
		
		SELF:oFieldList:=DBEListView{DBServerItemType.Field , oDBServerEditor}
		SELF:oFieldList:Name := "FieldList"
		
	SELF:oFieldList:Location := Point{16,20}
//		SELF:oFieldList:Location := Point{16,50}
	SELF:oFieldList:Size := Size{508,100}
//		SELF:oFieldList:Size := Size{508,90}
		
		SELF:oFieldList:TabIndex := 0
		SELF:oFieldList:Anchor := AnchorStyles.Left + AnchorStyles.Top + AnchorStyles.Bottom
		SELF:oFieldList:View := View.Details
		SELF:oFieldList:GridLines := TRUE
		SELF:oFieldList:HideSelection := FALSE
		SELF:oFieldList:MultiSelect := FALSE
		SELF:oFieldList:HeaderStyle := ColumnHeaderStyle.Nonclickable
		SELF:oGroupBox3:Controls:Add(SELF:oFieldList)
		SELF:oGroupBox2:=GroupBox{}
		SELF:oGroupBox2:Name := "GroupBox2"
		SELF:oGroupBox2:Location := Point{8,88}
		SELF:oGroupBox2:Size := Size{540,140}
		SELF:oGroupBox2:Text := "&Indexes / Orders"
		SELF:oGroupBox2:TabIndex := 1
		SELF:oGroupBox2:FlatStyle := FlatStyle.System
		SELF:Controls:Add(SELF:oGroupBox2)
		
		SELF:oIndexList:=DBEListView{DBServerItemType.Index , oDBServerEditor}
		SELF:oIndexList:Name := "IndexList"
		SELF:oIndexList:Location := Point{16,20}
		SELF:oIndexList:Size := Size{192,108}
		SELF:oIndexList:TabIndex := 0
		SELF:oIndexList:View := View.Details
		SELF:oIndexList:GridLines := TRUE
		SELF:oIndexList:MultiSelect := FALSE
		SELF:oIndexList:HeaderStyle := ColumnHeaderStyle.Nonclickable
		SELF:oGroupBox2:Controls:Add(SELF:oIndexList)
		SELF:oOrderList := DBEListView{DBServerItemType.Order , oDBServerEditor}
		SELF:oOrderList:Name := "OrderList"
		SELF:oOrderList:Location := Point{216,20}
		SELF:oOrderList:Size := Size{308,108}
		SELF:oOrderList:TabIndex := 1
		SELF:oOrderList:View := View.Details
		SELF:oOrderList:GridLines := TRUE
		SELF:oOrderList:MultiSelect := FALSE
		SELF:oOrderList:HeaderStyle := ColumnHeaderStyle.Nonclickable
		SELF:oGroupBox2:Controls:Add(SELF:oOrderList)
	RETURN

END CLASS




CLASS DBEDesignDBServer INHERIT DesignItem
	EXPORT eType AS DBServerItemType
	EXPORT oItem AS DBEDesignListViewItem
	EXPORT aOrders AS ArrayList
	CONSTRUCTOR(_eType AS DBServerItemType , _oItem AS DBEDesignListViewItem , _oEditor AS VODBServerEditor)

		SUPER(_oEditor)

		LOCAL oProp AS VODesignProperty
		
		SELF:aPages := List<STRING>{}
		SELF:aPages:Add("General")
		SELF:eType := _eType
		SELF:oItem := _oItem
		SELF:aOrders := ArrayList{}

		DO CASE
		CASE SELF:eType == DBServerItemType.DBServer
			SELF:AddProperty(VODesignProperty{"classname","Name","",PropertyType.Text , PropertyStyles.ReadOnly})
/*			SELF:AddProperty(VODesignProperty{"dbfpath","Path","",PropertyType.Text})
			SELF:AddProperty(VODesignProperty{"dbfname","Filename","",PropertyType.Text})*/
			SELF:AddProperty(VODesignProperty{"filename","Filename","",PropertyType.Text})
			SELF:AddProperty(VODesignProperty{"hlcaption","Caption","",PropertyType.Text})
			SELF:AddProperty(VODesignProperty{"superclass","Inherit from Class","",PropertyType.Text})
			SELF:AddProperty(VODesignProperty{"hldescription","Description","",PropertyType.Text})
			SELF:AddProperty(VODesignProperty{"hlhelpcontext","Help Context","",PropertyType.Text})
			SELF:AddProperty(VODesignProperty{"share","Shared","","__genBOOL"})
			SELF:AddProperty(VODesignProperty{"ro","ReadOnly","","__genBOOL"})
			SELF:AddProperty(VODesignProperty{"rdd","Driver","",PropertyType.Text})
			SELF:AddProperty(VODesignProperty{"order","Default Index Order","",PropertyType.Numeric})
			SELF:AddProperty(VODesignProperty{"noaccass","No Access/Assin","","YESNO"})


		CASE SELF:eType == DBServerItemType.Field

			SELF:AddProperty(VODesignProperty{"fldname","Name","",PropertyType.Text})
			oProp := VODesignProperty{"pos","pos","",PropertyType.Numeric}
			oProp:cPage := "_Hidden"
			SELF:AddProperty(oProp)
			oProp := VODesignProperty{"included","included","",PropertyType.Numeric}
			oProp:cPage := "_Hidden"
			SELF:AddProperty(oProp)
			SELF:AddProperty(VODesignProperty{"caption","Caption","",PropertyType.Text , PropertyStyles.NoAuto})
			SELF:AddProperty(VODesignProperty{"description","Description","",PropertyType.Text , PropertyStyles.NoAuto})

			SELF:AddProperty(VODesignProperty{"classname" , "FieldSpec" , "" , PropertyType.Type , PropertyStyles.NoAuto + PropertyStyles.NoNULL})
			SELF:AddProperty(VODesignProperty{"superclass" , "Inherit from" , "" , PropertyType.Text , PropertyStyles.NoAuto})
			SELF:AddProperty(VODesignProperty{"hlname" , "FS Name" , "" , PropertyType.Text})
	
			SELF:AddProperty(VODesignProperty{"hlcaption" , "FS Caption" , "" , PropertyType.Text , PropertyStyles.NoAuto})
			SELF:AddProperty(VODesignProperty{"hldescription" , "FS Description" , "" , PropertyType.Text , PropertyStyles.NoAuto})
			SELF:AddProperty(VODesignProperty{"hlhelpcontext" , "FS Help Context" , "" , PropertyType.Text , PropertyStyles.NoAuto})
	
//			SELF:AddProperty(VODesignProperty{"Type","Type","Type","__fieldspecDataTypes",PropertyStyles.NoAuto})
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

		CASE SELF:eType == DBServerItemType.Index
			SELF:AddProperty(VODesignProperty{"name","Name","",PropertyType.Text})
			SELF:AddProperty(VODesignProperty{"filename","Filename","",PropertyType.Text})

		CASE SELF:eType == DBServerItemType.Order
			SELF:AddProperty(VODesignProperty{"tag","Tag","",PropertyType.Text})
			SELF:AddProperty(VODesignProperty{"duplicate","Duplicate Allowed","","YESNO" , PropertyStyles.NoAuto})
			SELF:AddProperty(VODesignProperty{"ascending","Ascending","","YESNO" , PropertyStyles.NoAuto})
			SELF:AddProperty(VODesignProperty{"keyexp","Key Expression","",PropertyType.Text , PropertyStyles.NoAuto})
			SELF:AddProperty(VODesignProperty{"forexp","For Expression","",PropertyType.Text , PropertyStyles.NoAuto})

		END CASE
	RETURN

	ACCESS Name AS STRING
		LOCAL cName AS STRING
		DO CASE
		CASE SELF:eType == DBServerItemType.DBServer
			cName := SELF:GetProperty("classname"):TextValue
		CASE SELF:eType == DBServerItemType.Field
			cName := SELF:GetProperty("fldname"):TextValue
		CASE SELF:eType == DBServerItemType.Index
			cName := SELF:GetProperty("name"):TextValue
		CASE SELF:eType == DBServerItemType.Order
			cName := SELF:GetProperty("tag"):TextValue
		END CASE
	RETURN cName

	METHOD InitValues(cName AS STRING) AS VOID
		SELF:oDesigner:BeginAction()
		DO CASE
		CASE SELF:eType == DBServerItemType.Field
			SELF:oDesigner:StartAction(DesignerBasicActionType.SetProperty , ActionData{SELF:cGuid , "fldname" , cName})
			SELF:oDesigner:StartAction(DesignerBasicActionType.SetProperty , ActionData{SELF:cGuid , "type" , 0})
			SELF:oDesigner:StartAction(DesignerBasicActionType.SetProperty , ActionData{SELF:cGuid , "len" , 10})
			SELF:oDesigner:StartAction(DesignerBasicActionType.SetProperty , ActionData{SELF:cGuid , "dec" , 0})

			SELF:oDesigner:StartAction(DesignerBasicActionType.SetProperty , ActionData{SELF:cGuid , "classname" , ((VODBServerEditor)SELF:oDesigner):Name + "_" + cName})
			SELF:oDesigner:StartAction(DesignerBasicActionType.SetProperty , ActionData{SELF:cGuid , "hlname" , cName})
			SELF:oDesigner:StartAction(DesignerBasicActionType.SetProperty , ActionData{SELF:cGuid , "hlcaption" , cName})
			SELF:oDesigner:StartAction(DesignerBasicActionType.SetProperty , ActionData{SELF:cGuid , "hlhelpcontext" , ((VODBServerEditor)SELF:oDesigner):Name + "_" + cName})

			SELF:oDesigner:StartAction(DesignerBasicActionType.SetProperty , ActionData{SELF:cGuid , "pos" , SELF:oItem:Index + 1})
			SELF:oDesigner:StartAction(DesignerBasicActionType.SetProperty , ActionData{SELF:cGuid , "included" , 1})

		CASE SELF:eType == DBServerItemType.Index
			SELF:oDesigner:StartAction(DesignerBasicActionType.SetProperty , ActionData{SELF:cGuid , "name" , ((VODBServerEditor)SELF:oDesigner):Name + "_" + VODBServerEditor.NameFromFilename(cName)})
			SELF:oDesigner:StartAction(DesignerBasicActionType.SetProperty , ActionData{SELF:cGuid , "filename" , cName})

		CASE SELF:eType == DBServerItemType.Order
			SELF:oDesigner:StartAction(DesignerBasicActionType.SetProperty , ActionData{SELF:cGuid , "tag" , cName})
		END CASE
		SELF:oDesigner:EndAction()
	RETURN
	
END CLASS




INTERNAL CLASS DBEListView INHERIT ListView
	PROTECT oEdit AS DBETextBox
	PROTECT oItemEdit AS DBEDesignListViewItem
	PROTECT oDBEditor AS VODBServerEditor
	PROTECT lAppending AS LOGIC
	PROTECT eType AS DBServerItemType
	PROTECT oDragPoint AS Point
	PROTECT oDragItem AS DBEDesignListViewItem
	CONSTRUCTOR(_eType AS DBServerItemType,_oDBEditor AS VODBServerEditor)
		SUPER()
		SELF:eType := _eType
		SELF:oDBEditor := _oDBEditor
		SELF:HideSelection := FALSE
		SELF:oEdit := DBETextBox{}
		SELF:oEdit:Visible := FALSE
		SELF:oEdit:AutoSize := FALSE
		SELF:Controls:Add(SELF:oEdit)
		SELF:AllowDrop := TRUE
		
		SELF:ContextMenu := ContextMenu{}
		SELF:ContextMenu:Popup += EventHandler{ SELF , @ContextMenuPopUp() }
		DO CASE
		CASE SELF:eType == DBServerItemType.Field
			SELF:ContextMenu:MenuItems:Add("Add Field" , EventHandler{ SELF , @ContextAdd() })
			SELF:ContextMenu:MenuItems:Add("Insert Field" , EventHandler{ SELF , @ContextInsert() })
			SELF:ContextMenu:MenuItems:Add("Delete Field" , EventHandler{ SELF , @ContextRemove() })
			SELF:ContextMenu:MenuItems:Add("-")
			SELF:ContextMenu:MenuItems:Add("Exclude Field" , EventHandler{ SELF , @ContextToggleInclude() })
		CASE SELF:eType == DBServerItemType.Index
			SELF:ContextMenu:MenuItems:Add("Add Index" , EventHandler{ SELF , @ContextAdd() })
			SELF:ContextMenu:MenuItems:Add("Insert Index" , EventHandler{ SELF , @ContextInsert() })
			SELF:ContextMenu:MenuItems:Add("Delete Index" , EventHandler{ SELF , @ContextRemove() })
			SELF:ContextMenu:MenuItems:Add("-")
			SELF:ContextMenu:MenuItems:Add("Export Index" , EventHandler{ SELF , @ContextExport() })
		CASE SELF:eType == DBServerItemType.Order
			SELF:ContextMenu:MenuItems:Add("Add Order" , EventHandler{ SELF , @ContextAdd() })
			SELF:ContextMenu:MenuItems:Add("Insert Order" , EventHandler{ SELF , @ContextInsert() })
			SELF:ContextMenu:MenuItems:Add("Delete Order" , EventHandler{ SELF , @ContextRemove() })
		END CASE
	RETURN
	METHOD ContextMenuPopUp(o AS OBJECT,e AS EventArgs) AS VOID
		LOCAL oDesign AS DBEDesignDBServer
		IF SELF:SelectedItems:Count != 0
			oDesign := ((DBEDesignListViewItem)SELF:SelectedItems[0]):oDesign
		END IF
		DO CASE
		CASE SELF:eType == DBServerItemType.Field
			SELF:ContextMenu:MenuItems[2]:Enabled := oDesign != NULL
			SELF:ContextMenu:MenuItems[4]:Enabled := oDesign != NULL
			IF oDesign != NULL
				SELF:ContextMenu:MenuItems[4]:Text := iif((INT)oDesign:GetProperty("included"):Value == 0 , "Include Field" , "Exclude Field")
			END IF
		CASE SELF:eType == DBServerItemType.Index
			SELF:ContextMenu:MenuItems[2]:Enabled := oDesign != NULL
			SELF:ContextMenu:MenuItems[4]:Enabled := oDesign != NULL
		CASE SELF:eType == DBServerItemType.Order
			SELF:ContextMenu:MenuItems[0]:Enabled := SELF:oDBEditor:MultipleOrdersSupported
			SELF:ContextMenu:MenuItems[1]:Enabled := SELF:oDBEditor:MultipleOrdersSupported
			SELF:ContextMenu:MenuItems[2]:Enabled := oDesign != NULL .and. SELF:Items:Count != 1
		END CASE
	RETURN
	METHOD ContextAdd(o AS OBJECT , e AS EventArgs) AS VOID
		SELF:Append()
	RETURN
	METHOD ContextInsert(o AS OBJECT , e AS EventArgs) AS VOID
		SELF:Insert()
	RETURN
	METHOD ContextRemove(o AS OBJECT , e AS EventArgs) AS VOID
		SELF:Delete()
	RETURN
	METHOD ContextToggleInclude(o AS OBJECT , e AS EventArgs) AS VOID
		SELF:ToggleInclude()
	RETURN
	METHOD ContextExport(o AS OBJECT , e AS EventArgs) AS VOID
		LOCAL oItem AS DBEDesignListViewItem
		IF SELF:SelectedItems:Count==0
			RETURN
		END IF
		IF SELF:eType == DBServerItemType.Index
			oItem := (DBEDesignListViewItem)SELF:SelectedItems[0]
			SELF:oDBEditor:ExportIndex(oItem:oDesign)
		END IF
	RETURN
	
	PROTECTED METHOD ProcessCmdKey(Msg REF Message,KeyData AS Keys) AS LOGIC
		IF KeyData == Keys.Escape
			RETURN FALSE
		END IF
	RETURN SUPER:ProcessCmdKey(REF Msg , KeyData)

	PROTECTED METHOD OnMouseDown(e AS MouseEventArgs) AS VOID
		DO CASE
		CASE e:Clicks==2 .and. e:Button==MouseButtons.Left
			SELF:ShowEdit(FALSE)
		CASE e:Clicks==1 .and. e:Button==MouseButtons.Left 
			IF SELF:SelectedItems:Count == 1
				SELF:oDragPoint := e:Location
			END IF
/*		CASE e:Clicks==1 .and. e:Button==MouseButtons.Left 
			IF SELF:Items:Count==0
				SELF:Append()
			ELSEIF SELF:GetItemAt(e:X,e:Y) == NULL
				SELF:Append()
			END IF*/
		END CASE
	RETURN
	PROTECTED METHOD OnMouseUp(e AS MouseEventArgs) AS VOID
		SELF:oDragPoint := Point{0,0}
		SELF:oDragItem := NULL
		DO CASE
		CASE e:Clicks==2 .and. e:Button==MouseButtons.Left
			SELF:ShowEdit(FALSE)
		CASE e:Clicks==1 .and. e:Button==MouseButtons.Left 
			IF SELF:Items:Count==0
				SELF:Append()
			ELSEIF SELF:GetItemAt(e:X,e:Y) == NULL
				SELF:Append()
			END IF
		END CASE
	RETURN
	PROTECTED METHOD OnMouseMove(e AS MouseEventArgs) AS VOID
		SUPER:OnMouseMove(e)
		IF SELF:lAppending .or. SELF:oEdit:Visible
			RETURN
		END IF
		IF SELF:oDragPoint:X != 0 .and. (Math.Abs(SELF:oDragPoint:X - e:X) > 2 .or. Math.Abs(SELF:oDragPoint:Y - e:Y) > 2)
			SELF:oDragItem := (DBEDesignListViewItem)SELF:SelectedItems[0]
			SELF:DoDragDrop(1 , DragDropEffects.Move)
		END IF
	RETURN
/*	PROTECTED METHOD OnMouseUp(e AS MouseEventArgs) AS VOID
		SELF:FindForm():Text += "A"
		RETURN
		IF SELF:lAppending
			SELF:oEdit:Focus()
		ELSE
			SUPER:OnMouseUp(e)
		END IF
	RETURN*/

	PROTECTED METHOD OnDragOver(e AS DragEventArgs) AS VOID
		LOCAL oItem AS DBEDesignListViewItem
		LOCAL oPoint AS Point
		SUPER:OnDragOver(e)
		e:Effect := DragDropEffects.None
		oPoint := SELF:PointToClient(Point{e:X,e:Y})
		oItem := (DBEDesignListViewItem)SELF:GetItemAt( oPoint:X , oPoint:Y )
		IF SELF:oDragItem != NULL .and. oItem != NULL
			e:Effect := DragDropEffects.Move
		END IF
	RETURN

	PROTECTED METHOD OnDragDrop(e AS DragEventArgs) AS VOID
		LOCAL oItem AS DBEDesignListViewItem
		LOCAL oPoint AS Point
		LOCAL nIndex AS INT
		LOCAL n AS INT

		SUPER:OnDragOver(e)
		
		IF SELF:oDragItem == NULL
			RETURN
		END IF
		
		oPoint := SELF:PointToClient(Point{e:X,e:Y})
		oItem := (DBEDesignListViewItem)SELF:GetItemAt( oPoint:X , oPoint:Y )
		IF oItem != NULL
			nIndex := oItem:Index
			IF nIndex != SELF:oDragItem:Index
				SELF:oDBEditor:BeginAction()

				DO CASE
				CASE SELF:eType == DBServerItemType.Field
					SELF:oDBEditor:StartAction(DesignerBasicActionType.SetProperty , ActionData{SELF:oDragItem:oDesign:cGuid , "pos" , nIndex + 1})
					FOR n := 0 UPTO SELF:Items:Count - 1
						oItem := (DBEDesignListViewItem)SELF:Items[n]
						SELF:oDBEditor:StartAction(DesignerBasicActionType.SetProperty , ActionData{oItem:oDesign:cGuid , "pos" , oItem:Index + 1})
					NEXT
				CASE SELF:eType == DBServerItemType.Index
					SELF:oDBEditor:StartAction(DesignerBasicActionType.SetIndex , ActionData{SELF:oDragItem:oDesign:cGuid , NULL , nIndex})
				CASE SELF:eType == DBServerItemType.Order
					SELF:oDBEditor:StartAction(DesignerBasicActionType.SetIndex , ActionData{SELF:oDragItem:oDesign:cGuid , NULL , nIndex})
				END CASE

				SELF:oDBEditor:EndAction()
			ENDIF
		ENDIF
		SELF:oDragItem := NULL
	RETURN

	PROTECTED METHOD OnGotFocus(e AS EventArgs) AS VOID
		SUPER:OnGotFocus(e)
		IF SELF:Items:Count == 0 .and. SELF:oItemEdit == NULL
			SELF:Append()
		END IF
	RETURN

	PROTECTED METHOD OnSelectedIndexChanged(e AS EventArgs) AS VOID
		SUPER:OnSelectedIndexChanged(e)
		IF SELF:eType == DBServerItemType.Order .and. SELF:oDBEditor:lFillingOrders
			RETURN
		END IF
		IF SELF:SelectedItems:Count!=0
			SELF:oDBEditor:oLastDesign := ((DBEDesignListViewItem)SELF:SelectedItems[0]):oDesign
			SELF:oDBEditor:DisplayProperties()
		END IF
		
		IF SELF:eType == DBServerItemType.Index
			SELF:oDBEditor:FillOrders()
		ENDIF
	RETURN
	
	PROTECTED METHOD OnKeyDown(e AS KeyEventArgs) AS VOID
		SUPER:OnKeyDown(e)
		DO CASE
		CASE e:KeyData == Keys.F2
			SELF:ShowEdit(FALSE)
		CASE e:KeyData == Keys.Enter
			SELF:Append()
		CASE e:KeyData == Keys.Insert
			SELF:Insert()
		CASE e:KeyData == Keys.Delete
			SELF:Delete()
		CASE e:KeyCode == Keys.Down
			IF SELF:SelectedIndices:Count == 1 .and. SELF:SelectedIndices[0] == SELF:Items:Count - 1
				SELF:Append()
			END IF
		CASE SELF:oDBEditor:Standalone .and. e:KeyCode == Keys.Z .and. e:Modifiers == Keys.Control
			SELF:oDBEditor:Undo()
		CASE SELF:oDBEditor:Standalone .and. e:KeyCode == Keys.Z .and. e:Modifiers == Keys.Control + Keys.Shift
			SELF:oDBEditor:Redo()
		END CASE
	RETURN

	METHOD ToggleInclude() AS VOID
		LOCAL oItem AS DBEDesignListViewItem
		IF SELF:SelectedItems:Count==0
			RETURN
		END IF
		IF SELF:eType == DBServerItemType.Field
			oItem := (DBEDesignListViewItem)SELF:SelectedItems[0]
			SELF:oDBEditor:ToggleInclude(oItem:oDesign)
		END IF
	RETURN
	
	METHOD Delete() AS VOID
		LOCAL oItem AS DBEDesignListViewItem
		IF SELF:SelectedItems:Count==0
			RETURN
		END IF
		IF SELF:eType == DBServerItemType.Order .and. ! SELF:oDBEditor:MultipleOrdersSupported
			RETURN
		END IF
		oItem := (DBEDesignListViewItem)SELF:SelectedItems[0]
		SELF:oDBEditor:DeleteItem(oItem:oDesign)
	RETURN

	METHOD Append() AS VOID

		IF SELF:lAppending .or. SELF:oItemEdit != NULL
			RETURN
		END IF
		
		IF SELF:eType == DBServerItemType.Order
			IF SELF:oDBEditor:oIndexList:SelectedItems:Count == 0
				RETURN
			ENDIF
			IF !SELF:oDBEditor:MultipleOrdersSupported
				RETURN
			ENDIF
		ENDIF
		
		SELF:Items:Add(DBEDesignListViewItem{SELF:eType,SELF:oDBEditor})
		SELF:SelectedItems:Clear()
		SELF:Items[SELF:Items:Count-1]:Selected := TRUE
		SELF:ShowEdit(TRUE)
	RETURN

	METHOD Insert() AS VOID
		LOCAL nIndex AS INT

		IF SELF:lAppending .or. SELF:oItemEdit != NULL
			RETURN
		END IF
		
		IF SELF:eType == DBServerItemType.Order
			IF SELF:oDBEditor:oIndexList:SelectedItems:Count == 0
				RETURN
			ENDIF
			IF ! SELF:oDBEditor:MultipleOrdersSupported
				RETURN
			ENDIF
		ENDIF

		IF SELF:SelectedItems:Count == 0
			RETURN
		END IF
		nIndex := SELF:SelectedItems[0]:Index
		SELF:Items:Insert(nIndex , DBEDesignListViewItem{SELF:eType,SELF:oDBEditor})
		SELF:SelectedItems:Clear()
		SELF:Items[nIndex]:Selected := TRUE
		SELF:ShowEdit(TRUE)
	RETURN

	METHOD ShowEdit(_lAppending AS LOGIC) AS VOID
		LOCAL oRect AS Rectangle
		LOCAL oItem AS DBEDesignListViewItem

		IF SELF:lAppending .or. SELF:oItemEdit != NULL
			RETURN
		END IF
		
		IF SELF:eType == DBServerItemType.Order .and. !SELF:oDBEditor:MultipleOrdersSupported
			RETURN
		END IF
		
		IF SELF:SelectedItems:Count == 0
			IF SELF:FocusedItem == NULL
				RETURN
			ELSE
				SELF:FocusedItem:Selected:=TRUE
			END IF
		END IF
		oItem := (DBEDesignListViewItem)SELF:SelectedItems[0]
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

	METHOD CancelEdit() AS VOID
		LOCAL nIndex AS INT

		IF SELF:oItemEdit==NULL
			RETURN
		END IF
		IF SELF:lAppending
			nIndex := SELF:oItemEdit:Index
			SELF:Items:Remove(SELF:oItemEdit)
			IF SELF:Items:Count > nIndex
				SELF:Items[nIndex]:Selected:=TRUE
				SELF:Items[nIndex]:Focused:=TRUE
			ELSEIF SELF:Items:Count!=0
				SELF:Items[SELF:Items:Count-1]:Selected:=TRUE
				SELF:Items[SELF:Items:Count-1]:Focused:=TRUE
			END IF
		ELSE
			SELF:oItemEdit:Selected:=TRUE
			SELF:oItemEdit:Focused:=TRUE
		END IF
		SELF:oItemEdit := NULL
		SELF:Focus()
		Application.DoEvents()
		SELF:lAppending:=FALSE
	RETURN

	METHOD AcceptEdit() AS VOID
		
		IF SELF:oItemEdit == NULL
			RETURN
		END IF
		
		IF ! VODBServerEditor.IsNameValid(SELF:oEdit:Text:Trim() , SELF:eType)
			SELF:oItemEdit:Focused := TRUE
			SELF:oItemEdit := NULL
			SELF:Focus()
			SELF:lAppending := FALSE
			RETURN
		END IF
		
		IF SELF:lAppending
			SELF:oDBEditor:BeginAction()
			DO CASE
			CASE SELF:eType == DBServerItemType.Field
				SELF:oDBEditor:StartAction(DesignerBasicActionType.Create , ActionData{NULL , NULL , SELF:oItemEdit})
			CASE SELF:eType == DBServerItemType.Index
				SELF:oDBEditor:StartAction(DesignerBasicActionType.Create , ActionData{NULL , NULL , SELF:oItemEdit})
			CASE SELF:eType == DBServerItemType.Order
				LOCAL oItem AS DBEDesignListViewItem
				oItem := (DBEDesignListViewItem)SELF:oDBEditor:oIndexList:SelectedItems[0]
				SELF:oItemEdit:oDesign := (DBEDesignDBServer)SELF:oDBEditor:StartAction(DesignerBasicActionType.Create , ActionData{NULL , SELF:oItemEdit:Index:ToString() , oItem:oDesign:cGuid})
			END CASE
			SELF:oItemEdit:oDesign:InitValues(SELF:oEdit:Text:Trim())
			
			IF SELF:eType == DBServerItemType.Index
				IF SELF:oDBEditor:GetAllDesignItems(DBServerItemType.Index):Count == 1
					IF (INT)SELF:oDBEditor:oMainDesign:GetProperty("order"):Value == 0
						SELF:oDBEditor:StartAction(DesignerBasicActionType.SetProperty , ActionData{SELF:oDBEditor:oMainDesign:cGuid , "order" , 1})
					END IF
				END IF

				LOCAL oOrder AS DBEDesignDBServer
				oOrder := (DBEDesignDBServer)SELF:oDBEditor:StartAction(DesignerBasicActionType.Create , ActionData{NULL , NULL , SELF:oItemEdit:oDesign:cGuid})
				SELF:oDBEditor:StartAction(DesignerBasicActionType.SetProperty , ActionData{oOrder:cGuid , "tag" , VODBServerEditor.NameFromFilename(SELF:oEdit:Text:Trim())})
				SELF:oDBEditor:FillOrders()
			END IF
			
			SELF:oDBEditor:EndAction()
		ELSE
			DO CASE
			CASE SELF:eType == DBServerItemType.Field
				SELF:oDBEditor:StartAction(DesignerBasicActionType.SetProperty , ActionData{SELF:oItemEdit:oDesign:cGuid , "fldname" , SELF:oEdit:Text:Trim()})
			CASE SELF:eType == DBServerItemType.Index
				SELF:oDBEditor:StartAction(DesignerBasicActionType.SetProperty , ActionData{SELF:oItemEdit:oDesign:cGuid , "filename" , SELF:oEdit:Text:Trim()})
			CASE SELF:eType == DBServerItemType.Order
				SELF:oDBEditor:StartAction(DesignerBasicActionType.SetProperty , ActionData{SELF:oItemEdit:oDesign:cGuid , "tag" , SELF:oEdit:Text:Trim()})
			END CASE
		END IF

		SELF:oItemEdit:SetValues()
		SELF:oItemEdit:Focused := TRUE
		SELF:oItemEdit := NULL
		SELF:Focus()
		SELF:lAppending := FALSE
	RETURN

	METHOD MoveUpDown(lUp AS LOGIC) AS VOID
/*		LOCAL oItem AS ListViewItem
		LOCAL nIndex AS INT
		IF SELF:SelectedItems:Count == 0
			RETURN
		ENDIF
		oItem := SELF:SelectedItems[0]
		nIndex := oItem:Index
		IF (lUp .and. nIndex == 0) .or. (!lUp .and. nIndex == SELF:Items:Count - 1)
			RETURN
		ENDIF
		SELF:Items:Remove(oItem)
		IF lUp
			SELF:Items:Insert(nIndex - 1 , oItem)
		ELSE
			SELF:Items:Insert(nIndex + 1 , oItem)
		ENDIF
		SELF:oDBEditor:SetModified(TRUE)
		SELF:oDBEditor:oIndexList:FillOrders()*/
	RETURN

END CLASS

CLASS DBEDesignListViewItem INHERIT ListViewItem
	EXPORT oDesign AS DBEDesignDBServer
	EXPORT oDBEditor AS VODBServerEditor
	EXPORT eType AS DBServerItemType
	CONSTRUCTOR(_eType AS DBServerItemType,_oDBEditor AS VODBServerEditor)
		SUPER()
		SELF:oDBEditor:=_oDBEditor
		SELF:eType:=_eType
		SELF:SubItems:Add("")
		SELF:SubItems:Add("")
		SELF:SubItems:Add("")
		SELF:SubItems:Add("")
		SELF:oDesign:=DBEDesignDBServer{SELF:eType,SELF,SELF:oDBEditor}
	RETURN
	CONSTRUCTOR(_eType AS DBServerItemType,_oDBEditor AS VODBServerEditor,_oDesign AS DBEDesignDBServer)
		SUPER()
		SELF:oDBEditor:=_oDBEditor
		SELF:eType:=_eType
		SELF:SubItems:Add("")
		SELF:SubItems:Add("")
		SELF:SubItems:Add("")
		SELF:SubItems:Add("")
		SELF:oDesign := _oDesign
		SELF:oDesign:oItem := SELF
		SELF:SetValues()
	RETURN
	METHOD SetValues() AS VOID
		DO CASE
		CASE SELF:eType == DBServerItemType.Field
			SELF:Text := SELF:oDesign:GetProperty("fldname"):TextValue
			SELF:SubItems[1]:Text:= SELF:oDesign:GetProperty("Type"):TextValue
			SELF:SubItems[2]:Text := SELF:oDesign:GetProperty("Len"):TextValue
			SELF:SubItems[3]:Text := SELF:oDesign:GetProperty("Dec"):TextValue
			SELF:SubItems[4]:Text := SELF:oDesign:GetProperty("classname"):TextValue
			IF (INT)SELF:oDesign:GetProperty("included"):Value == 0
				SELF:ForeColor := Color.Red
			ELSE
				SELF:ForeColor := Color.Black
			END IF
		CASE SELF:eType == DBServerItemType.Index
			SELF:Text := SELF:oDesign:GetProperty("filename"):TextValue
		CASE SELF:eType == DBServerItemType.Order
			SELF:Text := SELF:oDesign:GetProperty("tag"):TextValue
			SELF:SubItems[1]:Text := SELF:oDesign:GetProperty("keyexp"):TextValue
		END CASE
	RETURN
	
END CLASS





INTERNAL CLASS DBETextBox INHERIT TextBox
	PROTECT oList AS DBEListView
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
	METHOD Show(_oList AS DBEListView) AS VOID
		SUPER:Show()
		SELF:oList:=_oList
	RETURN

	PROTECTED METHOD OnKeyDown(e AS KeyEventArgs) AS VOID
		SUPER:OnKeyDown(e)
		DO CASE
		CASE e:KeyData==Keys.Enter
			IF SELF:Text:Trim()==""
				RETURN
			END IF
			SELF:oList:AcceptEdit()
			SELF:Hide()
		CASE e:KeyData==Keys.Escape
			SELF:Text := ""
			SELF:oList:CancelEdit()
			SELF:Hide()
		END CASE
	RETURN

	PROTECTED METHOD OnKeyPress(e AS KeyPressEventArgs) AS VOID
		SUPER:OnKeyPress(e)
		IF (INT)e:KeyChar == 27 .or. (INT)e:KeyChar == 13
			e:Handled:=TRUE
		END IF
	RETURN

END CLASS




CLASS DBServerCode
	EXPORT aClass,aInit AS List<STRING>
	EXPORT aFieldDesc,aIndexList AS List<STRING>
	EXPORT aAdditional AS List< List<STRING> >
	EXPORT aAccessAssign AS List< List<STRING> >
	EXPORT cFieldDesc AS STRING
	EXPORT cIndexList AS STRING
	EXPORT cOrderList AS STRING

	CONSTRUCTOR()
		SELF:Reset()
	RETURN
	
	INTERNAL METHOD Reset() AS VOID
		SELF:aClass := List<STRING>{}
		SELF:aInit := List<STRING>{}
		SELF:aFieldDesc := List<STRING>{}
		SELF:aIndexList := List<STRING>{}
		SELF:aAccessAssign := List< List<STRING> > {}
		SELF:aAdditional := List< List<STRING> > {}

		SELF:cFieldDesc := ""
		SELF:cIndexList := ""
		SELF:cOrderList := ""
	RETURN
	
	METHOD Read(cDirectory AS STRING) AS LOGIC
		SELF:Reset()
	RETURN SELF:ReadDed(cDirectory) .and. SELF:ReadFed(cDirectory)

	METHOD ReadDed(cDirectory AS STRING) AS LOGIC
		LOCAL oStream AS System.IO.StreamReader
		LOCAL cLine,cUpper AS STRING
		LOCAL aRead AS List<STRING>
		LOCAL cCavoWed AS STRING
		LOCAL cOrigDir AS STRING
		LOCAL lInMacros AS LOGIC
		LOCAL cSection AS STRING
		LOCAL nAt AS INT
		
        cOrigDir := cDirectory
        TRY
        	cCavoWed := cDirectory + "\Properties\CAVODED.TPL"
	        IF !System.IO.File.Exists(cCavoWed)
		        cCavoWed := cDirectory + "\CAVODED.TPL"
				IF !System.IO.File.Exists(cCavoWed)
					cDirectory := Directory.GetParent(cDirectory):FullName
					cCavoWed := cDirectory + "\CAVODED.TPL"
			        IF !System.IO.File.Exists(cCavoWed)
			        	cCavoWed := cDirectory + "\Properties\CAVODED.TPL"
				        IF !System.IO.File.Exists(cCavoWed) .and. Funcs.InstallTemplatesFolder != ""
				        	cCavoWed := Funcs.InstallTemplatesFolder  + "\CAVODED.TPL"
				        ENDIF
			        ENDIF
				ENDIF
			END IF
        END TRY
		IF !System.IO.File.Exists(cCavoWed)
			MessageBox.Show("File Cavoded.tpl was not found, please locate it on disk." , "DBServer Editor")
		ENDIF
		DO WHILE !System.IO.File.Exists(cCavoWed)
			LOCAL oDlg AS OpenFileDialog
			oDlg := OpenFileDialog{}
			oDlg:Filter := "CavoDED files (*.tpl)|*.tpl"
			oDlg:Title := "Open cavoded.tpl file"
			IF oDlg:ShowDialog() == DialogResult.OK
				cCavoWed := oDlg:FileName:ToLower()
            TRY
               IF cCavoWed:Contains("cavoded") .and. cCavoWed:EndsWith(".tpl")
                  File.Copy(cCavoWed , cOrigDir + "\cavoded.tpl" , FALSE)
               ENDIF
            END TRY
			ELSE
				RETURN FALSE
			ENDIF
		END DO
	
		IF ! System.IO.File.Exists(cCavoWed)
			RETURN FALSE
		ENDIF
			
	    oStream := System.IO.StreamReader{cCavoWed , System.Text.Encoding.GetEncoding(0)}
		DO WHILE oStream:Peek()!=-1
		    cLine := oStream:ReadLine()
		    IF cLine:Trim():StartsWith(";")
		    	LOOP
		    ENDIF
		    cUpper := cLine:Trim():ToUpper()
		    IF cUpper:StartsWith("[")
		    	cSection := cUpper
		    END IF
		    DO CASE
		    CASE cUpper == "[CLASS]"
		    	aRead := SELF:aClass
		    CASE cUpper == "[INIT]"
		    	aRead := SELF:aInit
		    CASE cUpper == "[FIELDDESC]"
		    	aRead := SELF:aFieldDesc
		    CASE cUpper == "[INDEXLIST]"
		    	aRead := SELF:aIndexList
		    CASE cUpper == "[MACROS]"
		    	lInMacros := TRUE
		    	aRead := NULL
//		    CASE cUpper == "[DBSERVER]"
		    CASE cUpper == "[PREINIT]" .or. cUpper == "[POSTINIT]"
		    	aRead := NULL
		    CASE cUpper:StartsWith("[")
//		    	aRead := NULL
		    	aRead := List<STRING>{}
		    	SELF:aAdditional:Add(aRead)
		    CASE lInMacros
		    	LOCAL cLeft , cRight AS STRING
		    	nAt := cLine:IndexOf("=")
		    	IF nAt != -1
		    		cLeft := cLine:Substring(0 , nAt):Trim():ToUpper()
		    		cRight := cLine:Substring(nAt + 1):Trim()
		    		DO CASE
		    		CASE cLeft == "FIELDDESC"
		    			SELF:cFieldDesc := cRight
		    			SELF:TranslateTokens(SELF:cFieldDesc)
		    		CASE cLeft == "INDEX"
		    			SELF:cIndexList := cRight
		    			SELF:TranslateTokens(SELF:cIndexList)
		    		CASE cLeft == "ORDER"
		    			cRight := cRight:Replace("[" , e"\"")
		    			cRight := cRight:Replace("]" , e"\"")
		    			SELF:cOrderList := cRight
		    			SELF:TranslateTokens(SELF:cOrderList)
		    		END CASE
		    	END IF
			OTHERWISE
				IF aRead != NULL
					nAt := cLine:ToUpper():LastIndexOf(" CLASS ")
					IF nAt > 10
						cLine := cLine:Substring(0 , nAt)
					END IF
//					IF cLine:ToUpper():StartsWith("METHOD") .and. cLine:ToUpper():Contains("INIT")
					IF cLine:ToUpper():StartsWith("METHOD") .and. (cLine:ToUpper():Contains(" INIT ") .or. cLine:ToUpper():Contains(" INIT("))
						nAt := cLine:ToUpper():IndexOf("INIT")
						cLine := "CONSTRUCTOR" + cLine:Substring(nAt + 4)
						aRead:Add(cLine)
					ELSE
						cLine := cLine:Replace("SUPER:Init" , "SUPER")
						IF cSection == "[INIT]"
							cLine := cLine:Replace("RETURN SELF" , "RETURN")
						END IF
						cLine := cLine:Replace("'\\'" , e"\"\\\\\"")
						aRead:Add(cLine)
					END IF
				ENDIF
		    ENDCASE
		END DO
		oStream:Close()
	
	RETURN TRUE

	METHOD TranslateTokens(cMacro REF STRING) AS VOID
		cMacro := cMacro:Replace("\r\n" , ChrW(0))
		cMacro := cMacro:Replace("\r" , ChrW(0))
		cMacro := cMacro:Replace("\n" , ChrW(0))
		cMacro := cMacro:Replace("\t" , Chr(9))
		cMacro := cMacro:Replace(e"\\\"" , e"\"")
	RETURN
		
	METHOD ReadFed(cDirectory AS STRING) AS LOGIC
		LOCAL oStream AS System.IO.StreamReader
		LOCAL cLine,cUpper AS STRING
		LOCAL aRead AS List<STRING>
		LOCAL cCavoWed AS STRING
		LOCAL cOrigDir AS STRING
		LOCAL nAt AS INT

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
			MessageBox.Show("File Cavofed.tpl was not found, please locate it on disk." , "DBServer Editor")
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
	
		IF ! System.IO.File.Exists(cCavoWed)
			RETURN FALSE
		ENDIF
			
	    oStream := System.IO.StreamReader{cCavoWed , System.Text.Encoding.GetEncoding(0)}
		DO WHILE oStream:Peek()!=-1
		    cLine := oStream:ReadLine()
		    IF cLine:Trim():StartsWith(";")
		    	LOOP
		    ENDIF
		    cUpper := cLine:Trim():ToUpper()
		    DO CASE
		    CASE cUpper == "[DBSERVER]"
		    	aRead := List<STRING>{}
		    	SELF:aAccessAssign:Add(aRead)
		    CASE cUpper:StartsWith("[")
		    	aRead := NULL
			OTHERWISE
				IF aRead != NULL
					nAt := cLine:ToUpper():LastIndexOf(" CLASS ")
					IF nAt > 10
						cLine := cLine:Substring(0 , nAt)
					END IF
					aRead:Add(cLine)
				ENDIF
		    ENDCASE
		END DO
		oStream:Close()
	
	RETURN TRUE
	
END CLASS

CLASS DBHelpers
	STATIC METHOD DBH_DBUseArea(cDriver AS STRING, cDbfFileName AS STRING) AS LOGIC
	RETURN DbUseArea(TRUE , cDriver , cDbfFileName , NIL , FALSE , FALSE)
	STATIC METHOD DBH_DBUseArea(cDriver AS STRING, cDbfFileName AS STRING, cAlias AS STRING) AS LOGIC
	RETURN DbUseArea(TRUE , cDriver , cDbfFileName , cAlias , FALSE , FALSE)
	STATIC METHOD DBH_DBCreate(cFileName AS STRING, oFields AS List<OBJECT>, cDriver AS STRING) AS LOGIC
		LOCAL aFields AS ARRAY
		aFields := {}
		FOREACH oRecord AS OBJECT[] IN oFields
			AAdd(aFields, {oRecord[1], oRecord[2], oRecord[3], oRecord[4]})
		NEXT
	RETURN DbCreate(cFileName , aFields , cDriver)
	STATIC METHOD DBH_DBCreateOrder(cName AS STRING, cFileName AS STRING, cKey AS STRING) AS LOGIC
	RETURN DbCreateOrder(cName , cFileName , cKey)
	
	STATIC METHOD DBH_DBStruct() AS List<OBJECT>
		LOCAL aStruct AS ARRAY
		LOCAL aRet AS List<OBJECT>
		aRet := List<OBJECT>{}
		aStruct := DbStruct()
		FOR LOCAL n := 1 AS DWORD UPTO ALen(aStruct)
			aRet:Add(<OBJECT>{aStruct[n,1], aStruct[n,2], aStruct[n,3], aStruct[n,4]})
		NEXT
	RETURN aRet

	STATIC METHOD DBH_DBCloseArea() AS LOGIC
	RETURN DbCloseArea()
	STATIC METHOD DBH_Used() AS LOGIC
	RETURN Used()
	STATIC METHOD DBH_SetAnsi() AS LOGIC
	RETURN SetAnsi()
	STATIC METHOD DBH_SetAnsi(lAnsi AS LOGIC) AS LOGIC
	RETURN SetAnsi(lAnsi)
	STATIC METHOD DBH_GoTop(cAlias AS STRING) AS LOGIC
		DbSelectArea(cAlias)
	RETURN DbGoTop()
	STATIC METHOD DBH_DBSkip(cAlias AS STRING) AS LOGIC
		DbSelectArea(cAlias)
	RETURN DbSkip()
	STATIC METHOD DBH_DBCloseAll() AS LOGIC
	RETURN DbCloseAll()
	STATIC METHOD DBH_FCount(cAlias AS STRING) AS DWORD
		DbSelectArea(cAlias)
	RETURN FCount()
	STATIC METHOD DBH_FieldPos(cAlias AS STRING, cField AS STRING) AS DWORD
		DbSelectArea(cAlias)
	RETURN FieldPos(cField)
	STATIC METHOD DBH_FieldName(cAlias AS STRING, dField AS DWORD) AS STRING
		DbSelectArea(cAlias)
	RETURN FieldName(dField)
	STATIC METHOD DBH_FieldGet(cAlias AS STRING, dField AS DWORD) AS OBJECT
		DbSelectArea(cAlias)
	RETURN FieldGet(dField)
	STATIC METHOD DBH_FieldPut(cAlias AS STRING, dField AS DWORD, oValue AS OBJECT) AS OBJECT
		DbSelectArea(cAlias)
	RETURN FieldPut(dField, oValue)
	STATIC METHOD DBH_LastRec(cAlias AS STRING) AS DWORD
		DbSelectArea(cAlias)
	RETURN LastRec()
	STATIC METHOD DBH_EOF(cAlias AS STRING) AS LOGIC
		DbSelectArea(cAlias)
	RETURN Eof()
	STATIC METHOD DBH_DBAppend(cAlias AS STRING, lRelease AS LOGIC) AS LOGIC
		DbSelectArea(cAlias)
	RETURN DbAppend(lRelease)

	STATIC METHOD DBH_RDDInfo(nOrdinal AS OBJECT, oNewVale AS OBJECT) AS LOGIC
	RETURN RddInfo(nOrdinal, oNewVale)

END CLASS
