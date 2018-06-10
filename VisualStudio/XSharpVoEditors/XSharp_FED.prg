USING System.Collections
USING System.Collections.Generic
USING System.Windows.Forms
USING System.IO
USING Xide
using XSharpModel
using XSharp.VODesigners

BEGIN NAMESPACE XSharp.VOEditors
CLASS XSharp_VOFieldSpecEditor INHERIT VOFieldSpecEditor
	PROTECT oXProject as XProject

	CONSTRUCTOR(_oSurface AS Control , _oGrid AS DesignerGrid )
		SUPER(_oSurface , _oGrid) 
	RETURN
	METHOD Open(cFileName as STRING) AS LOGIC
		LOCAL aFiles AS IList<XFile>
		LOCAL aFieldSpecs as ArrayList
		LOCAL lModule AS LOGIC
		LOCAL lMerge := FALSE AS LOGIC
		LOCAL oFile as XFile
		oFile := XSharpModel.XSolution.FindFile(cFileName)
		IF oFile == NULL_OBJECT
			LOCAL cPrgFileName as STRING
			cPrgFileName := XFuncs.GetModuleFilenameFromBinary(cFileName)+".PRG"
			oFile := XSharpModel.XSolution.FindFile(cPrgFileName)
			if (oFile != NULL_OBJECT)
				oXProject := oFile:Project
				XFuncs.EnsureFileNodeExists(oXProject, cFileName)
				oFile := XSharpModel.XSolution.FindFile(cFileName)
			ENDIF
		ENDIF
		if (oFile != NULL_OBJECT)
			oXProject := oFile:Project
		else
		endif
		if oXProject == NULL
			XFuncs.ErrorBox("Cannot find project for file "+cFileName)
			RETURN FALSE
		ENDIF

		SELF:oListView:Items:Clear()
		SELF:aFilesToDelete:Clear()
		SELF:lLoadedAsXml := FALSE

		aFieldSpecs := OpenXml(cFileName , SELF)
		IF aFieldSpecs:Count != 0
//			MessageBox.Show("Fieldspec definitions read from xml file: " + cFileName , "FieldSpec Editor")
			SELF:lLoadedAsXml := TRUE
		ENDIF
		
		IF aFieldSpecs:Count == 1 .and. aFieldSpecs[0] == NULL // empty template
			SELF:oSurface:Controls:Add(SELF:oListView)
			SELF:cLoadedDir := FileInfo{cFileName}:DirectoryName
			RETURN TRUE
		END IF
		aFiles := XFuncs.FindItemsOfType(oXProject, XFileType.VOFieldSpec, NULL)
		FOREACH oFs as XFile in aFiles
			local cFullName as STRING
			cFullName := oFS:FullPath
			lModule := Funcs.GetModuleFilenameFromBinary(cFullName):ToUpper() == Funcs.GetModuleFilenameFromBinary(cFileName):ToUpper()
			IF lModule
				IF cFullName:ToUpper():Contains(".FIELDSPECS.VNFS") .or. ;
					cFullName:ToUpper():Contains(".FIELDSPECS.XSFS")
					IF .not. SELF:lLoadedAsXml
						XFuncs.WarningBox(e"Please open FieldSpec definitions via FieldSpec Editor")
						RETURN FALSE
					END IF
				ELSE
					IF SELF:lLoadedAsXml .and. !lMerge
						IF .not. XFuncs.QuestionBox("Additional FieldSpec definitions were found in module, do you want to merge them ?" , "FieldSpec Editor")
							EXIT
						ELSE
							lMerge := TRUE
						END IF
					END IF
					VAR oDesign := VOFieldSpecEditor.OpenVNfs(oFs:Name , SELF)
					IF oDesign != NULL
						aFieldSpecs:Add(oDesign)
						SELF:aFilesToDelete:Add(oFs:Name)
					END IF
				END IF
			END IF
		NEXT

		SELF:oSurface:Controls:Add(SELF:oListView)

		SELF:cLoadedDir := FileInfo{cFileName}:DirectoryName
		IF aFieldSpecs:Count == 0
			RETURN FALSE
		END IF
		
		FOREACH  oDesign  as  FSEDesignFieldSpec in aFieldSpecs
			IF oDesign != NULL .and. .not. SELF:NameExists(oDesign:Name)
				oDesign:oItem:SetValues()
				SELF:oListView:Items:Add(oDesign:oItem)
			END IF
		NEXT
		
		SELF:LoadUsedFieldSpecNames()

		SELF:InitReadOnlyProperties()
		IF SELF:oListView:Items:Count != 0
			SELF:oListView:SelectedIndices:Add(0)
		END IF
		SELF:GiveFocus()

	RETURN TRUE
	

	PROTECTED METHOD GetSaveFileStreams(cVNfsFileName REF STRING , cPrgFileName REF STRING , ;
								oPrgStream AS EditorStream, lVnfrmOnly AS LOGIC) AS LOGIC
		LOCAL lSuccess AS LOGIC
		LOCAL lError AS LOGIC
		
		IF .not. SELF:lLoadedAsXml
			cVNfsFileName := Funcs.GetModuleFilenameFromBinary(cVNfsFileName) + ".Fieldspecs.xsfs"
			IF .not. VOFieldSpecEditor.lEditorCloseWarningShown
				IF .not. XFuncs.QuestionBox(e"By saving the editor contents, FieldSpec definitions will be written to file: \n\n" + cVNfsFileName + ;
										 e"\n\nwhich will be added as a new item to the Project. " + ;
										 e"The current editor will be closed and all selected .xsfs and .vnfs files will be removed from the project." + ;
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
					XFuncs.ErrorBox("File was not found : " + cPrgFileName)
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

			XFuncs.ErrorBox(e:Message )
			lSuccess := FALSE

		END TRY

		IF !lSuccess
			IF oPrgStream:IsValid
				oPrgStream:Close()
			ENDIF
		END IF

	RETURN lSuccess
	METHOD SavePrg(oStream AS XSharp_EditorStream , aDesign AS ArrayList) AS LOGIC
		LOCAL aPrevNames AS List<STRING>
		LOCAL oCode AS CodeContents
		VAR oGenerator := CodeGenerator{oStream:Editor}
		oGenerator:BeginCode()
		
		FOREACH oDesign as FSEDesignFieldSpec in aDesign
			oCode := GetCodeContents(oDesign)
			VAR cName := oDesign:GetProperty("classname"):TextValue
			oGenerator:WriteEntity(XIde.EntityType._Class	   , cName , cName , EntityOptions.AddUser, oCode:aClass)
			oGenerator:WriteEntity(XIde.EntityType._Constructor , cName , cName , EntityOptions.None, oCode:aConstructor)
			oGenerator:WriteEndClass(cName)
		NEXT
		
		aPrevNames := List<STRING>{}
		FOREACH cName as STRING IN SELF:aUsedFieldSpecNames
			aPrevNames:Add(cName)
		NEXT
		SELF:LoadUsedFieldSpecNames()
		FOREACH cName as STRING IN aPrevNames
			IF .not. SELF:aUsedFieldSpecNames:Contains(cName)
				oGenerator:DeleteEntity(XIde.EntityType._Class , cName , cName)
				oGenerator:DeleteEntity(XIde.EntityType._Constructor , cName , cName)
			END IF
		NEXT
		oGenerator:EndCode()
		oStream:Save()
	RETURN TRUE
 
	METHOD Save(cFileName AS STRING , lFsOnly AS LOGIC) AS LOGIC
		LOCAL oPrgStream AS XSharp_EditorStream
		LOCAL cOrigFilename AS STRING
		LOCAL cPrgFileName := "" AS STRING
		LOCAL aDesign AS ArrayList
		LOCAL lAutoRecover AS LOGIC
		LOCAL lSuccess := FALSE AS LOGIC

		lAutoRecover := lFsOnly .or. cFileName:ToUpper():Contains("~AUTORECOVER")
		IF lAutoRecover
			TRY
				aDesign := SELF:GetAllDesignItems()
				SaveToXml(cFileName , aDesign)
				lSuccess := TRUE
			END TRY
			RETURN lSuccess
		END IF

		IF .not. SELF:CheckIfValid()
			RETURN FALSE
		END IF
		
		cOrigFilename := cFileName

		IF !VOFieldSpecEditor.LoadTemplate(SELF:cLoadedDir)
			RETURN FALSE
		ENDIF
		lFsOnly := lFsOnly .or. SELF:lStandalone
		
		oPrgStream := XSharp_EditorStream{}

		IF SELF:GetSaveFileStreams(cFileName , REF cPrgFileName , oPrgStream , lFsOnly)
			aDesign := SELF:GetAllDesignItems()
			SaveToXml(cFileName , aDesign)
			IF !lFsOnly
				SELF:SavePrg(oPrgStream , aDesign)
			END IF
			lSuccess := TRUE
			SELF:nActionSaved := SELF:nAction
			SELF:ResetModified()
			XFuncs.EnsureFileNodeExists(oXProject, cFileName)
			FOREACH cDelete as STRING in SELF:aFilesToDelete
				IF FileInfo{cDelete}:FullName:ToUpper() != FileInfo{cOrigFilename}:FullName:ToUpper()
					XFuncs.DeleteFile(oXProject, cDelete)
				END IF
			NEXT
			SELF:aFilesToDelete:Clear()
			SELF:LoadUsedFieldSpecNames()
		ENDIF
		
	RETURN lSuccess

END CLASS

END NAMESPACE