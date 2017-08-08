USING System.Collections
USING System.Collections.Generic
USING System.Windows.Forms
USING System.IO
USING Xide
USING XSharpModel
STATIC DEFINE DefaultCaption  := "XSharp VO DbServer Editor"

BEGIN NAMESPACE XSharp.VOEditors
CLASS XSharp_VODbServerEditor INHERIT VODbServerEditor
	PROTECT oXProject as XProject

	CONSTRUCTOR(_oSurface AS Control , _oGrid AS DesignerGrid )
		SUPER(_oSurface , _oGrid) 
		oXProject  := NULL_OBJECT
	RETURN

	METHOD Open(cFileName as STRING) AS LOGIC
		LOCAL oFile as XFile
		local lOk as LOGIC
		oFile := XSharpModel.XSolution.FindFile(cFileName)
		if (oFile != NULL_OBJECT)
			oXProject := oFile:Project
		endif
		if oXProject == NULL
			XFuncs.ErrorBox("Cannot find project for file "+cFileName)
			RETURN FALSE
		ENDIF
		SELF:ReadAllAvailableFieldSpecs(Funcs.GetModuleFilenameFromBinary(cFileName))
		SELF:aFilesToDelete:Clear()
		SELF:cLoadedDir := FileInfo{cFileName}:DirectoryName
		SELF:cDefaultFileName := cFileName
		SELF:lLoading := TRUE
		lOk := SELF:OpenXML(cFileName)
		IF (! lOk)
			TRY
				lOk := SELF:OpenVNDBS(cFileName)
			CATCH
			END TRY
		ENDIF
		SELF:lLoading := FALSE
		IF lOk
			SELF:oSurface:Controls:Add(SELF:oPanel)
			SELF:oGrid:PropertyModified := self:oPropertyUpdatedHandler
			SELF:oGrid:ControlKeyPressed := SELF:oControlKeyPressedHandler
			SELF:InitReadonlyProperties()
			SELF:GiveFocus()
		ENDIF
		RETURN lOk
	METHOD ReadAllAvailableFieldSpecs(cModule as STRING) AS VOID
		local oBinaries as IList<XFile>
		SELF:aAvailableFieldSpecs:Clear()
		SELF:aFieldSpecsInModule:Clear()
		oBinaries := XFuncs.FindItemsOfType(oXProject, XFileType.VOFieldSpec, NULL)
		FOREACH oFile as XFile in oBinaries
			LOCAL cName as STRING
			LOCAL lInModule as LOGIC
			cName	  := oFile:FullPath
			lInModule := Funcs.GetModuleFileNameFromBinary(cName):ToUpper() == cModule:ToUpper()
			if cName:ToUpper():Contains(".FIELDSPECS.VNFS") .or. ;
				cName:ToUpper():Contains(".FIELDSPECS.XSFS")
				VAR aFS := VOFieldSpecEditor.OpenXML(cName, NULL)
				self:aAvailableFieldSpecs:AddRange(aFs)
				if (lInModule)
					SELF:aFieldSpecsInModule:AddRange(aFs)		
				endif
			ELSE
				LOCAL fs as FSEDesignFieldSpec
				fs := VOFieldSpecEditor.OPenVNFS(cName, (VOFieldSpecEditor) NULL)
				SELF:aAvailableFieldspecs:Add(fs)
				if (lInModule)
					SELF:aFieldSpecsInModule:Add(fs)		
				endif
			ENDIF			
		NEXT

	PROTECTED METHOD GetSaveFileStreams(cVNFrmFileName AS STRING , oPrgStream AS EditorStream, lVnfrmOnly AS LOGIC) AS LOGIC
		LOCAL cPrgFileName AS STRING
		LOCAL lSuccess AS LOGIC
		LOCAL lError AS LOGIC
		
		TRY

			cPrgFileName := Funcs.GetModuleFilenameFromBinary(cVNFrmFileName) + ".prg"
			lError := FALSE
			IF !lVnfrmOnly
				IF !File.Exists(cPrgFileName)
					XFuncs.ErrorBox("File was not found : " + cPrgFileName , "DBServer Editor")
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
	
	METHOD Save(cFileName AS STRING , lSrvOnly AS LOGIC) AS LOGIC
		LOCAL oPrgStream AS EditorStream
		LOCAL lSaveFieldSpecs := FALSE AS LOGIC
		LOCAL oCode AS CodeContents
		LOCAL cFieldSpec AS STRING
		LOCAL aCode AS ArrayList
		LOCAL cModule AS STRING
		LOCAL lSuccess := FALSE AS LOGIC
		LOCAL lAutoRecover AS LOGIC
		
		lAutoRecover := lSrvOnly .or. cFileName:ToUpper():Contains("~AUTORECOVER")

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
		VAR aFieldSpecs := SELF:aFieldSpecsInModule

		aCode := ArrayList{}
		VAR aDesign := SELF:GetAllDesignItems(DBServerItemType.Field)
		FOREACH oDesign as DBEDesignDBServer in aDesign
			cFieldSpec := oDesign:GetProperty("classname"):TextValue
			cFieldSpec := cFieldSpec:Trim():ToUpper()
			IF SELF:IsFieldSpecInModule(cFieldSpec)
				lSaveFieldSpecs := TRUE
				FOREACH oFieldSpec as FSEDesignFieldSpec in aFieldSpecs
					IF oFieldSpec:Name:ToUpper() == cFieldSpec
						aCode:Add(oFieldSpec)
						SELF:CopyPropertyValues(oDesign , oFieldSpec)
						EXIT
					END IF
				NEXT
			ELSEIF .not. SELF:IsAvailableFieldSpec(cFieldSpec) // New FieldSpec
				lSaveFieldSpecs := TRUE
				VAR oFieldSpec := FSEDesignFieldSpec{NULL , NULL}
				aCode:Add(oFieldSpec)
				SELF:CopyPropertyValues(oDesign , oFieldSpec)
				aFieldSpecs:Add(oFieldSpec)
			END IF
		NEXT

		IF aFieldSpecs:Count != 0 .and. lSaveFieldSpecs
			LOCAL cFieldSpecFileName as STRING
			cFieldSpecFileName := cModule + ".FieldSpecs.xsfs"
			VOFieldSpecEditor.SaveToXml( cFieldSpecFileName, aFieldSpecs)
			SELF:oXProject:AddFile(cFieldSpecFileName)
			FOREACH oFieldSpec as  FSEDesignFieldSpec in aFieldSpecs
				IF .not. String.IsNullOrEmpty(oFieldSpec:cVNfsFileName) .and. .not. oFieldSpec:cVNfsFileName:ToUpper():Contains(".FIELDSPECS.VNFS")
					SELF:oXProject:RemoveFile(oFieldSpec:cVNfsFileName)
					IF File.Exists(oFieldSpec:cVNfsFileName)
						TRY
							File.Delete(oFieldSpec:cVNfsFileName)
						END TRY
					END IF
				END IF
			NEXT
		END IF

		SELF:ReadAllAvailableFieldspecs(cModule)

		lSrvOnly		:= lSrvOnly .or. SELF:lStandalone
		oPrgStream  := XSharp_EditorStream{}
		IF SELF:GetSaveFileStreams(cFileName , oPrgStream , lSrvOnly)
			TRY
				SELF:SaveToXml(cFileName)
				IF !lSrvOnly
					SELF:SavePrg(oPrgStream , oCode , aCode)
				END IF
				lSuccess := TRUE
	
				IF !lSrvOnly
					SELF:nActionSaved := SELF:nAction
				END IF
			END TRY
		END IF
		
		IF lSuccess
			FOREACH cFile as STRING in SELF:aFilesToDelete
				oXProject:RemoveFile(cFile)
				IF File.Exists(cFileName)
					TRY
						File.Delete(cFile)
					END TRY
				END IF
			NEXT
			SELF:aFilesToDelete:Clear()
		END IF
		
	RETURN lSuccess
	return FALSE
END CLASS

END NAMESPACE