USING System.Collections.Generic
USING System.Windows.Forms
USING System.IO
USING Xide
USING XSharp.VODesigners

BEGIN NAMESPACE XSharp.VOEditors
CLASS XSharp_VOMenuEditor INHERIT VOMenuEditor
	PROTECT oXProject AS XSharpModel.XProject
	CONSTRUCTOR(_oSurface AS Control , _oGrid AS DesignerGrid )
		SUPER(_oSurface , _oGrid) 
	RETURN

	METHOD Open(cFileName AS STRING) AS LOGIC
		VAR oFile := XSharpModel.XSolution.FindFile(cFileName)
		IF (oFile != NULL_OBJECT)
			oXProject := oFile:Project
		ENDIF
		IF oXProject == NULL
			XFuncs.ErrorBox("Cannot find project for file "+cFileName)
			RETURN FALSE
		ENDIF
		RETURN SUPER:Open(cFileName)

	PROTECTED METHOD GetSaveFileStreams(cMnuName AS STRING , oMnuStream REF FileStream , ;
								oRCStream AS EditorStream , oPrgStream AS EditorStream , ;
								lMnuOnly AS LOGIC , oRCAccelStream AS EditorStream ) AS LOGIC
		LOCAL cRCFileName AS STRING
		LOCAL cRCAccelFileName AS STRING
		LOCAL cPrgFileName AS STRING
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
		
		lAccelerators := SELF:HasAccelerators
		
		TRY

			oFileInfo := FileInfo{cMnuName}
			oBaseDir := oFileInfo:Directory
			cBaseDir := oBaseDir:FullName
			cBaseName := oFileInfo:Name
			nAt := cBaseName:ToLower():IndexOf(".vnmnu")
			IF (nAt == -1)
				nAt := cBaseName:ToLower():IndexOf(".xsmnu")
			ENDIF
			IF nAt != -1
				cBaseName := cBaseName:Substring(0 , nAt)
			ENDIF
			cRCFileName := cBaseDir + "\Resources\" + cBaseName + ".rc"
			cRCAccelFileName := cBaseDir + "\Resources\" + cBaseName + "_Accelerator.rc"
			cAlternative := cBaseDir + "\" + cBaseName + ".rc"
			IF !File.Exists(cRCFileName) 
				cRCFileName := cAlternative
				cRCAccelFileName := cBaseDir + "\" + cBaseName + "_Accelerator.rc"
			ENDIF
			
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
			IF !lMnuOnly
				XFuncs.EnsureFileNodeExists(oXProject, cRCFileName)
				IF lAccelerators 
					XFuncs.EnsureFileNodeExists(oXProject, cRCAccelFileName)
				ENDIF
				IF !File.Exists(cPrgFileName)
					XFuncs.ErrorBox("File was not found : " + cPrgFileName)
					lError := TRUE
				END IF
			END IF
			
			lSuccess := FALSE
			IF !lError
				IF !lMnuOnly
					oRCStream:Load(cRCFileName)
					IF lAccelerators .and. oRCAccelStream != NULL_OBJECT
						oRCAccelStream:Load(cRCAccelFileName)
					END IF
					oPrgStream:Load(cPrgFileName)
					lSuccess := oRCStream:IsValid .and. oPrgStream:IsValid .and. oRCStream:IsValid
				ELSE
					lSuccess := TRUE
				END IF
			END IF
			IF lSuccess .and. !lError
				lSuccess := FALSE
				oMnuStream := File.Open(cMnuName , FileMode.Create , FileAccess.Write , FileShare.None)
				lSuccess := TRUE
			ENDIF
			
		CATCH e AS Exception

			XFuncs.ErrorBox(e:Message )
			lSuccess := FALSE

		END TRY

		IF !lSuccess
			IF oMnuStream != NULL
				oMnuStream:Close()
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
		END IF

	RETURN lSuccess

	METHOD GetCodeContents() AS CodeContents 
		VAR oCode := SUPER:GetCodeContents()
		// remove IDM_MenuName
		oCode:aDefines:RemoveAt(0)
		oCode:aDefineValues:RemoveAt(0)
		IF SELF:HasAccelerators .and. oCode:aDefines[0]:Startswith("IDA_")
			// remove IDA_MenuName
			oCode:aDefines:RemoveAt(0)
			oCode:aDefineValues:RemoveAt(0)
		ENDIF	
		RETURN oCode


	METHOD Save(cFileName AS STRING , lMnuOnly AS LOGIC) AS LOGIC
		LOCAL oRCStream , oRCAccelStream , oPrgStream  AS XSharp_EditorStream
		LOCAL oMnuStream := NULL AS FileStream
		LOCAL oCode AS CodeContents
		LOCAL lSuccess := FALSE AS LOGIC

		IF SELF:lReadOnly
			RETURN FALSE
		ENDIF
		
		lMnuOnly := lMnuOnly .or. SELF:lStandalone
		IF cFileName:ToUpper():Contains("~AUTORECOVER")
			lMnuOnly := TRUE // in case it isn't already
		END IF

		oPrgStream		:= XSharp_EditorStream{}
		oRcStream		:= XSharp_EditorStream{}
		oRcAccelStream	:= XSharp_EditorStream{}
		
		oCode := SELF:GetCodeContents()

		IF SELF:GetSaveFileStreams(cFileName , REF oMnuStream , oRCStream , oPrgStream, lMnuOnly , oRCAccelStream )
			IF .not. SELF:SaveToXml(oMnuStream)
				RETURN FALSE
			ENDIF
			IF !lMnuOnly
				SELF:SaveRC(oRCStream , oRCAccelStream , oCode )
				SELF:SavePrg(oPrgStream , oCode)
			END IF
			lSuccess := TRUE
			IF !lMnuOnly
				SELF:nActionSaved := SELF:nAction
			END IF
		ENDIF
	RETURN lSuccess


	METHOD _WriteDefines(oGenerator AS CodeGenerator, oCode AS CodeContents ) AS VOID		
		// in x#, always add the #defines in the .rc header, no need to put them in a .vh anymore
		FOR VAR nDef := 0 UPTO oCode:aDefines:Count - 1
			oGenerator:AddLine("#define " +oCode:aDefines[nDef] +" "+ oCode:aDefineValues[nDef])
		NEXT

	METHOD SaveRC(oStream AS XSharp_EditorStream , oAccelStream AS XSharp_EditorStream , oCode AS CodeContents ) AS LOGIC
		LOCAL oGenerator AS CodeGenerator
		oGenerator := CodeGenerator{oStream:Editor}
		oGenerator:BeginCode()
		oGenerator:Clear()
		XFuncs.WriteHeader(oGenerator, "XSharp.MenuEditor")
		SELF:_WriteDefines(oGenerator, oCode)
		FOREACH cResource AS STRING IN oCode:aResource
			oGenerator:AddLine(cResource)
		NEXT
		oGenerator:AddLine("")
		
		IF oAccelStream:IsValid
			oGenerator := CodeGenerator{oAccelStream:Editor}
			oGenerator:BeginCode()
			oGenerator:Clear()
			XFuncs.WriteHeader(oGenerator, "XSharp.MenuEditor")
			SELF:_WriteDefines(oGenerator, oCode)
			FOREACH cResource AS STRING IN oCode:aAccelResource
				oGenerator:AddLine(cResource)
			NEXT
			oAccelStream:Save()
		ELSE
			oStream:Editor:AddLine("")
			FOREACH cResource AS STRING IN oCode:aAccelResource
				oStream:Editor:AddLine(cResource)
			NEXT
		END IF
		oStream:Save()

	RETURN TRUE
	METHOD SavePrg(oStream AS XSharp_EditorStream , oCode AS CodeContents) AS LOGIC
		LOCAL cName AS STRING
		LOCAL oGenerator AS CodeGenerator
		oGenerator := CodeGenerator{oStream:Editor}
		oGenerator:BeginCode()
		oGenerator:DeleteDefines(oCode:aDefines)
		oGenerator:AddXSharpDefines(oCode:aDefines , oCode:aDefineValues)
		cName := SELF:oMainNode:oDesign:Name
		oGenerator:RemoveDefines("IDM_"+cName)
		oGenerator:WriteEntity(EntityType._Class	  , cName , cName , EntityOptions.None , oCode:aClass)
		oGenerator:WriteEntity(EntityType._Constructor, cName , cName , EntityOptions.None , oCode:aConstructor)
		IF SELF:HasAccelerators
			cName := SELF:oMainNode:oDesign:Name + "_Accelerator"
			oGenerator:WriteEntity(EntityType._Class ,		cName , cName ,EntityOptions.None , oCode:aAccelClass)
			oGenerator:WriteEntity(EntityType._Constructor , cName , cName , EntityOptions.None, oCode:aAccelConstructor)
		END IF
		oGenerator:WriteEndClass(cName)
		oGenerator:EndCode()
		oStream:Save()
	RETURN TRUE



END CLASS

END NAMESPACE