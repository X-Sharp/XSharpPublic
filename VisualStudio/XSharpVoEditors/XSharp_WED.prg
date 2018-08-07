USING System.Collections.Generic
USING System.Windows.Forms
USING System.IO
USING Xide
USING XSharp.VODesigners

BEGIN NAMESPACE XSharp.VOEditors
CLASS XSharp_VOWindowEditor INHERIT VOWindowEditor
	PROTECT oXProject AS XSharpModel.XProject
	PROTECT aStylesUsed AS List<STRING>		// List of styles used in this window
	CONSTRUCTOR(_oSurface AS Control , _oOptions AS WindowDesignerOptions , _oGrid AS DesignerGrid , _oToolBox AS ToolBox)
		SUPER(_oSurface , _oOptions , _oGrid , _oToolBox)
		aStylesUsed := List<STRING>{}
	RETURN

	METHOD AddStyles(aStyles AS STRING[]) AS VOID
		FOREACH cStyle AS STRING IN aStyles
			IF !aStylesUsed:Contains(cStyle)
				aStylesUsed:Add(cStyle)
			ENDIF
		NEXT
		RETURN

	PROTECTED VIRTUAL METHOD CreateControl(cControl AS STRING, oParent AS Control , cGuid AS STRING) AS DesignWindowItem
		LOCAL oItem AS XSharpDesignWindowItem
		// Use our own DesignWindowItem so we can collect the list of styles used
		oItem := XSharpDesignWindowItem{SELF , VOWindowEditorTemplate.Get(cControl)}
		IF String.IsNullOrEmpty(cGuid)
			cGuid := Guid.NewGuid():ToString()
		ENDIF
		oItem:cGuid := cGuid
		oParent:Controls:Add(oItem:Control)

		IF oItem:IsForm
			SELF:oWindowDesign	:= oItem
			SELF:oWindow		:= oItem:Control
		ENDIF

		IF oItem:Column != NULL
			oParent:Controls:Add(oItem:Column)
		END IF
		IF SELF:ViewMode == ViewMode.Browse
			oItem:lBrowseView := TRUE
			oItem:Control:Hide()
		ELSE
			IF oItem:Column != NULL_OBJECT
				oItem:Column:Hide()
			END IF
		ENDIF
	RETURN oItem

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

	PROTECTED METHOD GetSaveFileStreams(cVNFrmFileName AS STRING , oVNFrmStream REF FileStream , ;
								oRCStream AS EditorStream , oPrgStream AS EditorStream , oVhStream AS EditorStream , ;
								cVhName REF STRING , lVnfrmOnly AS LOGIC , lRcInSameFolder REF LOGIC) AS LOGIC
		LOCAL cRCFileName AS STRING
		LOCAL cPrgFileName AS STRING
		LOCAL cHeaderFile AS STRING
		LOCAL cFileName AS STRING
		LOCAL cBaseName AS STRING
		LOCAL oFileInfo AS FileInfo
		LOCAL oBaseDir AS DirectoryInfo
		LOCAL cBaseDir AS STRING
		LOCAL cAlternative AS STRING
		LOCAL lSuccess AS LOGIC
		LOCAL lError AS LOGIC
		LOCAL nAt AS INT
		
		TRY

			oFileInfo	:= FileInfo{cVNFrmFileName}
			oBaseDir	:= oFileInfo:Directory
			cBaseDir	:= oBaseDir:FullName
			cBaseName	:= oFileInfo:Name
			
			IF cBaseName:ToLower():Endswith(".vnfrm") .or. cBaseName:ToLower():Endswith(".xsfrm")
				cBaseName	:= System.IO.Path.GetFileNameWithoutExtension(cBaseName)
			ENDIF
			cRCFileName		:= cBaseDir + "\Resources\" + cBaseName + ".rc"
			cAlternative	:= cBaseDir + "\" + cBaseName + ".rc"
			IF !File.Exists(cRCFileName) .and. File.Exists(cAlternative)
				cRCFileName := cAlternative
				lRcInSameFolder := TRUE
			ENDIF
						
			cFileName	:= cBaseName
			nAt			:= cFileName:LastIndexOf('.')
			IF nAt != -1 // strip out window name
				cFileName := cFileName:Substring(0 , nAt)
			ENDIF
			cPrgFileName := cBaseDir + "\" + cFileName + ".prg"
			cHeaderFile  := System.IO.Path.ChangeExtension(cPrgFileName, ".vh")
			IF !File.Exists(cHeaderFile)
				cHeaderFile  := System.IO.Path.ChangeExtension(cHeaderFile, ".xh")
			ENDIF
			cAlternative := oBaseDir:Parent:FullName + "\" + cFileName + ".prg"
			IF !File.Exists(cPrgFileName) 
				cPrgFileName := cAlternative
				cHeaderFile  := System.IO.Path.ChangeExtension(cPrgFileName, ".vh")
				IF !File.Exists(cHeaderFile)
					cHeaderFile  := System.IO.Path.ChangeExtension(cHeaderFile, ".xh")
				ENDIF
			ENDIF
			cVhName := System.IO.Path.GetFileName(cHeaderFile)
	
			lError := FALSE
			IF !lVnfrmOnly
				IF ! File.Exists(cRCFileName)
					cRCFileName := System.IO.Path.ChangeExtension(cVNFrmFileName, ".rc")
				ENDIF
				XFuncs.EnsureFileNodeExists(oXProject, cRCFileName)
				IF !File.Exists(cPrgFileName)
					XFuncs.ErrorBox("File was not found : " + cPrgFileName)
					lError := TRUE
				END IF
			END IF
			
			lSuccess := FALSE
			IF !lError
				IF !lVnfrmOnly
					oRCStream:Load(cRCFileName)
					oPrgStream:Load(cPrgFileName)
					IF File.Exists(cHeaderFile) // else old transporter version
						oVhStream:Load(cHeaderFile)
					ENDIF
					lSuccess := oRCStream:IsValid .and. oPrgStream:IsValid
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

			XFuncs.WarningBox(e:Message )
			lSuccess := FALSE

		END TRY

		IF !lSuccess
			IF oVNFrmStream != NULL
				oVNFrmStream:Close()
			ENDIF
			IF oRCStream:IsValid
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

	VIRTUAL METHOD Save() AS VOID
		SELF:Save(SELF:cDefaultFileName , FALSE)
	
	VIRTUAL METHOD Save(cVNFrmFileName AS STRING , lBinaryOnly AS LOGIC) AS LOGIC
		LOCAL oPrg , oVh  , oRC AS XSharp_EditorStream
		LOCAL lRcInSameFolder AS LOGIC
		LOCAL lOldTransporter AS LOGIC
		LOCAL oVNFrm AS FileStream
		LOCAL cVhName AS STRING

		IF cVNFrmFileName:ToUpper():Contains("~AUTORECOVER")
			lBinaryOnly := TRUE
		END IF

		SELF:ArrangeColumnsOrder(TRUE)
		SELF:ArrangeControlOrder()
		
		oPrg := XSharp_EditorStream{}
		oVh := XSharp_EditorStream{}
		oRc := XSharp_EditorStream{}
		
		oVNFrm := NULL; cVhName := NULL; lRcInSameFolder := FALSE // Grrr
		IF .not. SELF:GetSaveFileStreams(cVNFrmFileName , oVNFrm , oRC , oPrg , oVh , REF cVhName , lBinaryOnly , REF lRcInSameFolder)
			RETURN FALSE
		ENDIF
		
		lOldTransporter := .not. oVh:IsValid
		
		SELF:SaveToXml(oVNFrm)

		IF .not. lBinaryOnly
			LOCAL oCode AS CodeContents
			oCode := SELF:GetCodeContents()

			SELF:SaveRC(oRC , oCode , cVhName , lOldTransporter , lRcInSameFolder)

			IF oVh:IsValid
				SELF:SaveVh(oVh , oCode)
			END IF

			SELF:SavePrg(oPrg , oCode , lOldTransporter , cVhName)

			SELF:nActionSaved := SELF:nAction
		END IF

	RETURN TRUE

	METHOD SaveRC(oStream AS XSharp_EditorStream , oCode AS CodeContents , cVhName AS STRING , lOldTransporter AS LOGIC , lRcInSameFolder AS LOGIC) AS LOGIC
		LOCAL oGenerator AS CodeGenerator
		oGenerator := CodeGenerator{oStream:Editor}
		oGenerator:BeginCode()

		oGenerator:Clear()
		XFuncs.WriteHeader(oGenerator, "XSharp.FormEditor")
		// in x#, always add the #defines in the .rc header, no need to put them in a .vh anymore
		FOR LOCAL n := 0 UPTO oCode:aDefines:Count - 1
			oGenerator:AddDefine(oCode:aDefines[n] , oCode:aDefineValues[n])
		NEXT
		// Write the styles that are used in Alphabetical order
		aStylesUsed:Sort()
		FOREACH cStyle AS STRING IN SELF:aStylesUsed
			oGenerator:AddDefine(cStyle, "0x"+VODefines.GetDefineValue(cStyle):ToString("X8"))
		NEXT
		oGenerator:AddLine("")
		FOREACH cResource AS STRING IN oCode:aResource
			oGenerator:AddLine(cResource)
		NEXT
		oStream:Save()
	RETURN TRUE

	METHOD SavePrg(oDest AS XSharp_EditorStream , oCode AS CodeContents , lOldTransporter AS LOGIC , cVhName AS STRING) AS LOGIC
		LOCAL aEntity AS List<STRING>
		LOCAL cFormName AS STRING
		LOCAL cPrevName AS STRING
		
		LOCAL oGenerator AS CodeGenerator
		oGenerator := CodeGenerator{oDest:Editor}
		oGenerator:BeginCode()

		LOCAL ceTab AS STRING
		ceTab := ""

		cFormName := SELF:oWindowDesign:Name
		// needed for renaming a window:
		cPrevName := SELF:oWindowDesign:Name

		// don't delete the header file for now, maybe the .vh file contains defines for other windows in the same prg
		oGenerator:DeleteDefines(oCode:aDefines)
		oGenerator:RemoveDefines(cPrevName)
		oGenerator:AddXSharpDefines(oCode:aDefines , oCode:aDefineValues,TRUE)
	
		oGenerator:WriteEntity(EntityType._Class , cPrevName , cPrevName , EntityOptions.AddUser , oCode:aClass)
		oGenerator:WriteEntity(EntityType._Constructor , cFormName , cFormName , EntityOptions.None , oCode:aConstructor)

		LOCAL lAccessAssign AS LOGIC
		LOCAL oProp AS DesignProperty

		aEntity := List<STRING>{}
		
		oProp := SELF:oWindowDesign:GetPropertyByMember("NoAcc")
		lAccessAssign := oProp != NULL .and. oProp:TextValue:Trim() == "" .and. oProp:cPage != "_Hidden"
		FOREACH cName AS STRING IN oCode:aAccessAssign

			IF lAccessAssign
				aEntity:Clear()
				aEntity:Add(ceTab + "ACCESS " + cName)
				aEntity:Add(ceTab + ceTab+ "RETURN SELF:FieldGet( #" + cName + " )")
				oGenerator:WriteEntity(EntityType._Access , cName , cFormName , EntityOptions.None , aEntity)

				aEntity:Clear()
				aEntity:Add(ceTab + "ASSIGN " + cName + "( uValue )")
				aEntity:Add(ceTab + ceTab+"SELF:FieldPut( #" + cName + " , uValue )")
				oGenerator:WriteEntity(EntityType._Assign , cName , cFormName , EntityOptions.None , aEntity)

			ELSE
				oGenerator:DeleteEntity(EntityType._Access , cName , cFormName)
				oGenerator:DeleteEntity(EntityType._Assign , cName , cFormName)
			END IF
		NEXT
		
		oGenerator:WriteEndClass(cFormName)
		oGenerator:EndCode()
	RETURN oDest:Save()

	METHOD SaveVh(oStream AS XSharp_EditorStream , oCode AS CodeContents) AS LOGIC
		LOCAL oGenerator AS CodeGenerator
		oGenerator := CodeGenerator{oStream:Editor}
		oGenerator:BeginCode()

		oGenerator:DeleteDefines(oCode:aDefines)
		
		oStream:Save()
	RETURN TRUE

	// The following is the original vulcan code which generates callbacks in the code
	// Always generates the code directly in a VS editor file
	// Of course needs adjustment to work with x#'s project system
	
	ACCESS XFile AS XSharpModel.XFile
		LOCAL cPrgFileName AS STRING
		cPrgFileName := Funcs.GetModuleFilenameFromBinary(SELF:cDefaultFileName) + ".prg"
	RETURN XSharpModel.XSolution.FindFile(cPrgFileName)

	PROTECTED VIRTUAL METHOD WriteCallback(oDesign AS DesignWindowItem , cName AS STRING) AS VOID
		LOCAL oProject AS XSharpModel.XProject
		LOCAL oFile AS XSharpModel.XFile
		LOCAL oType AS XSharpModel.XType
		//LOCAL oEditor AS VulcanEditor
		LOCAL aLines AS List<STRING>
		LOCAL cClass AS STRING
		LOCAL lOpen AS LOGIC

		cClass := SELF:oWindowDesign:Name
		
		oFile := SELF:XFile
		IF oFile == NULL
			RETURN
		END IF
		oProject := oFile:Project

		IF cName == "Click"
			cName := oDesign:Name
			aLines := List<STRING>{}
			aLines:Add("METHOD " + cName + "( )")
			aLines:Add("")
			aLines:Add("RETURN NIL")
		ELSE
			aLines := GetTemplate(cName)
		END IF
		IF aLines == NULL
			XFuncs.WarningBox("Could not find code template for event: " + cName)
			RETURN
		END IF
		// Find Method for this class
	    VAR rootNs := oFile:Project:ProjectNode:RootNameSpace
		oType := oProject:Lookup(cClass,true)
		IF oType == NULL_OBJECT
			oType := oProject:Lookup(rootNs+"."+cClass,true)
		ENDIF
		IF cName:ToUpper() == "CLASSDECLARATION"
	        IF (oType != NULL_OBJECT)
				oType:OpenEditor()
	            RETURN
	        ENDIF
		    
        ELSEIF oType != NULL_OBJECT
			FOREACH VAR oMember IN oType:Members
				IF string.Compare(oMember:Name, cName, TRUE) == 0
					oMember:OpenEditor()
					RETURN
				ENDIF
			NEXT
        ENDIF		
		lOpen := FALSE
		VAR source := oProject:ProjectNode:DocumentGetText(oFile:FullPath, REF lOpen)

		IF .not. lOpen
			oProject:ProjectNode:OpenElement(oFile:FullPath, 1,1)
		END IF
		source := oProject:ProjectNode:DocumentGetText(oFile:FullPath, REF lOpen)
		IF .not. lOpen
			RETURN
		END IF
		
		VAR oEditor := XSharp_EditorStream{}
		oEditor:Load(oFile:FullPath)
		VAR nLine := oType:Range:EndLine -1
		aLines:Reverse()
		FOREACH VAR cLine IN aLines
			VAR cNew := VOWindowEditor.SubStituteTpl(cLine, cClass, oWindowDesign:cInitMethod)
			oFile:Project:ProjectNode:DocumentInsertLine(oFile:FullPath, nLine, cNew)
		NEXT
		oFile:Project:ProjectNode:OpenElement(oFile:FullPath, nLine,1)
		XSharpModel.XSolution.WalkFile(oFile:FullPath)
	RETURN

END CLASS


END NAMESPACE