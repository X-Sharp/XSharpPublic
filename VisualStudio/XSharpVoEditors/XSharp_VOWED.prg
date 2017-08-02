USING System.Collections.Generic
USING System.Windows.Forms
USING System.IO
USING Xide
BEGIN NAMESPACE XSharp.VOEditors
CLASS XSharp_VOWindowEditor INHERIT VOWindowEditor

	CONSTRUCTOR(_oSurface AS Control , _oOptions AS WindowDesignerOptions , _oGrid AS DesignerGrid , _oToolBox AS ToolBox)
		SUPER(_oSurface , _oOptions , _oGrid , _oToolBox)
	RETURN

	VIRTUAL METHOD Save() AS VOID
		SELF:Save(SELF:cDefaultFileName , FALSE)
	
	VIRTUAL METHOD Save(cVNFrmFileName AS STRING , lBinaryOnly AS LOGIC) AS LOGIC
		LOCAL oPrg , oVh  , oRC AS XSharp_EditorStream
		LOCAL lRcInSameFolder AS LOGIC
		LOCAL lOldTransporter AS LOGIC
		LOCAL oVNFrm AS FileStream
		LOCAL cVhName AS STRING
		LOCAL lXsFrm := FALSE as LOGIC
		LOCAL cXsFrmFileName := "" as STRING

		IF /*SELF:lStandalone .or. */cVNFrmFileName:ToUpper():Contains("~AUTORECOVER")
			lBinaryOnly := TRUE
		END IF

		SELF:ArrangeColumnsOrder(TRUE)
		SELF:ArrangeControlOrder()
		
		oPrg := XSharp_EditorStream{}
		oVh := XSharp_EditorStream{}
		oRc := XSharp_EditorStream{}
		
        oVNFrm := NULL; cVhName := NULL; lRcInSameFolder := FALSE // Grrr
		if System.IO.Path.GetExtension(cVnFrmFileName):ToLower() == ".xsfrm"
			lXsFrm := TRUE
			cXsFrmFileName := cVnFrmFileName
			cVnFrmFileName := System.IO.Path.ChangeExtension(cXsFrmFileName, "vnfrm") 
			System.IO.File.Copy(cXsFrmFileName, cVnFrmFileName,TRUE)
		endif
		IF .not. SELF:GetSaveFileStreams(cVNFrmFileName , oVNFrm , oRC , oPrg , oVh , REF cVhName , lBinaryOnly , REF lRcInSameFolder)
			RETURN FALSE
		ENDIF
		
		lOldTransporter := .not. oVh:IsValid
		SELF:SaveToXml(oVNFrm)
		if lXsFrm
			System.IO.File.Copy(cVnFrmFileName, cXsFrmFileName, true )
			System.IO.File.Delete(cVnFrmFileName)
		endif

		IF .not. lBinaryOnly
			LOCAL oCode AS CodeContents
			oCode := SELF:GetCodeContents()

			SELF:SaveRC(oRC , oCode , cVhName , lOldTransporter , lRcInSameFolder)

			IF oVh:IsValid
				SELF:SaveVh(oVh , oCode)
			END IF

			SELF:SavePrg(oPrg , oCode , lOldTransporter)

			SELF:nActionSaved := SELF:nAction
		END IF

	RETURN TRUE

	METHOD SaveRC(oStream AS XSharp_EditorStream , oCode AS CodeContents , cVhName AS STRING , lOldTransporter AS LOGIC , lRcInSameFolder AS LOGIC) AS LOGIC
		LOCAL oGenerator AS CodeGenerator
		oGenerator := CodeGenerator{oStream:Editor}
		oGenerator:BeginCode(TRUE)

		oGenerator:Clear()
		oGenerator:AddInclude("VOWin32APILibrary.vh")
		IF lOldTransporter
			FOR LOCAL n := 0 UPTO oCode:aDefines:Count - 1
				oGenerator:AddDefine(oCode:aDefines[n] , oCode:aDefineValues[n])
			NEXT
		ELSE
			IF lRcInSameFolder
				oGenerator:AddInclude(cVhName)
			ELSE
				oGenerator:AddInclude("..\" + cVhName)
			END IF
		END IF
		oGenerator:AddLine("")
		FOR LOCAL n := 0 UPTO oCode:aResource:Count - 1
			oGenerator:AddLine(oCode:aResource[n])
		NEXT
		oStream:Save()
	RETURN TRUE

	METHOD SavePrg(oDest AS XSharp_EditorStream , oCode AS CodeContents , lOldTransporter AS LOGIC) AS LOGIC
		LOCAL aEntity AS List<STRING>
		LOCAL cFormName AS STRING
		LOCAL cPrevName AS STRING
//		LOCAL lHasDesPrg AS LOGIC
		LOCAL n AS INT
		
		LOCAL oGenerator AS CodeGenerator
		oGenerator := CodeGenerator{oDest:Editor}
		oGenerator:BeginCode(TRUE)

		LOCAL /*cTab , */ceTab AS STRING
//		cTab := e"\t"
//		ceTab := e"\t"
		ceTab := ""

		cFormName := SELF:oWindowDesign:Name
		// needed for renaming a window:
//		cPrevName := oWEditor:cPreviousName
		cPrevName := SELF:oWindowDesign:Name

		oGenerator:DeleteDefines(oCode:aDefines)
		IF lOldTransporter
			oGenerator:AddDefines(oCode:aDefines , oCode:aDefineValues , FALSE)
		ENDIF
	
		oGenerator:WriteEntity(EntityType._Class , cPrevName , cPrevName , EntityOptions.AddUser , oCode:aClass)
		oGenerator:WriteEntity(EntityType._Constructor , cFormName , cFormName , EntityOptions.None , oCode:aConstructor)

		LOCAL lAccessAssign AS LOGIC
		LOCAL oProp AS DesignProperty
		LOCAL cName AS STRING

		aEntity := List<STRING>{}
		
		oProp := SELF:oWindowDesign:GetPropertyByMember("NoAcc")
		lAccessAssign := oProp != NULL .and. oProp:TextValue:Trim() == "" .and. oProp:cPage != "_Hidden"
		FOR n := 0 UPTO oCode:aAccessAssign:Count - 1
			cName := oCode:aAccessAssign[n]

			aEntity:Clear()
			aEntity:Add(ceTab + "ACCESS " + cName)
			aEntity:Add(ceTab + "RETURN SELF:FieldGet( #" + cName + " )")
			IF lAccessAssign
				oGenerator:WriteEntity(EntityType._Access , cName , cFormName , EntityOptions.None , aEntity)
			ELSE
				oGenerator:DeleteEntity(EntityType._Access , cName , cFormName)
			END IF

			aEntity:Clear()
			aEntity:Add(ceTab + "ASSIGN " + cName + "( uValue )")
			aEntity:Add(ceTab + "SELF:FieldPut( #" + cName + " , uValue )")
			IF lAccessAssign
				oGenerator:WriteEntity(EntityType._Assign , cName , cFormName , EntityOptions.None , aEntity)
			ELSE
				oGenerator:DeleteEntity(EntityType._Assign , cName , cFormName)
			END IF
		NEXT
		
		// this is for separate .designer.prg file, similar to winforms. Ignore for VS
/*
		IF lHasDesPrg
			oGenerator:WriteEndClass(cFormName)
			oGenerator:EndCode()
			oGenerator:BeginCode(FALSE)

			aEntity:Clear()
			aEntity:Add("PARTIAL CLASS " + cFormName)
			oGenerator:WriteEntity(EntityType._Class , cPrevName , cPrevName , EntityOptions.ClearOldFields , aEntity)

			oGenerator:DeleteEntity(EntityType._Constructor , "CONSTRUCTOR" , cFormName)
			FOR n := 0 UPTO oCode:aAccessAssign:Count - 1
				cName := oCode:aAccessAssign[n]
				oGenerator:DeleteEntity(EntityType._Access , cName , cFormName)
				oGenerator:DeleteEntity(EntityType._Assign , cName , cFormName)
			NEXT
		END IF
*/
		oGenerator:WriteEndClass(cFormName)

		oGenerator:EndCode()
	RETURN oDest:Save()

	METHOD SaveVh(oStream AS XSharp_EditorStream , oCode AS CodeContents) AS LOGIC
		LOCAL oGenerator AS CodeGenerator
		oGenerator := CodeGenerator{oStream:Editor}
		oGenerator:BeginCode(TRUE)

		oGenerator:DeleteDefines(oCode:aDefines)
		oGenerator:AddDefines(oCode:aDefines, oCode:aDefineValues)
		
		oStream:Save()
	RETURN TRUE

	// The following is the original vulcan code which generates callbacks in the code
	// Always generates the code directly in a VS editor file
	// Of course needs adjustment to work with x#'s project system
	
/*	ACCESS Module AS Vulcan.CodeModel.Module
		LOCAL cPrgFileName AS STRING
		cPrgFileName := Funcs.GetModuleFilenameFromBinary(SELF:cDefaultFileName) + ".prg"
	RETURN Vulcan.CodeModel.Projects.Find(cPrgFileName)

	PROTECTED VIRTUAL METHOD WriteCallback(oDesign AS DesignWindowItem , cName AS STRING) AS VOID
		LOCAL oProject AS Vulcan.CodeModel.Project
		LOCAL oModule AS Vulcan.CodeModel.Module
		LOCAL oMember AS Vulcan.CodeModel.Member
		LOCAL oType AS Vulcan.CodeModel.TypeInfo
		LOCAL oEditor AS VulcanEditor
		LOCAL aLines AS List<STRING>
		LOCAL lManaged AS LOGIC
		LOCAL cClass AS STRING
		LOCAL lDirty AS LOGIC
		LOCAL lOpen AS LOGIC
		LOCAL nLine AS INT
		LOCAL n AS INT

		cClass := SELF:oWindowDesign:Name
		
		oModule := SELF:Module
		IF oModule == NULL
			RETURN
		END IF
		oProject := oModule:Parent

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
			Funcs.WarningBox("Could not find code template for event: " + cName)
			RETURN
		END IF
		// Find Method for this class
		IF cName:ToUpper() == "CLASSDECLARATION"
		    oType := oProject:FindType(SELF:oWindowDesign:Name, FALSE)
	        IF (oType != NULL_OBJECT)
	            oProject:OpenInEditor(oType)
	            RETURN
	        ENDIF
		    
        ELSE
		    oMember := oProject:FindMember(SELF:oWindowDesign:Name, cName, Vulcan.CodeModel.ElementType.Method, FALSE)
	        IF (oMember != NULL_OBJECT)
	            oProject:OpenInEditor(oMember)
	            RETURN
	        ENDIF
        ENDIF		
		oModule:GetInfo(lManaged , lOpen , lDirty)
		IF .not. lManaged
			RETURN
		END IF
		IF .not. lOpen
			oModule:OpenInEditor()
		END IF
		oModule:GetInfo(lManaged , lOpen , lDirty)
		IF .not. lOpen
			RETURN
		END IF
		
		oEditor := VulcanEditor{Funcs.BufferToLines(oModule:SourceCode)} 
		nLine := oEditor:GetLastClassLine(cClass)
		IF nLine <= 0 .or. nLine > oModule:GetLineCount()
			RETURN
		END IF

		nLine ++
		IF nLine >= oModule:GetLineCount()
			nLine := oModule:GetLineCount() - 1
		END IF

		TRY
			FOR n := 0 UPTO aLines:Count - 1
				oModule:InsertLine(nLine + n , SubstituteTpl(aLines[n] , cClass , SELF:oWindowDesign:cInitMethod) + CRLF)
			NEXT
		END TRY
		
		oModule:OpenInEditor(nLine , 1)

	RETURN
*/
END CLASS

END NAMESPACE