
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


USING System.Collections.Generic
USING System.Xml
DEFINE EXTENSION := "xsproj"  AS STRING


CLASS ProjectConverter
	PROTECT oDoc 	AS XmlDocument
	PROTECT nGroup  AS LONG
	PROTECT cSchema AS STRING
	PROTECT cGuid   AS STRING
	PROTECT cPath	AS STRING
    PROTECT lUseXsRuntime AS LOGIC
	PROTECT oProgress AS IProgress
	PROTECT lRuntimeAdded AS LOGIC
	PROTECT oProjectNode AS XmlElement
	PROTECT lHasProjectExtensions AS LOGIC
	PROPERTY Guid AS STRING GET cGuid
	CONSTRUCTOR(oProg AS IProgress, lUseXsRT AS LOGIC)
		cSchema := "http://schemas.microsoft.com/developer/msbuild/2003"
		nGroup  := 0
        cGuid   := System.Guid.NewGuid().ToString("B"):ToUpper()
        lRuntimeAdded := FALSE
        SELF:oProgress := oProg
        SELF:lUseXsRuntime := lUseXsRT

METHOD ConvertProjectFile(cSource AS STRING, cTarget AS STRING, useXsRuntime AS LOGIC) AS LOGIC
	oProgress:WriteLine("Creating ... "+System.IO.Path.GetFileName(cTarget))
	cPath := System.IO.Path.GetDirectoryName(cSource)+"\"
	IF ! SELF:LoadFile(cSource)
		RETURN FALSE
	ENDIF
	SELF:WalkNode(oDoc)
	IF (!lHasProjectExtensions ) .AND. oProjectNode != NULL_OBJECT
		VAR oExt := oDoc:CreateElement("ProjectExtensions",cSchema)
		VAR oCap := oDoc:CreateElement("ProjectCapabilities",cSchema)
		oExt:AppendChild(oCap)
		oCap:AppendChild(oDoc:CreateElement("ProjectConfigurationsDeclaredAsItems",cSchema))
		oProjectNode:AppendChild(oExt)
	ENDIF
	SELF:SaveFile(cTarget)
	RETURN TRUE

  METHOD Save2String() AS STRING STRICT
      LOCAL cString         AS STRING
      LOCAL oWriter             AS System.Xml.XmlTextWriter
      LOCAL oStringWriter       AS System.IO.StringWriter
      cString := oDoc:OuterXml
      oStringWriter := System.IO.StringWriter{}
      oWriter     := System.Xml.XmlTextWriter{oStringWriter}
      oWriter:Formatting := System.Xml.Formatting.Indented
      oDoc:WriteTo(oWriter)
      cString := oStringWriter:ToString()
      RETURN cString

   METHOD SaveFile(strFileName AS STRING) AS LOGIC STRICT
      LOCAL lRet        AS LOGIC
      LOCAL oTargetFile  AS System.IO.StreamWriter
      lRet := FALSE
      IF oDoc:HasChildNodes
         oTargetFile := System.IO.StreamWriter{strFileName, FALSE}
         oTargetFile:Write( SELF:Save2String() )
         oTargetFile:Close()
         lRet := TRUE
      ENDIF
      RETURN lRet


METHOD CloneNode(oNode AS XmlNode) AS XmlNode
	LOCAL oResult AS XmlNode
	oResult := oNode:CloneNode(FALSE)
	oResult:InnerText := oNode:InnerText
	RETURN oResult

METHOD UpdateNode(oParent AS XmlNode, oElement AS XmlElement) AS VOID
	LOCAL oChild AS XmlElement
	LOCAL oAttribute AS XmlAttribute
	IF aDelete:Contains(oElement:Name:ToLower())
		// remove the node
		oParent:RemoveChild(oElement)
	ELSEIF aRename:ContainsKey(oElement:Name)
		// Renaming Nodes is not supported. Create a new node and
		// Copy its attributes and children
		// and then replace the existing node
		oChild := oDoc:CreateElement(aRename[oElement:Name],cSchema)
		DO WHILE oElement:HasAttributes
			oChild:SetAttributeNode(oElement:RemoveAttributeNode(oElement:Attributes[0]))
		ENDDO
		DO WHILE oElement:HasChildNodes
			oChild:AppendChild(oElement:FirstChild)
		ENDDO
		oParent:ReplaceChild(oChild, oElement)
	ELSE
		// Some nodes that require special processing
		SWITCH oElement:Name:ToLower()
		CASE "compile"
		CASE "none"
		CASE "vobinary"
		CASE "nativeresource"
			VAR cItem := oElement:GetAttribute("Include")
			cItem := cPath+cItem
			VAR oChild1 := oElement:FirstChild
			IF oChild1 != NULL .AND. oChild1:Name:ToLowerInvariant() == "dependentupon"
				VAR  cInnerText := oChild1:InnerText
				IF cInnerText:Contains("\")
					// check to see if parent node is in the same folder as the child node.
					IF System.IO.File.Exists(cItem) .AND. System.IO.File.Exists(cPath+cInnerText)
						IF System.IO.Path.GetFullPath(cItem) == System.IO.Path.GetFullPath(cPath+cInnerText)
							// paths are equal, only write filename
							cInnerText := System.IO.Path.GetFileName(cInnerText)
							oChild1:InnerText := cInnerText
						ENDIF
					ENDIF
				ENDIF
			ENDIF

		CASE "import"
			// change import
			oAttribute := (XmlAttribute) oElement:Attributes:GetNamedItem("Project")
			IF oAttribute:Value:ToLower():Contains("vulcan.net")
				oAttribute:Value := "$(MSBuildExtensionsPath)\XSharp\XSharp.targets"
			ENDIF
		CASE "project"
			// Only for main project node, not for project node inside projectreference
			IF (oParent IS XMLDocument)
				// Import the schema
				SELF:oProjectNode := oElement
			ENDIF

		CASE "projectguid"
			// new guid
			oElement:InnerText := cGuid
		CASE "projecttypeguids"
			// WPF project
			// our WPF ProjectFactory
			// Generic WPF project
			// Our project factory
			oElement:InnerText := "{5ADB76EC-7017-476A-A8E0-25D4202FFCF0};{60DC8134-EBA5-43B8-BCC9-BB4BC16C2548};{AA6C8D78-22FF-423A-9C7C-5F2393824E04}"
		CASE "projectreference"
			// update extension in project references
			oAttribute := (XmlAttribute) oElement:Attributes:GetNamedItem("Include")
			IF oAttribute != NULL
				oAttribute:Value := oAttribute:Value:Replace(".vnproj", "."+EXTENSION)
			ENDIF
		CASE "propertygroup"
			// Only insert code inside the first propertygroup
			IF ++nGroup == 1
				LOCAL oImport AS XmlNode
				// Add Import
				oImport := oDoc:CreateElement("Import",cSchema)
				oAttribute := oDoc:CreateAttribute("Project")
				oAttribute:Value := "$(MSBuildExtensionsPath)\XSharp\XSharp.Default.props"
				oImport:Attributes:Append(oAttribute)
				oParent:InsertBefore(oImport, oElement)
				// Add propertygroup before Import with Label="Globals" and child <XSharpProjectExtensionsPath>
				oChild := oDoc:CreateElement( "PlatformTarget", cSchema)
				oChild:InnerText := "x86"
				oElement:InsertBefore(oChild,oElement:FirstChild)
				oChild := oDoc:CreateElement( "Dialect", cSchema)
				oChild:InnerText := "Vulcan"
				oElement:InsertBefore(oChild,oElement:FirstChild)
			ELSE
				// ALl other project groups.
				// Adjust the condition when needed
				VAR oCondition := (XMLAttribute) oElement:Attributes:GetNamedItem("Condition")
				// <PropertyGroup Condition="'$(Configuration)|$(Platform)' == 'Release|AnyCPU'" Label="Configuration">
				IF oCondition != NULL
					VAR  cInnerText := oCondition:InnerText
					IF ! cInnerText:ToLower():Contains("platform")
						IF cInnerText:ToLower():Contains("release")
							oCondition:InnerText := "'$(Configuration)|$(Platform)' == 'Release|AnyCPU'"
						ELSE
							oCondition:InnerText := "'$(Configuration)|$(Platform)' == 'Debug|AnyCPU'"
						ENDIF
					ENDIF
				ENDIF
			ENDIF
		CASE "projectextensions"
			// add <ProjectCapabilities><ProjectConfigurationsDeclaredAsItems /></ProjectCapabilities>
			oChild := oDoc:CreateElement("ProjectCapabilities",cSchema)
			oChild:AppendChild(oDoc:CreateElement("ProjectConfigurationsDeclaredAsItems",cSchema))
			oElement:AppendChild(oChild)
			lHasProjectExtensions := TRUE
		CASE "reference"
			// Add RT assemblies after 1st reference
            VAR cAttribute := oElement:GetAttribute("Include")
            IF cAttribute != NULL .and. SELF:lUseXsRuntime
                SWITCH cAttribute:Tolower()
                CASE "vulcanvowin32apilibrary"
                CASE "vulcanvosystemclasses"
                CASE "vulcanvorddclasses"
                CASE "vulcanvosqlclasses"
                CASE "vulcanvoguiclasses"
                CASE "vulcanvointernetclasses"
                CASE "vulcanvoconsoleclasses"
                    cAttribute := cAttribute:Substring(6)
                    oElement:SetAttribute("Include", cAttribute)
                    // remove all version info in the children of the oElement
                    DO WHILE oElement:FirstChild != NULL
                        oElement:RemoveChild(oElement:FirstChild)
                    ENDDO
                    SELF:AddReferenceSubNodes(oElement, cAttribute)
                OTHERWISE
                    NOP
                END SWITCH
            ENDIF
			IF !SELF:lRuntimeAdded
                LOCAL cRt1, cRt2 AS STRING
                IF SELF:lUseXsRuntime
                    cRT1 := "XSharp.Core"
                    cRT2 := "XSharp.VO"
                ELSE
                    cRT1 := "VulcanRT"
                    cRT2 := "VulcanRTFuncs"
                ENDIF
                SELF:AddReference(oElement, cRT1)
                SELF:AddReference(oElement, cRT2)
				SELF:lRuntimeAdded := TRUE
			ENDIF
		OTHERWISE
			// All other elements should not be changed
			NOP
		END SWITCH
	ENDIF
	RETURN

METHOD AddReference(oElement AS XMLNode, cReference AS STRING) AS VOID
    VAR oChild := oDoc:CreateElement("Reference",cSchema)
    oElement:ParentNode:AppendChild(oChild)
	VAR oAttribute := oDoc:CreateAttribute("Include")
	oAttribute:Value := cReference
	oChild:Attributes:Append(oAttribute)
    SELF:AddReferenceSubNodes(oChild, cReference)

METHOD AddReferenceSubNodes(oRef AS XmlNode, cReference AS STRING) AS VOID
    VAR oSub := oDoc:CreateElement("Name",cSchema)
    oSub:InnerText := cReference
    oRef:AppendChild(oSub)
    oSub := oDoc:CreateElement("AssemblyName",cSchema)
    oSub:InnerText := cReference+".DLL"
    oRef:AppendChild(oSub)
	RETURN
METHOD WalkNode(oNode AS XmlNode) AS VOID
	LOCAL aChildren AS List<XmlNode>
	aChildren := List<XmlNode>{}
	FOREACH oChild AS XmlNode IN oNode:ChildNodes
		aChildren:Add(oChild)
	NEXT
	FOREACH oChild AS XmlNode IN aChildren
		IF oChild IS XmlElement
			SELF:UpdateNode(oNode, (XmlElement) oChild)
		ENDIF
		SELF:WalkNode((XMlNode) oChild)
	NEXT
	RETURN

    PROTECT METHOD _LoadFileFromReader(oReader AS System.IO.TextReader) AS LOGIC STRICT
        LOCAL lOk AS LOGIC
        TRY
            oDoc := System.Xml.XmlDocument{}
            oDoc:Load(oReader)
            lOk := TRUE
        CATCH
            lOk := FALSE
        END TRY
        RETURN lOk

   METHOD LoadFile(strFileName AS STRING) AS LOGIC STRICT
        LOCAL lResult := FALSE AS LOGIC
        IF System.IO.File.Exists(strFileName)
            LOCAL oReader AS System.IO.TextReader
            oReader := System.IO.StreamReader{strFileName,TRUE}
            lResult := SELF:_LoadFileFromReader(oReader)
            oReader:Close()
        ENDIF
        RETURN lResult

   METHOD LoadFileFromString(strXMLString AS STRING) AS LOGIC STRICT
        LOCAL lResult AS LOGIC
        LOCAL oReader AS System.IO.StringReader
        oReader := System.IO.StringReader{strXMLString}
        lResult := SELF:_LoadFileFromReader(oReader)
        oReader:Close()
        RETURN lResult


	STATIC PROTECT aRename AS Dictionary<STRING, STRING>
	STATIC PROTECT aDelete AS List<STRING>
	STATIC CONSTRUCTOR()
		aRename := Dictionary<STRING, STRING> {StringComparer.CurrentCultureIgnoreCase}
		aRename:Add("ZeroBasedArrays","AZ")
		aRename:Add("CaseSensitiveIdentifiers","CS")
		aRename:Add("CheckFloatOverflows","FOVF")
		aRename:Add("ImplicitNamespaceLookup","INS")
		aRename:Add("AllowLateBinding","LB")
		aRename:Add("UseDefaultNamespace","NS")
		aRename:Add("CheckForOverflowUnderflow","OVF")
		aRename:Add("GeneratePreprocessorOutput","PPO")
		aRename:Add("AllowUnsafeCode","UnSafe")
		aRename:Add("VOCompatibleStringInit","VO2")
		aRename:Add("AllMethodsVirtual","VO3")
		aRename:Add("ImplicitSignConversions","VO4")
		aRename:Add("VOCompatibleCallingConventions","VO5")
		aRename:Add("VOResolveFunctionPointers","VO6")
		aRename:Add("VOCompatibleImplicitConversions","VO7")
		aRename:Add("VOCompatiblePreprocessor","VO8")
		aRename:Add("VOCompatibleImplicitRetvals","VO9")
		aRename:Add("VOCompatibleIIF","VO10")
		aRename:Add("VOCompatibleArithmeticConversions","VO11")
		aRename:Add("IntegerDivisionsReturnFloat","VO12")
		aRename:Add("VOCompatibleStringComparisons","VO13")
		aRename:Add("VOFloatLiterals","VO14")
		aRename:Add("AdditionalOptions","CommandLineOption")
		// Items to delete

		VAR aTemp := List<STRING>{}
		aTemp:Add("DisabledWarnings")
		aTemp:Add("ProjectName")
		aTemp:Add("ProjectExt")
		aTemp:Add("ProjectDir")
		aTemp:Add("ProjectFileName")
		aTemp:Add("ProjectPath")
		aTemp:Add("ProjectView")
		aTemp:Add("SchemaVersion")
		aDelete := List<STRING>{}
		FOREACH s AS STRING IN aTemp
			aDelete:Add(s:ToLower())
		NEXT


	RETURN

STATIC METHOD Convert(cFile AS STRING, oProgress AS IProgress, lUseXsRt AS LOGIC) AS VOID
	LOCAL oConverter AS ProjectConverter
	LOCAL cSource AS STRING
	LOCAL cTarget AS STRING
	oConverter := ProjectConverter{oProgress,lUseXsRT}
	cSource := cFile
	cTarget := System.IO.Path.ChangeExtension(cSource, EXTENSION)
	IF oConverter:ConvertProjectFile(cSource, cTarget,lUseXsRt)
		oProgress:WriteLine( "Converted "+System.IO.Path.GetFileName(cSource)+" to " +System.IO.Path.GetFileName(cTarget))
	ELSE
		oProgress:WriteLine( "An error occurred")
	ENDIF
	RETURN


END CLASS



