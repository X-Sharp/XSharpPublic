USING System.Collections.Generic
USING System.Xml     
USING STATIC System.Console
FUNCTION Start AS VOID
	LOCAL cLine AS STRING      
	LOCAL cCopy	AS STRING
	DO WHILE TRUE
		WriteLine("Enter name name of the solution or the project to convert <Empty = Exit>:")
		cLine := ReadLine():Trim()                                                    
		IF String.IsNullOrEmpty(cLine)
			EXIT
		ENDIF                                                         
		WriteLine("Copy the source files to .XS files Yes/No ? <Empty = Yes>:")       
		cCopy := ReadLine():Trim()                                                    
		IF String.IsNullOrEmpty(cCopy)
			cCopy := "Y"
		ELSE
			cCopy := cCopy:Substring(0,1):ToUpper()
		ENDIF                                                         
		IF System.IO.File.Exists(cLine)
			IF System.IO.Path.GetExtension(cLine):Tolower() == ".sln"
				ConvertSolution(cLine, cCopy == "Y")
			ELSEIF System.IO.Path.GetExtension(cLine):Tolower() == ".vnproj"
				ConvertProject(cLine, cCopy == "Y")  
			ELSE
				WriteLine("Invalid project extension (sln or vnproj expected)")
			ENDIF
		ELSE
			WriteLine(cLine +" not found")
		ENDIF
	END DO
	RETURN 
FUNCTION ConvertSolution(cFile AS STRING, lCopy AS LOGIC) AS VOID
	LOCAL oSolutionConverter AS SolutionConverter
	oSolutionConverter := SolutionConverter{cFile, lCopy}
	IF oSolutionConverter:Convert()
		WriteLine( "Converted " +cFile)
	ELSE                     
		WriteLine( "Error converting "+cFile)
	ENDIF
FUNCTION ConvertProject(cFile AS STRING, lCopy AS LOGIC) AS VOID
	LOCAL oConverter AS ProjectFileConverter
	LOCAL cSource AS STRING
	LOCAL cTarget AS STRING
	oConverter := ProjectFileConverter{}
	cSource := cFile
	cTarget := System.IO.Path.ChangeExtension(cSource, "xsprj")
	IF oConverter:ConvertProjectFile(cSource, cTarget, lCopy)
		WriteLine( "Converted "+System.IO.Path.GetFileName(cSource)+" to " +System.IO.Path.GetFileName(cTarget))
	ELSE
		WriteLine( "An error occurred")
	ENDIF
	RETURN
CLASS ProjectFileConverter
	PROTECT oDoc 	AS XmlDocument  
	PROTECT nGroup  AS LONG
	PROTECT cSchema AS STRING  
	PROTECT cGuid   AS STRING
	PROTECT lCopyFiles AS LOGIC
	PROTECT cPath	AS STRING
	PROPERTY Guid AS STRING GET cGuid
	CONSTRUCTOR()
		cSchema := "http://schemas.microsoft.com/developer/msbuild/2003"
		nGroup  := 0
        cGuid   := System.Guid.NewGuid().ToString("B"):ToUpper()
        
METHOD ConvertProjectFile(cSource AS STRING, cTarget AS STRING, lCopy AS LOGIC) AS LOGIC
	WriteLine("Processing ... "+System.IO.Path.GetFileName(cSource))
	SELF:lCopyFiles := lCopy 
	cPath := System.IO.Path.GetDirectoryName(cSource)+"\"
	IF ! SELF:LoadFile(cSource)
		RETURN FALSE
	ENDIF                            
	SELF:WalkNode(oDoc)   
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
	LOCAL cSource, cTarget AS STRING
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
			// copy prg -> xs
			IF lCopyFiles
				oAttribute := (XmlAttribute) oElement:Attributes:GetNamedItem("Include")
				IF oAttribute != NULL
					cSource := oAttribute:Value				
					IF System.IO.Path.GetExtension(cSource:Trim()):ToLower() == ".prg"
						cTarget := System.IO.Path.ChangeExtension(cSource, ".xs")
						IF System.IO.File.Exists(cPath+cSource)
							System.IO.File.Copy(cPath+cSource, cPath+cTarget, TRUE)
							oAttribute:Value := cTarget    
							WriteLine("Created "+cTarget)
						ENDIF
					ENDIF
				ENDIF
			ENDIF		
		CASE "none"    
			// Copy .vh files to .xh
			IF lCopyFiles
				oAttribute := (XmlAttribute) oElement:Attributes:GetNamedItem("Include")
				IF oAttribute != NULL
					cSource := oAttribute:Value				
					IF System.IO.Path.GetExtension(cSource:Trim()):ToLower() == ".vh"
						cTarget := System.IO.Path.ChangeExtension(cSource, ".xh")
						IF System.IO.File.Exists(cPath+cSource)
							System.IO.File.Copy(cPath+cSource, cPath+cTarget, TRUE)
							oAttribute:Value := cTarget    
							WriteLine("Created "+cTarget)
						ENDIF
					ENDIF
				ENDIF
			ENDIF		

		CASE "import"               
			// change import 
			oAttribute := (XmlAttribute) oElement:Attributes:GetNamedItem("Project")
			IF oAttribute:Value:ToLower():Contains("vulcan.net")
				oAttribute:Value := "$(XSharpProjectExtensionsPath)XSharp.props"
			ENDIF
		CASE "projectguid"   
			// new guid
			oElement:InnerText := cGuid
		CASE "projectreference"
			// update extension in project references 
			oAttribute := (XmlAttribute) oElement:Attributes:GetNamedItem("Include")
			IF oAttribute != NULL
				oAttribute:Value := oAttribute:Value:Replace(".vnproj", ".xsprj")
			ENDIF
		CASE "propertygroup"
			// Only insert code before the first propertygroup
			IF ++nGroup == 1
				LOCAL oImport AS XmlNode
				// Add Import
				oImport := oDoc:CreateElement("Import",cSchema)
				oAttribute := oDoc:CreateAttribute("Project")
				oAttribute:Value := "$(MSBuildExtensionsPath)\XSharp\XSharp.Default.props"
				oImport:Attributes:Append(oAttribute)
				oParent:InsertBefore(oImport, oElement)
				// Add propertygroup before Import with Label="Globals" and child <XSharpProjectExtensionsPath>
				LOCAL oGroup AS XmlNode
				oGroup := oDoc:CreateElement("PropertyGroup",cSchema)
				oAttribute := oDoc:CreateAttribute("Label")
				oAttribute:Value := "Globals"
				oGroup:Attributes:Append(oAttribute)
				oParent:InsertBefore(oGroup, oImport)
				oChild := oDoc:CreateElement( "XSharpProjectExtensionsPath", cSchema)
				oChild:InnerText := "$(MSBuildExtensionsPath)\XSharp\"
				oGroup:AppendChild(oChild)   
	
				LOCAL lHasCondition := FALSE AS LOGIC
				IF oElement:HasAttributes    
					lHasCondition := (oElement:Attributes:GetNamedItem("Condition") != NULL)
				ENDIF
				IF ! lHasCondition
					oChild := oDoc:CreateElement("Platform", cSchema)
					oAttribute := oDoc:CreateAttribute("Condition")
					oAttribute:Value := " '$(Platform)' == '' "
					oChild:Attributes:Append(oAttribute)
					oChild:InnerText := "AnyCPU"
					oElement:PrependChild(oChild)
				ENDIF
			ENDIF
		CASE "projectextensions"
			// Import the schema
			oChild := oDoc:CreateElement("Import", cSchema)
			oAttribute := oDoc:CreateAttribute("Project")
			oAttribute:Value := "$(XSharpProjectExtensionsPath)XSharp.targets"
			oChild:Attributes:Append(oAttribute)   
			oParent:InsertBefore(oChild, oElement)  
			// add <ProjectCapabilities><ProjectConfigurationsDeclaredAsItems /></ProjectCapabilities>
			oChild := oDoc:CreateElement("ProjectCapabilities",cSchema)
			oChild:AppendChild(oDoc:CreateElement("ProjectConfigurationsDeclaredAsItems",cSchema))
			oElement:AppendChild(oChild)
		OTHERWISE
			NOP
		END SWITCH
	ENDIF                                   
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
	
        
END CLASS        

CLASS SolutionConverter
	PROTECT cFileName AS STRING
	PROTECT cBackup   AS STRING    
	PROTECT aGuids	  AS Dictionary<STRING, STRING>
	PROTECT aFiles    AS List<STRING>
	PROTECT cPath	  AS STRING                
	PROTECT lCopyFiles AS LOGIC
	CONSTRUCTOR(cName AS STRING, lCopy AS LOGIC)
		cFileName := cName
		cBackup   := System.IO.Path.ChangeExtension(cFileName, ".bak")
		cPath     := System.IO.Path.GetDirectoryName(cFileName)  
		lCopyFiles:= lCopy
		IF ! cPath:EndsWith("\")
			cPath += "\"
		ENDIF
		System.IO.File.Copy(cFileName, cBackup, TRUE)
		aGuids := Dictionary<STRING, STRING>{}
		aFiles := List<STRING> {}
	METHOD Convert() AS LOGIC
		LOCAL oReader AS System.IO.TextReader
		LOCAL oWriter AS System.IO.TextWriter
		WriteLine("Processing..."+cBackup)
		oReader := System.IO.StreamReader{cBackup}
		oWriter := System.IO.StreamWriter{cFileName}
		DO WHILE oReader:Peek() >= 0
			LOCAL cLine AS STRING
			cLine := oReader:ReadLine()
			IF cLine:Trim():StartsWith("Project(", StringComparison.OrdinalIgnoreCase) 
				IF cLine:ToUpper():Contains("{5891B814-A2E0-4E64-9A2F-2C2ECAB940FE}")
					// Vulcan Project
					// Line looks like this:
					// Project("VULCANGUID") = "Name", "Filename.vnproj", "PROJECTGUID"
					// We can split the line based on the double quote
					// then we get
					// a[1] = Project(
					// a[2] = VULCANGUID
					// a[3] = ) = 
					// a[4] = Name
					// a[5] = , 
					// a[6] = Filename.vnproj
					// a[7] = ,
					// a[8] = PROJECTGUID                        
					// a[9] = <empty>
					// We can then convert the vnproj file and change it to xsproj
					// and replace VULCANGUID with XSHUID
					// RelativePath with changed extension
					// ProjectGUID with new GUID
					LOCAL aElements AS STRING[]
					aElements := cLine:Split(e"\"":ToCharArray(), StringSplitOptions.None)
					IF aElements:Length == 9
						LOCAL cNewFile AS STRING
						LOCAL oConverter AS ProjectFileConverter
						oConverter := ProjectFileConverter{}
						aElements[2] := "{AA6C8D78-22FF-423A-9C7C-5F2393824E04}"	// XS Guid
						cNewFile := System.IO.Path.ChangeExtension(aElements[6], ".xsprj")
						oConverter:ConvertProjectFile(cPath+aElements[6], cPath+cNewFile, lCopyFiles)     
						aFiles:Add(cPath+cNewFile)
						aElements[6] := cNewFile      
						IF ! aGuids:ContainsKey(aElements[8])
							aGuids:Add(aElements[8], oConverter:Guid)
						ENDIF
						aElements[8] := oConverter:Guid    
						
						cLine := ""
						FOREACH VAR cElement IN aElements
							IF !String.IsNullOrEmpty(cLine)
								cLine += e"\""
							ENDIF
							cLine += cElement
						NEXT
					ELSE
						// Something wrong  
						WriteLine( "Incorrect line in Solution:", cLine)
						
					ENDIF
				ENDIF
			ELSE
				FOREACH VAR item IN aGuids
					IF cLine:Contains(item:Key)
						cLine := cLine:Replace(item:Key, item:Value)
					ENDIF
				NEXT				
			ENDIF
			oWriter:WriteLine(cLine)
		ENDDO    
		oReader:Close()
		oWriter:Close()
		// Now update project references in all xsprj files
		FOREACH VAR sFile IN aFiles
			LOCAL sContents AS STRING
			LOCAL lChanged := FALSE AS LOGIC
			sContents := System.IO.File.ReadAllText(sFile)
			FOREACH VAR sItem IN aGuids
				IF sContents:Contains(sItem:Key)
					sContents := sContents:Replace(sItem:Key, sItem:Value)
					lChanged := TRUE
				ELSEIF 	sContents:Contains(sItem:Key:ToLower())
					sContents := sContents:Replace(sItem:Key:ToLower(), sItem:Value)
					lChanged := TRUE
				ENDIF
			NEXT                    
			IF lChanged
				System.IO.File.WriteAllText(sFile, sContents)
			ENDIF
		NEXT
		
		RETURN TRUE		
		
		
		
END CLASS	

