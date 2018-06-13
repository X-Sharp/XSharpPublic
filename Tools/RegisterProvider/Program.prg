#include "BuildNumber.h"
DEFINE PROVIDERVERSION := "XSharp.CodeDom.XSharpCodeDomProvider,XSharpCodeDomProvider, Version="+VERSION_NUMBER_STR+", Culture=neutral, PublicKeyToken=ed555a0467764586, ProcessorArchitecture=MSIL" AS STRING
USING System.Collections.Generic
USING System.Xml     
USING STATIC System.Console
FUNCTION Start AS VOID
	LOCAL cCopy AS STRING   
	LOCAL aFiles AS List<STRING>  
	TRY
		WriteToLog("---------------------------")
		WriteToLog("Registering CodeDomProvider")
		WriteToLog("---------------------------")
		aFiles := GetConfigFiles()
		FOREACH VAR cFile IN aFiles
			WriteToLog("Processing file "+cFile)
			VAR nNext := 0
			cCopy := System.IO.Path.ChangeExtension(cFile, ".X#Bak")
			DO WHILE System.IO.File.Exists(cCopy)
				nNext += 1
				cCopy := System.IO.Path.ChangeExtension(cFile, ".X#Bak")
				cCopy += nNext:ToString()
			ENDDO
			WriteToLog("Writing backup to "+cCopy)
			System.IO.File.Copy(cFile, cCopy, TRUE)  
			ConfigPatcher{}:PatchMachineConfig(cFile)  
			VAR cOld := System.IO.File.ReadAllText(cFile)
			VAR cNew := System.IO.File.ReadAllText(cCopy)
			IF String.Compare(cOld, cNew) == 0    
				WriteToLog("No changes, kill backup")
				System.IO.File.Delete(cCopy) 
			ELSE
				WriteToLog("File was changed, keep backup")
			ENDIF
		NEXT
	CATCH e AS Exception   
		WriteToLog("Exception:")
		WriteToLog(e:Message)
		
		Console.WriteLine("Failed to update Machine.config:")
		Console.WriteLine(e:Message)
		Console.WriteLine("Make sure you are running this program as administrator")
		Console.ReadLine()
	END TRY
	RETURN 
CLASS ConfigPatcher
	PROTECT oDoc 	AS XmlDocument     
	PROTECT oConfig AS XMLElement
	PROTECT oCodeDom AS XMLElement
	PROTECT oCompilers AS XMLElement
	PROTECT oXSharp AS XMLElement
	        
METHOD PatchMachineConfig(cFile AS STRING) AS LOGIC
	WriteLine("Processing ... "+System.IO.Path.GetFileName(cFile))
	IF ! SELF:LoadFile(cFile)
		RETURN FALSE
	ENDIF                        
	SELF:WalkNode(oDoc)     
	IF oConfig == NULL_OBJECT
		WriteToLog( "Invalid Machine.config, Configuration node missing")
		RETURN FALSE
	ENDIF
	IF oCodeDom == NULL_OBJECT
		WriteToLog( "Creating element system.codedom")
		oCodeDom := oDoc:CreateElement("system.codedom")
		oConfig:AppendChild(oCodeDom)
	ENDIF
	IF oCompilers != NULL_OBJECT
		IF oXSharp != NULL_OBJECT            
			WriteToLog( "Deleting old XSharp Node")
			oCompilers:RemoveChild(oXSharp)
			oXSharp := NULL_OBJECT
		ENDIF                     
		
	ELSE                     
		WriteToLog( "Creating element compilers")
		oCompilers := oDoc:CreateElement("compilers")
		oCodeDom:AppendChild(ocompilers)
	ENDIF                         
	WriteToLog( "Writing new XSharp Element")
	oXSharp := oDoc:CreateElement("compiler")
	oCompilers:AppendChild(oXSharp)
	oXSharp:SetAttribute("language", "XSharp")
	oXSharp:SetAttribute("extension", ".prg")
	oXSharp:SetAttribute("type",PROVIDERVERSION)
	SELF:SaveFile(cFile)
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
		// Some nodes that require special processing
		SWITCH oElement:Name:ToLower()    

		CASE "configuration"  
			oConfig := oElement
		CASE "system.codedom" 
			oCodeDom := oElement
		CASE "compilers"	  
			oCompilers := oElement
		CASE "compiler"		 
			IF oElement:HasAttributes  
				IF oElement:GetAttribute("language"):ToLower() =="xsharp"
					oXSharp := oElement
				ENDIF
			ENDIF
		OTHERWISE
			NOP
		END SWITCH
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
      
END CLASS        

FUNCTION GetConfigFiles AS List<STRING>
	VAR aFiles := System.Collections.Generic.List<STRING>{}
	VAR sFile := System.Runtime.InteropServices.RuntimeEnvironment:SystemConfigurationFile:tolower()
	VAR aVersion := <STRING>{"v1.0.3705","v1.1.4322", "v2.0.50727","v4.0.30319"}	
	VAR sMask := "" 
	VAR Is64Bits := FALSE
	Is64Bits := IntPtr.Size == 8
	FOREACH VAR sVersion IN aVersion
		IF sFile:Contains(sVersion)
		   sMask := sVersion
		ENDIF
	NEXT     
	IF String.IsNullOrEmpty(sMask)
		WriteToLog("Configfile has Unknown version: "+sFile)
		RETURN  aFiles
	ENDIF
	FOREACH VAR sVersion IN aVersion
		VAR sSearch := sFile:Replace(sMask, sVersion)
		IF System.IO.File.Exists(sSearch)
			aFiles:Add(sSearch)
		ENDIF
		IF Is64Bits
			sSearch := sSearch:Replace("\framework64","\framework")
		ELSE
			sSearch := sSearch:Replace("\framework","\framework64")
		ENDIF                            
		IF System.IO.File.Exists(sSearch)
			aFiles:Add(sSearch)
		ENDIF
	NEXT        

	RETURN aFiles
	
	
FUNCTION WriteToLog(sLine AS STRING) AS LOGIC              
	TRY
		sLine := DateTime.Now.ToString() + " " + sLine+e"\r\n"
		System.IO.File.AppendAllText("Patch-MachineConfig.Log", sLine)
		RETURN TRUE
	CATCH E AS Exception
		Console.WriteLine("Error writing to logfile: ")
		Console.WriteLine(e:Message)
		RETURN FALSE
	END TRY
