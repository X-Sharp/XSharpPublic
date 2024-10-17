USING System.Collections.Generic
USING System.Xml
USING System.Linq
USING STATIC System.Console
FUNCTION Start(args as STRING[])  AS VOID
    LOCAL cCopy AS STRING
    LOCAL aFiles AS IList<STRING>
    LOCAL lRemove := FALSE as LOGIC
    TRY
        IF args?:Length > 0
            lRemove := args:Count( { sArg => sArg:ToUpper():Contains("UNINSTALL") }) > 0
        ENDIF
        WriteToLog("------------------------------------------------------")
        WriteToLog("Updating CodeDomProvider "+iif (lRemove, "Remove", "Add" )+" XSharp")
        WriteToLog("------------------------------------------------------")
        aFiles := GetConfigFiles()
        FOREACH VAR cFile IN aFiles
            WriteToLog("Processing file "+cFile)
            cCopy :=System.IO.Path.ChangeExtension(cFile, ".X#Bak")
            VAR bakFiles := System.IO.Directory.GetFiles(System.IO.Path.GetDirectoryName(cCopy),"*.X#Bak*")
            FOREACH var sFile in bakFiles
                WriteToLog("Deleting old backup "+sFile)
                System.IO.File.Delete(sFile)
            NEXT
            WriteToLog("Writing backup to "+cCopy)
            System.IO.File.Copy(cFile, cCopy, TRUE)
            IF lRemove
                ConfigPatcher{}:RemoveFromMachineConfig(cFile)
            ELSE
                ConfigPatcher{}:PatchMachineConfig(cFile)
            ENDIF
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

    METHOD RemoveFromMachineConfig(cFile AS STRING) AS LOGIC
        WriteLine("Processing ... "+System.IO.Path.GetFileName(cFile))
        IF ! SELF:LoadFile(cFile)
            RETURN FALSE
        ENDIF
        SELF:WalkNode(oDoc)
        IF oConfig == NULL_OBJECT
            WriteToLog( "Invalid Machine.config, Configuration node missing")
            RETURN FALSE
        ENDIF
        IF oCodeDom != NULL_OBJECT
            IF oCompilers != NULL_OBJECT
                IF oXSharp != NULL_OBJECT
                    WriteToLog( "Deleting old XSharp Node")
                    oCompilers:RemoveChild(oXSharp)
                    oXSharp := NULL_OBJECT
                ENDIF
                if oCompilers:ChildNodes:Count == 0
                    WriteToLog( "Deleting Compilers Node")
                    oCodeDom:RemoveChild(oCompilers)
                    oCompilers := NULL_OBJECT
                endif
                if oCodeDom:ChildNodes:Count == 0
                    WriteToLog( "Deleting System.CodeDom Node")
                    oConfig:RemoveChild(oCodeDom)
                    oCodeDom := NULL_OBJECT
                endif
            ENDIF
        ENDIF
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

FUNCTION GetConfigFiles AS IList<STRING>
    var sFolder := System.Runtime.InteropServices.RuntimeEnvironment.GetRuntimeDirectory()
    var pos     := sFolder:ToLower():IndexOf("framework")
    var sBase   := sFolder:Substring(0, pos)
    var aFiles   := System.IO.Directory.GetFiles(sBase, "machine.config", System.IO.SearchOption.AllDirectories)
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



DEFINE PROVIDERVERSION := "XSharp.CodeDom.XSharpCodeDomProvider,XSharp.CodeDomProvider, Version="+XSharp.Constants.Version+", Culture=neutral, PublicKeyToken=ed555a0467764586, ProcessorArchitecture=MSIL" AS STRING
