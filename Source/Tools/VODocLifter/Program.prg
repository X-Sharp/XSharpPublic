USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
using SYstem.Xml
using System.Reflection

global aAssemblies as List<Assembly>
global oRT   as Assembly
global oVO   as Assembly

BEGIN NAMESPACE ConsoleApplication8

	FUNCTION Start() AS VOID STRICT
        LOCAL oRdr := VoDocReader{} AS VoDocReader
        aAssemblies := List<Assembly>{}
        aAssemblies:Add(System.Reflection.Assembly.LoadFrom("c:\XSharp\DevRt\Binaries\Documentation\VOWin32APILibrary.dll"))
        aAssemblies:Add(System.Reflection.Assembly.LoadFrom("c:\XSharp\DevRt\Binaries\Documentation\VOSystemClasses.dll"))
        aAssemblies:Add(System.Reflection.Assembly.LoadFrom("c:\XSharp\DevRt\Binaries\Documentation\VORDDClasses.dll"))
        aAssemblies:Add(System.Reflection.Assembly.LoadFrom("c:\XSharp\DevRt\Binaries\Documentation\VOSQLClasses.dll"))
        aAssemblies:Add(System.Reflection.Assembly.LoadFrom("c:\XSharp\DevRt\Binaries\Documentation\VOGUIClasses.dll"))
        aAssemblies:Add(System.Reflection.Assembly.LoadFrom("c:\XSharp\DevRt\Binaries\Documentation\VOReportClasses.dll"))
        aAssemblies:Add(System.Reflection.Assembly.LoadFrom("c:\XSharp\DevRt\Binaries\Documentation\VOInterNetClasses.dll"))
        oRdr:Read()
	
END NAMESPACE 



CLASS VoDocReader
    PROTECT oDoc 	AS XmlDocument
    PROTECT cPath   AS STRING
    PROTECT cFile   AS STRING
    protect currentSection as string
    protect CurrentLines as List<String>
    PROTECT cTitle AS STRING
    PROTECT Title   AS List<STRING>
    protect Arguments as List<String>
    protect Returns as List<String>
    protect Description as List<String>
    protect Notes as List<String>
    protect Syntax as List<String>
    protect Examples as List<String>
    protect SeeAlso as List<String>
    PROTECT Ignore AS List<STRING>
    PROTECT output AS stringBuilder
    protect inCode as LOGIC
    PROTECT inList AS LOGIC
    CONSTRUCTOR()
        cPath := "c:\XSharp\VOHelp\Cavo\CavoRef\Topics"

    METHOD Read() AS VOID STRICT
        VAR files := System.IO.Directory.GetFiles(cPath, "*.xml")
        output := StringBuilder{}
        output:AppendLine(e"<?xml version=\"1.0\" encoding=\"utf-8\"?>")
        output:AppendLine("<doc><members>")
        FOREACH VAR sFile IN files
            VAR sFileName := System.IO.Path.GetFileName(sFile):ToLower()
            DO CASE
            CASE sFilename:StartsWith("class")
                NOP
            CASE sFilename:StartsWith("method")
                NOP
            CASE sFilename:StartsWith("a_")
                NOP
            CASE sFilename:StartsWith("export")
                NOP
            OTHERWISE
                LOOP
            ENDCASE
            console.WriteLine(sFile)
            processFile(sFile)
        NEXT
        
        output:AppendLine("</members></doc>")
        var result := output:ToString()
        result := result:Replace("&lt;%APP%&gt;","X#")
        result := result:Replace("&lt;%app%&gt;","X#")
        System.IO.File.WriteAllText("c:\XSharp\DevRt\Docs\SDKFunctionDocs.xml",result)
        console.Write("done")
        console.ReadLine()

    method processfile(sFile as string) as void
        var oDoc := System.Xml.XmlDocument{}
        VAR reader := System.IO.StreamReader{sFile,TRUE}
        cFile       := sFile
        Title       := List<String>{}
        Arguments   := List<String>{}
        Returns     := List<String>{}
        Description := List<String>{}
        Notes       := List<String>{}
        Syntax      := List<String>{}
        Examples    := List<String>{}
        SeeAlso     := List<STRING>{}
        Ignore      := List<STRING>{}
        CurrentLines := Title
        oDoc:Load(reader)
        reader:Close()
        SELF:WalkNode(oDoc)
        sFile := System.IO.Path.GetFileNameWithoutExtension(sFile):ToLower()
        sFile := sFile:Replace("function_","")
        output:AppendLine(e"<member name=\""+cTitle+e"\">")
        DumpSection(Title, "summary")
        DumpParameters(Arguments)
        DumpSection(Returns,"returns")
        local hasNotes := false as logic
        FOREACH VAR sLine IN Notes
            if sLine != "<br/>"
                if (! hasNotes)
                    Description:Add(e"<note type=\"tip\">")
                    hasNotes := true
                endif
                Description:Add(sLine)
            endif
        next
        if (hasNotes)
            Description:Add("</note>")
        endif
        DumpSection(Description,"remarks")
        //DumpSection("Syntax", Syntax,"")
        DumpSection(Examples,"example")
        DumpSection(Seealso,"")
        output:AppendLine("</member>")

        
    METHOD WalkNode(oNode AS XmlNode) AS VOID
	    LOCAL aChildren AS List<XmlNode>
	    aChildren := List<XmlNode>{}
	    FOREACH oChild AS XmlNode IN oNode:ChildNodes
		    aChildren:Add(oChild)
	    NEXT
        inCode := FALSE
	    FOREACH oChild AS XmlNode IN aChildren
		    SELF:WalkNode((XMlNode) oChild)
            if oChild:Name == "para"
                ProcessPara(oChild)
            ELSEIF oChild:Name == "title"
                ProcessTitle(oChild)
            ENDIF
	    NEXT
	    RETURN

    METHOD ProcessPara(oNode AS XmlNode) AS VOID
        local cLine := "" as STRING
        local lCode := FALSE as LOGIC
        if oNode:ChildNodes:Count > 0
            foreach oText as XmlNode in oNode:ChildNodes
                if oText:Name == "link"
                    // Process see also and other links later
                    NOP
                endif
                if oText:Name == "tab"
                    cLine += "&#0009;"
                endif
                IF oText:Name == "br"
                    cLine += "<br/>"+environment.NewLine
                ENDIF
                VAR atts := oText:Attributes
                foreach att as XmlAttribute in atts
                    if att:Name:ToLower() == "styleclass"
                        switch att:Value
                        CASE "Heading1"
                            IF SELF:InList
                                SELF:InList := FALSE
                                SELF:CurrentLines:Add("</list>")
                            ENDIF
                            switch oText:InnerText:ToLower()
                                case "syntax"
                                    CurrentLines := Syntax
                                case "arguments"
                                    CurrentLines := Arguments
                                case "returns"
                                    CurrentLines := Returns
                                CASE "description"
                                    IF Title:Count == 0
                                        CurrentLines := Title
                                    ELSE
                                        CurrentLines := Description
                                     ENDIF
                                case "notes"
                                CASE "note"
                                    CurrentLines := Notes
                                CASE "examples"
                                case "example"
                                    CurrentLines := Examples
                                case "prototype"
                                    CurrentLines := Syntax
                                case "seealso"
                                case "see also"
                                    CurrentLines := SeeAlso
                                CASE "class"
                                CASE "properties"
                                CASE "methods"
                                    CurrentLines := Ignore
                                CASE "purpose"
                                    NOP
                                OTHERWISE
                                   NOP
                            END SWITCH
                        case "Normal"
                            cLine += oText:InnerXML
                        CASE "List"
                            cLine += oText:InnerXML
                        case "Code Example"
                            lCode := TRUE
                            cLine += oText:InnerXML
                        OTHERWISE

                            cLine += oText:InnerXML
                        end switch
                    endif
                next
            NEXT
            DO WHILE cLine:EndsWith("&#0009;")
                cLine := cLine:Substring(0,cLine:Length-"&#0009;":Length)
            ENDDO

            IF currentLines == SELF:Description
                IF cLine:ToLower():Contains( "&#0009;description")
                    SELF:inList := TRUE
                    VAR cPref := cLine:Substring(0, cLine:IndexOf("&#0009;description",StringComparison.OrdinalIgnoreCase))
                    cLine := e"<list type=\"table\">\r\n<listheader><term>"+cPref+"</term><description>Description</description></listheader>"
                ELSEIF SELF:inList .AND. cLine:Contains("&#0009;")
                    DO WHILE cLine:Contains("&#0009;&#0009;")
                        cLine := cLine:Replace("&#0009;&#0009;","&#0009;")
                    ENDDO
                    cLine := "<item><term>"+cLine:Replace("&#0009;","</term><description>")+"</description></item>"
                ELSEIF SELF:inList
                    SELF:InList := FALSE
                    CurrentLines:Add("</list>")
                ENDIF
            ENDIF
            cLine := cLine:Replace("&#0009;<br/>","<br/>")
            IF ! String.IsNullOrEmpty(cLine)
                IF lCode
                    cLine := "<code>"+cLine+"</code>"
                endif
                if !cLine:Contains("uses the PASCAL calling convention")
                    CurrentLines:Add(cLine)
                endif
            elseif CurrentLines:Count > 0
                CurrentLines:Add("<br/>")
            ENDIF
        ENDIF

    METHOD ProcessTitle(oNode AS XmlNode) AS VOID
        cTitle := oNode:InnerText:Replace(":","."):Trim()
        IF cTitle:EndsWith("Access/Assign",StringComparison.OrdinalIgnoreCase)
            cTitle := "P:"+cTitle:Replace("Access/Assign","")
            cTitle := cTitle:Replace("access/assign","")
        ELSEIF cTitle:EndsWith("Access",StringComparison.OrdinalIgnoreCase)
            cTitle := "P:"+cTitle:Replace("Access","")
            cTitle := cTitle:Replace("access","")
        ELSEIF cTitle:EndsWith("Assign",StringComparison.OrdinalIgnoreCase)
            cTitle := "P:"+cTitle:Replace("Assign","")
            cTitle := cTitle:Replace("assign","")
        ELSEIF cTitle:EndsWith("Class",StringComparison.OrdinalIgnoreCase)
            cTitle := "T:"+cTitle:Replace("Class","")
            cTitle := cTitle:Replace("class","")
            
        ELSEIF cTitle:EndsWith("Export",StringComparison.OrdinalIgnoreCase)
            cTitle := "F:"+cTitle:Replace("Export","")
            cTitle := cTitle:Replace("export","")
        ELSEIF cTitle:EndsWith("Method",StringComparison.OrdinalIgnoreCase)
            cTitle := "M:"+cTitle:Replace("Method",""):Replace("()","")
            cTitle := cTitle:Replace("method","")
        ELSEIF cFile:IndexOf("class_",Stringcomparison.OrdinalIgnoreCase) >= 0
            cTitle := "T:"+cTitle
        ELSE
            cTitle := "M:"+cTitle:Replace("()","")
        ENDIF
        cTitle := cTitle:Trim()
        IF cTitle:EndsWith(".Init")
            cTitle := cTitle:Replace(".Init",".#ctor")
        ENDIF
        LOCAL cClass, cMember,cParams AS STRING
        IF ! cTitle:StartsWith("T:")
            cClass  := cTitle:Substring(2)
            cClass  := cClass:Substring(0, cClass:IndexOf("."))
            cClass  := "VO."+cClass
            cMember := cTitle:Substring(cTitle:IndexOf(".")+1)
            cParams := ""
            FOREACH VAR oAsm IN aAssemblies
                VAR oType := oAsm:GetType(cClass,FALSE,TRUE)
                IF oType != NULL
                    cClass := oType:FullName
                    LOCAL oMembers := oType:GetMember(cMember:Replace("#","."),BindingFlags.IgnoreCase+BindingFlags.Public+BindingFlags.Instance) AS MemberInfo[]
                    IF oMembers != NULL .AND. oMembers:Length > 0
                        VAR oMember := oMembers[1]
                        cMember := oMember:Name
                        IF oMember IS ConstructorInfo VAR ctor
                            FOREACH param AS parameterinfo IN ctor:GetParameters()
                                IF cParams:Length > 0
                                    cParams += ","
                                ENDIF
                                cParams += param:ParameterType:FullName:Replace("&","@")
                            NEXT
                        ELSEIF oMember IS MethodInfo VAR met
                            FOREACH param AS parameterinfo IN met:GetParameters()
                                IF cParams:Length > 0
                                    cParams += ","
                                ENDIF
                                cParams += param:ParameterType:FullName:Replace("&","@")
                            NEXT
                        ENDIF
                    ENDIF
                ENDIF
            NEXT
            IF cParams:Length > 0
                cParams := "("+cParams+")"
            ENDIF
            cTitle := cTitle:Substring(0,2)+cClass+"."+cMember:Replace(".","#")+cParams
        ELSE
                cTitle := cTitle:Substring(0,2)+"VO."+cTitle:Substring(2)
            
        ENDIF
        RETURN

    Method DumpSection(lines as List<String>, tag as string) as void
        local lastIsCode := FALSE as LOGIC
        local aTemp as List<String>
        local lHasCode := FALSE as LOGIC
        aTemp := List<string>{}
        IF (lines == SeeAlso)
            /*
            foreach var c in Lines
                var aElements := c:Split(',',StringSplitOptions.RemoveEmptyEntries)
                foreach var s in aElements
                    VAR sElement := s:Trim():Replace("()","")
                    FOREACH oAsm AS Assembly IN aAssemblies
                        VAR type := oVO:GetType("XSharp.VO.Functions")
                        IF funcs:Length > 1
                            LOCAL proto AS STRING
                            // /// <seealso cref='M:XSharp.Core.Functions.L2Bin(System.Int32)' >L2Bin</seealso>
                            proto := "O:"+type:FullName+"."+funcs[1]:Name
                            VAR sLine := "<seealso cref='"+proto+"'>"+sElement+" Overload</seealso>"
                            output:AppendLine(sLine)
                        ELSEIF funcs:Length == 1
                            LOCAL funcinfo := (System.Reflection.MethodInfo) funcs[1] AS System.Reflection.MethodInfo
                            LOCAL proto AS STRING
                            // /// <seealso cref='M:XSharp.Core.Functions.L2Bin(System.Int32)' >L2Bin</seealso>
                            proto := "M:"+type:FullName+"."+funcinfo:Name
                            VAR pars := funcinfo:GetParameters()
                            IF pars:Length > 0
                                proto += "("
                                LOCAL first := TRUE AS LOGIC
                                FOREACH p AS ParameterInfo IN pars
                                    IF ! first
                                        proto += ","
                                    ENDIF
                                    VAR partype := p:ParameterType:FullName
                                    IF !String.IsNullOrEmpty(partype)
                                        partype := partype:Replace("&","@")
                                        proto += partype
                                    ENDIF
                                    first := FALSE
                                NEXT
                                proto += ")"
                            ENDIF
                            VAR sLine := "<seealso cref='"+proto+"'>"+sElement+"</seealso>"
                            output:AppendLine(sLine)
                        ELSE
                        VAR sLine := "<seealso>"+sElement:Trim()+"</seealso>"
                        output:AppendLine(sLine)
                    endif
                next
            next
            RETURN
                    */
        ENDIF
        IF ! String.IsNullOrEmpty(tag)
            VAR cTemp := ""
            FOREACH VAR c IN lines
                cTemp += c
            NEXT
            IF cTemp:Trim():Length > 0
                output:AppendLine("<"+tag+">")
                SELF:inList := FALSE
                FOREACH VAR c IN lines
                    VAR cLine := c:Trim()
                    IF cLine:Contains("<list")
                        SELF:inList := TRUE
                    ENDIF
                    IF cLine:Contains("</list>")
                        SELF:inList := FALSE
                    ENDIF
                    cLine := cLine:Replace("</code><code>","")
                    IF ! String.IsNullOrEmpty(cLine:Trim())
                        IF lastIsCode .AND. cLine:StartsWith("<code>")
                            cLine := cLine:Replace("<code>","")
                        ENDIF
                        LastIsCode := cLine:Contains("<code>") .OR. cLine:Contains("</code>")
                        lHasCode := lHasCode .OR. LastIsCode
                    ENDIF
                    aTemp:Add(cLine)
                NEXT
                IF lHasCode 
                    LOCAL i AS LONG
                    FOR i := 0 TO aTemp:Count-1
                        LOCAL sLine AS STRING
                        sLine := aTemp[i]
                        IF i < aTemp:Count-1 .AND. sLine:Contains("</code>")
                            LOCAL sNext AS STRING
                            sNext := aTemp[i+1]
                            IF sNext:Contains("<code>") .OR. sNext:Contains("</code>")
                               sLine := sLine:Replace("</code>", "")
                            ENDIF    
                        ENDIF
                        aTemp[i] := sLine
                    NEXT
                ENDIF
                FOREACH VAR s IN aTemp
                    VAR sLine := s
                    IF sLine:Contains("<code>")
                        sLine := sLine:Replace("<code>",e"<code language=\"X#\">")
                    ENDIF
                    output:AppendLine(sLine)
                NEXT
                IF SELF:inList
                    output:AppendLine("</list>")
                    SELF:inList := FALSE
                ENDIF
                output:AppendLine("</"+tag+">")
            endif
        ENDIF

    Method DumpParameters(lines as List<String>) as void
        local lastLine := "" as string
        FOREACH aline AS STRING IN lines
            VAR line := aline:Replace('"','\'')
            var index := line:IndexOf("&#0009;")
            VAR name := line
            IF (index >= 0)
                name   := line:Substring(0, index)
                name := name:replace("&lt;","")
                name := name:replace("&gt;",""):Trim()
                VAR desc    := line:Substring(index+7)
                WHILE desc:StartsWith("&#0009;")
                    desc := desc:Substring(7)
                END
                IF String.IsNullOrEmpty(name) .OR. name:StartsWith("<code>")
                    IF lastline:EndsWith("</param>")
                        lastline := lastline:Replace("</param>","")
                    ENDIF
                    lastline += "<br/>"+Environment.NewLine+"&#0009;"+name+desc+"</param>"
                ELSE
                    IF lastline:IndexOf("<param") == -1 .AND.  lastline:Trim():Length > 0
                        lastline := "<param>"+lastline
                    ENDIF
                    output:AppendLine(lastline)
                    lastline := e"<param name=\""+name+e"\">"+desc+"</param>"
                ENDIF
            endif            
        next
        IF ! String.IsNullOrEmpty(lastline)
            IF lastline:IndexOf("<param") == -1 .AND.  lastline:Trim():Length > 0
                lastline := "<param>"+lastline
            ENDIF
            output:AppendLine(lastline)
        endif

END CLASS


