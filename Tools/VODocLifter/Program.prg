USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
using SYstem.Xml
using System.Reflection

global oCore as Assembly
global oRT   as Assembly
global oVO   as Assembly

BEGIN NAMESPACE ConsoleApplication8

	FUNCTION Start() AS VOID STRICT
        local oRdr := VoDocReader{} as VoDocReader
        oCore  := System.Reflection.Assembly.LoadFrom("c:\XSharp\DevRt\Binaries\Documentation\XSharp.Core.dll")
        oRT    := System.Reflection.Assembly.LoadFrom("c:\XSharp\DevRt\Binaries\Documentation\XSharp.RT.dll")
        oVO    := System.Reflection.Assembly.LoadFrom("c:\XSharp\DevRt\Binaries\Documentation\XSharp.VO.dll")
        oRdr:Read()
	
END NAMESPACE 



CLASS VoDocReader
    PROTECT oDoc 	AS XmlDocument
    PROTECT cPath   AS STRING
    protect currentSection as string
    protect CurrentLines as List<String>
    protect Title   as List<String>
    protect Arguments as List<String>
    protect Returns as List<String>
    protect Description as List<String>
    protect Notes as List<String>
    protect Syntax as List<String>
    protect Examples as List<String>
    protect SeeAlso as List<String>
    protect output as stringBuilder
    protect inCode as LOGIC
    CONSTRUCTOR()
        cPath := "c:\XSharp\VOHelp\Cavo\CavoRef\Topics"

    METHOD Read() AS VOID STRICT
        var files := System.IO.Directory.GetFiles(cPath, "Function_*.xml")
        output := StringBuilder{}
        output:AppendLine(e"<?xml version=\"1.0\" encoding=\"utf-8\"?>")
        output:AppendLine("<Runtimefunctions>")
        foreach var sFile in files
            processfile(sFile)
        next
        output:AppendLine("</Runtimefunctions>")
        var result := output:ToString()
        result := result:Replace("&lt;%APP%&gt;","X#")
        result := result:Replace("&lt;%app%&gt;","X#")
        System.IO.File.WriteAllText("c:\XSharp\DevRt\Runtime\XSharp.RT\VoFunctionDocs.xml",result)
        console.Write("done")
        

    method processfile(sFile as string) as void
        var oDoc := System.Xml.XmlDocument{}
        var reader := System.IO.StreamReader{sFile,TRUE}
        Title       := List<String>{}
        Arguments   := List<String>{}
        Returns     := List<String>{}
        Description := List<String>{}
        Notes       := List<String>{}
        Syntax      := List<String>{}
        Examples    := List<String>{}
        SeeAlso     := List<String>{}
        CurrentLines := Title
        oDoc:Load(reader)
        reader:Close()
        SELF:WalkNode(oDoc)
        sFile := System.IO.Path.GetFileNameWithoutExtension(sFile):ToLower()
        sFile := sFile:Replace("function_","")
        output:AppendLine("<"+sFile+">")
        DumpSection(Title, "summary")
        DumpParameters(Arguments)
        DumpSection(Returns,"returns")
        local hasNotes := false as logic
        foreach var sLine in Notes
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
        output:AppendLine("</"+sFile+">")

        
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
            endif
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
                var atts := oText:Attributes
                foreach att as XmlAttribute in atts
                    if att:Name:ToLower() == "styleclass"
                        switch att:Value
                        case "Heading1"
                            switch oText:InnerText:ToLower()
                                case "syntax"
                                    CurrentLines := Syntax
                                case "arguments"
                                    CurrentLines := Arguments
                                case "returns"
                                    CurrentLines := Returns
                                case "description"
                                    CurrentLines := Description
                                case "notes"
                                    CurrentLines := Notes
                                case "examples"
                                    CurrentLines := Examples
                                case "prototype"
                                    CurrentLines := Syntax
                                case "seealso"
                                case "see also"
                                    CurrentLines := SeeAlso
                            end switch
                        case "Normal"
                            cLine += oText:InnerXML
                        case "List"
                            cLine += oText:InnerXML
                        case "Code Example"
                            lCode := TRUE
                            cLine += oText:InnerXML
                        otherwise
                            cLine += oText:InnerXML
                        end switch
                    endif
                next
            next
            IF ! String.IsNullOrEmpty(cLine)
                if lCode
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
        RETURN

    Method DumpSection(lines as List<String>, tag as string) as void
        local lastIsCode := FALSE as LOGIC
        local aTemp as List<String>
        local lHasCode := FALSE as LOGIC
        aTemp := List<string>{}
        if (lines == SeeAlso)
            foreach var c in Lines
                var aElements := c:Split(',',StringSplitOptions.RemoveEmptyEntries)
                foreach var s in aElements
                    var sElement := s:Trim():Replace("()","")
                    var type := oVO:GetType("XSharp.VO.Functions")
                    var funcs := type:GetMember(sElement,BindingFlags.IgnoreCase+BindingFlags.Static+BindingFlags.Public)
                    if funcs:Length == 0
                        type := oRT:GetType("XSharp.RT.Functions")
                        funcs := type:GetMember(sElement,BindingFlags.IgnoreCase+BindingFlags.Static+BindingFlags.Public)
                    endif
                    if funcs:Length == 0
                        type := oCore:GetType("XSharp.Core.Functions")
                        funcs := type:GetMember(sElement,BindingFlags.IgnoreCase+BindingFlags.Static+BindingFlags.Public)
                    endif
                    if funcs:Length > 1
                        local proto as string
                        // /// <seealso cref='M:XSharp.Core.Functions.L2Bin(System.Int32)' >L2Bin</seealso>
                        proto := "O:"+type:FullName+"."+funcs[1]:Name
                        var sLine := "<seealso cref='"+proto+"'>"+sElement+" Overload</seealso>"
                        output:AppendLine(sLine)
                    elseif funcs:Length == 1
                        local funcinfo := (System.Reflection.MethodInfo) funcs[1] as System.Reflection.MethodInfo
                        local proto as string
                        // /// <seealso cref='M:XSharp.Core.Functions.L2Bin(System.Int32)' >L2Bin</seealso>
                        proto := "M:"+type:FullName+"."+funcinfo:Name
                        var pars := funcinfo:GetParameters()
                        if pars:Length > 0
                            proto += "("
                            local first := true as logic
                            foreach p as ParameterInfo in pars
                                if ! first
                                    proto += ","
                                endif
                                var partype := p:ParameterType:FullName
                                if !String.IsNullOrEmpty(partype)
                                    partype := partype:Replace("&","@")
                                    proto += partype
                                endif
                                first := false
                            next
                            proto += ")"
                        endif
                        var sLine := "<seealso cref='"+proto+"'>"+sElement+"</seealso>"
                        output:AppendLine(sLine)
                    else
                        var sLine := "<seealso>"+sElement:Trim()+"</seealso>"
                        output:AppendLine(sLine)
                    endif
                next
            next
            RETURN
        endif
        IF ! String.IsNullOrEmpty(tag)
            output:AppendLine("<"+tag+">")
            foreach var c in lines
                var cLine := c:Trim()
                cLine := cLine:Replace("</code><code>","")
                if ! String.IsNullOrEmpty(cLine:Trim())
                    if lastIsCode .and. cLine:StartsWith("<code>")
                        cLine := cLine:Replace("<code>","")
                    endif
                    LastIsCode := cLine:Contains("<code>") .or. cLine:Contains("</code>")
                    lHasCode := lHasCode .or. LastIsCode
                endif
                aTemp:Add(cLine)
            next
            if lHasCode 
                local i as long
                FOR i := 0 to aTemp:Count-1
                    local sLine as string
                    sLine := aTemp[i]
                    if i < aTemp:Count-1 .and. sLine:Contains("</code>")
                        LOCAL sNext as STRING
                        sNext := aTemp[i+1]
                        if sNext:Contains("<code>") .or. sNext:Contains("</code>")
                           sLine := sLine:Replace("</code>", "")
                        endif    
                    endif
                    aTemp[i] := sLine
                NEXT
            ENDIF
            foreach var s in aTemp
                var sLine := s
                if sLine:Contains("<code>")
                    sLine := sLine:Replace("<code>",e"<code language=\"X#\">")
                endif
                output:AppendLine(sLine)
            next
            output:AppendLine("</"+tag+">")
        endif

    Method DumpParameters(lines as List<String>) as void
        local lastLine := "" as string
        FOREACH line as string in lines
            var index := line:IndexOf("&#0009;")
            if (index >= 0)
                var name   := line:Substring(0, index)
                name := name:replace("&lt;","")
                name := name:replace("&gt;",""):Trim()
                var desc    := line:Substring(index+7)
                if String.IsNullOrEmpty(name) .or. name:StartsWith("<code>")
                    if lastline:EndsWith("</param>")
                        lastline := lastline:Replace("</param>","")
                    endif
                    lastline += "<br/>"+Environment.NewLine+"&#0009;"+name+desc+"</param>"
                else
                    output:AppendLine(lastline)
                    lastline := e"<param name=\""+name+e"\">"+desc+"</param>"
                endif
            endif
        next
        if ! String.IsNullOrEmpty(lastline)
            output:AppendLine(lastline)
        endif

END CLASS

