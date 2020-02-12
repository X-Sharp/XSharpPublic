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
        local oRdr := FoxDocReader{} as FoxDocReader
        oRdr:Read()
	
END NAMESPACE 



CLASS FoxDocReader
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
    PROTECT inCode AS LOGIC
    PROTECT nFunction  AS LONG
    PROTECT inpre := FALSE AS LOGIC
    PROTECT aFiles AS List<STRING>

    CONSTRUCTOR()
        cPath := "c:\XSharp\FoxHelp\HelpFile-master\sources\dv_foxhelp\html"

    METHOD Read() AS VOID STRICT
        VAR files := System.IO.Directory.GetFiles(cPath, "*.htm")
        aFiles := List<STRING>{}
        nFunction := 0
        FOREACH VAR sFile IN files
            processfile(sFile)
        NEXT
        aFiles:Sort()
        output := StringBuilder{}
        output:AppendLine(e"<?xml version=\"1.0\" encoding=\"utf-8\"?>")
        output:AppendLine("<Comments>")
        FOREACH VAR sFile IN aFiles
            VAR cTitle := sFile:Substring(0,sFile:IndexOf("|"))
            sFile := sFile:Substring(sFile:IndexOf("|")+1)
            ? cTitle
            SELF:WalkFile(sFile,cTitle)
        NEXT
        output:AppendLine("</Comments>")
        var result := output:ToString()
        result := result:Replace("&lt;%APP%&gt;","X#")
        result := result:Replace("&lt;%app%&gt;","X#")
        System.IO.File.WriteAllText("c:\XSharp\DevRt\Runtime\XSharp.VFP\VFPDocs.xml",result)
        Console.WriteLine()
        console.Write("done")
        Console.ReadLine()
        

    METHOD processfile(sFile AS STRING) AS VOID
        VAR cText   := System.IO.File.ReadAllText(sFile)        
        LOCAL cKey AS STRING
        cKey := e"<span id=\"nsrTitle\">"
        VAR nPos := cText:IndexOf(cKey)
        IF nPos > 0
            VAR sub := cText:SubString(nPos+cKey:Length)
            nPos := sub:IndexOf("</span>") 
            IF nPos > 0 .AND. sub:ToLower():Contains("function")
                VAR cTitle := sub:Substring(0, nPos)
                IF cTitle:ToLower():EndsWith("function")
                    ++ nFunction
                    aFiles:Add( cTitle +"|"+sFile)
                ENDIF
            ENDIF
        ENDIF


       
//        sFile := System.IO.Path.GetFileNameWithoutExtension(sFile):ToLower()
//        sFile := sFile:Replace("function_","")
//        output:AppendLine("<"+sFile+">")
//        DumpSection(Title, "summary")
//        DumpParameters(Arguments)
//        DumpSection(Returns,"returns")
//        local hasNotes := false as logic
//        foreach var sLine in Notes
//            if sLine != "<br/>"
//                if (! hasNotes)
//                    Description:Add(e"<note type=\"tip\">")
//                    hasNotes := true
//                endif
//                Description:Add(sLine)
//            endif
//        next
//        if (hasNotes)
//            Description:Add("</note>")
//        endif
//        DumpSection(Description,"remarks")
//        //DumpSection("Syntax", Syntax,"")
//        DumpSection(Examples,"example")
//        DumpSection(Seealso,"")
//        output:AppendLine("</"+sFile+">")
//

METHOD WalkFile(sFile AS STRING, cTitle AS STRING) AS VOID
VAR lines := System.IO.File.ReadAllLines(sFile)
VAR inPre := FALSE
VAR inPar  := FALSE
        Title       := List<STRING>{}
        Arguments   := List<STRING>{}
        Returns     := List<STRING>{}
        Description := List<STRING>{}
        Notes       := List<STRING>{}
        Syntax      := List<STRING>{}
        Examples    := List<STRING>{}
        SeeAlso     := List<STRING>{}
        CurrentLines := Title

IF cTitle:Contains("(")
    cTitle := cTitle:Substring(0, cTitle:IndexOf("("))
ENDIF
FOREACH VAR sline IN lines
        VAR line := sline:Trim()
        IF line:Contains(e"<div id=\"mainSection\">")
            CurrentLines := Title
        ELSEIF line:Contains(e"<div id=\"syntaxSection\"")
            CurrentLines := Syntax
        ELSEIF line:Contains(e"<dl><dt><span class=\"nonLinkTerms\">")
            CurrentLines := SELF:Arguments
            CurrentLines:Add(line)
        ELSEIF line:Contains(e"<div id=\"remarksSection\"")
            CurrentLines := SELF:Description
        ELSEIF line:Contains(e"<div id=\"codeExampleSection\"")
            CurrentLines := SELF:Examples
        ELSEIF line:Contains(e"<div id=\"returnValueSection\"")
            CurrentLines := SELF:Returns
        ENDIF
        IF line:Contains("<pre>")
            inPRe := TRUE
        ENDIF
        IF line:StartsWith("<p>")
            IF !line:Contains("</p>")
                inPar := TRUE
            ENDIF
            line := line:Replace("<p>",""):Replace("</p>","")
            CurrentLines:add(line)
        ELSEIF line:Startswith(e"<div class=\"code\">")
            CurrentLines:add(line)
        ELSEIF inPre .OR. inPar
            CurrentLines:add(line)
        ELSEIF line:Contains("/dt")
            CurrentLines:add(line)
        ENDIF
        
        IF CurrentLines == Arguments
            IF line:StartsWith(e"<span class=\"parameter\">")
                Arguments:add(line)
            ENDIF
        ENDIF
        IF line:contains("</pre>")
            inPre := FALSE
        ENDIF
        IF line:contains("</p>")
            inPar := FALSE
        ENDIF
        IF line:Contains("Parameters</h4>")
            CurrentLines := SELF:Description
        ENDIF
    NEXT
    cTitle := cTitle:ToLower()
    output:AppendLine("<"+cTitle+">")
    WriteSection(SELF:Title, "summary")
    VAR parName := ""
    VAR parDesc := ""
    VAR inDt := FALSE
    FOREACH VAR sline IN SELF:Arguments
        VAR line := sline
        IF line:contains("<dt>")
            inDt := TRUE
        ELSEIF line:contains("</dt>")
            inDt := FALSE
        ENDIF
        IF line:startswith(e"<span class=\"parameter\">") .AND. inDt
            IF ! string.IsNullOrEmpty(parName) .OR. ! STRING.IsNullOrEmpty(parDesc)
                WritePar(parName, parDesc)
            ENDIF
            line := line:replace(e"<span class=\"parameter\">",""):Replace("</span>","")
            parName := line
            parDesc := ""
        ELSE
            IF ! parDesc:Contains(line) .AND. ! String.IsNullOrEmpty(line)
                parDesc += line + Environment.NewLine
            endif
        ENDIF

    NEXT
    WritePar(parName, parDesc)
    WriteSection(SELF:Returns, "returns")
    WriteSection(SELF:Description, "remarks")
    WriteSection(SELF:Examples, "examples")

    output:AppendLine("</"+cTitle+">")

RETURN

METHOD WritePar(parName AS STRING, parDesc AS STRING) AS VOID
parName := parName:Trim()
parDesc := parDesc:Trim()
IF ! string.IsNullOrEmpty(parName) 
    parName := RemoveTags(parName)
    parname := parName:Replace(e"\"","'")
    output:AppendLine(e"<param name=\""+parName+e"\">")
    output:AppendLine(RemoveTags(parDesc))
    output:AppendLine("</param>")
ENDIF

METHOD WriteSection(aLines AS List<STRING>, header AS STRING) AS VOID
IF aLines:Count > 0
    output:AppendLine("<"+header+">")
    FOREACH VAR sline IN aLines
        VAR line := sline
        IF line:Contains("<") .OR. line:Contains("&")
            line := RemoveTags(line)
        ENDIF
        output:AppendLine(line)
    NEXT
    IF SELF:InPre
    output:Append("</code>")
    ENDIF
    output:AppendLine("</"+header+">")
ENDIF



METHOD RemoveTags(line AS STRING) AS STRING
LOCAL sb AS stringbuilder
sb := stringBuilder{}
line := line:Trim()
VAR elements := line:Split("<>":ToCharArray(),StringSplitOptions.RemoveEmptyEntries)
FOR VAR i := 1 TO elements:Length
    VAR element := elements[i]
    SWITCH element
        CASE "h1"
        CASE "h2"
        CASE "h3"
        CASE "/h1"
        CASE "/h2"
        CASE "/h3"
        CASE "th"
        CASE "tr"
        CASE "td"
        CASE "dt"
        CASE "dd"
        CASE "dl"
        CASE "/dl"
        CASE "/th"
        CASE "/tr"
        CASE "/td"
        CASE "/div"
        CASE "/table"
        CASE "/dt"
        CASE "/dd"
        CASE "/a"
        CASE "/img"
        CASE "del"
        CASE "/del"
        CASE "/span"
        CASE "p"
        CASE "/p"
        CASE "Copy Code"
            NOP
        CASE "b"
        CASE "/b"
        CASE "br/"
        CASE "br /"
            sb:Append("<"+elements[i]+">")
        CASE "pre" 
            sb:Append(e"<code language=\"X#\">")
            inPre := TRUE
        CASE "/pre"
            IF inPre
                sb:Append("</code>")
                inPre := FALSE
            endif
        OTHERWISE
            IF element:Startswith("td")
                NOP
            ELSEIF element:Startswith("span")
                NOP
            ELSEIF element:Startswith("img")
                NOP
            ELSEIF element:Startswith("a")
                NOP
            ELSEIF element:Startswith("h1")
                NOP
            ELSEIF element:Startswith("h2")
                NOP
            ELSEIF element:Startswith("h3")
                NOP
            ELSEIF element:Startswith("div")
                NOP
            ELSEIF element:Startswith("table")
            ELSE
                IF element:Contains("&")
                    element := element:Replace("&gt;","{gt}")
                    element := element:Replace("&lt;","{lt}")
                    element := element:Replace("&amp;","{amp}")
                    element := element:Replace("&","&amp;")
                    element := element:Replace("{gt}","&gt;")
                    element := element:Replace("{lt}","&lt;")
                    element := element:Replace("{amp}","&amp;")
                ENDIF
                IF element:Contains("&") .AND. ! element:Contains("&amp;") .AND. ! element:Contains("&gt;") .AND. ! element:Contains("&lt;")  .AND. ! element:Contains("&nbsp;") 
                    NOP
                ENDIF
                sb:Append(element)
                
            ENDIF
    END SWITCH
NEXT
RETURN sb:ToString()



END CLASS

