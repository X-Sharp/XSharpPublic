USING System.IO   
USING System.Collections.Generic   
FUNCTION Start AS VOID   
    VAR result := List<STRING>{}
    VAR aLines := File.ReadAllLines("Report.XML")    
    LOCAL cMember := "" AS STRING
    FOREACH VAR cLine IN aLines
        VAR line := cLine:Trim()
        IF line:StartsWith("<member name=")
            cMember := line:SubString(16)
            IF cMember:EndsWith(""">")
                cMember := cMember:SubString(0, cMember:Length-2)
            ENDIF                                                
            IF cMember:StartsWith("VO.")
                cMember := cMember:Substring(3)
            ENDIF
            VAR pos := cMember:IndexOf("(")
            IF pos > 0
                cMember := cMember:Substring(0, pos)
            ENDIF    
            cMember := cMember:Replace("#ctor","ctor")                               
            result:Add("<"+cMember+">")
            
        ELSEIF line:StartsWith("</member")
            result:Add("</"+cMember+">")
        ELSE
            result:Add(cLine)            
        ENDIF
    NEXT
    System.IO.File.WriteAllLines("ReportNew.xml", result)    
