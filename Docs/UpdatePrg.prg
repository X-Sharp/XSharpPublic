USING System.IO   
USING System.Collections.Generic   
FUNCTION Start AS VOID   
    LOCAL cDir AS STRING
    LOCAL aFiles AS STRING[]
    cDir := "c:\XSharp\DevRt\Runtime\VOSdkTyped\Source\VOSdk\SQL_Classes_SDK"
    aFiles := Directory.GetFiles(cDir, "*.prg")
    FOREACH VAR cFile IN aFiles    
        ? cFile
        UpdateFile(cFile, "Sql.xml")
    NEXT

    
FUNCTION UpdateFile(cFile AS STRING, cXML AS STRING) AS VOID    
    
    
    VAR aLines := File.ReadAllLines(cFile)    
    VAR result := List<STRING>{}
    VAR lastWasComment := FALSE    
    VAR className := "" 
    FOREACH VAR cLine IN aLines
        VAR line := cLine:TrimStart():ToLower()
        VAR name := ""                       
        VAR isClass := FALSE
        IF line:StartsWith("///")
            result:Add(cLine)
            lastWasComment := TRUE
            LOOP
        ELSEIF line:Trim():Length == 0
            result:Add(cLine)
        ENDIF
        IF line:StartsWith("class ")                        
            name := GetEntityName(cLine, 2)        
            isClass := TRUE
        ELSEIF line:StartsWith("partial ")                        
            name := GetEntityName(cLine, 3)        
            isClass := TRUE
        ELSEIF line:StartsWith("method ")          
            name := GetEntityName(cLine, 2)        
        ELSEIF line:StartsWith("access ")          
            name := GetEntityName(cLine, 2)        
        ELSEIF line:StartsWith("assign ")          
            name := GetEntityName(cLine, 2)        
        ELSEIF line:StartsWith("property ")          
            name := GetEntityName(cLine, 2)        
        ELSEIF line:StartsWith("function ")          
            name := GetEntityName(cLine, 2)        
        ELSEIF line:StartsWith("procedure ")          
            name := GetEntityName(cLine, 2)        
        ELSEIF line:StartsWith("constructor")          
            name := "ctor"
        ELSEIF line:StartsWith("destructor")          
            name := "dtor"
        ELSEIF line:StartsWith("end class")
            className := ""
        ENDIF
        IF ! String.IsNullOrEmpty(name) .and. ! lastWasComment  
            IF name:StartsWith("__")   .or. className:StartsWith("__")
                result:Add(" /// <exclude />")
            ELSE
            // /// <include file="Rdd.xml" path="doc/DbFileSpec.Recno/*" />
            VAR cmtLine := "/// <include file=""" + cXml+""" path=""doc/"
            IF !String.IsNullOrEmpty(className)
                cmtLine += className:Trim()+"."
            ENDIF                              
            cmtLine += name + "/*"" />"            
            result:Add(cmtLine)
            ENDIF
        ENDIF                  
        IF isClass
            className := name
        ENDIF
        result:Add(cLine)      
        
        lastWasComment := FALSE

    NEXT
    File.WriteAllLines(Path.ChangeExtension(cFile, ".new"), result)
    RETURN  
    
FUNCTION GetEntityName(cLine AS STRING, pos AS LONG) AS STRING
    VAR words := cLine:Split( <char> {' ','\t','('}, StringSplitOptions.RemoveEmptyEntries)
    IF words:Length >= pos
        RETURN words[pos]
    ENDIF
    RETURN ""
    
