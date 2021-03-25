USING System.Reflection
USING System.Collections.Generic
USING System.Linq
USING System.Text
GLOBAL gaFiles AS STRING[]
GLOBAL gsPath AS STRING
GLOBAL gsCatPath AS STRING
FUNCTION Start AS VOID   
    gaFiles := <STRING>{"XSharp.Core.DLL", "XSharp.RT.DLL","XSharp.VO.DLL", ;
        "XSharp.Data.DLL","XSharp.VFP.DLL","XSharp.RDD.DLL","XSharp.XPP.DLL",   ;
        "VOSystemClasses.DLL", "VORDDClasses.DLL","VOGUIClasses.DLL", ;
        "VOSQLClasses.DLL","VOInternetClasses.DLL", "VOConsoleClasses.DLL",;
        "XSharp.RT.Debugger.DLL", "XSharp.VOSystemClasses.DLL", ;
        "XSharp.VOConsoleClasses.DLL","XSharp.VOSQLClasses.DLL","XSharp.VORDDClasses.DLL"}
    gsPath  := "c:\XSharp\DevRt\Binaries\Documentation\"    
    gsCatPath := "c:\XSharp\DevRt\Docs\Categories\"        
    TRY
    //CreateClassList() 
    //CreateClassSectionFiles()
    //WriteClassTopics()
    //CreateFunctionList()
    CreateFunctionSectionFiles()  
    WriteFunctionTopics()
    CATCH e AS Exception
        ? e:ToString()
        Console.ReadLine()
    END TRY  
    

FUNCTION WriteClassTopics() AS VOID
    VAR aInfo       := CreateClassList()
    VAR aCategories := ReadClassCategories()
    VAR catList     := List<STRING>{}                  
    FOREACH VAR item IN aInfo               
        VAR info := item:Value
        FOREACH VAR cat IN info:Categories     
            IF ! String.IsNullOrEmpty(cat:ToLower())
                IF ! catList:Contains(cat:ToLower())
                    catList:Add(cat:ToLower())
                ENDIF              
            ENDIF                
        NEXT
    NEXT
    VAR missing := FALSE         
    catList:Sort()
    FOREACH VAR cat IN catList
        IF ! aCategories:ContainsKey(cat)
            ? "Class Category "+cat+" is missing"
            missing := TRUE
        ENDIF
    NEXT
    IF missing
        RETURN
    ENDIF
    FOREACH VAR cat IN catList
        ? cat
        VAR sFile := gsCatPath+cat+"_Classes.aml"
        VAR contents := System.IO.File.ReadAllText(sFile)
        VAR left   := contents:Substring(0, contents:IndexOf("<content>")+9)
        VAR right  := contents:Substring(contents:IndexOf("</content>"))
        VAR sb     := StringBuilder{} 
        sb:Append(left)       
        sb:AppendLine("<table><tableHeader>")
        sb:AppendLine("<row><entry><para>Namespace</para></entry>")
        sb:AppendLine("<entry><para>Class</para></entry>")
        sb:AppendLine("<entry><para>Description</para></entry>")
        sb:AppendLine("</row></tableHeader>")
        FOREACH VAR item IN aInfo  
            VAR info := item:Value
            VAR match := FALSE
            FOREACH VAR keyword IN info:Categories
                IF String.Compare(keyword, cat, TRUE) == 0
                    match := TRUE
                    EXIT
                ENDIF
            NEXT                             
            IF match                     
                sb:AppendLine("<row><entry><para>"+info:NameSpace+"</para></entry>")
                sb:Append("<entry><para><codeEntityReference qualifyHint=""false"" autoUpgrade=""true"">")
                sb:Append("T:")                                               
                sb:Append(info:NameSpace+".")
                sb:Append(info:Name)
                sb:AppendLine("</codeEntityReference></para></entry>")
                sb:AppendLine("<entry><para>"+info:Description+"</para></entry></row>")
            ENDIF
        NEXT
        sb:AppendLine("</table>")
        sb:Append(right)
        System.IO.File.WriteAllText(sFile, sb:ToString())
    NEXT
    
    RETURN
    
FUNCTION WriteFunctionTopics() AS VOID
    VAR aInfo       := CreateFunctionList()
    VAR aCategories := ReadFunctionCategories()
    VAR catList     := List<STRING>{}                  
    FOREACH VAR item IN aInfo               
        VAR info := item:Value
        FOREACH VAR cat IN info:Categories     
            IF ! String.IsNullOrEmpty(cat:ToLower())
                IF ! catList:Contains(cat:ToLower())
                    catList:Add(cat:ToLower())
                ENDIF              
            ENDIF                
        NEXT
    NEXT
    VAR missing := FALSE         
    catList:Sort()
    FOREACH VAR cat IN catList
        IF ! aCategories:ContainsKey(cat)
            ? "Function Category "+cat+" is missing"
            missing := TRUE
        ENDIF
    NEXT
    IF missing
        RETURN
    ENDIF
    FOREACH VAR cat IN catList
        ? cat
        VAR sFile := gsCatPath+cat+"_Functions.aml"
        VAR contents := System.IO.File.ReadAllText(sFile)
        VAR left   := contents:Substring(0, contents:IndexOf("<content>")+9)
        VAR right  := contents:Substring(contents:IndexOf("</content>"))
        VAR sb     := StringBuilder{} 
        sb:Append(left)       
        sb:AppendLine("<table><tableHeader>")
        sb:AppendLine("<row><entry><para>Assembly</para></entry>")
        sb:AppendLine("<entry><para>Function</para></entry>")
        sb:AppendLine("<entry><para>Description</para></entry>")
        sb:AppendLine("</row></tableHeader>")
        FOREACH VAR item IN aInfo  
            VAR info := item:Value
            VAR match := FALSE
            FOREACH VAR keyword IN info:Categories
                IF String.Compare(keyword, cat, TRUE) == 0
                    match := TRUE
                    EXIT
                ENDIF
            NEXT                             
            IF match                     
                VAR assembly := info:assembly:replace(".DLL","")
                sb:AppendLine("<row><entry><para>"+assembly+"</para></entry>")
                sb:Append("<entry><para><codeEntityReference qualifyHint=""false"" autoUpgrade=""true"">")
                sb:Append("M:")                                               
                sb:Append(Assembly)
                sb:Append(".Functions.")
                sb:Append(info:Name)
                sb:Append(info:Signature)
                sb:AppendLine("</codeEntityReference></para></entry>")
                sb:AppendLine("<entry><para>"+info:Description+"</para></entry></row>")
            ENDIF
        NEXT
        sb:AppendLine("</table>")
        sb:Append(right)
        System.IO.File.WriteAllText(sFile, sb:ToString())
    NEXT
    
    RETURN
                  

FUNCTION CreateFunctionSectionFiles AS VOID
    VAR sTemplate := gsCatPath+"templateFunctions.aml"
    VAR aLines := ReadFunctionCategories()
    FOREACH VAR pair IN aLines              
        VAR sLine := pair:Value
        ? sLine
        VAR sFile := gsCatPath+sLine+"_Functions.aml"
        IF ! System.IO.File.Exists(sFile)        
            VAR file := System.IO.File.ReadAllText(sTemplate)
            file := file.Replace("id=""default""", "id=""cat-"+sLine+"_Functions""")
            file := file.Replace("title=""default functions""", "title="""+sLine+" Functions""")
            file := file.Replace("<title>default Functions</title>", "<title>"+sLine +" Functions</title>")
            System.IO.File.WriteAllText(sFile, file)
        ENDIF
    NEXT

FUNCTION CreateClassSectionFiles AS VOID
    VAR sTemplate := gsCatPath+"templateClasses.aml"
    VAR aLines := ReadClassCategories()
    FOREACH VAR pair IN aLines              
        VAR sLine := pair:Value
        ? sLine
        VAR sFile := gsCatPath+sLine+"_Classes.aml"
        IF ! System.IO.File.Exists(sFile)        
            VAR file := System.IO.File.ReadAllText(sTemplate)
            file := file.Replace("id=""default""", "id=""cat-"+sLine+"_Classes""")
            file := file.Replace("title=""default Classes""", "title="""+sLine+" Classes""")
            file := file.Replace("<title>default Classes</title>", "<title>"+sLine +" Classes</title>")
            System.IO.File.WriteAllText(sFile, file)
        ENDIF
    NEXT

FUNCTION CreateClassList AS SortedDictionary<STRING, MyClassInfo>
    VAR aInfo := ReadClassList()     
    VAR aExcluded := ReadExcludedClasses()
    FOREACH VAR sFile IN gaFiles                    
        TRY
        VAR asm   := Assembly.LoadFrom(gsPath+sFile)
        VAR types := asm:GetTypes()
        FOREACH oType AS System.Type IN types
            IF !oType:FullName:EndsWith("Functions") .and. ;
                 oType:Name:IndexOf("$") == -1 .and.oType:Name:IndexOf("<") == -1 .and. ;
                 ! oType:Name:StartsWith("_")  .and. oType:IsPublic
                VAR classInfo := MyClassInfo{} { assembly := sFile, name := oType:Name, namespace := oType:Namespace}
                IF aExcluded:ContainsKey(classInfo:key) 
                    IF aInfo:ContainsKey(classInfo:key)
                        ? "Delete", classInfo:key
                        aInfo:Remove(classInfo:key)
                    ENDIF
                    LOOP                        
                ENDIF
                classInfo:Type := oType
                IF !aInfo:ContainsKey(classInfo:Key)
                    ? oType:FullName
                    aInfo:Add(classInfo:Key, classInfo)
                ENDIF
                
            ENDIF
        NEXT   
        CATCH e AS Exception
            ? sFile
            ? e:ToString()
        END TRY
    NEXT    
    VAR output := System.IO.File.CreateText("RuntimeClasses.CSV") 
    FOREACH VAR item IN aInfo
        VAR groups := item:Value:Categories
        VAR sGroups := ""
        FOREACH VAR sGroup IN groups
            sGroups += ","+sGroup
        NEXT
        output:Write(item:Value:assembly+",")
        output:Write(item:Value:namespace+",")
        output:Write(item:Value:Name)
        output:WriteLine(sGroups )
    NEXT
    output:Close()   
    RETURN aInfo
FUNCTION CreateFunctionList AS  SortedDictionary<STRING, FunctionInfo>
    VAR aInfo := ReadFunctionList() 
    VAR aExcluded := ReadExcludedFunctions()
    FOREACH VAR sFile IN gaFiles                    
        TRY
        VAR asm   := Assembly.LoadFrom(gsPath+sFile)
        VAR types := asm:GetTypes()
        FOREACH type AS System.Type IN types
            IF type:FullName:EndsWith(".Functions")
                VAR functions := type:GetMethods(BindingFlags.Public+BindingFlags.Static)
                FOREACH m AS MethodInfo IN functions      
                    VAR name := m:Name
                    IF (name:StartsWith("$") || name:StartsWith("__")   )
                        LOOP
                    ENDIF
                    VAR info := FunctionInfo{}{ Assembly := sFile, Name := m.Name}
                    IF aExcluded:ContainsKey(info:key) 
                        IF aInfo:ContainsKey(info:key)
                            ? "Delete", info:key
                            aInfo:Remove(info:key)
                        ENDIF
                        LOOP                        
                    ENDIF
                    VAR overloads := type:GetMember(m.Name)
                    IF aInfo:ContainsKey(info:Key)
                        info := aInfo[info:Key]
                    ENDIF
                    info:Overloads := overloads
                    
                    IF m:GetCustomAttributes(typeof(ObsoleteAttribute),FALSE):Count() > 0 
                        VAR delete := overloads:Length == 1
                        IF ! delete
                            delete := TRUE
                            FOREACH VAR o IN overloads
                                IF o:GetCustomAttributes(typeof(ObsoleteAttribute),FALSE):Count() == 0                                 
                                    delete := FALSE
                                ENDIF
                            NEXT                  
                        ENDIF     
                        IF delete .and. aInfo:ContainsKey(info:key)
                            aInfo:Remove(info:key)
                        ENDIF
                        LOOP                        
                    ENDIF
                    TRY      
                        IF !aInfo:ContainsKey(info:Key)
                            ? "Add", m.Name                                        
                            aInfo:Add(info:key, info )
                        ENDIF
                    CATCH e AS Exception
                        ? e:ToString()
                        
                    END TRY
                NEXT
            ENDIF
        NEXT
        CATCH e AS ReflectionTypeLoadException
            ? "Failed to load ", sFile
            ? e:ToString()           
            ? 
            Console.ReadLine()
        END TRY
    NEXT                     
    VAR output := System.IO.File.CreateText("RuntimeFunctions.CSV")
    FOREACH VAR item IN aInfo
        VAR groups := item:Value:Categories
        VAR sGroups := ""
        FOREACH VAR sGroup IN groups
            sGroups += ","+sGroup
        NEXT
        output:Write(item:Value:assembly+",")
        output:Write(item:Value:Name)
        output:WriteLine(sGroups )
    NEXT
    output:Close()   
    RETURN aInfo
    
    
FUNCTION ReadFunctionList() AS SortedDictionary<STRING, FunctionInfo>     
    VAR aInfo := SortedDictionary<STRING, FunctionInfo> {}
    IF System.IO.File.Exists("RuntimeFunctions.CSV")
        VAR aLines := System.IO.File.ReadAllLines("RuntimeFunctions.CSV")
        FOREACH VAR line IN aLines
            VAR aElements := line:Split(<char>{','})
            IF aElements:Length >= 2            
                VAR info := FunctionInfo{} {assembly := aElements[1], Name := aElements[2]}
                VAR groups := STRING[]{aElements:Length -2}
                FOR VAR i := 1 TO aElements:Length-2
                    groups[i] := aElements[i+2]
                NEXT                           
                info:Categories := groups
                aInfo:Add(info:Key, info)
            ENDIF
        NEXT
    ENDIF
    RETURN aInfo                     

FUNCTION ReadClassList() AS SortedDictionary<STRING, MyClassInfo>     
    VAR aInfo := SortedDictionary<STRING, MyClassInfo> {}
    IF System.IO.File.Exists("RuntimeClasses.CSV")
        VAR aLines := System.IO.File.ReadAllLines("RuntimeClasses.CSV")
        FOREACH VAR line IN aLines
            VAR aElements := line:Split(<char>{','})
            IF aElements:Length >= 3           
                VAR info := MyClassInfo{} {assembly := aElements[1], Namespace := aElements[2], Name := aElements[3]}
                VAR groups := STRING[]{aElements:Length -3}
                FOR VAR i := 1 TO aElements:Length-3
                    groups[i] := aElements[i+3]
                NEXT                           
                info:Categories := groups
                IF !aInfo:ContainsKey(info:Key)
                    aInfo:Add(info:Key, info)
                ELSE 
                    NOP
                ENDIF
            ENDIF
        NEXT
    ENDIF
    RETURN aInfo 

FUNCTION ReadExcludedFunctions() AS SortedDictionary<STRING, FunctionInfo>     
    VAR aInfo := SortedDictionary<STRING, FunctionInfo> {}
    IF System.IO.File.Exists("ExcludedFunctions.CSV")
        VAR aLines := System.IO.File.ReadAllLines("ExcludedFunctions.CSV")
        FOREACH VAR line IN aLines
            VAR aElements := line:Split(<char>{','})
            IF aElements:Length >= 2            
                VAR info := FunctionInfo{} {assembly := aElements[1], Name := aElements[2]}
                aInfo:Add(info:Key, info)
            ENDIF
        NEXT
    ENDIF
    RETURN aInfo                     

FUNCTION ReadExcludedClasses() AS SortedDictionary<STRING, MyClassInfo>     
    VAR aInfo := SortedDictionary<STRING, MyClassInfo> {}
    IF System.IO.File.Exists("ExcludedClasses.CSV")
        VAR aLines := System.IO.File.ReadAllLines("ExcludedClasses.CSV")
        FOREACH VAR line IN aLines
            VAR aElements := line:Split(<char>{','})
           IF aElements:Length >= 3           
                VAR info := MyClassInfo{} {assembly := aElements[1], Namespace := aElements[2], Name := aElements[3]}
                aInfo:Add(info:Key, info)
            ENDIF
        NEXT
    ENDIF
    RETURN aInfo                     
    
    
FUNCTION ReadFunctionCategories() AS SortedDictionary<STRING, STRING>
    VAR list := SortedDictionary<STRING, STRING>{StringComparer.OrdinalIgnoreCase}
    IF System.IO.File.Exists("FunctionCategories.CSV")    
        VAR aLines := System.IO.File.ReadAllLines("FunctionCategories.CSV")        
        FOREACH VAR sLine IN aLines              
            IF ! String.IsNullOrEmpty(sLine)
                list:Add(sLine:Trim(),sline:Trim())
            ENDIF
        NEXT
    ENDIF
    RETURN list


FUNCTION ReadClassCategories() AS SortedDictionary<STRING, STRING>
    VAR list := SortedDictionary<STRING, STRING>{StringComparer.OrdinalIgnoreCase}
    IF System.IO.File.Exists("ClassesCategories.CSV")    
        VAR aLines := System.IO.File.ReadAllLines("ClassesCategories.CSV")        
        FOREACH VAR sLine IN aLines              
            IF ! String.IsNullOrEmpty(sLine)
                list:Add(sLine:Trim(), sLine:Trim())
            ENDIF
        NEXT
    ENDIF
    RETURN list



CLASS MyClassInfo
    PROPERTY Assembly   AS STRING AUTO
    PROPERTY Name       AS STRING AUTO  := ""
    PROPERTY NameSpace  AS STRING AUTO  := ""
    PROPERTY Description AS STRING AUTO   := ""
    PROPERTY Categories AS STRING[] AUTO  := STRING[]{0}
    PROPERTY Key AS STRING 
        GET     
            IF String.IsNullOrEmpty(NameSpace)
                RETURN Name:ToLower()
            ELSE
                RETURN NameSpace:ToLower():PadRight(100) +":"+Name:ToLower()
            ENDIF
        END GET
    END PROPERTY
    PROPERTY Type AS System.Type AUTO
END CLASS    



 
CLASS FunctionInfo
    PROPERTY Assembly AS STRING AUTO
    PROPERTY Name AS STRING AUTO
    PROPERTY Description AS STRING AUTO   := ""
    PROPERTY Categories AS STRING[] AUTO  := STRING[]{0}
    PROPERTY Key AS STRING GET Assembly:ToLower():PadRight(25) +":"+Name:ToLower()
    PROPERTY Overloads AS MemberInfo[] AUTO
    PROPERTY Signature AS STRING 
        GET                                     
            IF OverLoads == NULL .or. overloads:Length == 0
                RETURN ""
            ENDIF          
            LOCAL overload := NULL AS MethodInfo                                                                    
            FOREACH m AS MemberInfo IN  SELF:Overloads
                IF m:MemberType  == MemberTypes.Method
                    VAR o := (MethodInfo) m
                    IF !o:IsGenericMethod
                        overload := o
                        EXIT
                    ENDIF
                ENDIF
            NEXT     
            IF overload == NULL
                overload := (methodInfo) SELF:Overloads[1]
            ENDIF
            VAR PARAMS := overload:GetParameters()
            IF PARAMS:Length == 0
                RETURN ""
            ENDIF
            VAR sb := StringBuilder{}                        
            VAR genArgs := overload:GetGenericArguments()
            IF overload:IsGenericMethod
                sb:Append("``"+genArgs:Length:ToString())
            ENDIF
            sb:Append("(")
            FOREACH par AS ParameterInfo IN PARAMS
                IF sb:Length > 1
                    sb:Append(",")
                ENDIF                               
                sb:Append(par:ParameterType:FullName)
            NEXT
            sb:Append(")")                           
            sb:Replace("&","@")
            RETURN sb:ToString()            
        END GET
    END PROPERTY
END CLASS
