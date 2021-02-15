USING System.Reflection
USING System.Collections.Generic
USING System.Linq
FUNCTION Start AS VOID
    LOCAL aFiles AS STRING[]               
    LOCAL sPath  AS STRING
    LOCAL aLines AS STRING[]                                         
    LOCAL aInfo  AS Dictionary<STRING, STRING[]>
    aFiles := <STRING>{"XSharp.Core.DLL", "XSharp.RT.DLL","XSharp.VO.DLL","XSharp.Data.DLL","XSharp.VFP.DLL","XSharp.RDD.DLL"}
    sPath := "c:\XSharp\DevRt\Binaries\Documentation\"    
    aInfo := Dictionary<STRING, STRING[]> {}
    IF System.IO.File.Exists("RuntimeFunctions.CSV")
        aLines := System.IO.File.ReadAllLines("RuntimeFunctions.CSV")
        FOREACH VAR line IN aLines
            VAR aElements := line:Split(<char>{','})
            IF aElements:Length >= 2
                VAR cKey := aElements[1]+":"+aElements[2]
                VAR groups := STRING[]{aElements:Length -2}
                FOR VAR i := 1 TO aElements:Length-2
                    groups[i] := aElements[i+2]
                NEXT                           
                aInfo:Add(cKey, groups)
            ENDIF
        NEXT
    ENDIF
    FOREACH VAR sFile IN aFiles                    
        TRY
        VAR asm   := Assembly.LoadFrom(sPath+sFile)
        VAR types := asm:GetTypes()
        FOREACH type AS System.Type IN types
            IF type:FullName:EndsWith(".Functions")
                VAR functions := type:GetMethods(BindingFlags.Public+BindingFlags.Static)
                FOREACH m AS MethodInfo IN functions      
                    VAR name := m:Name
                    IF (name:StartsWith("$") || name:StartsWith("_")   )
                        LOOP
                    ENDIF
                    IF m:GetCustomAttributes(typeof(ObsoleteAttribute),FALSE):Count() > 0
                        VAR key := sFile+":"+m.Name
                        IF aInfo:ContainsKey(key)
                            aInfo:Remove(key)
                        ENDIF
                        LOOP                        
                    ENDIF
                    TRY      
                        VAR key := sFile+":"+m.Name
                        IF !aInfo:ContainsKey(m.Name)
                            ? m.Name
                            aInfo:Add(key, STRING[]{0})
                        ENDIF
                    CATCH  AS Exception
                        
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
        VAR sKey := item:Key:Replace(":",",")
        VAR groups := item:Value
        VAR sGroups := ""
        FOREACH VAR sGroup IN groups
            sGroups += ","+sGroup
        NEXT
        output:WriteLine(sKey+sGroups )
    NEXT
    output:Close()
    
