// XDbResultHelpers.prg
// Created by    : fabri
// Creation Date : 7/3/2020 8:45:05 PM
// Created for   : 
// WorkStation   : FABPORTABLE


USING System
USING System.Collections.Generic
USING System.Text
USING LanguageService.CodeAnalysis.XSharp
USING System.Linq

BEGIN NAMESPACE XSharpModel

    /// <summary>
    /// The XDbResultHelpers class.
    /// </summary>
    CLASS XDbResultHelpers
        
        ///
        STATIC METHOD BuildTypesInFile( origin AS XFile, found AS IList<XDbResult>) AS IList<XTypeDefinition>
            VAR result := List<XTypeDefinition>{}
            LOCAL idProject := -1 AS INT64
            //
            TRY
                FOREACH VAR element IN found
                    // Skip types found in another project
                    IF idProject != -1 .AND. element:IdProject != idProject
                        LOOP
                    ENDIF
                    idProject   := element:IdProject  
                    VAR name    := element:TypeName
                    VAR idType  := element:IdType
                    VAR fileName := element:FileName
                    IF fileName == NULL
                        fileName := origin:FullPath
                    ENDIF
                    VAR file       := XFile{ fileName, origin:Project}
                    file:Virtual   := TRUE
                    file:Id        := element:IdFile
                    VAR range    := TextRange{element:StartLine, element:StartColumn, element:EndLine, element:EndColumn}
                    VAR interval := TextInterval{element:Start, element:Stop}
                    VAR xtype := XTypeDefinition{name, element:Kind,element:Attributes, range, interval, file}
                    xtype:Namespace := element:Namespace
                    xtype:Id  := element:IdType
                    result:Add(xtype)
                NEXT
            CATCH e AS Exception
                XSolution.WriteOutputMessage("GetTypesInFile: "+ e:Message)
            END TRY
        RETURN result		
        
        
        STATIC METHOD BuildFullTypesInFile( origin AS XFile, found AS IList<XDbResult>) AS IList<XTypeDefinition>
            VAR result := List<XTypeDefinition>{}
            LOCAL idProject := -1 AS INT64
            //
            TRY
                FOREACH VAR element IN found
                    // Skip types found in another project
                    IF idProject != -1 .AND. element:IdProject != idProject
                        LOOP
                    ENDIF
                    idProject   := element:IdProject  
                    //
                    VAR name    := element:TypeName
                    VAR idType  := element:IdType
                    VAR fileName := element:FileName
                    IF fileName == NULL
                        fileName := origin:FullPath
                    ENDIF
                    VAR file       := XFile{ fileName, origin:Project}
                    file:Virtual   := TRUE
                    // If we don't set Interactive, the EntityList will be emptied after the Parse() operation
                    file:Interactive := TRUE
                    file:Id        := element:IdFile
					VAR members := XDatabase.GetMembers(idType)
                    // now create a temporary source for the parser
                    VAR source     := GetTypeSource(element, members)
                    VAR walker := SourceWalker{file}
                    walker:Parse(source, FALSE)
                    IF walker:EntityList:Count > 0
                        VAR xElement      := walker:EntityList:First()
                        IF xElement IS XTypeDefinition VAR xtype
                            xtype:Range       := TextRange{element:StartLine, element:StartColumn, element:EndLine, element:EndColumn}
                            xtype:Interval    := TextInterval{element:Start, element:Stop}
                            xtype:XmlComments := element:XmlComments
                            xtype:ClassType   := (XSharpDialect) element:ClassType
                            xtype:Namespace := element:Namespace
                            xtype:Id  := element:IdType
                            VAR xmembers := xtype:XMembers
                            IF xmembers:Count == members:Count
                                LOCAL i AS INT
                                FOR i := 0 TO members:Count-1
                                    VAR xmember := (XMemberDefinition) xmembers[i] 
                                    VAR melement := members[i]
                                    xmember:Range       := TextRange{melement:StartLine, melement:StartColumn, melement:EndLine, melement:EndColumn}
                                    xmember:Interval    := TextInterval{melement:Start, melement:Stop}
                                    IF xmember:Name == melement:MemberName
                                        xmember:XmlComments := melement:XmlComments
                                    ENDIF
                                NEXT
                            ENDIF
                            result:Add(xtype)
                        ENDIF
                    ENDIF
                NEXT
            CATCH e AS Exception
                XSolution.WriteOutputMessage("GetTypesInFile: "+ e:Message)
            END TRY
            //
        RETURN result
        
        STATIC METHOD BuildFullFuncsInFile( origin AS XFile, found AS IList<XDbResult>) AS IList<XMemberDefinition>
            VAR result := List<XMemberDefinition>{}
            LOCAL idProject := -1 AS INT64
            //
            TRY
                // now create a temporary source for the parser
                VAR source     := GetTypeSource( NULL, found)
                IF found:Count() == 0
                    RETURN result
                ENDIF
                VAR element := found:First()
                VAR fileName := element:FileName
                IF fileName == NULL
                    fileName := "dummy.prg"
                ENDIF
                VAR file       := XFile{ fileName, origin:Project}
                file:Virtual   := TRUE
                // If we don't set Interactive, the EntityList will be emptied after the Parse() operation
                file:Interactive := TRUE
                file:Id         := element:IdFile
                VAR walker      := SourceWalker{file}
                walker:Parse(source, FALSE)
                IF walker:EntityList:Count > 0 .AND. ( walker:EntityList:Count == found:Count )
                    LOCAL i AS INT
                    FOR i := 0 TO found:Count-1
                        VAR entity := walker:EntityList[i]
                        IF entity IS XMemberDefinition VAR xMember
                            VAR melement := found[i]
                            xMember:Range       := TextRange{melement:StartLine, melement:StartColumn, melement:EndLine, melement:EndColumn}
                            xMember:Interval    := TextInterval{melement:Start, melement:Stop}
                            result:Add( xMember )
                        ENDIF
                    NEXT
                ENDIF
            CATCH e AS Exception
                XSolution.WriteOutputMessage("BuildFullFuncsInFile: "+ e:Message)
            END TRY
        RETURN result				
        // Comes from XProject, would be worth to merge source ??? 			
        PRIVATE STATIC METHOD GetTypeSource(element AS XDbResult, members AS IList<XDbResult>) AS STRING
            VAR sb := StringBuilder{}
            IF element != NULL
                sb:AppendLine(element:SourceCode)
            ENDIF
            FOREACH VAR xmember IN members
                sb:AppendLine(xmember:SourceCode)
                SWITCH xmember:Kind
                    CASE Kind.Property
                        VAR source := xmember:SourceCode:ToLower():Replace('\t',' ')
                        IF source:Contains(" get") .OR. ;
                                source:Contains(" set") .OR. ;
                                source:Contains(" auto") 
                                // single line
                            NOP
                        ELSE
                            sb:AppendLine("END PROPERTY")
                        ENDIF
                    CASE Kind.Event
                        VAR source := xmember:SourceCode:ToLower():Replace('\t',' ')
                        IF source:Contains(" add") .OR. ;
                                source:Contains(" remove") 
                                // single line
                            NOP
                        ELSE
                            sb:AppendLine("END EVENT")
                        ENDIF
                END SWITCH
            NEXT
            IF element != NULL
                SWITCH element:Kind
                    CASE Kind.Class
                        IF element:ClassType == (INT) XSharpDialect.XPP
                            sb:AppendLine("ENDCLASS")
                        ELSEIF element:ClassType == (INT) XSharpDialect.FoxPro
                            sb:AppendLine("ENDDEFINE")
                        ELSE
                            sb:AppendLine("END CLASS")
                        ENDIF
                    CASE Kind.Structure
                        sb:AppendLine("END STRUCTURE")
                    CASE Kind.Interface
                        sb:AppendLine("END INTERFACE")
                END SWITCH         
            ENDIF
            RETURN sb:ToString()			
        END CLASS
END NAMESPACE // global::XSharpCodeModel.Model
