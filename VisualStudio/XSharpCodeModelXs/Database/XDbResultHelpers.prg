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
        STATIC METHOD BuildTypesInFile( origin AS XFile, found AS IList<XDbResult>) AS IList<XSourceTypeSymbol>
            VAR result := List<XSourceTypeSymbol>{}
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
                    VAR xtype := XSourceTypeSymbol{element, file}
                    result:Add(xtype)
                NEXT
            CATCH e AS Exception
                XSolution.WriteOutputMessage("GetTypesInFile: "+ e:Message)
            END TRY
        RETURN result


        STATIC METHOD BuildFullTypesInFile( origin AS XFile, found AS IList<XDbResult>) AS IList<XSourceTypeSymbol>
            VAR result := List<XSourceTypeSymbol>{}
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
                    VAR fileName := element:FileName
                    IF fileName == NULL
                        fileName := origin:FullPath
                    ENDIF
                    VAR file       := XFile{ fileName, origin:Project}
                    file:Virtual   := TRUE
                    // If we don't set Interactive, the EntityList will be emptied after the Parse() operation
                    file:Interactive := TRUE
                    file:Id        := element:IdFile
					VAR members := XDatabase.GetMembers(element:IdType)
                    // now create a temporary source for the parser
                    VAR source     := GetTypeSource(element, members)
                    VAR walker := SourceWalker{file, FALSE}
                    IF walker:EntityList:Count > 0
                        VAR xElement      := walker:EntityList:First()
                        IF xElement IS XSourceTypeSymbol VAR xtype
                            xElement:CopyValuesFrom(element)
                            VAR xmembers := xtype:XMembers
                            IF xmembers:Count == members:Count
                                LOCAL i AS INT
                                FOR i := 0 TO members:Count-1
                                    VAR xmember := (XSourceMemberSymbol) xmembers[i]
                                    VAR melement := members[i]
                                    xmember:CopyValuesFrom(melement)
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

        STATIC METHOD BuildFullFuncsInFile( origin AS XFile, found AS IList<XDbResult>) AS IList<XSourceMemberSymbol>
            VAR result := List<XSourceMemberSymbol>{}
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
                VAR walker      := SourceWalker{file, FALSE}
                IF walker:EntityList:Count > 0 .AND. ( walker:EntityList:Count == found:Count )
                    LOCAL i AS INT
                    FOR i := 0 TO found:Count-1
                        VAR entity := walker:EntityList[i]
                        IF entity IS XSourceMemberSymbol VAR xMember
                            VAR melement := found[i]
                            xMember:CopyValuesFrom(melement)
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
