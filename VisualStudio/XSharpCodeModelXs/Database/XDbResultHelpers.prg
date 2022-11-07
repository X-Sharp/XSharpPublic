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
                XSettings.LogException(e, __FUNCTION__)
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
                    VAR source     := XSourceTypeSymbol.GetTypeSource(element, members, file)
                    VAR walker := SourceWalker{file, FALSE}
                    walker:Parse(source)
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
                XSettings.LogException(e, __FUNCTION__)
            END TRY
            //
        RETURN result

        STATIC METHOD BuildFullFuncsInFile( origin AS XFile, found AS IList<XDbResult>) AS IList<XSourceMemberSymbol>
            VAR result := List<XSourceMemberSymbol>{}
            LOCAL idProject := -1 AS INT64
            //
            TRY
                // now create a temporary source for the parser
                IF found:Count() == 0
                    RETURN result
                ENDIF
                VAR source     := XSourceTypeSymbol.GetTypeSource( NULL, found, origin)
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
                XSettings.LogException(e, __FUNCTION__)
            END TRY
        RETURN result
    END CLASS
END NAMESPACE // global::XSharpCodeModel.Model
