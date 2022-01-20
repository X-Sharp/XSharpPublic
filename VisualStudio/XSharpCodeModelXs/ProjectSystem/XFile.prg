//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Collections.Concurrent
USING System.Collections.Generic
USING System.Collections.ObjectModel
USING System
USING System.Linq
USING System.Diagnostics
USING XSharpModel
USING LanguageService.CodeAnalysis.XSharp
USING STATIC XSharpModel.XFileTypeHelpers
BEGIN NAMESPACE XSharpModel
    DELEGATE FindMemberComparer (oElement AS XSourceEntity, nValue AS LONG ) AS LONG

    [DebuggerDisplay("{FullPath,nq}")];
    CLASS XFile
        #region Fields
        PROPERTY Id             AS INT64          AUTO      GET INTERNAL SET
        PRIVATE _globalType 	AS XSourceTypeSymbol
        PRIVATE _type			AS XFileType
        PRIVATE _typeList		AS XDictionary<STRING, XSourceTypeSymbol>
        PRIVATE _entityList	    AS SynchronizedCollection<XSourceEntity>
        PRIVATE _usings			AS SynchronizedCollection<STRING>
        PRIVATE _usingStatics	AS SynchronizedCollection<STRING>
        PRIVATE _project        AS XProject
        #endregion
        // Methods
        CONSTRUCTOR(fullPath AS STRING, project AS XProject)
            SUPER()
            //
            SELF:FullPath := fullPath
            SELF:LastChanged := System.DateTime.MinValue
            SELF:_type := GetFileType(fullPath)
            SELF:_project := project
            SELF:_usings		:= SynchronizedCollection<STRING>{}
            SELF:AddDefaultUsings()
            SELF:_usingStatics	:= SynchronizedCollection<STRING>{}
            SELF:_entityList    := SynchronizedCollection<XSourceEntity>{}
            SELF:_typeList		:= XDictionary<STRING, XSourceTypeSymbol>{System.StringComparer.InvariantCultureIgnoreCase}

        PROPERTY CommentTasks AS IList<XCommentTask> AUTO
        PROPERTY EntityList   AS IList<XSourceEntity>
            GET
                IF _entityList != null
                    RETURN _entityList
                ENDIF
                RETURN List<XSourceEntity>{}
            END GET
        END PROPERTY
        PROPERTY Dialect AS XSharpDialect GET _project:Dialect
        PROPERTY Virtual AS LOGIC AUTO

        METHOD AddDefaultUsings() AS VOID
            IF SELF:_usings:IndexOf("XSharp") == -1
                SELF:_usings:Insert(0, "XSharp")
            ENDIF
            IF SELF:_usings:IndexOf("System") == -1
                SELF:_usings:Insert(0, "System")
            ENDIF
            RETURN


        METHOD FirstMember() AS IXMemberSymbol
            IF (! SELF:HasCode)
                RETURN NULL
            ENDIF
            IF SELF:_typeList?:Count == 0
                RETURN NULL
            ENDIF
            VAR element := SELF:_typeList:FirstOrDefault()
            RETURN element:Value:Members:FirstOrDefault()
            ///
            /// <Summary>Find member in file based on 0 based line number</Summary>
            ///
            ///

        METHOD FindMember(oDel AS FindMemberComparer, nValue AS LONG) AS XSourceEntity
            LOCAL oResult := NULL_OBJECT AS XSourceEntity
            LOCAL oLast AS XSourceEntity
            // perform binary search to speed up things
            VAR current := 0
            VAR bottom := 0
            IF (_entityList == NULL_OBJECT)
                RETURN NULL_OBJECT
            ENDIF
            LOCAL entities  AS IList<XSourceEntity>
            entities := SELF:_entityList:ToArray()
            VAR top := entities:Count
            oLast := entities:FirstOrDefault()
            DO WHILE top - bottom > 1
                // determine middle
                current := (bottom + top) / 2
                VAR oElement := entities[current]
                VAR result := oDel(oElement, nValue)
                IF result == 0 .and. oElement:Kind != Kind.Namespace
                    // found
                    RETURN oElement
                ELSEIF result = 1 // element is after the search point
                    top := current
                ELSE		// element is before the search point
                    oLast := oElement
                    bottom := current
                ENDIF
            ENDDO
            IF oResult == NULL
                oResult := oLast	// the last entity we saw before the selected line
            ENDIF
            RETURN oResult

        PRIVATE METHOD CompareByLine(oElement AS XSourceEntity, nLine AS LONG) AS LONG
            LOCAL nResult AS LONG
            LOCAL nStart, nEnd AS LONG
            nStart := oElement:Range:StartLine
            nEnd   := oElement:Range:EndLine
            IF oElement IS XSourceTypeSymbol
                VAR oType := oElement ASTYPE XSourceTypeSymbol
                IF oType:Members:Count > 0 .AND. oType:Members[0] IS XSourceMemberSymbol VAR xmember
                    nEnd := xmember:Range:StartLine-1
                ENDIF
            ENDIF
            IF nStart <= nLine .AND. nEnd>= nLine
                nResult := 0
            ELSEIF nStart > nLine
                nResult := 1
            ELSE
                nResult := -1
            ENDIF
            RETURN nResult

        METHOD FindMemberAtRow(nLine AS LONG) AS XSourceEntity
            RETURN SELF:FindMember(CompareByLine, nLine)

            ///
            /// <Summary>Find member in file based on 0 based position</Summary>
            ///
            ///
        PRIVATE METHOD CompareByPosition(oElement AS XSourceEntity, nPos AS LONG) AS LONG
            LOCAL nResult AS LONG
            LOCAL nStart, nEnd AS LONG
            nStart := oElement:Interval:Start
            nEnd   := oElement:Interval:Stop
            IF oElement IS XSourceTypeSymbol
                VAR oType := oElement ASTYPE XSourceTypeSymbol
                IF oType:Members:Count > 0 .AND. oType:Members[0] IS XSourceMemberSymbol VAR xmember
                    nEnd := xmember:Interval:Start-2
                ENDIF
            ENDIF
            IF nStart <= nPos .AND. nEnd >= nPos
                nResult := 0
            ELSEIF nStart > nPos
                nResult := 1
            ELSE
                nResult := -1
            ENDIF
            RETURN nResult

        METHOD FindMemberAtPosition(nPos AS LONG) AS XSourceEntity
            RETURN SELF:FindMember(CompareByPosition, nPos)


        METHOD Clear() AS VOID
            SELF:_usings:Clear()
            SELF:_usingStatics:Clear()
            SELF:_entityList:Clear()
            SELF:_typeList:Clear()
            SELF:AddDefaultUsings()
            IF SELF:HasCode
                SELF:_globalType	:= XSourceTypeSymbol.CreateGlobalType(SELF)
                SELF:_typeList:Add(SELF:_globalType:Name, SELF:_globalType)
            ENDIF

        METHOD SetTypes(types AS IDictionary<STRING, XSourceTypeSymbol>, usings AS IList<STRING>, ;
            staticUsings AS IList<STRING>, aEntities AS IList<XSourceEntity>) AS VOID
            IF SELF:HasCode
                WriteOutputMessage("-->> SetTypes() "+ SELF:SourcePath)
                LOCAL globalType 	  AS XSourceTypeSymbol
                globalType := types:Values:Where( { x => XSourceTypeSymbol.IsGlobalType(x)} ).FirstOrDefault()
                SELF:Clear()
                SELF:_usings:AddRange(usings)
                SELF:_usingStatics:AddRange(staticUsings)
                SELF:_entityList:AddRange(aEntities)
                SELF:_typeList:Append(types)
                IF globalType != NULL
                    SELF:_globalType := globalType
                ENDIF
                WriteOutputMessage(String.Format("<<-- SetTypes() {0} (Types: {1}, Entities: {2})", SELF:SourcePath, _typeList:Count, SELF:_entityList:Count))
                IF ContentsChanged != NULL
                    ContentsChanged()
                ENDIF
            ENDIF

        METHOD FindType(typeName AS STRING, nameSpace := "" AS STRING) AS IXTypeSymbol
            VAR usings := SELF:Usings:ToList()
            IF ! String.IsNullOrEmpty(nameSpace)
                usings:Add(nameSpace)
            ENDIF
            RETURN SELF:Project:FindType(typeName, usings)

        METHOD SaveToDatabase() AS VOID
            IF ! SELF:Virtual
               XDatabase.Update(SELF)
               SELF:Project:ClearCache(SELF)
               IF ! SELF:Interactive
                  SELF:Clear()
               ENDIF
            ENDIF

        METHOD ParseContents(cSource := "" AS STRING) AS VOID
            //
            IF SELF:HasCode

                WriteOutputMessage("-->> ParseContents()")
                BEGIN USING VAR walker := SourceWalker{SELF}
                    TRY
                        if String.IsNullOrEmpty(cSource)
                            walker:Parse(FALSE)
                        else
                            walker:Parse(cSource, FALSE)
                        endif

                    CATCH exception AS System.Exception
                        XSolution.WriteException(exception,__FUNCTION__)
                    END TRY
                END USING
                WriteOutputMessage("<<-- ParseContents()")
            ENDIF

       METHOD WriteOutputMessage(message AS STRING) AS VOID
            XSolution.WriteOutputMessage("XModel.File "+message)

      #region Properties
            // Properties
        PROPERTY AllUsingStatics AS IList<STRING>
            GET

                IF (! SELF:HasCode)

                    RETURN NULL
                ENDIF
                //WriteOutputMessage("-->> AllUsingStatics")
                VAR statics := List<STRING>{}
                statics:AddRange(SELF:_usingStatics)
                IF SELF:Project != NULL .AND. SELF:Project:ProjectNode != NULL .AND. SELF:Project:ProjectNode:ParseOptions:HasRuntime

                    FOREACH asm AS XAssembly IN SELF:Project:AssemblyReferences

                        VAR globalclass := asm:GlobalClassName
                        IF (! String.IsNullOrEmpty(globalclass))

                            statics:AddUnique(globalclass)
                        ENDIF
                    NEXT
                ENDIF
                //WriteOutputMessage("<<-- AllUsingStatics")
                RETURN statics
            END GET
        END PROPERTY

        PROPERTY ContentHashCode AS DWORD
            GET
                IF ! SELF:HasCode .OR. SELF:_typeList == NULL
                    RETURN 0
                ENDIF
                VAR hash := 0U
                TRY
                    FOREACH VAR entity IN SELF:EntityList:ToArray()
                        BEGIN UNCHECKED
                            hash += (DWORD) entity:Prototype:GetHashCode()
                            hash += (DWORD) entity:Range:StartLine
                        END UNCHECKED
                    NEXT
                CATCH
                    NOP
                END TRY
                RETURN hash
            END GET
        END PROPERTY

        PROPERTY FullPath           AS STRING AUTO GET INTERNAL SET
        PROPERTY GlobalType         AS XSourceTypeSymbol GET SELF:_globalType
        PROPERTY HasCode            AS LOGIC GET SELF:IsSource .OR. SELF:IsXaml .OR. SELF:IsHeader
        PROPERTY HasParseErrors     AS LOGIC AUTO
        PROPERTY Interactive        AS LOGIC AUTO
        PROPERTY IsHeader           AS LOGIC GET SELF:_type == XFileType.Header
        PROPERTY IsSource           AS LOGIC GET SELF:_type == XFileType.SourceCode
        PROPERTY IsXaml             AS LOGIC GET SELF:_type == XFileType.XAML
        PROPERTY LastChanged        AS System.DateTime   AUTO GET INTERNAL SET
        PROPERTY Size               AS INT64              AUTO GET INTERNAL SET
        PROPERTY Name               AS STRING GET System.IO.Path.GetFileNameWithoutExtension(SELF:FullPath)
        PROPERTY UpdatedOnDisk      AS LOGIC
            GET
               VAR fi                  := System.IO.FileInfo{SELF:FullPath}
               RETURN SELF:LastChanged != fi:LastWriteTime .OR. SELF:Size != fi:Length
            END GET
        END PROPERTY


        PROPERTY Project AS XProject
            GET
                IF SELF:_project == NULL
                    SELF:_project := XSolution.OrphanedFilesProject
                    SELF:_project:AddFile(SELF:FullPath)
                ENDIF
                RETURN SELF:_project
            END GET
            INTERNAL SET
                SELF:_project := value
            END SET
        END PROPERTY

        PROPERTY SourcePath AS STRING
            GET
                IF (SELF:IsXaml)
                    RETURN SELF:XamlCodeBehindFile
                ENDIF
                RETURN SELF:FullPath
            END GET
        END PROPERTY

        PROPERTY TypeList AS IDictionary<STRING, XSourceTypeSymbol>
            GET
                IF ! SELF:HasCode .OR. SELF:_typeList==NULL
                    RETURN NULL
                ENDIF
                RETURN SELF:_typeList:ToReadOnlyDictionary()
            END GET
        END PROPERTY

        PROPERTY Usings AS IList<STRING>
            GET
                IF ! SELF:HasCode .OR. _usings == NULL
                    RETURN <STRING>{}
                ENDIF
                RETURN _usings:ToArray()
            END GET
         END PROPERTY
        PROPERTY StaticUsings AS IList<STRING>
            GET
                IF ! SELF:HasCode .OR. _usingStatics == NULL
                    RETURN <STRING>{}
                ENDIF
                RETURN _usingStatics:ToArray()
            END GET
         END PROPERTY
        PROPERTY UsingsStr AS STRING
            GET
               IF _usings == NULL
                  return String.Empty
               endif
               var sb := System.Text.StringBuilder{}
               FOREACH var str in _usings
                  IF (sb:Length > 0)
                     sb:AppendLine("")
                  ENDIF
                  sb:Append(str)
               NEXT

               RETURN sb:ToString()
            END GET
            SET
               SELF:_usings:Clear()
               if ! String.IsNullOrEmpty(value)
                  self:_usings:AddRange(value:Split( <CHAR>{'\r','\n'},StringSplitOptions.RemoveEmptyEntries))
               endif
            END SET
        END PROPERTY

        PROPERTY StaticUsingsStr AS STRING
            GET
             IF _usingStatics == NULL
                  return String.Empty
               endif
                 var sb := System.Text.StringBuilder{}
               FOREACH var str in _usingStatics
                  IF (sb:Length > 0)
                     sb:AppendLine("")
                  ENDIF
                  sb:Append(str)
               NEXT
               RETURN sb:ToString()
            END GET
            SET
               SELF:_usingStatics:Clear()
               if ! String.IsNullOrEmpty(value)
                  self:_usingStatics:AddRange(value:Split( <CHAR>{'\r','\n'},StringSplitOptions.RemoveEmptyEntries))
               endif
            END SET
        END PROPERTY


        PROPERTY XamlCodeBehindFile AS STRING
            GET
                VAR projectNode := SELF:Project:ProjectNode
                RETURN System.IO.Path.ChangeExtension(System.IO.Path.Combine(projectNode:IntermediateOutputPath, System.IO.Path.GetFileName(SELF:FullPath)), ".g.prg")
            END GET
        END PROPERTY

        PROPERTY XFileType AS XFileType GET SELF:_type

        PUBLIC EVENT ContentsChanged AS Action
      #endregion

    END CLASS

END NAMESPACE


