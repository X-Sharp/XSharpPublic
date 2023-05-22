//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Collections.Concurrent
USING System.Collections.Generic
USING System.Collections.ObjectModel
USING System
USING System.IO
USING System.Linq
USING System.Diagnostics
USING XSharpModel
USING LanguageService.CodeAnalysis.XSharp
USING STATIC XSharpModel.XFileTypeHelpers
BEGIN NAMESPACE XSharpModel
    //DELEGATE FindMemberComparer (oElement AS XSourceEntity, nValue AS LONG ) AS LONG

    [DebuggerDisplay("{FullPath,nq}")];
    CLASS XFile
        #region Fields
        PROPERTY Id             AS INT64          AUTO      GET INTERNAL SET
        PRIVATE _globalType 	AS XSourceTypeSymbol
        PRIVATE _type			AS XFileType
        PRIVATE _typeList		AS XDictionary<STRING, XSourceTypeSymbol>
        PRIVATE _entityList	    AS SynchronizedCollection<XSourceEntity>
        PRIVATE _lastEntity     as XSourceEntity
        PRIVATE _lastLine       as LONG
        PRIVATE _lastPos        as LONG
        PRIVATE _usings			AS SynchronizedCollection<STRING>
        PRIVATE _usingStatics	AS SynchronizedCollection<STRING>
        PRIVATE _project        AS XProject
        PRIVATE _includeFiles  AS List<XInclude>
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
            SELF:_includeFiles := List<XInclude>{}
            SELF:AddDefaultUsings()
            SELF:_usingStatics	:= SynchronizedCollection<STRING>{}
            SELF:_entityList    := SynchronizedCollection<XSourceEntity>{}
            SELF:_typeList		:= XDictionary<STRING, XSourceTypeSymbol>{System.StringComparer.InvariantCultureIgnoreCase}
            SELF:Clear()
            IF SELF:HasCode
                SELF:_globalType	:= XSourceTypeSymbol.CreateGlobalType(SELF)
                SELF:_typeList:Add(SELF:_globalType:Name, SELF:_globalType)
            ENDIF


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


        METHOD FindMemberAtRow(nLine AS LONG) AS XSourceEntity
            IF (_entityList == NULL_OBJECT)
                RETURN NULL_OBJECT
            ENDIF
            IF SELF:_lastEntity != NULL .and. SELF:_lastLine == nLine
                //WriteOutputMessage(i"FindMemberAtRow {nLine} {_lastEntity}")
                RETURN SELF:_lastEntity
            ENDIF
            var entities := SELF:_entityList:Where( { e => e:IncludesLine(nLine)})
            var result := entities:LastOrDefault()
            if result == null_object .and. _entityList:Count > 0
                var temp := _entityList:First()
                if nLine < temp:Range:StartLine
                    result := temp
                else
                    temp := _entityList:Last()
                    if nLine > temp:Range:EndLine
                        result := temp
                        do while result:Parent is XSourceEntity var source .and. source:Range:EndLine >result:Range:EndLine
                            result := source
                        enddo

                    endif
                endif
            endif
            SELF:_lastEntity := result
            SELF:_lastLine   := nLine
            SELF:_lastPos    := -1
            //WriteOutputMessage(i"FindMemberAtRow {nLine} {result}")
            return result

            ///
            /// <Summary>Find member in file based on 0 based position</Summary>
            ///
            ///

        METHOD FindMemberAtPosition(nPos AS LONG) AS XSourceEntity
            IF (_entityList == NULL_OBJECT)
                RETURN NULL_OBJECT
            ENDIF
            IF SELF:_lastEntity != NULL .and. nPos == SELF:_lastPos
                RETURN SELF:_lastEntity
            ENDIF

            var entities := SELF:_entityList:Where( { e => e:IncludesPosition(nPos)})
            var result := entities:LastOrDefault()
            if result == null_object .and. _entityList:Count > 0
                var temp := _entityList:First()
                if nPos < temp:Interval:Start
                    result := temp
                else
                    temp := _entityList:Last()
                    if nPos > temp:Interval:Stop
                        result := temp
                        do while result:Parent is XSourceEntity var source .and. source:Interval:Stop >result:Interval:Stop
                            result := source
                        enddo
                    endif
                endif
            endif
            SELF:_lastEntity := result
            SELF:_lastLine   := -1
            SELF:_lastPos    := nPos
            return result


        METHOD Clear() AS VOID
            SELF:_usings:Clear()
            SELF:_usingStatics:Clear()
            SELF:_entityList:Clear()
            SELF:_lastEntity := NULL
            SELF:_typeList:Clear()
            SELF:AddDefaultUsings()

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
        METHOD NofityClients as VOID
            IF ContentsChanged != NULL
                ContentsChanged()
            ENDIF
            RETURN

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
                IF SELF:Project != NULL
                    statics:AddRange(SELF:Project:AllUsingStatics)
                ENDIF
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
        PROPERTY IncludeFiles       AS IList<XInclude> GET _includeFiles
        PROPERTY Interactive        AS LOGIC AUTO
        PROPERTY IsHeader           AS LOGIC GET SELF:_type == XFileType.Header
        PROPERTY IsSource           AS LOGIC GET SELF:_type == XFileType.SourceCode .or. SELF:_type == XFileType.PreprocessorOutput
        PROPERTY IsXaml             AS LOGIC GET SELF:_type == XFileType.XAML
        PROPERTY LastChanged        AS System.DateTime   AUTO GET INTERNAL SET
        PROPERTY Size               AS INT64              AUTO GET INTERNAL SET
        PROPERTY Name               AS STRING GET Path.GetFileNameWithoutExtension(SELF:FullPath)

        PROPERTY UpdatedOnDisk      AS LOGIC
            GET
               VAR fi                  := FileInfo{SELF:FullPath}
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
                var fn := Path.GetFileName(SELF:FullPath)
                fn := Path.ChangeExtension( fn, ".g.i.prg")
                return Path.Combine(projectNode:IntermediateOutputPath, fn)
            END GET
        END PROPERTY

        PROPERTY XFileType AS XFileType GET SELF:_type

        PUBLIC EVENT ContentsChanged AS Action
      #endregion

    END CLASS

END NAMESPACE


