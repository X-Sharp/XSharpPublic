//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Collections.Concurrent
USING System.Collections.Generic
USING System
USING System.Linq
USING System.Diagnostics
USING XSharpModel
USING LanguageService.CodeAnalysis.XSharp
USING STATIC XSharpModel.XFileTypeHelpers
BEGIN NAMESPACE XSharpModel
    DELEGATE FindMemberComparer (oElement AS XEntityDefinition, nValue AS LONG ) AS LONG

    [DebuggerDisplay("{FullPath,nq}")];
    CLASS XFile
        #region Fields
        PROPERTY Id           AS INT64          AUTO      GET INTERNAL SET 
        PRIVATE _globalType	AS XTypeDefinition
        PRIVATE _lock			AS OBJECT
        PRIVATE _parsed			AS LOGIC
        PRIVATE _type			AS XFileType
        PRIVATE _typeList		AS ConcurrentDictionary<STRING, XTypeDefinition>
        PRIVATE _entityList	AS List<XEntityDefinition>
        PRIVATE _usings			AS List<STRING>
        PRIVATE _usingStatics	AS List<STRING>
        PRIVATE _project      AS XProject
        #endregion
        // Methods
        CONSTRUCTOR(fullPath AS STRING, project AS XProject)
            SUPER()
            //
            SELF:FullPath := fullPath
            SELF:LastChanged := System.DateTime.MinValue
            SELF:_type := GetFileType(fullPath)
            SELF:_parsed := ! SELF:HasCode
            SELF:_lock := OBJECT{}
            SELF:_project := project

        PROPERTY EntityList AS 	List<XEntityDefinition> GET _entityList
        PROPERTY Dialect AS XSharpDialect GET _project:Dialect
        PROPERTY Virtual AS LOGIC AUTO

        METHOD AddDefaultUsings() AS void
            IF SELF:_usings:IndexOf("XSharp") == -1
                SELF:_usings:Insert(0, "XSharp")
            ENDIF
            IF SELF:_usings:IndexOf("System") == -1
                SELF:_usings:Insert(0, "System")
            ENDIF
            RETURN


        METHOD FirstMember() AS IXMember
            IF (! SELF:HasCode)
                RETURN NULL
            ENDIF
            IF (SELF:TypeList == NULL || SELF:TypeList:Count == 0)
                RETURN NULL
            ENDIF
            BEGIN LOCK SELF:_lock
                VAR element := SELF:TypeList:FirstOrDefault()
                RETURN element:Value:Members:FirstOrDefault()
            END
            ///
            /// <Summary>Find member in file based on 0 based line number</Summary>
            ///
            ///

        METHOD FindMember(oDel AS FindMemberComparer, nValue AS LONG) AS XEntityDefinition
            LOCAL oResult := NULL_OBJECT AS XEntityDefinition
            LOCAL oLast AS XEntityDefinition
            // perform binary search to speed up things
            VAR current := 0
            VAR bottom := 0
            IF (_entityList == NULL_OBJECT)
                RETURN NULL_OBJECT
            ENDIF
            VAR top := _entityList:Count
            oLast := _entityList:FirstOrDefault()
            DO WHILE top - bottom > 1
                // determine middle
                current := (bottom + top) / 2
                VAR oElement := _entityList[current]
                VAR result := oDel(oElement, nValue)
                IF result == 0
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

        PRIVATE METHOD CompareByLine(oElement AS XEntityDefinition, nLine AS LONG) AS LONG
            LOCAL nResult AS LONG
            LOCAL nStart, nEnd AS LONG
            nStart := oElement:Range:StartLine
            nEnd   := oElement:Range:EndLine
            IF oElement IS XTypeDefinition
                VAR oType := oElement ASTYPE XTypeDefinition
                IF oType:Members:Count > 0 .and. oType:Members[0] is XMemberDefinition VAR xmember
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

        METHOD FindMemberAtRow(nLine AS LONG) AS XEntityDefinition
            RETURN SELF:FindMember(CompareByLine, nLine)

            ///
            /// <Summary>Find member in file based on 0 based position</Summary>
            ///
            ///
        PRIVATE METHOD CompareByPosition(oElement AS XEntityDefinition, nPos AS LONG) AS LONG
            LOCAL nResult AS LONG
            LOCAL nStart, nEnd AS LONG
            nStart := oElement:Interval:Start
            nEnd   := oElement:Interval:Stop
            IF oElement IS XTypeDefinition
                VAR oType := oElement ASTYPE XTypeDefinition
                IF oType:Members:Count > 0 .and. oType:Members[0] is XMemberDefinition VAR xmember
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

        METHOD FindMemberAtPosition(nPos AS LONG) AS XEntityDefinition
            RETURN SELF:FindMember(CompareByPosition, nPos)


        METHOD InitTypeList() AS VOID
            SELF:_usings		:= List<STRING>{}
            SELF:_usingStatics	:= List<STRING>{}
            SELF:_entityList    := List<XEntityDefinition>{}
            SELF:_typeList		:= ConcurrentDictionary<STRING, XTypeDefinition>{System.StringComparer.InvariantCultureIgnoreCase}
            SELF:AddDefaultUsings()
            IF SELF:HasCode
                SELF:_globalType	:= XTypeDefinition.CreateGlobalType(SELF)
                SELF:_typeList:TryAdd(SELF:_globalType:Name, SELF:_globalType)
            ENDIF

        METHOD SetTypes(types AS IDictionary<STRING, XTypeDefinition>, usings AS IList<STRING>, ;
            staticUsings AS IList<STRING>, aEntities AS IList<XEntityDefinition>) AS VOID
            IF SELF:HasCode
                //WriteOutputMessage("-->> SetTypes() "+ SELF:SourcePath)
                BEGIN LOCK SELF
                    FOREACH VAR type IN _typeList
                        SELF:Project:RemoveType(type:Value)
                    NEXT
                     
                    SELF:_typeList:Clear()
                    SELF:_usings:Clear()
                    SELF:_usingStatics:Clear()
                    FOREACH type AS KeyValuePair<STRING, XTypeDefinition> IN types
                        SELF:_typeList:TryAdd(type:Key, type:Value)
                        IF (XTypeDefinition.IsGlobalType(type:Value))
                            SELF:_globalType := type:Value
                        ENDIF
                        SELF:Project:AddType(type:Value)
                    NEXT
                    SELF:_usings:AddRange(usings)
                    SELF:AddDefaultUsings()
                    SELF:_usingStatics:AddRange(staticUsings)
                    SELF:_entityList:Clear()
                    SELF:_entityList:AddRange(aEntities)
                END LOCK
                //WriteOutputMessage(String.Format("<<-- SetTypes() {0} (Types: {1}, Entities: {2})", SELF:SourcePath, _typeList:Count, SELF:_entityList:Count))
            ENDIF
      
         
        METHOD SaveToDatabase() AS VOID
            IF ! SELF:Virtual
               XDatabase.Update(SELF)
               IF ! SELF:Interactive
                  SELF:InitTypeList()
               ENDIF
            ENDIF            
         
        METHOD WaitParsing() AS VOID
            //
            IF SELF:HasCode

                //WriteOutputMessage("-->> WaitParsing()")
                BEGIN LOCK SELF:_lock

                    IF ! SELF:Parsed
                        BEGIN USING VAR walker := SourceWalker{SELF}
                            TRY
                                walker:Parse(FALSE)
                                
                            CATCH exception AS System.Exception
                                XSolution.WriteException(exception)
                            END TRY
                        END USING
                    ENDIF
                END LOCK
                //WriteOutputMessage("<<-- WaitParsing()")
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
                BEGIN LOCK SELF:_lock

                    statics:AddRange(SELF:_usingStatics)
                    IF SELF:Project != NULL .AND. SELF:Project:ProjectNode != NULL .AND. SELF:Project:ProjectNode:ParseOptions:HasRuntime

                        FOREACH asm AS XAssembly IN SELF:Project:AssemblyReferences

                            VAR globalclass := asm:GlobalClassName
                            IF (! String.IsNullOrEmpty(globalclass))

                                statics:AddUnique(globalclass)
                            ENDIF
                        NEXT
                    ENDIF
                END LOCK
                //WriteOutputMessage("<<-- AllUsingStatics")
                RETURN statics
            END GET
        END PROPERTY

        PROPERTY ContentHashCode AS DWORD
            GET
                IF ! SELF:HasCode .OR. SELF:TypeList == NULL
                    RETURN 0
                ENDIF
                BEGIN LOCK SELF:_lock
                    VAR hash := 0U
                    FOREACH var entity in SELF:EntityList
                        BEGIN UNCHECKED
                           hash += (DWORD) entity:Prototype:GetHashCode()
                           hash += (DWORD) entity:Range:StartLine
                        END UNCHECKED
                    NEXT
                    RETURN hash
                END LOCK
            END GET
        END PROPERTY

        PROPERTY FullPath           AS STRING AUTO GET INTERNAL SET 
        PROPERTY GlobalType         AS XTypeDefinition GET SELF:_globalType
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

        PROPERTY Parsed AS LOGIC
            GET
                //WriteOutputMessage("-->> Parsed")
                LOCAL flag AS LOGIC
                BEGIN LOCK SELF:_lock

                    flag := SELF:_parsed
                END LOCK
                //WriteOutputMessage("<<-- Parsed")
                RETURN flag
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
        END PROPERTY

        PROPERTY SourcePath AS STRING
            GET
                IF (SELF:IsXaml)
                    RETURN SELF:XamlCodeBehindFile
                ENDIF
                RETURN SELF:FullPath
            END GET
        END PROPERTY

        PROPERTY TypeList AS IDictionary<STRING, XTypeDefinition>
            GET
                IF ! SELF:HasCode
                    RETURN NULL
                ENDIF
                BEGIN LOCK SELF:_lock
                    RETURN SELF:_typeList
                END LOCK
            END GET
        END PROPERTY

        PROPERTY Usings AS IList<STRING>
            GET
                IF ! SELF:HasCode
                    RETURN NULL
                ENDIF
                RETURN _usings:ToArray()
            END GET
        END PROPERTY

        PROPERTY XamlCodeBehindFile AS STRING
            GET
                VAR projectNode := SELF:Project:ProjectNode
                RETURN System.IO.Path.ChangeExtension(System.IO.Path.Combine(projectNode:IntermediateOutputPath, System.IO.Path.GetFileName(SELF:FullPath)), ".g.prg")
            END GET
        END PROPERTY

        PROPERTY XFileType AS XFileType GET SELF:_type
      #endregion
 
    END CLASS

END NAMESPACE


