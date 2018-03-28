//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System
using System.Collections.Generic
using System.Linq
using System.Text
using System.Threading.Tasks
using EnvDTE
using LanguageService.CodeAnalysis
using LanguageService.CodeAnalysis.XSharp
using System.Collections.Concurrent
using System.Collections.Immutable
using EnvDTE80
using Microsoft.VisualStudio
using Microsoft.VisualStudio.Shell.Interop
using System.Diagnostics
BEGIN NAMESPACE XSharpModel
    [DebuggerDisplay("{Name,nq}")];
    CLASS XProject
        // Fields
        PRIVATE _AssemblyReferences := List<AssemblyInfo>{} AS List<AssemblyInfo>
        PRIVATE _loaded AS Logic
        PRIVATE _parseOptions := null AS XSharpParseOptions
        PRIVATE _projectNode AS IXSharpProject
        PRIVATE _projectOutputDLLs := Dictionary<string, string>{System.StringComparer.OrdinalIgnoreCase} AS Dictionary<string, string>
        PRIVATE _ReferencedProjects := List<XProject>{} AS List<XProject>
        PRIVATE _StrangerProjects := List<Project>{} AS List<Project>
        PRIVATE _typeController AS SystemTypeController
        PRIVATE _unprocessedProjectReferences := List<string>{} AS List<string>
        PRIVATE _unprocessedStrangerProjectReferences := List<string>{} AS List<string>
        public  FileWalkComplete AS XProject.OnFileWalkComplete
        PRIVATE xOtherFilesDict AS ConcurrentDictionary<string, XFile>
        PRIVATE xSourceFilesDict AS ConcurrentDictionary<string, XFile>

        // Methods
         constructor(project as IXSharpProject)
			SUPER()
            //
            SELF:_projectNode := project
            SELF:xSourceFilesDict := ConcurrentDictionary<string, XFile>{System.StringComparer.OrdinalIgnoreCase}
            SELF:xOtherFilesDict := ConcurrentDictionary<string, XFile>{System.StringComparer.OrdinalIgnoreCase}
            SELF:_typeController := SystemTypeController{}
            SELF:_loaded := TRUE
            IF (SELF:_projectNode == null)

            ENDIF

        METHOD AddAssemblyReference(path AS string) AS void
            LOCAL item AS AssemblyInfo
            //
            item := SystemTypeController.LoadAssembly(path)
            SELF:_AssemblyReferences:Add(item)
            item:AddProject(SELF)

        METHOD AddAssemblyReference(reference AS VSLangProj.Reference) AS void
            LOCAL item AS AssemblyInfo
            //
            item := SystemTypeController.LoadAssembly(reference)
            SELF:_AssemblyReferences:Add(item)
            item:AddProject(SELF)

        METHOD AddFile(filePath AS string) AS Logic
            LOCAL xFile AS XFile
            //
            xFile := XFile{filePath}
            RETURN SELF:AddFile(xFile)

        METHOD AddFile(xFile AS XFile) AS Logic
            LOCAL file AS XFile
            LOCAL xamlCodeBehindFile AS string
            //
            IF (xFile != null)
                //
                IF (xFile:IsSource)
                    //
                    IF (SELF:xSourceFilesDict:ContainsKey(xFile:FullPath))
                        //
                        SELF:xSourceFilesDict:TryRemove(xFile:FullPath, out file)
                    ENDIF
                    xFile:Project := SELF
                    RETURN SELF:xSourceFilesDict:TryAdd(xFile:FullPath, xFile)
                ENDIF
                IF (xFile:IsXaml)
                    //
                    xFile:Project := SELF
                    xamlCodeBehindFile := xFile:XamlCodeBehindFile
                    IF (SELF:xSourceFilesDict:ContainsKey(xamlCodeBehindFile))
                        //
                        SELF:xSourceFilesDict:TryRemove(xamlCodeBehindFile, out  file)
                    ENDIF
                    SELF:xSourceFilesDict:TryAdd(xamlCodeBehindFile, xFile)
                    IF (SELF:xOtherFilesDict:ContainsKey(xFile:FullPath))
                        //
                        SELF:xOtherFilesDict:TryRemove(xFile:FullPath, out file)
                    ENDIF
                    xFile:Project := SELF
                    RETURN SELF:xOtherFilesDict:TryAdd(xFile:FullPath, xFile)
                ENDIF
                IF (SELF:xOtherFilesDict:ContainsKey(xFile:FullPath))
                    //
                    SELF:xOtherFilesDict:TryRemove(xFile:FullPath, out file)
                ENDIF
                xFile:Project := SELF
                RETURN SELF:xOtherFilesDict:TryAdd(xFile:FullPath, xFile)
            ENDIF
            RETURN FALSE

        METHOD AddProjectOutput(sProjectURL AS string, sOutputDLL AS string) AS void
            //
            IF (SELF:_projectOutputDLLs:ContainsKey(sProjectURL))
                //
                SELF:_projectOutputDLLs:Item[sProjectURL] := sOutputDLL
            ELSE
                //
                SELF:_projectOutputDLLs:Add(sProjectURL, sOutputDLL)
            ENDIF

        METHOD AddProjectReference(url AS string) AS Logic
            //
            IF (! SELF:_unprocessedProjectReferences:Contains(url))
                //
                SELF:_unprocessedProjectReferences:Add(url)
                RETURN TRUE
            ENDIF
            RETURN FALSE

        METHOD AddStrangerProjectReference(url AS string) AS Logic
            //
            IF (! SELF:_unprocessedStrangerProjectReferences:Contains(url))
                //
                SELF:_unprocessedStrangerProjectReferences:Add(url)
                RETURN TRUE
            ENDIF
            RETURN FALSE

        METHOD ClearAssemblyReferences() AS void
            //
            FOREACH info AS AssemblyInfo IN SELF:_AssemblyReferences
                //
                info:RemoveProject(SELF)
            NEXT
            SELF:_AssemblyReferences:Clear()

        METHOD FindFullPath(fullPath AS string) AS XFile
            //
            IF (SELF:xSourceFilesDict:ContainsKey(fullPath))
                //
                RETURN SELF:xSourceFilesDict:Item[fullPath]
            ENDIF
            IF (SELF:xOtherFilesDict:ContainsKey(fullPath))
                //
                RETURN SELF:xOtherFilesDict:Item[fullPath]
            ENDIF
            RETURN null

        METHOD FindFunction(name AS string) AS XTypeMember
            LOCAL members AS System.Collections.Immutable.IImmutableList<XTypeMember>
            //
            FOREACH file AS XFile IN SELF:SourceFiles
                //
                members := file:GlobalType:Members
                IF (members != null)
                    //
                    FOREACH oMember as XTypeMember in members
                        IF (oMember:Kind == Kind.Procedure .or. oMember:Kind == Kind.Function ) .and. string.Compare(oMember:Name, name, true) == 0
							RETURN oMember
						ENDIF
					NEXT
                ENDIF
            NEXT
            RETURN null

        METHOD FindSystemType(name AS string, usings AS IReadOnlyList<string>) AS System.Type
            //
            SELF:ResolveProjectReferenceDLLs()
            RETURN SELF:_typeController:FindType(name, usings, SELF:_AssemblyReferences)

        METHOD GetAssemblyNamespaces() AS System.Collections.Immutable.ImmutableList<string>
            //
            RETURN SELF:_typeController:GetNamespaces(SELF:_AssemblyReferences)

        PRIVATE METHOD GetStrangerOutputDLL(sProject AS string, p AS Project) AS string
			var path := ""
            TRY
                //
                var activeConfiguration := p:ConfigurationManager:ActiveConfiguration
                var item := activeConfiguration:Properties:Item("OutputPath")
                
                IF item != null
					path := (string) item:Value
                ENDIF
                FOREACH group AS OutputGroup IN activeConfiguration:OutputGroups
                    //
                    IF group:FileCount == 1  .AND. group:CanonicalName == "Built"
						var names := (System.Array) group:Filenames
                        FOREACH str as string	 IN names
                            path := System.IO.Path.Combine(path, str)
                        NEXT
                    ENDIF
                NEXT
                IF ! System.IO.Path.IsPathRooted(path)
                    path := System.IO.Path.Combine(System.IO.Path.GetDirectoryName(sProject), path)
                ENDIF
            CATCH exception as System.Exception
                //
                IF (System.Diagnostics.Debugger.IsAttached)
                    //
                    System.Diagnostics.Debug.WriteLine(exception:Message)
                ENDIF
            END TRY
            RETURN path

        METHOD Lookup(typeName AS string, caseInvariant AS Logic) AS XType
            LOCAL @@type AS XType
            LOCAL type2 AS XType
            LOCAL fileArray AS XFile[]
            //
            @@type := null
            type2 := null
            fileArray := System.Linq.Enumerable.ToArray<XFile>(SELF:xSourceFilesDict:Values)
            FOREACH file AS XFile IN fileArray
                //
                IF (file:TypeList != null)
                    //
                    file:TypeList:TryGetValue(typeName, out type2)
                    IF (((type2 != null) .AND. ! caseInvariant) .AND. ((@@type:FullName != typeName) .AND. (@@type:Name != typeName)))
                        //
                        @@type := null
                    ENDIF
                    IF (type2 != null)
                        //
                        IF (! type2:IsPartial)
                            //
                            RETURN type2
                        ENDIF
                        IF (@@type != null)
                            //
                            @@type := @@type:Merge(type2)
                        ELSE
                            //
                            @@type := type2:Duplicate()
                        ENDIF
                    ENDIF
                ENDIF
            NEXT
            RETURN @@type

        METHOD LookupForStranger(typeName AS string, caseInvariant AS Logic) AS CodeElement
            //
            RETURN null

        METHOD LookupFullName(typeName AS string, caseInvariant AS Logic) AS XType
            LOCAL @@type AS XType
            LOCAL otherType AS XType
            LOCAL fileArray AS XFile[]
            LOCAL type3 AS XType
            //
            @@type := null
            otherType := null
            fileArray := System.Linq.Enumerable.ToArray<XFile>(SELF:xSourceFilesDict:Values)
            FOREACH file AS XFile IN fileArray
                //
                type3 := null
                IF (file:TypeList != null)
                    //
                    IF (file:TypeList:TryGetValue(typeName, out type3))
                        //
                        otherType := type3
                        IF (! caseInvariant .AND. ((type3:FullName != typeName) .AND. (type3:Name != typeName)))
                            //
                            otherType := null
                        ENDIF
                    ENDIF
                    IF (otherType != null)
                        //
                        IF (! otherType:IsPartial)
                            //
                            RETURN otherType
                        ENDIF
                        IF (@@type != null)
                            //
                            @@type := @@type:Merge(otherType)
                        ELSE
                            //
                            @@type := otherType:Duplicate()
                        ENDIF
                    ENDIF
                ENDIF
            NEXT
            RETURN @@type

        METHOD LookupFullNameReferenced(typeName AS string, caseInvariant AS Logic) AS XType
            LOCAL fullName AS XType
            //
            fullName := null
            FOREACH project AS XProject IN SELF:ReferencedProjects
                //
                fullName := project:LookupFullName(typeName, caseInvariant)
                IF (fullName != null)
                    //
                    RETURN fullName
                ENDIF
            NEXT
            RETURN fullName

        METHOD LookupReferenced(typeName AS string, caseInvariant AS Logic) AS XType
            LOCAL @@type AS XType
            //
            @@type := null
            FOREACH project AS XProject IN SELF:ReferencedProjects
                //
                @@type := project:Lookup(typeName, caseInvariant)
                IF (@@type != null)
                    //
                    RETURN @@type
                ENDIF
            NEXT
            RETURN @@type

        METHOD RemoveAssemblyReference(fileName AS string) AS void
            //
            FOREACH info AS AssemblyInfo IN SELF:_AssemblyReferences
                //
                IF (String.Equals(info:FileName, fileName, System.StringComparison.OrdinalIgnoreCase))
                    //
                    SELF:_AssemblyReferences:Remove(info)
                    EXIT

                ENDIF
            NEXT

        METHOD RemoveFile(url AS string) AS void
            LOCAL file AS XFile
            LOCAL file2 AS XFile
            //
            IF (SELF:xOtherFilesDict:ContainsKey(url))
                //
                SELF:xOtherFilesDict:TryRemove(url, out file)
                IF (file:IsXaml)
                    //
                    url := file:XamlCodeBehindFile
                ENDIF
            ENDIF
            IF (SELF:xSourceFilesDict:ContainsKey(url))
                //
                SELF:xSourceFilesDict:TryRemove(url, out file2)
            ENDIF

        METHOD RemoveProjectOutput(sProjectURL AS string) AS void
            //
            IF (SELF:_projectOutputDLLs:ContainsKey(sProjectURL))
                //
                SELF:RemoveProjectReferenceDLL(SELF:_projectOutputDLLs:Item[sProjectURL])
                SELF:_projectOutputDLLs:Remove(sProjectURL)
            ENDIF

        METHOD RemoveProjectReference(url AS string) AS Logic
            LOCAL item AS XProject
            LOCAL outputFile AS string
            //
            IF (SELF:_unprocessedProjectReferences:Contains(url))
                //
                SELF:_unprocessedProjectReferences:Remove(url)
                RETURN TRUE
            ENDIF
            item := XSolution.FindProject(url)
            IF (SELF:_ReferencedProjects:Contains(item))
                //
                outputFile := item:ProjectNode:OutputFile
                SELF:_ReferencedProjects:Remove(item)
                RETURN TRUE
            ENDIF
            SELF:RemoveProjectOutput(url)
            RETURN FALSE

        METHOD RemoveProjectReferenceDLL(DLL AS string) AS void
            //
            SELF:RemoveAssemblyReference(DLL)

        METHOD RemoveStrangerProjectReference(url AS string) AS Logic
            LOCAL item AS Project
            //
            IF (SELF:_unprocessedStrangerProjectReferences:Contains(url))
                //
                SELF:_unprocessedStrangerProjectReferences:Remove(url)
                RETURN TRUE
            ENDIF
            SELF:RemoveProjectOutput(url)
            item := SELF:ProjectNode:FindProject(url)
            IF ((item != null) .AND. SELF:_StrangerProjects:Contains(item))
                //
                SELF:_StrangerProjects:Remove(item)
                RETURN TRUE
            ENDIF
            RETURN FALSE

        METHOD ResolveProjectReferenceDLLs() AS void
            //
            IF (SELF:hasUnprocessedReferences)
                //
                SELF:ResolveUnprocessedProjectReferences()
                SELF:ResolveUnprocessedStrangerReferences()
            ENDIF
            FOREACH str AS string IN SELF:_projectOutputDLLs:Values
                //
                IF (SystemTypeController.FindAssemblyByLocation(str) == null)
                    //
                    SELF:AddAssemblyReference(str)
                ENDIF
            NEXT

        PRIVATE METHOD ResolveUnprocessedProjectReferences() AS void
            LOCAL list AS List<string>
            LOCAL item AS XProject
            LOCAL outputFile AS string
            //
            IF (SELF:_unprocessedProjectReferences:Count != 0)
                //
                list := List<string>{}
                FOREACH str AS string IN SELF:_unprocessedProjectReferences
                    //
                    item := XSolution.FindProject(str)
                    IF (item != null)
                        //
                        list:Add(str)
                        SELF:_ReferencedProjects:Add(item)
                        outputFile := item:ProjectNode:OutputFile
                        SELF:AddProjectOutput(str, outputFile)
                    ENDIF
                NEXT
                FOREACH str3 AS string IN list
                    //
                    SELF:_unprocessedProjectReferences:Remove(str3)
                NEXT
            ENDIF

        PRIVATE METHOD ResolveUnprocessedStrangerReferences() AS void
            LOCAL list AS List<string>
            LOCAL item AS Project
            LOCAL strangerOutputDLL AS string
            //
            IF (SELF:_unprocessedStrangerProjectReferences:Count != 0)
                //
                list := List<string>{}
                FOREACH str AS string IN SELF:_unprocessedStrangerProjectReferences
                    //
                    item := SELF:ProjectNode:FindProject(str)
                    IF (item != null)
                        //
                        list:Add(str)
                        SELF:_StrangerProjects:Add(item)
                        strangerOutputDLL := SELF:GetStrangerOutputDLL(str, item)
                        SELF:AddProjectOutput(str, strangerOutputDLL)
                    ENDIF
                NEXT
                FOREACH str3 AS string IN list
                    //
                    SELF:_unprocessedStrangerProjectReferences:Remove(str3)
                NEXT
            ENDIF

        PRIVATE METHOD SearchInItems(projectItems AS ProjectItems, typeName AS string, caseInvariant AS Logic) AS CodeElement
            //
            RETURN null

        METHOD UnLoad() AS void
            //
            SELF:Loaded := FALSE
            FOREACH info AS AssemblyInfo IN SELF:_AssemblyReferences
                //
                info:RemoveProject(SELF)
            NEXT
            SELF:_AssemblyReferences:Clear()

        METHOD UpdateAssemblyReference(fileName AS string) AS void
            //
            SystemTypeController.LoadAssembly(fileName):AddProject(SELF)

        METHOD Walk() AS void
            //
            ModelWalker.GetWalker():AddProject(SELF)

        METHOD WalkFile(file AS XFile) AS void
            //
            ModelWalker.GetWalker():FileWalk(file)


        // Properties
        PROPERTY AssemblyReferences AS List<AssemblyInfo>
            GET
                //
                RETURN SELF:_AssemblyReferences
            END GET
        END PROPERTY

        PRIVATE PROPERTY hasUnprocessedReferences AS Logic
            GET
                //
                RETURN ((SELF:_unprocessedProjectReferences:Count + SELF:_unprocessedStrangerProjectReferences:Count) > 0)
            END GET
        END PROPERTY

        PROPERTY Loaded AS Logic
            GET
                //
                RETURN SELF:_loaded
            END GET
            SET
                //
                SELF:_loaded := value
            END SET
        END PROPERTY

        PROPERTY Name AS string
            GET
                //
                RETURN System.IO.Path.GetFileNameWithoutExtension(SELF:ProjectNode:Url)
            END GET
        END PROPERTY

        PROPERTY Namespaces AS ImmutableList<XType>
            GET
                //
                var list := List<XType>{}
                var fileArray := SELF:SourceFiles:ToArray()
                FOREACH file AS XFile IN fileArray
                    //
                    IF (file:TypeList != null)
                        var values := file:TypeList:Values
                        foreach elmt as XType in values
                            IF elmt:Kind == Kind.Namespace
                                if list:Find( { x => x:Name:ToLowerInvariant() == elmt:Name:ToLowerInvariant() } ) == null
									list:Add(elmt)
								endif
                            ENDIF
						next
                    ENDIF
                NEXT
                RETURN System.Collections.Immutable.ImmutableList.ToImmutableList<XType>(list)
            END GET
        END PROPERTY

        PROPERTY OtherFiles AS List<XFile>
            GET
                //
                RETURN System.Linq.Enumerable.ToList<XFile>(SELF:xOtherFilesDict:Values)
            END GET
        END PROPERTY

        PROPERTY ParseOptions AS XSharpParseOptions
            GET
                //
                IF (SELF:_parseOptions == null)
                    //
                    IF (SELF:ProjectNode == null)
                        //
                        SELF:_parseOptions := XSharpParseOptions.Default
                    ELSE
                        //
                        SELF:_parseOptions := SELF:ProjectNode:ParseOptions
                    ENDIF
                ENDIF
                RETURN SELF:_parseOptions
            END GET
        END PROPERTY

        PROPERTY ProjectNode AS IXSharpProject GET SELF:_projectNode SET SELF:_projectNode := value

        PROPERTY ReferencedProjects AS System.Collections.Immutable.IImmutableList<XProject>
            GET
                //
                SELF:ResolveUnprocessedProjectReferences()
                RETURN System.Collections.Immutable.ImmutableList.ToImmutableList<XProject>(SELF:_ReferencedProjects)
            END GET
        END PROPERTY

        PROPERTY SourceFiles AS List<XFile>
            GET
                //
                RETURN System.Linq.Enumerable.ToList<XFile>(SELF:xSourceFilesDict:Values)
            END GET
        END PROPERTY

        PROPERTY StrangerProjects AS System.Collections.Immutable.IImmutableList<Project>
            GET
                //
                SELF:ResolveUnprocessedStrangerReferences()
                RETURN System.Collections.Immutable.ImmutableList.ToImmutableList<Project>(SELF:_StrangerProjects)
            END GET
        END PROPERTY


        public delegate OnFileWalkComplete(xFile AS XFile) as void

    END CLASS

END NAMESPACE 

