//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System
USING System.Collections.Generic
USING System.ComponentModel
USING System.Linq
USING System.Text
USING System.Threading.Tasks
USING LanguageService.CodeAnalysis
USING LanguageService.CodeAnalysis.XSharp
USING System.Collections.Concurrent
USING System.Diagnostics
USING System.Reflection


#pragma options ("az", ON)
BEGIN NAMESPACE XSharpModel
[DebuggerDisplay("{Name,nq}")];
CLASS XProject
#region Fields
    // Fields
    PROTECTED _id    := -1                    AS INT64
    PRIVATE _AssemblyReferences					AS List<XAssembly>
    PRIVATE _AssemblyDict					        AS XDictionary<INT64 ,XAssembly>
    PRIVATE _AssemblyTypeCache                    AS XDictionary<STRING, XPETypeSymbol>
    PRIVATE _parseOptions := NULL					AS XSharpParseOptions
    PRIVATE _projectNode							AS IXSharpProject
    PRIVATE _projectOutputDLLs					AS ConcurrentDictionary<STRING, STRING>
    PRIVATE _ReferencedProjects					AS List<XProject>
    PRIVATE _StrangerProjects						AS List<Object>
    PRIVATE _unprocessedAssemblyReferences		AS List<STRING>
    PRIVATE _unprocessedProjectReferences		AS List<STRING>
    PRIVATE _unprocessedStrangerProjectReferences	AS List<STRING>
    PRIVATE _failedStrangerProjectReferences   AS List<STRING>
    PRIVATE _OtherFilesDict					 AS XFileDictionary
    PRIVATE _SourceFilesDict					 AS XFileDictionary
    PRIVATE _FunctionClasses                   AS List<STRING>
    PRIVATE _ImplicitNamespaces                AS List<STRING>
    PRIVATE _dependentProjectList              AS STRING
    PRIVATE _dependentAssemblyList             AS STRING
    PRIVATE _name                              AS STRING
    PRIVATE _lastRefCheck                      AS DateTime

    PRIVATE _cachedAllNamespaces               AS IList<STRING>
    PRIVATE _cachedUsingStatics                AS IList<STRING>
    PUBLIC  FileWalkComplete				     AS XProject.OnFileWalkComplete
    PUBLIC  ProjectWalkComplete				 AS XProject.OnProjectWalkComplete

#endregion
#region Properties
    PROPERTY Id   AS INT64                     GET _id INTERNAL SET _id := value
    PROPERTY FileWalkCompleted                 AS LOGIC AUTO
    PROPERTY FileName                          AS STRING GET iif (_projectNode != null, _projectNode:Url, "")
    PROPERTY HasFiles                          AS LOGIC GET _SourceFilesDict:Keys:Count > 0 .or. _OtherFilesDict:Keys:Count > 0

    PROPERTY DependentAssemblyList             AS STRING
    GET
        IF String.IsNullOrEmpty(_dependentAssemblyList)
            SELF:_AssemblyDict := XDictionary<INT64, XAssembly>{}
            SELF:_AssemblyTypeCache  := XDictionary<STRING, XPETypeSymbol>{}
            VAR result := ""
            var core := SystemTypeController.mscorlib
            if core != null
                result := core:Id:ToString()
            ENDIF
            FOREACH VAR assembly IN SELF:AssemblyReferences:ToArray()
                _AssemblyDict:Add(assembly:Id, assembly)
                IF result:Length > 0
                    result += ","
                ENDIF
                result += assembly:Id:ToString()
            NEXT
            if core != NULL
                IF ! _AssemblyDict.ContainsKey(core:Id)
                    _AssemblyDict:Add(core:Id, core)
                    _AssemblyReferences:Add(core)
                endif
            endif
            _dependentAssemblyList := result
        ENDIF
        RETURN _dependentAssemblyList

    END GET
    END PROPERTY

    PROPERTY DependentProjectList              AS STRING
    GET
        IF String.IsNullOrEmpty(_dependentProjectList) .OR. ;
                (_dependentProjectList == SELF:Id:ToString() .AND. _ReferencedProjects:Count > 0)
            VAR result := ""
            result := SELF:Id:ToString()
            FOREACH VAR dependent IN _ReferencedProjects:ToArray()
                result += ","
                result += dependent:Id:ToString()
            NEXT
            _dependentProjectList := result
        ENDIF
        RETURN _dependentProjectList

    END GET
    END PROPERTY

    PROPERTY Dialect                           AS XSharpDialect
    GET
        TRY
            IF _projectNode != NULL
                RETURN _projectNode:Dialect
            ENDIF
            RETURN XSharpDialect.Core
        CATCH e AS Exception
            XSolution.WriteException(e,__FUNCTION__)
        END TRY
        RETURN XSharpDialect.Core

    END GET
    END PROPERTY
    PROPERTY FunctionClasses AS List<STRING>
    GET
        IF _FunctionClasses == NULL
            VAR result := List<STRING>{}
            result.Add(XLiterals.GlobalName)
            FOREACH VAR asm IN SELF:AssemblyReferences:ToArray()
                VAR gcn := asm:GlobalClassName
                IF !String.IsNullOrEmpty(gcn) .AND.  result:IndexOf(gcn) == -1
                    result:Add(gcn)
                ENDIF
            NEXT
            _FunctionClasses := result
        ENDIF
        RETURN _FunctionClasses
    END GET
    END PROPERTY

    PROPERTY ImplicitNamespaces AS List<STRING>
    GET
        IF _ImplicitNamespaces == NULL
            VAR result := List<STRING>{}
            IF SELF:ParseOptions:ImplicitNamespace
                FOREACH project AS XProject IN SELF:ReferencedProjects:ToArray()
                    VAR ns := project:ProjectNode?:ParseOptions?:DefaultNamespace
                    IF ! String.IsNullOrEmpty(ns) .AND. result:IndexOf(ns) == -1
                        result:Add(ns)
                    ENDIF
                NEXT
                FOREACH VAR asm IN SELF:AssemblyReferences:ToArray()
                    IF asm:IsXSharp
                        FOREACH VAR ns IN asm:ImplicitNamespaces
                            IF result:IndexOf(ns) == -1
                                result:Add(ns)
                            ENDIF
                        NEXT
                    ENDIF
                NEXT
            ENDIF
            _ImplicitNamespaces := result
        ENDIF
        RETURN _ImplicitNamespaces
    END GET
    END PROPERTY

    PROPERTY IncludeFiles as List<string>
    GET
        return XDatabase.GetProjectIncludeFiles(SELF)
    END GET
    END PROPERTY

    PROPERTY RootNamespace as STRING GET _projectNode:RootNameSpace



#endregion
    CONSTRUCTOR(project AS IXSharpProject)
        SUPER()
        SELF:_AssemblyReferences := List<XAssembly>{}
        SELF:_AssemblyTypeCache  := XDictionary<STRING, XPETypeSymbol>{}
        SELF:_unprocessedAssemblyReferences       := List<STRING>{}
        SELF:_unprocessedProjectReferences        := List<STRING>{}
        SELF:_unprocessedStrangerProjectReferences:= List<STRING>{}
        SELF:_failedStrangerProjectReferences     := List<STRING>{}
        SELF:_projectOutputDLLs := ConcurrentDictionary<STRING, STRING>{StringComparer.OrdinalIgnoreCase}
        SELF:_ReferencedProjects := List<XProject>{}
        SELF:_StrangerProjects := List<Object>{}
        SELF:_projectNode := project
        SELF:_SourceFilesDict   := XFileDictionary{SELF}
        SELF:_OtherFilesDict    := XFileDictionary{SELF}
        SELF:_name              := System.IO.Path.GetFileNameWithoutExtension(project:Url)
        SELF:_lastRefCheck      := DateTime.MinValue
        SELF:_cachedAllNamespaces   := NULL
        SELF:_cachedUsingStatics   := NULL

        SELF:Loaded := TRUE
        SELF:FileWalkCompleted := FALSE
        XSolution.Add(SELF)
        var cFile := XSolution.BuiltInFunctions
        IF ! String.IsNullOrEmpty(cFile) .and. System.IO.File.Exists(cFile)
            SELF:AddFile(cFile)
        ENDIF

    PUBLIC METHOD Close() AS VOID
        ModelWalker.RemoveProject(SELF)
        XSolution.Remove(SELF)
        _projectNode := NULL
        SELF:UnLoad()
        RETURN

    PRIVATE METHOD _clearTypeCache() AS VOID
        SELF:_ImplicitNamespaces    := NULL
        SELF:_dependentAssemblyList := NULL
        SELF:_cachedAllNamespaces   := NULL
        SELF:_cachedUsingStatics   := NULL
        SELF:_AssemblyTypeCache := NULL
        RETURN

#region AssemblyReferences

    METHOD RefreshReferences(asmList as IList<string>) AS VOID
        var oldAsm := XDictionary<string, string>{StringComparer.OrdinalIgnoreCase}
        var newAsm := List<string>{}
        SELF:LogReferenceMessage("RefreshReferences , old "+SELF:AssemblyReferenceNames:Count:ToString()+" new "+asmList:Count:ToString())
        foreach var name in SELF:AssemblyReferenceNames
            oldAsm:Add(name, name)
        next

        FOREACH var asmFile in asmList
            if oldAsm:ContainsKey(asmFile)
                oldAsm:Remove(asmFile)
            else
                newAsm:Add(asmFile)
            endif
        next
        FOREACH var item in oldAsm
            SELF:RemoveAssemblyReference(item:Value)
        NEXT
        FOREACH var item in newAsm
            SELF:AddAssemblyReference(item)
        NEXT
        RETURN

    METHOD AddAssemblyReference(path AS STRING) AS VOID
        IF ! String.IsNullOrEmpty(path)
            IF _AssemblyReferences:Any( { asm as XAssembly => asm:FileName:ToLower() == path.ToLower()} )
                RETURN
            endif
            SELF:LogReferenceMessage("AddAssemblyReference: "+path)
            SELF:_clearTypeCache()
            BEGIN LOCK _unprocessedAssemblyReferences
                IF ! _unprocessedAssemblyReferences:Contains(path)
                    _unprocessedAssemblyReferences.Add(path)
                ENDIF
            END LOCK
        ENDIF

    METHOD ClearAssemblyReferences() AS VOID
        SELF:LogReferenceMessage("ClearAssemblyReferences() ")
        SELF:_clearTypeCache()
        FOREACH VAR asm IN SELF:_AssemblyReferences:ToArray()
            asm:RemoveProject(SELF)
        NEXT
        SELF:_AssemblyReferences:Clear()

    METHOD LogReferenceMessage(message as string) AS VOID
        IF XSettings.EnableReferenceInfoLog
            SELF:WriteOutputMessage(message)
        ENDIF
    METHOD LogTypeMessage(message as string) AS VOID
        IF XSettings.EnableTypelookupLog
            SELF:WriteOutputMessage(message)
        ENDIF

    METHOD RemoveAssemblyReference(fileName AS STRING) AS VOID
        IF ! String.IsNullOrEmpty(fileName)
            SELF:_clearTypeCache()
            BEGIN LOCK _unprocessedAssemblyReferences
                IF _unprocessedAssemblyReferences:Contains(fileName)
                    SELF:LogReferenceMessage("RemoveAssemblyReference() "+fileName)
                    _unprocessedAssemblyReferences.Remove(fileName)
                ENDIF
            END LOCK
            FOREACH VAR asm IN SELF:_AssemblyReferences:ToArray()
                IF String.Equals(asm:FileName, fileName, System.StringComparison.OrdinalIgnoreCase)
                    SELF:LogReferenceMessage("RemoveAssemblyReference() "+fileName)
                    SELF:_AssemblyReferences:Remove(asm)
                    EXIT
                ENDIF
            NEXT
        ENDIF

    PRIVATE METHOD LoadReference(cDLL AS STRING) AS VOID
        IF ! String.IsNullOrEmpty(cDLL)
            VAR assemblyInfo := SystemTypeController.LoadAssembly(cDLL)
            SELF:_AssemblyReferences:Add(assemblyInfo)
            assemblyInfo:AddProject(SELF)
        ENDIF
        RETURN

    PRIVATE METHOD ResolveUnprocessedAssemblyReferences() AS VOID
        LOCAL loaded AS List<STRING>
        //
        IF SELF:_unprocessedAssemblyReferences:Count > 0 .AND. ! XSettings.DisableAssemblyReferences
            SELF:LogReferenceMessage("ResolveUnprocessedAssemblyReferences()")
            loaded := List<STRING>{}
            FOREACH path AS STRING IN SELF:_unprocessedAssemblyReferences:ToArray()
                IF System.IO.File.Exists(path)
                    SELF:LoadReference(path)
                    loaded:Add(path)
                ENDIF
            NEXT
            IF loaded:Count > 0
                FOREACH path AS STRING IN loaded
                    IF SELF:_unprocessedAssemblyReferences:Contains(path)
                        SELF:_unprocessedAssemblyReferences:Remove(path)
                    ENDIF
                NEXT
                SELF:_clearTypeCache()
            ENDIF
            XSolution.SetStatusBarText("")
        ENDIF
        RETURN

    PROPERTY RefCheckTimeOut as LOGIC
    GET
        var now := DateTime.Now
        LOCAL diff := now - SELF:_lastRefCheck as TimeSpan
        IF diff:TotalSeconds < 15
            RETURN FALSE
        ENDIF
        SELF:_lastRefCheck := now
        RETURN TRUE
    END GET
    END PROPERTY

    METHOD ResolveReferences() AS VOID
        IF SELF:hasUnprocessedReferences
            IF ! SELF:RefCheckTimeOut
                RETURN
            ENDIF
            SELF:LogReferenceMessage("<<-- ResolveReferences()")
            XSolution.SetStatusBarText(String.Format("Loading referenced types for project {0}", SELF:Name))

            TRY
                SELF:ResolveUnprocessedAssemblyReferences()
                SELF:ResolveUnprocessedProjectReferences()
                SELF:ResolveUnprocessedStrangerReferences()
                FOREACH DLL AS STRING IN SELF:_projectOutputDLLs:Values:ToArray()
                    VAR fullName := SystemTypeController.FindAssemblyByLocation(DLL)
                    LOCAL asm := NULL as XAssembly
                    if ! String.IsNullOrEmpty(fullName)
                        asm      := SystemTypeController.FindAssembly(fullName)
                    ENDIF
                    local lAdd := TRUE as LOGIC
                    IF asm != NULL .and. SELF:AssemblyReferences:Contains(asm)
                        lAdd := FALSE
                    ENDIF
                    IF lAdd
                        SELF:AddAssemblyReference(DLL)
                    ENDIF
                NEXT
                // repeat the assemblyreferences because we can have _projectOutputDLLs added to the list
                SELF:ResolveUnprocessedAssemblyReferences()
            CATCH
                NOP
            END TRY
            SELF:LogReferenceMessage(">>-- ResolveReferences()")
        ENDIF
        RETURN

    METHOD UpdateAssemblyReference(fileName AS STRING) AS VOID
        IF ! XSettings.DisableAssemblyReferences .AND. ! String.IsNullOrEmpty(fileName)
            SystemTypeController.LoadAssembly(fileName):AddProject(SELF)
        ENDIF

#endregion

#region ProjectReferences
    METHOD FindProjectReference(url AS STRING) AS XProject
        FOREACH VAR prj IN SELF:_ReferencedProjects
            IF String.Compare(prj:FileName, url, TRUE) == 0
                RETURN prj
            ENDIF
        NEXT
        RETURN NULL
    METHOD AddProjectReference(url AS STRING) AS LOGIC
        IF ! String.IsNullOrEmpty(url)
            VAR prj := SELF:FindProjectReference(url)
            IF (prj != NULL)
                // Project has already been added and has been resolved
                RETURN FALSE
            ENDIF
            SELF:_clearTypeCache()
            IF ! SELF:_unprocessedProjectReferences:Contains(url)
                SELF:LogReferenceMessage("Add XSharp ProjectReference "+url)
                SELF:_unprocessedProjectReferences:Add(url)
                RETURN TRUE
            ENDIF
        ENDIF
        RETURN FALSE

    METHOD AddProjectOutput(sProjectURL AS STRING, sOutputDLL AS STRING) AS VOID
        IF ! String.IsNullOrEmpty(sProjectURL) .AND. ! String.IsNullOrEmpty(sOutputDLL)
            SELF:LogReferenceMessage("AddProjectOutput "+sProjectURL+"("+sOutputDLL+")")
            IF SELF:_projectOutputDLLs:ContainsKey(sProjectURL)
                SELF:_projectOutputDLLs:Item[sProjectURL] := sOutputDLL
            ELSE
                SELF:_projectOutputDLLs:TryAdd(sProjectURL, sOutputDLL)
            ENDIF
            LOCAL found := FALSE AS LOGIC
            FOREACH VAR asm IN SELF:AssemblyReferences:ToArray()
                IF String.Equals(asm:FileName, sOutputDLL,StringComparison.OrdinalIgnoreCase)
                    found := TRUE
                    EXIT
                ENDIF
            NEXT
            IF ! found
                SELF:_unprocessedAssemblyReferences:Add(sOutputDLL)
            ENDIF
            SELF:_clearTypeCache()
        ENDIF

    METHOD RemoveProjectOutput(sProjectURL AS STRING) AS VOID
        IF ! String.IsNullOrEmpty(sProjectURL)
            IF SELF:_projectOutputDLLs:ContainsKey(sProjectURL)
                SELF:LogReferenceMessage("RemoveProjectOutput() "+sProjectURL)
                SELF:RemoveProjectReferenceDLL(SELF:_projectOutputDLLs:Item[sProjectURL])
                SELF:_projectOutputDLLs:TryRemove(sProjectURL, OUT VAR _)
            ENDIF
            SELF:_clearTypeCache()
        ENDIF

    METHOD RemoveProjectReference(url AS STRING) AS LOGIC
        IF ! String.IsNullOrEmpty(url)
            LOCAL prj AS XProject
            LOCAL outputname AS STRING
            SELF:_dependentProjectList  := ""
            SELF:_clearTypeCache()
            IF SELF:_unprocessedProjectReferences:Contains(url)
                SELF:LogReferenceMessage("RemoveProjectReference() "+url)
                SELF:_unprocessedProjectReferences:Remove(url)
                RETURN TRUE
            ENDIF
            // Does this url belongs to a project in the Solution ?
            prj := XSolution.FindProject(url)
            IF (SELF:_ReferencedProjects:Contains(prj))
                //
                outputname := prj:ProjectNode:OutputFile
                SELF:_ReferencedProjects:Remove(prj)
                SELF:_dependentProjectList  := ""
                RETURN TRUE
            ENDIF
            SELF:RemoveProjectOutput(url)
        ENDIF
        RETURN FALSE

    METHOD RemoveProjectReferenceDLL(DLL AS STRING) AS VOID
        IF ! String.IsNullOrEmpty(DLL)
            SELF:LogReferenceMessage("RemoveProjectReferenceDLL() "+DLL)
            SELF:_clearTypeCache()
            SELF:RemoveAssemblyReference(DLL)
        ENDIF

    PRIVATE METHOD ResolveUnprocessedProjectReferences() AS VOID
        LOCAL existing AS List<STRING>
        LOCAL p AS XProject
        LOCAL outputFile AS STRING
        IF SELF:_unprocessedProjectReferences:Count > 0  .AND. ! XSettings.DisableXSharpProjectReferences
            SELF:LogReferenceMessage("ResolveUnprocessedProjectReferences()")
            existing := List<STRING>{}
            FOREACH sProject AS STRING IN SELF:_unprocessedProjectReferences:ToArray()
                p := XSolution.FindProject(sProject)
                IF p != NULL
                    existing:Add(sProject)
                    SELF:_ReferencedProjects:Add(p)
                    outputFile := p:ProjectNode:OutputFile
                    SELF:AddProjectOutput(sProject, outputFile)
                ENDIF
            NEXT
            FOREACH sProject AS STRING IN existing
                SELF:_unprocessedProjectReferences:Remove(sProject)
            NEXT
            SELF:_clearTypeCache()
        ENDIF

#endregion

#region References TO other project systems
    METHOD AddStrangerProjectReference(url AS STRING) AS LOGIC
        IF ! String.IsNullOrEmpty(url)
            SELF:LogReferenceMessage("Add Foreign ProjectReference"+url)
            IF ! SELF:_unprocessedStrangerProjectReferences:Contains(url)
                SELF:_unprocessedStrangerProjectReferences:Add(url)
                RETURN TRUE
            ENDIF
        ENDIF
        RETURN FALSE

    PRIVATE METHOD GetStrangerOutputDLL(sProject AS STRING, p AS Dynamic) AS STRING
        VAR outputFile := ""
        TRY
            VAR properties := p:Properties
            VAR propType := properties:Item("OutputType")
            VAR propName := properties:Item("AssemblyName")
            VAR propPath := p:ConfigurationManager:ActiveConfiguration:Properties:Item( "OutputPath")
            IF propName != NULL .AND. propPath != NULL .AND. propType != NULL

                VAR path    := (STRING) propPath:Value
                VAR type    := (INT)    propType:Value
                outputFile  := (STRING) propName:Value
                IF type == 2 // __VSPROJOUTPUTTYPE.VSPROJ_OUTPUTTYPE_LIBRARY, in Microsoft.VisualStudio.Shell.Interop.11.0.dll
                    outputFile	+= ".dll"
                ELSE
                    outputFile	+= ".exe"
                ENDIF
                outputFile	:= System.IO.Path.Combine(path, outputFile)
                IF ! System.IO.Path.IsPathRooted(outputFile)
                    outputFile := System.IO.Path.Combine(System.IO.Path.GetDirectoryName(sProject), outputFile)
                    // remove ../ and other garbage from the path
                    outputFile := System.IO.Path.GetFullPath(outputFile)
                ENDIF
            ENDIF
        CATCH exception AS Exception
            XSolution.WriteException(exception,__FUNCTION__)
        END TRY
        RETURN outputFile


    METHOD RemoveStrangerProjectReference(url AS STRING) AS LOGIC
        IF ! String.IsNullOrEmpty(url)

            SELF:_clearTypeCache()
            IF SELF:_unprocessedStrangerProjectReferences:Contains(url)
                SELF:LogReferenceMessage("RemoveStrangerProjectReference() "+url)
                SELF:_unprocessedStrangerProjectReferences:Remove(url)
                RETURN TRUE
            ENDIF
            IF SELF:_failedStrangerProjectReferences:Contains(url)
                SELF:LogReferenceMessage("RemoveStrangerProjectReference() "+url)
                SELF:_failedStrangerProjectReferences:Remove(url)
                RETURN TRUE
            ENDIF

            SELF:RemoveProjectOutput(url)
            var prj := SELF:ProjectNode:FindProject(url)
            IF prj != NULL .AND. SELF:_StrangerProjects:Contains(prj)
                SELF:_StrangerProjects:Remove(prj)
                RETURN TRUE
            ENDIF
        ENDIF
        RETURN FALSE

    METHOD RefreshStrangerProjectDLLOutputFiles_Worker AS VOID
        FOREACH proj as Dynamic IN SELF:_StrangerProjects:ToArray()
            LOCAL sProjectURL := proj:FullName AS STRING
            VAR mustAdd     := FALSE
            LOCAL outputFile  := SELF:GetStrangerOutputDLL(sProjectURL, proj) AS STRING
            IF SELF:_projectOutputDLLs:ContainsKey(sProjectURL)
                // when the output file name of the referenced project has changed
                // then remove the old name
                IF outputFile:ToLower() != SELF:_projectOutputDLLs:Item[sProjectURL]:ToLower()
                    SELF:LogReferenceMessage(i"DLL has been renamed to {outputFile} so remove the old DLL {SELF:_projectOutputDLLs:Item[sProjectURL]}")
                    SELF:_projectOutputDLLs:TryRemove(sProjectURL, OUT VAR _)
                    mustAdd := TRUE
                ENDIF
            ELSE
                mustAdd := TRUE
            ENDIF
            IF mustAdd
                SELF:AddStrangerProjectReference(sProjectURL)
            ENDIF
        NEXT
        RETURN


    PRIVATE METHOD RefreshStrangerProjectDLLOutputFiles() AS VOID
        // Check if any DLL has changed
        IF SELF:_StrangerProjects:Count > 0 .AND. ! XSettings.DisableForeignProjectReferences .and. SELF:RefCheckTimeOut
            SELF:LogReferenceMessage("--> RefreshStrangerProjectDLLOutputFiles() "+SELF:_StrangerProjects:Count():ToString())
            _projectNode:RunInForeGroundThread ( { => SELF:RefreshStrangerProjectDLLOutputFiles_Worker() })
            SELF:LogReferenceMessage("<-- RefreshStrangerProjectDLLOutputFiles()")
        ENDIF

    METHOD ResolveUnprocessedStrangerReferences_Worker AS VOID
        LOCAL existing AS List<STRING>
        LOCAL outputFile AS STRING
        SELF:LogReferenceMessage("ResolveUnprocessedStrangerReferences()" +_unprocessedStrangerProjectReferences:Count:ToString())
        existing := List<STRING>{}
        FOREACH sProject AS STRING IN SELF:_unprocessedStrangerProjectReferences:ToArray()
            VAR p := SELF:ProjectNode:FindProject(sProject)
            IF (p != NULL)
                SELF:_StrangerProjects:Add(p)
                outputFile := SELF:GetStrangerOutputDLL(sProject, p)
                IF !String.IsNullOrEmpty(outputFile)
                    SELF:AddProjectOutput(sProject, outputFile)
                ELSE
                    IF ! SELF:_failedStrangerProjectReferences:Contains(sProject)
                        SELF:_failedStrangerProjectReferences:Add(sProject)
                    ENDIF
                ENDIF
                // Always remove the project, to prevent endless retries
                existing:Add(sProject)
            ENDIF
        NEXT
        FOREACH sProject AS STRING IN existing
            IF SELF:_unprocessedStrangerProjectReferences:Contains(sProject)
                SELF:_unprocessedStrangerProjectReferences:Remove(sProject)
            ENDIF
        NEXT
    PRIVATE METHOD ResolveUnprocessedStrangerReferences() AS VOID
        IF SELF:_unprocessedStrangerProjectReferences:Count > 0 .AND. ! XSettings.DisableForeignProjectReferences
            _projectNode:RunInForeGroundThread ( { => SELF:ResolveUnprocessedStrangerReferences_Worker() })

            SELF:_clearTypeCache()
        ENDIF

#endregion

#region 'Normal' Files

    METHOD ZapFiles() AS VOID
        SELF:_SourceFilesDict:Clear()
        SELF:_OtherFilesDict:Clear()
    METHOD AddFile(filePath AS STRING) AS LOGIC
        LOCAL xamlCodeBehindFile AS STRING
        SELF:WriteOutputMessage(i"AddFile {filePath}")
        // DO NOT read the file ID from the database here.
        // This is called during startup of the solution, we try to do as little as possible
        VAR type := XFileTypeHelpers.GetFileType(filePath)
        IF type == XFileType.SourceCode
            SELF:_SourceFilesDict:Add(filePath)
        ELSEIF type == XFileType.XAML
            VAR xFile := XFile{filePath, SELF}
            xamlCodeBehindFile := xFile:XamlCodeBehindFile
            SELF:_SourceFilesDict:Add(xamlCodeBehindFile)
            SELF:_OtherFilesDict:Add(filePath)
        ELSE
            SELF:_OtherFilesDict:Add(filePath)
        ENDIF
        IF SELF:Name != OrphanedFilesProject.OrphanName .and. XSolution.OrphanedFilesProject != NULL
            if filePath != XSolution.BuiltInFunctions
                var xFile := XSolution.OrphanedFilesProject:FindXFile(filePath)
                if xFile != NULL
                    XSolution.OrphanedFilesProject:RemoveFile(filePath)
                    xFile:Project := SELF
                ENDIF
            ENDIF
        ENDIF
        RETURN TRUE



    METHOD FindXFile(fullPath AS STRING) AS XFile
        IF ! String.IsNullOrEmpty(fullPath)
            VAR file := SELF:_SourceFilesDict:Find(fullPath)
            IF file == NULL
                file := SELF:_OtherFilesDict:Find(fullPath)
            ENDIF
            RETURN file
        ENDIF
        RETURN NULL


    METHOD RemoveFile(url AS STRING) AS VOID
        IF ! String.IsNullOrEmpty(url)
            SELF:WriteOutputMessage(i"RemoveFile {url}")
            IF SELF:_OtherFilesDict:Remove(url)
                VAR file := XFile{url, SELF}
                IF file:IsXaml
                    url := file:XamlCodeBehindFile
                ENDIF
            ENDIF
            SELF:_SourceFilesDict:Remove( url)
            XDatabase.DeleteFile(url)
        ENDIF
#endregion

#region Lookup Types and Functions


    METHOD FindGlobalsInAssemblyReferences(name AS STRING, lLike := FALSE as LOGIC) AS IList<IXMemberSymbol>
        LogTypeMessage(i"FindGlobalsInAssemblyReferences {name} {lLike}")
        var dbresult := XDatabase.FindAssemblyGlobalOrDefine(name, SELF:DependentAssemblyList, lLike)
        var result := SELF:_MembersFromGlobalType(dbresult)
        LogTypeMessage(i"FindGlobalsInAssemblyReferences {name}, found {result.Count} occurences")
        RETURN result

     METHOD FindFunctionsInAssemblyReferences(name AS STRING, lLike := FALSE as LOGIC) AS IList<IXMemberSymbol>
        LogTypeMessage(i"FindFunctionsInAssemblyReferences {name} {lLike} ")
        var dbresult := XDatabase.FindAssemblyFunction(name, SELF:DependentAssemblyList, lLike)
        var result := SELF:_MembersFromGlobalType(dbresult)
        LogTypeMessage(i"FindFunctionsInAssemblyReferences {name}, found {result.Count} occurences")
        RETURN result


    PRIVATE METHOD _MembersFromGlobalType(dbresult as IList<XDbResult>) AS IList<IXMemberSymbol>
        VAR result := List<IXMemberSymbol>{}
        foreach var item in dbresult
            if _AssemblyDict:TryGetValue(item:IdAssembly, out var asm)
                IF asm:GlobalMembers:TryGetValue(item:MemberName, out var pem)

                    result:Add(pem)
                endif
            endif
        next
        RETURn result

    METHOD FindGlobalMembersLike(name AS STRING, lCurrentProject AS LOGIC) AS IList<IXMemberSymbol>
        LogTypeMessage(i"FindGlobalMembersLike {name} Current Project {lCurrentProject}")
        VAR result := List<IXMemberSymbol>{}
        var projList := ""
        if lCurrentProject
            projList := SELF:Id:ToString()
        ELSE
            projList := SELF:DependentProjectList
        ENDIF
        var dbresult := XDatabase.FindProjectGlobalOrDefineLike(name, projList)
        LOCAL xFile := null as XFile
        LOCAL cFile := "" AS STRING
        foreach element as XDbResult in dbresult
            if element:FileName != cFile
                xFile := XSolution.FindFile(element:FileName)
                cFile := element:FileName
            endif
            var xmember := XSourceMemberSymbol{element, xFile}
            result:Add(xmember)
        next
        LogTypeMessage(i"FindGlobalMembersLike {name}, found {result.Count} occurences")
        RETURN result

    METHOD FindFunctionsLike(name AS STRING, lCurrentProject AS LOGIC) AS IList<IXMemberSymbol>
        LogTypeMessage(i"FindFunctionsLike {name} Current Project {lCurrentProject}")
        VAR result := List<IXMemberSymbol>{}
        var projList := ""
        if lCurrentProject
            projList := SELF:Id:ToString()
        ELSE
            projList := SELF:DependentProjectList
        ENDIF
        var dbresult := XDatabase.FindFunctionLike(name, projList)
        LOCAL xFile := NULL as XFile
        LOCAL cFile := "" AS STRING
        foreach element as XDbResult in dbresult
            if element:FileName != cFile
                xFile := XSolution.FindFile(element:FileName)
                cFile := element:FileName
            endif
            var xmember := XSourceMemberSymbol{element, xFile}
            result:Add(xmember)
        next
        LogTypeMessage(i"FindFunctionsLike {name}, found {result.Count} occurences")
        RETURN result


    METHOD FindFunction(name AS STRING, lRecursive := TRUE AS LOGIC) AS IXMemberSymbol
        // we look in the project references and assembly references
        // pass the list of ProjectIds and AssemblyIds to the database engine
        LogTypeMessage(ie"FindFunction {name} ")

        VAR projectIds    := SELF:Id:ToString()
        IF lRecursive
            projectIds    := SELF:DependentProjectList
        ENDIF
        VAR result := XDatabase.FindFunction(name, projectIds)
        VAR xmember := GetGlobalMember(result)
        LogTypeMessage(ie"FindFunction {name}, result {iif (xmember != NULL, xmember.FullName, \"not found\"} ")
        RETURN xmember

    METHOD FindGlobalOrDefine(name AS STRING, lRecursive := TRUE AS LOGIC) AS IXMemberSymbol
        // we look in the project references and assembly references
        // pass the list of ProjectIds and AssemblyIds to the database engine
        LogTypeMessage(ie"FindGlobalOrDefine {name}")
        VAR projectIds    := SELF:Id:ToString()
        IF lRecursive
            projectIds    := SELF:DependentProjectList
        ENDIF
        VAR result := XDatabase.FindProjectGlobalOrDefine(name, projectIds)
        VAR xmember := GetGlobalMember(result)
        LogTypeMessage(ie"FindGlobalOrDefine {name}, result {iif (xmember != NULL, xmember.FullName, \"not found\"} ")
        RETURN xmember

    PRIVATE METHOD GetGlobalMember(result AS IList<XDbResult>) AS IXMemberSymbol
        IF result:Count > 0
            // Get the source code and parse it into a member
            // we know that it will be of the globals class
            // the global class that will be shared by the functions/globals will only
            // contain the members from the result collection
            LOCAL source AS STRING
            LOCAL file := NULL AS XFile
            VAR members := List<XSourceMemberSymbol>{}
            source := ""
            FOREACH VAR element IN result
                VAR xfile    := SELF:FindXFile(element:FileName)
                IF xfile == NULL
                    xfile := XSolution.FindFullPath(element:FileName)
                ENDIF
                if (xfile != null)
                    file := xfile
                    VAR xmember := XSourceMemberSymbol{element, xfile}
                    source += element:SourceCode+Environment.NewLine
                    members:Add(xmember)
                    if xmember:File != null
                        file := xmember:File
                    endif
                endif
            NEXT
            VAR walker := SourceWalker{file, FALSE}
            walker:Parse(source)
            VAR first := (XSourceMemberSymbol) walker:EntityList:First()
            VAR parentType := (XSourceTypeSymbol) first:ParentType
            VAR entities := List<XSourceMemberSymbol>{}
            FOR VAR i := 0 TO walker:EntityList:Count -1
                VAR entity := walker:EntityList[i]
                IF entity IS XSourceMemberSymbol VAR xsms
                    entities:Add(xsms)
                    IF i < result:Count
                        // copy location and xml comments
                        xsms:CopyValuesFrom(result[i])
                    ENDIF
                ENDIF
            NEXT
            parentType:SetMembers(entities)
            RETURN first
        ENDIF
        RETURN NULL

    METHOD GetProjectTypesInNamespace(namespace AS STRING, usings AS IList<STRING>) AS IList<XSourceTypeSymbol>
        if namespace.EndsWith(".")
            namespace := namespace.Substring(0, namespace.Length-1)
        ENDIF
        VAR result := XDatabase.GetProjectTypesInNamespace(namespace, SELF:DependentProjectList )
        // convert the database objects to the SourceTypeSymbols
        return GetSourceTypes(result)


    METHOD GetAssemblyTypesInNamespaceLike(namespace AS STRING, usings AS IList<STRING>) AS IList<XPETypeSymbol>
        if namespace.EndsWith(".")
            namespace := namespace.Substring(0, namespace.Length-1)
        ENDIF
        var myUsings := usings.ToList()
        myUsings:Add(namespace)
        VAR result := XDatabase.GetAssemblyTypesInNamespace(namespace, SELF:DependentAssemblyList )
        // convert the database objects to the PeTypeSymbols
        result := FilterUsings(result, myUsings,"",false)
        RETURN GetPETypes(result)

    METHOD GetAssemblyTypesLike(startWith AS STRING, usings AS IList<STRING>) AS IList<XPETypeSymbol>
        VAR result := XDatabase.GetAssemblyTypesLike(startWith, SELF:DependentAssemblyList )
        // convert the database objects to the PeTypeSymbols
        result := FilterUsings(result, usings,startWith,true)
        RETURN GetPETypes(result)

    PRIVATE METHOD FindAssemblyById(IdAssembly as INT64) AS XAssembly

        if _AssemblyDict:ContainsKey(IdAssembly)
            RETURN _AssemblyDict[IdAssembly]
        ENDIF
        return NULL

    PRIVATE METHOD GetPETypes(found AS IList<XDbResult>) AS IList<XPETypeSymbol>
        LOCAL IdAssembly := -1 AS INT64
        LOCAL fullTypeName:= ""  AS STRING
        VAR result := List<XPETypeSymbol>{}
        LOCAL lastAsm := NULL as XAssembly
        FOREACH VAR element IN found
            // Skip types found in another project
            // we do not use the fullname because for generics that ends with <T> and not `1
            if String.IsNullOrEmpty(element:Namespace)
                fullTypeName := element:TypeName
            else
                fullTypeName := element:Namespace+"."+element:TypeName
            endif
            IF SELF:GetTypeFromCache(fullTypeName, OUT VAR peTypeFound)
                result:Add(peTypeFound)
                LOOP
            endif
            IdAssembly   := element:IdAssembly
            if (lastAsm == null .or. lastAsm:Id != IdAssembly)
                lastAsm := FindAssemblyById(IdAssembly)
            ENDIF
            if lastAsm != null .and. lastAsm:Types:ContainsKey(fullTypeName)
                var peType := lastAsm:Types[fullTypeName]
                result:Add(peType)
                IF SELF:_AssemblyTypeCache != NULL
                    IF !SELF:GetTypeFromCache(fullTypeName, OUT NULL)
                        SELF:_AssemblyTypeCache:Add(fullTypeName, peType)
                    endif
                ENDIF
            ENDIF
        NEXT
        RETURN result

    METHOD GetTypeFromCache(typeName AS STRING, result OUT XPETypeSymbol) AS LOGIC
        result := NULL
        RETURN SELF:_AssemblyTypeCache != NULL .and. SELF:_AssemblyTypeCache:TryGetValue(typeName, out result)

    METHOD FindSystemType(name AS STRING, usings AS IList<STRING>) AS XPETypeSymbol
        LogTypeMessage("FindSystemType() "+name)
        IF ! XSettings.DisableForeignProjectReferences
            SELF:RefreshStrangerProjectDLLOutputFiles()
        ENDIF
        SELF:ResolveReferences()
        IF GetTypeFromCache(name, out var petype)
            return petype
        endif
        // lookup the type in the Database now
        var result := XDatabase.GetAssemblyTypes(name, SELF:DependentAssemblyList)
        result     := FilterUsings(result, usings, name, FALSE)
        var peTypes := GetPETypes(result)
        LOCAL type as XPETypeSymbol
        type := peTypes:FirstOrDefault()
        IF type != NULL
            LogTypeMessage("FindSystemType() "+name+" found "+type:FullName)
        ELSE
            LogTypeMessage("FindSystemType() "+name+" not found ")
        ENDIF
        RETURN type

    PROPERTY AssemblyNamespaces AS IList<STRING>
    GET
        SELF:ResolveReferences()
        RETURN XDatabase.GetAssemblyNamespaces(SELF:DependentAssemblyList)
    END GET
    END PROPERTY


    METHOD GetCommentTasks() AS IList<XCommentTask>
        VAR tasks := XDatabase.GetCommentTasks(SELF:Id:ToString())
        LOCAL oFile AS XFile
        VAR result := List<XCommentTask>{}
        FOREACH VAR item IN tasks
            IF oFile == NULL .OR. oFile:FullPath != item:FileName
                oFile := XFile{item:FileName, SELF}
            ENDIF
            VAR task := XCommentTask{}
            task:File := oFile
            task:Line := item:Line
            task:Column := item:Column
            task:Priority := item:Priority
            task:Comment := item:Comment
            result:Add(task)
        NEXT
        RETURN result


    PRIVATE METHOD AdjustUsings(typeName REF STRING, usings AS IList<STRING>) AS IList<STRING>
        VAR pos := typeName:LastIndexOf(".")
        VAR myusings := HashSet<string>{StringComparer.OrdinalIgnoreCase}
        // when we have a fully qualified typename then add that to the start of the list
        IF pos > 0
            VAR ns   := typeName:Substring(0,pos)
            myusings:Add(ns)
            typeName := typeName:Substring(pos+1)
        ENDIF
        myusings:Add(SELF:RootNamespace)
        myusings:AddRange(usings)
        myusings:AddRange(SELF:ImplicitNamespaces)
        RETURN myusings:ToList()


    PRIVATE _lastFound := NULL AS XSourceTypeSymbol
    PRIVATE _lastName  := NULL AS STRING

    METHOD GetTypesLike( startWith AS STRING, usings AS IList<STRING>) AS IList<XSourceTypeSymbol>
        VAR result := XDatabase.GetProjectTypesLike(startWith, SELF:DependentProjectList)
        result := FilterUsings(result,usings,startWith,TRUE)
        RETURN SELF:GetSourceTypes(result)

    METHOD ClearCache(file as XFile) AS VOID
        IF SELF:_lastFound != NULL
            SELF:_lastFound := NULL
            SELF:_lastName  := NULL
        ENDIF

    METHOD FindType(typeName as STRING, usings AS IList<STRING>) AS IXTypeSymbol
        LOCAL result as IXTypeSymbol
        local systemFirst as LOGIC
        typeName := typeName:GetSystemTypeName(ParseOptions:XSharpRuntime)
        if SELF:GetTypeFromCache(typeName, OUT VAR petype)
            return petype
        ENDIF
        systemFirst := typeName.StartsWith("XSharp." ) .or. typeName.StartsWith("System.")
        if (systemFirst)
            result := SELF:FindSystemType(typeName, usings)
            if result == NULL
                result := SELF:Lookup(typeName, usings)
            endif

        else
            result := SELF:Lookup(typeName, usings)
            if result == NULL
                result := SELF:FindSystemType(typeName, usings)
            ENDIF
        endif
        RETURN result

    METHOD Lookup(typeName AS STRING) AS XSourceTypeSymbol
        VAR usings := List<STRING>{}
        RETURN Lookup(typeName, usings)

    METHOD Lookup(typeName AS STRING, usings AS IList<STRING>) AS XSourceTypeSymbol
        // lookup Type definition in this project and X# projects referenced by this project
        LogTypeMessage(i"Lookup {typeName}")
        VAR originalName := typeName
        IF originalName == _lastName .and. _lastFound != null
            RETURN _lastFound
        ENDIF
        usings := AdjustUsings(REF typeName, usings)
        VAR pos := typeName:IndexOf('<')
        IF pos > 0
            typeName := typeName:Substring(0, pos)
        ENDIF
        VAR result  := XDatabase.GetProjectTypes(typeName, SELF:DependentProjectList)
        result      := FilterUsings(result,usings, typeName, FALSE)

        var tmp  := GetType(result)
        // RvdH make sure that the full typename matches if we are looking with a fully qualified type
        // So when looking for the Type Foo.SomeType (defined in an external assembly) we should not return
        // Bar.SomeType (defined in code).

        if tmp != null .and. originalName:Contains(".") .and. tmp:FullName != originalName
            tmp := NULL
        endif
        _lastFound  := tmp
        _lastName   := originalName
        LogTypeMessage(ie"Lookup {typeName}, result {iif(_lastFound != NULL, _lastFound.FullName, \"not found\" } ")

        RETURN _lastFound


    METHOD FilterUsings(list AS IList<XDbResult> , usings AS IList<STRING>, typeName AS STRING, partial AS LOGIC) AS IList<XDbResult>
        VAR result := List<XDbResult>{}
        VAR checkCase := SELF:ParseOptions:CaseSensitive
        usings := AdjustUsings(REF typeName, usings)
        FOREACH VAR element IN list
            IF checkCase
                IF partial
                    IF !element:TypeName:StartsWith(typeName)
                        LOOP
                    ENDIF
                ELSE
                    IF element:TypeName != typeName
                        LOOP
                    ENDIF
                ENDIF
            ENDIF
            IF String.IsNullOrEmpty(element:Namespace)
                result:Add(element)
            ELSE
                FOREACH VAR using IN usings
                    // when we have USING System.IO we want to include types in System.IO and System.
                    // so we include when the USING starts with the namespace of the type
                    IF using:StartsWith(element:Namespace, StringComparison.OrdinalIgnoreCase)
                        result:Add(element)
                        EXIT
                    ENDIF
                NEXT
            ENDIF
        NEXT
        RETURN result

    PRIVATE METHOD GetType(found AS IList<XDbResult>) AS XSourceTypeSymbol
        if found:Count == 0
            RETURN NULL
        ENDIF
        LOCAL idProject := -1 AS INT64
        LOCAL fullTypeName:= ""  AS STRING
        LOCAL namespace := "" AS STRING
        LOCAL sTypeIds := ""  as STRING
        LOCAL aFiles   := XDictionary<INT64, XFile>{} AS XDictionary<INT64, XFile>
        LOCAL cXmlComment as STRING
        LOCAL projectIds as STRING
        local interfaces as STRING
        local baseTypeName as STRING
        projectIds := ","+self:DependentProjectList+","
        interfaces := ""
        FOREACH var element in found
            var id := ","+element:IdProject.ToString()+","
            IF projectIds.IndexOf(id) >= 0
                IF sTypeIds:Length > 0
                    sTypeIds += ", "
                ENDIF
                sTypeIds += element:IdType:ToString()
                if ! String.IsNullOrEmpty(element:XmlComments)
                    cXmlComment := element:XmlComments
                ENDIF
                var pos := element:SourceCode.ToLower().IndexOf("implements ")
                if pos > 0
                    interfaces += element:SourceCode:Substring(pos +10).Trim()+","
                endif
                if ! String.IsNullOrEmpty(element:BaseTypeName)
                    baseTypeName := element:BaseTypeName
                endif
            ENDIF
        NEXT
        IF sTypeIds:Length == 0
            RETURN NULL
        ENDIF
        interfaces := interfaces:Replace(";","")
        interfaces := interfaces:Replace("\r","")
        interfaces := interfaces:Replace("\n","")
        VAR aIF := interfaces.Split(<CHAR>{c','}, StringSplitOptions.RemoveEmptyEntries)
        //todo Collect interfaces from IMPLEMENTS clauses
        VAR members  := XDatabase.GetMembers(sTypeIds):ToArray()
        VAR oType   := found[0]
        var project := XSolution.FindProject(oType:Project)
        VAR file    := project:GetFileById(oType:IdFile)
        IF file == null
            file := project:FindXFile(oType:FileName)
        ENDIF
        IF file == NULL
            RETURN NULL
        ENDIF
        VAR source  := XSourceTypeSymbol.GetTypeSource(oType, members, file)
        aFiles:Add(oType:IdFile, file)
        VAR walker        := SourceWalker{file, FALSE}
        walker:SaveToDisk := FALSE
        walker:Parse(source) // we are not interested in locals but we also do not want to update the database here
        IF walker:EntityList:Count > 0
            namespace      := oType:Namespace
            fullTypeName   := oType:Namespace+"."+oType:TypeName
            idProject      := oType:IdProject
            VAR name       := oType:TypeName
            VAR xElement      := walker:EntityList:First()
            LOCAL xtype AS XSourceTypeSymbol
            IF xElement IS XSourceTypeSymbol VAR xE
                xtype := xE
            ELSEIF xElement IS XSourceMemberSymbol VAR xE2
                xtype := (XSourceTypeSymbol) xE2:ParentType
            ENDIF
            IF xtype != NULL
                xtype:SetInterfaces(aIF)
                xtype:BaseTypeName    := baseTypeName
                xtype:CopyValuesFrom(oType)
                xtype:Namespace   := namespace
                xtype:ClassType   := (XSharpDialect) oType:ClassType
                VAR xmembers := xtype:XMembers:ToArray()
                VAR dict := XDictionary<STRING, IList<XSourceMemberSymbol>>{}
                FOREACH m as XSourceMemberSymbol in xmembers
                    var key := m:Kind:ToString()+" "+m:Name
                    if ! dict:ContainsKey(key)
                        dict:Add(key, List<XSourceMemberSymbol>{})
                    ENDIF
                    dict[key]:Add(m)
                NEXT
                LOCAL i AS INT
                FOR i := 0 TO members:Length-1
                    LOCAL melement  := members[i] as XDbResult
                    var key := melement:Kind:ToString()+" "+melement:MemberName
                    if dict:ContainsKey(key)
                        var list := dict[key]
                        if list:Count > 0
                            var xmember := list:First()
                            if list:Count > 1
                                foreach var m in list
                                    if EqualSourceCode(m, melement)
                                        xmember := m
                                        exit
                                    endif
                                next
                            endif
                            melement:UpdateLocation(xmember)
                            if aFiles:ContainsKey(melement:IdFile)
                                xmember:File         := aFiles[melement:IdFile]
                            ELSE
                                file := SELF:GetFileById(melement:IdFile)
                                aFiles:Add(melement:IdFile, file)
                                xmember:File         := file
                            ENDIF
                        ELSE
                            NOP
                        ENDIF
                    ELSE
                        NOP
                    ENDIF
                NEXT

                return xtype
            ENDIF

        ENDIF
        RETURN NULL

    PRIVATE METHOD EqualSourceCode(mlhs as XSourceMemberSymbol, mrhs as XDbResult) AS LOGIC
        if mlhs:SourceCode == mrhs:SourceCode
            return true
        endif
        if mlhs:Visibility != mrhs:Visibility
            return false
        endif
        var lhs := mlhs:SourceCode:ToLower()
        var rhs := mrhs:SourceCode:ToLower()
        if mlhs:Visibility == Modifiers.Hidden
            if lhs:StartsWith("private")
                lhs := lhs:Substring(7):Trim()
            elseif lhs:StartsWith("hidden")
                lhs := lhs:Substring(6):Trim()
            endif
        elseif mlhs:Visibility == Modifiers.Export
            if lhs:StartsWith("export")
                lhs := lhs:Substring(6):Trim()
            elseif lhs:StartsWith("public")
                lhs := lhs:Substring(6):Trim()
            endif
        endif
        if mrhs:Visibility == Modifiers.Hidden
            if rhs:StartsWith("private")
                rhs := rhs:Substring(7):Trim()
            elseif rhs:StartsWith("hidden")
                rhs := rhs:Substring(6):Trim()
            endif
        elseif mrhs:Visibility == Modifiers.Export
            if rhs:StartsWith("export")
                rhs := rhs:Substring(6):Trim()
            elseif rhs:StartsWith("public")
                rhs := rhs:Substring(6):Trim()
            endif
        endif
        return lhs == rhs


    PRIVATE METHOD GetSourceTypes(found AS IList<XDbResult>) AS IList<XSourceTypeSymbol>
        VAR result        := List<XSourceTypeSymbol>{}
        LOCAL idProject   := -1 AS INT64
        LOCAL fullTypeName:= ""  AS STRING
        FOREACH VAR element IN found
            // Skip types found in another project
            IF idProject != -1 .AND. element:IdProject != idProject
                LOOP
            ENDIF
            // skip types from different namespaces
            IF fullTypeName:Length > 0 .AND. fullTypeName != element:Namespace+"."+element:TypeName
                LOOP
            ENDIF
            VAR file       := XSolution.FindFullPath(element:FileName)
            if (file != null)
                file:Virtual   := TRUE
                file:Id        := element:IdFile
                VAR xtype := XSourceTypeSymbol{element, file}
                result:Add(xtype)
            endif
        NEXT
        RETURN result

    PRIVATE METHOD GetExtensionMethodsPerFile(list as IList<XDbResult>, oFile as XFile) AS IList<IXMemberSymbol>
        VAR entities := List<IXMemberSymbol>{}
        local sb as StringBuilder
        local sLastType := "" as STRING

        sb := StringBuilder{}
        FOREACH VAR element IN list
            if sLastType != element:TypeName
                if ! String.IsNullOrEmpty(sLastType)
                    sb:AppendLine("END CLASS")
                endif
                sb:AppendLine("PARTIAL STATIC CLASS "+element:TypeName)
                sLastType := element:TypeName
            endif
            sb:AppendLine(element:SourceCode)

        NEXT
        sb:AppendLine("END CLASS")
        VAR walker := SourceWalker{oFile, FALSE}
        walker:Parse(sb:ToString())
        foreach var entity in walker:EntityList
            if entity is XSourceMemberSymbol var sms
                entities:Add(sms)
            endif
        next
        if entities:Count == list:Count
            FOR var i := 0 to entities:Count-1
                var entity := (XSourceMemberSymbol) entities[i]
                var dbres  := list[i]
                dbres:UpdateLocation(entity)
            next
        endif
        RETURN entities

    PRIVATE METHOD GetExtensionMethods(list as IList<XDbResult>) AS IList<IXMemberSymbol>
        VAR entities := List<IXMemberSymbol>{}
        var fileNames := List<String>{}
        if list:Count > 0
            foreach element as XDbResult in list
                if !fileNames:Contains(element:FileName)
                    fileNames:Add(element:FileName)
                endif
            next
            foreach var fileName in fileNames
                var entitiesPerFile := List<XDbResult>{}
                foreach element as XDbResult in list:ToArray()
                    if element:FileName == fileName
                        entitiesPerFile:Add(element)
                        list:Remove(element)
                    endif
                next
                var oFile := SELF:FindXFile(fileName)
                entities:AddRange(GetExtensionMethodsPerFile(entitiesPerFile, oFile))
            next
        endif
        return entities



    METHOD GetExtensions( typeName AS STRING) AS IList<IXMemberSymbol>
        local result := List<IXMemberSymbol>{} as List<IXMemberSymbol>
        local type as IXTypeSymbol
        local names as List<String>
        local usings as List<String>
        usings := List<String>{}
        names := List<String>{}
        names:Add(typeName)
        type := SELF:FindType(typeName, usings)
        if type != null
            foreach var ifname in type:Interfaces
                var iftype := SELF:FindType(ifname, usings)
                if iftype != null
                    names:Add(iftype:FullName)
                endif
            next
        endif
        foreach var name in names
            result:AddRange(SystemTypeController.LookForExtensions( name, SELF:_AssemblyReferences))
            var dbList := XDatabase.GetExtensionMethods(SELF:DependentProjectList, name)
            result:AddRange(GetExtensionMethods(dbList))
        next
        return result

    METHOD GetFileById(nId as INT64) AS XFile
        VAR name := SELF:_SourceFilesDict:FindById(nId)
        IF name != NULL
            RETURN SELF:FindXFile(name)
        ENDIF
        RETURN NULL

    METHOD GetFilesOfType(type as XFileType, lRecursive as LOGIC) AS IList<string>
        LOCAL sProjectIds as STRING
        IF lRecursive
            sProjectIds := SELF:DependentProjectList
        ELSE
            sProjectIds := SELF:Id:ToString()
        ENDIF
        RETURN XDatabase.GetFilesOfType(type, sProjectIds)


#endregion

    METHOD UnLoad() AS VOID
        SELF:Loaded := FALSE
        SELF:LogReferenceMessage("UnLoad() "+SELF:FileName)
        FOREACH VAR asm IN SELF:_AssemblyReferences:ToArray()
            asm:RemoveProject(SELF)
        NEXT
        SELF:_AssemblyReferences:Clear()
        SystemTypeController.UnloadUnusedAssemblies()



    METHOD Walk() AS VOID
        IF XSettings.EnableParseLog
            WriteOutputMessage("Walk() ")
        ENDIF
        ModelWalker.AddProject(SELF)

    METHOD WalkFile(file AS XFile, lNotify := FALSE AS LOGIC) AS VOID
        ModelWalker.FileWalk(file)
        IF FileWalkComplete != NULL .AND. lNotify
            FileWalkComplete(file)
        ENDIF

    PUBLIC DELEGATE OnFileWalkComplete(xFile AS XFile) AS VOID

    PUBLIC DELEGATE OnProjectWalkComplete( xProject AS XProject ) AS VOID

#region Properties
    PROPERTY AssemblyReferences AS List<XAssembly>
    GET
        SELF:ResolveReferences()
        RETURN SELF:_AssemblyReferences
    END GET
    END PROPERTY

    PROPERTY AssemblyReferenceNames AS IList<String>
    GET
        var result := List<String>{}
        result:AddRange(SELF:_unprocessedAssemblyReferences)
        FOREACH var reference in SELF:_AssemblyReferences
            result:Add(reference:FileName)
        NEXT
        return result:ToArray()
    END GET
    END PROPERTY

    PRIVATE PROPERTY hasUnprocessedReferences AS LOGIC
    GET
        IF SELF:Loaded
            RETURN SELF:_unprocessedAssemblyReferences:Count + ;
                SELF:_unprocessedProjectReferences:Count + ;
                SELF:_unprocessedStrangerProjectReferences:Count > 0
        ENDIF
        RETURN FALSE
    END GET
    END PROPERTY

    PROPERTY Loaded AS LOGIC AUTO

    METHOD Rename(newName AS STRING) AS VOID
        VAR oldName := _name
        newName     := System.IO.Path.GetFileNameWithoutExtension(newName)
        _name       := newName
        XSolution.RenameProject(oldName, newName)
        RETURN

    PROPERTY Name AS STRING
    GET
        RETURN _name
    END GET
    END PROPERTY
    PRIVATE _prjNameSpaces AS IList<STRING>
    PRIVATE _lastNameSpaces := DateTime.MinValue AS DateTime
    PROPERTY ProjectNamespaces AS IList<STRING>
    GET
        if _prjNameSpaces != NULL .and. DateTime.Now:Subtract(_lastNameSpaces) < TimeSpan{0,0,10}
            return _prjNameSpaces
        ENDIF
        _prjNameSpaces := XDatabase.GetProjectNamespaces(SELF:DependentProjectList)
        _lastNameSpaces     :=  DateTime.Now
        return _prjNameSpaces
    END GET
    END PROPERTY

    PROPERTY AllNamespaces AS IList<STRING>
    GET
        IF _cachedAllNamespaces != NULL
            RETURN _cachedAllNamespaces
        ENDIF
        VAR result := SELF:ProjectNamespaces
        VAR asmNS  := SELF:AssemblyNamespaces
        IF result:Count > asmNS:Count
            FOREACH ns AS STRING IN asmNS
                IF !result:Contains(ns)
                    result:Add(ns)
                ENDIF
            NEXT
        ELSE
            FOREACH ns AS STRING IN result
                IF !asmNS:Contains(ns)
                    asmNS:Add(ns)
                ENDIF
            NEXT
            result := asmNS
        ENDIF
        _cachedAllNamespaces := result
        RETURN result
    END GET
    END PROPERTY

    PROPERTY AllUsingStatics as IList<String>
    GET
        IF _cachedUsingStatics != NULL
            RETURN _cachedUsingStatics
        ENDIF
        VAR statics := List<STRING>{}
        IF SELF:ProjectNode != NULL .AND. SELF:ProjectNode:ParseOptions:HasRuntime
            FOREACH asm AS XAssembly IN SELF:AssemblyReferences
                VAR globalclass := asm:GlobalClassName
                IF (! String.IsNullOrEmpty(globalclass))
                    statics:AddUnique(globalclass)
                ENDIF
            NEXT
        ENDIF
        _cachedUsingStatics := statics:ToArray()
        RETURN _cachedUsingStatics
    END GET
    END PROPERTY


    PROPERTY OtherFiles AS List<STRING> GET SELF:_OtherFilesDict:Keys:ToList()

    METHOD ResetParseOptions(newOptions AS XSharpParseOptions) AS VOID
        SELF:_parseOptions := newOptions
        RETURN

    PROPERTY ParseOptions AS XSharpParseOptions
    GET
        IF SELF:_parseOptions == NULL
            IF SELF:ProjectNode == NULL
                SELF:_parseOptions := XSharpParseOptions.Default
            ELSE
                SELF:_parseOptions := SELF:ProjectNode:ParseOptions
            ENDIF
        ENDIF
        RETURN SELF:_parseOptions
    END GET
    END PROPERTY
    PROPERTY ProjectNode AS IXSharpProject GET SELF:_projectNode

    PROPERTY ReferencedProjects AS IList<XProject>
    GET
        SELF:ResolveUnprocessedProjectReferences()
        RETURN SELF:_ReferencedProjects.ToArray()
    END GET
    END PROPERTY

    PROPERTY SourceFiles AS List<STRING>
    GET
        RETURN SELF:_SourceFilesDict:Keys:ToList()
    END GET
    END PROPERTY

    PROPERTY StrangerProjects AS Object[]
    GET
        SELF:ResolveUnprocessedStrangerReferences()
        RETURN SELF:_StrangerProjects:ToArray()
    END GET
    END PROPERTY

#endregion

    PRIVATE METHOD WriteOutputMessage(message AS STRING) AS VOID
        XSolution.WriteOutputMessage("XModel.Project "+SELF:Name+" "+message)

    CLASS XFileDictionary
        PROTECTED dict AS ConcurrentDictionary<STRING, INT64>
        PROTECTED prj as XProject
        CONSTRUCTOR(project as XProject)
            dict := ConcurrentDictionary<STRING, INT64>{StringComparer.OrdinalIgnoreCase}
            prj := project

        METHOD Clear() AS VOID
            SELF:dict:Clear()
        METHOD Add(fileName AS STRING) AS VOID
            IF !dict:ContainsKey(fileName)
                dict:TryAdd(fileName,-1)
            ENDIF

        METHOD Find(fileName AS STRING) AS XFile
            IF dict:ContainsKey(fileName)
                VAR result := XFile{fileName, prj}
                XDatabase.Read(result)
                dict[fileName] := result:Id
                RETURN result
            ENDIF
            RETURN NULL

        METHOD FindById(nId as INT64) AS STRING
            FOREACH Var pair in dict
                IF pair:Value == nId
                    RETURN pair:Key
                ENDIF
            NEXT
            RETURN NULL

        METHOD Remove(fileName AS STRING) AS LOGIC
            IF dict:ContainsKey(fileName)
                RETURN dict:TryRemove(fileName, OUT VAR _)
            ENDIF
            RETURN FALSE
        PROPERTY Keys AS ICollection<STRING> GET dict:Keys
        METHOD First() as XFile
            if dict:Count > 0
                return SELF:Find(dict:Keys:First())
            endif
            return NULL
    END CLASS



END CLASS

END NAMESPACE

