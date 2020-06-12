//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING System.Threading.Tasks
USING EnvDTE
USING LanguageService.CodeAnalysis
USING LanguageService.CodeAnalysis.XSharp
USING System.Collections.Concurrent
USING System.Diagnostics
USING System.Reflection

BEGIN NAMESPACE XSharpModel
   [DebuggerDisplay("{Name,nq}")];
   CLASS XProject
      #region Fields
      // Fields
      PROTECTED _id    := -1                    AS Int64                         
      PRIVATE _AssemblyReferences					AS List<AssemblyInfo>
      PRIVATE _parseOptions := NULL					AS XSharpParseOptions
      PRIVATE _projectNode							   AS IXSharpProject
      PRIVATE _projectOutputDLLs						AS ConcurrentDictionary<STRING, STRING>
      PRIVATE _ReferencedProjects					AS List<XProject>
      PRIVATE _StrangerProjects						AS List<EnvDTE.Project>
      PRIVATE _unprocessedAssemblyReferences		AS List<STRING>
      PRIVATE _unprocessedProjectReferences		AS List<STRING>
      PRIVATE _unprocessedStrangerProjectReferences	AS List<STRING>
      PRIVATE _failedStrangerProjectReferences   AS List<STRING>
      PRIVATE _unprocessedFiles                  AS List<STRING>
      PRIVATE _OtherFilesDict							 AS ConcurrentDictionary<STRING, XFile>
      PRIVATE _SourceFilesDict						 AS ConcurrentDictionary<STRING, XFile>
      PRIVATE _FunctionClasses                   AS List<STRING>
      PRIVATE _ImplicitNamespaces                AS List<STRING>
      PRIVATE _dependentProjectList              AS STRING
      PRIVATE _dependentAssemblyList             AS STRING
      PUBLIC  FileWalkComplete						AS XProject.OnFileWalkComplete
      
      #endregion
      #region Properties
      PROPERTY Id   AS INT64                     GET _id INTERNAL SET _id := value
      PROPERTY FileWalkCompleted                 AS LOGIC AUTO
      PROPERTY FileName                          AS STRING get _projectNode:Url
      PROPERTY DependentAssemblyList             AS STRING
         GET
            IF String.IsNullOrEmpty(_dependentAssemblyList)
               var result := ""
               FOREACH var assembly in _AssemblyReferences
                  if result:Length > 0
                     result += ","
                  endif
                  result += assembly:Id:ToString()
               NEXT
               _dependentAssemblyList := result
            ENDIF
            RETURN _dependentAssemblyList
            
         END GET
      END PROPERTY
         
      PROPERTY DependentProjectList              AS STRING
         GET
            IF String.IsNullOrEmpty(_dependentProjectList)
               var result := ""
               result := SELF:Id:ToString()
               FOREACH var dependent in _ReferencedProjects
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
                  return _projectNode:Dialect
               ENDIF
               RETURN XSharpDialect.Core
            CATCH e as Exception
               XSolution.WriteException(e)
            END TRY
            RETURN XSharpDialect.Core
            
         END GET
      END PROPERTY
      PROPERTY FunctionClasses as List<String>
         GET
            if _FunctionClasses == NULL
               VAR result := List<String>{}
               result.Add(XLiterals.GlobalName)
               FOREACH var asm in SELF:AssemblyReferences
                  var gcn := asm:GlobalClassName
                  IF !String.IsNullOrEmpty(gcn) .and.  result:IndexOf(gcn) == -1
                     result:Add(gcn)
                  ENDIF
               NEXT
               _FunctionClasses := result
            ENDIF
            return _FunctionClasses
         END GET
      END PROPERTY   
      
      PROPERTY ImplicitNamespaces as List<String>
         GET
            if _ImplicitNamespaces == NULL
               VAR result := List<String>{}
               IF SELF:ParseOptions:ImplicitNamespace
                  FOREACH project AS XProject IN SELF:ReferencedProjects
                     VAR ns := project:ProjectNode:ParseOptions:DefaultNamespace
                     IF ! String.IsNullOrEmpty(ns) .AND. result:IndexOf(ns) == -1
                        result:Add(ns)
                     ENDIF
                  NEXT
                  FOREACH var asm in SELF:AssemblyReferences
                     FOREACH var ns in asm:ImplicitNamespaces
                        IF result:IndexOf(ns) == -1
                           result:Add(ns)
                        ENDIF
                     NEXT
                  NEXT
               ENDIF
               _ImplicitNamespaces := result
            ENDIF
            return _ImplicitNamespaces
         END GET
      END PROPERTY   
      
      
      #endregion
      CONSTRUCTOR(project AS IXSharpProject)
         SUPER()
         SELF:_AssemblyReferences := List<AssemblyInfo>{}
         SELF:_unprocessedAssemblyReferences       := List<STRING>{}
         SELF:_unprocessedProjectReferences        := List<STRING>{}
         SELF:_unprocessedStrangerProjectReferences:= List<STRING>{}
         SELF:_failedStrangerProjectReferences     := List<STRING>{}
         SELF:_unprocessedFiles := List<STRING>{}
         SELF:_projectOutputDLLs := ConcurrentDictionary<STRING, STRING>{StringComparer.OrdinalIgnoreCase}
         SELF:_ReferencedProjects := List<XProject>{}
         SELF:_StrangerProjects := List<Project>{}
         SELF:_projectNode := project
         SELF:_SourceFilesDict := ConcurrentDictionary<STRING, XFile>{StringComparer.OrdinalIgnoreCase}
         SELF:_OtherFilesDict := ConcurrentDictionary<STRING, XFile>{StringComparer.OrdinalIgnoreCase}
         SELF:Loaded := TRUE
         SELF:FileWalkCompleted := FALSE
         IF ! XLiterals.Initialized
            XLiterals.SetKeywordCase(SELF:ProjectNode:KeywordsUppercase)
         ENDIF
         
      PRIVATE METHOD _clearTypeCache() AS VOID
         _ImplicitNamespaces    := NULL
         _dependentAssemblyList := NULL
         RETURN
         #region AssemblyReferences
         
         METHOD AddAssemblyReference(path AS STRING) AS VOID
            IF ! String.IsNullOrEmpty(path)
               SELF:WriteOutputMessage("AddAssemblyReference (string) "+path)
               SELF:_clearTypeCache()
               BEGIN LOCK _unprocessedAssemblyReferences
                  IF ! _unprocessedAssemblyReferences:Contains(path)
                     _unprocessedAssemblyReferences.Add(path)
                  ENDIF
               END LOCK
            ENDIF
            
         METHOD AddAssemblyReference(reference AS VSLangProj.Reference) AS VOID
            //LOCAL assemblyInfo AS AssemblyInfo
            IF reference != NULL
               SELF:WriteOutputMessage("AddAssemblyReference (VSLangProj.Reference) "+reference:Path)
               SELF:_clearTypeCache()
               IF ! AssemblyInfo.DisableAssemblyReferences
                  IF ! String.IsNullOrEmpty(reference:Path)
                     AddAssemblyReference(reference:Path)
                     //                        ELSE
                     //                            // create an assembly reference with the reference object and no real contents
                     //                            assemblyInfo := SystemTypeController.LoadAssembly(reference)
                     //                            SELF:_AssemblyReferences:Add(assemblyInfo)
                     //                            assemblyInfo:AddProject(SELF)
                  ENDIF
               ENDIF
            ENDIF
            
         METHOD ClearAssemblyReferences() AS VOID
            SELF:WriteOutputMessage("ClearAssemblyReferences() ")
            SELF:_clearTypeCache()
            FOREACH asm AS AssemblyInfo IN SELF:_AssemblyReferences
               asm:RemoveProject(SELF)
            NEXT
            SELF:_AssemblyReferences:Clear()
            
            
         METHOD RemoveAssemblyReference(fileName AS STRING) AS VOID
            IF ! String.IsNullOrEmpty(fileName)
               SELF:WriteOutputMessage("RemoveAssemblyReference() "+fileName)
               SELF:_clearTypeCache()
               BEGIN LOCK _unprocessedAssemblyReferences
                  IF _unprocessedAssemblyReferences:Contains(fileName)
                     _unprocessedAssemblyReferences.Remove(fileName)
                  ENDIF
               END LOCK
               FOREACH info AS AssemblyInfo IN SELF:_AssemblyReferences
                  IF String.Equals(info:FileName, fileName, System.StringComparison.OrdinalIgnoreCase)
                     SELF:_AssemblyReferences:Remove(info)
                     EXIT
                  ENDIF
               NEXT
            ENDIF
            
         PRIVATE METHOD LoadReference(cDLL AS STRING) AS VOID
            IF ! String.IsNullOrEmpty(cDLL)
               SELF:ProjectNode:SetStatusBarText(String.Format("Loading referenced types for project '{0}' from '{1}'", SELF:Name, System.IO.Path.GetFileName(cDLL)))
               VAR assemblyInfo := SystemTypeController.LoadAssembly(cDLL)
               SELF:_AssemblyReferences:Add(assemblyInfo)
               assemblyInfo:AddProject(SELF)
            ENDIF
            RETURN
            
         PRIVATE METHOD ResolveUnprocessedAssemblyReferences() AS VOID
            LOCAL loaded AS List<STRING>
            //
            IF SELF:_unprocessedAssemblyReferences:Count > 0 .AND. ! AssemblyInfo.DisableAssemblyReferences
               SELF:WriteOutputMessage("ResolveUnprocessedAssemblyReferences()")
               loaded := List<STRING>{}
               BEGIN LOCK _unprocessedAssemblyReferences
                  FOREACH path AS STRING IN SELF:_unprocessedAssemblyReferences
                     IF System.IO.File.Exists(path)
                        SELF:LoadReference(path)
                        loaded:Add(path)
                     ENDIF
                  NEXT
                  FOREACH path AS STRING IN loaded
                     IF SELF:_unprocessedAssemblyReferences:Contains(path)
                        SELF:_unprocessedAssemblyReferences:Remove(path)
                     ENDIF
                  NEXT
               END LOCK
               SELF:_clearTypeCache()
               SELF:ProjectNode:SetStatusBarText("")
            ENDIF
            RETURN
            
         METHOD ResolveReferences() AS VOID
            IF SELF:hasUnprocessedReferences
               SELF:WriteOutputMessage("<<-- ResolveReferences()")
               SELF:ProjectNode:SetStatusBarText(String.Format("Loading referenced types for project {0}", SELF:Name))
               SELF:ProjectNode:SetStatusBarAnimation(TRUE, 0)
               
               TRY
                  SELF:ResolveUnprocessedAssemblyReferences()
                  SELF:ResolveUnprocessedProjectReferences()
                  SELF:ResolveUnprocessedStrangerReferences()
                  FOREACH DLL AS STRING IN SELF:_projectOutputDLLs:Values
                     IF SystemTypeController.FindAssemblyByLocation(DLL) != NULL
                        SELF:WriteOutputMessage("ResolveReferences: No need to load types for Foreign assembly. Assembly is already loaded: "+DLL)
                        SELF:AddAssemblyReference(DLL)
                     ENDIF
                  NEXT
                  // repeat the assemblyreferences because we can have _projectOutputDLLs added to the list
                  SELF:ResolveUnprocessedAssemblyReferences()
               END TRY
               SELF:ProjectNode:SetStatusBarAnimation(FALSE, 0)
               SELF:ProjectNode:SetStatusBarText("")
               SELF:WriteOutputMessage(">>-- ResolveReferences()")
            ENDIF
            RETURN
            
         METHOD UpdateAssemblyReference(fileName AS STRING) AS VOID
            IF ! AssemblyInfo.DisableAssemblyReferences .AND. ! String.IsNullOrEmpty(fileName)
               SystemTypeController.LoadAssembly(fileName):AddProject(SELF)
            ENDIF
            
            #endregion
            
         #region ProjectReferences
         
         METHOD AddProjectReference(url AS STRING) AS LOGIC
            IF ! String.IsNullOrEmpty(url)
               SELF:_ImplicitNamespaces := NULL
               SELF:_dependentAssemblyList := NULL
               SELF:_dependentProjectList  := ""
               SELF:WriteOutputMessage("Add XSharp ProjectReference "+url)
               IF ! SELF:_unprocessedProjectReferences:Contains(url)
                  SELF:_unprocessedProjectReferences:Add(url)
                  RETURN TRUE
               ENDIF
            ENDIF
            RETURN FALSE
            
         METHOD AddProjectOutput(sProjectURL AS STRING, sOutputDLL AS STRING) AS VOID
            IF ! String.IsNullOrEmpty(sProjectURL) .AND. ! String.IsNullOrEmpty(sOutputDLL)
               SELF:WriteOutputMessage("AddProjectOutput "+sProjectURL+"("+sOutputDLL+")")
               IF SELF:_projectOutputDLLs:ContainsKey(sProjectURL)
                  SELF:_projectOutputDLLs:Item[sProjectURL] := sOutputDLL
               ELSE
                  SELF:_projectOutputDLLs:TryAdd(sProjectURL, sOutputDLL)
               ENDIF
               SELF:_clearTypeCache()
            ENDIF
            
         METHOD RemoveProjectOutput(sProjectURL AS STRING) AS VOID
            IF ! String.IsNullOrEmpty(sProjectURL)
               WriteOutputMessage("RemoveProjectOutput() "+sProjectURL)
               IF SELF:_projectOutputDLLs:ContainsKey(sProjectURL)
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
               WriteOutputMessage("RemoveProjectReference() "+url)
               SELF:_clearTypeCache()
               IF SELF:_unprocessedProjectReferences:Contains(url)
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
               WriteOutputMessage("RemoveProjectReferenceDLL() "+DLL)
               SELF:_clearTypeCache()
               SELF:RemoveAssemblyReference(DLL)
            ENDIF
            
         PRIVATE METHOD ResolveUnprocessedProjectReferences() AS VOID
            LOCAL existing AS List<STRING>
            LOCAL p AS XProject
            LOCAL outputFile AS STRING
            IF SELF:_unprocessedProjectReferences:Count > 0  .AND. ! AssemblyInfo.DisableXSharpProjectReferences
               WriteOutputMessage("ResolveUnprocessedProjectReferences()")
               existing := List<STRING>{}
               FOREACH sProject AS STRING IN SELF:_unprocessedProjectReferences
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
            WriteOutputMessage("Add Foreign ProjectReference"+url)
            IF ! SELF:_unprocessedStrangerProjectReferences:Contains(url)
               SELF:_unprocessedStrangerProjectReferences:Add(url)
               RETURN TRUE
            ENDIF
         ENDIF
         RETURN FALSE
         
      PRIVATE METHOD saveGetProperty(props AS EnvDTE.Properties, name AS STRING) AS EnvDTE.Property
         LOCAL p AS EnvDTE.Property
         TRY
            p := props:Item(name)
         CATCH
            p := NULL
         END TRY
         RETURN p
         
      PRIVATE METHOD GetStrangerOutputDLL(sProject AS STRING, p AS Project) AS STRING
         VAR outputFile := ""
         TRY
            LOCAL propTypepropName := NULL AS EnvDTE.Property
            VAR propType := saveGetProperty(p:Properties, "OutputType")
            VAR propName := saveGetProperty(p:Properties, "AssemblyName")
            VAR propPath := saveGetProperty(p:ConfigurationManager:ActiveConfiguration:Properties, "OutputPath")
            IF propName != NULL .AND. propPath != NULL .AND. propType != NULL
               VAR path    := (STRING) propPath:Value
               VAR type    := (INT) propType:Value
               outputFile	:= (STRING) propName:Value
               IF type == 2 // __VSPROJOUTPUTTYPE.VSPROJ_OUTPUTTYPE_LIBRARY
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
            XSolution.WriteException(exception)
         END TRY
         RETURN outputFile
         
         
      METHOD RemoveStrangerProjectReference(url AS STRING) AS LOGIC
         IF ! String.IsNullOrEmpty(url)
            LOCAL prj AS Project
            WriteOutputMessage("RemoveStrangerProjectReference() "+url)
            SELF:_clearTypeCache()
            IF SELF:_unprocessedStrangerProjectReferences:Contains(url)
               SELF:_unprocessedStrangerProjectReferences:Remove(url)
               RETURN TRUE
            ENDIF
            IF SELF:_failedStrangerProjectReferences:Contains(url)
               SELF:_failedStrangerProjectReferences:Remove(url)
               RETURN TRUE
            ENDIF
            
            SELF:RemoveProjectOutput(url)
            prj := SELF:ProjectNode:FindProject(url)
            IF prj != NULL .AND. SELF:_StrangerProjects:Contains(prj)
               SELF:_StrangerProjects:Remove(prj)
               RETURN TRUE
            ENDIF
         ENDIF
         RETURN FALSE
         
      PRIVATE METHOD RefreshStrangerProjectDLLOutputFiles() AS VOID
         // Check if any DLL has changed
         IF SELF:_StrangerProjects:Count > 0 .AND. ! AssemblyInfo.DisableForeignProjectReferences
            WriteOutputMessage("--> RefreshStrangerProjectDLLOutputFiles() "+SELF:_StrangerProjects:Count():ToString())
            FOREACH p AS EnvDTE.Project IN SELF:_StrangerProjects
               VAR sProjectURL := p:FullName
               VAR mustAdd     := FALSE
               VAR outputFile  := SELF:GetStrangerOutputDLL(sProjectURL, p)
               IF SELF:_projectOutputDLLs:ContainsKey(sProjectURL)
                  // when the output file name of the referenced project has changed
                  // then remove the old name
                  IF outputFile:ToLower() != SELF:_projectOutputDLLs:Item[sProjectURL]:ToLower()
                     WriteOutputMessage(i"DLL has been renamed to {outputFile} so remove the old DLL {SELF:_projectOutputDLLs:Item[sProjectURL]}")
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
            WriteOutputMessage("<-- RefreshStrangerProjectDLLOutputFiles()")
         ENDIF
         
      PRIVATE METHOD ResolveUnprocessedStrangerReferences() AS VOID
         LOCAL existing AS List<STRING>
         LOCAL p AS Project
         LOCAL outputFile AS STRING
         IF SELF:_unprocessedStrangerProjectReferences:Count > 0 .AND. ! AssemblyInfo.DisableForeignProjectReferences
            WriteOutputMessage("ResolveUnprocessedStrangerReferences()" +_unprocessedStrangerProjectReferences:Count:ToString())
            existing := List<STRING>{}
            FOREACH sProject AS STRING IN SELF:_unprocessedStrangerProjectReferences
               p := SELF:ProjectNode:FindProject(sProject)
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
            SELF:_clearTypeCache()
         ENDIF
         
      #endregion
      
      #region 'Normal' Files
      
      METHOD AddFile(filePath AS STRING) AS LOGIC
         BEGIN LOCK _unprocessedFiles
            SELF:_unprocessedFiles:Add(filePath)
         END LOCK
         RETURN FALSE
         
      METHOD AddFile(xFile AS XFile) AS LOGIC
         LOCAL xamlCodeBehindFile AS STRING
         IF xFile != NULL
            xFile:Project := SELF
            XDatabase:Read(xFile)
            IF xFile:IsSource
               IF SELF:_SourceFilesDict:ContainsKey(xFile:FullPath)
                  SELF:_SourceFilesDict:TryRemove(xFile:FullPath, OUT VAR _)
               ENDIF
               RETURN SELF:_SourceFilesDict:TryAdd(xFile:FullPath, xFile)
            ENDIF
            IF xFile:IsXaml
               xamlCodeBehindFile := xFile:XamlCodeBehindFile
               IF SELF:_SourceFilesDict:ContainsKey(xamlCodeBehindFile)
                  SELF:_SourceFilesDict:TryRemove(xamlCodeBehindFile, OUT  VAR _)
               ENDIF
               SELF:_SourceFilesDict:TryAdd(xamlCodeBehindFile, xFile)
               IF SELF:_OtherFilesDict:ContainsKey(xFile:FullPath)
                  SELF:_OtherFilesDict:TryRemove(xFile:FullPath, OUT VAR _)
               ENDIF
               RETURN SELF:_OtherFilesDict:TryAdd(xFile:FullPath, xFile)
            ENDIF
            IF SELF:_OtherFilesDict:ContainsKey(xFile:FullPath)
               SELF:_OtherFilesDict:TryRemove(xFile:FullPath, OUT VAR _)
            ENDIF
            RETURN SELF:_OtherFilesDict:TryAdd(xFile:FullPath, xFile)
         ENDIF
         RETURN FALSE
         
      METHOD FindFullPath(fullPath AS STRING) AS XFile
         IF ! String.IsNullOrEmpty(fullPath)
            IF SELF:_SourceFilesDict:ContainsKey(fullPath)
               RETURN SELF:_SourceFilesDict:Item[fullPath]
            ENDIF
            IF SELF:_OtherFilesDict:ContainsKey(fullPath)
               RETURN SELF:_OtherFilesDict:Item[fullPath]
            ENDIF
            BEGIN LOCK SELF:_unprocessedFiles
               // THis may happen when the IDE reopens a file before we had the chance to background scan it.
               IF SELF:_unprocessedFiles:Contains(fullPath)
                  SELF:_unprocessedFiles:Remove(fullPath)
                  VAR xFile := XFile{fullPath}
                  SELF:AddFile(xFile)
                  RETURN xFile
               ENDIF
            END LOCK
         ENDIF
         RETURN NULL
         
      METHOD RemoveFile(url AS STRING) AS VOID
         LOCAL file AS XFile
         IF ! String.IsNullOrEmpty(url)
            IF SELF:_OtherFilesDict:ContainsKey(url)
               SELF:_OtherFilesDict:TryRemove(url, OUT file)
               IF file != NULL .AND. file:IsXaml
                  url := file:XamlCodeBehindFile
               ENDIF
            ENDIF
            IF SELF:_SourceFilesDict:ContainsKey(url)
               SELF:_SourceFilesDict:TryRemove(url, OUT file)
               IF file != NULL
                  FOREACH VAR type IN file:TypeList:Values
                     SELF:RemoveType(type)
                  NEXT
               ENDIF
            ENDIF
         ENDIF
         #endregion
         
      #region Lookup Types and Functions
      METHOD FindFunction(name AS STRING) AS IXMember
         // we look in the project references and assembly references
         // pass the list of ProjectIds and AssemblyIds to the databasse engine
         var result := XDatabase.FindFunction(name, SELF:DependentProjectList, SELF:DependentAssemblyList, SELF:FunctionClasses)
         VAR xres := result:FirstOrDefault()
         IF xres != NULL
            // XDbResult needs to be translated to XMember
            // either we open the source file or we read the type from the Assembly
//            mem :=
//            RETURN mem
         ENDIF
//         return mem         
         RETURN NULL                  
         
      
//         WriteOutputMessage("FindFunction() "+name)
//         var xType := SELF:LookupMergedType(XLiterals.GlobalName)
//         if xType == NULL
//            xType := SELF:LookupMergedType(XLiterals.GlobalName)
//         ENDIF
//         IF xType != NULL
//            var mem := xType:GetMembers(name,TRUE):Where (;
//            { x=> (x.Kind == Kind.Procedure .OR. x:Kind == Kind.Function)   }):FirstOrDefault()
//            if mem != null
//               WriteOutputMessage("FindFunction()  found: "+mem:FullName)
//               return mem
//            ENDIF
//         ENDIF
         
      METHOD FindGlobalOrDefine(name AS STRING) AS IXMember
//         WriteOutputMessage("FindGlobalOrDefine() "+name)
//         var xType := SELF:LookupMergedType(XLiterals.GlobalName)
//         if xType == NULL
//            SELF:MergeType(XLiterals.GlobalName,TRUE)
//            xType := SELF:LookupMergedType(XLiterals.GlobalName)
//         ENDIF
//         if xType != NULL  
//            var oMember := xType:GetMembers(name,TRUE):Where (;
//            { x=> (x.Kind == Kind.VOGlobal .OR. x:Kind == Kind.VODefine)   }):FirstOrDefault()
//            IF oMember != NULL
//               WriteOutputMessage("FindGlobalOrDefine()  found: "+oMember:FullName)
//               RETURN oMember
//            ENDIF
//         ENDIF
         
         RETURN NULL
      METHOD FindSystemType(name AS STRING, usings AS IList<STRING>) AS XTypeReference
         WriteOutputMessage("FindSystemType() "+name)
//         IF ! AssemblyInfo.DisableForeignProjectReferences
//            SELF:RefreshStrangerProjectDLLOutputFiles()
//         ENDIF
//         SELF:ResolveReferences()
//         IF _ExternalTypeCache:ContainsKey(name)
//            WriteOutputMessage("FindSystemType() "+name+" found in cache")
//            RETURN _ExternalTypeCache[name]
//         ENDIF
//         FOREACH VAR u IN usings
//            VAR fullname := u+"."+name
//            IF _ExternalTypeCache:ContainsKey(fullname)
//               WriteOutputMessage("FindSystemType() "+fullname+" found in cache")
//               RETURN _ExternalTypeCache[fullname]
//            ENDIF
//         NEXT
//         VAR type := SystemTypeController.FindType(name, usings, SELF:_AssemblyReferences)
//         IF type != NULL
//            WriteOutputMessage("FindSystemType() "+name+" found "+type:FullName)
//            IF !_ExternalTypeCache:ContainsKey(type:FullName)
//               _ExternalTypeCache:TryAdd(type:FullName, type)
//            ENDIF
//         ENDIF
//         RETURN type
      RETURN NULL         
      METHOD GetAssemblyNamespaces() AS IList<STRING>
         RETURN SystemTypeController.GetNamespaces(SELF:_AssemblyReferences)
         
      METHOD Lookup(typeName AS STRING, caseInvariant AS LOGIC) AS XTypeDefinition
//         LOCAL xType AS XTypeDefinition
//         WriteOutputMessage("Lookup() "+typeName)
//         xType := SELF:LookupMergedType(typeName)
//         IF xType != NULL
//            WriteOutputMessage("Lookup()  found: "+xType:FullName)
//            RETURN xType
//         ENDIF
//         IF _TypeDict:ContainsKey(typeName)
//            VAR files := _TypeDict[typeName]:ToArray()
//            FOREACH sFile AS STRING IN files
//               // It seems sometimes the Key has changed; may be after a reparse ?
//               // To just TRY to get the file
//               IF _SourceFilesDict:TryGetValue( sFile, OUT var file )
//                  IF file:TypeList:TryGetValue(typeName, OUT VAR xTemp ) .AND. xTemp  != NULL
//                     IF (! caseInvariant .AND. ((xTemp:FullName != typeName) .AND. xTemp:Name != typeName))
//                        xTemp := NULL
//                     ENDIF
//                  ENDIF
//                  IF xTemp != NULL
//                     IF ! xTemp:IsPartial
//                        WriteOutputMessage("Lookup()  found: "+xTemp:FullName + " in " + file:Name )
//                        RETURN xTemp
//                     ENDIF
//                     IF xType != NULL
//                        xType := xType:Merge(xTemp)
//                     ELSE
//                        xType := XTypeDefinition{xTemp}
//                     ENDIF
//                  ENDIF
//               ENDIF
//            NEXT
//         ENDIF
//         IF xType != NULL
//            WriteOutputMessage("Lookup()  found: "+xType:FullName)
//         ENDIF
//         IF xType == NULL
//            xType := SELF:LookupReferenced(typeName, caseInvariant)
//         ENDIF
//         RETURN xType
         RETURN NULL       
      METHOD LookupReferenced(typeName AS STRING, caseInvariant AS LOGIC) AS XTypeDefinition
         LOCAL xType AS XTypeDefinition
         xType := NULL
         IF ! AssemblyInfo.DisableXSharpProjectReferences
            WriteOutputMessage("LookupReferenced() "+typeName)
            FOREACH project AS XProject IN SELF:ReferencedProjects
               IF project == SELF
                  LOOP
               ENDIF
               xType := project:Lookup(typeName, caseInvariant)
               IF xType != NULL
                  WriteOutputMessage(String.Format("LookupReferenced() found in {0}: {1} ", project:Name, xType:FullName))
                  RETURN xType
               ENDIF
            NEXT
         ENDIF
         RETURN xType
         
      METHOD GetExtensions( systemType AS IXType ) AS List<IXMember>
         RETURN SystemTypeController.LookForExtensions( systemType, SELF:_AssemblyReferences)
         
         
         #endregion
         
      METHOD UnLoad() AS VOID
         SELF:Loaded := FALSE
         WriteOutputMessage("UnLoad() ")
         FOREACH asm AS AssemblyInfo IN SELF:_AssemblyReferences
            asm:RemoveProject(SELF)
         NEXT
         SELF:_AssemblyReferences:Clear()
         
      METHOD BuildFileList AS VOID
         BEGIN LOCK SELF:_unprocessedFiles
            FOREACH VAR file IN SELF:_unprocessedFiles
               SELF:AddFile(XFile{file})
            NEXT
            SELF:_unprocessedFiles:Clear()
         END LOCK
         
         
      METHOD Walk() AS VOID
         WriteOutputMessage("Walk() ")
         ModelWalker.GetWalker():AddProject(SELF)
         
      METHOD WalkFile(file AS XFile) AS VOID
         ModelWalker.GetWalker():FileWalk(file)
         IF FileWalkComplete != NULL
            FileWalkComplete(file)
         ENDIF
         
      PUBLIC DELEGATE OnFileWalkComplete(xFile AS XFile) AS VOID
      
      #region Properties
      PROPERTY AssemblyReferences AS List<AssemblyInfo>
         GET
            RETURN SELF:_AssemblyReferences
         END GET
      END PROPERTY
      
      PRIVATE PROPERTY hasUnprocessedReferences AS LOGIC
         GET
            RETURN SELF:_unprocessedAssemblyReferences:Count + ;
            SELF:_unprocessedProjectReferences:Count + ;
            SELF:_unprocessedStrangerProjectReferences:Count > 0
         END GET
      END PROPERTY
      
      PROPERTY Loaded AS LOGIC AUTO
      
      PROPERTY Name AS STRING
         GET
            RETURN System.IO.Path.GetFileNameWithoutExtension(SELF:ProjectNode:Url)
         END GET
      END PROPERTY
      
      PROPERTY Namespaces AS IList<XTypeDefinition>
         GET
            VAR types := List<XTypeDefinition>{}
            VAR fileArray := SELF:SourceFiles:ToArray()
            FOREACH file AS XFile IN fileArray
               IF file:TypeList != NULL
                  VAR values := file:TypeList:Values
                  FOREACH elmt AS XTypeDefinition IN values
                     IF elmt:Kind == Kind.Namespace
                        IF types:Find( { x => x:Name:ToLowerInvariant() == elmt:Name:ToLowerInvariant() } ) == NULL
                           types:Add(elmt)
                        ENDIF
                     ENDIF
                  NEXT
               ENDIF
            NEXT
            RETURN types
         END GET
      END PROPERTY
      
      PROPERTY OtherFiles AS List<XFile> GET SELF:_OtherFilesDict:Values:ToList()
      
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
      PROPERTY IsVsBuilding AS LOGIC GET IIF(SELF:_projectNode == NULL, FALSE, SELF:_projectNode:IsVsBuilding)
      PROPERTY ProjectNode AS IXSharpProject GET SELF:_projectNode SET SELF:_projectNode := VALUE
      
      PROPERTY ReferencedProjects AS IList<XProject>
         GET
            SELF:ResolveUnprocessedProjectReferences()
            RETURN SELF:_ReferencedProjects.ToArray()
         END GET
      END PROPERTY
      
      PROPERTY SourceFiles AS List<XFile> 
         GET 
            SELF:BuildFileList()
            RETURN SELF:_SourceFilesDict:Values:ToList()
         END GET
      END PROPERTY
      
      PROPERTY StrangerProjects AS IList<Project>
         GET
            SELF:ResolveUnprocessedStrangerReferences()
            RETURN SELF:_StrangerProjects:ToArray()
         END GET
      END PROPERTY
      
      #endregion
      
      #region Types
      INTERNAL METHOD AddType(xType AS XTypeDefinition) AS VOID
         // only add global types that have members
//         IF ! XTypeDefinition.IsGlobalType(xType) .OR. xType:Members:Count > 0
//            VAR typeName := xType:FullName
//            VAR fileName := xType:File:FullPath
//            IF xType:File:IsXaml
//               fileName := xType:File:XamlCodeBehindFile
//            ENDIF
//            BEGIN LOCK _TypeDict
//               SELF:_TypeDict:Add(typeName, fileName)
//               
//            END LOCK
//         ENDIF
//         RETURN
         
      INTERNAL METHOD RemoveType(xType AS XTypeDefinition) AS VOID
//         VAR typeName := xType:FullName
//         VAR fileName := xType:File:FullPath
//         IF xType:File:IsXaml
//            fileName := xType:File:XamlCodeBehindFile
//         ENDIF
//         BEGIN LOCK _TypeDict
//            IF SELF:_TypeDict:ContainsKey(typeName)
//               IF SELF:_TypeDict[typeName]:Contains(fileName)
//                  SELF:_TypeDict[typeName]:Remove(fileName)
//               ENDIF
//               IF SELF:_TypeDict[typeName]:Count == 0
//                  SELF:_TypeDict:Remove(typeName)
//               ENDIF
//            ENDIF
//            
//         END LOCK
         RETURN
         
      #endregion
      
      PRIVATE METHOD WriteOutputMessage(message AS STRING) AS VOID
      XSolution.WriteOutputMessage("XModel.Project "+SELF:Name+" "+message)
   END CLASS
   
END NAMESPACE

