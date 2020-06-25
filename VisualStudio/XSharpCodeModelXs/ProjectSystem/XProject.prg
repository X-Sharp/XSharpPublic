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
      PRIVATE _parseOptions := NULL					AS XSharpParseOptions
      PRIVATE _projectNode							   AS IXSharpProject
      PRIVATE _projectOutputDLLs						AS ConcurrentDictionary<STRING, STRING>
      PRIVATE _ReferencedProjects					AS List<XProject>
      PRIVATE _StrangerProjects						AS List<EnvDTE.Project>
      PRIVATE _unprocessedAssemblyReferences		AS List<STRING>
      PRIVATE _unprocessedProjectReferences		AS List<STRING>
      PRIVATE _unprocessedStrangerProjectReferences	AS List<STRING>
      PRIVATE _failedStrangerProjectReferences   AS List<STRING>
      PRIVATE _OtherFilesDict							 AS XFileDictionary
      PRIVATE _SourceFilesDict						 AS XFileDictionary
      PRIVATE _FunctionClasses                   AS List<STRING>
      PRIVATE _ImplicitNamespaces                AS List<STRING>
      PRIVATE _dependentProjectList              AS STRING
      PRIVATE _dependentAssemblyList             AS STRING
      PRIVATE _name                              AS STRING
      PUBLIC  FileWalkComplete						AS XProject.OnFileWalkComplete
      
      #endregion
      #region Properties
      PROPERTY Id   AS INT64                     GET _id INTERNAL SET _id := value
      PROPERTY FileWalkCompleted                 AS LOGIC AUTO
      PROPERTY FileName                          AS STRING GET _projectNode:Url
      PROPERTY DependentAssemblyList             AS STRING
         GET
            IF String.IsNullOrEmpty(_dependentAssemblyList)
               VAR result := ""
               FOREACH VAR assembly IN _AssemblyReferences
                  IF result:Length > 0
                     result += ","
                  ENDIF
                  result += assembly:Id:ToString()
               NEXT
               _dependentAssemblyList := result
            ENDIF
            RETURN _dependentAssemblyList
            
         END GET
      END PROPERTY
      
      PROPERTY DependentProjectList              AS STRING
         GET
            IF String.IsNullOrEmpty(_dependentProjectList) .OR. _dependentProjectList == SELF:Id:ToString()
               VAR result := ""
               result := SELF:Id:ToString()
               FOREACH VAR dependent IN _ReferencedProjects
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
               XSolution.WriteException(e)
            END TRY
            RETURN XSharpDialect.Core
            
         END GET
      END PROPERTY
      PROPERTY FunctionClasses AS List<STRING>
         GET
            IF _FunctionClasses == NULL
               VAR result := List<STRING>{}
               result.Add(XLiterals.GlobalName)
               FOREACH VAR asm IN SELF:AssemblyReferences
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
                  FOREACH project AS XProject IN SELF:ReferencedProjects
                     VAR ns := project:ProjectNode:ParseOptions:DefaultNamespace
                     IF ! String.IsNullOrEmpty(ns) .AND. result:IndexOf(ns) == -1
                        result:Add(ns)
                     ENDIF
                  NEXT
                  FOREACH VAR asm IN SELF:AssemblyReferences
                     FOREACH VAR ns IN asm:ImplicitNamespaces
                        IF result:IndexOf(ns) == -1
                           result:Add(ns)
                        ENDIF
                     NEXT
                  NEXT
               ENDIF
               _ImplicitNamespaces := result
            ENDIF
            RETURN _ImplicitNamespaces
         END GET
      END PROPERTY   
      
      
      #endregion
      CONSTRUCTOR(project AS IXSharpProject)
         SUPER()
         SELF:_AssemblyReferences := List<XAssembly>{}
         SELF:_unprocessedAssemblyReferences       := List<STRING>{}
         SELF:_unprocessedProjectReferences        := List<STRING>{}
         SELF:_unprocessedStrangerProjectReferences:= List<STRING>{}
         SELF:_failedStrangerProjectReferences     := List<STRING>{}
         SELF:_projectOutputDLLs := ConcurrentDictionary<STRING, STRING>{StringComparer.OrdinalIgnoreCase}
         SELF:_ReferencedProjects := List<XProject>{}
         SELF:_StrangerProjects := List<EnvDTE.Project>{}
         SELF:_projectNode := project
         SELF:_SourceFilesDict   := XFileDictionary{}
         SELF:_OtherFilesDict    := XFileDictionary{}
         SELF:_name              := System.IO.Path.GetFileNameWithoutExtension(project:Url)
         SELF:Loaded := TRUE
         SELF:FileWalkCompleted := FALSE
         XSolution.Add(SELF) 
      PUBLIC METHOD Close() AS VOID
         XSolution.Remove(SELF)
         _projectNode := NULL
         SELF:UnLoad()
         RETURN
      PRIVATE METHOD _clearTypeCache() AS VOID
         _ImplicitNamespaces    := NULL
         _dependentAssemblyList := NULL
         RETURN
         #region AssemblyReferences
         
         METHOD AddAssemblyReference(path AS STRING) AS VOID
            IF ! String.IsNullOrEmpty(path)
               IF XSettings.EnableReferenceInfoLog
                  SELF:WriteOutputMessage("AddAssemblyReference (string) "+path)
               ENDIF
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
               IF XSettings.EnableReferenceInfoLog
                  SELF:WriteOutputMessage("AddAssemblyReference (VSLangProj.Reference) "+reference:Path)
               ENDIF
               SELF:_clearTypeCache()
               IF ! XSettings.DisableAssemblyReferences
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
            IF XSettings.EnableReferenceInfoLog
               SELF:WriteOutputMessage("ClearAssemblyReferences() ")
            ENDIF
            SELF:_clearTypeCache()
            FOREACH VAR asm IN SELF:_AssemblyReferences
               asm:RemoveProject(SELF)
            NEXT
            SELF:_AssemblyReferences:Clear()
            
            
         METHOD RemoveAssemblyReference(fileName AS STRING) AS VOID
            IF ! String.IsNullOrEmpty(fileName)
               IF XSettings.EnableReferenceInfoLog
                  SELF:WriteOutputMessage("RemoveAssemblyReference() "+fileName)
               ENDIF
               SELF:_clearTypeCache()
               BEGIN LOCK _unprocessedAssemblyReferences
                  IF _unprocessedAssemblyReferences:Contains(fileName)
                     _unprocessedAssemblyReferences.Remove(fileName)
                  ENDIF
               END LOCK
               FOREACH VAR asm IN SELF:_AssemblyReferences
                  IF String.Equals(asm:FileName, fileName, System.StringComparison.OrdinalIgnoreCase)
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
               IF XSettings.EnableReferenceInfoLog
                  SELF:WriteOutputMessage("ResolveUnprocessedAssemblyReferences()")
               ENDIF
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
               IF XSettings.EnableReferenceInfoLog
                  SELF:WriteOutputMessage("<<-- ResolveReferences()")
               ENDIF
               SELF:ProjectNode:SetStatusBarText(String.Format("Loading referenced types for project {0}", SELF:Name))
               
               TRY
                  SELF:ResolveUnprocessedAssemblyReferences()
                  SELF:ResolveUnprocessedProjectReferences()
                  SELF:ResolveUnprocessedStrangerReferences()
                  FOREACH DLL AS STRING IN SELF:_projectOutputDLLs:Values
                     IF SystemTypeController.FindAssemblyByLocation(DLL) != NULL
                        SELF:AddAssemblyReference(DLL)
                     ENDIF
                  NEXT
                  // repeat the assemblyreferences because we can have _projectOutputDLLs added to the list
                  SELF:ResolveUnprocessedAssemblyReferences()
               END TRY
               IF XSettings.EnableReferenceInfoLog
                  SELF:WriteOutputMessage(">>-- ResolveReferences()")
               ENDIF
            ENDIF
            RETURN
            
         METHOD UpdateAssemblyReference(fileName AS STRING) AS VOID
            IF ! XSettings.DisableAssemblyReferences .AND. ! String.IsNullOrEmpty(fileName)
               SystemTypeController.LoadAssembly(fileName):AddProject(SELF)
            ENDIF
            
            #endregion
         
         #region ProjectReferences
         
         METHOD AddProjectReference(url AS STRING) AS LOGIC
            IF ! String.IsNullOrEmpty(url)
               SELF:_ImplicitNamespaces := NULL
               SELF:_dependentAssemblyList := NULL
               SELF:_dependentProjectList  := ""
               IF XSettings.EnableReferenceInfoLog
                  SELF:WriteOutputMessage("Add XSharp ProjectReference "+url)
               ENDIF
               IF ! SELF:_unprocessedProjectReferences:Contains(url)
                  SELF:_unprocessedProjectReferences:Add(url)
                  RETURN TRUE
               ENDIF
            ENDIF
            RETURN FALSE
            
         METHOD AddProjectOutput(sProjectURL AS STRING, sOutputDLL AS STRING) AS VOID
            IF ! String.IsNullOrEmpty(sProjectURL) .AND. ! String.IsNullOrEmpty(sOutputDLL)
               IF XSettings.EnableReferenceInfoLog
                  SELF:WriteOutputMessage("AddProjectOutput "+sProjectURL+"("+sOutputDLL+")")
               ENDIF
               IF SELF:_projectOutputDLLs:ContainsKey(sProjectURL)
                  SELF:_projectOutputDLLs:Item[sProjectURL] := sOutputDLL
               ELSE
                  SELF:_projectOutputDLLs:TryAdd(sProjectURL, sOutputDLL)
               ENDIF
               SELF:_clearTypeCache()
            ENDIF
            
         METHOD RemoveProjectOutput(sProjectURL AS STRING) AS VOID
            IF ! String.IsNullOrEmpty(sProjectURL)
               IF XSettings.EnableReferenceInfoLog
                  WriteOutputMessage("RemoveProjectOutput() "+sProjectURL)
               ENDIF
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
               IF XSettings.EnableReferenceInfoLog
                  WriteOutputMessage("RemoveProjectReference() "+url)
               ENDIF
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
               IF XSettings.EnableReferenceInfoLog
                  WriteOutputMessage("RemoveProjectReferenceDLL() "+DLL)
               ENDIF
               SELF:_clearTypeCache()
               SELF:RemoveAssemblyReference(DLL)
            ENDIF
            
         PRIVATE METHOD ResolveUnprocessedProjectReferences() AS VOID
            LOCAL existing AS List<STRING>
            LOCAL p AS XProject
            LOCAL outputFile AS STRING
            IF SELF:_unprocessedProjectReferences:Count > 0  .AND. ! XSettings.DisableXSharpProjectReferences
               IF XSettings.EnableReferenceInfoLog
                  WriteOutputMessage("ResolveUnprocessedProjectReferences()")
               ENDIF
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
            IF XSettings.EnableReferenceInfoLog
               WriteOutputMessage("Add Foreign ProjectReference"+url)
            ENDIF
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
      
      PRIVATE METHOD IVarGet(oValue AS OBJECT, cProperty AS STRING) AS OBJECT
         IF oValue == NULL
            RETURN NULL
         ENDIF
         VAR props  := TypeDescriptor.GetProperties(oValue,FALSE)
         VAR prop   := props[cProperty]
         IF prop != NULL
            RETURN prop:GetValue(oValue)
         ENDIF
         RETURN NULL         
      
      PRIVATE METHOD GetStrangerOutputDLL(sProject AS STRING, p AS EnvDTE.Project) AS STRING
         VAR outputFile := ""
         TRY
            VAR propType := saveGetProperty(p:Properties, "OutputType")
            VAR propName := saveGetProperty(p:Properties, "AssemblyName")
            VAR propPath := saveGetProperty(p:ConfigurationManager:ActiveConfiguration:Properties, "OutputPath")
            IF propName != NULL .AND. propPath != NULL .AND. propType != NULL
               
               VAR path    := (STRING) IVarGet(propPath,"Value")
               VAR type    := (INT)    IVarGet(propType,"Value")
               outputFile	:= (STRING) IVarGet(propName, "Value")
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
            XSolution.WriteException(exception)
         END TRY
         RETURN outputFile
         
      
      METHOD RemoveStrangerProjectReference(url AS STRING) AS LOGIC
         IF ! String.IsNullOrEmpty(url)
            LOCAL prj AS EnvDTE.Project
            IF XSettings.EnableReferenceInfoLog
               WriteOutputMessage("RemoveStrangerProjectReference() "+url)
            ENDIF
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
         IF SELF:_StrangerProjects:Count > 0 .AND. ! XSettings.DisableForeignProjectReferences
            IF XSettings.EnableReferenceInfoLog
               WriteOutputMessage("--> RefreshStrangerProjectDLLOutputFiles() "+SELF:_StrangerProjects:Count():ToString())
            ENDIF
            FOREACH p AS EnvDTE.Project IN SELF:_StrangerProjects
               VAR sProjectURL := p:FullName
               VAR mustAdd     := FALSE
               VAR outputFile  := SELF:GetStrangerOutputDLL(sProjectURL, p)
               IF SELF:_projectOutputDLLs:ContainsKey(sProjectURL)
                  // when the output file name of the referenced project has changed
                  // then remove the old name
                  IF outputFile:ToLower() != SELF:_projectOutputDLLs:Item[sProjectURL]:ToLower()
                     IF XSettings.EnableReferenceInfoLog
                        WriteOutputMessage(i"DLL has been renamed to {outputFile} so remove the old DLL {SELF:_projectOutputDLLs:Item[sProjectURL]}")
                     ENDIF
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
            IF XSettings.EnableReferenceInfoLog
               WriteOutputMessage("<-- RefreshStrangerProjectDLLOutputFiles()")
            ENDIF
         ENDIF
      
      PRIVATE METHOD ResolveUnprocessedStrangerReferences() AS VOID
         LOCAL existing AS List<STRING>
         LOCAL p AS EnvDTE.Project
         LOCAL outputFile AS STRING
         IF SELF:_unprocessedStrangerProjectReferences:Count > 0 .AND. ! XSettings.DisableForeignProjectReferences
            IF XSettings.EnableReferenceInfoLog
               WriteOutputMessage("ResolveUnprocessedStrangerReferences()" +_unprocessedStrangerProjectReferences:Count:ToString())
            ENDIF
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
         LOCAL xamlCodeBehindFile AS STRING
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
         RETURN TRUE
      
      
      METHOD FindXFile(fullPath AS STRING) AS XFile
         IF ! String.IsNullOrEmpty(fullPath)
            VAR file := SELF:_SourceFilesDict:Find(fullPath,SELF)
            IF file == NULL
               file := SELF:_OtherFilesDict:Find(fullPath,SELF)
            ENDIF
            RETURN file
         ENDIF
         RETURN NULL


      METHOD RemoveFile(url AS STRING) AS VOID
         IF ! String.IsNullOrEmpty(url)
            IF SELF:_OtherFilesDict:Remove(url)
               VAR file := XFile{url, SELF}
               IF file:IsXaml
                  url := file:XamlCodeBehindFile
               ENDIF
            ENDIF
            SELF:_SourceFilesDict:Remove( url)
         ENDIF
         #endregion
      
      #region Lookup Types and Functions
      METHOD FindGlobalMembersInAssemblyReferences(name AS STRING) AS IList<IXMember>
        IF XSettings.EnableTypelookupLog
            WriteOutputMessage(i"FindGlobalMembersInAssemblyReferences {name} ")
        ENDIF
         VAR result := List<IXMember>{}
         FOREACH VAR asm IN AssemblyReferences
            IF !String.IsNullOrEmpty(asm:GlobalClassName)
               VAR type := asm:GetType(asm.GlobalClassName)
               IF type != NULL
                  VAR methods := type:GetMember(name)
                  IF methods.Length > 0
                     result:AddRange(methods)
                  ENDIF
               ENDIF
            ENDIF
         NEXT
         IF XSettings.EnableTypelookupLog
            WriteOutputMessage(i"FindGlobalMembersInAssemblyReferences {name}, found {result.Count} occurences")
         ENDIF
         RETURN result
         
      
      
      METHOD FindFunction(name AS STRING, lRecursive := TRUE AS LOGIC) AS IXMember
         // we look in the project references and assembly references
         // pass the list of ProjectIds and AssemblyIds to the database engine
        IF XSettings.EnableTypelookupLog
            WriteOutputMessage(ie"FindFunction {name} ")
        ENDIF
         
         VAR projectIds    := SELF:Id:ToString()
         IF lRecursive
            projectIds    := SELF:DependentProjectList
         ENDIF
         VAR result := XDatabase.FindFunction(name, projectIds)
         VAR xmember := GetGlobalMember(result)
         IF XSettings.EnableTypelookupLog
               WriteOutputMessage(ie"FindFunction {name}, result {iif (xmember != NULL, xmember.FullName, \"not found\"} ")
         ENDIF
         RETURN xmember
         
      METHOD FindGlobalOrDefine(name AS STRING, lRecursive := TRUE AS LOGIC) AS IXMember
         // we look in the project references and assembly references
         // pass the list of ProjectIds and AssemblyIds to the database engine
         IF XSettings.EnableTypelookupLog
              WriteOutputMessage(ie"FindGlobalOrDefine {name}")
         ENDIF
         VAR projectIds    := SELF:Id:ToString()
         IF lRecursive
            projectIds    := SELF:DependentProjectList
         ENDIF
         VAR result := XDatabase.FindGlobalOrDefine(name, projectIds)
         VAR xmember := GetGlobalMember(result)
         IF XSettings.EnableTypelookupLog
               WriteOutputMessage(ie"FindGlobalOrDefine {name}, result {iif (xmember != NULL, xmember.FullName, \"not found\"} ")
         ENDIF
         RETURN xmember         
         
      PRIVATE METHOD GetGlobalMember(result AS IList<XDbResult>) AS IXMember
         IF result:Count > 0
            // Get the source code and parse it into a member
            // we know that it will be of the globals class
            VAR element    := result:First()
            VAR source     := element:SourceCode
            VAR file       := XFile{element:FileName,SELF}
            file:Virtual   := TRUE
            VAR walker := SourceWalker{file}
            walker:Parse(source, FALSE)
            IF walker:EntityList:Count > 0
               VAR xElement      := walker:EntityList:First()
               IF xElement IS XMemberDefinition VAR xmember
                  xmember:Range       := TextRange{element:StartLine, element:StartColumn, element:EndLine, element:EndColumn}
                  xmember:Interval    := TextInterval{element:Start, element:Stop}
                  xmember:XmlComments := element:XmlComments
                  RETURN xmember
               ENDIF
            ENDIF
         ENDIF
         RETURN NULL    
      
      METHOD FindSystemTypesByName(typeName AS STRING, usings AS IReadOnlyList<STRING>) AS IList<XTypeReference>
         usings := AdjustUsings(REF typeName, usings)
         VAR result := XDatabase.GetReferenceTypes(typeName, SELF:DependentAssemblyList )
         result := FilterUsings(result,usings)
         RETURN GetRefType(result)
         
         
         
      PRIVATE METHOD GetRefType(found AS IList<XDbResult>) AS IList<XTypeReference>
         LOCAL IdAssembly := -1 AS INT64
         LOCAL fullTypeName:= ""  AS STRING
         VAR result := List<XTypeReference>{}
         FOREACH VAR element IN found
            // Skip types found in another project
            fullTypeName := element:FullName
            IdAssembly   := element:IdAssembly
            VAR name    := element:TypeName
            VAR idType  := element:IdType
            FOREACH VAR asm IN SELF:AssemblyReferences
               IF asm:Id == IdAssembly
                  IF asm:Types:ContainsKey(fullTypeName)
                     result:Add(asm:Types[fullTypeName])
                  ENDIF
                  EXIT
               ENDIF
            NEXT
         NEXT
         RETURN result
         
      
      METHOD FindSystemType(name AS STRING, usings AS IList<STRING>) AS XTypeReference
         IF XSettings.EnableTypelookupLog
            WriteOutputMessage("FindSystemType() "+name)
         ENDIF
         IF ! XSettings.DisableForeignProjectReferences
            SELF:RefreshStrangerProjectDLLOutputFiles()
         ENDIF
         SELF:ResolveReferences()
         VAR type := SystemTypeController.FindType(name, usings, SELF:_AssemblyReferences)
         IF XSettings.EnableTypelookupLog
            IF type != NULL
               WriteOutputMessage("FindSystemType() "+name+" found "+type:FullName)
            ELSE
               WriteOutputMessage("FindSystemType() "+name+" not found ")
            ENDIF
         ENDIF
         RETURN type
         
      METHOD GetAssemblyNamespaces() AS IList<STRING>
         RETURN SystemTypeController.GetNamespaces(SELF:_AssemblyReferences)
         
         
      METHOD Lookup(typeName AS STRING) AS XTypeDefinition
         VAR usings := List<STRING>{}
         RETURN Lookup(typeName, usings)
         
      PRIVATE METHOD AdjustUsings(typeName REF STRING, usings AS IReadOnlyList<STRING>) AS IReadOnlyList<STRING>
         VAR pos := typeName:LastIndexOf(".")
         VAR myusings := List<STRING>{}
         myusings:AddRange(usings)
         myusings:AddRange(SELF:ImplicitNamespaces)
         IF pos > 0 .AND. ! typeName:EndsWith(".")
            VAR ns   := typeName:Substring(0,pos)
            typeName := typeName:Substring(pos+1)
            myusings:Add(ns)
         ENDIF
         RETURN myusings
         
      
      PRIVATE _lastFound := NULL AS XTypeDefinition
      PRIVATE _lastName  := NULL AS STRING
      
      METHOD GetTypes( startWith AS STRING, usings AS IReadOnlyList<STRING>) AS IList<XTypeDefinition>
         VAR result := XDatabase.GetTypesLike(startWith, SELF:DependentProjectList)
         result := FilterUsings(result,usings)
         VAR types := SELF:GetTypeList(result)
         RETURN types
                  
         
      
      METHOD Lookup(typeName AS STRING, usings AS IReadOnlyList<STRING>) AS XTypeDefinition
         IF XSettings.EnableTypelookupLog
            WriteOutputMessage(i"Lookup {typeName}")
         ENDIF
         VAR originalName := typeName
         IF originalName == _lastName
            RETURN _lastFound
         ENDIF
         usings := AdjustUsings(REF typeName, usings)
         VAR pos := typeName:IndexOf('<')
         IF pos > 0
            typeName := typeName:Substring(0, pos)
         ENDIF
         
         VAR result := XDatabase.GetTypes(typeName, SELF:DependentProjectList) 
         result := FilterUsings(result,usings)
         _lastFound := GetType(result)
         _lastName  := originalName
         IF XSettings.EnableTypelookupLog
            WriteOutputMessage(ie"Lookup {typeName}, result {iif(_lastFound != NULL, _lastFound.FullName, \"not found\" } ")
         ENDIF
         RETURN _lastFound
         
      METHOD LookupReferenced(typeName AS STRING) AS XTypeDefinition
         VAR usings := List<STRING>{}
         RETURN LookupReferenced(typeName, usings)
         
      METHOD LookupReferenced(typeName AS STRING, usings AS IReadOnlyList<STRING>) AS XTypeDefinition
        IF XSettings.EnableTypelookupLog
            WriteOutputMessage(i"LookupReferenced {typeName}")
         ENDIF
         VAR originalName := typeName
         IF originalName == _lastName
            RETURN _lastFound
         ENDIF
         usings := AdjustUsings(REF typeName, usings)
         VAR result := XDatabase.GetTypes(typeName, SELF:DependentProjectList)
         result := FilterUsings(result,usings)
         _lastFound := GetType(result)
         _lastName  := originalName
         IF XSettings.EnableTypelookupLog
            WriteOutputMessage(ie"LookupReferenced {typeName}, result {iif(_lastFound != NULL, _lastFound.FullName, \"not found\" } ")
         ENDIF
          
         RETURN _lastFound
         
      METHOD FilterUsings(list AS IList<XDbResult> , usings AS IReadOnlyList<STRING>) AS IList<XDbResult>
         VAR result := List<XDbResult>{}
         FOREACH VAR element IN list
            IF String.IsNullOrEmpty(element:Namespace)
               result:Add(element)
            ELSEIF usings:Contains(element:Namespace)
               result:Add(element)
            ELSE
               NOP   // namespace does not match
            ENDIF
         NEXT
         RETURN result
         
      PRIVATE METHOD GetType(found AS IList<XDbResult>) AS XTypeDefinition
         VAR result := List<XTypeDefinition>{}
         LOCAL idProject := -1 AS INT64
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
            fullTypeName := element:Namespace+"."+element:TypeName
            idProject   := element:IdProject  
            VAR name    := element:TypeName
            VAR idType  := element:IdType
            VAR file       := XFile{element:FileName,SELF}
            file:Virtual   := TRUE
            file:Id        := element:IdFile
            VAR members := XDatabase.GetMembers(idType):ToArray()
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
                  VAR xmembers := xtype:XMembers:ToArray()
                  IF xmembers:Length == members:Length
                     LOCAL i AS INT
                     FOR i := 0 TO members:Length-1
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
         // note result is a collection, so zero based.
         IF result:Count == 1
            RETURN result[0]
         ELSEIF result:Count == 0
            RETURN NULL
         ELSE // merge the types
            VAR xType := result[0]
            FOREACH VAR xOther IN result
               IF xOther != xType 
                  xType := xType:Merge(xOther)
               ENDIF
            NEXT
            RETURN xType
         ENDIF
        
         
  PRIVATE METHOD GetTypeList(found AS IList<XDbResult>) AS IList<XTypeDefinition>
         VAR result := List<XTypeDefinition>{}
         LOCAL idProject := -1 AS INT64
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
            fullTypeName := element:Namespace+"."+element:TypeName
            idProject   := element:IdProject  
            VAR name    := element:TypeName
            VAR idType  := element:IdType
            VAR file       := XFile{element:FileName,SELF}
            file:Virtual   := TRUE
            file:Id        := element:IdFile
            VAR range    := TextRange{element:StartLine, element:StartColumn, element:EndLine, element:EndColumn}
            VAR interval := TextInterval{element:Start, element:Stop}
            VAR xtype := XTypeDefinition{name, element:Kind,element:Attributes, range, interval, file}
            xtype:Namespace := element:Namespace
            xtype:Id  := element:IdType
            result:Add(xtype)
         NEXT
         RETURN result
         
         
      PRIVATE METHOD GetTypeSource(element AS XDbResult, members AS IList<XDbResult>) AS STRING
         VAR sb := StringBuilder{}
         sb:AppendLine(element:SourceCode)
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
         RETURN sb:ToString()
      
      METHOD GetExtensions( typeName AS STRING) AS IList<IXMember>
         RETURN SystemTypeController.LookForExtensions( typeName, SELF:_AssemblyReferences)
         
         #endregion
      
      METHOD UnLoad() AS VOID
         SELF:Loaded := FALSE
         IF XSettings.EnableReferenceInfoLog
            WriteOutputMessage("UnLoad() ")
         ENDIF
         FOREACH VAR asm IN SELF:_AssemblyReferences
            asm:RemoveProject(SELF)
         NEXT
         SELF:_AssemblyReferences:Clear()
         SystemTypeController.UnloadUnusedAssemblies()

         
         
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
      PROPERTY AssemblyReferences AS List<XAssembly>
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
      
      PROPERTY Namespaces AS IList<STRING>
         GET
            VAR list := XDatabase.GetNamespaces(SELF:DependentProjectList)
            VAR result := List<STRING>{}
            FOREACH VAR db IN list
               result:Add(db:Namespace)
            NEXT
            RETURN result
         END GET
      END PROPERTY
      
      PROPERTY OtherFiles AS List<STRING> GET SELF:_OtherFilesDict:Keys:ToList()
      
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
      
      PROPERTY StrangerProjects AS IList<EnvDTE.Project>
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
      
      CLASS XFileDictionary
         PROTECTED dict AS ConcurrentDictionary<STRING, INT64>
         CONSTRUCTOR()
            dict := ConcurrentDictionary<STRING, INT64>{StringComparer.OrdinalIgnoreCase}
            
         METHOD Add(fileName AS STRING) AS VOID
           IF !dict:ContainsKey(fileName)
               dict:TryAdd(fileName,-1)
           ENDIF
            
         METHOD Find(fileName AS STRING, project AS XProject) AS XFile
            IF dict:ContainsKey(fileName)
               VAR result := XFile{fileName, project}
               XDatabase.Read(result)
               dict[fileName] := result:Id
               RETURN result
            ENDIF
            RETURN NULL

         METHOD Remove(fileName AS STRING) AS LOGIC
            IF dict:ContainsKey(fileName)
               RETURN dict:TryRemove(fileName, OUT VAR _)
            ENDIF
            RETURN FALSE        
         PROPERTY Keys AS ICollection<STRING> GET dict:Keys
      END CLASS
      
   END CLASS
   
END NAMESPACE

