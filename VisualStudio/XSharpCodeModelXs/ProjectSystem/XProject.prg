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
USING Microsoft.VisualStudio
USING Microsoft.VisualStudio.Shell.Interop
USING System.Diagnostics

BEGIN NAMESPACE XSharpModel
	[DebuggerDisplay("{Name,nq}")];
	CLASS XProject
		// Fields
		PRIVATE _AssemblyReferences						AS List<AssemblyInfo>
		PRIVATE _parseOptions := NULL					AS XSharpParseOptions
		PRIVATE _projectNode							AS IXSharpProject
		PRIVATE _projectOutputDLLs						AS Dictionary<STRING, STRING>
		PRIVATE _ReferencedProjects						AS List<XProject>
		PRIVATE _StrangerProjects						AS List<EnvDTE.Project>
		PRIVATE _typeController							AS SystemTypeController
		PRIVATE _unprocessedAssemblyReferences			AS Dictionary<STRING, STRING>
		PRIVATE _unprocessedProjectReferences			AS List<STRING>
		PRIVATE _unprocessedStrangerProjectReferences	AS List<STRING>
		PRIVATE _mergedTypes							AS Dictionary<STRING, XType>
		PUBLIC  FileWalkComplete						AS XProject.OnFileWalkComplete
		PRIVATE _OtherFilesDict							AS ConcurrentDictionary<STRING, XFile>
		PRIVATE _SourceFilesDict						AS ConcurrentDictionary<STRING, XFile>
		PRIVATE _TypeDict								AS Dictionary<STRING, List<STRING>>
		
		
		CONSTRUCTOR(project AS IXSharpProject)
			SUPER()
			SELF:_AssemblyReferences := List<AssemblyInfo>{} 
			SELF:_unprocessedAssemblyReferences := Dictionary<STRING, STRING>{StringComparer.OrdinalIgnoreCase} 
			SELF:_unprocessedProjectReferences := List<STRING>{} 
			SELF:_unprocessedStrangerProjectReferences := List<STRING>{} 
			SELF:_projectOutputDLLs := Dictionary<STRING, STRING>{StringComparer.OrdinalIgnoreCase} 
			SELF:_mergedTypes   := Dictionary<STRING, XType>{StringComparer.OrdinalIgnoreCase} 
			SELF:_ReferencedProjects := List<XProject>{} 
			SELF:_StrangerProjects := List<Project>{} 
			SELF:_projectNode := project
			SELF:_SourceFilesDict := ConcurrentDictionary<STRING, XFile>{StringComparer.OrdinalIgnoreCase}
			SELF:_OtherFilesDict := ConcurrentDictionary<STRING, XFile>{StringComparer.OrdinalIgnoreCase}
			SELF:_typeController := SystemTypeController{}
			SELF:_TypeDict := Dictionary<STRING, List<STRING> > {StringComparer.OrdinalIgnoreCase}
			SELF:Loaded := TRUE
			
		#region AssemblyReferences		
			
		METHOD AddAssemblyReference(path AS STRING) AS VOID
			IF ! _unprocessedAssemblyReferences:ContainsKey(path)
				_unprocessedAssemblyReferences.Add(path,path)
			ENDIF
				
		METHOD AddAssemblyReference(reference AS VSLangProj.Reference) AS VOID
			LOCAL assemblyInfo AS AssemblyInfo
			IF ! String.IsNullorEmpty(reference:Path)
				AddAssemblyReference(reference:Path)
			ELSE
				assemblyInfo := SystemTypeController.LoadAssembly(reference)
				SELF:_AssemblyReferences:Add(assemblyInfo)
				assemblyInfo:AddProject(SELF)
			ENDIF
				
		METHOD ClearAssemblyReferences() AS VOID
			FOREACH asm AS AssemblyInfo IN SELF:_AssemblyReferences
				asm:RemoveProject(SELF)
			NEXT
			SELF:_AssemblyReferences:Clear()
				
				
		METHOD RemoveAssemblyReference(fileName AS STRING) AS VOID
			
			IF _unprocessedAssemblyReferences:ContainsKey(fileName)
				_unprocessedAssemblyReferences:Remove(fileName)
			ENDIF
			FOREACH info AS AssemblyInfo IN SELF:_AssemblyReferences
				IF String.Equals(info:FileName, fileName, System.StringComparison.OrdinalIgnoreCase)
					SELF:_AssemblyReferences:Remove(info)
					EXIT
				ENDIF
			NEXT
				
		PRIVATE METHOD ResolveUnprocessedAssemblyReferences() AS VOID
			LOCAL loaded AS List<STRING>
			//
			IF SELF:_unprocessedAssemblyReferences:Count != 0
				loaded := List<STRING>{}
				FOREACH path AS STRING IN SELF:_unprocessedAssemblyReferences:Keys
					LOCAL assemblyInfo AS AssemblyInfo
					IF System.IO.File.Exists(path)
						assemblyInfo := SystemTypeController.LoadAssembly(path)
						SELF:_AssemblyReferences:Add(assemblyInfo)
						assemblyInfo:AddProject(SELF)
						loaded:Add(path)
					ENDIF
				NEXT
				FOREACH path AS STRING IN Loaded
					IF SELF:_unprocessedAssemblyReferences:ContainsKey(path)
						SELF:_unprocessedAssemblyReferences:Remove(path)
					ENDIF
				NEXT
			ENDIF
			RETURN 
				
		METHOD ResolveProjectReferenceDLLs() AS VOID
			IF SELF:hasUnprocessedReferences
				System.Diagnostics.Trace.WriteLine("<<-- ResolveProjectReferenceDLLs()")
				SELF:ProjectNode:SetStatusBarText(String.Format("Loading referenced types for project {0}", SELF:Name))
				SELF:ProjectNode:SetStatusBarAnimation(TRUE, 0)
					
				TRY
					SELF:ResolveUnprocessedAssemblyReferences()
					SELF:ResolveUnprocessedProjectReferences()
					SELF:ResolveUnprocessedStrangerReferences()
					FOREACH DLL AS STRING IN SELF:_projectOutputDLLs:Values
						IF SystemTypeController.FindAssemblyByLocation(DLL) == NULL
							SELF:AddAssemblyReference(DLL)
						ENDIF
					NEXT
					// repeat the assemblyreferences because we can have _projectOutputDLLs added to the list
					SELF:ResolveUnprocessedAssemblyReferences()
				END TRY
				SELF:ProjectNode:SetStatusBarAnimation(FALSE, 0)
				SELF:ProjectNode:SetStatusBarText("")
				System.Diagnostics.Trace.WriteLine(">>-- ResolveProjectReferenceDLLs()")
			ENDIF
			RETURN
				
				
		METHOD UpdateAssemblyReference(fileName AS STRING) AS VOID
			SystemTypeController.LoadAssembly(fileName):AddProject(SELF)
				
				
		#endregion
				
		#region ProjectReferences
			
		METHOD AddProjectReference(url AS STRING) AS LOGIC
			IF ! SELF:_unprocessedProjectReferences:Contains(url)
				SELF:_unprocessedProjectReferences:Add(url)
				RETURN TRUE
			ENDIF
			RETURN FALSE
				
		METHOD AddProjectOutput(sProjectURL AS STRING, sOutputDLL AS STRING) AS VOID
			IF SELF:_projectOutputDLLs:ContainsKey(sProjectURL)
				SELF:_projectOutputDLLs:Item[sProjectURL] := sOutputDLL
			ELSE
				SELF:_projectOutputDLLs:Add(sProjectURL, sOutputDLL)
			ENDIF
				
		METHOD RemoveProjectOutput(sProjectURL AS STRING) AS VOID
			IF SELF:_projectOutputDLLs:ContainsKey(sProjectURL)
				SELF:RemoveProjectReferenceDLL(SELF:_projectOutputDLLs:Item[sProjectURL])
				SELF:_projectOutputDLLs:Remove(sProjectURL)
			ENDIF
				
		METHOD RemoveProjectReference(url AS STRING) AS LOGIC
			LOCAL prj AS XProject
			LOCAL outputname AS STRING
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
				RETURN TRUE
			ENDIF
			SELF:RemoveProjectOutput(url)
			RETURN FALSE
				
		METHOD RemoveProjectReferenceDLL(DLL AS STRING) AS VOID
			SELF:RemoveAssemblyReference(DLL)
				
		PRIVATE METHOD ResolveUnprocessedProjectReferences() AS VOID
			LOCAL existing AS List<STRING>
			LOCAL p AS XProject
			LOCAL outputFile AS STRING
			IF SELF:_unprocessedProjectReferences:Count != 0
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
			ENDIF
				
		#endregion
				
		#region References to other project systems
		METHOD AddStrangerProjectReference(url AS STRING) AS LOGIC
			IF ! SELF:_unprocessedStrangerProjectReferences:Contains(url)
				SELF:_unprocessedStrangerProjectReferences:Add(url)
				RETURN TRUE
			ENDIF
			RETURN FALSE
		
		PRIVATE METHOD saveGetProperty(props AS EnvDte.Properties, name AS STRING) AS EnvDte.Property
			LOCAL p AS EnvDte.Property
			try
				p := props:Item(name)
			CATCH
				p := NULL
			END TRY
			return p

		PRIVATE METHOD GetStrangerOutputDLL(sProject AS STRING, p AS Project) AS STRING
			VAR outputFile := ""
			TRY
				LOCAL propTypepropName := NULL AS EnvDte.Property
				var propType := saveGetProperty(p:Properties, "OutputType") 
				var propName := saveGetProperty(p:Properties, "AssemblyName") 
				var propPath := saveGetProperty(p:ConfigurationManager:ActiveConfiguration:Properties, "OutputPath")
				IF propName != NULL .and. propPath != NULL .and. propType != null
					VAR path    := (STRING) propPath:Value
					var type    := (int) propType:Value
					outputFile	:= (STRING) propName:Value 
					IF type == 2 // __VSPROJOUTPUTTYPE.VSPROJ_OUTPUTTYPE_LIBRARY
						outputFile	+= ".dll"
					ELSE
						outputFile	+= ".exe"
					endif
					outputFile	:= System.IO.Path.Combine(path, outputFile) 
					IF ! System.IO.Path.IsPathRooted(outputFile)
						outputFile := System.IO.Path.Combine(System.IO.Path.GetDirectoryName(sProject), outputFile)
						// remove ../ and other garbage from the path
						outputFile := System.IO.Path.GetFullPath(outputFile)
					ENDIF
				ENDIF
			CATCH exception AS Exception
				IF (System.Diagnostics.Debugger.IsAttached)
					System.Diagnostics.Debug.WriteLine(exception:Message)
				ENDIF
			END TRY
			RETURN outputFile
			
			
		METHOD RemoveStrangerProjectReference(url AS STRING) AS LOGIC
			LOCAL prj AS Project
			IF SELF:_unprocessedStrangerProjectReferences:Contains(url)
				SELF:_unprocessedStrangerProjectReferences:Remove(url)
				RETURN TRUE
			ENDIF
			SELF:RemoveProjectOutput(url)
			prj := SELF:ProjectNode:FindProject(url)
			IF prj != NULL .AND. SELF:_StrangerProjects:Contains(prj)
				SELF:_StrangerProjects:Remove(prj)
				RETURN TRUE
			ENDIF
			RETURN FALSE

		PRIVATE METHOD RefreshStrangerProjectDLLOutputFiles() AS VOID
			// Check if any DLL has changed
			FOREACH p AS EnvDte.Project IN SELF:_StrangerProjects
				VAR sProjectURL := p:FullName
				VAR mustAdd     := FALSE
				VAR outputFile  := SELF:GetStrangerOutputDLL(sProjectURL, p)
				IF SELF:_projectOutputDLLs:ContainsKey(sProjectURL)
					IF outputFile:ToLower() != SELF:_projectOutputDLLs:Item[sProjectURL]:ToLower()
						SELF:_projectOutputDLLs:Remove(sProjectURL)
						mustAdd := TRUE
					ENDIF
				ELSE
					mustAdd := TRUE
				ENDIF
				IF mustAdd
					SELF:_unprocessedStrangerProjectReferences:Add(sProjectURL)
				ENDIF
			NEXT
			
		PRIVATE METHOD ResolveUnprocessedStrangerReferences() AS VOID
			LOCAL existing AS List<STRING>
			LOCAL p AS Project
			LOCAL outputFile AS STRING
			IF SELF:_unprocessedStrangerProjectReferences:Count != 0
				existing := List<STRING>{}
				FOREACH sProject AS STRING IN SELF:_unprocessedStrangerProjectReferences
					p := SELF:ProjectNode:FindProject(sProject)
					IF (p != NULL)
						SELF:_StrangerProjects:Add(p)
						outputFile := SELF:GetStrangerOutputDLL(sProject, p)
						if !String.IsNullOrEmpty(outputFile)
							existing:Add(sProject)
							SELF:AddProjectOutput(sProject, outputFile)
						endif
					ENDIF
				NEXT
				FOREACH sProject AS STRING IN existing
					SELF:_unprocessedStrangerProjectReferences:Remove(sProject)
				NEXT
			ENDIF
			
			#endregion

		#region 'Normal' Files
		
		METHOD AddFile(filePath AS STRING) AS LOGIC
			RETURN SELF:AddFile(XFile{filePath})
			
		METHOD AddFile(xFile AS XFile) AS LOGIC
			LOCAL xamlCodeBehindFile AS STRING
			IF xFile != NULL
				xFile:Project := SELF
				LOCAL oldFile AS XFile
				IF xFile:IsSource
					IF SELF:_SourceFilesDict:ContainsKey(xFile:FullPath)
						SELF:_SourceFilesDict:TryRemove(xFile:FullPath, OUT oldFile)
					ENDIF
					RETURN SELF:_SourceFilesDict:TryAdd(xFile:FullPath, xFile)
				ENDIF
				IF xFile:IsXaml
					xamlCodeBehindFile := xFile:XamlCodeBehindFile
					IF SELF:_SourceFilesDict:ContainsKey(xamlCodeBehindFile)
						SELF:_SourceFilesDict:TryRemove(xamlCodeBehindFile, OUT  oldFile)
					ENDIF
					SELF:_SourceFilesDict:TryAdd(xamlCodeBehindFile, xFile)
					IF SELF:_OtherFilesDict:ContainsKey(xFile:FullPath)
						SELF:_OtherFilesDict:TryRemove(xFile:FullPath, OUT oldFile)
					ENDIF
					RETURN SELF:_OtherFilesDict:TryAdd(xFile:FullPath, xFile)
				ENDIF
				IF SELF:_OtherFilesDict:ContainsKey(xFile:FullPath)
					SELF:_OtherFilesDict:TryRemove(xFile:FullPath, OUT oldFile)
				ENDIF
				RETURN SELF:_OtherFilesDict:TryAdd(xFile:FullPath, xFile)
			ENDIF
			RETURN FALSE
			
		METHOD FindFullPath(fullPath AS STRING) AS XFile
			IF SELF:_SourceFilesDict:ContainsKey(fullPath)
				RETURN SELF:_SourceFilesDict:Item[fullPath]
			ENDIF
			IF SELF:_OtherFilesDict:ContainsKey(fullPath)
				RETURN SELF:_OtherFilesDict:Item[fullPath]
			ENDIF
			RETURN NULL
			
		METHOD RemoveFile(url AS STRING) AS VOID
			LOCAL file AS XFile
			IF SELF:_OtherFilesDict:ContainsKey(url)
				SELF:_OtherFilesDict:TryRemove(url, OUT file)
				IF file != NULL .and. file:IsXaml
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
			
			#endregion					
			
		#region Lookup Types and Functions
		METHOD FindFunction(name AS STRING) AS XTypeMember
			IF _TypeDict:ContainsKey(XType.GlobalName)
				VAR fileNames := _TypeDict[XType.GlobalName]:ToArray()
				FOREACH sfile AS STRING IN filenames
					VAR xFile := _SourceFilesDict[sFile]
					VAR members := xFile:GlobalType:Members
					IF members != NULL
						FOREACH oMember AS XTypeMember IN members
							IF (oMember:Kind == Kind.Procedure .or. oMember:Kind == Kind.Function ) .and. string.Compare(oMember:Name, name, TRUE) == 0
								RETURN oMember
							ENDIF
						NEXT
					ENDIF
				NEXT
			ENDIF
			RETURN NULL
			
		METHOD FindSystemType(name AS STRING, usings AS IList<STRING>) AS Type
			SELF:RefreshStrangerProjectDLLOutputFiles()
			SELF:ResolveProjectReferenceDLLs()
			RETURN SELF:_typeController:FindType(name, usings, SELF:_AssemblyReferences)
			
		METHOD GetAssemblyNamespaces() AS IList<STRING>
			RETURN SELF:_typeController:GetNamespaces(SELF:_AssemblyReferences)
			
		METHOD Lookup(typeName AS STRING, caseInvariant AS LOGIC) AS XType
			LOCAL xType AS XType
			LOCAL xTemp AS XType
			xType := SELF:LookupMergedType(typeName)
			IF xType != NULL
				RETURN xType
			ENDIF
			xTemp := NULL
			IF _typeDict:ContainsKey(typeName)
				VAR files := _typeDict[typeName]:ToArray()
				FOREACH sfile AS STRING IN files
					VAR file := _SourceFilesDict[sFile]
					IF file:TypeList:TryGetValue(typeName, OUT xTemp ) .and. xTemp  != NULL
						IF (! caseInvariant .AND. ((xTemp:FullName != typeName) .AND. xTemp:Name != typeName))
							xTemp := NULL
						ENDIF
					ENDIF
					IF xTemp != NULL
						IF ! xTemp:IsPartial
							RETURN xTemp
						ENDIF
						IF xType != NULL
							xType := xType:Merge(xTemp)
						ELSE
							xType := XType{xTemp}
						ENDIF
					ENDIF
				NEXT
			ENDIF
			IF xType != NULL
				SELF:AddMergedType(xType)
			ENDIF
			RETURN xType
			
		METHOD LookupReferenced(typeName AS STRING, caseInvariant AS LOGIC) AS XType
			LOCAL xType AS XType
			xType := NULL
			FOREACH project AS XProject IN SELF:ReferencedProjects
				xType := project:Lookup(typeName, caseInvariant)
				IF xType != NULL
					RETURN xType
				ENDIF
			NEXT
			RETURN xType
			
			#endregion
			
		METHOD UnLoad() AS VOID
			SELF:Loaded := FALSE
			FOREACH asm AS AssemblyInfo IN SELF:_AssemblyReferences
				asm:RemoveProject(SELF)
			NEXT
			SELF:_AssemblyReferences:Clear()
			
		METHOD Walk() AS VOID
			ModelWalker.GetWalker():AddProject(SELF)
			
		METHOD WalkFile(file AS XFile) AS VOID
			ModelWalker.GetWalker():FileWalk(file)
			
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
		
		PROPERTY Namespaces AS IList<XType>
			GET
				VAR types := List<XType>{}
				VAR fileArray := SELF:SourceFiles:ToArray()
				FOREACH file AS XFile IN fileArray
					IF file:TypeList != NULL
						VAR values := file:TypeList:Values
						FOREACH elmt AS XType IN values
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
		
		PROPERTY ProjectNode AS IXSharpProject GET SELF:_projectNode SET SELF:_projectNode := VALUE
		
		PROPERTY ReferencedProjects AS ILIst<XProject>
			GET
				SELF:ResolveUnprocessedProjectReferences()
				RETURN SELF:_ReferencedProjects.ToArray()
			END GET
		END PROPERTY
		
		PROPERTY SourceFiles AS List<XFile> GET SELF:_SourceFilesDict:Values:ToList()
		
		PROPERTY StrangerProjects AS IList<Project>
			GET
				SELF:ResolveUnprocessedStrangerReferences()
				RETURN SELF:_StrangerProjects:ToArray()
			END GET
		END PROPERTY
		#endregion
		
		#region Types
		INTERNAL METHOD AddType(xType AS XType) AS VOID
			// only add global types that have members
			IF ! XType.IsGlobalType(xType) .or. xType:Members:Count > 0
				VAR typeName := xType:FullName
				VAR fileName := xType:File:FullPath
				BEGIN LOCK _TypeDict
					IF !SELF:_TypeDict:ContainsKey(typeName)
						SELF:_typeDict[typeName] := List<STRING>{}
					ENDIF
					SELF:_TypeDict[typeName]:Add(fileName)
				END LOCK
			ENDIF
			RETURN
			
		INTERNAL METHOD RemoveType(xType AS XType) AS VOID
			VAR typeName := xType:FullName
			VAR fileName := xType:File:FullPath
			BEGIN LOCK _TypeDict
				IF SELF:_TypeDict:ContainsKey(typeName) 
					IF SELF:_typeDict[typeName]:Contains(fileName)
						SELF:_TypeDict[typeName]:Remove(filename)
					ENDIF
					IF SELF:_TypeDict[typeName]:Count == 0
						SELF:_typeDict:Remove(typeName)
					ENDIF
				ENDIF
			END LOCK
			RETURN
			
			#endregion
		#region Merged Types		
		INTERNAL METHOD AddMergedType(xType AS XType) AS VOID
			IF xType:Name != XElement.GlobalName
				VAR name := xType:FullName
				IF SELF:_mergedTypes:ContainsKey(name)
					SELF:_mergedTypes:Remove(name)
				ENDIF
				SELF:_mergedTypes:Add(name, xType)
			ENDIF
			RETURN
			
		INTERNAL METHOD RemoveMergedType(fullName AS STRING) AS VOID
			IF fullName != XElement.GlobalName
				IF SELF:_mergedTypes:ContainsKey(fullName)
					SELF:_mergedTypes:Remove(fullName)
				ENDIF
			ENDIF
			RETURN
			
		INTERNAL METHOD LookupMergedType(fullName AS STRING) AS XType
			IF _mergedTypes:ContainsKey(fullname)
				RETURN _mergedTypes[fullName]
			ENDIF
			RETURN NULL
			#endregion
	END CLASS
	
END NAMESPACE 

