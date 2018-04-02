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

begin namespace XSharpModel
	[DebuggerDisplay("{Name,nq}")];
	class XProject
		// Fields
		private _AssemblyReferences						as List<AssemblyInfo>
		private _parseOptions := null					as XSharpParseOptions
		private _projectNode							as IXSharpProject
		private _projectOutputDLLs						as Dictionary<string, string>
		private _ReferencedProjects						as List<XProject>
		private _StrangerProjects						as List<Project>
		private _typeController							as SystemTypeController
		private _unprocessedAssemblyReferences			as Dictionary<string, string>
		private _unprocessedProjectReferences			as List<string>
		private _unprocessedStrangerProjectReferences	as List<string>
		public  FileWalkComplete						as XProject.OnFileWalkComplete
		private xOtherFilesDict							as ConcurrentDictionary<string, XFile>
		private xSourceFilesDict						as ConcurrentDictionary<string, XFile>
		
		// Methods
		constructor(project as IXSharpProject)
			super()
			self:_AssemblyReferences := List<AssemblyInfo>{} 
			self:_unprocessedAssemblyReferences := Dictionary<string, string>{StringComparer.OrdinalIgnoreCase} 
			self:_unprocessedProjectReferences := List<string>{} 
			self:_unprocessedStrangerProjectReferences := List<string>{} 
			self:_projectOutputDLLs := Dictionary<string, string>{StringComparer.OrdinalIgnoreCase} 

			self:_ReferencedProjects := List<XProject>{} 
			SELF:_StrangerProjects := List<Project>{} 
			self:_projectNode := project
			self:xSourceFilesDict := ConcurrentDictionary<string, XFile>{StringComparer.OrdinalIgnoreCase}
			self:xOtherFilesDict := ConcurrentDictionary<string, XFile>{StringComparer.OrdinalIgnoreCase}
			self:_typeController := SystemTypeController{}
			self:Loaded := true
			if (self:_projectNode == null)
				
			endif

		#region AssemblyReferences		
		
		method AddAssemblyReference(path as string) as void
			if ! _unprocessedAssemblyReferences:ContainsKey(path)
				_unprocessedAssemblyReferences.Add(path,path)
			endif
		
		method AddAssemblyReference(reference as VSLangProj.Reference) as void
			local assemblyInfo as AssemblyInfo
			if ! String.IsNullorEmpty(reference:Path)
				AddAssemblyReference(reference:Path)
			else
				assemblyInfo := SystemTypeController.LoadAssembly(reference)
				self:_AssemblyReferences:Add(assemblyInfo)
				assemblyInfo:AddProject(self)
			endif

		method ClearAssemblyReferences() as void
			foreach asm as AssemblyInfo in self:_AssemblyReferences
				asm:RemoveProject(self)
			next
			self:_AssemblyReferences:Clear()


		method RemoveAssemblyReference(fileName as string) as void
			
			if _unprocessedAssemblyReferences:ContainsKey(fileName)
				_unprocessedAssemblyReferences:Remove(fileName)
			endif
			foreach info as AssemblyInfo in self:_AssemblyReferences
				if String.Equals(info:FileName, fileName, System.StringComparison.OrdinalIgnoreCase)
					self:_AssemblyReferences:Remove(info)
					exit
				endif
			next

		private method ResolveUnprocessedAssemblyReferences() as void
			local loaded as List<string>
			//
			if self:_unprocessedAssemblyReferences:Count != 0
				loaded := List<string>{}
				foreach path as string in self:_unprocessedAssemblyReferences:Keys
					local assemblyInfo as AssemblyInfo
					if System.IO.File.Exists(path)
						assemblyInfo := SystemTypeController.LoadAssembly(path)
						self:_AssemblyReferences:Add(assemblyInfo)
						assemblyInfo:AddProject(self)
						loaded:Add(path)
					endif
				next
				foreach path as string in Loaded
					self:_unprocessedAssemblyReferences:Remove(path)
				next
			endif
			return 
		

		#endregion

		#region ProjectReferences

		method AddProjectReference(url as string) as logic
			//
			if ! self:_unprocessedProjectReferences:Contains(url)
				//
				self:_unprocessedProjectReferences:Add(url)
				return true
			endif
			return false
		method AddProjectOutput(sProjectURL as string, sOutputDLL as string) as void
			if self:_projectOutputDLLs:ContainsKey(sProjectURL)
				self:_projectOutputDLLs:Item[sProjectURL] := sOutputDLL
			else
				self:_projectOutputDLLs:Add(sProjectURL, sOutputDLL)
			endif

		method RemoveProjectOutput(sProjectURL as string) as void
			if self:_projectOutputDLLs:ContainsKey(sProjectURL)
				self:RemoveProjectReferenceDLL(self:_projectOutputDLLs:Item[sProjectURL])
				self:_projectOutputDLLs:Remove(sProjectURL)
			endif
		
		method RemoveProjectReference(url as string) as logic
			local prj as XProject
			local outputname as string
			if self:_unprocessedProjectReferences:Contains(url)
				self:_unprocessedProjectReferences:Remove(url)
				return true
			endif
			// Does this url belongs to a project in the Solution ?
			prj := XSolution.FindProject(url)
			if (self:_ReferencedProjects:Contains(prj))
				//
				outputname := prj:ProjectNode:OutputFile
				self:_ReferencedProjects:Remove(prj)
				return true
			endif
			self:RemoveProjectOutput(url)
			return false
		
		method RemoveProjectReferenceDLL(DLL as string) as void
			self:RemoveAssemblyReference(DLL)

		private method ResolveUnprocessedProjectReferences() as void
			local existing as List<string>
			local p as XProject
			local outputFile as string
			if self:_unprocessedProjectReferences:Count != 0
				existing := List<string>{}
				foreach sProject as string in self:_unprocessedProjectReferences
					p := XSolution.FindProject(sProject)
					if p != null
						existing:Add(sProject)
						self:_ReferencedProjects:Add(p)
						outputFile := p:ProjectNode:OutputFile
						self:AddProjectOutput(sProject, outputFile)
					endif
				next
				foreach sProject as string in existing
					//
					self:_unprocessedProjectReferences:Remove(sProject)
				next
			endif

		#endregion

		#region References to other project systems
		method AddStrangerProjectReference(url as string) as logic
			//
			if (! self:_unprocessedStrangerProjectReferences:Contains(url))
				//
				self:_unprocessedStrangerProjectReferences:Add(url)
				return true
			endif
			return false

		private method GetStrangerOutputDLL(sProject as string, p as Project) as string
			var outputFile := ""
			try
				var activeConfiguration := p:ConfigurationManager:ActiveConfiguration
				var item := activeConfiguration:Properties:Item("OutputPath")
				var path := ""
				if item != null
					path := (string) item:Value
				endif
				foreach group as OutputGroup in activeConfiguration:OutputGroups
					if group:FileCount == 1  .AND. group:CanonicalName == "Built"
						var names := (System.Array) group:Filenames
						foreach str as string	 in names
							outputFile := System.IO.Path.Combine(path, str)
						next
					endif
				next
				if ! System.IO.Path.IsPathRooted(path)
					outputFile := System.IO.Path.Combine(System.IO.Path.GetDirectoryName(sProject), path)
				endif
			catch exception as Exception
				if (System.Diagnostics.Debugger.IsAttached)
					System.Diagnostics.Debug.WriteLine(exception:Message)
				endif
			end try
			return outputFile
		
		method LookupForStranger(typeName as string, caseInvariant as logic) as CodeElement
			return null


		method RemoveStrangerProjectReference(url as string) as logic
			local prj as Project
			if self:_unprocessedStrangerProjectReferences:Contains(url)
				self:_unprocessedStrangerProjectReferences:Remove(url)
				return true
			endif
			self:RemoveProjectOutput(url)
			prj := self:ProjectNode:FindProject(url)
			if prj != null .AND. self:_StrangerProjects:Contains(prj)
				self:_StrangerProjects:Remove(prj)
				return true
			endif
			return false


		private method ResolveUnprocessedStrangerReferences() as void
			local existing as List<string>
			local p as Project
			local outputFile as string
			if self:_unprocessedStrangerProjectReferences:Count != 0
				existing := List<string>{}
				foreach sProject as string in self:_unprocessedStrangerProjectReferences
					p := self:ProjectNode:FindProject(sProject)
					if (p != null)
						existing:Add(sProject)
						self:_StrangerProjects:Add(p)
			            outputFile := SELF:GetStrangerOutputDLL(sProject, p)
			            SELF:AddProjectOutput(sProject, outputFile)
					endif
				next
				foreach sProject as string in existing
					self:_unprocessedStrangerProjectReferences:Remove(sProject)
				next
			endif

		#endregion
			
		method ResolveProjectReferenceDLLs() as void
			if self:hasUnprocessedReferences
				System.Diagnostics.Trace.WriteLine("<<-- ResolveProjectReferenceDLLs()")
				try
					self:ProjectNode:SetStatusBarText("Resolving references for project "+self:Name)
					Self:ProjectNode:SetStatusBarAnimation(true,6) // Searching see shell.idl
					self:ResolveUnprocessedAssemblyReferences()
					self:ResolveUnprocessedProjectReferences()
					self:ResolveUnprocessedStrangerReferences()
					foreach DLL as string in self:_projectOutputDLLs:Values
						if SystemTypeController.FindAssemblyByLocation(DLL) == null
							self:AddAssemblyReference(DLL)
						endif
					next
					// repeat the assemblyreferences because we can have _projectOutputDLLs added to the list
					self:ResolveUnprocessedAssemblyReferences()
				finally
					self:ProjectNode:SetStatusBarText("")
					self:ProjectNode:SetStatusBarAnimation(false,6)
				end try
				System.Diagnostics.Trace.WriteLine(">>-- ResolveProjectReferenceDLLs()")
			endif
			return
			
		#region 'Normal' Files
		method AddFile(filePath as string) as logic
			return self:AddFile(XFile{filePath})
		
		method AddFile(xFile as XFile) as logic
			local file as XFile
			local xamlCodeBehindFile as string
			if xFile != null
				xFile:Project := self
				if xFile:IsSource
					if self:xSourceFilesDict:ContainsKey(xFile:FullPath)
						self:xSourceFilesDict:TryRemove(xFile:FullPath, out file)
					endif
					return self:xSourceFilesDict:TryAdd(xFile:FullPath, xFile)
				endif
				if xFile:IsXaml
					xamlCodeBehindFile := xFile:XamlCodeBehindFile
					if self:xSourceFilesDict:ContainsKey(xamlCodeBehindFile)
						self:xSourceFilesDict:TryRemove(xamlCodeBehindFile, out  file)
					endif
					self:xSourceFilesDict:TryAdd(xamlCodeBehindFile, xFile)
					if self:xOtherFilesDict:ContainsKey(xFile:FullPath)
						self:xOtherFilesDict:TryRemove(xFile:FullPath, out file)
					endif
					return self:xOtherFilesDict:TryAdd(xFile:FullPath, xFile)
				endif
				if self:xOtherFilesDict:ContainsKey(xFile:FullPath)
					self:xOtherFilesDict:TryRemove(xFile:FullPath, out file)
				endif
				return self:xOtherFilesDict:TryAdd(xFile:FullPath, xFile)
			endif
			return false

		method FindFullPath(fullPath as string) as XFile
			if self:xSourceFilesDict:ContainsKey(fullPath)
				return self:xSourceFilesDict:Item[fullPath]
			endif
			if self:xOtherFilesDict:ContainsKey(fullPath)
				return self:xOtherFilesDict:Item[fullPath]
			endif
			return null

		method RemoveFile(url as string) as void
			local file as XFile
			if self:xOtherFilesDict:ContainsKey(url)
				self:xOtherFilesDict:TryRemove(url, out file)
				if file:IsXaml
					url := file:XamlCodeBehindFile
				endif
			endif
			if self:xSourceFilesDict:ContainsKey(url)
				self:xSourceFilesDict:TryRemove(url, out file)
			endif
		
		#endregion					
		
		
		method FindFunction(name as string) as XTypeMember
			local members as IImmutableList<XTypeMember>
			foreach file as XFile in self:SourceFiles
				members := file:GlobalType:Members
				if members != null
					//
					foreach oMember as XTypeMember in members
						if (oMember:Kind == Kind.Procedure .or. oMember:Kind == Kind.Function ) .and. string.Compare(oMember:Name, name, true) == 0
							return oMember
						endif
					next
				endif
			next
			return null
		
		method FindSystemType(name as string, usings as IReadOnlyList<string>) as Type
			self:ResolveProjectReferenceDLLs()
			return self:_typeController:FindType(name, usings, self:_AssemblyReferences)
		
		method GetAssemblyNamespaces() as ImmutableList<string>
			return self:_typeController:GetNamespaces(self:_AssemblyReferences)
		
		method Lookup(typeName as string, caseInvariant as logic) as XType
			local xType as XType
			local xTemp as XType
			local aFiles as XFile[]
			//
			xType := null
			xTemp := null
			aFiles := self:xSourceFilesDict:Values:ToArray()
			foreach file as XFile in aFiles
				if file:TypeList != null
					file:TypeList:TryGetValue(typeName, out xTemp)
					if xTemp != null .AND. ! caseInvariant .AND. xType:FullName != typeName .AND. xType:Name != typeName
						xType := null
					endif
					if xTemp != null
						if ! xTemp:IsPartial
							return xTemp
						endif
						if xType != null
							xType := xType:Merge(xTemp)
						else
							xType := XType{xTemp}
						endif
					endif
				endif
			next
			return xType
		
		method LookupFullName(typeName as string, caseInvariant as logic) as XType
			local xType as XType
			local xTemp as XType
			local fileArray as XFile[]
			local x as XType
			//
			xType := null
			xTemp := null
			fileArray := self:xSourceFilesDict:Values:ToArray()
			foreach file as XFile in fileArray
				x := null
				if file:TypeList != null
					if (file:TypeList:TryGetValue(typeName, out x))
						xTemp := x
						if (! caseInvariant .AND. ((x:FullName != typeName) .AND. (x:Name != typeName)))
							xTemp := null
						endif
					endif
					if xTemp != null
						if ! xTemp:IsPartial
							return xTemp
						endif
						if xType != null
							xType := xType:Merge(xTemp)
						else
							xType := XType{xTemp}
						endif
					endif
				endif
			next
			return xType
		
		method LookupFullNameReferenced(typeName as string, caseInvariant as logic) as XType
			local xType as XType
			xType := null
			foreach project as XProject in self:ReferencedProjects
				xType := project:LookupFullName(typeName, caseInvariant)
				if xType != null
					return xType
				endif
			next
			return xType
		
		method LookupReferenced(typeName as string, caseInvariant as logic) as XType
			local xType as XType
			xType := null
			foreach project as XProject in self:ReferencedProjects
				xType := project:Lookup(typeName, caseInvariant)
				if xType != null
					return xType
				endif
			next
			return xType
		
		private method SearchInItems(projectItems as ProjectItems, typeName as string, caseInvariant as logic) as CodeElement
			return null
		
		method UnLoad() as void
			self:Loaded := false
			foreach asm as AssemblyInfo in self:_AssemblyReferences
				asm:RemoveProject(self)
			next
			self:_AssemblyReferences:Clear()
		
		method UpdateAssemblyReference(fileName as string) as void
			SystemTypeController.LoadAssembly(fileName):AddProject(self)
		
		method Walk() as void
			ModelWalker.GetWalker():AddProject(self)
		
		method WalkFile(file as XFile) as void
			ModelWalker.GetWalker():FileWalk(file)
		
		
		// Properties
		property AssemblyReferences as List<AssemblyInfo>
			get
				return self:_AssemblyReferences
			end get
		end property
		
		private property hasUnprocessedReferences as logic
			get
				return self:_unprocessedAssemblyReferences:Count + ;
						self:_unprocessedProjectReferences:Count + ;
						self:_unprocessedStrangerProjectReferences:Count > 0
			end get
		end property
		
		property Loaded as logic auto
		
		property Name as string
			get
				return System.IO.Path.GetFileNameWithoutExtension(self:ProjectNode:Url)
			end get
		end property
		
		property Namespaces as ImmutableList<XType>
			get
				var types := List<XType>{}
				var fileArray := self:SourceFiles:ToArray()
				foreach file as XFile in fileArray
					if file:TypeList != null
						var values := file:TypeList:Values
						foreach elmt as XType in values
							if elmt:Kind == Kind.Namespace
								if types:Find( { x => x:Name:ToLowerInvariant() == elmt:Name:ToLowerInvariant() } ) == null
									types:Add(elmt)
								endif
							endif
						next
					endif
				next
				return types.ToImmutableList()
			end get
		end property
		
		property OtherFiles as List<XFile> get self:xOtherFilesDict:Values:ToList()
		
		property ParseOptions as XSharpParseOptions
			get
				if self:_parseOptions == null
					if self:ProjectNode == null
						self:_parseOptions := XSharpParseOptions.Default
					else
						self:_parseOptions := self:ProjectNode:ParseOptions
					endif
				endif
				return self:_parseOptions
			end get
		end property
		
		property ProjectNode as IXSharpProject get self:_projectNode set self:_projectNode := value
		
		property ReferencedProjects as IImmutableList<XProject>
			get
				self:ResolveUnprocessedProjectReferences()
				return self:_ReferencedProjects.ToImmutableList()
			end get
		end property
		
		property SourceFiles as List<XFile> get self:xSourceFilesDict:Values:ToList()
		
		property StrangerProjects as IImmutableList<Project>
			get
				self:ResolveUnprocessedStrangerReferences()
				return self:_StrangerProjects:ToImmutableList()
			end get
		end property
		
		public delegate OnFileWalkComplete(xFile as XFile) as void
		
	end class
	
end namespace 

