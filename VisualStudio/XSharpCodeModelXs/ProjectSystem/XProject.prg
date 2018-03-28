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
		private _AssemblyReferences := List<AssemblyInfo>{} as List<AssemblyInfo>
		private _loaded as logic
		private _parseOptions := null as XSharpParseOptions
		private _projectNode as IXSharpProject
		private _projectOutputDLLs := Dictionary<string, string>{StringComparer.OrdinalIgnoreCase} as Dictionary<string, string>
		private _ReferencedProjects := List<XProject>{} as List<XProject>
		private _StrangerProjects := List<Project>{} as List<Project>
		private _typeController as SystemTypeController
		private _unprocessedProjectReferences := List<string>{} as List<string>
		private _unprocessedStrangerProjectReferences := List<string>{} as List<string>
		public  FileWalkComplete as XProject.OnFileWalkComplete
		private xOtherFilesDict as ConcurrentDictionary<string, XFile>
		private xSourceFilesDict as ConcurrentDictionary<string, XFile>
		
		// Methods
		constructor(project as IXSharpProject)
			super()
			//
			self:_projectNode := project
			self:xSourceFilesDict := ConcurrentDictionary<string, XFile>{StringComparer.OrdinalIgnoreCase}
			self:xOtherFilesDict := ConcurrentDictionary<string, XFile>{StringComparer.OrdinalIgnoreCase}
			self:_typeController := SystemTypeController{}
			self:_loaded := true
			if (self:_projectNode == null)
				
			endif
		
		method AddAssemblyReference(path as string) as void
			local item as AssemblyInfo
			//
			item := SystemTypeController.LoadAssembly(path)
			self:_AssemblyReferences:Add(item)
			item:AddProject(self)
		
		method AddAssemblyReference(reference as VSLangProj.Reference) as void
			local item as AssemblyInfo
			//
			item := SystemTypeController.LoadAssembly(reference)
			self:_AssemblyReferences:Add(item)
			item:AddProject(self)
		
		method AddFile(filePath as string) as logic
			local xFile as XFile
			//
			xFile := XFile{filePath}
			return self:AddFile(xFile)
		
		method AddFile(xFile as XFile) as logic
			local file as XFile
			local xamlCodeBehindFile as string
			//
			if (xFile != null)
				//
				if (xFile:IsSource)
					//
					if (self:xSourceFilesDict:ContainsKey(xFile:FullPath))
						//
						self:xSourceFilesDict:TryRemove(xFile:FullPath, out file)
					endif
					xFile:Project := self
					return self:xSourceFilesDict:TryAdd(xFile:FullPath, xFile)
				endif
				if (xFile:IsXaml)
					//
					xFile:Project := self
					xamlCodeBehindFile := xFile:XamlCodeBehindFile
					if (self:xSourceFilesDict:ContainsKey(xamlCodeBehindFile))
						//
						self:xSourceFilesDict:TryRemove(xamlCodeBehindFile, out  file)
					endif
					self:xSourceFilesDict:TryAdd(xamlCodeBehindFile, xFile)
					if (self:xOtherFilesDict:ContainsKey(xFile:FullPath))
						//
						self:xOtherFilesDict:TryRemove(xFile:FullPath, out file)
					endif
					xFile:Project := self
					return self:xOtherFilesDict:TryAdd(xFile:FullPath, xFile)
				endif
				if (self:xOtherFilesDict:ContainsKey(xFile:FullPath))
					//
					self:xOtherFilesDict:TryRemove(xFile:FullPath, out file)
				endif
				xFile:Project := self
				return self:xOtherFilesDict:TryAdd(xFile:FullPath, xFile)
			endif
			return false
		
		method AddProjectOutput(sProjectURL as string, sOutputDLL as string) as void
			//
			if (self:_projectOutputDLLs:ContainsKey(sProjectURL))
				//
				self:_projectOutputDLLs:Item[sProjectURL] := sOutputDLL
			else
				//
				self:_projectOutputDLLs:Add(sProjectURL, sOutputDLL)
			endif
		
		method AddProjectReference(url as string) as logic
			//
			if (! self:_unprocessedProjectReferences:Contains(url))
				//
				self:_unprocessedProjectReferences:Add(url)
				return true
			endif
			return false
		
		method AddStrangerProjectReference(url as string) as logic
			//
			if (! self:_unprocessedStrangerProjectReferences:Contains(url))
				//
				self:_unprocessedStrangerProjectReferences:Add(url)
				return true
			endif
			return false
		
		method ClearAssemblyReferences() as void
			//
			foreach info as AssemblyInfo in self:_AssemblyReferences
				//
				info:RemoveProject(self)
			next
			self:_AssemblyReferences:Clear()
		
		method FindFullPath(fullPath as string) as XFile
			//
			if (self:xSourceFilesDict:ContainsKey(fullPath))
				//
				return self:xSourceFilesDict:Item[fullPath]
			endif
			if (self:xOtherFilesDict:ContainsKey(fullPath))
				//
				return self:xOtherFilesDict:Item[fullPath]
			endif
			return null
		
		method FindFunction(name as string) as XTypeMember
			local members as IImmutableList<XTypeMember>
			//
			foreach file as XFile in self:SourceFiles
				//
				members := file:GlobalType:Members
				if (members != null)
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
			//
			self:ResolveProjectReferenceDLLs()
			return self:_typeController:FindType(name, usings, self:_AssemblyReferences)
		
		method GetAssemblyNamespaces() as ImmutableList<string>
			//
			return self:_typeController:GetNamespaces(self:_AssemblyReferences)
		
		private method GetStrangerOutputDLL(sProject as string, p as Project) as string
			var path := ""
			try
				//
				var activeConfiguration := p:ConfigurationManager:ActiveConfiguration
				var item := activeConfiguration:Properties:Item("OutputPath")
				
				if item != null
					path := (string) item:Value
				endif
				foreach group as OutputGroup in activeConfiguration:OutputGroups
					//
					if group:FileCount == 1  .AND. group:CanonicalName == "Built"
						var names := (System.Array) group:Filenames
						foreach str as string	 in names
							path := System.IO.Path.Combine(path, str)
						next
					endif
				next
				if ! System.IO.Path.IsPathRooted(path)
					path := System.IO.Path.Combine(System.IO.Path.GetDirectoryName(sProject), path)
				endif
			catch exception as Exception
				//
				if (System.Diagnostics.Debugger.IsAttached)
					//
					System.Diagnostics.Debug.WriteLine(exception:Message)
				endif
			end try
			return path
		
		method Lookup(typeName as string, caseInvariant as logic) as XType
			local type as XType
			local type2 as XType
			local fileArray as XFile[]
			//
			type := null
			type2 := null
			fileArray := System.Linq.Enumerable.ToArray<XFile>(self:xSourceFilesDict:Values)
			foreach file as XFile in fileArray
				//
				if (file:TypeList != null)
					//
					file:TypeList:TryGetValue(typeName, out type2)
					if (((type2 != null) .AND. ! caseInvariant) .AND. ((type:FullName != typeName) .AND. (type:Name != typeName)))
						//
						type := null
					endif
					if (type2 != null)
						//
						if (! type2:IsPartial)
							//
							return type2
						endif
						if (type != null)
							//
							type := type:Merge(type2)
						else
							//
							type := type2:Duplicate()
						endif
					endif
				endif
			next
			return type
		
		method LookupForStranger(typeName as string, caseInvariant as logic) as CodeElement
			//
			return null
		
		method LookupFullName(typeName as string, caseInvariant as logic) as XType
			local type as XType
			local otherType as XType
			local fileArray as XFile[]
			local type3 as XType
			//
			type := null
			otherType := null
			fileArray := System.Linq.Enumerable.ToArray<XFile>(self:xSourceFilesDict:Values)
			foreach file as XFile in fileArray
				//
				type3 := null
				if (file:TypeList != null)
					//
					if (file:TypeList:TryGetValue(typeName, out type3))
						//
						otherType := type3
						if (! caseInvariant .AND. ((type3:FullName != typeName) .AND. (type3:Name != typeName)))
							//
							otherType := null
						endif
					endif
					if (otherType != null)
						//
						if (! otherType:IsPartial)
							//
							return otherType
						endif
						if (type != null)
							//
							type := type:Merge(otherType)
						else
							//
							type := otherType:Duplicate()
						endif
					endif
				endif
			next
			return type
		
		method LookupFullNameReferenced(typeName as string, caseInvariant as logic) as XType
			local fullName as XType
			//
			fullName := null
			foreach project as XProject in self:ReferencedProjects
				//
				fullName := project:LookupFullName(typeName, caseInvariant)
				if (fullName != null)
					//
					return fullName
				endif
			next
			return fullName
		
		method LookupReferenced(typeName as string, caseInvariant as logic) as XType
			local type as XType
			//
			type := null
			foreach project as XProject in self:ReferencedProjects
				//
				type := project:Lookup(typeName, caseInvariant)
				if (type != null)
					//
					return type
				endif
			next
			return type
		
		method RemoveAssemblyReference(fileName as string) as void
			//
			foreach info as AssemblyInfo in self:_AssemblyReferences
				//
				if (String.Equals(info:FileName, fileName, System.StringComparison.OrdinalIgnoreCase))
					//
					self:_AssemblyReferences:Remove(info)
					exit
					
				endif
			next
		
		method RemoveFile(url as string) as void
			local file as XFile
			local file2 as XFile
			//
			if (self:xOtherFilesDict:ContainsKey(url))
				//
				self:xOtherFilesDict:TryRemove(url, out file)
				if (file:IsXaml)
					//
					url := file:XamlCodeBehindFile
				endif
			endif
			if (self:xSourceFilesDict:ContainsKey(url))
				//
				self:xSourceFilesDict:TryRemove(url, out file2)
			endif
		
		method RemoveProjectOutput(sProjectURL as string) as void
			//
			if (self:_projectOutputDLLs:ContainsKey(sProjectURL))
				//
				self:RemoveProjectReferenceDLL(self:_projectOutputDLLs:Item[sProjectURL])
				self:_projectOutputDLLs:Remove(sProjectURL)
			endif
		
		method RemoveProjectReference(url as string) as logic
			local item as XProject
			local outputFile as string
			//
			if (self:_unprocessedProjectReferences:Contains(url))
				//
				self:_unprocessedProjectReferences:Remove(url)
				return true
			endif
			item := XSolution.FindProject(url)
			if (self:_ReferencedProjects:Contains(item))
				//
				outputFile := item:ProjectNode:OutputFile
				self:_ReferencedProjects:Remove(item)
				return true
			endif
			self:RemoveProjectOutput(url)
			return false
		
		method RemoveProjectReferenceDLL(DLL as string) as void
			//
			self:RemoveAssemblyReference(DLL)
		
		method RemoveStrangerProjectReference(url as string) as logic
			local item as Project
			//
			if (self:_unprocessedStrangerProjectReferences:Contains(url))
				//
				self:_unprocessedStrangerProjectReferences:Remove(url)
				return true
			endif
			self:RemoveProjectOutput(url)
			item := self:ProjectNode:FindProject(url)
			if ((item != null) .AND. self:_StrangerProjects:Contains(item))
				//
				self:_StrangerProjects:Remove(item)
				return true
			endif
			return false
		
		method ResolveProjectReferenceDLLs() as void
			//
			if (self:hasUnprocessedReferences)
				//
				self:ResolveUnprocessedProjectReferences()
				self:ResolveUnprocessedStrangerReferences()
			endif
			foreach str as string in self:_projectOutputDLLs:Values
				//
				if (SystemTypeController.FindAssemblyByLocation(str) == null)
					//
					self:AddAssemblyReference(str)
				endif
			next
		
		private method ResolveUnprocessedProjectReferences() as void
			local list as List<string>
			local item as XProject
			local outputFile as string
			//
			if (self:_unprocessedProjectReferences:Count != 0)
				//
				list := List<string>{}
				foreach str as string in self:_unprocessedProjectReferences
					//
					item := XSolution.FindProject(str)
					if (item != null)
						//
						list:Add(str)
						self:_ReferencedProjects:Add(item)
						outputFile := item:ProjectNode:OutputFile
						self:AddProjectOutput(str, outputFile)
					endif
				next
				foreach str3 as string in list
					//
					self:_unprocessedProjectReferences:Remove(str3)
				next
			endif
		
		private method ResolveUnprocessedStrangerReferences() as void
			local list as List<string>
			local item as Project
			local strangerOutputDLL as string
			//
			if (self:_unprocessedStrangerProjectReferences:Count != 0)
				//
				list := List<string>{}
				foreach str as string in self:_unprocessedStrangerProjectReferences
					//
					item := self:ProjectNode:FindProject(str)
					if (item != null)
						//
						list:Add(str)
						self:_StrangerProjects:Add(item)
						strangerOutputDLL := self:GetStrangerOutputDLL(str, item)
						self:AddProjectOutput(str, strangerOutputDLL)
					endif
				next
				foreach str3 as string in list
					//
					self:_unprocessedStrangerProjectReferences:Remove(str3)
				next
			endif
		
		private method SearchInItems(projectItems as ProjectItems, typeName as string, caseInvariant as logic) as CodeElement
			//
			return null
		
		method UnLoad() as void
			//
			self:Loaded := false
			foreach info as AssemblyInfo in self:_AssemblyReferences
				//
				info:RemoveProject(self)
			next
			self:_AssemblyReferences:Clear()
		
		method UpdateAssemblyReference(fileName as string) as void
			//
			SystemTypeController.LoadAssembly(fileName):AddProject(self)
		
		method Walk() as void
			//
			ModelWalker.GetWalker():AddProject(self)
		
		method WalkFile(file as XFile) as void
			//
			ModelWalker.GetWalker():FileWalk(file)
		
		
		// Properties
		property AssemblyReferences as List<AssemblyInfo>
			get
				//
				return self:_AssemblyReferences
			end get
		end property
		
		private property hasUnprocessedReferences as logic
			get
				//
				return ((self:_unprocessedProjectReferences:Count + self:_unprocessedStrangerProjectReferences:Count) > 0)
			end get
		end property
		
		property Loaded as logic
			get
				//
				return self:_loaded
			end get
			set
				//
				self:_loaded := value
			end set
		end property
		
		property Name as string
			get
				//
				return System.IO.Path.GetFileNameWithoutExtension(self:ProjectNode:Url)
			end get
		end property
		
		property Namespaces as ImmutableList<XType>
			get
				//
				var list := List<XType>{}
				var fileArray := self:SourceFiles:ToArray()
				foreach file as XFile in fileArray
					//
					if (file:TypeList != null)
						var values := file:TypeList:Values
						foreach elmt as XType in values
							if elmt:Kind == Kind.Namespace
								if list:Find( { x => x:Name:ToLowerInvariant() == elmt:Name:ToLowerInvariant() } ) == null
									list:Add(elmt)
								endif
							endif
						next
					endif
				next
				return System.Collections.Immutable.ImmutableList.ToImmutableList<XType>(list)
			end get
		end property
		
		property OtherFiles as List<XFile>
			get
				//
				return System.Linq.Enumerable.ToList<XFile>(self:xOtherFilesDict:Values)
			end get
		end property
		
		property ParseOptions as XSharpParseOptions
			get
				//
				if (self:_parseOptions == null)
					//
					if (self:ProjectNode == null)
						//
						self:_parseOptions := XSharpParseOptions.Default
					else
						//
						self:_parseOptions := self:ProjectNode:ParseOptions
					endif
				endif
				return self:_parseOptions
			end get
		end property
		
		property ProjectNode as IXSharpProject get self:_projectNode set self:_projectNode := value
		
		property ReferencedProjects as System.Collections.Immutable.IImmutableList<XProject>
			get
				//
				self:ResolveUnprocessedProjectReferences()
				return System.Collections.Immutable.ImmutableList.ToImmutableList<XProject>(self:_ReferencedProjects)
			end get
		end property
		
		property SourceFiles as List<XFile>
			get
				//
				return System.Linq.Enumerable.ToList<XFile>(self:xSourceFilesDict:Values)
			end get
		end property
		
		property StrangerProjects as System.Collections.Immutable.IImmutableList<Project>
			get
				//
				self:ResolveUnprocessedStrangerReferences()
				return System.Collections.Immutable.ImmutableList.ToImmutableList<Project>(self:_StrangerProjects)
			end get
		end property
		
		
		public delegate OnFileWalkComplete(xFile as XFile) as void
		
	end class
	
end namespace 

