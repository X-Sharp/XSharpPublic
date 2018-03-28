//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System.Collections.Concurrent
using System.Collections.Generic
using System
begin namespace XSharpModel
	static class XSolution
		// Fields
		static private _orphanedFilesProject := null as XProject
		const private OrphanedFiles := "(OrphanedFiles)" as string
		static initonly private xProjects := ConcurrentDictionary<string, XProject>{StringComparer.OrdinalIgnoreCase} as ConcurrentDictionary<string, XProject>
		
		// Methods
		static method Add(project as XProject) as logic
			return Add(project:Name, project)
		
		static method Add(projectName as string, project as XProject) as logic
			if (xProjects:ContainsKey(projectName))
				return false
			endif
			return xProjects:TryAdd(projectName, project)
		
		static method CloseAll() as void
			xProjects:Clear()
			SystemTypeController.Clear()
			if ((_orphanedFilesProject != null) .AND. xProjects:TryAdd("(OrphanedFiles)", _orphanedFilesProject))
				foreach var info in _orphanedFilesProject:AssemblyReferences
					SystemTypeController.LoadAssembly(info:FileName)
				next
			endif
		
		static method FileClose(fileName as string) as void
			if FindFile(fileName):Project == _orphanedFilesProject
				_orphanedFilesProject:RemoveFile(fileName)
			endif
		
		static method FindFile(fileName as string) as XFile
			foreach var project in xProjects
				var file := project:Value:FindFullPath(fileName)
				if file != null
					return file
				endif
			next
			return null
		
		static method FindFullPath(fullPath as string) as XFile
			
			foreach var project in xProjects
				var file := project:Value:FindFullPath(fullPath)
				if file != null
					return file
				endif
			next
			return null
		
		static method FindProject(projectFile as string) as XProject
			local project as XProject
			projectFile := System.IO.Path.GetFileNameWithoutExtension(projectFile)
			if xProjects:TryGetValue(projectFile, out project)
				return project
			endif
			return null
		
		static method Remove(projectName as string) as logic
			local project as XProject
			local flag2 as logic
			if (xProjects:ContainsKey(projectName))
				project := xProjects:Item[projectName]
				project:UnLoad()
				flag2 := xProjects:TryRemove(projectName, out project)
				SystemTypeController.UnloadUnusedAssemblies()
				return flag2
			endif
			return false
		
		static method Remove(project as XProject) as logic
			if project != null
				return Remove(project:Name)
			endif
			return false
		
		static method WalkFile(fileName as string) as void
			local file as XFile
			file := FindFile(fileName)
			if (file != null)
				ModelWalker.GetWalker():FileWalk(file)
			endif
			return 		
		
		// Properties
		static property OrphanedFilesProject as XProject
			get
				if (_orphanedFilesProject == null)
					_orphanedFilesProject := XProject{OrphanedFilesProject{}}
					var projectNode := (OrphanedFilesProject)(_orphanedFilesProject:ProjectNode)
					projectNode:Project := _orphanedFilesProject
					if (xProjects:TryAdd("(OrphanedFiles)", _orphanedFilesProject))
						projectNode:Project:AddAssemblyReference(typeof(string):Assembly:Location)
					endif
				endif
				return _orphanedFilesProject
			end get
		end property
		
		
	end class
	
end namespace 

