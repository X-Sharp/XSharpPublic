//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System.Collections.Concurrent
using System.Collections.Generic
using System
begin namespace XSharpModel
	static ;
		class XSolution
		// Fields
		static private _orphanedFilesProject := null as XProject
		const private OrphanedFiles := "(OrphanedFiles)" as string
		static initonly private xProjects := ConcurrentDictionary<string, XProject>{System.StringComparer.OrdinalIgnoreCase} as ConcurrentDictionary<string, XProject>
		
		// Methods
		static method Add(project as XProject) as logic
			return XSolution.Add(project:Name, project)
		
		static method Add(projectName as string, project as XProject) as logic
			if (XSolution.xProjects:ContainsKey(projectName))
				return false
			endif
			return XSolution.xProjects:TryAdd(projectName, project)
		
		static method CloseAll() as void
			XSolution.xProjects:Clear()
			SystemTypeController.Clear()
			if ((XSolution._orphanedFilesProject != null) .AND. XSolution.xProjects:TryAdd("(OrphanedFiles)", XSolution._orphanedFilesProject))
				foreach var info in XSolution._orphanedFilesProject:AssemblyReferences
					SystemTypeController.LoadAssembly(info:FileName)
				next
			endif
		
		static method FileClose(fileName as string) as void
			if XSolution.FindFile(fileName):Project == XSolution._orphanedFilesProject
				XSolution._orphanedFilesProject:RemoveFile(fileName)
			endif
		
		static method FindFile(fileName as string) as XFile
			foreach var project in XSolution.xProjects
				var file := project:Value:FindFullPath(fileName)
				if file != null
					return file
				endif
			next
			return null
		
		static method FindFullPath(fullPath as string) as XFile
			
			foreach var project in XSolution.xProjects
				var file := project:Value:FindFullPath(fullPath)
				if file != null
					return file
				endif
			next
			return null
		
		static method FindProject(projectFile as string) as XProject
			local project as XProject
			projectFile := System.IO.Path.GetFileNameWithoutExtension(projectFile)
			if XSolution.xProjects:TryGetValue(projectFile, out project)
				return project
			endif
			return null
		
		static method Remove(projectName as string) as logic
			local project as XProject
			local flag2 as logic
			if (XSolution.xProjects:ContainsKey(projectName))
				project := XSolution.xProjects:Item[projectName]
				project:UnLoad()
				flag2 := XSolution.xProjects:TryRemove(projectName, out project)
				SystemTypeController.UnloadUnusedAssemblies()
				return flag2
			endif
			return false
		
		static method Remove(project as XProject) as logic
			if project != null
				return XSolution.Remove(project:Name)
			endif
			return false
		
		static method WalkFile(fileName as string) as void
			local file as XFile
			file := XSolution.FindFile(fileName)
			if (file != null)
				ModelWalker.GetWalker():FileWalk(file)
			endif
			return 		
		
		// Properties
		static property OrphanedFilesProject as XProject
			get
				if (XSolution._orphanedFilesProject == null)
					XSolution._orphanedFilesProject := XProject{OrphanedFilesProject{}}
					var projectNode := (OrphanedFilesProject)(XSolution._orphanedFilesProject:ProjectNode)
					projectNode:Project := XSolution._orphanedFilesProject
					if (XSolution.xProjects:TryAdd("(OrphanedFiles)", XSolution._orphanedFilesProject))
						projectNode:Project:AddAssemblyReference(typeof(string):Assembly:Location)
					endif
				endif
				return XSolution._orphanedFilesProject
			end get
		end property
		
		
	end class
	
end namespace 

