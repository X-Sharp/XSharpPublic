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
		
		public static OutputWindow as IOutPutWindow
		// Methods
		STATIC CONSTRUCTOR 
			OutputWindow := ModelOutputWindow{}

		STATIC METHOD WriteOutputMessage(message AS STRING) AS VOID
			OutputWindow:DisplayOutPutMessage(message)
		STATIC METHOD WriteException(ex AS Exception) AS VOID
			local space := "" as STRING
			DO WHILE ex != null
				WriteOutputMessage(String.Format("{0}***** exception {1}", space, ex:GetType()))
				WriteOutputMessage(ex:Message)
				WriteOutputMessage(ex:StackTrace)
				space += " "
				ex := ex:InnerException
			ENDDO
			RETURN


		static method Add(project as XProject) as logic
			return Add(project:Name, project)
		
		STATIC METHOD ADD(projectName AS STRING, project AS XProject) AS LOGIC
			WriteOutputMessage("XModel.Solution.Add() "+projectName)
			if xProjects:ContainsKey(projectName)
				return false
			endif
			return xProjects:TryAdd(projectName, project)
		
		STATIC METHOD CloseAll() AS VOID
			WriteOutputMessage("XModel.Solution.CloseAll()")
			xProjects:Clear()
			SystemTypeController.Clear()
			if _orphanedFilesProject != null .AND. xProjects:TryAdd("(OrphanedFiles)", _orphanedFilesProject)
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
			project := null
			if xProjects:TryGetValue(projectFile, out project) .and. project != null
				return project
			endif
			return null
		
		STATIC METHOD REMOVE(projectName AS STRING) AS LOGIC
			WriteOutputMessage("XModel.Solution.Remove() "+projectName)
			if xProjects:ContainsKey(projectName)
				var project := xProjects:Item[projectName]
				project:UnLoad()
				var result := xProjects:TryRemove(projectName, out project)
				SystemTypeController.UnloadUnusedAssemblies()
				return result
			endif
			return false
		
		static method Remove(project as XProject) as logic
			if project != null
				return Remove(project:Name)
			endif
			return false
		
		static method WalkFile(fileName as string) as void
			var file := FindFile(fileName)
			if file != null
				ModelWalker.GetWalker():FileWalk(file)
			endif
			return 		
		
		// Properties
		static property OrphanedFilesProject as XProject
			get
				if _orphanedFilesProject == null
					_orphanedFilesProject := XProject{OrphanedFilesProject{}}
					var projectNode := (OrphanedFilesProject)(_orphanedFilesProject:ProjectNode)
					projectNode:Project := _orphanedFilesProject
					if xProjects:TryAdd("(OrphanedFiles)", _orphanedFilesProject)
						projectNode:Project:AddAssemblyReference(typeof(string):Assembly:Location)
					endif
				endif
				return _orphanedFilesProject
			end get
		end property
		
		
	end class

	CLASS ModelOutputWindow IMPLEMENTS IOutputWindow
		METHOD DisplayOutPutMessage(message AS STRING) AS VOID
			System.Diagnostics.Debug.WriteLine(message)
			RETURN
	END CLASS	

	INTERFACE IOutputWindow
		METHOD DisplayOutPutMessage(message as STRING) AS VOID
	end interface
end namespace 

