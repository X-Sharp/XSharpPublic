//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Collections.Concurrent
USING System.Collections.Generic
USING System
BEGIN NAMESPACE XSharpModel
	STATIC CLASS XSolution
		// Fields
		STATIC PRIVATE _orphanedFilesProject := NULL AS XProject
		CONST PRIVATE OrphanedFiles := "(OrphanedFiles)" AS STRING
		STATIC INITONLY PRIVATE xProjects := ConcurrentDictionary<STRING, XProject>{StringComparer.OrdinalIgnoreCase} AS ConcurrentDictionary<STRING, XProject>

		PUBLIC STATIC OutputWindow AS IOutPutWindow
		// Methods
		STATIC CONSTRUCTOR
			OutputWindow := ModelOutputWindow{}
            VAR x := XSolution.OrphanedFilesProject
            OutputWindow:DisplayOutPutMessage("XSolution Loaded")


		STATIC METHOD WriteOutputMessage(message AS STRING) AS VOID
			OutputWindow:DisplayOutPutMessage(message)
		STATIC METHOD WriteException(ex AS Exception) AS VOID
			LOCAL space := "" AS STRING
			DO WHILE ex != NULL
				WriteOutputMessage(String.Format("{0}***** exception {1}", space, ex:GetType()))
				WriteOutputMessage(ex:Message)
				WriteOutputMessage(ex:StackTrace)
				space += " "
				ex := ex:InnerException
			ENDDO
			RETURN


		STATIC METHOD Add(project AS XProject) AS LOGIC
			RETURN ADD(project:Name, project)

		STATIC METHOD Add(projectName AS STRING, project AS XProject) AS LOGIC
			WriteOutputMessage("XModel.Solution.Add() "+projectName)
			IF xProjects:ContainsKey(projectName)
				RETURN FALSE
			ENDIF
			RETURN xProjects:TryAdd(projectName, project)

		STATIC METHOD CloseAll() AS VOID
			WriteOutputMessage("XModel.Solution.CloseAll()")
			xProjects:Clear()
			SystemTypeController.Clear()
			IF _orphanedFilesProject != NULL .AND. xProjects:TryAdd("(OrphanedFiles)", _orphanedFilesProject)
				FOREACH VAR info IN _orphanedFilesProject:AssemblyReferences
					SystemTypeController.LoadAssembly(info:FileName)
				NEXT
			ENDIF

		STATIC METHOD FileClose(fileName AS STRING) AS VOID
			IF FindFile(fileName):Project == _orphanedFilesProject
				_orphanedFilesProject:RemoveFile(fileName)
			ENDIF

		STATIC METHOD FindFile(fileName AS STRING) AS XFile
			FOREACH VAR project IN xProjects
				VAR file := project:Value:FindFullPath(fileName)
				IF file != NULL
					RETURN file
				ENDIF
			NEXT
			RETURN NULL

		STATIC METHOD FindFullPath(fullPath AS STRING) AS XFile
			FOREACH VAR project IN xProjects
				VAR file := project:Value:FindFullPath(fullPath)
				IF file != NULL
					RETURN file
				ENDIF
			NEXT
			RETURN NULL

		STATIC METHOD FindProject(projectFile AS STRING) AS XProject
			LOCAL project AS XProject
			projectFile := System.IO.Path.GetFileNameWithoutExtension(projectFile)
			project := NULL
			IF xProjects:TryGetValue(projectFile, OUT project) .AND. project != NULL
				RETURN project
			ENDIF
			RETURN NULL

		STATIC METHOD Remove(projectName AS STRING) AS LOGIC
			WriteOutputMessage("XModel.Solution.Remove() "+projectName)
			IF xProjects:ContainsKey(projectName)
				VAR project := xProjects:Item[projectName]
				project:UnLoad()
				VAR result := xProjects:TryRemove(projectName, OUT project)
				SystemTypeController.UnloadUnusedAssemblies()
				RETURN result
			ENDIF
			RETURN FALSE

		STATIC METHOD Remove(project AS XProject) AS LOGIC
			IF project != NULL
				RETURN REMOVE(project:Name)
			ENDIF
			RETURN FALSE

		STATIC METHOD WalkFile(fileName AS STRING) AS VOID
			VAR file := FindFile(fileName)
			IF file != NULL
				ModelWalker.GetWalker():FileWalk(file)
			ENDIF
			RETURN

		// Properties
		STATIC PROPERTY OrphanedFilesProject AS XProject
			GET
				IF _orphanedFilesProject == NULL
					_orphanedFilesProject := XProject{OrphanedFilesProject{}}
					VAR projectNode := (OrphanedFilesProject)(_orphanedFilesProject:ProjectNode)
					projectNode:Project := _orphanedFilesProject
					IF xProjects:TryAdd("(OrphanedFiles)", _orphanedFilesProject)
						projectNode:Project:AddAssemblyReference(TYPEOF(STRING):Assembly:Location)
					ENDIF
				ENDIF
				RETURN _orphanedFilesProject
			END GET
		END PROPERTY


	END CLASS

	CLASS ModelOutputWindow IMPLEMENTS IOutputWindow
		METHOD DisplayOutPutMessage(message AS STRING) AS VOID
			System.Diagnostics.Debug.WriteLine(message)
			RETURN
	END CLASS

	INTERFACE IOutputWindow
		METHOD DisplayOutPutMessage(message AS STRING) AS VOID
	END INTERFACE
END NAMESPACE

