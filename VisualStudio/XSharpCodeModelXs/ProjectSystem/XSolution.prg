//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Collections.Concurrent
USING System.Collections.Generic
USING System.IO
USING System.Linq
USING System
BEGIN NAMESPACE XSharpModel
   STATIC CLASS XSolution
      // Fields
      STATIC PRIVATE _orphanedFilesProject := NULL AS XProject
      STATIC PROPERTY IsOpen as LOGIC GET !String.IsNullOrEmpty(_fileName)
      STATIC PRIVATE _projects AS ConcurrentDictionary<STRING, XProject>
      STATIC PRIVATE _fileName   AS STRING
      STATIC PRIVATE _sqldb      AS STRING
      STATIC PRIVATE _commentTokens AS List<XCommentToken>
      STATIC PROPERTY IsClosing  AS LOGIC AUTO
      
      STATIC PROPERTY FileName AS STRING GET _fileName
      STATIC PROPERTY CommentTokens AS IList<XCommentToken> GET _commentTokens
      // Methods
      STATIC CONSTRUCTOR
         _projects := ConcurrentDictionary<STRING, XProject>{StringComparer.OrdinalIgnoreCase}
         CreateOrphanedFilesProject()
         IsClosing   := FALSE
         _commentTokens := List < XCommentToken >{}

      STATIC METHOD SetCommentTokens( aTokens AS IList<XCommentToken>) AS VOID
         _commentTokens:Clear()
         _commentTokens:AddRange(aTokens)

      STATIC METHOD WriteOutputMessage(message AS STRING) AS VOID
         IF XSettings.EnableLogging
            XSettings.DisplayOutputMessage(message)
         ENDIF
      STATIC METHOD WriteException(ex AS Exception) AS VOID
         XSettings.DisplayException(ex)
         RETURN

      STATIC METHOD Open(cFile as STRING) AS VOID
         _fileName := cFile
         var folder := Path.GetDirectoryName(_fileName)
         folder     := Path.Combine(folder, ".vs")
         IF ! Directory.Exists(folder)
            Directory.CreateDirectory(folder)
         ENDIF
         folder    := Path.Combine(folder, Path.GetFileNameWithoutExtension(_fileName))
         IF ! Directory.Exists(folder)
            Directory.CreateDirectory(folder)
         ENDIF
         _sqldb    := Path.Combine(folder, "X#Model.xsdb")
         XDatabase.CreateOrOpenDatabase(_sqldb)
         VAR dbprojectList := XDatabase.GetProjectFileNames()
         FOREACH var project in _projects:Values
            XDatabase.Read(project)
            FOREACH VAR dbproject  in dbprojectList
               if String.Compare(dbproject, project:FileName, StringComparison.OrdinalIgnoreCase) == 0
                  dbprojectList:Remove(dbproject)
                  EXIT
               ENDIF
            NEXT
         NEXT
         if dbprojectList:Count > 0
            FOREACH var dbproject in dbprojectList
               XDatabase.DeleteProject(dbproject)
            NEXT
         endif
         ModelWalker.Start()
         
      STATIC METHOD AddOrphan(fileName as STRING) AS XFile
            OrphanedFilesProject:AddFile(fileName)
            return OrphanedFilesProject:FindXFile(fileName)
        

      INTERNAL STATIC METHOD Add(project AS XProject) AS LOGIC
         RETURN @@Add(project:Name, project)

      INTERNAL STATIC METHOD Add(projectName AS STRING, project AS XProject) AS LOGIC
         WriteOutputMessage("XModel.Solution.Add() "+projectName)
         IF _projects:ContainsKey(projectName)
            RETURN FALSE
         ENDIF
         VAR lOk := _projects:TryAdd(projectName, project)
         IF lOk .and. IsOpen
            XDatabase.Read(project)
         ENDIF
         RETURN lOk
         

   STATIC METHOD Close() AS VOID
      IF IsOpen
         WriteOutputMessage("XModel.Solution.CloseSolution()")
         ModelWalker.Suspend()
         ModelWalker.GetWalker():StopThread()
         XDatabase.CloseDatabase(_sqldb)
         
         FOREACH VAR pair IN _projects:ToArray()
               var project := (XProject) pair:Value
               project:UnLoad()
               project:Close()
         NEXT
         _projects:Clear()
         SystemTypeController.Clear()
         _orphanedFilesProject := NULL
         _fileName  := NULL
      ENDIF
      
      STATIC METHOD FileClose(fileName AS STRING) AS VOID
         IF FindFile(fileName):Project == _orphanedFilesProject
            _orphanedFilesProject:RemoveFile(fileName)
         ENDIF

      STATIC METHOD FindFile(fileName AS STRING) AS XFile
         FOREACH VAR project IN _projects
            VAR file := project:Value:FindXFile(fileName)
            IF file != NULL
               RETURN file
            ENDIF
         NEXT
         RETURN NULL

      STATIC METHOD FindFullPath(fullPath AS STRING) AS XFile
         FOREACH VAR project IN _projects
            VAR file := project:Value:FindXFile(fullPath)
            IF file != NULL
               RETURN file
            ENDIF
         NEXT
         RETURN NULL

      STATIC METHOD FindProject(projectFile AS STRING) AS XProject
         LOCAL project AS XProject
         projectFile := System.IO.Path.GetFileNameWithoutExtension(projectFile)
         project := NULL
         IF _projects:TryGetValue(projectFile, OUT project) .AND. project != NULL
            RETURN project
         ENDIF
         RETURN NULL

      INTERNAL STATIC METHOD Remove(projectName AS STRING) AS LOGIC
         WriteOutputMessage("XModel.Solution.Remove() "+projectName)
         IF _projects:ContainsKey(projectName)
            VAR result := _projects:TryRemove(projectName, OUT VAR p)
            IF (p != NULL)
               FOREACH otherProject AS XProject IN _projects:Values
                  otherProject:RemoveProjectReference(p:FileName)
               NEXT
            ENDIF
            RETURN result
         ENDIF
         RETURN FALSE

      INTERNAL STATIC METHOD RenameProject(oldName AS STRING, newName AS STRING) AS VOID
         IF _projects:ContainsKey(oldName)
            _projects:TryRemove(oldName, OUT VAR project)    
            IF project != NULL
               _projects:TryAdd(newName, project)
            ENDIF
         ENDIF

      INTERNAL STATIC METHOD Remove(project AS XProject) AS LOGIC
         IF project != NULL .AND. project:ProjectNode != NULL  .AND. _projects:Count > 0
            RETURN XSolution.Remove(project:Name)
         ENDIF
         RETURN FALSE

      STATIC METHOD WalkFile(fileName AS STRING) AS VOID
         VAR file := FindFile(fileName)
         IF file != NULL
            ModelWalker.GetWalker():FileWalk(file)
         ENDIF
         RETURN

      STATIC METHOD CreateOrphanedFilesProject() AS VOID
         var prj := OrphanedFilesProject{}
         _orphanedFilesProject := XProject{prj}
         VAR projectNode := (OrphanedFilesProject)(_orphanedFilesProject:ProjectNode)
         projectNode:Project := _orphanedFilesProject
         IF _projects:TryAdd(prj:Name, _orphanedFilesProject)
            projectNode:Project:AddAssemblyReference(TYPEOF(STRING):Assembly:Location)
         ENDIF

      STATIC METHOD SetStatusBarText(cText AS STRING) AS VOID
         IF _projects:Count > 0
            VAR project := _projects:First():Value
            project:ProjectNode:SetStatusBarText(cText)
         ENDIF

      STATIC METHOD SetStatusBarAnimation(onOff AS LOGIC, id AS SHORT) AS VOID
         IF _projects:Count > 0
            VAR project := _projects:First():Value
            project:ProjectNode:SetStatusBarAnimation(onOff, id)
         ENDIF
         
         

      // Properties
      STATIC PROPERTY OrphanedFilesProject AS XProject
         GET
            IF _orphanedFilesProject == NULL
               CreateOrphanedFilesProject()
            ENDIF
            RETURN _orphanedFilesProject
         END GET
      END PROPERTY
      
         

   END CLASS


END NAMESPACE

