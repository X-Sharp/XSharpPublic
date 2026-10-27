//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING LanguageService.CodeAnalysis.XSharp.SyntaxParser
USING LanguageService.SyntaxTree
USING LanguageService.SyntaxTree.Misc
USING LanguageService.SyntaxTree.Tree
USING System.Collections.Concurrent
USING System.Text
USING System.IO
USING System.Threading
USING System.Threading.Tasks
USING XSharp.Parser
USING LanguageService.CodeAnalysis
USING LanguageService.CodeAnalysis.Text
USING XSharp.Settings

NAMESPACE XSharpModel
Internal Enum WalkerthreadState
MEMBER None
MEMBER Running
MEMBER Stopped
END ENUM
STATIC CLASS ModelWalker
// Fields
STATIC PRIVATE _projects       AS ConcurrentQueue<XProject>
STATIC PRIVATE _projectsForTypeResolution AS ConcurrentQueue<XProject>
STATIC PRIVATE _generation AS LONG     // incremented by Stop(); each walker thread walks for one generation
STATIC PRIVATE _WalkerThread AS System.Threading.Thread
STATIC PRIVATE suspendLevel  AS LONG
STATIC PRIVATE _gate as OBJECT
STATIC PRIVATE iProcessed AS LONG
STATIC PRIVATE iTotal     AS LONG
STATIC PRIVATE cProject   AS STRING
STATIC PRIVATE _state     AS WalkerthreadState

// Methods

STATIC CONSTRUCTOR
    suspendLevel := 0
    _gate := Object{}
    _state := WalkerthreadState.None
    Clear()
STATIC METHOD AddProject(xProject AS XProject) AS VOID
    //WriteOutputMessage("-->> AddProject() "+xProject:Name)
    EnqueueProject(xProject)
    IF (! IsRunning .AND. ! XSettings.IsVsBuilding)
        Walk()
ENDIF

PRIVATE STATIC METHOD EnqueueProject(xProject AS XProject) AS VOID
    BEGIN LOCK _gate
        VAR lAdd2Queue := TRUE
        FOREACH project AS XProject IN _projects
            IF (String.Equals(project:Name, xProject:Name, StringComparison.OrdinalIgnoreCase))
                lAdd2Queue := FALSE
                EXIT
            ENDIF
        NEXT
        IF (lAdd2Queue)
            _projects:Enqueue(xProject)
        ENDIF
    END LOCK

INTERNAL STATIC METHOD Clear() AS VOID
    _projects := ConcurrentQueue<XProject>{}
    _projectsForTypeResolution := ConcurrentQueue<XProject>{}
RETURN

STATIC METHOD RemoveProject(xProject AS XProject) AS VOID
    //WriteOutputMessage("-->> RemoveProject() "+xProject:Name)
    BEGIN LOCK _gate
        LOCAL found := FALSE AS LOGIC
        VAR  aList  := List<XProject>{}
        FOREACH project AS XProject IN _projects
            IF (String.Equals(project:Name, xProject:Name, StringComparison.OrdinalIgnoreCase))
                found := TRUE
            ELSE
                aList:Add(project)
            ENDIF
        NEXT
        IF found
            Clear()
            FOREACH VAR project IN aList
                _projects:Enqueue(project)
            NEXT
        ENDIF
    END LOCK
    IF (! IsRunning .AND. ! XSettings.IsVsBuilding)
        Walk()
ENDIF
//WriteOutputMessage("<<-- RemoveProject()")

INTERNAL STATIC METHOD FileWalk(file AS XFile) AS VOID
    IF ! XSolution.IsOpen .or. file == NULL
        RETURN
    ENDIF
    LOCAL walkFile := TRUE AS LOGIC
    IF file:UpdatedOnDisk
        WriteOutputMessage("..."+file:FullPath)
        BEGIN USING VAR walker := SourceWalker{file}
            TRY
                walker:Parse(FALSE)

            CATCH e AS Exception
                XSettings.Exception(e)
                walkFile := FALSE
            FINALLY
                IF walker != NULL
                    walker:Dispose()
                ENDIF
            END TRY
        END USING
    ENDIF
RETURN


STATIC METHOD Start  AS VOID
    IF ModelWalker.IsSuspended
        ModelWalker.Resume()
    ELSE
        ModelWalker.suspendLevel  := 0
        _state := WalkerthreadState.Running
    ENDIF

STATIC METHOD Resume() AS VOID
    ModelWalker.suspendLevel--

STATIC METHOD Stop AS VOID
    BEGIN LOCK _gate
        _state := WalkerthreadState.Stopped
        _generation++
    END LOCK
    ModelWalker.suspendLevel  := 1
    _WalkerThread := NULL
    Clear()

STATIC PROPERTY MustAbort as LOGIC GET _state == WalkerthreadState.Stopped

// A walker thread may still be busy with the files of a project when Stop() is called. When the next
// solution starts a new walker thread, _state is Running again, so the old thread also checks its generation.
PRIVATE STATIC METHOD IsAborted(generation AS LONG) AS LOGIC
    RETURN MustAbort .OR. generation != _generation


STATIC METHOD Suspend() AS VOID
    ModelWalker.suspendLevel++
    IF (_projects:Count > 0)
        IF _projects:TryPeek(OUT VAR _)
            XSolution.SetStatusBarText("")
        ENDIF
    ENDIF

STATIC METHOD Walk() AS VOID
    IF ModelWalker.suspendLevel <= 0 .AND. XSolution.IsOpen
        TRY
            IF _WalkerThread == NULL .or. ! _WalkerThread:IsAlive
                VAR start := System.Threading.ParameterizedThreadStart{ null, @Walker() }
                _state := WalkerthreadState.Running
                _WalkerThread := System.Threading.Thread{start}
                _WalkerThread:IsBackground := TRUE
                _WalkerThread:Name := "X# CodeModel Walker"
                _WalkerThread:Start(_generation)
            ENDIF
        CATCH exception AS System.Exception
            XSettings.Exception(exception)

        END TRY
ENDIF


PRIVATE STATIC METHOD WalkSource(generation AS LONG) AS VOID
    LOCAL project AS XProject
    LOCAL parallelOptions AS System.Threading.Tasks.ParallelOptions
    project := NULL
    IF  ! XSettings.IsVsBuilding
        DO WHILE _projects:Count > 0
            if IsAborted(generation) .or. IsSuspended .or. ! HasWork
                RETURN
            ENDIF
            IF _projects:Count == 0 .OR. ! _projects:TryDequeue( OUT project)
                EXIT
            ENDIF
            IF !project:IsLoaded
                LOOP
            ENDIF

            XSolution.SetStatusBarText(String.Format("Start scanning project {0}", project:Name))
            WriteOutputMessage("-->> Walker("+project.Name+")")
            LOCAL aFiles AS STRING[]
            aFiles := project:SourceFiles:ToArray()
            BEGIN LOCK _gate
                iProcessed := 0
                iTotal     := aFiles:Length
                cProject   := project:Name
            END LOCK
            parallelOptions := ParallelOptions{}
            IF (System.Environment.ProcessorCount > 1)
                parallelOptions:MaxDegreeOfParallelism := (System.Environment.ProcessorCount*3)  /4
            ENDIF
            XSolution.SetStatusBarAnimation(TRUE, 3)
            VAR walk := ProjectWalk{project, generation}
            VAR lFailed := FALSE
            TRY
                Parallel.ForEach(aFiles, parallelOptions, { fileName => walkOneFile(walk, fileName) })
            CATCH e AS Exception
                XSettings.Exception(e)
                lFailed := TRUE     // do not walk it again and again: finish it as before
            END TRY
            IF walk:Walked < aFiles:Length .AND. ! lFailed
                // walkOneFile skipped files: Stop() (the solution closes) or Suspend() (completion, quick info
                // and so on while the walk runs). Do not finish the project as if it was walked completely.
                XSolution.SetStatusBarText("")
                XSolution.SetStatusBarAnimation(FALSE, 3)
                IF ! IsAborted(generation) .AND. project:IsLoaded
                    // Walk it again later. The files that were walked are not parsed again (UpdatedOnDisk).
                    EnqueueProject(project)
                ENDIF
                RETURN
            ENDIF
            BEGIN LOCK _gate
                // Stop() takes the lock too: after this check the project goes into the queue of this walk
                IF IsAborted(generation)
                    RETURN
                ENDIF
                _projectsForTypeResolution:Enqueue(project)
            END LOCK
            XSolution.SetStatusBarText("")
            XSolution.SetStatusBarAnimation(FALSE, 3)
            XDatabase.Read(project)
            WriteOutputMessage("<<-- Walker("+project.Name+")")
            aFiles := project:OtherFiles:ToArray()
            FOREACH VAR file IN aFiles
                VAR oFile := project:FindXFile(file)
                IF oFile:UpdatedOnDisk
                    XDatabase.Update(oFile)
                ENDIF
            NEXT
            // Delete files that do not exist anymore
            var allfiles := XDatabase.GetFileNames(project)
            foreach var fileName in allfiles
                if ! System.IO.File.Exists(fileName)
                    XDatabase.DeleteFile(fileName)
                endif
            next
            // Collect the include file orphans left behind by this walk. This used to run
            // once per file inside the database lock, which is far too often for a full
            // anti join over the whole table.
            XDatabase.DeleteOrphanIncludeFiles()
            project:ProjectWalkComplete?:Invoke(project)
            project:FileWalkCompleted := TRUE
        ENDDO
    ENDIF
    RETURN
PRIVATE STATIC METHOD WalkReferences(generation AS LONG) AS VOID
    IF _projectsForTypeResolution:Count != 0 .AND. ! XSettings.IsVsBuilding
        DO WHILE _projectsForTypeResolution:Count > 0
            if IsAborted(generation)
                RETURN
            ENDIF
            LOCAL project as XProject
            BEGIN LOCK _gate
                IF ! _projectsForTypeResolution:TryDequeue( OUT project)
                    EXIT
                ENDIF
            END LOCK
            WriteOutputMessage("-->> Walker() Resolve types "+project:Name)
            project:ResolveReferences()
            WriteOutputMessage("<<-- Walker() Resolve types "+project:Name)
        ENDDO
    ENDIF
    RETURN
PRIVATE STATIC METHOD Walker(oGeneration AS OBJECT) AS VOID
    VAR generation := (LONG) oGeneration
    DO WHILE TRUE
        if IsAborted(generation) .or. IsSuspended
            RETURN
        ENDIF
        WalkSource(generation)
        if IsAborted(generation) .or. IsSuspended
            RETURN
        ENDIF
        WalkReferences(generation)
        System.Threading.Thread.Sleep(1000)
        IF _projectsForTypeResolution:Count == 0 .and. _projects:Count == 0
            EXIT
        ENDIF
    ENDDO

PRIVATE STATIC METHOD UpdateProgress (fileName as STRING) AS VOID
    BEGIN LOCK _gate
        iProcessed++
        IF iProcessed % 5 == 0 .OR. iProcessed == iTotal
            XSolution.SetStatusBarText(String.Format("Walking {0} : Processing File {1} of {2}", cProject, iProcessed, iTotal))
        ENDIF
    END LOCK
PRIVATE STATIC METHOD walkOneFile(walk AS ProjectWalk, fileName AS STRING) AS VOID
    VAR project := walk:Project
    IF IsAborted(walk:Generation)
        RETURN
    ENDIF
    IF project:IsLoaded .AND. XSolution.IsOpen
        IF ModelWalker.IsSuspended
            RETURN
        ENDIF
        UpdateProgress(fileName)
        VAR file := project:FindXFile(fileName)
        FileWalk(file)
        Interlocked.Increment(REF walk:Walked)
    ENDIF
RETURN

// Properties
STATIC PROPERTY HasWork AS LOGIC
    GET
        BEGIN LOCK _gate
            RETURN _projects:Count > 0
        END LOCK
    END GET
END PROPERTY
STATIC PROPERTY IsSuspended AS LOGIC
    GET
        BEGIN LOCK _gate
            return ModelWalker.suspendLevel > 0
        END LOCK
    END GET
END PROPERTY

STATIC PROPERTY IsRunning AS LOGIC
    GET
        TRY
            IF _WalkerThread != NULL
                RETURN _WalkerThread:IsAlive
            ENDIF
        CATCH exception AS System.Exception
            //
            XSettings.Exception(exception)
        END TRY
        RETURN FALSE
    END GET
END PROPERTY

STATIC METHOD WriteOutputMessage(message AS STRING) AS VOID
    IF XSettings.EnableParseLog
        XSettings.Information("XModel.Walker "+message)
    ENDIF


END CLASS

/// <summary>
/// The walk of the source files of one project. One instance per walk instead of static fields: a walker
/// thread that still finishes its files after Stop() must not change the project or the count of the
/// walker thread of the next solution.
/// </summary>
INTERNAL CLASS ProjectWalk
    INTERNAL INITONLY Project    AS XProject
    INTERNAL INITONLY Generation AS LONG
    INTERNAL Walked              AS LONG    // files that were walked, see ModelWalker.WalkSource

    CONSTRUCTOR(oProject AS XProject, nGeneration AS LONG)
        Project    := oProject
        Generation := nGeneration
        Walked     := 0
END CLASS

