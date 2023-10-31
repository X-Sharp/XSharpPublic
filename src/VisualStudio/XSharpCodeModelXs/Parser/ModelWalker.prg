//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING LanguageService.CodeAnalysis.XSharp.SyntaxParser
USING LanguageService.SyntaxTree
USING LanguageService.SyntaxTree.Misc
USING LanguageService.SyntaxTree.Tree
USING System
USING System.Collections.Generic
USING System.Diagnostics
USING System.Linq
USING System.Text
USING System.IO
USING System.Threading
USING System.Threading.Tasks
USING System.Collections.Concurrent
USING XSharp.Parser
USING LanguageService.CodeAnalysis
USING LanguageService.CodeAnalysis.Text
USING XSharp.Settings

BEGIN NAMESPACE XSharpModel
    Internal Enum WalkerthreadState
        MEMBER None
        MEMBER Running
        MEMBER Stopped
    END ENUM
    STATIC CLASS ModelWalker
        // Fields
        STATIC PRIVATE _projects       AS ConcurrentQueue<XProject>
        STATIC PRIVATE _projectsForTypeResolution AS ConcurrentQueue<XProject>
        STATIC PRIVATE _currentProject AS XProject
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
            IF (! IsRunning .AND. ! XSettings.IsVsBuilding)
                Walk()
        ENDIF

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
                        XSettings.Exception(e, __FUNCTION__)
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
            END LOCK
            ModelWalker.suspendLevel  := 1
            _WalkerThread := NULL
            Clear()

 STATIC PROPERTY MustAbort as LOGIC GET _state == WalkerthreadState.Stopped


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
                        VAR start := System.Threading.ThreadStart{ null, @Walker() }
                        _state := WalkerthreadState.Running
                        _WalkerThread := System.Threading.Thread{start}
                        _WalkerThread:IsBackground := TRUE
                        _WalkerThread:Name := "X# CodeModel Walker"
                        _WalkerThread:Start()
                    ENDIF
                CATCH exception AS System.Exception
                    XSettings.Exception(exception,"Cannot start Background walker Thread")

                END TRY
        ENDIF


        PRIVATE STATIC METHOD WalkSource() AS VOID
            LOCAL project AS XProject
            LOCAL parallelOptions AS System.Threading.Tasks.ParallelOptions
            project := NULL
            IF  ! XSettings.IsVsBuilding
                DO WHILE _projects:Count > 0
                    if MustAbort .or. IsSuspended .or. ! HasWork
                        RETURN
                    ENDIF
                    IF _projects:Count == 0 .OR. ! _projects:TryDequeue( OUT project)
                        EXIT
                    ENDIF
                    IF !project:Loaded
                        LOOP
                    ENDIF

                    XSolution.SetStatusBarText(String.Format("Start scanning project {0}", project:Name))
                    _currentProject := project
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
                    TRY
                        Parallel.ForEach(aFiles, parallelOptions, walkOneFile)
                    CATCH e AS Exception
                        XSettings.Exception(e, __FUNCTION__)
                    END TRY
                    BEGIN LOCK _gate
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
                    if _currentProject != null
                        _currentProject:ProjectWalkComplete?:Invoke(_currentProject)
                    endif
                    _currentProject := NULL
                    project:FileWalkCompleted := TRUE
                ENDDO
            ENDIF
            RETURN
        PRIVATE STATIC METHOD WalkReferences() AS VOID
            IF _projectsForTypeResolution:Count != 0 .AND. ! XSettings.IsVsBuilding
                DO WHILE _projectsForTypeResolution:Count > 0
                    if MustAbort
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
        PRIVATE STATIC METHOD Walker() AS VOID
            DO WHILE TRUE
                if MustAbort .or. IsSuspended
                    RETURN
                ENDIF
                WalkSource()
                if MustAbort .or. IsSuspended
                    RETURN
                ENDIF
                WalkReferences()
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
        PRIVATE STATIC METHOD walkOneFile(fileName AS STRING) AS VOID
            VAR project := _currentProject
            IF MustAbort .or. project == NULL
                RETURN
            ENDIF
            IF project:Loaded .AND. XSolution.IsOpen
                IF ModelWalker.IsSuspended
                    RETURN
                ENDIF
                UpdateProgress(fileName)
                VAR file := project:FindXFile(fileName)
                FileWalk(file)
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
                    XSettings.Exception(exception,"Cannot check Background walker Thread")
                END TRY
                RETURN FALSE
            END GET
        END PROPERTY

        STATIC METHOD WriteOutputMessage(message AS STRING) AS VOID
            IF XSettings.EnableParseLog
                XSettings.Information("XModel.Walker "+message)
            ENDIF


    END CLASS

END NAMESPACE

