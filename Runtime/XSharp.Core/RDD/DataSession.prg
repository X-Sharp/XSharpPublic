//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.Collections.Generic
USING System.Threading
USING SD := System.Diagnostics

/// <summary>The DataSession class contains a list of workareas/cursors</summary>
/// <remarks>This class also manages the life time of all opened datasessions <br/>
/// When it detects that a datasession for a thread is no longer needed (because the thread has stopped)
/// then it will close that datasession and all tables that are opened by it. <br/>
/// At application shutdown it will also close all opened datasessions including private
/// datasessions that could be opened by forms or reports.</remarks>
[SD.DebuggerDisplay("{DebuggerDisplay(),nq}")];
CLASS XSharp.RDD.DataSession INHERIT Workareas
    #region Static Fields
    [SD.DebuggerBrowsable(SD.DebuggerBrowsableState.Collapsed)] ;
    PRIVATE STATIC gnSessionId AS LONG
    [SD.DebuggerBrowsable(SD.DebuggerBrowsableState.Collapsed)] ;
    PRIVATE STATIC sessions    AS List<DataSession>
    [SD.DebuggerBrowsable(SD.DebuggerBrowsableState.Collapsed)] ;
    PROTECTED STATIC timer       AS System.Timers.Timer
    /// <summary>List of all open DataSessions</summary>
    PUBLIC STATIC PROPERTY Sessions   AS DataSession[] GET sessions:ToArray()
    #endregion
    #region Static methods
    STATIC CONSTRUCTOR
        LOCAL domain := AppDomain.CurrentDomain AS AppDomain
        domain:ProcessExit += _CloseSessionsAtProcessExit
        sessions    := List<DataSession>{}
        gnSessionId := 0
        timer       := System.Timers.Timer{5000}
        timer:Elapsed += OnTimer
        timer:AutoReset := TRUE

    /// <summary>Add a DataSession to the list of open datasessions</summary>
    /// <param name="session">The DataSession to add </param>
    STATIC METHOD Add(session as DataSession) AS VOID
        BEGIN LOCK sessions
            sessions:Add(session)
            if sessions:Count > 1
                timer:Enabled   := TRUE
            endif
        END LOCK

    /// <summary>Remove a DataSession from the list of open datasessions</summary>
    /// <param name="session">The DataSession to remove </param>
     STATIC METHOD Close(session as DataSession) AS VOID
         var old := RuntimeState.SetDataSession(session)
         TRY
             session:CloseAll()
         CATCH ex as Exception
             System.Diagnostics.Debug.WriteLine("Error in CloseOneSession: "+ex:ToString())
         FINALLY
            RuntimeState.SetDataSession(old)
         END TRY
         RETURN

    PROTECTED STATIC METHOD OnTimer(source as OBJECT, e as System.Timers.ElapsedEventArgs) AS VOID
        LOCAL deleted := NULL as List<DataSession>
        //
        // please note that the OnTimer runs on its own thread.
        // when we close a session here then we set the Workareas in the runtimestate
        // for the timer thread !
        //
        BEGIN LOCK sessions
            FOREACH var session in sessions:ToArray()
                SWITCH session:Thread:ThreadState
                CASE ThreadState.Aborted
                CASE ThreadState.Stopped
                    // Close session
                    Close(session)
                    if deleted == null
                        deleted := List<DataSession>{}
                    endif
                    deleted:Add(session)
                CASE ThreadState.AbortRequested
                CASE ThreadState.Background
                CASE ThreadState.Running
                CASE ThreadState.StopRequested
                CASE ThreadState.Suspended
                CASE ThreadState.SuspendRequested
                OTHERWISE
                    NOP
                END SWITCH
            NEXT
        END LOCK
        if deleted != null
            BEGIN LOCK sessions
                foreach var session in deleted
                    sessions:Remove(session)
                next
                timer:Enabled := sessions:Count > 1
            END LOCK
        endif

    PRIVATE STATIC METHOD _CloseSessionsAtProcessExit(sender AS OBJECT, e AS EventArgs)  AS VOID
        // This runs at process exit in the GC Thread
        IF sessions != NULL
            BEGIN LOCK sessions
                FOREACH VAR session IN sessions:ToArray()

                    Close(session)
                NEXT
                RuntimeState.SetDataSession(NULL)
                sessions:Clear()
            END LOCK
        ENDIF
        RETURN

    #endregion
    #region Properties
    PRIVATE PROPERTY Thread as Thread AUTO
    /// <summary>The unique id for the datasession</summary>
    PUBLIC PROPERTY Id   AS INT AUTO GET PRIVATE SET
    /// <summary>The name for the datasession.</summary>
    PUBLIC PROPERTY Name as STRING AUTO
    #endregion
    /// <summary>Construct a new datasession</summary>
    /// <param name="cName">The name for this datasession.</param>
    PUBLIC CONSTRUCTOR(cName as STRING)
        SUPER()
        SELF:Name   := cName
        SELF:Id     := ++gnSessionId
        SELF:Thread := Thread.CurrentThread
        DataSession.Add(SELF)

    /// <summary>Construct a new datasession</summary>
    /// <param name="cName">The name for this datasession.</param>
    /// <param name="nId">The ID for the datasession.</param>
    PUBLIC CONSTRUCTOR(nId AS LONG, cName as STRING)
        SUPER()
        SELF:Name   := cName
        SELF:Id     := nId
        IF nId > gnSessionId
            gnSessionId := nId
        ENDIF
        SELF:Thread := Thread.CurrentThread
        DataSession.Add(SELF)
    INTERNAL METHOD DebuggerDisplay() AS STRING
        IF Name:IndexOf("DataSession", StringComparison.OrdinalIgnoreCase) >= 0
            return i"{Name} Id: {Id}"
        else
            return i"DataSession {Name} Id: {Id}"
        endif

END CLASS
