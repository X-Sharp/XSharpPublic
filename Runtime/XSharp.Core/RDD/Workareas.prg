//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.Collections.Generic
USING System.Diagnostics
USING System.Linq
#undef AUTOCLOSETIMER
#translate IsValidArea (<nArea>) => (<nArea> > 0 .and. <nArea> <= MaxWorkareas)

/// <summary>Class that contains the list of open Workareas. Each thread will have its own list.</summary>
ABSTRACT CLASS XSharp.RDD.Workareas
    // Not static because every thread can have its own Workareas structure
    #region Constants
    [DebuggerBrowsable(DebuggerBrowsableState.Never)];
    PUBLIC CONST MaxWorkareas := 4096 AS DWORD
    #endregion
    // This is a table across threads that has all the RDDs and Workareas
    STATIC PROTECTED _AllRDDs   AS Dictionary<IRdd, Workareas>
#ifdef AUTOCLOSETIMER
    STATIC PRIVATE _oTimer       as System.Threading.Timer

    STATIC METHOD Timer_Tick(state as Object) AS VOID
        LOCAL aToClose := NULL as IList<IRdd>
        BEGIN LOCK _AllRDDs
            FOREACH var oRDD in _AllRDDs:Keys
                IF oRDD IS IClosedRDD var oClosed
                    LOCAL lRemove := oClosed:Closed AS LOGIC
                    IF ! lRemove
                        LOCAL areas := _AllRDDs[oRDD] as Workareas
                        IF ! areas:Thread:IsAlive
                            lRemove := TRUE
                            oRDD:Close()
                        ENDIF
                    ENDIF
                    IF lRemove
                        if aToClose = NULL
                            aToClose := List<IRdd>{}
                        ENDIF
                        aToClose:Add(oRDD)
                    ENDIF
                ENDIF
            NEXT
        END LOCK
        IF aToClose != NULL
            FOREACH VAR oRDD in aToClose
                _CloseArea(oRDD)
            NEXT
        ENDIF
#endif
    STATIC CONSTRUCTOR()
        _AllRDDs  := Dictionary<IRdd, Workareas>{}
#ifdef AUTOCLOSETIMER
    // every 30 seconds we do some cleanup for dangling RDD objects
        _oTimer   := System.Threading.Timer{Timer_Tick,NULL, 0, 30_000}
#endif

    RETURN
    /// <exclude />

    STATIC METHOD _Add(oRDD AS IRdd, oWA AS Workareas) AS VOID
        BEGIN LOCK _AllRDDs
            IF _AllRDDs:ContainsKey(oRDD)
                // this can happen when a RDD was created in a backgroundthread
                // and is moved to the main thread for example
                _AllRDDs[oRDD] := oWA
            ELSE
                _AllRDDs.Add(oRDD, oWA)
            ENDIF
        END LOCK
        RETURN

    STATIC METHOD _Remove(oRDD AS IRdd) AS LOGIC
        BEGIN LOCK _AllRDDs
            IF _AllRDDs:ContainsKey(oRDD)
                _AllRDDs:Remove(oRDD)
                RETURN TRUE
            ENDIF
        END LOCK
        RETURN FALSE

    STATIC METHOD _FindRDD (oRDD AS IRdd, oWa OUT Workareas) AS LOGIC
        oWa  := NULL
        BEGIN LOCK _AllRDDs
            IF _AllRDDs:ContainsKey(oRDD)
                oWa  := _AllRDDs[oRDD]
                RETURN TRUE
            ENDIF
        END LOCK
        RETURN FALSE

    STATIC METHOD _CloseArea(oRDD AS IRdd) AS LOGIC
        LOCAL oWa  AS Workareas
        IF Workareas._FindRDD(oRDD, OUT oWa)
            RETURN oWa:CloseArea(oRDD:Area)
        ENDIF
        RETURN FALSE

        #region Fields
        PRIVATE Aliases            AS Dictionary<STRING, DWORD>	// 1 based area numbers !
        PRIVATE RDDs	           AS Dictionary<DWORD, IRdd>
        [DebuggerBrowsable(DebuggerBrowsableState.Never)];
        PRIVATE iCurrentWorkarea    AS DWORD
        PRIVATE WorkareaStack       AS Stack<DWORD>
#ifdef AUTOCLOSETIMER
        PRIVATE Thread              AS System.Threading.Thread
#endif
        // xbase++ has a cargo slot per Workarea
        [DebuggerBrowsable(DebuggerBrowsableState.Never)];
        PRIVATE cargo    := Dictionary<DWORD, OBJECT>{} AS Dictionary<DWORD, OBJECT>      // 1 based area number and value

        INTERNAL METHOD PushCurrentWorkarea(dwNew AS DWORD) AS VOID
            WorkareaStack:Push(iCurrentWorkarea)
            iCurrentWorkarea := dwNew

        INTERNAL METHOD PopCurrentWorkarea() AS DWORD
            IF WorkareaStack:Count > 0
                iCurrentWorkarea := WorkareaStack:Pop()
                RETURN iCurrentWorkarea
            ENDIF
            RETURN 0

    #endregion
    /// <exclude />
    CONSTRUCTOR()
        Aliases 			:= Dictionary<STRING, DWORD>{ (INT) MaxWorkareas, StringComparer.OrdinalIgnoreCase}
        RDDs				:= Dictionary<DWORD, IRdd>{}
        iCurrentWorkarea	:= 1
        WorkareaStack       := Stack<DWORD>{}
#ifdef AUTOCLOSETIMER
        Thread              := System.Threading.Thread.CurrentThread
#endif

    ///<summary>Close All RDDs referenced by this Workarea list</summary>
    /// <returns>TRUE when all areas were closed succesfully. When one or more areas failed to close then FALSE is returned.</returns>
    PUBLIC METHOD CloseAll() AS LOGIC
        LOCAL lResult := TRUE AS LOGIC
        // Please note that this returns TRUE when no areas are open, unlike DbCloseArea()
        // which returns FALSE when the area involved is not open
        BEGIN LOCK RDDs
            RuntimeState.LastRddError := NULL
            FOREACH VAR element IN Aliases
                VAR nArea := element:Value
                IF RDDs:ContainsKey(nArea) .AND. RDDs[nArea] != NULL
                    VAR oRdd := RDDs[nArea]
                    TRY
                        IF ! oRdd:Close()
                            lResult := FALSE
                        ENDIF
                    CATCH e AS Exception
                        lResult := FALSE
                        RuntimeState.LastRddError  := e
                    END TRY
                ENDIF
            NEXT
            RDDs:Clear()
            Aliases:Clear()
            cargo:Clear()
            iCurrentWorkarea := 1
        END LOCK
        RETURN lResult

    ///<summary>Commit changes in all workares in this Workarea list</summary>
    /// <returns>TRUE when all areas were committed succesfully. When one or more areas failed to commit then FALSE is returned.</returns>
    PUBLIC METHOD CommitAll() AS LOGIC
        LOCAL lResult := TRUE AS LOGIC
        BEGIN LOCK RDDs
            RuntimeState.LastRddError  := NULL
            FOREACH VAR pair IN RDDs
                IF pair:Value != NULL
                    VAR oRdd := pair:Value
                    TRY
                        lResult := lResult .AND. oRdd:Flush()
                    CATCH e AS Exception
                        lResult := FALSE
                        RuntimeState.LastRddError  := e
                    END TRY
                ENDIF
            NEXT
        END LOCK
        RETURN lResult

    ///<summary>Close area with 1 based Workarea number</summary>
    /// <param name="nArea">1 based Area number for which to find the alias</param>
    /// <returns>TRUE when the area was closed succesfully.</returns>
    PUBLIC METHOD CloseArea( nArea AS DWORD) AS LOGIC
        LOCAL lResult := FALSE AS LOGIC
        IF IsValidArea(nArea)
            BEGIN LOCK RDDs
                //RuntimeState.LastRddError  := NULL
                IF RDDs:ContainsKey(nArea)
                    VAR oRdd := RDDs[ nArea]
                    IF oRdd != NULL_OBJECT
                        TRY
                            lResult := oRdd:Close()
                            Aliases:Remove(oRdd:Alias)
                            // Remove from Global Thread-Wide list
                            Workareas._Remove(oRdd)
                        CATCH e AS Exception
                            lResult			:= FALSE
                            RuntimeState.LastRddError 	:= e
                        END TRY
                    ENDIF
                    RDDs:Remove(nArea)
                ELSE
                    lResult        := FALSE
                ENDIF

                SELF:SetCargo(nArea, NULL)
            END LOCK
        ENDIF
        RETURN lResult

    /// <summary>Close the area where this RDD object is used.</summary>
    /// <param name="oRDD">Object of te RDD that needs to be closed.</param>
    /// <returns>TRUE when the area was closed.</returns>
    /// <remarks>This will close the area, even when the method is called from another thread.</remarks>
    PUBLIC METHOD CloseArea(oRDD AS IRdd) AS LOGIC
        // This can be called from any thread and will close the RDD
        // in the right Workarea even if that area is from another thread
        RETURN Workareas._CloseArea(oRDD)

    /// <summary> Return 1 based Workarea Number for Alias or 0 when no found</summary>
    /// <param name="sAlias">Alias of area to look for. Case INsensitive </param>
    /// <returns>Area number of an the table with the given alias.</returns>
    PUBLIC METHOD FindAlias(sAlias AS STRING) AS DWORD
        BEGIN LOCK RDDs
            IF !String.IsNullOrEmpty(sAlias)
                IF Aliases != NULL .AND. Aliases:ContainsKey(sAlias)
                    RETURN Aliases[sAlias]
                ENDIF
            ENDIF
        END LOCK
        RETURN 0

    ///<summary> Return 1 based empty Workarea</summary>
    /// <param name="fromStart">Do we start to search from the start (TRUE) or the end (FALSE)</param>
    /// <returns>Area number of an area where no table is open.</returns>
    PUBLIC METHOD FindEmptyArea(fromStart AS LOGIC) AS DWORD
        LOCAL i AS DWORD
        BEGIN LOCK RDDs
            IF fromStart
                FOR i := 1 TO MaxWorkareas
                    IF ! RDDs:ContainsKey(i)
                        RETURN i
                    ENDIF
                NEXT
            ELSE
                FOR i := MaxWorkareas DOWNTO 1
                    IF ! RDDs:ContainsKey(i)
                        RETURN i
                    ENDIF
                NEXT
            ENDIF
        END LOCK
        RETURN 0

    /// <summary>Get Alias for 1 based Workarea Number</summary>
    /// <param name="nArea">1 based Area number for which to find the alias</param>
    /// <returns>Alias or an empty string when there is no table open in the area</returns>
    PUBLIC METHOD GetAlias( nArea AS DWORD) AS STRING
        IF ISVALIDAREA(nArea)
            BEGIN LOCK RDDs
                IF RDDs:ContainsKey(nArea) .AND. RDDs[nArea] != NULL
                    RETURN RDDs[nArea]:Alias
                ENDIF
            END LOCK
        ENDIF
        RETURN String.Empty

    ///<summary>Get RDD object for 1 based Workarea Number</summary>
    PUBLIC METHOD GetRDD( nArea AS DWORD) AS IRdd
        IF IsValidArea(nArea)
            BEGIN LOCK RDDs
                IF RDDs:ContainsKey(nArea)
                    RETURN RDDs[ nArea]
                ENDIF
            END LOCK
        ENDIF
        RETURN NULL

    ///<summary>Set RDD object and ALias for 1 based Workarea Number</summary>
    PUBLIC METHOD SetArea( nArea AS DWORD, oRDD AS IRdd) AS LOGIC
        // sAlias and oRdd may be empty (when clearing the RDD)
        IF IsValidArea(nArea)
            oRDD:Area := nArea
            VAR sAlias := oRDD:Alias
            IF ! String.IsNullOrEmpty(sAlias)
                sAlias := sAlias:ToUpperInvariant()
            ENDIF
            IF Aliases:ContainsKey(sAlias) .AND. Aliases[sAlias] != nArea
                RETURN FALSE
            ENDIF
            BEGIN LOCK RDDs
                IF RDDs:ContainsKey(nArea) .AND. RDDs [nArea] != NULL
                    Workareas._Remove(RDDs [nArea])
                ENDIF
                RDDs[ nArea] 	:= oRDD
                IF ! Aliases:ContainsKey(sAlias)
                    Aliases:Add(sAlias, nArea)
                ENDIF
                SELF:SetCargo(nArea, NULL)
                Workareas._Add(oRDD, SELF)
            END LOCK
            RETURN TRUE
        ENDIF
        RETURN FALSE

    ///<summary>Unlock All RDDs referenced by this Workarea list</summary>
    PUBLIC METHOD UnLockAll() AS LOGIC
        LOCAL lResult := TRUE AS LOGIC
        BEGIN LOCK RDDs
            RuntimeState.LastRddError := NULL
            FOREACH VAR pair IN RDDs
                IF pair:Value  != NULL
                    TRY
                            VAR oRdd := pair:Value
                        lResult := lResult .AND. oRdd:UnLock(0)
                    CATCH e AS Exception
                        RuntimeState.LastRddError := e
                    END TRY
                ENDIF
            NEXT
        END LOCK
        RETURN lResult

    ///<summary>Get 1 based Current Workarea Number</summary>
    PUBLIC PROPERTY CurrentWorkareaNO AS DWORD GET iCurrentWorkarea SET iCurrentWorkarea := VALUE

    ///<summary>Get Current Workarea Object</summary>
    PUBLIC PROPERTY CurrentWorkarea AS IRdd
        GET
            LOCAL  nArea AS DWORD
            nArea := iCurrentWorkarea
            IF IsValidArea(nArea)
                BEGIN LOCK RDDs
                    IF RDDs:ContainsKey(nArea)
                        RETURN RDDs[ nArea]
                    ENDIF
                END LOCK
            ENDIF
            RETURN NULL
        END GET
    END PROPERTY


    PUBLIC METHOD GetCargo(nArea AS DWORD) AS OBJECT
        IF IsValidArea(nArea)
            IF SELF:cargo:ContainsKey(nArea)
                RETURN SELF:cargo[nArea]
            ENDIF
        ENDIF
        RETURN NULL

    PUBLIC METHOD SetCargo(nArea AS DWORD, newCargo AS OBJECT) AS VOID
        IF IsValidArea(nArea)
            IF newCargo == NULL
                    IF SELF:cargo:ContainsKey(nArea)
                        SELF:cargo:Remove(nArea)
                ENDIF
            ELSE
                SELF:cargo[nArea] := newCargo
            ENDIF
        ENDIF
        RETURN

END CLASS
