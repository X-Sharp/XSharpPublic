//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING XSharp
USING XSharp.RDD
USING XSharp.RDD.Enums
USING XSharp.RDD.Support
USING System.Collections.Generic
USING System.IO
USING System.Reflection
USING System.Linq
USING System.Text
USING System.Diagnostics

// Some UDCs to make the code easier to read
#define EVENTS
#ifdef EVENTS
#command RAISE      <kNot> <oData>  => IF HasEvents;  RaiseEvent(oRdd , DbNotificationType.<kNot>, <oData>) ;  ENDIF
#command BEFOREBULK                 => RAISE BeforeBulkOperation    __FUNCTION__
#command AFTERBULK                  => RAISE AfterBulkOperation     __FUNCTION__
#command BEFOREMOVE                 => RAISE BeforeMove             __FUNCTION__
#command AFTERMOVE                  => RAISE AfterMove              __FUNCTION__
#translate SaveBagName( <cBagName>, <oOrder>)    => <cBagName> + "-"+iif(<oOrder> == NULL, "", <oOrder>:ToString())
#else
#command RAISE      <kNot> <oData>  =>
#command BEFOREBULK                 =>
#command AFTERBULK                  =>
#command BEFOREMOVE                 =>
#command AFTERMOVE                  =>
#endif

/// <summary>
/// The CoreDb class contains the methods to manipulate Workareas. <br/>
/// The class is stateless. The Workarea state is stored in the RuntimeState.
/// </summary>
CLASS XSharp.CoreDb
    /// <exclude />
    STATIC METHOD CWA(cFunction AS STRING, lThrow := TRUE AS LOGIC) AS IRdd
        LOCAL oResult AS IRdd
        RuntimeState.LastRddError := NULL
        oResult := RuntimeState.DataSession:CurrentWorkarea
        IF oResult != NULL_OBJECT
            RETURN oResult
        ENDIF
        IF lThrow
            RddError.PostNoTableError(cFunction)
        ENDIF
        RETURN NULL

        /// <exclude />
    STATIC METHOD CWANum(cFunction AS STRING)  AS DWORD
        VAR oWA := RuntimeState.DataSession:CurrentWorkarea
        IF oWA != NULL
            RETURN oWA:Area
        ENDIF
        RddError.PostNoTableError(cFunction)
        RETURN 0

        /// <exclude />
    INTERNAL STATIC METHOD AdjustPath(cPath as STRING) AS STRING
        IF String.IsNullOrEmpty(cPath)
            cPath := GetDefault()
        ENDIF
        IF String.IsNullOrEmpty(cPath)
            cPath := System.Environment.CurrentDirectory
        ENDIF
        RETURN cPath

    INTERNAL STATIC METHOD  RddNameToType( cRDDName AS STRING ) AS Type
        LOCAL ret := NULL AS  Type
        LOCAL oRdd        AS RegisteredRDD
        oRdd := RegisteredRDD.Find(cRDDName)
        IF oRdd != NULL
            oRdd:Load()
            ret := oRdd:RddType
        ENDIF
        IF ret == NULL
            LOCAL loadedAssemblies := AppDomain.CurrentDomain:GetAssemblies() AS Assembly[]
            FOREACH VAR asm IN loadedAssemblies
                ret := asm:GetType( cRDDName, FALSE, TRUE )
                IF ret != NULL
                    EXIT
                ENDIF
            NEXT
        ENDIF
        RETURN ret

        /// <exclude />
    INTERNAL STATIC METHOD BuildTrans( dbti AS DbTransInfo, lpfn AS _FieldNames, src AS IRdd, dst AS IRdd ) AS LOGIC
        LOCAL oTmp          AS OBJECT
        LOCAL srcFieldCount AS DWORD
        LOCAL dstFieldCount AS DWORD
        LOCAL fMatch        AS LOGIC
        LOCAL i             AS LONG
        LOCAL j             AS LONG
        LOCAL uiSrc         AS LONG
        LOCAL uiDst         AS LONG

        oTmp := src:Info( DbInfo.DBI_FCOUNT, NULL)
        srcFieldCount := Convert.ToUInt32( oTmp )
        oTmp := dst:Info( DbInfo.DBI_FCOUNT, NULL )
        dstFieldCount := Convert.ToUInt32( oTmp )
        i := (INT) src:Info(DbInfo.DBI_GETRECSIZE, NULL)
        j := (INT) dst:Info(DbInfo.DBI_GETRECSIZE, NULL)
        fMatch := (DWORD) dbti:ItemCount == srcFieldCount .AND. srcFieldCount == dstFieldCount .AND. i == j
        j := 0
        FOR i := 0 UPTO lpfn:FieldCount -1

            uiSrc := src:FieldIndex( lpfn:Fields[i] )       // returns a 1 based index
            uiDst := dst:FieldIndex( lpfn:Fields[i]  )

            IF uiSrc == 0 || uiDst == 0
                fMatch := FALSE
            ELSE

                dbti:Items[j]:Source       := uiSrc
                dbti:Items[j]:Destination  := uiDst
                j++

                IF fMatch
                    LOCAL srcFld AS RddFieldInfo
                    LOCAL dstFld AS RddFieldInfo
                    srcFld := src:GetField(uiSrc)
                    dstFld := dst:GetField(uiDst)
                    if srcFld:IsMemo .or. dstFld:IsMemo
                        fMatch := FALSE
                    else
                        fMatch := uiSrc == uiDst && srcFld:SameType( dstFld )
                    endif
                ENDIF
            ENDIF
        NEXT

        dbti:ItemCount := j

        RETURN fMatch
        /// <exclude />
    INTERNAL STATIC METHOD TransSetInfo(oRdd AS IRdd, info AS DbTransInfo, cFunc AS STRING,nDest AS DWORD, fldNames AS _FieldNames,;
                                        uCobFor AS ICodeblock ,uCobWhile AS ICodeblock ,;
                                        nNext AS OBJECT,nRecno AS OBJECT,lRest AS LOGIC) AS VOID
        LOCAL oDest := RuntimeState.DataSession.GetRDD(nDest) AS IRdd
        IF oDest == NULL
            RddError.PostNoTableError(cFunc)
        ENDIF
        info:Destination := oDest
        IF CoreDb.BuildTrans( info, fldNames, oRdd, oDest )
            info:Flags |= DbTransInfoFlags.SameStructure
            LOCAL oCanPutRecSource AS OBJECT
            LOCAL oCanPutRecTarget AS OBJECT
            oCanPutRecSource := oRdd:Info(DbInfo.DBI_CANPUTREC, NULL)
            oCanPutRecTarget := oDest:Info(DbInfo.DBI_CANPUTREC, NULL)
            IF oCanPutRecSource IS LOGIC VAR lPutSource .AND. oCanPutRecTarget IS LOGIC var lPutTarget
                IF lPutSource .and. lPutTarget
                    info:Flags |= DbTransInfoFlags.CanPutRec
                ENDIF
            ENDIF
        ENDIF
        info:Scope:ForBlock     := uCobFor
        info:Scope:WhileBlock   := uCobWhile
        IF nNext IS LONG
            TRY
                info:Scope:NextCount := Convert.ToInt32(nNext)
            CATCH e AS Exception
                Fail(e)
                info:Scope:NextCount := 0
            END TRY
        ELSE
            info:Scope:NextCount := 0
        ENDIF
        IF nRecno IS LONG
            info:Scope:RecId := nRecno
        ENDIF
        info:Scope:Rest  := lRest
        RETURN
        /// <exclude />
    INTERNAL STATIC METHOD AliasFromFilename( cFilename AS STRING, cAlias REF STRING , lMakeUnique AS LOGIC) AS LOGIC
        LOCAL ret AS LOGIC
        TRY
            cAlias := Path.GetFileNameWithoutExtension( cFilename )
            ret    := ! String.IsNullOrEmpty( cAlias )
        CATCH e AS ArgumentException
            Fail(e)
            ret := FALSE
        END TRY
        IF ret
            LOCAL sb AS StringBuilder
            sb := StringBuilder{cAlias:Length}
            FOREACH VAR c IN cAlias
                IF Char.IsLetterOrDigit(c)
                    sb:Append(c)
                ELSE
                    sb:Append('_')
                ENDIF
            NEXT
            cAlias := sb:ToString():ToUpperInvariant()
            IF lMakeUnique
                LOCAL cTemp := cAlias AS STRING
                LOCAL nCounter := 0 AS LONG
                VAR oSession  := RuntimeState.DataSession
                LOCAL nExistingArea := oSession:FindAlias(cAlias) AS DWORD
                DO WHILE nExistingArea != 0
                    nCounter++
                    cAlias := cTemp+"_"+nCounter:ToString()
                    nExistingArea := oSession:FindAlias(cAlias)
                ENDDO
            ENDIF
        ENDIF
        RETURN ret


        // Check if Alias is used for current thread
    INTERNAL STATIC METHOD IsAliasUnused( cAlias AS STRING ) AS LOGIC
        RETURN RuntimeState.DataSession:FindAlias(cAlias) == 0

        // Create RDD Object from RDD Type
    INTERNAL STATIC METHOD CreateRDDInstance( rddType AS Type , cAlias AS STRING) AS IRdd
        LOCAL ret    AS IRdd
        TRY
            ret := (IRdd) rddType:InvokeMember( NULL, BindingFlags.DeclaredOnly | BindingFlags.Public | BindingFlags.Instance | BindingFlags.CreateInstance, NULL, NULL, <OBJECT>{} )
            ret:Alias := cAlias:ToUpperInvariant()
        CATCH e AS Exception
            Fail(e)
            ret := NULL
        END TRY
        RETURN ret

    INTERNAL STATIC METHOD Fail(e AS Exception) AS VOID
        RuntimeState.LastRddError := e
        VAR oRdd := RuntimeState.DataSession:CurrentWorkarea
        LOCAL procName := ProcName(1) AS STRING
        IF procName == "COREDB:DO"
            procName := ProcName(2)
        ENDIF
        RAISE OperationFailed procName

    [DebuggerStepThroughAttribute];
    INTERNAL STATIC METHOD Do<T>(action AS @@Func<T>) AS T
        TRY
            RETURN action()
        CATCH e AS RddError
            Fail(e)
        END TRY
        RETURN DEFAULT(T)

     /// <summary>
     /// An event to which you can subscribe to be notified of operations on Workareas.
     /// </summary>
     PUBLIC STATIC EVENT Notify AS DbNotifyEventHandler

     PRIVATE STATIC PROPERTY HasEvents AS LOGIC GET Notify != NULL

     PRIVATE STATIC METHOD RaiseEvent(oRdd AS IRdd, nEvent AS DbNotificationType, oData AS OBJECT) AS VOID
        IF Notify != NULL
             Notify ( oRdd, DbNotifyEventArgs{ nEvent, oData})
        ENDIF
        RETURN
        /// <summary>
        /// Return the alias of a specified work area as a string.
        /// </summary>
        /// <param name="nArea"></param>
        /// <returns>
        /// </returns>
        /// <remarks><note type="tip">Alias(), VoDbAlias() and CoreDb.Alias() are aliases</note></remarks>
    STATIC METHOD Alias(nArea AS DWORD) AS STRING
        RETURN CoreDb.Do ({ =>
        RETURN RuntimeState.DataSession:GetAlias(nArea)
        })

        /// <summary>
        /// Add a new record.
        /// </summary>
        /// <param name="lReleaseLocks"></param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBAppend() but is strongly typed.
        /// <include file="CoreComments.xml" path="Comments/LastError/*" />
        /// <note type="tip">VoDbAppend() and CoreDb.Append() are aliases</note>
        /// </remarks>
    STATIC METHOD Append(lReleaseLocks AS LOGIC) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        VAR result := oRdd:Append(lReleaseLocks)
        RAISE RecordAppended oRdd:RecNo
        RETURN result

        })

        /// <summary>Retrieve information about a memo column.</summary>
        /// <param name="nOrdinal"></param>
        /// <param name="nPos"></param>
        /// <param name="oRet"></param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <seealso cref="O:XSharp.RT.Functions.VoDbBlobInfo" >VoDbBlobInfo overloads </seealso>
        /// <seealso cref="O:XSharp.VoDb.BlobInfo" >BlobInfo overloads in VoDb</seealso>
        /// <seealso cref="BlobInfo" >BlobInfo overloads in CoreDb</seealso>

    STATIC METHOD BlobInfo(nOrdinal AS DWORD,nPos AS DWORD,oRet AS OBJECT) AS LOGIC
        RETURN CoreDb.Do ({ =>
        RETURN CoreDb.BlobInfo(nOrdinal, nPos, REF oRet)
        })

    /// <inheritdoc cref="BlobInfo(System.UInt32,System.UInt32,System.Object)" />
    /// <param name="oRet">The returnvalue is returned through this parameter. When set on entry this is the new value of the setting.</param>
    STATIC METHOD BlobInfo(nOrdinal AS DWORD,nPos AS DWORD,oRet REF OBJECT) AS LOGIC
        var oTemp := oRet
        var result := CoreDb.Do ({ =>
            RETURN CoreDb.BlobInfo(nOrdinal, nPos, REF oTemp)
        })
        oRet := oTemp
        return result

        /// <summary>
        /// Determine when beginning-of-file is encountered.
        /// </summary>
        /// <returns>TRUE after an attempt to skip backward beyond the first logical record in a database file or if
        /// the current database file contains no records; otherwise, FALSE.  If there is no database file open in the
        /// current work area, CoreDbBOF() returns TRUE.</returns>
        /// <remarks>This function is like BOF().
        /// <note type="tip">VoDbBof() and CoreDb.Bof() are aliases</note></remarks>
        /// <seealso cref="M:XSharp.RT.Functions.Bof" >Bof Function </seealso>
        /// <seealso cref="M:XSharp.RT.Functions.VoDbBof" >VoDbBof Function in XSharp.VO</seealso>
    STATIC METHOD Bof() AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        RETURN oRdd:BoF
        })

        /// <summary>Refresh the buffer for the current Workarea, discarding any changes that were made.</summary>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBBuffRefresh().
        /// <include file="CoreComments.xml" path="Comments/LastError/*" />
        /// <note type="tip">VoDbBuffRefresh() and CoreDb.BuffRefresh() are aliases</note></remarks>
    STATIC METHOD BuffRefresh() AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        oRdd:RecInfo(DbRecordInfo.DBRI_UPDATED,0, NULL)
        RETURN TRUE
        })
        /// <summary>
        /// Clear a logical filter condition.
        /// </summary>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBClearFilter().
        /// <include file="CoreComments.xml" path="Comments/LastError/*" />
        /// <note type="tip">VoDbClearFilter() and CoreDb.ClearFilter() are aliases</note></remarks>

    STATIC METHOD ClearFilter() AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        BEFOREBULK
            VAR result := oRdd:ClearFilter()
        AFTERBULK
        RETURN result
        })

        /// <summary>
        /// Clear a locate condition by deleting the locate code block.
        /// </summary>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBClearLocate().
        /// <include file="CoreComments.xml" path="Comments/LastError/*" />
        /// <note type="tip">VoDbClearLocate() and CoreDb.ClearLocate() are aliases</note></remarks>
    STATIC METHOD ClearLocate() AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        RETURN oRdd:ClearScope()
        })

        /// <summary>Clear any active relations.</summary>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBClearRelation().
        /// <include file="CoreComments.xml" path="Comments/LastError/*" />
        /// <note type="tip">VoDbClearRelation() and CoreDb.ClearRelation() are aliases</note></remarks>

    STATIC METHOD ClearRelation() AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        BEFOREBULK
            VAR result := oRdd:ClearRel()
        AFTERBULK
        RETURN result
        })

        /// <summary>Clear the active locate condition.</summary>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBClearScope().
        /// <include file="CoreComments.xml" path="Comments/LastError/*" />
        /// <note type="tip">VoDbClearScope() and CoreDb.ClearScope() are aliases</note></remarks>
    STATIC METHOD ClearScope() AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        RETURN oRdd:ClearScope()
        })
        /// <summary>
        /// Close all files in all work areas.
        /// </summary>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBCloseAll().
        /// <include file="CoreComments.xml" path="Comments/LastError/*" />
        /// <note type="tip">VoDbCloseAll() and CoreDb.CloseAll() are aliases</note></remarks>
    STATIC METHOD CloseAll() AS LOGIC
        RETURN CoreDb.Do ({ =>
        var oSession := RuntimeState.DataSession
        VAR oRdd := oSession:CurrentWorkarea  // needed for BEFOREBULK and AFTERBULK
        BEFOREBULK
            VAR result := oSession:CloseAll()
        AFTERBULK
        RETURN result
        })
        /// <summary>
        /// Close all files in a work area.
        /// </summary>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBCloseArea().
        /// <include file="CoreComments.xml" path="Comments/LastError/*" />
        /// <note type="tip">VoDbCloseArea() and CoreDb.CloseArea() are aliases</note></remarks>
    STATIC METHOD CloseArea() AS LOGIC
        RETURN CoreDb.Do ({ =>
            VAR oSession := RuntimeState.DataSession
            VAR uiNewArea := oSession:CurrentWorkareaNO
            VAR oRdd := oSession:CurrentWorkarea
            IF oRdd != NULL
                RAISE FileClose  oRdd:Info(DbInfo.DBI_FULLPATH,NULL)
                RETURN oSession:CloseArea(uiNewArea)
            ENDIF
            RETURN FALSE
        })

        /// <summary>
        /// Discard all the changes to the current workares
        /// </summary>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    STATIC METHOD Refresh() AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        RETURN oRdd:Refresh()
        })


        /// <summary>
        /// Flush pending updates in one work area.
        /// </summary>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBCommit().
        /// <include file="CoreComments.xml" path="Comments/LastError/*" />
        /// <note type="tip">VoDbCommit() and CoreDb.Commit() are aliases</note></remarks>
    STATIC METHOD Commit() AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        LOCAL lOk := oRdd:Skip(0) AS LOGIC
        IF lOk
            lOk := oRdd:Flush()
        ENDIF
        IF (HasEvents)
            IF lOk
                RaiseEvent(oRdd, DbNotificationType.FileCommit, oRdd:Info(DbInfo.DBI_FULLPATH, NULL))
            ELSE
                RaiseEvent(oRdd, DbNotificationType.OperationFailed, __FUNCTION__)
            ENDIF
        ENDIF
        RETURN lOk
        })

        /// <summary>
        /// Flush pending updates in all work areas.
        /// </summary>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBCommitAll().
        /// <include file="CoreComments.xml" path="Comments/LastError/*" />
        /// <note type="tip">VoDbCommitAll() and CoreDb.CommitAll() are aliases</note></remarks>

    STATIC METHOD CommitAll() AS LOGIC
        RETURN CoreDb.Do ({ =>
        VAR oSession := RuntimeState.DataSession
        VAR oRdd := oSession:CurrentWorkarea
        BEFOREBULK
            VAR result := oSession:CommitAll()
        AFTERBULK
        RETURN result
        })
        /// <summary>
        /// Resume a pending locate condition.
        /// </summary>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBContinue().
        /// <include file="CoreComments.xml" path="Comments/LastError/*" />
        /// <note type="tip">VoDbContinue() and CoreDb.Continue() are aliases</note></remarks>

    STATIC METHOD Continue() AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        RETURN oRdd:Continue()
        })

    /// <inheritdoc cref="Create(System.String,XSharp.RDD.Support.RddFieldInfo[],System.Type,System.Boolean,System.String,System.String,System.Boolean,System.Boolean)" />
    /// <param name="cRddName">Name of RDD to use</param>

    STATIC METHOD Create( cName AS STRING, aStruct AS RddFieldInfo[], cRddName AS STRING, lNew AS LOGIC, cAlias AS STRING, cDelim AS STRING, lKeep AS LOGIC, lJustOpen AS LOGIC ) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL rddType AS Type
        IF ( rddType := CoreDb.RddNameToType( cRddName ) ) == NULL
            RddError.PostArgumentError( __FUNCTION__, EDB_RDDNOTFOUND, nameof(cRddName), 3, <OBJECT>{ cRddName } )
            RETURN FALSE
        ELSE
            RETURN CoreDb.Create( cName, aStruct, rddType, lNew, cAlias, cDelim, lKeep, lJustOpen )
        ENDIF
        })

    /// <inheritdoc cref="Create(System.String,XSharp.RDD.Support.RddFieldInfo[],System.Type,System.Boolean,System.String,System.String,System.Boolean,System.Boolean)" />
    /// <param name="rddList">List of RDDs to use when creating the file</param>
    STATIC METHOD Create( cName AS STRING, aStruct AS RddFieldInfo[], rddList AS _RddList, lNew AS LOGIC, cAlias AS STRING, cDelim AS STRING, lKeep AS LOGIC, lJustOpen AS LOGIC ) AS LOGIC
        LOCAL oRdd := NULL  AS RegisteredRDD
        FOREACH VAR name IN rddList:atomRddName
            oRdd := RegisteredRDD.Find(name)
            oRdd:Load()
        NEXT
        IF oRdd != NULL_OBJECT
            RETURN CoreDb.Create(cName, aStruct:ToArray(), oRdd:RddType, lNew, cAlias, cDelim, lKeep, lJustOpen)
        ENDIF
        RETURN FALSE

        /// <overloads>
        /// <summary>
        /// Create new file through the specified RDDs
        /// </summary>
        /// </overloads>
        /// <summary>
        /// Create new file through the specified RDDs
        /// </summary>
        /// <param name="cName">Name of the file to create. When no extension is specified then the default extension for the RDD will be used.</param>
        /// <param name="aStruct">Structure to use when creating the file.</param>
        /// <param name="rddType">Type of the class that must be used to work with the RDD.</param>
        /// <param name="lNew">TRUE opens the database file in a new work area (first available).  FALSE opens it in the current work area.  lNew is useful only when lOpen has a value of TRUE. The default is FALSE.</param>
        /// <param name="cAlias">The alias to be associated with the work area where the file is opened.  Within a single thread, X# will not accept duplicate aliases.  cAlias is useful only when lOpen has a value of TRUE.  The default alias is the filename without extension</param>
        /// <param name="cDelim">The delimiter for fields within a delimited database file. The default is a NULL string </param>
        /// <param name="lKeep">TRUE specifies that the file should remain open after creating. FALSE closes the file.</param>
        /// <param name="lJustOpen">TRUE specifies that an existing database file be opened. FALSE specifies that that a new database file be opened.  The default is FALSE.  This can be used to open existing SDF and delimited files, which do not have a structure in the header ? in which case, an empty aStruct should be used.</param>
        /// <returns>TRUE when succesfull, otherwise FALSE. When an error has occurred then you can retrieve that error from RuntimeState.LastRddError.</returns>
        /// <seealso cref="M:XSharp.RT.Functions.DbCreate(XSharp.__Usual,XSharp.__Usual,XSharp.__Usual,XSharp.__Usual,XSharp.__Usual,XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)" >DbCreate() function </seealso>
        /// <seealso cref="O:XSharp.RT.Functions.VoDbCreate" >VoDbCreate overloads</seealso>

    STATIC METHOD Create( cName AS STRING, aStruct AS RddFieldInfo[], rddType AS System.Type, lNew AS LOGIC, cAlias AS STRING, cDelim AS STRING, lKeep AS LOGIC, lJustOpen AS LOGIC ) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL uiOldArea := 0 AS DWORD
        LOCAL uiNewArea := 0 AS DWORD
        LOCAL ret   := FALSE   AS LOGIC
        VAR oSession := RuntimeState.DataSession
        RuntimeState.LastRddError := NULL
        IF String.IsNullOrEmpty( cName )
            RddError.PostArgumentError( __FUNCTION__, EDB_USE, nameof(cName), 1, <OBJECT>{ cName } )
        ELSEIF aStruct == NULL
            RddError.PostArgumentError( __FUNCTION__, EDB_USE, nameof(aStruct), 2 ,NULL)
        ELSEIF lNew && ! ( ret := CoreDb.Select( 0, REF uiOldArea ) )
            RddError.PostError( __FUNCTION__, EG_CREATE, EDB_NOAREAS )
        ELSE
            ret := TRUE
        ENDIF
        IF lNew
            uiNewArea := oSession:FindEmptyArea(TRUE)
        ELSE
            // VO Closes the current Workarea
            uiNewArea := oSession:CurrentWorkareaNO
            oSession:CloseArea(uiNewArea)
        ENDIF
        oSession:CurrentWorkareaNO := uiNewArea
        IF ret .AND. String.IsNullOrEmpty( cAlias )
            ret := CoreDb.AliasFromFilename( cName, REF cAlias ,!lKeep)
            IF ! ret
                RddError.PostArgumentError( __FUNCTION__, EDB_BADALIAS, nameof(cAlias), 5, <OBJECT>{ cAlias } )
            ENDIF
        ENDIF
        IF ret
            cAlias := cAlias:ToUpperInvariant()
        ENDIF
        IF ret
            ret := CoreDb.IsAliasUnused( cAlias )
            IF ! ret
                RddError.PostArgumentError( __FUNCTION__, EDB_DUPALIAS, nameof(cAlias), 5, <OBJECT>{ cAlias } )
            ENDIF
        ENDIF
        // Now all arguments are valid. So lets create the RDD Object and try to create the file
        LOCAL oRdd AS IRdd
        oRdd := CoreDb.CreateRDDInstance(rddType, cAlias)
        IF oRdd == NULL
            RddError.PostArgumentError( __FUNCTION__, EDB_DRIVERLOAD, nameof(rddType), 3, <OBJECT>{ rddType } )
            ret := FALSE
        ELSEIF ! CoreDb.IsAliasUnused( cAlias )
            RddError.PostArgumentError( __FUNCTION__, EDB_DUPALIAS, nameof(cAlias), 4, <OBJECT>{ cAlias } )
            ret := FALSE
        ELSE
            IF ! String.IsNullOrEmpty( cDelim )
                oRdd:Info( DBI_SETDELIMITER, cDelim )
            ENDIF
            IF ret
                ret := oRdd:CreateFields(aStruct)
            ENDIF
            IF ret
                LOCAL dboi := DbOpenInfo{} AS DbOpenInfo
                dboi:FullName  := cName
                var path := System.IO.Path.GetDirectoryName(cName)
                path           := CoreDb.AdjustPath(path)
                dboi:FullName  := System.IO.Path.Combine(path, System.IO.Path.GetFileName(dboi:FullName))
                dboi:Shared    := FALSE
                dboi:ReadOnly  := FALSE
                dboi:Alias     := cAlias
                dboi:Workarea  := uiNewArea
                oRdd:Alias     := cAlias
                ret := oSession:SetArea(uiNewArea, oRdd)
                IF lJustOpen
                    ret := oRdd:Open( dboi )
                ELSE
                    ret := oRdd:Create( dboi )
                ENDIF
                RAISE FileCreate  oRdd:Info(DbInfo.DBI_FULLPATH,NULL)

            ENDIF
            IF ret .AND. ! lKeep
                oSession:CloseArea(uiNewArea)
                IF uiOldArea != 0
                    oSession:CurrentWorkareaNO := uiOldArea
                ENDIF
            ENDIF
        ENDIF
        RETURN ret
        })

        /// <summary>Return the name of the alias.</summary>
        /// <remarks>In the FoxPro dialect the full path is returned.</remarks>
        STATIC METHOD Dbf AS STRING
            LOCAL oRdd := CoreDb.CWA("DBF", FALSE) AS IRdd
            IF oRdd != NULL
                IF XSharp.RuntimeState.Dialect == XSharpDialect.FoxPro
                   RETURN (STRING) oRdd:Info(DBI_FULLPATH, NULL)
                ELSE
                   RETURN oRdd:Alias
                ENDIF
            ENDIF
            RETURN String.Empty

        /// <summary>
        /// Mark the current record for deletion.
        /// </summary>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBDelete().
    /// <include file="CoreComments.xml" path="Comments/LastError/*" />
    /// <note type="tip">VoDbDelete() and CoreDb.Delete() are aliases</note></remarks>
    /// <seealso cref="M:XSharp.RT.Functions.DbDelete">DbDelete Function</seealso>
    STATIC METHOD Delete() AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        RAISE BeforeRecordDeleted  oRdd:RecNo
        VAR result := oRdd:Delete()
        RAISE AfterRecordDeleted  oRdd:RecNo
        RETURN result

        })

        /// <summary>
        /// Return the deleted status of the current record.
        /// </summary>
        /// <returns>TRUE if the current record is marked for deletion; otherwise, FALSE.
        /// If there is no database file in use in the current work area, CoreDbDeleted() returns FALSE.</returns>
        /// <seealso cref="M:XSharp.RT.Functions.Deleted">Deleted Function</seealso>
        /// <remarks><note type="tip">VoDbDelete() and CoreDb.Delete() are aliases</note></remarks>

    STATIC METHOD Deleted() AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        RETURN oRdd:Deleted
        })

        /// <summary>
        /// Determine when end-of-file is encountered.
        /// </summary>
        /// <returns>TRUE when an attempt is made to move the record pointer beyond the last logical record in a
        /// database file or if the current database file contains no records; otherwise, FALSE.  If there is no
        /// database file open in the current work area, CoreDbEOF() returns TRUE.</returns>
        /// <remarks>This function is like EOF().
        /// <note type="tip">VoDbEof() and CoreDb.Eof() are aliases</note></remarks>
        /// <seealso cref="M:XSharp.RT.Functions.Eof" >Eof Function </seealso>
        /// <seealso cref="M:XSharp.RT.Functions.VoDbEof" >VoDbEof Function in XSharp.VO</seealso>
    STATIC METHOD Eof() AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        RETURN oRdd:EoF
        })

        /// <summary>
        /// Evaluate a code block for each record that matches a specified scope and/or condition.
        /// </summary>
        /// <param name="uBlock">The code block to execute for each record that matches the scope and conditions. </param>
        /// <param name="uCobFor">A code block that defines a condition that each record within the scope must meet in order to be processed.</param>
        /// <param name="uCobWhile">A code block that defines another condition that each record must meet in order to be processed.  As soon as a record is encountered that causes the condition to fail, the operation terminates.  If no scope is specified, <paramref name="cbWhileCondition"/> changes the default scope to <paramref name="lRest"/>.</param>
        /// <param name="uNext">The number of records to process, starting at the current record.</param>
        /// <param name="nRecno">The number of the record to process.</param>
        /// <param name="lRest">TRUE processes only records from the current record to end-of-file.  FALSE processes all records.</param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>
        /// By default, Eval() operates on the currently selected work area.
        /// It will operate on an unselected work area if you specify it as part of an aliased expression.
        /// On each iteration, Eval() evaluates the specified code block.
        /// All records within the scope or matching the condition are processed until end-of-file is reached.
        /// Eval() can be used as a primitive for the construction of commands that process database files.
        /// In fact, many of the database processing commands are created using Eval().
        /// </remarks>

    STATIC METHOD Eval(uBlock AS ICodeblock,uCobFor AS ICodeblock,uCobWhile AS ICodeblock,uNext AS OBJECT,nRecno AS OBJECT,lRest AS LOGIC) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL nNext AS LONG
        IF uBlock == NULL
            THROW Error.ArgumentError(__FUNCTION__, nameof(uBlock),1, <OBJECT>{uBlock})
        ELSE
            TRY
                IF uNext != NULL
                    nNext := Convert.ToInt32(uNext)
                ELSE
                    nNext := 0
                ENDIF
            CATCH e AS Exception
                Fail(e)
                THROW Error.ArgumentError(__FUNCTION__, nameof(uNext),4, <OBJECT>{uNext})
            END TRY
        ENDIF
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        LOCAL oInfo AS DbEvalInfo
        oInfo := DbEvalInfo{}
        oInfo:Block := uBlock
        oInfo:ScopeInfo:ForBlock    := uCobFor
        oInfo:ScopeInfo:WhileBlock  := uCobWhile
        oInfo:ScopeInfo:NextCount   := nNext
        oInfo:ScopeInfo:RecId       := nRecno
        oInfo:ScopeInfo:Rest        := lRest
        RETURN oRdd:DbEval(oInfo)
        })

        /// <summary>Return the number of fields in the current Workarea</summary>

    STATIC METHOD FCount() AS DWORD
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__,FALSE) AS IRdd
        IF (oRdd != NULL)
            RETURN (DWORD) oRdd:FieldCount
        ENDIF
        RETURN 0

        /// <summary>
        /// Retrieve the value of a specified database field.
        /// </summary>
        /// <param name="nPos">The 1 based position of the field in the database file structure for the current work area.</param>
        /// <param name="oRet">The returnvalue is returned through this parameter</param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    STATIC METHOD FieldGet(nPos AS DWORD,oRet REF OBJECT) AS LOGIC
        var oTemp := oRet
        var result := CoreDb.Do( { =>
            LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
            oTemp := oRdd:GetValue((INT) nPos)
            return TRUE
        })
        oRet := oTemp
        return result

        /// <summary>
        /// Retrieve the value of a specified database field as an array of bytes
        /// </summary>
        /// <param name="nPos">The 1 based position of the field in the database file structure for the current work area.</param>
        /// <param name="oRet">The returnvalue is returned through this parameter</param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function only works with RDDs that inherit from the DBF RDD. The Advantage RDD does not support this.</remarks>
    STATIC METHOD FieldGetBytes(nPos AS DWORD,oRet REF BYTE[]) AS LOGIC
        var oTemp := oRet
        var lResult :=  CoreDb.Do( { =>
            LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
             IF oRdd IS Workarea VAR oDBF
                LOCAL oFld AS RddFieldInfo

                oFld := oDBF:GetField((LONG) nPos)
                IF oFld != NULL
                    IF oFld:FieldType.IsMemo()
                        VAR nBlock := oDBF:_getMemoBlockNumber( (LONG)nPos)
                        IF nBlock == 0
                            oTemp := BYTE[]{0}
                        ELSEIF oDBF:Memo != NULL
                            oTemp := (BYTE[]) oDBF:Memo:GetValue((LONG) nPos)
                        ELSE
                            oTemp := BYTE[]{0}
                        ENDIF
                    ELSE
                        VAR nOffSet := oFld:Offset
                        VAR nLen    := oFld:Length
                        LOCAL result AS BYTE[]
                        result := BYTE[]{nLen}
                        VAR aCopy := oDBF:GetRec()
                        System.Array.Copy(aCopy, nOffSet, result,0, nLen)
                        oTemp := result
                    ENDIF
                    RETURN TRUE
                ENDIF
            ENDIF
            oTemp := NULL
            RETURN FALSE
        })
        oRet := oTemp
        return lResult

    /// <summary>
    /// Retrieve the value of a specified database field as an array of bytes
    /// </summary>
    /// <param name="nPos">The 1 based position of the field in the database file structure for the current work area.</param>
    /// <param name="aValue">The value to write to the field</param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    /// <remarks>This function only works with RDDs that inherit from the DBF RDD. The Advantage RDD does not support this.</remarks>

    STATIC METHOD FieldPutBytes(nPos AS DWORD, aValue AS BYTE[]) AS LOGIC
        VAR lResult :=  CoreDb.Do( { =>
             LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
             IF oRdd IS Workarea VAR oWa
                // var oCanPut := oWa:Info(DBI_CANPUTREC, NULL)
                //IF oCanPut IS LOGIC VAR lCanPut .and. lCanPut
                    LOCAL oFld AS RddFieldInfo
                    oFld := oWa:GetField((LONG) nPos)
                    IF oFld != NULL
                        IF oFld:FieldType.IsMemo()
                            RETURN FieldPut(nPos, aValue)
                        ELSE
                            VAR nOffSet := oFld:Offset
                            VAR nLen    := oFld:Length
                            IF aValue != NULL
                                IF aValue:Length >= nLen
                                    VAR aCopy := oWa:GetRec()
                                    System.Array.Copy(aValue, 0, aCopy, nOffSet, nLen)
                                    oWa:PutRec(aCopy)
                                    RETURN TRUE
                                ELSE
                                    VAR oError := Error{EG_DATAWIDTH, __FUNCTION__, i"Not enough bytes for field. Required is {nLen} bytes" }
                                    THROW oError
                                ENDIF
                            ENDIF
                        ENDIF
                    ENDIF
                //ELSE
                //    VAR oError := Error{EG_UNSUPPORTED, __FUNCTION__, "RDD does not support PutRec" }
                //    THROW oError
                //ENDIF
            ENDIF
            RETURN FALSE
        })
        return lResult
        /// <inheritdoc cref="IRdd.FieldInfo" />
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBFieldInfo().
        /// <include file="CoreComments.xml" path="Comments/LastError/*" />
        /// </remarks>
        /// <seealso cref='O:XSharp.RT.Functions.VoDbFieldInfo' >VoDbFieldInfo overloads </seealso>
        /// <seealso cref='O:XSharp.VoDb.FieldInfo' >FieldInfo overloads in CoreDb</seealso>

    STATIC METHOD FieldInfo(nOrdinal AS DWORD,nFldPos AS DWORD,oValue AS OBJECT) AS LOGIC
        RETURN CoreDb.Do ({ =>
        RETURN CoreDb.FieldInfo(nOrdinal, nFldPos, REF oValue)
        })

    /// <inheritdoc cref="FieldInfo(System.UInt32,System.UInt32,System.Object)" />
    /// <param name="oRet">The returnvalue is returned through this parameter. When set on entry this is the new value of the setting.</param>
    STATIC METHOD FieldInfo(nOrdinal AS DWORD,nFldPos AS DWORD,oRet REF OBJECT) AS LOGIC
        VAR oTemp := oRet
        VAR lResult :=  CoreDb.Do( { =>
            LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
            oTemp := oRdd:FieldInfo((INT) nFldPos, (INT) nOrdinal, oTemp)
            RETURN TRUE
        })
        oRet := oTemp
        RETURN lResult

    /// <summary>
    /// Return the name of a field as a string.
    /// </summary>
    /// <param name="nPos">The 1 based position of the field in the database file structure for the current work area.</param>
    /// <returns>
    /// </returns>

    STATIC METHOD FieldName(nPos AS DWORD) AS STRING
        LOCAL oRdd := CoreDb.CWA("FieldName") AS IRdd
        IF (oRdd != NULL)
            RETURN oRdd:FieldName((INT) nPos)
        ENDIF
        RETURN String.Empty
        /// <summary>
        /// Return the position of a field.
        /// </summary>
        /// <param name="sFieldName"></param>
        /// <returns>
        /// </returns>
    STATIC METHOD FieldPos(sFieldName AS STRING) AS DWORD
        LOCAL oRdd := CoreDb.CWA("FieldPos",FALSE) AS IRdd
        IF (oRdd != NULL)
            RETURN (DWORD) oRdd:FieldIndex(sFieldName)
        ENDIF
        RETURN 0

        /// <summary>
        /// Set the value of a specified database field.
        /// </summary>
        /// <param name="nPos">The 1 based position of the field in the database file structure for the current work area.</param>
        /// <param name="xValue">The value to write to the field</param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>
        /// <include file="CoreComments.xml" path="Comments/LastError/*" />
        /// </remarks>
    STATIC METHOD FieldPut(nPos AS DWORD,xValue AS OBJECT) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        RAISE BeforeFieldUpdate oRdd:FieldName((INT) nPos)
        VAR result := oRdd:PutValue((INT) nPos, xValue)
        RAISE AfterFieldUpdate oRdd:FieldName((INT) nPos)
        RETURN result
        })

        /// <summary>Import contents from file into Memo field </summary>
        /// <param name="nPos">The 1 based position of the field in the database file structure for the current work area.</param>
        /// <param name="cFile">The name of the file where the value must be written.</param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBFileGet().
        /// <include file="CoreComments.xml" path="Comments/LastError/*" />
        /// <note type="tip">VoDbFileGet() and CoreDb.FileGet() are aliases</note></remarks>
    STATIC METHOD FileGet(nPos AS DWORD,cFile AS STRING) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        RETURN oRdd:GetValueFile((INT) nPos, cFile)
        })

        /// <summary>Export field contents from Memo field to file</summary>
        /// <param name="nPos">The 1 based position of the field in the database file structure for the current work area.</param>
        /// <param name="cFile">The name of the file to write</param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks><note type="tip">VoDbFilePut() and CoreDb.FilePut() are aliases</note></remarks>
    STATIC METHOD FilePut(nPos AS DWORD,cFile AS STRING) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        RAISE BeforeFieldUpdate oRdd:FieldName((INT) nPos)
        VAR result :=  oRdd:PutValueFile((INT) nPos, cFile)
        RAISE AfterFieldUpdate oRdd:FieldName((INT) nPos)
        RETURN result
        })

        /// <summary>
        /// Return a filter.
        /// </summary>
        /// <returns>
        /// </returns>
        /// <remarks>This function is like DBFilter().
        /// <include file="CoreComments.xml" path="Comments/LastError/*" />
        /// <note type="tip">VoDbFilter() and CoreDb.Filter() are aliases</note></remarks>

    STATIC METHOD Filter() AS STRING
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        RETURN oRdd:FilterText
        })

        /// <summary>
        /// Lock an opened and shared database file.
        /// </summary>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBFlock().
        /// <include file="CoreComments.xml" path="Comments/LastError/*" />
        /// <note type="tip">VoDbFlock() and CoreDb.Flock() are aliases</note></remarks>
    STATIC METHOD Flock() AS LOGIC
        RETURN CoreDb.Do ({ =>
        VAR oRdd := CoreDb.CWA(__FUNCTION__)
        LOCAL dbli AS DbLockInfo
        dbli := DbLockInfo{}
        dbli:Result := FALSE
        dbli:Method := DbLockInfo.LockMethod.File
        RETURN oRdd:Lock(dbli)
        })

        /// <summary>
        /// Determine if the previous search operation succeeded.
        /// </summary>
        /// <returns>
        /// </returns>
        /// <remarks>This function is like Found().
        /// <include file="CoreComments.xml" path="Comments/LastError/*" />
        /// <note type="tip">VoDbFound() and CoreDb.Found() are aliases</note></remarks>

    STATIC METHOD Found() AS LOGIC
        RETURN CoreDb.Do ({ =>
        VAR oRdd := CoreDb.CWA(__FUNCTION__)
        RETURN oRdd:Found
        })


        /// <summary>
        /// Return the work area number.
        /// </summary>
        /// <returns>
        /// </returns>
        /// <remarks>This function is like DBGetSelect().
        /// <include file="CoreComments.xml" path="Comments/LastError/*" />
        /// <note type="tip">VoDbGetSelect() and CoreDb.GetSelect() are aliases</note></remarks>

    STATIC METHOD GetSelect() AS DWORD
        RETURN RuntimeState.DataSession:CurrentWorkareaNO
        /// <summary>
        /// Move to the last logical record.
        /// </summary>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBGoBottom().
        /// <include file="CoreComments.xml" path="Comments/LastError/*" />
        /// <note type="tip">VoDbGoBottom() and CoreDb.GoBottom() are aliases</note></remarks>


    STATIC METHOD GoBottom() AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        BEFOREMOVE
        VAR result := oRdd:GoBottom()
        AFTERMOVE
        RETURN result
        })
        /// <summary>
        /// Move to a record specified by record number.
        /// </summary>
        /// <param name="uRecId">ID of the record to goto</param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    STATIC METHOD Goto(uRecId AS OBJECT) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        BEFOREMOVE
        VAR result := oRdd:GoToId(uRecId)
        AFTERMOVE
        RETURN result
        })

        /// <summary>
        /// Move to the first logical record.
        /// </summary>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBGoTop().
        /// <include file="CoreComments.xml" path="Comments/LastError/*" />
        /// <note type="tip">VoDbGoTop() and CoreDb.GoTop() are aliases</note></remarks>


    STATIC METHOD GoTop() AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        BEFOREMOVE
        VAR result := oRdd:GoTop()
        AFTERMOVE
        RETURN result
        })


    STATIC METHOD  Header() AS LONG
        LOCAL oValue := NULL AS OBJECT
        IF CoreDb.Info(DBI_GETHEADERSIZE, REF oValue)
            RETURN (LONG) oValue
        ENDIF
        RETURN 0

        /// <summary>
        /// Retrieve information about a work area.
        /// </summary>
        /// <param name="nOrdinal"></param>
       /// <param name="oValue">If specified, this parameter is used to change the value of a setting. This parameter also receives the return value.</param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <seealso cref='O:XSharp.RT.Functions.VoDbInfo' >VoDbInfo overloads </seealso>
        /// <seealso cref='O:XSharp.VoDb.Info' >Info overloads in VoDb</seealso>

    STATIC METHOD Info(nOrdinal AS DWORD,oValue AS OBJECT) AS LOGIC
        RETURN CoreDb.Do ({ =>
        RETURN CoreDb.Info(nOrdinal, REF oValue)
        })


    /// <inheritdoc cref="Info" />
    /// <param name="oValue">If specified, this parameter is used to change the value of a setting. This parameter also receives the return value.</param>

    STATIC METHOD Info(nOrdinal AS DWORD,oValue REF OBJECT) AS LOGIC
        VAR oTemp := oValue
        VAR result := CoreDb.Do ({ =>
            RuntimeState.LastRddError := NULL
            LOCAL oRdd := CoreDb.CWA(__FUNCTION__, FALSE) AS IRdd
            IF oRdd != null
                IF (nOrdinal == DBI_RDD_OBJECT)
                    oTemp := oRdd
                ELSEIF (nOrdinal == DBI_RDD_LIST)
                    oTemp := _RddList{(Workarea) oRdd}
                ELSE
                    oTemp := oRdd:Info((INT) nOrdinal, oTemp)
                ENDIF
                if RuntimeState.LastRddError != NULL
                    RETURN FALSE
                ENDIF
                RETURN TRUE
            ENDIF
            RETURN FALSE
        })
        oValue := oTemp
        RETURN result

        /// <summary>Write values to destination Workarea in a JOIN operation</summary>
        /// <param name="nSelect"></param>
        /// <param name="struList"></param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    STATIC METHOD JoinAppend(nSelect AS DWORD,struList AS _JoinList) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL result := FALSE AS LOGIC
        LOCAL nCount AS LONG
        LOCAL nDestSel AS DWORD
        LOCAL nFld AS LONG
        LOCAL oRddDest AS IRdd
        LOCAL oRddSrc AS IRdd
        LOCAL oValue  AS OBJECT
        VAR oSession := RuntimeState.DataSession
        nCount := struList:Count
        nDestSel := struList:uiDestSel
        oRddDest := oSession:GetRDD(nDestSel)
        IF oRddDest == NULL
            RddError.PostNoTableError(__FUNCTION__)
        ELSE
            FOR nFld := 0 TO nCount-1
                oRddSrc := oSession:GetRDD(struList:Fields[nFld]:Area)
                IF oRddSrc == NULL_OBJECT
                    RddError.PostNoTableError(__FUNCTION__)
                ENDIF
                oValue := oRddSrc:GetValue((INT) struList:Fields[nFld]:Pos)
                result := oRddDest:PutValue(nFld, oValue)
                IF ! result
                    EXIT
                ENDIF
            NEXT
        ENDIF
        RETURN result
        })

        /// <summary>
        /// Return the number of the last record in a database file.
        /// </summary>
        /// <returns>
        /// </returns>
        /// <remarks> <note type="tip">VoDbLastRec() and CoreDb.LastRec() are aliases</note></remarks>

    STATIC METHOD LastRec() AS LONG
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        RETURN oRdd:RecCount
        })

        /// <summary>
        /// Search for the first record that matches a specified condition and scope.
        /// </summary>
        /// <param name="uCobFor"><include file="VoFunctionDocs.xml" path="Runtimefunctions/cbfor/*" /></param>
        /// <param name="uCobWhile"><include file="VoFunctionDocs.xml" path="Runtimefunctions/cbwhile/*" /></param>
        /// <param name="nNext">The number of records to process, starting with the current record.</param>
        /// <param name="uRecId">The number of the record to process.</param>
        /// <param name="lRest">TRUE processes only records from the current record to end-of-file.  FALSE processes all records.</param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBLocate() but strongly typed.
        /// <include file="CoreComments.xml" path="Comments/LastError/*" />
        /// </remarks>
    STATIC METHOD Locate(uCobFor AS ICodeblock,uCobWhile AS ICodeblock,nNext AS LONG,uRecId AS OBJECT,lRest AS LOGIC) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        LOCAL scopeinfo := DbScopeInfo{} AS DbScopeInfo
        LOCAL result AS LOGIC
        scopeinfo:ForBlock := uCobFor
        scopeinfo:WhileBlock := uCobWhile
        scopeinfo:Rest:= lRest
        scopeinfo:RecId := IIF(uRecId IS LONG, uRecId, NULL)
        scopeinfo:NextCount := nNext
        BEFOREMOVE
        result := oRdd:SetScope(scopeinfo)
        IF result
            result := oRdd:SkipScope(1)
        ENDIF
        AFTERMOVE
        RETURN result
        })

        /// <summary>Return Memo File extension</summary>
        /// <param name="cDriver"></param>
        /// <returns>
        /// </returns>
        /// <remarks> <note type="tip">VoDbMemoExt() and CoreDb.MemoExt() are aliases</note></remarks>

    STATIC METHOD MemoExt(cDriver AS STRING) AS STRING
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := NULL AS IRdd
        IF String.IsNullOrEmpty(cDriver)
            oRdd := CoreDb.CWA(__FUNCTION__, FALSE)
            IF oRdd == NULL
                cDriver := RuntimeState.DefaultRDD
            ENDIF
        ENDIF
        IF oRdd == NULL
            LOCAL oRegRDD AS RegisteredRDD
            oRegRDD:= RegisteredRDD.Find(cDriver)
            oRegRDD:Load()
            oRdd := CoreDb.CreateRDDInstance( oRegRDD:RddType, "XXTEMPXX" )
        ENDIF
        // Note that we do not close oRDD here. When it was crated the GC will throw it away
        RETURN (STRING) oRdd:Info(DBI_MEMOEXT, NULL)
        })
        /// <summary>
        /// Return the default index file extension for a work area as defined by the its RDD.
        /// </summary>
        /// <returns>Default extension for the current Workarea, or an empty string when no table is open in the current Workarea.
        /// </returns>
    STATIC METHOD OrdBagExt() AS STRING
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__,FALSE) AS IRdd
        IF oRdd == NULL
            // Get an RDD object
            LOCAL oRegRDD AS RegisteredRDD
            oRegRDD:= RegisteredRDD.Find(RuntimeState.DefaultRDD)
            oRegRDD:Load()
            oRdd := CoreDb.CreateRDDInstance( oRegRDD:RddType, "XXTEMPXX" )
        ENDIF
        VAR info := DbOrderInfo{}
        oRdd:OrderInfo(DBOI_DEFBAGEXT, info)
        RETURN (STRING) info:Result
        })

        /// <summary>
        /// Set the condition and scope for an order.
        /// </summary>
        /// <param name="ordCondInfo">An object defining the condition and scope information. </param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like OrdCondSet() but strongly typed and the condition information is passed in an object.
        /// <include file="CoreComments.xml" path="Comments/LastError/*" />
        /// </remarks>
    STATIC METHOD OrdCondSet(ordCondInfo AS DbOrderCondInfo) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        RETURN oRdd:OrderCondition(ordCondInfo)
        })
        /// <summary>
        /// Create or replace an order in an index file.
        /// </summary>
        /// <param name="cBagName">Orderbag name (index filename) to create the order in.</param>
        /// <param name="uOrder">Ordername to create.</param>
        /// <param name="cExpr">The order key expression specified as a string</param>
        /// <param name="uCobExpr">The order key expression specified as a codeblock.</param>
        /// <param name="lUnique">TRUE creates a unique order by including only those records with unique key values; FALSE uses all records in the database file. </param>
        /// <param name="ordCondInfo">An object defining the condition and scope information. </param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DbCreateOrder() but strongly typed and the condition information is passed in an object.
        /// <include file="CoreComments.xml" path="Comments/LastError/*" />
        /// </remarks>


    STATIC METHOD OrdCreate(cBagName AS STRING,oOrder AS OBJECT,cExpr AS STRING,oCodeBlock AS ICodeblock,lUnique AS LOGIC,ordCondInfo AS DbOrderCondInfo) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        VAR info := DbOrderCreateInfo{}
        info:BagName 		:= cBagName
        info:Order			:= oOrder
        info:Expression 	:= cExpr
        info:Block      	:= oCodeBlock
        info:Unique			:= lUnique
        info:OrdCondInfo 	:= ordCondInfo
        BEFOREBULK
        VAR result := oRdd:OrderCreate(info)
        AFTERBULK
        RAISE IndexCreate SAVEBAGNAME(cBagName , oOrder)
        RETURN result
        })
        /// <summary>
        /// Remove an order from an open index file.
        /// </summary>
        /// <param name="cBagName">The name of an index file, including an optional drive and directory.  Use this argument with &lt;cOrder&gt; to remove ambiguity when there are two or more orders with the same name in different index files.</param>
        /// <param name="oOrder">The name of the order or a number representing its position in the order list.  Using the order name is the preferred method since the position may be difficult to determine using multiple-order index files.  Invalid values are ignored.</param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DbDeleteOrder() but strongly typed.
        /// <include file="CoreComments.xml" path="Comments/LastError/*" />
        /// </remarks>


    STATIC METHOD OrdDestroy(cBagName AS STRING,oOrder AS OBJECT) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        VAR info := DbOrderInfo{}
        info:BagName := cBagName
        info:Order   := oOrder
        if oOrder == NULL
            RddError.PostArgumentError( __FUNCTION__, EDB_ORDDESTROY, nameof(oOrder), 2, <OBJECT>{ oOrder } )
        ENDIF
        VAR result := oRdd:OrderDestroy(info)
        RAISE IndexDelete SAVEBAGNAME(cBagName , oOrder)
        RETURN result
        })
        /// <summary>
        /// Return information about index files and the orders in them.
        /// </summary>
        /// <param name="nOrdinal">Specifies the type of information. This must match the values in the DBOI_ defines</param>
        /// <param name="cBagName">The name of an index file, including an optional drive and directory.  </param>
        /// <param name="oOrder">The name of the order about which you want to obtain information or a number representing its position in the order list.</param>
        /// <param name="oValue">If specified, this parameter is used to change the value of a setting. </param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DbOrderInfo() but strongly typed.
        /// <include file="CoreComments.xml" path="Comments/LastError/*" />
        /// </remarks>
        /// <seealso cref='O:XSharp.RT.Functions.VoDbOrderInfo' >VoDbOrderInfo overloads </seealso>
        /// <seealso cref='O:XSharp.VoDb.OrderInfo' >OrderInfo overloads in VoDb</seealso>

        STATIC METHOD OrderInfo(nOrdinal AS DWORD,cBagName AS STRING,oOrder AS OBJECT,oValue AS OBJECT) AS LOGIC
        RETURN CoreDb.Do ({ =>
        RETURN CoreDb.OrderInfo(nOrdinal, cBagName, oOrder, REF oValue)
        })

    /// <inheritdoc cref="OrderInfo" />
    /// <param name="oValue">If specified, this parameter is used to change the value of a setting and retrieve the current setting. </param>
    STATIC METHOD OrderInfo(nOrdinal AS DWORD,cBagName AS STRING,oOrder AS OBJECT,oValue REF OBJECT) AS LOGIC
        VAR oTemp := oValue
        VAR result := CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
            VAR info := DbOrderInfo{}
            info:BagName := cBagName
            info:Order   := oOrder
            info:Result  := oTemp
            oRdd:OrderInfo(nOrdinal, info)
            oTemp :=  info:Result
            RETURN TRUE
        })
        oValue := oTemp
        return result


        /// <summary>
        /// Open an index file and add specified orders to the order list in a work area.
        /// </summary>
        /// <param name="cBagName">The name of an index file, including an optional drive and directory.  Use this argument with &lt;cOrder&gt; to remove ambiguity when there are two or more orders with the same name in different index files.</param>
        /// <param name="oOrder">The name of the order or a number representing its position in the order list.  Using the order name is the preferred method since the position may be difficult to determine using multiple-order index files.  Invalid values are ignored.</param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DbSetIndex() but strongly typed.
        /// <include file="CoreComments.xml" path="Comments/LastError/*" />
        /// </remarks>
    STATIC METHOD OrdListAdd(cBagName AS STRING,oOrder AS OBJECT) AS LOGIC
        RETURN CoreDb.Do ({ =>
        cBagName := cBagName?:Trim()
        IF String.IsNullOrEmpty(cBagName)
            RddError.PostArgumentError( __FUNCTION__, EDB_SETINDEX, nameof(cBagName), 1, <OBJECT>{ cBagName } )
            RETURN FALSE
        ELSE
            LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
            VAR info := DbOrderInfo{}
            info:BagName := cBagName
            IF oOrder == NULL
                info:AllTags := TRUE
            ELSE
                info:Order   := oOrder
            ENDIF
            VAR result := oRdd:OrderListAdd(info)
            RAISE IndexOpen SAVEBAGNAME(cBagName , oOrder)
            RETURN result
        ENDIF
        })

        /// <summary>
        /// Remove orders from the order list in a work area and close associated index files.
        /// </summary>
        /// <param name="cBagName">The name of an index file, including an optional drive and directory.  Use this argument with &lt;cOrder&gt; to remove ambiguity when there are two or more orders with the same name in different index files.</param>
        /// <param name="oOrder">The name of the order or a number representing its position in the order list.  Using the order name is the preferred method since the position may be difficult to determine using multiple-order index files.  Invalid values are ignored.</param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBClearIndex() but strongly typed.
        /// <include file="CoreComments.xml" path="Comments/LastError/*" />
        /// </remarks>


    STATIC METHOD OrdListClear(cBagName AS STRING,oOrder AS OBJECT) AS LOGIC
        RETURN CoreDb.Do ({ =>

        LOCAL oRdd := CoreDb.CWA(__FUNCTION__,FALSE) AS IRdd
        IF oRdd == NULL
            RETURN TRUE // not logical but compatible with VO
        ELSE
            VAR info := DbOrderInfo{}
            cBagName := cBagName?:Trim()
            info:BagName := cBagName
            IF oOrder == NULL .AND. String.IsNullOrEmpty(cBagName)
                info:AllTags := TRUE
            ELSE
                info:Order   := oOrder
            ENDIF
            VAR result := oRdd:OrderListDelete(info)
            RAISE IndexClose SAVEBAGNAME(cBagName , oOrder)
            RETURN result
        ENDIF
        })
        /// <summary>
        /// Rebuild all orders in the order list of a work area.
        /// </summary>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like OrdListRebuild().
        /// <include file="CoreComments.xml" path="Comments/LastError/*" />
        /// </remarks>


    STATIC METHOD OrdListRebuild() AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        BEFOREBULK
        VAR result := oRdd:OrderListRebuild()
        AFTERBULK
        RETURN result
        })

        /// <summary>
        /// Set the controlling order for a work area.
        /// </summary>
        /// <param name="cBagName">The name of an index file, including an optional drive and directory.  Use this argument with &lt;cOrder&gt; to remove ambiguity when there are two or more orders with the same name in different index files.</param>
        /// <param name="oOrder">The name of the order or a number representing its position in the order list.  Using the order name is the preferred method since the position may be difficult to determine using multiple-order index files.  Invalid values are ignored.</param>
        /// <param name="strPreviousOrder"></param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DbSetOrder() but strongly typed.
        /// <include file="CoreComments.xml" path="Comments/LastError/*" />
        /// </remarks>
    STATIC METHOD OrdSetFocus(cBagName AS STRING,oOrder AS OBJECT) AS LOGIC
        RETURN CoreDb.Do ({ =>
            LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
            VAR info     := DbOrderInfo{}
            cBagName := cBagName?:Trim()
            info:BagName := cBagName
            if oOrder != NULL .and. SystemTypeToUsualType(oOrder:GetType()) == __UsualType.Symbol
                oOrder := oOrder:ToString()
            endif
            info:Order   := oOrder
            VAR result := oRdd:OrderListFocus(info)
            RAISE OrderChanged oRdd:OrderInfo(DBOI_NAME,info)
            RETURN  result
         })

        /// <summary>
        /// Set the controlling order for a work area.
        /// </summary>
        /// <param name="cBagName">The name of an index file, including an optional drive and directory.  Use this argument with &lt;cOrder&gt; to remove ambiguity when there are two or more orders with the same name in different index files.</param>
        /// <param name="oOrder">The name of the order or a number representing its position in the order list.  Using the order name is the preferred method since the position may be difficult to determine using multiple-order index files.  Invalid values are ignored.</param>
        /// <param name="strPreviousOrder">This parameter returns the previous order</param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DbSetOrder() but strongly typed.
        /// <include file="CoreComments.xml" path="Comments/LastError/*" />
        /// </remarks>
    STATIC METHOD OrdSetFocus(cBagName AS STRING,oOrder AS OBJECT, strPreviousOrder OUT STRING) AS LOGIC
        local strTemp := "" AS STRING
        var lResult := CoreDb.Do ({ =>
            LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
            LOCAL result AS OBJECT
            VAR info     := DbOrderInfo{}
            strTemp := String.Empty
            result := oRdd:OrderInfo(DBOI_NAME,info)
            IF result IS STRING VAR cOrder
                strTemp := cOrder
            ENDIF
            if oOrder != NULL .and. SystemTypeToUsualType(oOrder:GetType()) == __UsualType.Symbol
                oOrder := oOrder:ToString()
            endif
            cBagName     := cBagName?:Trim()
            info:BagName := cBagName
            info:Order   := oOrder
            VAR isOk := oRdd:OrderListFocus(info)
            IF HasEvents .AND. ! info:IsEmpty
                RAISE OrderChanged info:Result
            ENDIF
            RETURN isOk
        })
        strPreviousOrder := strTemp
        return lResult


        /// <summary>
        /// Remove all records that have been marked for deletion from a database file.
        /// </summary>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DbPack().
        /// <include file="CoreComments.xml" path="Comments/LastError/*" />
        /// <note type="tip">VoDbPack() and CoreDb.Pack() are aliases</note></remarks>
    STATIC METHOD Pack() AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        BEFOREBULK
        VAR result := oRdd:Pack()
        AFTERBULK
        RETURN result
        })


        /// <summary>
        /// </summary>
        /// <returns>
        /// </returns>
        /// <remarks><note type="tip">VoDbRddCount() and CoreDb.RddCount() are aliases</note></remarks>

    STATIC METHOD RddCount() AS DWORD
        RETURN CoreDb.Do ({ =>
        RETURN (DWORD) CoreDb.RddList():Length
        })
        /// <summary>Return and optionally change settings controlled directly by the RDD.</summary>
        /// <param name="nOrdinal">Ordinal number of the setting to set/retrieve.</param>
        /// <param name="oValue">The returnvalue is returned through this parameter. When set on entry this is the new value of the setting.</param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <seealso cref='O:XSharp.RT.Functions.VoDbRddInfo' >VoDbRddInfo overloads </seealso>
        /// <seealso cref='O:XSharp.VoDb.RddInfo' >RddInfo overloads in VoDb</seealso>

    STATIC METHOD RddInfo(nOrdinal AS DWORD,oValue REF OBJECT) AS LOGIC
        VAR oTemp := oValue
        VAR result := CoreDb.Do ({ =>
        LOCAL oResult AS OBJECT
            oResult := RuntimeState.GetValue<OBJECT> ((XSharp.Set) nOrdinal)
            IF oTemp != NULL_OBJECT
                RuntimeState.SetValue((XSharp.Set) nOrdinal, oTemp)
            ENDIF
            oTemp := oResult
            RETURN oResult != NULL
        })
        oValue := oTemp
        RETURN result


    /// <inheritdoc cref="RddInfo" />
    STATIC METHOD RddInfo(nOrdinal AS DWORD,oValue AS OBJECT) AS LOGIC
        RETURN CoreDb.Do ({ =>
        RuntimeState.SetValue((XSharp.Set) nOrdinal, oValue)
        RETURN TRUE
        })


    [Obsolete( "'CoreDbRddList( rddList, nRddType )' is not supported, use CoreDbRddList() instead", TRUE )];
    STATIC METHOD RddList(rddList AS _RddList,nRddType AS DWORD) AS LOGIC
        THROW  NotImplementedException{}


        /// <summary>Returns list of RDD names that are in use at this moment</summary>
    /// <remarks><note type="tip">VoDbRddList() and CoreDb.RddList() are aliases</note></remarks>
    STATIC METHOD RddList() AS STRING[]
        RETURN CoreDb.Do ({ =>
        LOCAL aList AS List<STRING>
        aList := List<STRING>{}
        LOCAL i AS DWORD
        var oSession := RuntimeState.DataSession
        FOR i := 1 TO DataSession.MaxWorkareas
            VAR oRdd := oSession.GetRDD(i)
            IF oRdd != NULL
                LOCAL cName AS STRING
                cName := oRdd:Driver
                IF !aList:Contains(cName)
                    aList:Add(cName)
                ENDIF
            ENDIF
        NEXT
        RETURN aList:ToArray()
        })


        /// <summary>
        /// Return an RDD name.
        /// </summary>
        /// <returns>The name of the RDD</returns>
        /// <remarks><note type="tip">VoDbRddName() and CoreDb.RddName() are aliases</note></remarks>

    STATIC METHOD RddName() AS STRING
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        RETURN oRdd:Driver
        })
        /// <summary>
        /// Return and optionally change the default RDD for the application.
        /// </summary>
        /// <param name="cNewRDD"></param>
        /// <returns>The previous default RDD</returns>
        /// <remarks><note type="tip">VoDbRddSetDefault() and CoreDb.RddSetDefault() are aliases</note></remarks>

    STATIC METHOD RddSetDefault(cNewRDD AS STRING) AS STRING
        LOCAL cOldRDD AS STRING
        cOldRDD := RuntimeState.DefaultRDD
        IF ! String.IsNullOrEmpty(cNewRDD)
            RuntimeState.DefaultRDD := cNewRDD
        ENDIF
        RETURN cOldRDD

        /// <summary>
        /// Restore the current record if it is marked for deletion.
        /// </summary>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBRecall().
        /// <include file="CoreComments.xml" path="Comments/LastError/*" />

        /// <note type="tip">VoDbRecall() and CoreDb.Recall() are aliases</note></remarks>

    STATIC METHOD Recall() AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        RAISE BeforeRecordRecalled  oRdd:RecNo
        VAR result := oRdd:Recall()
        RAISE AfterRecordRecalled  oRdd:RecNo
        RETURN result
        })


        /// <summary>Return the record length in the current Workarea</summary>
        STATIC METHOD RecSize AS LONG
            LOCAL nSize := NULL AS OBJECT
            IF CoreDb.Info(DbInfo.DBI_GETRECSIZE, REF nSize)
                RETURN (LONG) nSize
            ENDIF
            RETURN 0

        /// <summary>
        /// Return the current record number.
        /// </summary>
        /// <returns>The 1 based current record number.</returns>
        /// <remarks><note type="tip">VoDbRecno() and CoreDb.Recno() are aliases</note></remarks>

    STATIC METHOD Recno() AS DWORD
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        RETURN (DWORD) oRdd:RecNo
        })

        /// <summary>Get the contents of the current record as an array of bytes</summary>
        /// <returns>An array of bytes that contains the current record.</returns>
        /// <remarks><note type="tip">VoDbRecordGet() and CoreDb.RecordGet() are aliases</note></remarks>

    STATIC METHOD RecordGet() AS BYTE[]
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        RETURN oRdd:GetRec()
        })

        /// <summary>
        /// Retrieve information about a record.
        /// </summary>
        /// <param name="nOrdinal">This must match one of the values from the DbRecordInfo Enum</param>
        /// <param name="oRecID">Some of the DbRecordInfo enum values require a record number</param>
        /// <param name="oValue">Some of the DbRecordInfo enum values require a new value. </param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <seealso cref='O:XSharp.RT.Functions.VoDbRecordInfo'  >VoDbRecordInfo overloads</seealso>
        /// <seealso cref='O:XSharp.VoDb.RecordInfo'  >RecordInfo overloads in VoDb</seealso>
        /// <seealso cref='DbRecordInfo'>DbRecordInfo ENUM</seealso>

    STATIC METHOD RecordInfo(nOrdinal AS DWORD,oRecID AS OBJECT,oValue AS OBJECT) AS LOGIC
        RETURN CoreDb.Do ({ =>
        RETURN CoreDb.RecordInfo(nOrdinal, oRecID, REF oValue)
        })

    /// <inheritdoc cref="RecordInfo(System.UInt32,System.Object,System.Object)" />
    /// <param name="oRet">The returnvalue is returned through this parameter. When set on entry this is the new value of the setting.</param>
    STATIC METHOD RecordInfo(nOrdinal AS DWORD,oRecID AS OBJECT,oValue REF OBJECT) AS LOGIC
        VAR oTemp := oValue
        VAR result := CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        oTemp := oRdd:RecInfo( (INT) nOrdinal,oRecID, oTemp )
        RETURN TRUE
        })
        oValue := oTemp
        RETURN result

        /// <summary>Update the current record from an array of bytes</summary>
        /// <param name="aRecord">The bytes that form the record. Please note that if the DBF has a memo file, then this array must contain a valid position for the memo attached to the record.</param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks><note type="tip">VoDbRecordPut() and CoreDb.RecordPut() are aliases</note></remarks>

    STATIC METHOD RecordPut(aRecord AS BYTE[]) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        RETURN oRdd:PutRec(aRecord)
        })

        /// <summary>
        /// Return the linking expression of a specified relation.
        /// </summary>
        /// <param name="nPos">The position of the desired relation in the list of current work area relations.  The relations are numbered according to the order in which they were defined by relation setting.</param>
        /// <param name="pszRel">The linking expression defined to &lt;nRelation&gt;.</param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBSetRelation().
        /// <include file="CoreComments.xml" path="Comments/LastError/*" />
        /// </remarks>

    STATIC METHOD Relation(nPos AS DWORD,sRel REF STRING) AS LOGIC
        VAR sTemp := sRel
        VAR result := CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        sTemp :=  oRdd:RelText(nPos)
        RETURN TRUE
        })
        sRel := sTemp
        RETURN result

        /// <summary>
        /// Lock the current record.
        /// </summary>
        /// <param name="uRecId">The ID (usually a record number) of the record to be locked.  If specified, record locks held by the current process are retained.  If not specified, all locks held by the current process are released and the current record is assumed.</param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBRlock() but strongly typed.
        /// <include file="CoreComments.xml" path="Comments/LastError/*" />
        /// </remarks>

    STATIC METHOD RLock(uRecId AS OBJECT) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        IF oRdd:RecCount  == 0
            RETURN TRUE
        ENDIF
        LOCAL lockInfo AS DbLockInfo
        lockInfo := DbLockInfo{}
        lockInfo:RecId := uRecId
        IF uRecId == NULL
            lockInfo:Method  := DbLockInfo.LockMethod.Exclusive
        ELSE
            lockInfo:Method  := DbLockInfo.LockMethod.Multiple
        ENDIF
        VAR result := oRdd:Lock(lockInfo)
        RAISE RecordLocked IIF(uRecId == NULL, oRdd:RecNo, uRecId)
        RETURN result
        })
        /// <summary>
        /// Return the work area number of a relation.
        /// </summary>
        /// <param name="nPos">The position of the desired relation in the list of work area relations.  The relations are numbered according to the order in which they were defined by relation setting.</param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBRSelect() but strongly typed.
        /// <include file="CoreComments.xml" path="Comments/LastError/*" />
        /// <note type="tip">VoDbRSelect() and CoreDb.RSelect() are aliases</note></remarks>


    STATIC METHOD RSelect(nPos AS DWORD) AS DWORD
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        RETURN oRdd:RelArea(nPos)
        })
        /// <summary>
        /// Move to the record having the specified key value.
        /// </summary>
        /// <param name="oValue">Specifies the key value associated with the desired record.</param>
        /// <param name="lSoftSeek">Determines how the work area is positioned if the specified key value is not found: TRUE performs a soft seek; FALSE does not.</param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBSeek() but strongly typed.
        /// <include file="CoreComments.xml" path="Comments/LastError/*" />
        /// </remarks>

    STATIC METHOD Seek(oValue AS OBJECT,lSoftSeek AS LOGIC, lLast AS LOGIC) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        VAR info 		:= DbSeekInfo{}
        info:Value 		:= oValue
        info:SoftSeek 	:= lSoftSeek
        info:Last		:= lLast
        BEFOREMOVE
        VAR result := oRdd:Seek(info)
        AFTERMOVE
        RETURN result
        })


        /// <summary>
        /// Select a new work area and retrieve the current work area.
        /// </summary>
        /// <param name="nNew">The work area number for the new work area.</param>
        /// <param name="nOld">The work area number for the old work area.</param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBSelect() but strongly typed.
        /// <include file="CoreComments.xml" path="Comments/LastError/*" />
        /// <note type="tip">VoDbSelect() and CoreDb.Select() are aliases</note></remarks>

    STATIC METHOD Select(nNew AS DWORD,nOld OUT DWORD ) AS LOGIC
        LOCAL nTemp := 0 as DWORD
        var result := CoreDb.Do ({ =>
        VAR oSession := RuntimeState.DataSession
        nTemp := oSession:CurrentWorkareaNO
        IF nNew != nTemp
            IF nNew == 0
                nNew := oSession:FindEmptyArea(TRUE)
            ENDIF
            IF nNew > DataSession.MaxWorkareas
                RddError.PostArgumentError( __FUNCTION__, EDB_SELECT, nameof(nNew), 1, <OBJECT>{ nNew } )
            ELSE
                oSession:CurrentWorkareaNO :=  nNew
            ENDIF
        ENDIF
        RETURN TRUE
        })
        nOld := nTemp
        RETURN result

        /// <summary>
        /// Set a filter condition.
        /// </summary>
        /// <param name="oBlock">Codeblock that defines the filter. Please note that some RDDs (such as Advangate) will NOT use this compiled codeblock.</param>
        /// <param name="cFilter">String version of the filter. Some RDDs (such as Advantage) use this condition instead of the codeblock</param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBSetFilter() but strongly typed.
        /// <include file="CoreComments.xml" path="Comments/LastError/*" />
        /// </remarks>

    STATIC METHOD SetFilter(oBlock AS ICodeblock,cFilter AS STRING) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        VAR info 		 := DbFilterInfo{}
        info:FilterBlock := oBlock
        IF ! String.IsNullOrEmpty(cFilter)
            info:FilterText  := cFilter
        ENDIF
        BEFOREBULK
        VAR result := oRdd:SetFilter(info)
        AFTERBULK
        RETURN result
        })

        /// <summary>
        /// Set the found flag.
        /// </summary>
        /// <param name="lFound"></param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBSetFound().
        /// <note type="tip">VoDbSetFound() and CoreDb.SetFound() are aliases</note></remarks>


    STATIC METHOD SetFound(lFound AS LOGIC) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        oRdd:Found := lFound
        RETURN TRUE
        })
        /// <summary>
        /// Specify the code block for a locate condition.
        /// </summary>
        /// <param name="oBlock"></param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBSetLocate() but strongly typed.
        /// <include file="CoreComments.xml" path="Comments/LastError/*" />
        /// </remarks>

    STATIC METHOD SetLocate(oBlock AS ICodeblock) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        VAR scope := oRdd:GetScope()
        scope:ForBlock := oBlock
        oRdd:SetScope(scope)
        RETURN TRUE
        })

        /// <summary>
        /// Relate a specified work area to the current work area.
        /// </summary>
        /// <param name="cAlias">Alias of child area.</param>
        /// <param name="uCobKey">Relational key as codeblock.</param>
        /// <param name="cKey">Relational key as string.</param>
        /// <param name="cName">Name of the relation. Defaults to the parent alias + "_" + Child alias</param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBSetRelation() but strongly typed.
        /// <include file="CoreComments.xml" path="Comments/LastError/*" />
        /// </remarks>

    STATIC METHOD SetRelation(cAlias AS STRING,oKey  AS ICodeblock,cKey AS STRING, cName AS STRING) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        VAR oSession := RuntimeState.DataSession
        LOCAL nDest := oSession:FindAlias(cAlias) AS DWORD
        IF nDest == 0
            RddError.PostArgumentError(__FUNCTION__,EDB_SETRELATION, nameof(cAlias), 1, <OBJECT>{cAlias})
        ENDIF
        LOCAL oDest := oSession:GetRDD(nDest) AS IRdd
        IF oDest == NULL_OBJECT
            RddError.PostArgumentError(__FUNCTION__,EDB_SETRELATION, nameof(cAlias), 1, <OBJECT>{cAlias})
        ENDIF
        LOCAL oRelInfo AS DbRelInfo
        oRelInfo := DbRelInfo{}
        oRelInfo:Parent := oRdd
        oRelInfo:Child  := oDest
        oRelInfo:Key    := cKey
        oRelInfo:Block  := oKey
        IF String.IsNullOrEmpty(cName:Trim())
            cName := oRdd:Alias+"_"+oDest:Alias
        ENDIF
        oRelInfo:Name   := cName
        BEFOREMOVE
        VAR result :=  oRdd:SetRel(oRelInfo)
        AFTERMOVE
        RETURN result
        })


        /// <summary>Gets the locate condition.</summary>
        /// <returns>An object containing the scope for the current area.</returns>

    STATIC METHOD GetScope() AS DbScopeInfo
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        RETURN oRdd:GetScope()
        })

        /// <summary>Set the locate condition.</summary>
        /// <param name="scope">A Scope objhect describing the current scope.</param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks><note type="tip">VoDbSetScope() and CoreDb.SetScope() are aliases</note></remarks>

    STATIC METHOD SetScope(scope AS DbScopeInfo) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        RETURN oRdd:SetScope(scope)
        })
        /// <summary>
        /// Select a new work area.
        /// </summary>
        /// <param name="siNew">The number of the new work area. 0 indicates the first available free Workarea. -1 indicates the last available free Workarea.</param>
        /// <returns>The newly selected work area.</returns>
        /// <remarks>This function is like DBSetSelect() but strongly typed.
        /// <include file="CoreComments.xml" path="Comments/LastError/*" />
        /// <note type="tip">VoDbSetSelect() and CoreDb.SetSelect() are aliases</note></remarks>

    STATIC METHOD SetSelect(siNew AS INT) AS DWORD
        RETURN CoreDb.Do ({ =>
        VAR oSession := RuntimeState.DataSession
        IF siNew == -1
            siNew := (INT) oSession:FindEmptyArea(FALSE)
        ELSEIF siNew <= 0
            siNew := (INT) oSession:FindEmptyArea(TRUE)
        ENDIF
        IF siNew > DataSession.MaxWorkareas
            siNew := 0
        ELSEIF siNew == 0
            RddError.PostArgumentError( __FUNCTION__, EDB_SELECT, nameof(siNew), 1, <OBJECT>{siNew})
        ELSE
            RuntimeState.CurrentWorkarea := (DWORD) siNew
        ENDIF
        RETURN (DWORD) siNew
        })

        /// <summary>
        /// Move the record pointer relative to the current record.
        /// </summary>
        /// <param name="nRecords">The number of logical records to move, relative to the current record.  A positive value means to skip forward, and a negative value means to skip backward. </param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBSkip() but strongly typed.
        /// <include file="CoreComments.xml" path="Comments/LastError/*" />
        /// <note type="tip">DbSkip(), VoDbSkip() and CoreDb.Skip() are aliases</note></remarks>

    STATIC METHOD Skip(nRecords AS LONG) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        BEFOREMOVE
        VAR result := oRdd:Skip(nRecords)
        AFTERMOVE
        RETURN result
        })

        /// <summary>Position the cursor relative to its current position within the current scope.</summary>
        /// <param name="nRecords"></param>
        /// <param name="scope"></param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks><note type="tip">VoDbSkipScope() and CoreDb.SkipScope() are aliases</note></remarks>

    STATIC METHOD SkipScope(nRecords AS LONG,scope AS DbScopeInfo) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        oRdd:SetScope(scope)
        BEFOREMOVE
        VAR result := oRdd:Skip(nRecords)
        AFTERMOVE
        RETURN result
        })


        /// <summary>Copy records to a database file in sorted order.</summary>
        /// <param name="nDest">The work area number for the target Workarea.</param>
        /// <param name="fieldNames">The field names to write specified as an _FieldNames object.</param>
        /// <param name="uCobFor"><include file="VoFunctionDocs.xml" path="Runtimefunctions/cbfor/*" /></param>
        /// <param name="uCobWhile"><include file="VoFunctionDocs.xml" path="Runtimefunctions/cbwhile/*" /></param>
        /// <param name="nNext">The number of records to append, starting at the current record.</param>
        /// <param name="nRecno">The number of the record to append.</param>
        /// <param name="lRest">TRUE processes only records from the current record to end-of-file.  FALSE processes all records.</param>
        /// <param name="sortNames">The sort keys, specified as an _FieldNames object.</param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    STATIC METHOD Sort(nDest AS DWORD,fieldNames AS _FieldNames,uCobFor AS ICodeblock,uCobWhile AS ICodeblock, nNext AS OBJECT,nRecno AS OBJECT,lRest AS LOGIC,sortNames AS _FieldNames) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        LOCAL info AS DbSortInfo
        info := DbSortInfo{fieldNames:FieldCount, sortNames:FieldCount}
        CoreDb.TransSetInfo(oRdd, info:TransInfo, __FUNCTION__, nDest, fieldNames,uCobFor, uCobWhile, nNext, nRecno, lRest)
        // Now process the fieldnames in the sortnames list
        LOCAL nFld AS INT
        FOR nFld := 0 TO sortNames:FieldCount -1
            // Could be FieldName / ACDB to indicate the sort method
            VAR parts := sortNames:Fields[nFld]:Split('/')
            IF parts:Length > 1
                VAR part := parts[1]    // second element !
                IF part:IndexOf('A') > -1
                    info:Items[nFld]:Flags |= DbSortFlags.Default
                ENDIF
                IF part:IndexOf('C') > -1
                    info:Items[nFld]:Flags |= DbSortFlags.Case
                ENDIF
                IF part:IndexOf('D') > -1
                    info:Items[nFld]:Flags |= DbSortFlags.Descending
                ENDIF
                IF part:IndexOf('B') > -1
                    info:Items[nFld]:Flags |= DbSortFlags.Ascii
                ENDIF
            ENDIF
            LOCAL iField AS INT
            iField := oRdd:FieldIndex(parts[0])
            IF iField == 0
                RddError.PostArgumentError( __FUNCTION__, EDB_FIELDNAME, nameof(sortNames), 8, <OBJECT>{ sortNames:Fields[nFld] } )
            ENDIF
            info:Items[nFld]:FieldNo := iField
        NEXT
        BEFOREBULK
        VAR result := oRdd:Sort( info )
        AFTERBULK
        RETURN result
        })

        /// <summary>
        /// Select a new work area by specifying its alias as a string and return the number of the current work area.
        /// </summary>
        /// <param name="sAlias">The alias of the work area you want to select.</param>
        /// <returns>
        /// </returns>
    STATIC METHOD SymSelect(sAlias AS STRING) AS INT
        LOCAL ret AS DWORD
        IF String.IsNullOrEmpty(sAlias)
            ret := RuntimeState.CurrentWorkarea
        ELSE
            ret := RuntimeState.DataSession:FindAlias( sAlias )
            IF ret != 0
                CoreDb.SetSelect( (INT) ret )
            ENDIF
        ENDIF
        RETURN (INT) ret
        /// <summary>Copy one or more rows from one work area to another.</summary>
        /// <param name="nDest">The work area number for the target Workarea.</param>
        /// <param name="fldNames">The field names to write specified as an _FieldNames object.</param>
        /// <param name="uCobFor"><include file="VoFunctionDocs.xml" path="Runtimefunctions/cbfor/*" /></param>
        /// <param name="uCobWhile"><include file="VoFunctionDocs.xml" path="Runtimefunctions/cbwhile/*" /></param>
        /// <param name="sortNames">The sort keys, specified as an _FieldNames object.</param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    STATIC METHOD Trans(nDest AS DWORD,fldNames AS _FieldNames,uCobFor AS ICodeblock,uCobWhile AS ICodeblock, nNext AS OBJECT,nRecno AS OBJECT,lRest AS LOGIC) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        LOCAL info AS DbTransInfo
        info := DbTransInfo{fldNames:FieldCount}
        CoreDb.TransSetInfo(oRdd, info, __FUNCTION__, nDest, fldNames, uCobFor, uCobWhile, nNext, nRecno, lRest)
        BEFOREBULK
        VAR result := oRdd:Trans( info )
        AFTERBULK
        RETURN result

        })
        /// <summary>Copy a single row from one work area to another.</summary>
        /// <param name="nDest">The work area number for the target Workarea.</param>
        /// <param name="fldNames">The field names to write specified as an _FieldNames object.</param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks><note type="tip">VoDbTransRec() and CoreDb.TransRec() are aliases</note></remarks>

    STATIC METHOD TransRec(nDest AS DWORD,fldNames AS _FieldNames) AS LOGIC
        RETURN CoreDb.Do ({ =>
            LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
            LOCAL dbti := DbTransInfo{ fldNames:FieldCount} AS DbTransInfo
            LOCAL oDest := RuntimeState.DataSession.GetRDD(nDest) AS IRdd
            IF oDest == NULL_OBJECT
                RddError.PostNoTableError(__FUNCTION__)
            ENDIF
            dbti:Destination := oDest
            dbti:ItemCount := fldNames:FieldCount
            IF CoreDb.BuildTrans( dbti, fldNames, oRdd, oDest )
                dbti:Flags |= DbTransInfoFlags.SameStructure
                LOCAL oCanPutRec AS OBJECT
                oCanPutRec := oRdd:Info( DbInfo.DBI_CANPUTREC, NULL )
                IF oCanPutRec != NULL .AND. (LOGIC) oCanPutRec
                    oCanPutRec := oDest:Info(DbInfo.DBI_CANPUTREC, NULL )
                    IF oCanPutRec != NULL .AND. (LOGIC) oCanPutRec
                        dbti:Flags |= DbTransInfoFlags.CanPutRec
                    ENDIF
                ENDIF
            ENDIF
            RETURN oRdd:TransRec( dbti )
        })

        /// <summary>
        /// Release all locks for a work area.
        /// </summary>
        /// <param name="uRecno">The ID of the record to be unlocked.  To omit, specify NULL_OBJECT.
        /// This unlocks all locked records or the whole file. </param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBUnlock().
        /// <include file="CoreComments.xml" path="Comments/LastError/*" />
        /// </remarks>

    STATIC METHOD Unlock(uRecno AS OBJECT) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        VAR result := oRdd:UnLock(uRecno)
        RAISE RecordUnLocked IIF(uRecno == NULL, oRdd:RecNo, uRecno)
        RETURN result
        })


        /// <summary>Release all locks for all work areas.</summary>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBUnlockAll().
        /// <include file="CoreComments.xml" path="Comments/LastError/*" />
        /// <note type="tip">VoDbUnlockAll() and CoreDb.UnlockAll() are aliases</note></remarks>
    STATIC METHOD UnlockAll() AS LOGIC
        RETURN CoreDb.Do ({ =>
        VAR oSession := RuntimeState.DataSession
        VAR oRdd := oSession:CurrentWorkarea
        BEFOREBULK
            oSession:UnLockAll()
        AFTERBULK
        RETURN TRUE
        })

        /// <overloads>
        /// <summary>
        /// Open a file
        /// </summary>
        /// <seealso cref="O:XSharp.RT.Functions.VoDbUseArea" />
        /// <seealso cref="M:XSharp.RT.Functions.DbUseArea(XSharp.__Usual,XSharp.__Usual,XSharp.__Usual,XSharp.__Usual,XSharp.__Usual,XSharp.__Usual,XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)" />
        /// </overloads>
        /// <summary>
        /// Open a database file.
        /// </summary>
        /// <param name="lNew">Open RDD in new Workarea ?</param>
        /// <param name="rddList">RDDList structure that describes the RDD to use</param>
        /// <param name="cName">Name of the (dbf) file to open</param>
        /// <param name="cAlias">Alias to use for the Workarea</param>
        /// <param name="lShare">Should the file be opened shared ?</param>
        /// <param name="lReadOnly">Should the file be opened readonly ?</param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBUseArea() but strongly typed.
        /// <include file="CoreComments.xml" path="Comments/LastError/*" />
        /// <note type="tip">VoDbUseArea() and CoreDb.UseArea() are aliases</note></remarks>

    STATIC METHOD UseArea(lNew AS LOGIC,rddList AS _RddList,cName AS STRING,cAlias AS STRING,lShare AS LOGIC,lReadOnly AS LOGIC) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := NULL  AS RegisteredRDD
        FOREACH VAR name IN rddList:atomRddName
            oRdd := RegisteredRDD.Find(name)
            oRdd:Load()
        NEXT
        IF oRdd != NULL_OBJECT
            RETURN CoreDb.UseArea(lNew, oRdd:RddType, cName, cAlias, lShare, lReadOnly)
        ENDIF
        RETURN FALSE
        })
        /// <inheritdoc cref='UseArea(System.Boolean,XSharp._RddList,System.String,System.String,System.Boolean,System.Boolean)' />
        /// <param name="rddName">Name of the RDD to use.</param>


    STATIC METHOD UseArea(lNew AS LOGIC,rddName AS STRING,cName AS STRING,cAlias AS STRING,lShare AS LOGIC,lReadOnly AS LOGIC) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL rddType AS Type
        IF ( rddType := CoreDb.RddNameToType( rddName ) ) == NULL
            RddError.PostArgumentError( __FUNCTION__, EDB_RDDNOTFOUND, nameof(cName), 3, <OBJECT>{ rddName } )
        ELSE
            RETURN CoreDb.UseArea( lNew, rddType, cName, cAlias, lShare, lReadOnly )
        ENDIF
        RETURN FALSE
        })


        /// <inheritdoc cref='UseArea(System.Boolean,XSharp._RddList,System.String,System.String,System.Boolean,System.Boolean)' />
        /// <param name="rddType">Type of the RDD to use.</param>

    STATIC METHOD UseArea(lNew AS LOGIC,rddType AS System.Type,cName AS STRING,cAlias AS STRING,lShare AS LOGIC,lReadOnly AS LOGIC) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL ret   := FALSE AS LOGIC
        LOCAL area  := 0    AS DWORD
        VAR oSession := RuntimeState.DataSession
        IF String.IsNullOrEmpty( cName )
            RddError.PostArgumentError( __FUNCTION__, EDB_USE, nameof(cName), 3 , <OBJECT>{NULL})
        ELSE
            ret := TRUE
            NetErr( FALSE )
            cName := cName:Trim() // :ToUpperInvariant()

            IF String.IsNullOrEmpty( cAlias )
                TRY
                    cAlias := Path.GetFileNameWithoutExtension( cName )
                CATCH e  AS ArgumentException
                    Fail(e)
                    RddError.PostArgumentError( __FUNCTION__, EDB_USE, nameof(cName), 3, <OBJECT>{cName} )
                    ret := FALSE
                END TRY
            ENDIF
            cAlias := cAlias:ToUpperInvariant()
            IF lNew
                area := oSession:FindEmptyArea(TRUE)
                IF area > DataSession.MaxWorkareas  .OR. area == 0
                    ret := FALSE
                ELSE
                    oSession:CurrentWorkareaNO := area
                ENDIF
            ELSE
                area := oSession:CurrentWorkareaNO
            ENDIF
            IF ret
                oSession:CloseArea(area)
                LOCAL oRdd := CoreDb.CreateRDDInstance( rddType, cAlias ) AS IRdd

                IF oRdd == NULL
                    RddError.PostArgumentError( __FUNCTION__, EDB_DRIVERLOAD, nameof(rddType), 3, <OBJECT>{ rddType } )
                    ret := FALSE
                ELSEIF ! CoreDb.IsAliasUnused( cAlias )
                    RddError.PostArgumentError( __FUNCTION__, EDB_DUPALIAS, nameof(cAlias), 4, <OBJECT>{ cAlias } )
                    ret := FALSE
                ELSE
                    LOCAL dboi := DbOpenInfo{} AS DbOpenInfo
                    LOCAL uiArea AS DWORD
                    uiArea           := oSession:CurrentWorkareaNO
                    var path         := Path.GetDirectoryName(cName)
                    dboi:FullName    := Path.Combine(path, System.IO.Path.GetFileName(cName))
                    dboi:Shared      := lShare
                    dboi:ReadOnly    := lReadOnly
                    dboi:Alias       := cAlias
                    dboi:Workarea    := uiArea
                    oRdd:Alias        := cAlias
                    ret := oSession:SetArea(uiArea, oRdd)
                    IF (ret)
                        TRY
                            RuntimeState.LastRddError := NULL
                            ret := oRdd:Open( dboi )
                            RAISE FileOpen oRdd:Info(DbInfo.DBI_FULLPATH,NULL)

                        CATCH e AS Exception
                            Fail(e)
                            ret := FALSE
                        END TRY
                    ENDIF
                    IF ! ret
                        oSession:CloseArea(uiArea)
                    ENDIF
                    oSession:CurrentWorkareaNO := uiArea
                ENDIF
            ENDIF
        ENDIF
        RETURN ret
        })


        /// <summary>Remove all records from the current Workarea.</summary>
        /// <returns>TRUE if successful; otherwise, FALSE./// </returns>
        /// <remarks>This function is like DBZap().
        /// <include file="CoreComments.xml" path="Comments/LastError/*" />
        /// <note type="tip">VoDbZap() and CoreDb.Zap() are aliases</note></remarks>

    STATIC METHOD Zap() AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        BEFOREBULK
            VAR result := oRdd:Zap()
        AFTERBULK
        RETURN result
        })


        /// <summary>Return exception object from last RDD operation.</summary>
    STATIC METHOD  _ErrInfoPtr AS Exception
        IF RuntimeState.LastRddError == NULL
            RuntimeState.LastRddError := Exception {"No RDD Exception found in the runtime state"}
        ENDIF
        RETURN RuntimeState.LastRddError

END CLASS
