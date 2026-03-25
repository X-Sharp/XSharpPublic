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

/// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb/*" />
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
        LOCAL oDest := RuntimeState.DataSession:GetRDD(nDest) AS IRdd
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
                info:Scope:NextCount := Convert.ToUInt32(nNext)
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

     /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.Notify/*" />
     PUBLIC STATIC EVENT Notify AS DbNotifyEventHandler

     PRIVATE STATIC PROPERTY HasEvents AS LOGIC GET Notify != NULL

     PRIVATE STATIC METHOD RaiseEvent(oRdd AS IRdd, nEvent AS DbNotificationType, oData AS OBJECT) AS VOID
        IF Notify != NULL
             Notify ( oRdd, DbNotifyEventArgs{ nEvent, oData})
        ENDIF
        RETURN
    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.Alias/*" />
    STATIC METHOD Alias(nArea AS DWORD) AS STRING
        RETURN CoreDb.Do ({ =>
        RETURN RuntimeState.DataSession:GetAlias(nArea)
        })

    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.Append/*" />
    STATIC METHOD Append(lReleaseLocks AS LOGIC) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        VAR result := oRdd:Append(lReleaseLocks)
        RAISE RecordAppended oRdd:RecNo
        RETURN result

        })

    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.BlobInfo/*" />
    STATIC METHOD BlobInfo(nOrdinal AS DWORD,nPos AS DWORD,oRet AS OBJECT) AS LOGIC
        RETURN CoreDb.Do ({ =>
        RETURN CoreDb.BlobInfo(nOrdinal, nPos, REF oRet)
        })

    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.BlobInfo/*" />
    STATIC METHOD BlobInfo(nOrdinal AS DWORD,nPos AS DWORD,oRet REF OBJECT) AS LOGIC
        var oTemp := oRet
        var result := CoreDb.Do ({ =>
            RETURN CoreDb.BlobInfo(nOrdinal, nPos, REF oTemp)
        })
        oRet := oTemp
        return result

    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.Bof/*" />
    STATIC METHOD Bof() AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        RETURN oRdd:BoF
        })

    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.BuffRefresh/*" />
    STATIC METHOD BuffRefresh() AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        oRdd:RecInfo(DbRecordInfo.DBRI_UPDATED,0, NULL)
        RETURN TRUE
        })

    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.ClearFilter/*" />
    STATIC METHOD ClearFilter() AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        BEFOREBULK
            VAR result := oRdd:ClearFilter()
        AFTERBULK
        RETURN result
        })

    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.ClearLocate/*" />
    STATIC METHOD ClearLocate() AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        RETURN oRdd:ClearScope()
        })


    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.ClearRelation/*" />
    STATIC METHOD ClearRelation() AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        BEFOREBULK
            VAR result := oRdd:ClearRel()
        AFTERBULK
        RETURN result
        })

    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.ClearScope/*" />
    STATIC METHOD ClearScope() AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        RETURN oRdd:ClearScope()
        })
    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.CloseAll/*" />
    STATIC METHOD CloseAll() AS LOGIC
        RETURN CoreDb.Do ({ =>
        var oSession := RuntimeState.DataSession
        VAR oRdd := oSession:CurrentWorkarea  // needed for BEFOREBULK and AFTERBULK
        BEFOREBULK
            VAR result := oSession:CloseAll()
        AFTERBULK
        RETURN result
        })
    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.CloseArea/*" />
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

    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.Refresh/*" />
    STATIC METHOD Refresh() AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        RETURN oRdd:Refresh()
        })


    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.Commit/*" />
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


    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.CommitAll/*" />
    STATIC METHOD CommitAll() AS LOGIC
        RETURN CoreDb.Do ({ =>
        VAR oSession := RuntimeState.DataSession
        VAR oRdd := oSession:CurrentWorkarea
        BEFOREBULK
            VAR result := oSession:CommitAll()
        AFTERBULK
        RETURN result
        })

    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.Continue/*" />
    STATIC METHOD Continue() AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        RETURN oRdd:Continue()
        })


    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.Create/*" />
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

    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.Create/*" />
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


    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.Create/*" />
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
        ELSEIF lNew && ! ( ret := CoreDb.Select( 0, OUT uiOldArea ) )
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

        /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.Dbf/*" />
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

    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.Delete/*" />
    STATIC METHOD Delete() AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        RAISE BeforeRecordDeleted  oRdd:RecNo
        VAR result := oRdd:Delete()
        RAISE AfterRecordDeleted  oRdd:RecNo
        RETURN result

        })


    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.Deleted/*" />
    STATIC METHOD Deleted() AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        RETURN oRdd:Deleted
        })

    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.Eof/*" />
    STATIC METHOD Eof() AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        RETURN oRdd:EoF
        })


    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.Eval/*" />
    STATIC METHOD Eval(uBlock AS ICodeblock,uCobFor AS ICodeblock,uCobWhile AS ICodeblock,uNext AS OBJECT,nRecno AS OBJECT,lRest AS LOGIC) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL nNext AS DWORD
        IF uBlock == NULL
            THROW Error.ArgumentError(__FUNCTION__, nameof(uBlock),1, <OBJECT>{uBlock})
        ELSE
            TRY
                IF uNext != NULL
                    nNext := Convert.ToUInt32(uNext)
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


    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.FCount/*" />
    STATIC METHOD FCount() AS DWORD
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__,FALSE) AS IRdd
        IF (oRdd != NULL)
            RETURN (DWORD) oRdd:FieldCount
        ENDIF
        RETURN 0

    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.FieldGet/*" />
    STATIC METHOD FieldGet(nPos AS DWORD,oRet REF OBJECT) AS LOGIC
        var oTemp := oRet
        var result := CoreDb.Do( { =>
            LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
            oTemp := oRdd:GetValue((INT) nPos)
            return TRUE
        })
        oRet := oTemp
        return result

    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.FieldGetBytes/*" />
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


    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.FieldPutBytes/*" />
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

    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.FieldInfo/*" />
    STATIC METHOD FieldInfo(nOrdinal AS DWORD,nFldPos AS DWORD,oValue AS OBJECT) AS LOGIC
        RETURN CoreDb.Do ({ =>
        RETURN CoreDb.FieldInfo(nOrdinal, nFldPos, REF oValue)
        })

    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.FieldInfo/*" />
    STATIC METHOD FieldInfo(nOrdinal AS DWORD,nFldPos AS DWORD,oRet REF OBJECT) AS LOGIC
        VAR oTemp := oRet
        VAR lResult :=  CoreDb.Do( { =>
            LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
            oTemp := oRdd:FieldInfo((INT) nFldPos, (INT) nOrdinal, oTemp)
            RETURN TRUE
        })
        oRet := oTemp
        RETURN lResult


    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.FieldName/*" />
    STATIC METHOD FieldName(nPos AS DWORD) AS STRING
        LOCAL oRdd := CoreDb.CWA("FieldName") AS IRdd
        IF (oRdd != NULL)
            RETURN oRdd:FieldName((INT) nPos)
        ENDIF
        RETURN String.Empty
    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.FieldPos/*" />
    STATIC METHOD FieldPos(sFieldName AS STRING) AS DWORD
        LOCAL oRdd := CoreDb.CWA("FieldPos",FALSE) AS IRdd
        IF (oRdd != NULL)
            RETURN (DWORD) oRdd:FieldIndex(sFieldName)
        ENDIF
        RETURN 0

    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.FieldPut/*" />
    STATIC METHOD FieldPut(nPos AS DWORD,xValue AS OBJECT) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        RAISE BeforeFieldUpdate oRdd:FieldName((INT) nPos)
        VAR result := oRdd:PutValue((INT) nPos, xValue)
        RAISE AfterFieldUpdate oRdd:FieldName((INT) nPos)
        RETURN result
        })

    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.FileGet/*" />
    STATIC METHOD FileGet(nPos AS DWORD,cFile AS STRING) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        RETURN oRdd:GetValueFile((INT) nPos, cFile)
        })

    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.FilePut/*" />
    STATIC METHOD FilePut(nPos AS DWORD,cFile AS STRING) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        RAISE BeforeFieldUpdate oRdd:FieldName((INT) nPos)
        VAR result :=  oRdd:PutValueFile((INT) nPos, cFile)
        RAISE AfterFieldUpdate oRdd:FieldName((INT) nPos)
        RETURN result
        })


    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.Filter/*" />
    STATIC METHOD Filter() AS STRING
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        RETURN oRdd:FilterText
        })

    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.Flock/*" />
    STATIC METHOD Flock() AS LOGIC
        RETURN CoreDb.Do ({ =>
        VAR oRdd := CoreDb.CWA(__FUNCTION__)
        LOCAL dbli AS DbLockInfo
        dbli := DbLockInfo{}
        dbli:Result := FALSE
        dbli:Method := DbLockInfo.LockMethod.File
        RETURN oRdd:Lock(REF dbli)
        })


    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.Found/*" />
    STATIC METHOD Found() AS LOGIC
        RETURN CoreDb.Do ({ =>
        VAR oRdd := CoreDb.CWA(__FUNCTION__)
        RETURN oRdd:Found
        })



    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.GetSelect/*" />
    STATIC METHOD GetSelect() AS DWORD
        RETURN RuntimeState.DataSession:CurrentWorkareaNO


    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.GoBottom/*" />
    STATIC METHOD GoBottom() AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        BEFOREMOVE
        VAR result := oRdd:GoBottom()
        AFTERMOVE
        RETURN result
        })
    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.GoToId/*" />
    STATIC METHOD GoToId(uRecId AS OBJECT) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        BEFOREMOVE
        VAR result := oRdd:GoToId(uRecId)
        AFTERMOVE
        RETURN result
        })

   /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.GoTo/*" />
   STATIC METHOD GoTo(nRecord AS DWORD) AS LOGIC
       RETURN CoreDb.Do ({ =>
       LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
       BEFOREMOVE
       VAR result := oRdd:GoTo(nRecord)
       AFTERMOVE
       RETURN result
       })



    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.GoTop/*" />
    STATIC METHOD GoTop() AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        BEFOREMOVE
        VAR result := oRdd:GoTop()
        AFTERMOVE
        RETURN result
        })


    STATIC METHOD Header() AS LONG
        LOCAL oValue := NULL AS OBJECT
        IF CoreDb.Info(DBI_GETHEADERSIZE, REF oValue)
            RETURN (LONG) oValue
        ENDIF
        RETURN 0


    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.Info/*" />
    STATIC METHOD Info(nOrdinal AS DWORD,oValue AS OBJECT) AS LOGIC
        RETURN CoreDb.Do ({ =>
        RETURN CoreDb.Info(nOrdinal, REF oValue)
        })



    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.Info/*" />
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

    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.JoinAppend/*" />
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


    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.LastRec/*" />
    STATIC METHOD LastRec() AS DWORD
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        RETURN oRdd:RecCount
        })

    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.Locate/*" />
    STATIC METHOD Locate(uCobFor AS ICodeblock,uCobWhile AS ICodeblock,nNext AS DWORD,uRecId AS OBJECT,lRest AS LOGIC) AS LOGIC
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


    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.MemoExt/*" />
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
    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.OrdBagExt/*" />
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

    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.OrdCondSet/*" />
    STATIC METHOD OrdCondSet(ordCondInfo AS DbOrderCondInfo) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        RETURN oRdd:OrderCondition(ordCondInfo)
        })


    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.OrdCreate/*" />
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


    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.OrdDestroy/*" />
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

        /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.OrderInfo/*" />
        STATIC METHOD OrderInfo(nOrdinal AS DWORD,cBagName AS STRING,oOrder AS OBJECT,oValue AS OBJECT) AS LOGIC
        RETURN CoreDb.Do ({ =>
        RETURN CoreDb.OrderInfo(nOrdinal, cBagName, oOrder, REF oValue)
        })

    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.OrderInfo/*" />
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


    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.OrdListAdd/*" />
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



    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.OrdListClear/*" />
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


    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.OrdListRebuild/*" />
    STATIC METHOD OrdListRebuild() AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        BEFOREBULK
        VAR result := oRdd:OrderListRebuild()
        AFTERBULK
        RETURN result
        })

    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.OrdSetFocus/*" />
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

    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.OrdSetFocus_2/*" />
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
            VAR isOk := TRUE
            cBagName     := cBagName?:Trim()
            info:BagName := cBagName
            info:Order   := oOrder
            if (! info:IsEmpty)
                isOk := oRdd:OrderListFocus(info)
                IF HasEvents .AND. ! info:IsEmpty
                    RAISE OrderChanged info:Result
                ENDIF
            ENDIF
            RETURN isOk
        })
        strPreviousOrder := strTemp
        return lResult


    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.Pack/*" />
    STATIC METHOD Pack() AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        BEFOREBULK
        VAR result := oRdd:Pack()
        AFTERBULK
        RETURN result
        })



    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.RddCount/*" />
    STATIC METHOD RddCount() AS DWORD
        RETURN CoreDb.Do ({ =>
        RETURN (DWORD) CoreDb.RddList():Length
        })
    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.RddInfo/*" />
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


    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.RddList/*" />
    STATIC METHOD RddList() AS STRING[]
        RETURN CoreDb.Do ({ =>
        LOCAL aList AS List<STRING>
        aList := List<STRING>{}
        LOCAL i AS DWORD
        var oSession := RuntimeState.DataSession
        FOR i := 1 TO DataSession.MaxWorkareas
            VAR oRdd := oSession:GetRDD(i)
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



    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.RddName/*" />
    STATIC METHOD RddName() AS STRING
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        RETURN oRdd:Driver
        })

    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.RddSetDefault/*" />
    STATIC METHOD RddSetDefault(cNewRDD AS STRING) AS STRING
        LOCAL cOldRDD AS STRING
        cOldRDD := RuntimeState.DefaultRDD
        IF ! String.IsNullOrEmpty(cNewRDD)
            RuntimeState.DefaultRDD := cNewRDD
        ENDIF
        RETURN cOldRDD



    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.Recall/*" />
    STATIC METHOD Recall() AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        RAISE BeforeRecordRecalled  oRdd:RecNo
        VAR result := oRdd:Recall()
        RAISE AfterRecordRecalled  oRdd:RecNo
        RETURN result
        })


        /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.RecSize/*" />
        STATIC METHOD RecSize AS LONG
            LOCAL nSize := NULL AS OBJECT
            IF CoreDb.Info(DbInfo.DBI_GETRECSIZE, REF nSize)
                RETURN (LONG) nSize
            ENDIF
            RETURN 0


    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.Recno/*" />
    STATIC METHOD Recno() AS DWORD
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        RETURN (DWORD) oRdd:RecNo
        })


    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.RecordGet/*" />
    STATIC METHOD RecordGet() AS BYTE[]
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        RETURN oRdd:GetRec()
        })


    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.RecordInfo/*" />
    STATIC METHOD RecordInfo(nOrdinal AS DWORD,oRecID AS OBJECT,oValue AS OBJECT) AS LOGIC
        RETURN CoreDb.Do ({ =>
        RETURN CoreDb.RecordInfo(nOrdinal, oRecID, REF oValue)
        })

    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.RecordInfo/*" />
    STATIC METHOD RecordInfo(nOrdinal AS DWORD,oRecID AS OBJECT,oValue REF OBJECT) AS LOGIC
        VAR oTemp := oValue
        VAR result := CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        oTemp := oRdd:RecInfo( (INT) nOrdinal,oRecID, oTemp )
        RETURN TRUE
        })
        oValue := oTemp
        RETURN result


    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.RecordPut/*" />
    STATIC METHOD RecordPut(aRecord AS BYTE[]) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        RETURN oRdd:PutRec(aRecord)
        })


    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.Relation/*" />
    STATIC METHOD Relation(nPos AS DWORD,sRel REF STRING) AS LOGIC
        VAR sTemp := sRel
        VAR result := CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        sTemp :=  oRdd:RelText(nPos)
        RETURN TRUE
        })
        sRel := sTemp
        RETURN result


    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.RLock/*" />
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
        VAR result := oRdd:Lock(REF lockInfo)
        RAISE RecordLocked IIF(uRecId == NULL, oRdd:RecNo, uRecId)
        RETURN result
        })


    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.RSelect/*" />
    STATIC METHOD RSelect(nPos AS DWORD) AS DWORD
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        RETURN oRdd:RelArea(nPos)
        })

    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.Seek/*" />
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



    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.Select/*" />
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


    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.SetFilter/*" />
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



    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.SetFound/*" />
    STATIC METHOD SetFound(lFound AS LOGIC) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        oRdd:Found := lFound
        RETURN TRUE
        })

    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.SetLocate/*" />
    STATIC METHOD SetLocate(oBlock AS ICodeblock) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        VAR scope := oRdd:GetScope()
        scope:ForBlock := oBlock
        oRdd:SetScope(scope)
        RETURN TRUE
        })


    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.SetRelation/*" />
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



    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.GetScope/*" />
    STATIC METHOD GetScope() AS DbScopeInfo
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        RETURN oRdd:GetScope()
        })


    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.SetScope/*" />
    STATIC METHOD SetScope(scope AS DbScopeInfo) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        RETURN oRdd:SetScope(scope)
        })

    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.SetSelect/*" />
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


    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.Skip/*" />
    STATIC METHOD Skip(nRecords AS LONG) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        BEFOREMOVE
        VAR result := oRdd:Skip(nRecords)
        AFTERMOVE
        RETURN result
        })


    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.SkipScope/*" />
    STATIC METHOD SkipScope(nRecords AS LONG,scope AS DbScopeInfo) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        oRdd:SetScope(scope)
        BEFOREMOVE
        VAR result := oRdd:Skip(nRecords)
        AFTERMOVE
        RETURN result
        })


    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.Sort/*" />
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

    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.SymSelect/*" />
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
    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.Trans/*" />
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

    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.TransRec/*" />
    STATIC METHOD TransRec(nDest AS DWORD,fldNames AS _FieldNames) AS LOGIC
        RETURN CoreDb.Do ({ =>
            LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
            LOCAL dbti := DbTransInfo{ fldNames:FieldCount} AS DbTransInfo
            LOCAL oDest := RuntimeState.DataSession:GetRDD(nDest) AS IRdd
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


    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.Unlock/*" />
    STATIC METHOD Unlock(uRecno AS OBJECT) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        VAR result := oRdd:UnLock(uRecno)
        RAISE RecordUnLocked IIF(uRecno == NULL, oRdd:RecNo, uRecno)
        RETURN result
        })


    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.UnlockAll/*" />
    STATIC METHOD UnlockAll() AS LOGIC
        RETURN CoreDb.Do ({ =>
        VAR oSession := RuntimeState.DataSession
        VAR oRdd := oSession:CurrentWorkarea
        BEFOREBULK
            oSession:UnLockAll()
        AFTERBULK
        RETURN TRUE
        })


    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.UseArea/*" />
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


    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.UseArea_2/*" />
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



    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.UseArea_3/*" />
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



    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb.Zap/*" />
    STATIC METHOD Zap() AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRdd := CoreDb.CWA(__FUNCTION__) AS IRdd
        BEFOREBULK
            VAR result := oRdd:Zap()
        AFTERBULK
        RETURN result
        })


    /// <include file="XSharp.Core.Docs.xml" path="doc/CoreDb._ErrInfoPtr/*" />
    STATIC METHOD  _ErrInfoPtr AS Exception
        IF RuntimeState.LastRddError == NULL
            RuntimeState.LastRddError := Exception {"No RDD Exception found in the runtime state"}
        ENDIF
        RETURN RuntimeState.LastRddError

END CLASS
