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


INTERNAL STATIC CLASS VoDb
    INTERNAL STATIC METHOD  RddNameToType( cRDDName AS STRING ) AS Type
        LOCAL ret := NULL AS  Type
        LOCAL oRdd        AS RegisteredRDD
        oRdd := RegisteredRDD.Find(cRddName)
        IF oRDD != NULL   
            oRdd:Load()
            ret := oRdd:RddType
        ENDIF
        IF ret == NULL       
            LOCAL loadedAssemblies := AppDomain.CurrentDomain:GetAssemblies() AS Assembly[]
            LOCAL x AS INT
            
            FOR x := 1 UPTO loadedAssemblies:Length
                ret := loadedAssemblies[x]:GetType( cRDDName, FALSE, TRUE )
                IF ret != NULL
                    EXIT
                ENDIF   
            NEXT
        ENDIF
        RETURN ret
    INTERNAL STATIC METHOD BuildTrans( dbti AS DbTransInfo, lpfn AS _FieldNames, src AS IRDD, dst AS IRDD ) AS LOGIC
        LOCAL oTmp          AS OBJECT
        LOCAL srcFieldCount AS DWORD
        LOCAL dstFieldCount AS DWORD
        LOCAL fMatch        AS LOGIC
        LOCAL i             AS LONG
        LOCAL j             AS LONG
        LOCAL uiSrc         AS LONG
        LOCAL uiDst         AS LONG
        
        oTmp := src:Info( DBInfo.DBI_FCOUNT, NULL)
        srcFieldCount := Convert.ToUInt32( oTmp )
        oTmp := dst:Info( DBInfo.DBI_FCOUNT, NULL )
        dstFieldCount := Convert.ToUInt32( oTmp )
        
        fMatch := (DWORD) dbti:ItemCount == srcFieldCount && srcFieldCount == dstFieldCount
        j := 1
        FOR i := 0 UPTO lpfn:fieldCount -1
        
            uiSrc := src:FieldIndex( lpfn:fields[i] )
            uiDst := dst:FieldIndex( lpfn:fields[i]  )
            
            IF uiSrc == 0 || uiDst == 0
                fMatch := FALSE
            ELSE
                // Note that lpTransItems wants the Field Positions to start with 0
                dbti:Items[j]:Source       := uiSrc - 1
                dbti:Items[j]:Destination  := uiDst - 1
                j++
                
                IF fMatch
                    // todo
                    
                    LOCAL srcFld AS RddFieldInfo
                    LOCAL dstFld AS RddFieldInfo
                    srcFld := src:GetField(uiSrc)
                    dstFld := dst:GetField(uiDst)
                    fMatch := uiSrc == uiDst && srcFld:SameType( dstFld )
                ENDIF
            ENDIF 
        NEXT
        
        dbti:ItemCount := j -1
        
        RETURN fMatch
    INTERNAL STATIC METHOD TransSetInfo(oRdd AS IRDD, info AS DbTransInfo, cFunc AS STRING,nDest AS DWORD, fldNames AS _FieldNames,;
    uCobFor AS ICodeBlock ,uCobWhile AS ICodeBlock ,;
    nNext AS OBJECT,nRecno AS OBJECT,lRest AS LOGIC) AS VOID
        LOCAL oDest := RUntimeState.Workareas.GetRDD(nDest) AS IRDD
        IF oDest == NULL
            RddError.PostNoTableError(cFunc)
        ENDIF
        info:Destination := oDest
        IF !VoDb.BuildTrans( info, fldNames, oRDD, oDest )
            RddError.PostArgumentError( cFunc, EDB_DBSTRUCT, nameof(fldNames), 2, <OBJECT>{fldNames} )
        ENDIF
        info:Flags := DbTransInfo.Match
        LOCAL oCanPutRec AS OBJECT
        oCanPutRec := oRdd:Info(DBInfo.DBI_CANPUTREC, NULL)
        IF oCanPutRec IS LOGIC .AND. (LOGIC) oCanPutRec
            info:Flags |= DbTransInfo.PutRec
        ENDIF
        info:Scope:ForBlock := uCobFor
        info:Scope:WhileBlock := uCobWhile
        IF nNext != NULL
            TRY
                info:Scope:NextCount := Convert.ToInt32(nNext)
            CATCH AS Exception
                info:Scope:NextCount := 0
            END TRY
        ELSE
            info:Scope:NextCount := 0
        ENDIF
        info:Scope:RecId := nRecno
        info:Scope:Rest  := lRest     
        RETURN
    INTERNAL STATIC METHOD AliasFromFilename( cFilename AS STRING, cAlias REF STRING ) AS LOGIC
        LOCAL ret AS LOGIC
        TRY
            cAlias := Path.GetFileNameWithoutExtension( cFilename )
            ret    := ! String.IsNullOrEmpty( cAlias )
        CATCH AS ArgumentException
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
            cAlias := sb:ToString()
        ENDIF
        RETURN ret   
        
        
        // Check if Alias is used for current thread
    INTERNAL STATIC METHOD IsAliasUnused( cAlias AS STRING ) AS LOGIC
        RETURN RuntimeState.Workareas:FindAlias(cAlias) == 0
        
        // Create RDD Object from RDD Type
    INTERNAL STATIC METHOD CreateRDDInstance( rddType AS Type , cAlias AS STRING) AS IRDD
        LOCAL ret    AS IRDD
        TRY
            ret := (IRDD) rddType:InvokeMember( NULL, BindingFlags.DeclaredOnly | BindingFlags.Public | BindingFlags.Instance | BindingFlags.CreateInstance, NULL, NULL, <OBJECT>{} )
            ret:Alias := cAlias:ToUpperInvariant()
        CATCH
            ret := NULL
        END TRY
        RETURN ret 
        
    INTERNAL STATIC METHOD Do<T>(action AS @@func<t>) AS T
        TRY
            RETURN action()
        CATCH e AS RddError
            RuntimeState.LastRDDError := e
        END TRY
        RETURN DEFAULT(T)
        
END CLASS    
        
        
    
    /// <summary>
    /// Return the alias of a specified work area as a string.
    /// </summary>
    /// <param name="nArea"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBAlias(nArea AS DWORD) AS STRING
    RETURN VoDb.Do ({ =>
    RETURN RuntimeState.Workareas:GetAlias(nArea)
    })
    
    /// <summary>
    /// Add a new record.
    /// </summary>
    /// <param name="lReleaseLocks"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    /// <remarks>VODBAppend() is like DBAppend() but is strongly typed.  
    /// <span id='LastError' >
    /// <br/>This function, however, does not call the error
    /// handler and will therefore not produce a runtime error message or create an error object if it fails.<br/>
    /// Thus, it may be important to check the return value to determine if the function succeeded.<br/>
    /// The <see cref='P:XSharp.RuntimeState.LastRddError'>LastRddError property in the runtimestate</see>  will contain needed information
    /// regarding any error that occurs.</span>
    /// </remarks>
FUNCTION VODBAppend(lReleaseLocks AS LOGIC) AS LOGIC
    RETURN VoDb.Do ({ =>
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    RETURN oRDD:Append(lReleaseLocks)
    })

    /// <summary>
    /// </summary>
    /// <param name="nOrdinal"></param>
    /// <param name="nPos"></param>
    /// <param name="oRet"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION _VODBBlobInfo(nOrdinal AS DWORD,nPos AS DWORD,oRet AS OBJECT) AS LOGIC
    RETURN VoDb.Do ({ =>
        RETURN _VODbBlobInfo(nOrdinal, nPos, REF oRet)
    })

    /// <summary>
    /// </summary>
    /// <param name="nOrdinal"></param>
    /// <param name="nPos"></param>
    /// <param name="oRet"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION _VODBBlobInfo(nOrdinal AS DWORD,nPos AS DWORD,oRet REF OBJECT) AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
        oRet := oRDD:BlobInfo(nOrdinal, nPos)
        RETURN TRUE
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    RETURN FALSE   
    /// <summary>
    /// Determine when beginning-of-file is encountered.
    /// </summary>
    /// <returns>TRUE after an attempt to skip backward beyond the first logical record in a database file or if
    /// the current database file contains no records; otherwise, FALSE.  If there is no database file open in the
    /// current work area, VODBBOF() returns TRUE.</returns>
    /// <remarks>VODBBOF() is the same as BOF().</remarks>
    /// <seealso cref="M:XSharp.Core.Functions.Bof" >Eof Function </seealso>
FUNCTION VODBBof() AS LOGIC
    RETURN VoDb.Do ({ =>
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    RETURN oRDD:BoF
    })
    
    /// <summary>
    /// </summary>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    /// <remarks>VODBBuffRefresh() is like DBBuffRefresh().
    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBAppend(System.Boolean)" select="span[@id='LastError']" />
    /// </remarks>
FUNCTION VODBBuffRefresh() AS LOGIC
    RETURN VoDb.Do ({ =>
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    oRDD:RecInfo(0, DbRecordInfo.DBRI_Updated,NULL)
    RETURN TRUE
    })
    /// <summary>
    /// Clear a logical filter condition.
    /// </summary>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    /// <remarks>VODBClearFilter() is like DBClearFilter().  
    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBAppend(System.Boolean)" select="span[@id='LastError']" />
    /// </remarks>
      
FUNCTION VODBClearFilter() AS LOGIC
    RETURN VoDb.Do ({ =>
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    RETURN oRDD:ClearFilter()
    })
    
    /// <summary>
    /// Clear a locate condition by deleting the locate code block.
    /// </summary>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    /// <remarks>VODBClearLocate() is like DBClearLocate().  
    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBAppend(System.Boolean)" select="span[@id='LastError']" />
    /// </remarks>
    
FUNCTION VODBClearLocate() AS LOGIC
    RETURN VoDb.Do ({ =>
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    RETURN oRDD:ClearScope()
    })
    
    /// <summary>
    /// Clear any active relations.
    /// </summary>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    /// <remarks>VODBClearRelation() is like DBClearRelation().
    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBAppend(System.Boolean)" select="span[@id='LastError']" />
    /// </remarks>
FUNCTION VODBClearRelation() AS LOGIC
    RETURN VoDb.Do ({ =>
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    RETURN oRDD:ClearRel()
    })
    
    /// <summary>
    /// </summary>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    /// <remarks>VODBClearScope() is like DBClearScope().  
    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBAppend(System.Boolean)" select="span[@id='LastError']" />
    /// </remarks>
FUNCTION VODBClearScope() AS LOGIC
    RETURN VoDb.Do ({ =>
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    RETURN oRDD:ClearScope()
    })
    /// <summary>
    /// Close all files in all work areas.
    /// </summary>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    /// <remarks>VODBCloseAll() is like DBCloseAll().  
    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBAppend(System.Boolean)" select="span[@id='LastError']" />
    /// </remarks>
FUNCTION VODBCloseAll() AS LOGIC
    RETURN VoDb.Do ({ =>
    RETURN RuntimeState.Workareas:CloseAll()
    })    
    /// <summary>
    /// Close all files in a work area.
    /// </summary>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    /// <remarks>VODBCloseArea() is like DBCloseArea().  
    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBAppend(System.Boolean)" select="span[@id='LastError']" />
    /// </remarks>
FUNCTION VODBCloseArea() AS LOGIC
    RETURN VoDb.Do ({ =>
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    RETURN oRDD:Close()
    })
    
    /// <summary>
    /// Flush pending updates in one work area.
    /// </summary>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    /// <remarks>VODBCommit() is like DBCommit().  
    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBAppend(System.Boolean)" select="span[@id='LastError']" />
    /// </remarks>
FUNCTION VODBCommit() AS LOGIC
    RETURN VoDb.Do ({ =>
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    RETURN oRDD:Flush()
    })
    
    /// <summary>
    /// Flush pending updates in all work areas.
    /// </summary>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    /// <remarks>VODBCommitAll() is like DBCommitAll().  
    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBAppend(System.Boolean)" select="span[@id='LastError']" />
    /// </remarks>
FUNCTION VODBCommitAll() AS LOGIC
    RETURN VoDb.Do ({ =>
    RETURN RuntimeState.Workareas:CommitAll()
    })
    /// <summary>
    /// Resume a pending locate condition.
    /// </summary>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    /// <remarks>VODBContinue() is like DBContinue().  
    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBAppend(System.Boolean)" select="span[@id='LastError']" />
    /// </remarks>
FUNCTION VODBContinue() AS LOGIC
    RETURN VoDb.Do ({ =>
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    RETURN oRDD:Continue()
    })

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
    /// <param name="cRddName">Name of RDD to use when opening the file.</param>
    /// <param name="lNew">TRUE opens the database file in a new work area (first available).  FALSE opens it in the current work area.  lNew is useful only when lOpen has a value of TRUE. The default is FALSE.</param>
    /// <param name="cAlias">The alias to be associated with the work area where the file is opened.  Within a single thread, X# will not accept duplicate aliases.  cAlias is useful only when lOpen has a value of TRUE.  The default alias is the filename without extension</param>
    /// <param name="cDelim">The delimiter for fields within a delimited database file. The default is a NULL string </param>
    /// <param name="lKeep">TRUE specifies that the file should remain open after creating. FALSE closes the file.</param>
    /// <param name="lJustOpen">TRUE specifies that an existing database file be opened. FALSE specifies that that a new database file be opened.  The default is FALSE.  This can be used to open existing SDF and delimited files, which do not have a structure in the header — in which case, an empty aStruct should be used.</param>
    /// <returns>TRUE when succesfull, otherwise FALSE. When an error has occurred then you can retrieve that error from RuntimeState.LastRddError.</returns>
    /// <seealso cref="M:XSharp.VO.Functions.DbCreate(XSharp.__Usual,XSharp.__Usual,XSharp.__Usual,XSharp.__Usual,XSharp.__Usual,XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)">DBCreate function</seealso>
    /// <seealso cref="O:XSharp.VO.Functions.VODBCreate">VODbCreate overloads in XSharp.VO</seealso>
    /// <seealso cref="O:XSharp.Core.Functions.VODBCreate">VODbCreate overloads in XSharp.Core</seealso>
    
FUNCTION VODBCreate( cName AS STRING, aStruct AS IList<RddFieldInfo>, cRddName AS STRING, lNew AS LOGIC, cAlias AS STRING, cDelim AS STRING, lKeep AS LOGIC, lJustOpen AS LOGIC ) AS LOGIC
    RETURN VoDb.Do ({ =>
    LOCAL rddType AS Type
    IF ( rddType := VoDb.RddNameToType( cRddName ) ) == NULL
        RddError.PostArgumentError( __FUNCTION__, EDB_RDDNOTFOUND, nameof(cRddName), 3, <OBJECT>{ cRddName } )
        RETURN FALSE
    ELSE
        RETURN VODBCreate( cName, aStruct, rddType, lNew, cAlias, cDelim, lKeep, lJustOpen )
    ENDIF
    })
    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBCreate(System.String,System.Collections.Generic.IList{XSharp.RDD.Support.RddFieldInfo},System.String,System.Boolean,System.String,System.String,System.Boolean,System.Boolean)" />
    /// <param name="rddList">List of RDDs to use when creating the file</param>

FUNCTION VODBCreate( cName AS STRING, aStruct AS IList<RddFieldInfo>, rddList AS _RddList, lNew AS LOGIC, cAlias AS STRING, cDelim AS STRING, lKeep AS LOGIC, lJustOpen AS LOGIC ) AS LOGIC
    LOCAL oRdd := NULL  AS RegisteredRDD
    FOREACH VAR name IN rddList:atomRddName
        oRdd := RegisteredRDD.Find(name)
        oRdd:Load()
    NEXT
    IF oRdd != NULL_OBJECT
        RETURN VODBCreate(cName, aStruct, oRdd:RddType, lNew, cAlias, cDelim, lKeep, lJustOpen)
    ENDIF
    RETURN FALSE 
    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBCreate(System.String,System.Collections.Generic.IList{XSharp.RDD.Support.RddFieldInfo},System.String,System.Boolean,System.String,System.String,System.Boolean,System.Boolean)" />
    /// <param name="rddType">Type of the class that must be used to work with the RDD.</param>

FUNCTION VODBCreate( cName AS STRING, aStruct AS IList<RddFieldInfo>, rddType AS System.Type, lNew AS LOGIC, cAlias AS STRING, cDelim AS STRING, lKeep AS LOGIC, lJustOpen AS LOGIC ) AS LOGIC
    RETURN VoDbCreate(cName, aStruct:ToArray(), rddType, lNew, cAlias, cDelim, lKeep, lJustOpen)
    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBCreate(System.String,System.Collections.Generic.IList{XSharp.RDD.Support.RddFieldInfo},System.String,System.Boolean,System.String,System.String,System.Boolean,System.Boolean)" />
    /// <param name="aStruct">Structure to use when creating the file.</param>
    /// <param name="rddType">Type of the class that must be used to work with the RDD.</param>
    
FUNCTION VODBCreate( cName AS STRING, aStruct AS RddFieldInfo[], rddType AS System.Type, lNew AS LOGIC, cAlias AS STRING, cDelim AS STRING, lKeep AS LOGIC, lJustOpen AS LOGIC ) AS LOGIC
    RETURN VoDb.Do ({ =>
    LOCAL uiOldArea := 0 AS DWORD
    LOCAL uiNewArea := 0 AS DWORD
    LOCAL ret   := FALSE   AS LOGIC
    RuntimeState.LastRddError := NULL
    IF String.IsNullOrEmpty( cName )
        RddError.PostArgumentError( __FUNCTION__, EDB_USE, nameof(cName), 1, <OBJECT>{ cName } )
    ELSEIF aStruct == NULL
        RddError.PostArgumentError( __FUNCTION__, EDB_USE, nameof(aStruct), 2 ,NULL)
    ELSEIF lNew && ! ( ret := VODBSelect( 0, REF uiOldArea ) )
        RddError.PostError( __FUNCTION__, EG_CREATE, EDB_NOAREAS )
    ELSE
        ret := TRUE   
    ENDIF
    IF lNew
        uiNewArea := RuntimeState.Workareas:FindEmptyArea(TRUE)
    ELSE
        // VO Closes the current workarea
        uiNewArea := RuntimeState.Workareas:CurrentWorkAreaNO
        RuntimeState.Workareas:CloseArea(uiNewArea)
    ENDIF
    RuntimeState.Workareas:CurrentWorkAreaNO := uiNewArea
    IF ret && String.IsNullOrEmpty( cAlias ) && ! ( ret := VoDb.AliasFromFilename( cName, cAlias ) )
        RddError.PostArgumentError( __FUNCTION__, EDB_BADALIAS, nameof(cAlias), 5, <OBJECT>{ cAlias } )
    ENDIF   
    IF ret && ! ( ret := VoDb.IsAliasUnused( cAlias ) )
        RddError.PostArgumentError( __FUNCTION__, EDB_DUPALIAS, nameof(cAlias), 5, <OBJECT>{ cAlias } )
    ENDIF
    // Now all arguments are valid. So lets create the RDD Object and try to create the file
    LOCAL oRDD AS IRDD
    oRDD := VoDb.CreateRDDInstance(rddType, cAlias)
    IF oRDD == NULL
        RddError.PostArgumentError( __FUNCTION__, EDB_DRIVERLOAD, nameof(rddType), 3, <OBJECT>{ rddType } )
        ret := FALSE
    ELSEIF ! VoDb.IsAliasUnused( cAlias )
        RddError.PostArgumentError( __FUNCTION__, EDB_DUPALIAS, nameof(cAlias), 4, <OBJECT>{ cAlias } )
        ret := FALSE
    ELSE
        IF ! String.IsNullOrEmpty( cDelim )
            oRDD:Info( DBI_SETDELIMITER, cDelim ) 
        ENDIF
        IF ret
            ret := oRDD:CreateFields(aStruct)
        ENDIF
        IF ret
            LOCAL dboi := DBOPENINFO{} AS DbOpenInfo
            dboi:FileName  := Path.ChangeExtension( cName, NULL )
            IF cName:EndsWith(".")
                dboi:Extension := "."
            ELSE
                dboi:Extension := Path.GetExtension( cName )
            ENDIF        
            dboi:Shared    := FALSE
            dboi:ReadOnly  := FALSE
            dboi:Alias     := cAlias
            dboi:WorkArea  := uiNewArea
            oRDD:Alias     := cAlias
            ret := RuntimeState.Workareas:SetArea(uiNewArea, oRDD)
            IF lJustOpen
                ret := oRdd:Open( dboi )
            ELSE
                ret := oRdd:Create( dboi )
            ENDIF
        ENDIF
        IF ret .AND. ! lKeep
            RuntimeState.Workareas:CloseArea(uiNewArea)
            IF uiOldArea != 0
                RuntimeState.Workareas:CurrentWorkAreaNO := uiOldArea
            ENDIF
        ENDIF
    ENDIF
    RETURN ret
    })
    
    /// <summary>
    /// Mark the current record for deletion.
    /// </summary>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    /// <remarks>VODBDelete() is like DBDelete().  
    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBAppend(System.Boolean)" select="span[@id='LastError']" />
    /// </remarks>
    /// <seealso cref="M:XSharp.VO.Functions.DbDelete">DbDelete Function</seealso>
FUNCTION VODBDelete() AS LOGIC
    RETURN VoDb.Do ({ =>
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    RETURN oRDD:Delete()
    })
    
    /// <summary>
    /// Return the deleted status of the current record.
    /// </summary>
    /// <returns>TRUE if the current record is marked for deletion; otherwise, FALSE.  
    /// If there is no database file in use in the current work area, VODBDeleted() returns FALSE.</returns>
    /// <seealso cref="M:XSharp.Core.Functions.Deleted">Deleted Function</seealso>
FUNCTION VODBDeleted() AS LOGIC
    RETURN VoDb.Do ({ =>
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    RETURN oRDD:Deleted
    })
    
    /// <summary>
    /// Determine when end-of-file is encountered.
    /// </summary>
    /// <returns>TRUE when an attempt is made to move the record pointer beyond the last logical record in a
    /// database file or if the current database file contains no records; otherwise, FALSE.  If there is no
    /// database file open in the current work area, VODBEOF() returns TRUE.</returns>
    /// <remarks>VODBEOF() is the same as EOF().</remarks>
    /// <seealso cref="M:XSharp.Core.Functions.Eof" >Eof Function </seealso>
FUNCTION VODBEof() AS LOGIC
    RETURN VoDb.Do ({ =>
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    RETURN oRDD:EoF
    })
    
    /// <summary>
    /// Evaluate a code block for each record that matches a specified scope and/or condition.
    /// </summary>
    /// <param name="uBlock"></param>
    /// <param name="uCobFor"></param>
    /// <param name="uCobWhile"></param>
    /// <param name="uNext"></param>
    /// <param name="nRecno"></param>
    /// <param name="lRest"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION VODBEval(uBlock AS ICodeBlock,uCobFor AS ICodeBlock,uCobWhile AS ICodeBlock,uNext AS OBJECT,nRecno AS OBJECT,lRest AS LOGIC) AS LOGIC
    RETURN VoDb.Do ({ =>
    LOCAL nNext AS LONG
    IF uBlock == NULL
        THROW Error.ArgumentError(__FUNCTION__, nameof(uBlock),1, <OBJECT>{uBlock})
    ELSEIF uCobFor == NULL
        THROW Error.ArgumentError(__FUNCTION__, nameof(uCobFor),2, <OBJECT>{uCobFor})        
    ELSEIF uCobWhile == NULL    
        THROW Error.ArgumentError(__FUNCTION__, nameof(uCobWhile),3, <OBJECT>{uCobWhile})
    ELSE
        TRY
            IF uNext != NULL
                nNext := Convert.ToInt32(uNext)
            ELSE
                nNext := 0
            ENDIF
        CATCH AS Exception
            THROW Error.ArgumentError(__FUNCTION__, nameof(uNext),4, <OBJECT>{uNext})
        END TRY
    ENDIF
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    LOCAL oInfo AS DbEvalInfo
    oInfo := DbEvalInfo{}
    oInfo:Block := uBlock
    oInfo:ScopeInfo:ForBlock := uCobFor
    oInfo:ScopeInfo:WhileBlock := uCobWhile
    oInfo:ScopeInfo:NextCount  := nNext
    oInfo:ScopeInfo:RecId      := nRecno
    oInfo:ScopeInfo:Rest       := lRest
    RETURN oRDD:DbEval(oInfo)
    })    
    /// <summary>
    /// Retrieve the value of a specified database field.
    /// </summary>
    /// <param name="nPos"></param>
    /// <param name="oRet"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION _VODBFieldGet(nPos AS DWORD,oRet REF OBJECT) AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
        oRet := oRDD:GetValue((INT) nPos)
        RETURN TRUE
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    RETURN FALSE

    /// <summary>
    /// Retrieve field definition information about a field.
    /// </summary>
    /// <param name="nOrdinal"></param>
    /// <param name="nPos"></param>
    /// <param name="oRet"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    /// <remarks>VODBFieldInfo() is like DBFieldInfo().
    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBAppend(System.Boolean)" select="span[@id='LastError']" />
    /// </remarks>
FUNCTION _VODBFieldInfo(nOrdinal AS DWORD,nPos AS DWORD,oRet AS OBJECT) AS LOGIC
    RETURN VoDb.Do ({ =>
     RETURN _VODBFieldInfo(nOrdinal, nPos, REF oRet)
    }) 
    /// <summary>
    /// Retrieve field definition information about a field.
    /// </summary>
    /// <param name="nOrdinal"></param>
    /// <param name="nPos"></param>
    /// <param name="oRet"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    /// <remarks>VODBFieldInfo() is like DBFieldInfo().
    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBAppend(System.Boolean)" select="span[@id='LastError']" />
    /// </remarks>
FUNCTION _VODBFieldInfo(nOrdinal AS DWORD,nPos AS DWORD,oRet REF OBJECT) AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
        oRet := oRDD:FieldInfo((INT) nPos, (INT) nOrdinal, oRet)
        RETURN TRUE
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    RETURN FALSE
    /// <summary>
    /// Set the value of a specified database field.
    /// </summary>
    /// <param name="nPos"></param>
    /// <param name="xValue"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    /// <remarks>
    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBAppend(System.Boolean)" select="span[@id='LastError']" />
    /// </remarks>

FUNCTION VODBFieldPut(nPos AS DWORD,xValue AS OBJECT) AS LOGIC
    RETURN VoDb.Do ({ =>
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    RETURN oRDD:PutValue((INT) nPos, xValue)
    })    
    
    /// <summary>
    /// </summary>
    /// <param name="nPos"></param>
    /// <param name="cFile"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    /// <remarks>VODBFileGet() is like DBFileGet().
    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBAppend(System.Boolean)" select="span[@id='LastError']" />
    /// </remarks>
FUNCTION VODBFileGet(nPos AS DWORD,cFile AS STRING) AS LOGIC
    RETURN VoDb.Do ({ =>
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    RETURN oRDD:GetValueFile((INT) nPos, cFile)
    })
    
    /// <summary>
    /// </summary>
    /// <param name="nPos"></param>
    /// <param name="cFile"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION VODBFilePut(nPos AS DWORD,cFile AS STRING) AS LOGIC
    RETURN VoDb.Do ({ =>
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    RETURN oRDD:PutValueFile((INT) nPos, cFile)
    })
    
    /// <summary>
    /// Return a filter.
    /// </summary>
    /// <returns>
    /// </returns>
    /// <remarks>VODBFilter() is like DBFilter().
    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBAppend(System.Boolean)" select="span[@id='LastError']" />
    /// </remarks>
FUNCTION VODBFilter() AS STRING
    RETURN VoDb.Do ({ =>
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    RETURN oRDD:FilterText
    })
    
    /// <summary>
    /// Lock an opened and shared database file.
    /// </summary>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    /// <remarks>VODBFlock() is like DBFlock().
    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBAppend(System.Boolean)" select="span[@id='LastError']" />
    /// </remarks>
FUNCTION VODBFlock() AS LOGIC
    RETURN VoDb.Do ({ =>    
    VAR oRDD := RDDHelpers.CWA(__FUNCTION__)
    LOCAL dbli AS DbLockInfo
    dbli := DbLockInfo{}
    dbli:Result := FALSE
    dbli:@@METHOD := DbLockInfo.LockMethod.File
    RETURN oRDD:Lock(dbli)
    })
    
    /// <summary>
    /// Determine if the previous search operation succeeded.
    /// </summary>
    /// <returns>
    /// </returns>
    /// <remarks>VODBFound() is like Found().
    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBAppend(System.Boolean)" select="span[@id='LastError']" />
    /// </remarks>
FUNCTION VODBFound() AS LOGIC
    RETURN VoDb.Do ({ =>
    VAR oRDD := RDDHelpers.CWA(__FUNCTION__)
    RETURN oRDD:Found
    })
    
    
    
    /// <summary>
    /// Return the work area number.
    /// </summary>
    /// <returns>
    /// </returns>
    /// <remarks>VODBGetSelect() is like DBGetSelect().
    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBAppend(System.Boolean)" select="span[@id='LastError']" />
    /// </remarks>
FUNCTION VODBGetSelect() AS DWORD
    RETURN VoDb.Do ({ =>
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    RETURN oRDD:Area
    })
    
    /// <summary>
    /// Move to the last logical record.
    /// </summary>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    /// <remarks>VODBGoBottom() is like DBGoBottom().  
    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBAppend(System.Boolean)" select="span[@id='LastError']" />
    /// </remarks>
    
FUNCTION VODBGoBottom() AS LOGIC
    RETURN VoDb.Do ({ =>
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    RETURN oRDD:GoBottom()
    })
    /// <summary>
    /// Move to a record specified by record number.
    /// </summary>
    /// <param name="uRecId"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION VODBGoto(uRecId AS OBJECT) AS LOGIC
    RETURN VoDb.Do ({ =>
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    RETURN oRDD:GoToId(uRecID)
    })
    
    /// <summary>
    /// Move to the first logical record.
    /// </summary>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    /// <remarks>VODBGoTop() is like DBGoTop().  
    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBAppend(System.Boolean)" select="span[@id='LastError']" />
    /// </remarks>
    
FUNCTION VODBGoTop() AS LOGIC
    RETURN VoDb.Do ({ =>
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    RETURN oRDD:GoTop()
    })
    

    /// <summary>
    /// Retrieve information about a work area.
    /// </summary>
    /// <param name="nOrdinal"></param>
    /// <param name="ptrRet"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION _VODBInfo(nOrdinal AS DWORD,oRet AS OBJECT) AS LOGIC
     RETURN VoDb.Do ({ =>
            RETURN _VoDbInfo(nOrdinal, REF oRet)
    })
     

    /// <summary>
    /// Retrieve information about a work area.
    /// </summary>
    /// <param name="nOrdinal"></param>
    /// <param name="ptrRet"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION _VODBInfo(nOrdinal AS DWORD,oRet REF OBJECT) AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
        IF (nOrdinal == DBI_RDD_OBJECT)
            oRet := oRDD
        ELSE
            oRet := oRDD:Info((INT) nOrdinal, oRet)
        ENDIF
        RETURN TRUE
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    
    RETURN FALSE   
    
    /// <summary>
    /// </summary>
    /// <param name="nSelect"></param>
    /// <param name="struList"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION VODBJoinAppend(nSelect AS DWORD,struList AS _JoinList) AS LOGIC
    RETURN VoDb.Do ({ =>
    LOCAL result := FALSE AS LOGIC
    LOCAL nCount AS LONG
    LOCAL nDestSel AS DWORD
    LOCAL nFld AS LONG
    LOCAL oRDDDest AS IRDD
    LOCAL oRDDSrc AS IRDD
    LOCAL oValue  AS OBJECT
    nCount := struList:Count
    nDestSel := struList:uiDestSel
    oRDDDest := RuntimeState.Workareas.GetRDD(nDestSel)
    IF oRDDDest == NULL
        RddError.PostNoTableError(__FUNCTION__)
    ELSE
        FOR nFld := 0 TO nCount-1
            oRDDSrc := RuntimeState.Workareas.GetRDD(struList:Fields[nFld]:Area)
            IF oRDDSrc == NULL_OBJECT
                RddError.PostNoTableError(__FUNCTION__)
            ENDIF
            oValue := oRDDSrc:GetValue((INT) struList:Fields[nFld]:Pos)
            result := oRDDDest:PutValue(nFld, oValue)
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
FUNCTION VODBLastRec() AS LONG
    RETURN VoDb.Do ({ =>
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    RETURN oRDD:RecCount
    })  
    
    /// <summary>
    /// Search for the first record that matches a specified condition and scope.
    /// </summary>
    /// <param name="uCobFor"></param>
    /// <param name="uCobWhile"></param>
    /// <param name="nNext"></param>
    /// <param name="uRecId"></param>
    /// <param name="lRest"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    /// <remarks>VODBLocate() is like DBLocate() but strongly typed.  
    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBAppend(System.Boolean)" select="span[@id='LastError']" />
    /// </remarks>
FUNCTION VODBLocate(uCobFor AS ICodeBlock,uCobWhile AS ICodeBlock,nNext AS LONG,uRecId AS OBJECT,lRest AS LOGIC) AS LOGIC
    RETURN VoDb.Do ({ =>
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    LOCAL scopeinfo := DBSCOPEINFO{} AS DBSCOPEINFO
    scopeinfo:ForBlock := uCobFor
    scopeinfo:WhileBlock := uCobWhile
    scopeinfo:Rest:= lRest
    scopeinfo:RecId := uRecID
    scopeinfo:NextCount := nNext
    IF oRDD:SetScope(scopeinfo)
        RETURN oRDD:SkipScope(1)
    ENDIF
    RETURN FALSE
    })  
    
    /// <summary>
    /// </summary>
    /// <param name="cDriver"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBMemoExt(cDriver AS STRING) AS STRING
    RETURN VoDb.Do ({ =>
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    RETURN (STRING) oRDD:Info(DBI_MEMOEXT, NULL)
    })
    /// <summary>
    /// Return the default index file extension for a work area as defined by the its RDD.
    /// </summary>
    /// <returns>Default extension for the current workarea, or an empty string when no table is open in the current workarea.
    /// </returns>
FUNCTION VODBOrdBagExt() AS STRING
    RETURN VoDb.Do ({ =>
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    VAR info := DbOrderInfo{}
    RETURN (STRING) oRDD:OrderInfo( DBOI_BAGEXT, info)
    })
    
    /// <summary>
    /// Set the condition and scope for an order.
    /// </summary>
    /// <param name="ordCondInfo">An object defining the condition and scope information. </param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    /// <remarks>VODBOrdCondSet() is like OrdCondSet() but strongly typed and the condition information is passed in an object.
    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBAppend(System.Boolean)" select="span[@id='LastError']" />
    /// </remarks>
FUNCTION VODBOrdCondSet(ordCondInfo AS DbOrderCondInfo) AS LOGIC
    RETURN VoDb.Do ({ =>
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    RETURN oRDD:OrderCondition(ordCondInfo)
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
    /// <remarks>VODBOrdCreate() is like DbCreateOrder() but strongly typed and the condition information is passed in an object.
    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBAppend(System.Boolean)" select="span[@id='LastError']" />
    /// </remarks>
    
    
FUNCTION VODBOrdCreate(cBagName AS STRING,oOrder AS OBJECT,cExpr AS STRING,oCodeBlock AS ICodeBlock,lUnique AS LOGIC,ordCondInfo AS DbOrderCondInfo) AS LOGIC
    RETURN VoDb.Do ({ =>
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    VAR info := DbOrderCreateInfo{}
    info:BagName 		:= cBagName
    info:Order			:= oOrder
    info:Expression 	:= cExpr
    info:Block      	:= oCodeBlock
    info:Unique			:= lUnique
    info:OrdCondInfo 	:= ordCondInfo		
    RETURN oRDD:OrderCreate(info)
    })
    /// <summary>
    /// Remove an order from an open index file.
    /// </summary>
    /// <param name="cBagName"></param>
    /// <param name="oOrder"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    /// <remarks>VODBOrdDestroy() is like DbDeleteOrder() but strongly typed.
    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBAppend(System.Boolean)" select="span[@id='LastError']" />
    /// </remarks>
    
    
FUNCTION VODBOrdDestroy(cBagName AS STRING,oOrder AS OBJECT) AS LOGIC
    RETURN VoDb.Do ({ =>
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    VAR info := DbOrderInfo{}
    info:BagName := cBagName
    info:Order   := oOrder
    RETURN oRDD:OrderDestroy(info)
    })  
    /// <summary>
    /// Return information about index files and the orders in them.
    /// </summary>
    /// <param name="nOrdinal"></param>
    /// <param name="cBagName"></param>
    /// <param name="uOrder"></param>
    /// <param name="oValue"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    /// <remarks>VODBOrderInfo() is like DbOrderInfo() but strongly typed.
    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBAppend(System.Boolean)" select="span[@id='LastError']" />
    /// </remarks>
    
    
FUNCTION _VODBOrderInfo(nOrdinal AS DWORD,cBagName AS STRING,oOrder AS OBJECT,oValue AS OBJECT) AS LOGIC
    RETURN VoDb.Do ({ =>
    RETURN _VoDbOrderInfo(nOrdinal, cBagName, oOrder, REF oValue)
    })  
	/// <summary>
    /// Return information about index files and the orders in them.
    /// </summary>
    /// <param name="nOrdinal"></param>
    /// <param name="cBagName"></param>
    /// <param name="uOrder"></param>
    /// <param name="oValue"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION _VODBOrderInfo(nOrdinal AS DWORD,cBagName AS STRING,oOrder AS OBJECT,oValue REF OBJECT) AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
        VAR info := DbOrderInfo{}
        info:BagName := cBagName
        info:Order   := oOrder
        oValue :=  oRDD:OrderInfo(nOrdinal, info)
        RETURN TRUE
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    
    RETURN FALSE
    
    
    /// <summary>
    /// Open an index file and add specified orders to the order list in a work area.
    /// </summary>
    /// <param name="cBagName"></param>
    /// <param name="oOrder"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    /// <remarks>VODBOrdListAdd() is like DbSetIndex() but strongly typed.
    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBAppend(System.Boolean)" select="span[@id='LastError']" />
    /// </remarks>
FUNCTION VODBOrdListAdd(cBagName AS STRING,oOrder AS OBJECT) AS LOGIC
    RETURN VoDb.Do ({ =>
    
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    VAR info := DbOrderInfo{}
    info:BagName := cBagName
    IF oOrder == NULL
        info:AllTags := TRUE
    ELSE
        info:Order   := oOrder
    ENDIF
    RETURN oRDD:OrderListAdd(info)
    })
    
    /// <summary>
    /// Remove orders from the order list in a work area and close associated index files.
    /// </summary>
    /// <param name="cBagName"></param>
    /// <param name="oOrder"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    /// <remarks>VODBOrdListClear() is like DBClearIndex() but strongly typed.
    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBAppend(System.Boolean)" select="span[@id='LastError']" />
    /// </remarks>
    
    
FUNCTION VODBOrdListClear(cBagName AS STRING,oOrder AS OBJECT) AS LOGIC
    RETURN VoDb.Do ({ =>
    
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__,FALSE) AS IRDD
    IF oRDD == NULL
        RETURN TRUE // not logical but compatible with VO
    ELSE
        VAR info := DbOrderInfo{}
        info:BagName := cBagName
        IF oOrder == NULL
            info:AllTags := TRUE
        ELSE
            info:Order   := oOrder
        ENDIF
        RETURN oRDD:OrderListDelete(info) 
    ENDIF
    })  
    /// <summary>
    /// Rebuild all orders in the order list of a work area.
    /// </summary>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    /// <remarks>VODBOrdListClear() is like OrdListRebuild().
    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBAppend(System.Boolean)" select="span[@id='LastError']" />
    /// </remarks>
    
    
FUNCTION VODBOrdListRebuild() AS LOGIC
    RETURN VoDb.Do ({ =>
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    RETURN oRDD:OrderListRebuild()
    })
    
    /// <summary>
    /// Set the controlling order for a work area.
    /// </summary>
    /// <param name="cBagName"></param>
    /// <param name="oOrder"></param>
    /// <param name="strPreviousOrder"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    /// <remarks>VODBOrdSetFocus() is like DbSetOrder() but strongly typed.
    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBAppend(System.Boolean)" select="span[@id='LastError']" />
    /// </remarks>
    
    
FUNCTION VODBOrdSetFocus(cBagName AS STRING,oOrder AS OBJECT, strPreviousOrder OUT STRING) AS LOGIC
    strPreviousOrder := ""
    TRY
        LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
        VAR info     := DbOrderInfo{}
        info:BagName := cBagName
        info:Order   := oOrder
        strPreviousOrder := String.Empty
        VAR result := oRDD:OrderListFocus(info)
        IF result .AND. info:Result IS STRING
            strPreviousOrder := (STRING)info:Result
        ENDIF
        RETURN result
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    
    RETURN FALSE
    /// <summary>
    /// Remove all records that have been marked for deletion from a database file.
    /// </summary>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    /// <remarks>VODBPack() is like DbPack(). 
    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBAppend(System.Boolean)" select="span[@id='LastError']" />
    /// </remarks>
    
FUNCTION VODBPack() AS LOGIC
    RETURN VoDb.Do ({ =>
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    RETURN oRDD:Pack()
    })
    
    /// <summary>
    /// </summary>
    /// <param name="nRddType"></param>
    /// <returns>
    /// </returns>
[Obsolete( "'VODBRddCount( nRddType )' is not supported, use VODBRddCount() instead", TRUE )];
FUNCTION VODBRddCount(nRddType AS DWORD) AS DWORD
    RETURN 0
    
    /// <summary>
    /// </summary>
    /// <param name="nRddType"></param>
    /// <returns>
/// </returns>
FUNCTION VODBRddCount() AS DWORD
    RETURN VoDb.Do ({ =>
    RETURN (DWORD) VODBRddList():Length
    })
    /// <summary>
    /// </summary>
    /// <param name="nOrdinal"></param>
    /// <param name="ptrRet"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
     
    
FUNCTION _VODBRDDInfo(nOrdinal AS DWORD,oRet REF OBJECT) AS LOGIC
    TRY
        LOCAL oValue AS OBJECT
        oValue := RuntimeState.GetValue<OBJECT> ((INT) nOrdinal)
        IF oRet != NULL_OBJECT
            RuntimeState.SetValue((INT) nOrdinal, oRet)
        ENDIF
        oRet := oValue
        RETURN oValue != NULL
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    RETURN FALSE
    
    /// <summary>
    /// </summary>
    /// <param name="nOrdinal"></param>
    /// <param name="ptrRet"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>    
FUNCTION _VODBRDDInfo(nOrdinal AS DWORD,oRet AS OBJECT) AS LOGIC
    RETURN VoDb.Do ({ =>
    LOCAL oValue AS OBJECT
    oValue := RuntimeState.GetValue<OBJECT> ((INT) nOrdinal)
    RuntimeState.SetValue((INT) nOrdinal, oRet)
    RETURN TRUE
    })
    
    
[Obsolete( "'VODBRddList( rddList, nRddType )' is not supported, use VODBRddList() instead", TRUE )];
FUNCTION VODBRddList(rddList AS _RddList,nRddType AS DWORD) AS LOGIC
    THROW  NotImplementedException{}
    
FUNCTION VODBRddList() AS STRING[]
    RETURN VoDb.Do ({ =>
    LOCAL aList AS List<STRING>
    aList := List<STRING>{}
    LOCAL i AS DWORD
    FOR i := 1 TO WorkAreas.MaxWorkAreas
        VAR oRDD := RuntimeState.Workareas.GetRDD(i)
        IF oRDD != NULL
            LOCAL cName AS STRING
            cName := oRDD:SysName
            IF !aList:Contains(cName)
                aList:Add(cname)
            ENDIF
        ENDIF
    NEXT
    RETURN aList:ToArray()
    })
    
    
    /// <summary>
    /// Return an RDD name.                  
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBRddName() AS STRING
    RETURN VoDb.Do ({ =>
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    RETURN oRDD:SysName
    })
    /// <summary>
    /// Return and optionally change the default RDD for the application.
    /// </summary>
    /// <param name="cNewRDD"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBRddSetDefault(cNewRDD AS STRING) AS STRING
    LOCAL cOldRDD AS STRING
    cOldRDD := Runtimestate.DefaultRDD
    IF ! String.IsNullOrEmpty(cNewRDD)
        Runtimestate.DefaultRDD := cNewRDD
    ENDIF
    RETURN cOldRDD
    
    /// <summary>
    /// Restore the current record if it is marked for deletion.
    /// </summary>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    /// <remarks>VODBRecall() is like DBRecall().
    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBAppend(System.Boolean)" select="span[@id='LastError']" />
    /// </remarks>
    
FUNCTION VODBRecall() AS LOGIC
    RETURN VoDb.Do ({ =>
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    RETURN oRDD:Recall()
    })
    
    /// <summary>
    /// Return the current record number.
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBRecno() AS DWORD
    RETURN VoDb.Do ({ =>
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    RETURN (DWORD) oRDD:Recno
    })
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBRecordGet() AS BYTE[]
    RETURN VoDb.Do ({ =>
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    RETURN oRDD:GetRec()
    }) 

    /// <summary>
    /// Retrieve information about a record.
    /// </summary>
    /// <param name="nOrdinal"></param>
    /// <param name="oRecID"></param>
    /// <param name="oRet"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>    
FUNCTION _VODBRecordInfo(nOrdinal AS DWORD,oRecID AS OBJECT,oRet AS OBJECT) AS LOGIC
    RETURN VoDb.Do ({ =>
    RETURN _VODBRecordInfo(nOrdinal, oRecID, REF oRet)
    }) 

    /// <summary>
    /// Retrieve information about a record.
    /// </summary>
    /// <param name="nOrdinal"></param>
    /// <param name="oRecID"></param>
    /// <param name="oRet"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>    
FUNCTION _VODBRecordInfo(nOrdinal AS DWORD,oRecID AS OBJECT,oRet REF OBJECT) AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
        oRDD:RecInfo(oRecID, (INT) nOrdinal, oRet )
        RETURN TRUE
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    
    RETURN FALSE       
    
    /// <summary>
    /// </summary>
    /// <param name="aRecord"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>    
FUNCTION VODBRecordPut(aRecord AS BYTE[]) AS LOGIC
    RETURN VoDb.Do ({ =>
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    RETURN oRDD:PutRec(aRecord)
    })
    
    /// <summary>
    /// Return the linking expression of a specified relation.
    /// </summary>
    /// <param name="nPos"></param>
    /// <param name="pszRel"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    /// <remarks>VODBSetRelation() is like DBSetRelation().
    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBAppend(System.Boolean)" select="span[@id='LastError']" />
    /// </remarks>
    
FUNCTION VODBRelation(nPos AS DWORD,sRel REF STRING) AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
        sRel :=  oRDD:RelText(nPos)
        RETURN TRUE
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    
    RETURN FALSE   
    
    /// <summary>
    /// Lock the current record.
    /// </summary>
    /// <param name="uRecId"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    /// <remarks>VODBRlock() is like DBRlock() but strongly typed.
    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBAppend(System.Boolean)" select="span[@id='LastError']" />
    /// </remarks>
    
FUNCTION VODBRlock(uRecId AS OBJECT) AS LOGIC
    RETURN VoDb.Do ({ =>
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    LOCAL lockInfo AS DbLockInfo
    lockInfo := DbLockInfo{}
    lockInfo:RecId := uRecID
    lockInfo:@@METHOD  := DbLockInfo.LockMethod.Multiple
    RETURN oRDD:Lock(lockInfo)
    })
    /// <summary>
    /// Return the work area number of a relation.
    /// </summary>
    /// <param name="nPos"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    /// <remarks>VODBRSelect() is like DBRSelect() but strongly typed.
    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBAppend(System.Boolean)" select="span[@id='LastError']" />
    /// </remarks>
    
FUNCTION VODBRSelect(nPos AS DWORD) AS DWORD
    RETURN VoDb.Do ({ =>
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    RETURN oRDD:RelArea(nPos)
    })
    /// <summary>
    /// Move to the record having the specified key value.
    /// </summary>
    /// <param name="oValue"></param>
    /// <param name="lSoftSeek"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    /// <remarks>VODBSeek() is like DBSeek() but strongly typed.
    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBAppend(System.Boolean)" select="span[@id='LastError']" />
    /// </remarks>
    
FUNCTION VODBSeek(oValue AS OBJECT,lSoftSeek AS LOGIC) AS LOGIC
    RETURN VoDb.Do ({ =>
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    VAR info 		:= DbSeekInfo{}
    info:Value 		:= oValue          
    info:SoftSeek 	:= lSoftSeek
    //info:Last		:= lLast 
    RETURN oRDD:Seek(info)
    })
    
    
    /// <summary>
    /// Select a new work area and retrieve the current work area.
    /// </summary>
    /// <param name="nNew"></param>
    /// <param name="nOld"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    /// <remarks>VODBSelect() is like DBSelect() but strongly typed.
    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBAppend(System.Boolean)" select="span[@id='LastError']" />
    /// </remarks>
    
FUNCTION VODBSelect(nNew AS DWORD,nOld REF DWORD ) AS LOGIC
    TRY
        nOld := (DWORD) RuntimeState.Workareas:CurrentWorkAreaNO
        IF nNew != nOld
            IF nNew == 0
                nNew := (DWORD) RuntimeState.Workareas:FindEmptyArea(TRUE)
            ENDIF
            IF nNew > WorkAreas.MaxWorkareas
                RddError.PostArgumentError( __FUNCTION__, EDB_SELECT, nameof(nNew), 1, <OBJECT>{ nNew } )
            ELSE
                RuntimeState.Workareas:CurrentWorkAreaNO :=  nNew
            ENDIF
        ENDIF
        RETURN TRUE
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    
    RETURN FALSE   
    
    /// <summary>
    /// Set a filter condition.
    /// </summary>
    /// <param name="oBlock"></param>
    /// <param name="cFilter"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    /// <remarks>VODBSetFilter() is like DBSetFilter() but strongly typed.
    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBAppend(System.Boolean)" select="span[@id='LastError']" />
    /// </remarks>
    
FUNCTION VODBSetFilter(oBlock AS ICodeBlock,cFilter AS STRING) AS LOGIC
    RETURN VoDb.Do ({ =>
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    VAR info 		 := DbFilterInfo{}
    info:FilterBlock := oBlock         
    info:FilterText  := cFilter
    RETURN oRDD:SetFilter(info)
    })
    
    /// <summary>
    /// Set the found flag.
    /// </summary>
    /// <param name="lFound"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    /// <remarks>VODBSetFound() is like DBSetFound().
    /// </remarks>
    
FUNCTION VODBSetFound(lFound AS LOGIC) AS LOGIC
    RETURN VoDb.Do ({ =>
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    oRDD:Found := TRUE
    RETURN TRUE
    })
    /// <summary>
    /// Specify the code block for a locate condition.
    /// </summary>
    /// <param name="oBlock"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    /// <remarks>VODBSetLocate() is like DBSetLocate() but strongly typed.
    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBAppend(System.Boolean)" select="span[@id='LastError']" />
    /// </remarks>
    
FUNCTION VODBSetLocate(oBlock AS ICodeBlock) AS LOGIC
    RETURN VoDb.Do ({ =>
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    VAR scope := oRDD:GetScope()
    scope:ForBlock := oBlock
    oRDD:SetScope(scope)    
    RETURN TRUE
    })                      
    
    /// <summary>
    /// Relate a specified work area to the current work area.
    /// </summary>
    /// <param name="cAlias"></param>
    /// <param name="uCobKey"></param>
    /// <param name="cKey"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    /// <remarks>VODBSetRelation() is like DBSetRelation() but strongly typed.
    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBAppend(System.Boolean)" select="span[@id='LastError']" />
    /// </remarks>
    
FUNCTION VODBSetRelation(cAlias AS STRING,oKey  AS ICodeBlock,cKey AS STRING) AS LOGIC
    RETURN VoDb.Do ({ =>
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    LOCAL nDest := RuntimeState.Workareas.FindAlias(cAlias) AS DWORD
    IF nDest == 0
        RddError.PostArgumentError(__FUNCTION__,EDB_SETRELATION, nameof(cAlias), 1, <OBJECT>{cAlias})
    ENDIF
    LOCAL oDest := RuntimeState.Workareas.GetRDD(nDest) AS IRDD
    IF oDest == NULL_OBJECT
        RddError.PostArgumentError(__FUNCTION__,EDB_SETRELATION, nameof(cAlias), 1, <OBJECT>{cAlias})
    ENDIF
    LOCAL oRelInfo AS DbRelInfo
    oRelInfo := DbRelInfo{}
    oRelInfo:Parent := oRDD
    oRelInfo:Child  := oDest
    oRelInfo:Key    := cKey
    oRelInfo:Block  := oKey
    RETURN oRDD:SetRel(oRelInfo)
    })
    
    
    /// <summary>
    /// </summary>
    /// <param name="scope"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION VODBSetScope(scope AS DbScopeInfo) AS LOGIC
    RETURN VoDb.Do ({ =>
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    RETURN oRDD:SetScope(scope)
    })
    /// <summary>
    /// Select a new work area.
    /// </summary>
    /// <param name="siNew">The number of the new work area. 0 indicates the first available free workarea. -1 indicates the last available free workarea.</param>
    /// <returns>The newly selected work area.</returns>
    /// <remarks>VODBSetSelect() is like DBSetSelect() but strongly typed.
    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBAppend(System.Boolean)" select="span[@id='LastError']" />
    /// </remarks>
    
FUNCTION VODBSetSelect(siNew AS INT) AS DWORD
    RETURN VoDb.Do ({ =>
    IF siNew == -1
        siNew := (INT) RuntimeState.Workareas:FindEmptyArea(FALSE)
    ELSEIF siNEw == 0
        siNew := (INT) RuntimeState.Workareas:FindEmptyArea(TRUE)
    ENDIF
    IF siNew == 0 || siNew > Workareas.MaxWorkAreas
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
    /// <remarks>VODBSkip() is like DBSkip() but strongly typed.
    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBAppend(System.Boolean)" select="span[@id='LastError']" />
    /// </remarks>
    
FUNCTION VODBSkip(nRecords AS LONG) AS LOGIC
    RETURN VoDb.Do ({ =>
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    RETURN oRDD:Skip(nRecords)
    })
    
    /// <summary>
    /// </summary>
    /// <param name="nRecords"></param>
    /// <param name="scope"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION VODBSkipScope(nRecords AS LONG,scope AS DBSCOPEINFO) AS LOGIC
    RETURN VoDb.Do ({ =>
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    oRDD:SetScope(scope)    
    RETURN oRDD:Skip(nRecords)
    })
    
    
    /// <summary>
    /// </summary>
    /// <param name="nDest"></param>
    /// <param name="fieldNames"></param>
    /// <param name="uCobFor"></param>
    /// <param name="uCobWhile"></param>
    /// <param name="nNext"></param>
    /// <param name="nRecno"></param>
    /// <param name="lRest"></param>
    /// <param name="sortNames"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBSort(nDest AS DWORD,fieldNames AS _FieldNames,uCobFor AS ICodeBlock,uCobWhile AS ICodeBlock, nNext AS OBJECT,nRecno AS OBJECT,lRest AS LOGIC,sortNames AS _FieldNames) AS LOGIC
    RETURN VoDb.Do ({ =>
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    LOCAL info AS DbSortInfo
    info := DbSortInfo{fieldNames:fieldCount, sortNames:fieldCount}
    VoDb.TransSetInfo(oRDD, info:TransInfo, __FUNCTION__, nDest, fieldNames,uCobFor, uCobWhile, nNext, nRecno, lRest)        
    // Now process the fieldnames in the sortnames list
    LOCAL nFld AS INT
    FOR nFld := 0 TO sortNames:fieldCount -1
        // Could be FieldName / ACDB to indicate the sort method
        VAR parts := sortNames:fields[nFld]:Split('/')
        IF parts:Length > 1
            VAR part := parts[1]
            IF part:IndexOf('A') > -1
                info:Items[nFld]:Flags |= DbSortItem.SF_Default
            ENDIF
            IF part:IndexOf('C') > -1
                info:Items[nFld]:Flags |= DbSortItem.SF_Case
            ENDIF
            IF part:IndexOf('D') > -1
                info:Items[nFld]:Flags |= DbSortItem.SF_Descending
            ENDIF
            IF part:IndexOf('B') > -1
                info:Items[nFld]:Flags |= DbSortItem.SF_Ascii
            ENDIF
        ENDIF
        LOCAL iField AS INT
        iField := oRDD:FieldIndex(parts[0])
        IF iField == 0
            RddError.PostArgumentError( __FUNCTION__, EDB_FIELDNAME, nameof(sortNames), 8, <OBJECT>{ sortNames:fields[nFld] } )
        ENDIF
        info:Items[nFld]:FieldNo := iField
    NEXT
    RETURN oRDD:Sort( info )
    })
    
    /// <summary>
    /// </summary>
    /// <param name="nDest"></param>
    /// <param name="fldNames"></param>
    /// <param name="uCobFor"></param>
    /// <param name="uCobWhile"></param>
    /// <param name="nNext"></param>
    /// <param name="nRecno"></param>
    /// <param name="lRest"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>    
FUNCTION VODBTrans(nDest AS DWORD,fldNames AS _FieldNames,uCobFor AS ICodeBlock,uCobWhile AS ICodeBlock, nNext AS OBJECT,nRecno AS OBJECT,lRest AS LOGIC) AS LOGIC
    RETURN VoDb.Do ({ =>
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    LOCAL info AS DbTransInfo
    info := DbTransInfo{fldNames:fieldCount}
    VoDb.TransSetInfo(oRDD, info, __FUNCTION__, nDest, fldNames, uCobFor, uCobWhile, nNext, nRecno, lRest)
    RETURN oRDD:Trans( info )
    
    })
    /// <summary>
    /// </summary>
    /// <param name="nDest"></param>
    /// <param name="fldNames"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>    
FUNCTION VODBTransRec(nDest AS DWORD,fldNames AS _FieldNames) AS LOGIC
    RETURN VoDb.Do ({ =>
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    LOCAL dbti := DbTransInfo{ fldNames:fieldCount} AS DBTRANSINFO
    LOCAL oDest := RUntimeState.Workareas.GetRDD(nDest) AS IRDD
    IF oDest == NULL_OBJECT
        RddError.PostNoTableError(__FUNCTION__)
    ENDIF
    dbti:Destination := oDest
    dbti:ItemCount := fldNames:FieldCount
    IF !VoDb.BuildTrans( dbti, fldNames, oRDD, oDest )
        RddError.PostArgumentError( __FUNCTION__, EDB_DBSTRUCT, nameof(fldNames), 2, <OBJECT>{fldNames} )
    ENDIF
    LOCAL oCanPutRec AS OBJECT
    dbti:Flags |= DBTRANSINFO.Match
    oCanPutRec := oRDD:Info( DBInfo.DBI_CANPUTREC, NULL )
    IF oCanPutRec != NULL .AND. (LOGIC) oCanPutRec
        oCanPutRec := oDest:Info(DBInfo.DBI_CANPUTREC, NULL )
        IF oCanPutRec != NULL .AND. (LOGIC) oCanPutRec
            dbti:Flags |= DBTRANSINFO.PutRec
        ENDIF
    ENDIF
    RETURN oRDD:TransRec( dbti )
    })
    
    /// <summary>
    /// Release all locks for a work area.
    /// </summary>
    /// <param name="uRecno">The ID of the record to be unlocked.  To omit, specify NULL_OBJECT.
    /// This unlocks all locked records or the whole file. </param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    /// <remarks>VODBUnlock() is the same as DBUnlock().
    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBAppend(System.Boolean)" select="span[@id='LastError']" />
    /// </remarks>
    
FUNCTION VODBUnlock(uRecno AS OBJECT) AS LOGIC
    RETURN VoDb.Do ({ =>
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    RETURN oRDD:UnLock(uRecno)    
    })
    
    
    /// <summary>Release all locks for all work areas.</summary>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    /// <remarks>VODBUnlockAll() is the same as DBUnlockAll().
    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBAppend(System.Boolean)" select="span[@id='LastError']" />
    /// </remarks>
    
FUNCTION VODBUnlockAll() AS LOGIC
    RETURN VoDb.Do ({ =>
    RuntimeState.Workareas:UnlockAll()
    RETURN TRUE
    })
    
    /// <summary>
    /// Open a database file.
    /// </summary>
    /// <param name="lNew"></param>
    /// <param name="rddList"></param>
    /// <param name="cName"></param>
    /// <param name="cAlias"></param>
    /// <param name="lShare"></param>
    /// <param name="lReadOnly"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>    
    /// <remarks>VODBUseArea() is like DBUseArea() but strongly typed.
    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBAppend(System.Boolean)" select="span[@id='LastError']" />
    /// </remarks>
    
FUNCTION VODBUseArea(lNew AS LOGIC,rddList AS _RddList,cName AS STRING,cAlias AS STRING,lShare AS LOGIC,lReadOnly AS LOGIC) AS LOGIC
    RETURN VoDb.Do ({ =>
    LOCAL oRdd := NULL  AS RegisteredRDD
    FOREACH VAR name IN rddList:atomRddName
        oRdd := RegisteredRDD.Find(name)
        oRdd:Load()
    NEXT
    IF oRdd != NULL_OBJECT
        RETURN VODBUseArea(lNew, oRdd:RddType, cName, cAlias, lShare, lReadOnly)
    ENDIF
    RETURN FALSE
    })
    
    /// <summary>
    /// Open a database file.
    /// </summary>
    /// <param name="lNew"></param>
    /// <param name="rddName"></param>
    /// <param name="cName"></param>
    /// <param name="cAlias"></param>
    /// <param name="lShare"></param>
    /// <param name="lReadOnly"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    /// <remarks>VODBUseArea() is like DBUseArea() but strongly typed.
    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBAppend(System.Boolean)" select="span[@id='LastError']" />
    /// </remarks>
    
    
FUNCTION VODBUseArea(lNew AS LOGIC,rddName AS STRING,cName AS STRING,cAlias AS STRING,lShare AS LOGIC,lReadOnly AS LOGIC) AS LOGIC
    RETURN VoDb.Do ({ =>
    LOCAL rddType AS Type
    IF ( rddType := VoDb.RddNameToType( rddName ) ) == NULL
        RddError.PostArgumentError( __FUNCTION__, EDB_RDDNOTFOUND, nameof(cName), 3, <OBJECT>{ rddName } )
    ELSE
        RETURN VODBUseArea( lNew, rddType, cName, cAlias, lShare, lReadOnly )
    ENDIF
    RETURN FALSE
    })
    
    
    /// <summary>
    /// Open a database file.
    /// </summary>
    /// <param name="lNew"></param>
    /// <param name="rddType"></param>
    /// <param name="cName"></param>
    /// <param name="cAlias"></param>
    /// <param name="lShare"></param>
    /// <param name="lReadOnly"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    /// <remarks>VODBUseArea() is like DBUseArea() but strongly typed.
    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBAppend(System.Boolean)" select="span[@id='LastError']" />
    /// </remarks>
    
    
FUNCTION VODBUseArea(lNew AS LOGIC,rddType AS System.Type,cName AS STRING,cAlias AS STRING,lShare AS LOGIC,lReadOnly AS LOGIC) AS LOGIC
    RETURN VoDb.Do ({ =>
    LOCAL ret   := FALSE AS LOGIC
    LOCAL area  := 0    AS DWORD
    IF String.IsNullOrEmpty( cName )
        RddError.PostArgumentError( __FUNCTION__, EDB_USE, nameof(cName), 3 , <OBJECT>{NULL})
    ELSE
        ret := TRUE
        NetErr( FALSE )
        cName := cName:Trim() // :ToUpperInvariant()
        
        IF String.IsNullOrEmpty( cAlias )
            TRY
                cAlias := Path.GetFileNameWithoutExtension( cName )
            CATCH  AS ArgumentException
                RddError.PostArgumentError( __FUNCTION__, EDB_USE, nameof(cName), 3, <OBJECT>{cName} ) 
                ret := FALSE
            END TRY   
        ENDIF
        IF lNew
            area := RuntimeState.Workareas:FindEmptyArea(TRUE)
            IF area > Workareas.MaxWorkareas  .OR. area == 0
                ret := FALSE
            ELSE
                RuntimeState.Workareas:CurrentWorkAreaNO := area
            ENDIF
        ELSE
            area := RuntimeState.Workareas:CurrentWorkAreaNO
        ENDIF   
        IF ret
            RuntimeState.Workareas:CloseArea(area)
            LOCAL rdd := VoDb.CreateRDDInstance( rddType, cAlias ) AS IRDD
            
            IF rdd == NULL
                RddError.PostArgumentError( __FUNCTION__, EDB_DRIVERLOAD, nameof(rddType), 3, <OBJECT>{ rddType } )
                ret := FALSE
            ELSEIF ! VoDb.IsAliasUnused( cAlias )
                RddError.PostArgumentError( __FUNCTION__, EDB_DUPALIAS, nameof(cAlias), 4, <OBJECT>{ cAlias } )
                ret := FALSE
            ELSE
                LOCAL dboi := DBOPENINFO{} AS DBOPENINFO
                LOCAL uiArea AS DWORD
                uiArea := RuntimeState.Workareas:CurrentWorkAreaNO
                dboi:FileName     := Path.ChangeExtension( cName, NULL )
                dboi:Extension    := Path.GetExtension( cName )
                dboi:Shared      := lShare
                dboi:ReadOnly    := lReadOnly
                dboi:Alias       := cAlias
                dboi:WorkArea    := uiArea
                rdd:Alias        := cAlias
                ret := RuntimeState.Workareas:SetArea(uiArea, rdd)
                IF (ret)
                    TRY
                        RuntimeState.LastRddError := NULL
                        ret := rdd:Open( dboi )
                    CATCH e AS Exception
                        RuntimeState.LastRddError := e
                        ret := FALSE
                    END TRY
                ENDIF
                IF ! ret
                    RuntimeState.Workareas:CloseArea(uiArea)
                ENDIF
                RuntimeState.Workareas:CurrentWorkAreaNO := uiArea
            ENDIF   
        ENDIF
    ENDIF
    
    RETURN ret
    })
    
    
    /// <summary>Remove all records from the current workarea.</summary>
    /// <returns>TRUE if successful; otherwise, FALSE./// </returns>
    /// <remarks>VODBZap() is like DBZap().
    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBAppend(System.Boolean)" select="span[@id='LastError']" />
    /// </remarks>
    
FUNCTION VODBZap() AS LOGIC
    RETURN VoDb.Do ({ =>
    LOCAL oRDD := RDDHelpers.CWA(__FUNCTION__) AS IRDD
    RETURN oRDD:Zap()
    })

    
    
    
    
FUNCTION _VODBErrInfoPtr AS Exception
    RETURN RuntimeState.LastRDDError
