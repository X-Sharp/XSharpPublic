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

/// <summary>
/// The CoreDb class contains the methods to manipulate workareas. <br/>
/// The class is stateless. The workarea state is stored in the runtimestate.
/// </summary>
CLASS XSharp.CoreDb
    /// <exclude />
    STATIC METHOD CWA(cFunction AS STRING, lThrow := TRUE AS LOGIC) AS IRDD 
        LOCAL oResult AS IRDD
        RuntimeState.LastRddError := NULL
        oResult := RuntimeState.Workareas:CurrentWorkArea
        IF oResult != NULL_OBJECT
            RETURN oResult
        ENDIF
        IF lThrow
            RddError.PostNoTableError(cFunction)
        ENDIF
        RETURN NULL
        /// <exclude />    
    STATIC METHOD CWANum(cFunction AS STRING)  AS DWORD
        VAR oWA := RuntimeState.Workareas:CurrentWorkArea
        IF oWA != NULL
            RETURN oWA:Area
        ENDIF
        RddError.PostNoTableError(cFunction)
        RETURN 0
        /// <exclude />   
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
            FOREACH VAR asm IN loadedAssemblies
                ret := asm:GetType( cRDDName, FALSE, TRUE )
                IF ret != NULL
                    EXIT
                ENDIF   
            NEXT
        ENDIF
        RETURN ret
        /// <exclude />   
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
        i := (INT) src:Info(DbInfo.DBI_GETRECSIZE, NULL)
        j := (INT) dst:Info(DbInfo.DBI_GETRECSIZE, NULL)
        fMatch := (DWORD) dbti:ItemCount == srcFieldCount .AND. srcFieldCount == dstFieldCount .AND. i == j
        j := 0
        FOR i := 0 UPTO lpfn:fieldCount -1
        
            uiSrc := src:FieldIndex( lpfn:fields[i] )       // returns a 1 based index
            uiDst := dst:FieldIndex( lpfn:fields[i]  )
            
            IF uiSrc == 0 || uiDst == 0
                fMatch := FALSE
            ELSE
                
                dbti:Items[j]:Source       := uiSrc 
                dbti:Items[j]:Destination  := uiDst 
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
        
        dbti:ItemCount := j 
        
        RETURN fMatch
        /// <exclude />   
    INTERNAL STATIC METHOD TransSetInfo(oRdd AS IRDD, info AS DbTransInfo, cFunc AS STRING,nDest AS DWORD, fldNames AS _FieldNames,;
                                        uCobFor AS ICodeblock ,uCobWhile AS ICodeblock ,;
                                        nNext AS OBJECT,nRecno AS OBJECT,lRest AS LOGIC) AS VOID
        LOCAL oDest := RuntimeState.Workareas.GetRDD(nDest) AS IRDD
        IF oDest == NULL
            RddError.PostNoTableError(cFunc)
        ENDIF
        info:Destination := oDest
        IF CoreDb.BuildTrans( info, fldNames, oRDD, oDest )
            info:Flags |= DbTransInfoFlags.SameStructure
            LOCAL oCanPutRec AS OBJECT
            oCanPutRec := oRdd:Info(DBInfo.DBI_CANPUTREC, NULL)
            IF oCanPutRec IS LOGIC .AND. (LOGIC) oCanPutRec 
                info:Flags |= DbTransInfoFlags.CanPutRec
            ENDIF
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
        /// <exclude />   
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
        
        
        
        
        /// <summary>
        /// Return the alias of a specified work area as a string.
        /// </summary>
        /// <param name="nArea"></param>
        /// <returns>
        /// </returns>
        /// <remarks><note type="tip">VoDbAlias() and CoreDb.Alias() are aliases</note></remarks>
    STATIC METHOD Alias(nArea AS DWORD) AS STRING
        RETURN CoreDb.Do ({ =>
        RETURN RuntimeState.Workareas:GetAlias(nArea)
        })
        
        /// <summary>
        /// Add a new record.
        /// </summary>
        /// <param name="lReleaseLocks"></param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBAppend() but is strongly typed.  
        /// <span id='LastError' >
        /// <br/>This function, however, does not call the error
        /// handler and will therefore not produce a runtime error message or create an error object if it fails.<br/>
        /// Thus, it may be important to check the return value to determine if the function succeeded.<br/>
        /// The <see cref="P:XSharp.RuntimeState.LastRddError">LastRddError property in the runtimestate</see>  will contain needed information
        /// regarding any error that occurs.</span>
        /// <note type="tip">VoDbAppend() and CoreDb.Append() are aliases</note>
        /// </remarks>
    STATIC METHOD Append(lReleaseLocks AS LOGIC) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
        RETURN oRDD:Append(lReleaseLocks)
        })
        
        /// <summary>Retrieve information about a memo column.</summary>
        /// <param name="nOrdinal"></param>
        /// <param name="nPos"></param>
        /// <param name="oRet"></param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <seealso cref="O:XSharp.RT.Functions.VoDbBlobInfo" >VoDbBlobInfo overloads </seealso>
        /// <seealso cref="O:XSharp.VoDb.BlobInfo" >BlobInfo overloads in VoDb</seealso>
        /// <seealso cref="O:XSharp.CoreDb.BlobInfo" >BlobInfo overloads in CoreDb</seealso>
        
    STATIC METHOD BlobInfo(nOrdinal AS DWORD,nPos AS DWORD,oRet AS OBJECT) AS LOGIC
        RETURN CoreDb.Do ({ =>
        RETURN CoreDb.BlobInfo(nOrdinal, nPos, REF oRet)
        })
        
    /// <inheritdoc cref="M:XSharp.CoreDb.BlobInfo(System.UInt32,System.UInt32,System.Object)" />
    STATIC METHOD BlobInfo(nOrdinal AS DWORD,nPos AS DWORD,oRet REF OBJECT) AS LOGIC
        TRY
            LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
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
        /// current work area, CoreDbBOF() returns TRUE.</returns>
        /// <remarks>This function is like BOF().
        /// <note type="tip">VoDbBof() and CoreDb.Bof() are aliases</note></remarks>
        /// <seealso cref="M:XSharp.RT.Functions.Bof" >Bof Function </seealso>
        /// <seealso cref="M:XSharp.RT.Functions.VoDbBof" >VoDbBof Function in XSharp.VO</seealso>
    STATIC METHOD Bof() AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
        RETURN oRDD:BoF
        })
        
        /// <summary>
        /// </summary>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBBuffRefresh().
        /// <inheritdoc cref="M:XSharp.CoreDb.Append(System.Boolean)" select="span[@id='LastError']" />
        /// <note type="tip">VoDbBuffRefresh() and CoreDb.BuffRefresh() are aliases</note></remarks>
    STATIC METHOD BuffRefresh() AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
        oRDD:RecInfo(0, DbRecordInfo.DBRI_Updated,NULL)
        RETURN TRUE
        })
        /// <summary>
        /// Clear a logical filter condition.
        /// </summary>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBClearFilter().  
        /// <inheritdoc cref="M:XSharp.CoreDb.Append(System.Boolean)" select="span[@id='LastError']" />
        /// <note type="tip">VoDbClearFilter() and CoreDb.ClearFilter() are aliases</note></remarks>
        
    STATIC METHOD ClearFilter() AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
        RETURN oRDD:ClearFilter()
        })
        
        /// <summary>
        /// Clear a locate condition by deleting the locate code block.
        /// </summary>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBClearLocate().  
        /// <inheritdoc cref="M:XSharp.CoreDb.Append(System.Boolean)" select="span[@id='LastError']" />
        /// <note type="tip">VoDbClearLocate() and CoreDb.ClearLocate() are aliases</note></remarks>
    STATIC METHOD ClearLocate() AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
        RETURN oRDD:ClearScope()
        })
        
        /// <summary>Clear any active relations.</summary>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBClearRelation().
        /// <inheritdoc cref="M:XSharp.CoreDb.Append(System.Boolean)" select="span[@id='LastError']" />
        /// <note type="tip">VoDbClearRelation() and CoreDb.ClearRelation() are aliases</note></remarks>
        
    STATIC METHOD ClearRelation() AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
        RETURN oRDD:ClearRel()
        })
        
        /// <summary>Clear the active locate condition.</summary>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBClearScope().  
        /// <inheritdoc cref="M:XSharp.CoreDb.Append(System.Boolean)" select="span[@id='LastError']" />
        /// <note type="tip">VoDbClearScope() and CoreDb.ClearScope() are aliases</note></remarks>
    STATIC METHOD ClearScope() AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
        RETURN oRDD:ClearScope()
        })
        /// <summary>
        /// Close all files in all work areas.
        /// </summary>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBCloseAll().  
        /// <inheritdoc cref="M:XSharp.CoreDb.Append(System.Boolean)" select="span[@id='LastError']" />
        /// <note type="tip">VoDbCloseAll() and CoreDb.CloseAll() are aliases</note></remarks>
    STATIC METHOD CloseAll() AS LOGIC
        RETURN CoreDb.Do ({ =>
        RETURN RuntimeState.Workareas:CloseAll()
        })    
        /// <summary>
        /// Close all files in a work area.
        /// </summary>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBCloseArea().  
        /// <inheritdoc cref="M:XSharp.CoreDb.Append(System.Boolean)" select="span[@id='LastError']" />
        /// <note type="tip">VoDbCloseArea() and CoreDb.CloseArea() are aliases</note></remarks>
    STATIC METHOD CloseArea() AS LOGIC
        RETURN CoreDb.Do ({ =>
            VAR uiNewArea := RuntimeState.Workareas:CurrentWorkAreaNO
            RETURN RuntimeState.Workareas:CloseArea(uiNewArea)
        })
        
        /// <summary>
        /// Flush pending updates in one work area.
        /// </summary>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBCommit().  
        /// <inheritdoc cref="M:XSharp.CoreDb.Append(System.Boolean)" select="span[@id='LastError']" />
        /// <note type="tip">VoDbCommit() and CoreDb.Commit() are aliases</note></remarks>
    STATIC METHOD Commit() AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
        RETURN oRDD:Flush()
        })
        
        /// <summary>
        /// Flush pending updates in all work areas.
        /// </summary>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBCommitAll().  
        /// <inheritdoc cref="M:XSharp.CoreDb.Append(System.Boolean)" select="span[@id='LastError']" />
        /// <note type="tip">VoDbCommitAll() and CoreDb.CommitAll() are aliases</note></remarks>
        
    STATIC METHOD CommitAll() AS LOGIC
        RETURN CoreDb.Do ({ =>
        RETURN RuntimeState.Workareas:CommitAll()
        })
        /// <summary>
        /// Resume a pending locate condition.
        /// </summary>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBContinue().  
        /// <inheritdoc cref="M:XSharp.CoreDb.Append(System.Boolean)" select="span[@id='LastError']" />
        /// <note type="tip">VoDbContinue() and CoreDb.Continue() are aliases</note></remarks>
        
    STATIC METHOD Continue() AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
        RETURN oRDD:Continue()
        })
        
    /// <inheritdoc cref="M:XSharp.CoreDb.Create(System.String,XSharp.RDD.Support.RddFieldInfo[],System.Type,System.Boolean,System.String,System.String,System.Boolean,System.Boolean)" />
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
        
    /// <inheritdoc cref="M:XSharp.CoreDb.Create(System.String,XSharp.RDD.Support.RddFieldInfo[],System.Type,System.Boolean,System.String,System.String,System.Boolean,System.Boolean)" />
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
        /// <seealso cref="O:XSharp.CoreDb.Create" >Create overloads in CoreDb</seealso>
    
    STATIC METHOD Create( cName AS STRING, aStruct AS RddFieldInfo[], rddType AS System.Type, lNew AS LOGIC, cAlias AS STRING, cDelim AS STRING, lKeep AS LOGIC, lJustOpen AS LOGIC ) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL uiOldArea := 0 AS DWORD
        LOCAL uiNewArea := 0 AS DWORD
        LOCAL ret   := FALSE   AS LOGIC
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
            uiNewArea := RuntimeState.Workareas:FindEmptyArea(TRUE)
        ELSE
            // VO Closes the current workarea
            uiNewArea := RuntimeState.Workareas:CurrentWorkAreaNO
            RuntimeState.Workareas:CloseArea(uiNewArea)
        ENDIF
        RuntimeState.Workareas:CurrentWorkAreaNO := uiNewArea
        IF ret .AND. String.IsNullOrEmpty( cAlias ) && ! ( ret := CoreDb.AliasFromFilename( cName, cAlias ) )
            RddError.PostArgumentError( __FUNCTION__, EDB_BADALIAS, nameof(cAlias), 5, <OBJECT>{ cAlias } )
        ENDIF   
        IF ret .AND. ! ( ret := CoreDb.IsAliasUnused( cAlias ) )
            RddError.PostArgumentError( __FUNCTION__, EDB_DUPALIAS, nameof(cAlias), 5, <OBJECT>{ cAlias } )
        ENDIF
        // Now all arguments are valid. So lets create the RDD Object and try to create the file
        LOCAL oRDD AS IRDD
        oRDD := CoreDb.CreateRDDInstance(rddType, cAlias)
        IF oRDD == NULL
            RddError.PostArgumentError( __FUNCTION__, EDB_DRIVERLOAD, nameof(rddType), 3, <OBJECT>{ rddType } )
            ret := FALSE
        ELSEIF ! CoreDb.IsAliasUnused( cAlias )
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
        STATIC METHOD Dbf AS STRING
            LOCAL oRDD := CoreDb.CWA("DBF", FALSE) AS IRDD
            IF oRDD != NULL
                RETURN oRDD:Alias
            ENDIF                            
            RETURN String.Empty
            
        /// <summary>
        /// Mark the current record for deletion.
        /// </summary>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBDelete().  
        /// <inheritdoc cref="M:XSharp.CoreDb.Append(System.Boolean)" select="span[@id='LastError']" />
        /// <note type="tip">VoDbDelete() and CoreDb.Delete() are aliases</note></remarks>
        
        /// <seealso cref="M:XSharp.RT.Functions.DbDelete">DbDelete Function</seealso>
    STATIC METHOD Delete() AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
        RETURN oRDD:Delete()
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
        LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
        RETURN oRDD:Deleted
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
        LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
        RETURN oRDD:EoF
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
        LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
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

    STATIC METHOD FCount() AS DWORD
        LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
        IF (oRDD != NULL)
            RETURN (DWORD) oRDD:FieldCount
        ENDIF
        RETURN 0

        /// <summary>
        /// Retrieve the value of a specified database field.
        /// </summary>
        /// <param name="nPos"></param>
        /// <param name="oRet"></param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    STATIC METHOD FieldGet(nPos AS DWORD,oRet REF OBJECT) AS LOGIC
        TRY
            LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
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
        /// <remarks>This function is like DBFieldInfo().
        /// <inheritdoc cref="M:XSharp.CoreDb.Append(System.Boolean)" select="span[@id='LastError']" />
        /// </remarks>
        /// <seealso cref='O:XSharp.RT.Functions.VoDbFieldInfo' >VoDbFieldInfo overloads </seealso>
        /// <seealso cref='O:XSharp.VoDb.FieldInfo' >FieldInfo overloads in CoreDb</seealso>
        /// <seealso cref='O:XSharp.CoreDb.FieldInfo' >FieldInfo overloads in CoreDb</seealso>
        
    STATIC METHOD FieldInfo(nOrdinal AS DWORD,nPos AS DWORD,oRet AS OBJECT) AS LOGIC
        RETURN CoreDb.Do ({ =>
        RETURN CoreDb.FieldInfo(nOrdinal, nPos, REF oRet)
        })
        
    /// <inheritdoc cref="M:XSharp.CoreDb.FieldInfo(System.UInt32,System.UInt32,System.Object)" />
    STATIC METHOD FieldInfo(nOrdinal AS DWORD,nPos AS DWORD,oRet REF OBJECT) AS LOGIC
        TRY
            LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
            oRet := oRDD:FieldInfo((INT) nPos, (INT) nOrdinal, oRet)
            RETURN TRUE
        CATCH e AS Exception
            RuntimeState.LastRDDError := e
        END TRY
        RETURN FALSE
        
    /// <summary>
    /// Return the name of a field as a string.
    /// </summary>
    /// <param name="dwFieldPos"></param>
    /// <returns>
    /// </returns>

    STATIC METHOD FieldName(dwFieldPos AS DWORD) AS STRING
        LOCAL oRDD := CoreDb.CWA("FieldName") AS IRDD
        IF (oRDD != NULL)
            RETURN oRDD:FieldName((INT) dwFieldPos)
        ENDIF
        RETURN String.Empty   
        /// <summary>
        /// Return the position of a field.
        /// </summary>
        /// <param name="sFieldName"></param>
        /// <returns>
        /// </returns>
    STATIC METHOD FieldPos(sFieldName AS STRING) AS DWORD
        LOCAL oRDD := CoreDb.CWA("FieldPos",FALSE) AS IRDD
        IF (oRDD != NULL)
            RETURN (DWORD) oRDD:FieldIndex(sFieldName) 
        ENDIF
        RETURN 0   

        /// <summary>
        /// Set the value of a specified database field.
        /// </summary>
        /// <param name="nPos"></param>
        /// <param name="xValue"></param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>
        /// <inheritdoc cref="M:XSharp.CoreDb.Append(System.Boolean)" select="span[@id='LastError']" />
        /// </remarks>
    STATIC METHOD FieldPut(nPos AS DWORD,xValue AS OBJECT) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
        RETURN oRDD:PutValue((INT) nPos, xValue)
        })    
        
        /// <summary>Import contents from file into Memo field </summary>
        /// <param name="nPos"></param>
        /// <param name="cFile"></param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBFileGet().
        /// <inheritdoc cref="M:XSharp.CoreDb.Append(System.Boolean)" select="span[@id='LastError']" />
        /// <note type="tip">VoDbFileGet() and CoreDb.FileGet() are aliases</note></remarks>
    STATIC METHOD FileGet(nPos AS DWORD,cFile AS STRING) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
        RETURN oRDD:GetValueFile((INT) nPos, cFile)
        })
        
        /// <summary>Export field contents from Memo field to file</summary>
        /// <param name="nPos"></param>
        /// <param name="cFile"></param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks><note type="tip">VoDbFilePut() and CoreDb.FilePut() are aliases</note></remarks>
    STATIC METHOD FilePut(nPos AS DWORD,cFile AS STRING) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
        RETURN oRDD:PutValueFile((INT) nPos, cFile)
        })
        
        /// <summary>
        /// Return a filter.
        /// </summary>
        /// <returns>
        /// </returns>
        /// <remarks>This function is like DBFilter().
        /// <inheritdoc cref="M:XSharp.CoreDb.Append(System.Boolean)" select="span[@id='LastError']" />
        /// <note type="tip">VoDbFilter() and CoreDb.Filter() are aliases</note></remarks>
        
    STATIC METHOD Filter() AS STRING
        RETURN CoreDb.Do ({ =>
        LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
        RETURN oRDD:FilterText
        })
        
        /// <summary>
        /// Lock an opened and shared database file.
        /// </summary>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBFlock().
        /// <inheritdoc cref="M:XSharp.CoreDb.Append(System.Boolean)" select="span[@id='LastError']" />
        /// <note type="tip">VoDbFlock() and CoreDb.Flock() are aliases</note></remarks>
    STATIC METHOD Flock() AS LOGIC
        RETURN CoreDb.Do ({ =>    
        VAR oRDD := CoreDb.CWA(__FUNCTION__)
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
        /// <remarks>This function is like Found().
        /// <inheritdoc cref="M:XSharp.CoreDb.Append(System.Boolean)" select="span[@id='LastError']" />
        /// <note type="tip">VoDbFound() and CoreDb.Found() are aliases</note></remarks>
        
    STATIC METHOD Found() AS LOGIC
        RETURN CoreDb.Do ({ =>
        VAR oRDD := CoreDb.CWA(__FUNCTION__)
        RETURN oRDD:Found
        })
        
        
        
        /// <summary>
        /// Return the work area number.
        /// </summary>
        /// <returns>
        /// </returns>
        /// <remarks>This function is like DBGetSelect().
        /// <inheritdoc cref="M:XSharp.CoreDb.Append(System.Boolean)" select="span[@id='LastError']" />
        /// <note type="tip">VoDbGetSelect() and CoreDb.GetSelect() are aliases</note></remarks>
        
    STATIC METHOD GetSelect() AS DWORD
        RETURN RuntimeState.Workareas:CurrentWorkareaNo  
        /// <summary>
        /// Move to the last logical record.
        /// </summary>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBGoBottom().  
        /// <inheritdoc cref="M:XSharp.CoreDb.Append(System.Boolean)" select="span[@id='LastError']" />
        /// <note type="tip">VoDbGoBottom() and CoreDb.GoBottom() are aliases</note></remarks>
        
        
    STATIC METHOD GoBottom() AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
        RETURN oRDD:GoBottom()
        })
        /// <summary>
        /// Move to a record specified by record number.
        /// </summary>
        /// <param name="uRecId"></param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    STATIC METHOD Goto(uRecId AS OBJECT) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
        RETURN oRDD:GoToId(uRecID)
        })
        
        /// <summary>
        /// Move to the first logical record.
        /// </summary>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBGoTop().  
        /// <inheritdoc cref="M:XSharp.CoreDb.Append(System.Boolean)" select="span[@id='LastError']" />
        /// <note type="tip">VoDbGoTop() and CoreDb.GoTop() are aliases</note></remarks>
        
        
    STATIC METHOD GoTop() AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
        RETURN oRDD:GoTop()
        })


    STATIC METHOD  Header() AS LONG
        LOCAL oValue := NULL AS OBJECT
	    CoreDb.Info(DBI_GETHEADERSIZE, REF oValue)
        RETURN (LONG) oValue
        
        /// <summary>
        /// Retrieve information about a work area.
        /// </summary>
        /// <param name="nOrdinal"></param>
        /// <param name="ptrRet"></param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <seealso cref='O:XSharp.RT.Functions.VoDbInfo' >VoDbInfo overloads </seealso>
        /// <seealso cref='O:XSharp.VoDb.Info' >Info overloads in VoDb</seealso>
        /// <seealso cref='O:XSharp.CoreDb.Info' >Info overloads in CoreDb</seealso>
    
    STATIC METHOD Info(nOrdinal AS DWORD,oRet AS OBJECT) AS LOGIC
        RETURN CoreDb.Do ({ =>
        RETURN CoreDb.Info(nOrdinal, REF oRet)
        })
        
        
    /// <inheritdoc cref="M:XSharp.CoreDb.Info(System.UInt32,System.Object)" />
    STATIC METHOD Info(nOrdinal AS DWORD,oRet REF OBJECT) AS LOGIC
        TRY
            LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
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
        
        /// <summary>Write values to destination workarea in a JOIN operation</summary>
        /// <param name="nSelect"></param>
        /// <param name="struList"></param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
    STATIC METHOD JoinAppend(nSelect AS DWORD,struList AS _JoinList) AS LOGIC
        RETURN CoreDb.Do ({ =>
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
        /// <remarks> <note type="tip">VoDbLastRec() and CoreDb.LastRec() are aliases</note></remarks>
        
    STATIC METHOD LastRec() AS LONG
        RETURN CoreDb.Do ({ =>
        LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
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
        /// <remarks>This function is like DBLocate() but strongly typed.  
        /// <inheritdoc cref="M:XSharp.CoreDb.Append(System.Boolean)" select="span[@id='LastError']" />
        /// </remarks>
    STATIC METHOD Locate(uCobFor AS ICodeblock,uCobWhile AS ICodeblock,nNext AS LONG,uRecId AS OBJECT,lRest AS LOGIC) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
        LOCAL scopeinfo := DbScopeInfo{} AS DbScopeInfo
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
        
        /// <summary>Return Memo File extension</summary>
        /// <param name="cDriver"></param>
        /// <returns>
        /// </returns>
        /// <remarks> <note type="tip">VoDbMemoExt() and CoreDb.MemoExt() are aliases</note></remarks>
        
    STATIC METHOD MemoExt(cDriver AS STRING) AS STRING
        RETURN CoreDb.Do ({ =>
        LOCAL oRDD := NULL AS IRDD
        IF String.IsNullOrEmpty(cDriver)
            oRDD := CoreDb.CWA(__FUNCTION__, FALSE) 
            IF oRDD == NULL
                cDriver := RuntimeState.DefaultRDD
            ENDIF
        ENDIF
        IF oRDD == NULL
            LOCAL oRegRDD AS RegisteredRDD
            oRegRDD:= RegisteredRDD.Find(cDriver)
            oRegRdd:Load()
            oRDD := CoreDb.CreateRDDInstance( oRegRdd:RddType, "XXTEMPXX" )
        ENDIF
        RETURN (STRING) oRDD:Info(DBI_MEMOEXT, NULL)
        })
        /// <summary>
        /// Return the default index file extension for a work area as defined by the its RDD.
        /// </summary>
        /// <returns>Default extension for the current workarea, or an empty string when no table is open in the current workarea.
        /// </returns>
    STATIC METHOD OrdBagExt() AS STRING
        RETURN CoreDb.Do ({ =>
        LOCAL oRDD := CoreDb.CWA(__FUNCTION__,FALSE) AS IRDD
        IF oRDD == NULL
            // Get an RDD object
            LOCAL oRegRDD AS RegisteredRDD
            oRegRDD:= RegisteredRDD.Find(RuntimeState.DefaultRDD)
            oRegRdd:Load()
            oRDD := CoreDb.CreateRDDInstance( oRegRdd:RddType, "XXTEMPXX" )
        ENDIF
        VAR info := DbOrderInfo{}
        oRDD:OrderInfo(DBOI_BAGEXT, info)
        RETURN (STRING) info:Result
        })
        
        /// <summary>
        /// Set the condition and scope for an order.
        /// </summary>
        /// <param name="ordCondInfo">An object defining the condition and scope information. </param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like OrdCondSet() but strongly typed and the condition information is passed in an object.
        /// <inheritdoc cref="M:XSharp.CoreDb.Append(System.Boolean)" select="span[@id='LastError']" />
        /// </remarks>
    STATIC METHOD OrdCondSet(ordCondInfo AS DbOrderCondInfo) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
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
        /// <remarks>This function is like DbCreateOrder() but strongly typed and the condition information is passed in an object.
        /// <inheritdoc cref="M:XSharp.CoreDb.Append(System.Boolean)" select="span[@id='LastError']" />
        /// </remarks>
        
        
    STATIC METHOD OrdCreate(cBagName AS STRING,oOrder AS OBJECT,cExpr AS STRING,oCodeBlock AS ICodeblock,lUnique AS LOGIC,ordCondInfo AS DbOrderCondInfo) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
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
        /// <remarks>This function is like DbDeleteOrder() but strongly typed.
        /// <inheritdoc cref="M:XSharp.CoreDb.Append(System.Boolean)" select="span[@id='LastError']" />
        /// </remarks>
        
        
    STATIC METHOD OrdDestroy(cBagName AS STRING,oOrder AS OBJECT) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
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
        /// <remarks>This function is like DbOrderInfo() but strongly typed.
        /// <inheritdoc cref="M:XSharp.CoreDb.Append(System.Boolean)" select="span[@id='LastError']" />
        /// </remarks>
        /// <seealso cref='O:XSharp.RT.Functions.VoDbOrderInfo' >VoDbOrderInfo overloads </seealso>
        /// <seealso cref='O:XSharp.VoDb.OrderInfo' >OrderInfo overloads in VoDb</seealso>
        /// <seealso cref='O:XSharp.CoreDb.OrderInfo' >OrderInfo overloads in CoreDb</seealso>
    
        
    STATIC METHOD OrderInfo(nOrdinal AS DWORD,cBagName AS STRING,oOrder AS OBJECT,oValue AS OBJECT) AS LOGIC
        RETURN CoreDb.Do ({ =>
        RETURN CoreDb.OrderInfo(nOrdinal, cBagName, oOrder, REF oValue)
        })
        
    /// <inheritdoc cref="M:XSharp.CoreDb.OrderInfo(System.UInt32,System.String,System.Object,System.Object)" />
    STATIC METHOD OrderInfo(nOrdinal AS DWORD,cBagName AS STRING,oOrder AS OBJECT,oValue REF OBJECT) AS LOGIC
        TRY
            LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
            VAR info := DbOrderInfo{}
            info:BagName := cBagName
            info:Order   := oOrder
            info:Result  := oValue
			oRDD:OrderInfo(nOrdinal, info)
            oValue :=  info:Result
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
        /// <remarks>This function is like DbSetIndex() but strongly typed.
        /// <inheritdoc cref="M:XSharp.CoreDb.Append(System.Boolean)" select="span[@id='LastError']" />
        /// </remarks>
    STATIC METHOD OrdListAdd(cBagName AS STRING,oOrder AS OBJECT) AS LOGIC
        RETURN CoreDb.Do ({ =>
        
        LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
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
        /// <remarks>This function is like DBClearIndex() but strongly typed.
        /// <inheritdoc cref="M:XSharp.CoreDb.Append(System.Boolean)" select="span[@id='LastError']" />
        /// </remarks>
        
        
    STATIC METHOD OrdListClear(cBagName AS STRING,oOrder AS OBJECT) AS LOGIC
        RETURN CoreDb.Do ({ =>
        
        LOCAL oRDD := CoreDb.CWA(__FUNCTION__,FALSE) AS IRDD
        IF oRDD == NULL
            RETURN TRUE // not logical but compatible with VO
        ELSE
            VAR info := DbOrderInfo{}
            info:BagName := cBagName
            IF oOrder == NULL .AND. STRING.IsNullOrEmpty(cBagName)
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
        /// <remarks>This function is like OrdListRebuild().
        /// <inheritdoc cref="M:XSharp.CoreDb.Append(System.Boolean)" select="span[@id='LastError']" />
        /// </remarks>
        
        
    STATIC METHOD OrdListRebuild() AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
        RETURN oRDD:OrderListRebuild()
        })

        /// <summary>
        /// Set the controlling order for a work area.
        /// </summary>
        /// <param name="cBagName"></param>
        /// <param name="oOrder"></param>
        /// <param name="strPreviousOrder"></param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DbSetOrder() but strongly typed.
        /// <inheritdoc cref="M:XSharp.CoreDb.Append(System.Boolean)" select="span[@id='LastError']" />
        /// </remarks>
    STATIC METHOD OrdSetFocus(cBagName AS STRING,oOrder AS OBJECT) AS LOGIC
        TRY
            LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
            VAR info     := DbOrderInfo{}
            info:BagName := cBagName
            info:Order   := oOrder
            RETURN oRDD:OrderListFocus(info)
        CATCH e AS Exception
            RuntimeState.LastRDDError := e
        END TRY
        RETURN FALSE
        
        /// <summary>
        /// Set the controlling order for a work area.
        /// </summary>
        /// <param name="cBagName"></param>
        /// <param name="oOrder"></param>
        /// <param name="strPreviousOrder"></param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DbSetOrder() but strongly typed.
        /// <inheritdoc cref="M:XSharp.CoreDb.Append(System.Boolean)" select="span[@id='LastError']" />
        /// </remarks>
    STATIC METHOD OrdSetFocus(cBagName AS STRING,oOrder AS OBJECT, strPreviousOrder OUT STRING) AS LOGIC
        strPreviousOrder := ""
        TRY
            LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
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
        /// <remarks>This function is like DbPack(). 
        /// <inheritdoc cref="M:XSharp.CoreDb.Append(System.Boolean)" select="span[@id='LastError']" />
        /// <note type="tip">VoDbPack() and CoreDb.Pack() are aliases</note></remarks>
    STATIC METHOD Pack() AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
        RETURN oRDD:Pack()
        })
        
        
        /// <summary>
        /// </summary>
        /// <param name="nRddType"></param>
        /// <returns>
        /// </returns>
        /// <remarks><note type="tip">VoDbRddCount() and CoreDb.RddCount() are aliases</note></remarks>
        
    STATIC METHOD RddCount() AS DWORD
        RETURN CoreDb.Do ({ =>
        RETURN (DWORD) CoreDb.RddList():Length
        })
        /// <summary>Return and optionally change settings controlled directly by the RDD.</summary>
        /// <param name="nOrdinal"></param>
        /// <param name="ptrRet"></param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <seealso cref='O:XSharp.RT.Functions.VoDbRddInfo' >VoDbRddInfo overloads </seealso>
        /// <seealso cref='O:XSharp.VoDb.RddInfo' >RddInfo overloads in VoDb</seealso>
        /// <seealso cref='O:XSharp.CoreDb.RddInfo' >RddInfo overloads in CoreDb</seealso>
        
    STATIC METHOD RddInfo(nOrdinal AS DWORD,oRet REF OBJECT) AS LOGIC
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
        
    /// <inheritdoc cref="M:XSharp.CoreDb.RddInfo(System.UInt32,System.Object@)" />
    STATIC METHOD RddInfo(nOrdinal AS DWORD,oRet AS OBJECT) AS LOGIC
        RETURN CoreDb.Do ({ =>
        RuntimeState.GetValue<OBJECT> ((INT) nOrdinal)
        RuntimeState.SetValue((INT) nOrdinal, oRet)
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
        /// <remarks><note type="tip">VoDbRddName() and CoreDb.RddName() are aliases</note></remarks>
        
    STATIC METHOD RddName() AS STRING
        RETURN CoreDb.Do ({ =>
        LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
        RETURN oRDD:SysName
        })
        /// <summary>
        /// Return and optionally change the default RDD for the application.
        /// </summary>
        /// <param name="cNewRDD"></param>
        /// <returns>
        /// </returns>
        /// <remarks><note type="tip">VoDbRddSetDefault() and CoreDb.RddSetDefault() are aliases</note></remarks>
        
    STATIC METHOD RddSetDefault(cNewRDD AS STRING) AS STRING
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
        /// <remarks>This function is like DBRecall().
        /// <inheritdoc cref="M:XSharp.CoreDb.Append(System.Boolean)" select="span[@id='LastError']" />
        
        /// <note type="tip">VoDbRecall() and CoreDb.Recall() are aliases</note></remarks>
        
    STATIC METHOD Recall() AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
        RETURN oRDD:Recall()
        })


        STATIC METHOD RecSize AS LONG
            LOCAL nSize := NULL AS OBJECT
            CoreDb.Info(DBInfo.DBI_GETRECSIZE, REF nSize)
            RETURN (LONG) nSize

        /// <summary>
        /// Return the current record number.
        /// </summary>
        /// <returns>
        /// </returns>
        /// <remarks><note type="tip">VoDbRecno() and CoreDb.Recno() are aliases</note></remarks>
        
    STATIC METHOD Recno() AS DWORD
        RETURN CoreDb.Do ({ =>
        LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
        RETURN (DWORD) oRDD:Recno
        })
        
        /// <summary>Get the contents of the current record as an array of bytes</summary>
        /// <returns>
        /// </returns>
        /// <remarks><note type="tip">VoDbRecordGet() and CoreDb.RecordGet() are aliases</note></remarks>
        
    STATIC METHOD RecordGet() AS BYTE[]
        RETURN CoreDb.Do ({ =>
        LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
        RETURN oRDD:GetRec()
        }) 
        
        /// <summary>
        /// Retrieve information about a record.
        /// </summary>
        /// <param name="nOrdinal"></param>
        /// <param name="oRecID"></param>
        /// <param name="oRet"></param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <seealso cref='O:XSharp.RT.Functions.VoDbRecordInfo'  >VoDbRecordInfo overloads</seealso>
        /// <seealso cref='O:XSharp.VoDb.RecordInfo'  >RecordInfo overloads in VoDb</seealso>
        /// <seealso cref='O:XSharp.CoreDb.RecordInfo'  >RecordInfo overloads in CoreDb</seealso>
        
    STATIC METHOD RecordInfo(nOrdinal AS DWORD,oRecID AS OBJECT,oRet AS OBJECT) AS LOGIC
        RETURN CoreDb.Do ({ =>
        RETURN CoreDb.RecordInfo(nOrdinal, oRecID, REF oRet)
        }) 
        
    /// <inheritdoc cref="M:XSharp.CoreDb.RecordInfo(System.UInt32,System.Object,System.Object)" />
    STATIC METHOD RecordInfo(nOrdinal AS DWORD,oRecID AS OBJECT,oRet REF OBJECT) AS LOGIC
        TRY
            LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
            oRet := oRDD:RecInfo( (INT) nOrdinal,oRecID, oRet )
            RETURN TRUE
        CATCH e AS Exception
            RuntimeState.LastRDDError := e
        END TRY
        
        RETURN FALSE       
        
        /// <summary>Update the current record from an array of bytes</summary>
        /// <param name="aRecord"></param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks><note type="tip">VoDbRecordPut() and CoreDb.RecordPut() are aliases</note></remarks>
        
    STATIC METHOD RecordPut(aRecord AS BYTE[]) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
        RETURN oRDD:PutRec(aRecord)
        })
        
        /// <summary>
        /// Return the linking expression of a specified relation.
        /// </summary>
        /// <param name="nPos"></param>
        /// <param name="pszRel"></param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBSetRelation().
        /// <inheritdoc cref="M:XSharp.CoreDb.Append(System.Boolean)" select="span[@id='LastError']" />
        /// </remarks>
        
    STATIC METHOD Relation(nPos AS DWORD,sRel REF STRING) AS LOGIC
        TRY
            LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
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
        /// <remarks>This function is like DBRlock() but strongly typed.
        /// <inheritdoc cref="M:XSharp.CoreDb.Append(System.Boolean)" select="span[@id='LastError']" />
        /// </remarks>
        
    STATIC METHOD RLock(uRecId AS OBJECT) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
        LOCAL lockInfo AS DbLockInfo
        lockInfo := DbLockInfo{}
        lockInfo:RecId := uRecID
        IF uRecID == NULL
            lockInfo:@@METHOD  := DbLockInfo.LockMethod.Exclusive
        ELSE
            lockInfo:@@METHOD  := DbLockInfo.LockMethod.Multiple
        ENDIF
        RETURN oRDD:Lock(lockInfo)
        })
        /// <summary>
        /// Return the work area number of a relation.
        /// </summary>
        /// <param name="nPos"></param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBRSelect() but strongly typed.
        /// <inheritdoc cref="M:XSharp.CoreDb.Append(System.Boolean)" select="span[@id='LastError']" />
        /// <note type="tip">VoDbRSelect() and CoreDb.RSelect() are aliases</note></remarks>
        
        
    STATIC METHOD RSelect(nPos AS DWORD) AS DWORD
        RETURN CoreDb.Do ({ =>
        LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
        RETURN oRDD:RelArea(nPos)
        })
        /// <summary>
        /// Move to the record having the specified key value.
        /// </summary>
        /// <param name="oValue"></param>
        /// <param name="lSoftSeek"></param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBSeek() but strongly typed.
        /// <inheritdoc cref="M:XSharp.CoreDb.Append(System.Boolean)" select="span[@id='LastError']" />
        /// </remarks>
        
    STATIC METHOD Seek(oValue AS OBJECT,lSoftSeek AS LOGIC, lLast AS LOGIC) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
        VAR info 		:= DbSeekInfo{}
        info:Value 		:= oValue          
        info:SoftSeek 	:= lSoftSeek
        info:Last		:= lLast 
        RETURN oRDD:Seek(info)
        })
        
        
        /// <summary>
        /// Select a new work area and retrieve the current work area.
        /// </summary>
        /// <param name="nNew"></param>
        /// <param name="nOld"></param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBSelect() but strongly typed.
        /// <inheritdoc cref="M:XSharp.CoreDb.Append(System.Boolean)" select="span[@id='LastError']" />
        /// <note type="tip">VoDbSelect() and CoreDb.Select() are aliases</note></remarks>
        
    STATIC METHOD Select(nNew AS DWORD,nOld REF DWORD ) AS LOGIC
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
        /// <remarks>This function is like DBSetFilter() but strongly typed.
        /// <inheritdoc cref="M:XSharp.CoreDb.Append(System.Boolean)" select="span[@id='LastError']" />
        /// </remarks>
        
    STATIC METHOD SetFilter(oBlock AS ICodeblock,cFilter AS STRING) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
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
        /// <remarks>This function is like DBSetFound().
        /// <note type="tip">VoDbSetFound() and CoreDb.SetFound() are aliases</note></remarks>
        
        
    STATIC METHOD SetFound(lFound AS LOGIC) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
        oRDD:Found := lFound
        RETURN TRUE
        })
        /// <summary>
        /// Specify the code block for a locate condition.
        /// </summary>
        /// <param name="oBlock"></param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBSetLocate() but strongly typed.
        /// <inheritdoc cref="M:XSharp.CoreDb.Append(System.Boolean)" select="span[@id='LastError']" />
        /// </remarks>
        
    STATIC METHOD SetLocate(oBlock AS ICodeblock) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
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
        /// <remarks>This function is like DBSetRelation() but strongly typed.
        /// <inheritdoc cref="M:XSharp.CoreDb.Append(System.Boolean)" select="span[@id='LastError']" />
        /// </remarks>
        
    STATIC METHOD SetRelation(cAlias AS STRING,oKey  AS ICodeblock,cKey AS STRING) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
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
        
        
        /// <summary>Set the locate condition.</summary>
        /// <param name="scope"></param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks><note type="tip">VoDbSetFound() and CoreDb.SetFound() are aliases</note></remarks>
        
    STATIC METHOD SetScope(scope AS DbScopeInfo) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
        RETURN oRDD:SetScope(scope)
        })
        /// <summary>
        /// Select a new work area.
        /// </summary>
        /// <param name="siNew">The number of the new work area. 0 indicates the first available free workarea. -1 indicates the last available free workarea.</param>
        /// <returns>The newly selected work area.</returns>
        /// <remarks>This function is like DBSetSelect() but strongly typed.
        /// <inheritdoc cref="M:XSharp.CoreDb.Append(System.Boolean)" select="span[@id='LastError']" />
        /// <note type="tip">VoDbSetSelect() and CoreDb.SetSelect() are aliases</note></remarks>
        
    STATIC METHOD SetSelect(siNew AS INT) AS DWORD
        RETURN CoreDb.Do ({ =>
        IF siNew == -1
            siNew := (INT) RuntimeState.Workareas:FindEmptyArea(FALSE)
        ELSEIF siNEw <= 0
            siNew := (INT) RuntimeState.Workareas:FindEmptyArea(TRUE)
        ENDIF
        IF siNew > Workareas.MaxWorkAreas
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
        /// <inheritdoc cref="M:XSharp.CoreDb.Append(System.Boolean)" select="span[@id='LastError']" />
        /// <note type="tip">VoDbSkip() and CoreDb.Skip() are aliases</note></remarks>
        
        
    STATIC METHOD Skip(nRecords AS LONG) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
        RETURN oRDD:Skip(nRecords)
        })
        
        /// <summary>Position the cursor relative to its current position within the current scope.</summary>
        /// <param name="nRecords"></param>
        /// <param name="scope"></param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks><note type="tip">VoDbSkipScope() and CoreDb.SkipScope() are aliases</note></remarks>
        
    STATIC METHOD SkipScope(nRecords AS LONG,scope AS DBSCOPEINFO) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
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
    STATIC METHOD Sort(nDest AS DWORD,fieldNames AS _FieldNames,uCobFor AS ICodeblock,uCobWhile AS ICodeblock, nNext AS OBJECT,nRecno AS OBJECT,lRest AS LOGIC,sortNames AS _FieldNames) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
        LOCAL info AS DbSortInfo
        info := DbSortInfo{fieldNames:fieldCount, sortNames:fieldCount}
        CoreDb.TransSetInfo(oRDD, info:TransInfo, __FUNCTION__, nDest, fieldNames,uCobFor, uCobWhile, nNext, nRecno, lRest)        
        // Now process the fieldnames in the sortnames list
        LOCAL nFld AS INT
        FOR nFld := 0 TO sortNames:fieldCount -1
            // Could be FieldName / ACDB to indicate the sort method
            VAR parts := sortNames:fields[nFld]:Split('/')
            IF parts:Length > 1
                VAR part := parts[1]
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
            iField := oRDD:FieldIndex(parts[0])
            IF iField == 0
                RddError.PostArgumentError( __FUNCTION__, EDB_FIELDNAME, nameof(sortNames), 8, <OBJECT>{ sortNames:fields[nFld] } )
            ENDIF
            info:Items[nFld]:FieldNo := iField
        NEXT
        RETURN oRDD:Sort( info )
        })
        
        /// <summary>
        /// Select a new work area by specifying its alias as a string and return the number of the current work area.
        /// </summary>
        /// <param name="sAlias"></param>
        /// <returns>
        /// </returns>
    STATIC METHOD SymSelect(sAlias AS STRING) AS INT
        LOCAL ret AS DWORD
        IF String.IsNullOrEmpty(sAlias)
            ret := RuntimeState.CurrentWorkarea
        ELSE
            ret := RuntimeState.Workareas:FindAlias( sAlias )
            IF ret != 0
                CoreDB.SetSelect( (INT) ret )
            ENDIF
        ENDIF
        RETURN (INT) ret
        /// <summary>Copy one or more rows from one work area to another.</summary>
        /// <param name="nDest"></param>
        /// <param name="fldNames"></param>
        /// <param name="uCobFor"></param>
        /// <param name="uCobWhile"></param>
        /// <param name="nNext"></param>
        /// <param name="nRecno"></param>
        /// <param name="lRest"></param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>    
    STATIC METHOD Trans(nDest AS DWORD,fldNames AS _FieldNames,uCobFor AS ICodeblock,uCobWhile AS ICodeblock, nNext AS OBJECT,nRecno AS OBJECT,lRest AS LOGIC) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
        LOCAL info AS DbTransInfo
        info := DbTransInfo{fldNames:fieldCount}
        CoreDb.TransSetInfo(oRDD, info, __FUNCTION__, nDest, fldNames, uCobFor, uCobWhile, nNext, nRecno, lRest)
        RETURN oRDD:Trans( info )
        
        })
        /// <summary>Copy a single row from one work area to another.</summary>
        /// <param name="nDest"></param>
        /// <param name="fldNames"></param>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks><note type="tip">VoDbTransRec() and CoreDb.TransRec() are aliases</note></remarks>
        
    STATIC METHOD TransRec(nDest AS DWORD,fldNames AS _FieldNames) AS LOGIC
        RETURN CoreDb.Do ({ =>
            LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
            LOCAL dbti := DbTransInfo{ fldNames:fieldCount} AS DBTRANSINFO
            LOCAL oDest := RUntimeState.Workareas.GetRDD(nDest) AS IRDD
            IF oDest == NULL_OBJECT
                RddError.PostNoTableError(__FUNCTION__)
            ENDIF
            dbti:Destination := oDest
            dbti:ItemCount := fldNames:FieldCount
            IF CoreDb.BuildTrans( dbti, fldNames, oRDD, oDest )
                dbti:Flags |= DbTransInfoFlags.SameStructure
                LOCAL oCanPutRec AS OBJECT
                oCanPutRec := oRDD:Info( DBInfo.DBI_CANPUTREC, NULL )
                IF oCanPutRec != NULL .AND. (LOGIC) oCanPutRec
                    oCanPutRec := oDest:Info(DBInfo.DBI_CANPUTREC, NULL )
                    IF oCanPutRec != NULL .AND. (LOGIC) oCanPutRec
                        dbti:Flags |= DbTransInfoFlags.CanPutRec
                    ENDIF
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
        /// <remarks>This function is like DBUnlock().
        /// <inheritdoc cref="M:XSharp.CoreDb.Append(System.Boolean)" select="span[@id='LastError']" />
        /// </remarks>
        
    STATIC METHOD Unlock(uRecno AS OBJECT) AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
        RETURN oRDD:UnLock(uRecno)    
        })
        
        
        /// <summary>Release all locks for all work areas.</summary>
        /// <returns>TRUE if successful; otherwise, FALSE.</returns>
        /// <remarks>This function is like DBUnlockAll().
        /// <inheritdoc cref="M:XSharp.CoreDb.Append(System.Boolean)" select="span[@id='LastError']" />
        /// <note type="tip">VoDbUnlockAll() and CoreDb.UnlockAll() are aliases</note></remarks>
    STATIC METHOD UnlockAll() AS LOGIC
        RETURN CoreDb.Do ({ =>
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
        /// <remarks>This function is like DBUseArea() but strongly typed.
        /// <inheritdoc cref="M:XSharp.CoreDb.Append(System.Boolean)" select="span[@id='LastError']" />
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
        /// <remarks>This function is like DBUseArea() but strongly typed.
        /// <inheritdoc cref="M:XSharp.CoreDb.Append(System.Boolean)" select="span[@id='LastError']" />
        /// <note type="tip">VoDbUseArea() and CoreDb.UseArea() are aliases</note></remarks>
        
        
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
        /// <remarks>This function is like DBUseArea() but strongly typed.
        /// <inheritdoc cref="M:XSharp.CoreDb.Append(System.Boolean)" select="span[@id='LastError']" />
        /// <note type="tip">VoDbUseArea() and CoreDb.UseArea() are aliases</note></remarks>
        
    STATIC METHOD UseArea(lNew AS LOGIC,rddType AS System.Type,cName AS STRING,cAlias AS STRING,lShare AS LOGIC,lReadOnly AS LOGIC) AS LOGIC
        RETURN CoreDb.Do ({ =>
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
                LOCAL rdd := CoreDb.CreateRDDInstance( rddType, cAlias ) AS IRDD
                
                IF rdd == NULL
                    RddError.PostArgumentError( __FUNCTION__, EDB_DRIVERLOAD, nameof(rddType), 3, <OBJECT>{ rddType } )
                    ret := FALSE
                ELSEIF ! CoreDb.IsAliasUnused( cAlias )
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
        /// <remarks>This function is like DBZap().
        /// <inheritdoc cref="M:XSharp.CoreDb.Append(System.Boolean)" select="span[@id='LastError']" />
        /// <note type="tip">VoDbZap() and CoreDb.Zap() are aliases</note></remarks>
        
    STATIC METHOD Zap() AS LOGIC
        RETURN CoreDb.Do ({ =>
        LOCAL oRDD := CoreDb.CWA(__FUNCTION__) AS IRDD
        RETURN oRDD:Zap()
        })
        
        
        /// <summary>Return exception object from last RDD operation.</summary>    
    STATIC METHOD  _ErrInfoPtr AS Exception
        IF RuntimeState.LastRDDError == NULL
            RuntimeState.LastRDDError := Exception {"No RDD Exception found in the runtime state"}
        ENDIF
        RETURN RuntimeState.LastRDDError
        
END CLASS    
