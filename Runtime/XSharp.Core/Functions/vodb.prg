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
/// Return the alias of a specified work area as a string.
/// </summary>
/// <param name="nArea"></param>
/// <returns>
/// </returns>
FUNCTION VODBAlias(nArea AS DWORD) AS STRING
    TRY
        RETURN RuntimeState.Workareas:GetAlias(nArea)
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    RETURN String.Empty
    
    /// <summary>
    /// Add a new record.
    /// </summary>
    /// <param name="lReleaseLocks"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBAppend(lReleaseLocks AS LOGIC) AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBAppend") AS IRDD
        RETURN oRDD:Append(lReleaseLocks)
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    
    RETURN FALSE   
    
    /// <summary>
    /// </summary>
    /// <param name="nOrdinal"></param>
    /// <param name="nPos"></param>
    /// <param name="ptrRet"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBBlobInfo(nOrdinal AS DWORD,nPos AS DWORD,ptrRet REF OBJECT) AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBBlobInfo") AS IRDD
        ptrRet := oRDD:BlobInfo(nOrdinal, nPos)
        RETURN TRUE
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    RETURN FALSE
    /// <summary>
    /// Determine when beginning-of-file is encountered.
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBBof() AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBBof") AS IRDD
        RETURN oRDD:BoF
        
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    RETURN FALSE
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBBuffRefresh() AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBBuffRefresh") AS IRDD
        oRDD:RecInfo(0, DbRecordInfo.DBRI_Updated,NULL)
        RETURN TRUE
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    RETURN FALSE    
    /// <summary>
    /// Clear a logical filter condition.
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBClearFilter() AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBClearFilter") AS IRDD
        RETURN oRDD:ClearFilter()
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    RETURN FALSE   
    
    /// <summary>
    /// Clear a locate condition by deleting the locate code block.
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBClearLocate() AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBClearLocate") AS IRDD
        RETURN oRDD:ClearScope()
        
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    RETURN FALSE   
    
    /// <summary>
    /// Clear any active relations.
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBClearRelation() AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBClearRelation") AS IRDD
        RETURN oRDD:ClearRel()
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    RETURN FALSE   
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBClearScope() AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBClearScope") AS IRDD
        RETURN oRDD:ClearScope()
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    RETURN FALSE
    /// <summary>
    /// Close all files in all work areas.
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBCloseAll() AS LOGIC
    TRY
        RETURN RuntimeState.Workareas:CloseAll()
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    RETURN FALSE
    
    /// <summary>
    /// Close all files in a work area.
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBCloseArea() AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBFlock") AS IRDD
        RETURN oRDD:Close()
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    RETURN FALSE   
    
    /// <summary>
    /// Flush pending updates in one work area.
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBCommit() AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBCommit") AS IRDD
        RETURN oRDD:Flush()
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    RETURN FALSE   
    
    /// <summary>
    /// Flush pending updates in all work areas.
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBCommitAll() AS LOGIC
    TRY
        RETURN RuntimeState.Workareas:CommitAll()
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    RETURN FALSE 
    /// <summary>
    /// Resume a pending locate condition.
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBContinue() AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBContinue") AS IRDD
        RETURN oRDD:Continue()
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    RETURN FALSE   
    
    /// <summary>
    /// Create new file through the specified RDDs
    /// </summary>
    /// <param name="cName">Name of the file to create. When no extension is specified then the default extension for the RDD will be used.</param>
    /// <param name="aStruct">Structure to use when creating the file.</param>
    /// <param name="rddList">List of RDDs to use when creating the file</param>
    /// <param name="lNew">TRUE opens the database file in a new work area (first available).  FALSE opens it in the current work area.  <lNew> is useful only when <lOpen> has a value of TRUE. The default is FALSE.</param>
    /// <param name="cAlias">The alias to be associated with the work area where the file is opened.  Within a single thread, X# will not accept duplicate aliases.  cAlias is useful only when lOpen has a value of TRUE.  The default alias is the filename without extension</param>
    /// <param name="cDelim">The delimiter for fields within a delimited database file. The default is a NULL string </param>
    /// <param name="lKeep">TRUE specifies that the file should remain open after creating. FALSE closes the file.</param>
    /// <param name="lJustOpen">TRUE specifies that an existing database file be opened; FALSE specifies that that a new database file be opened.  The default is FALSE.  This can be used to open existing SDF and delimited files, which do not have a structure in the header — in which case, an empty aStruct should be used.</param>
    /// <returns>TRUE when succesfull, otherwise FALSE. When an error has occurred then you can retrieve that error from RuntimeState.LastRddError.</returns>
FUNCTION VODBCreate( cName AS STRING, aStruct AS IList<RddFieldInfo>, cRddName AS STRING, lNew AS LOGIC, cAlias AS STRING, cDelim AS STRING, lKeep AS LOGIC, lJustOpen AS LOGIC ) AS LOGIC
    TRY
        LOCAL rddType AS Type
        IF ( rddType := _VoDbRddNameToType( cRddName ) ) == NULL
            PostArgumentError( "VODBCreate", EDB_RDDNOTFOUND, nameof(cRddName), 3, <OBJECT>{ cRddName } )
        ELSE
            RETURN VODBCreate( cName, aStruct, rddType, lNew, cAlias, cDelim, lKeep, lJustOpen )
        ENDIF
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    RETURN FALSE
    
    /// <summary>
    /// Create new file through the specified RDDs
    /// </summary>
    /// <param name="cName">Name of the file to create. When no extension is specified then the default extension for the RDD will be used.</param>
    /// <param name="aStruct">Structure to use when creating the file.</param>
    /// <param name="rddList">List of RDDs to use when creating the file</param>
    /// <param name="lNew">TRUE opens the database file in a new work area (first available).  FALSE opens it in the current work area.  <lNew> is useful only when <lOpen> has a value of TRUE. The default is FALSE.</param>
    /// <param name="cAlias">The alias to be associated with the work area where the file is opened.  Within a single thread, X# will not accept duplicate aliases.  cAlias is useful only when lOpen has a value of TRUE.  The default alias is the filename without extension</param>
    /// <param name="cDelim">The delimiter for fields within a delimited database file. The default is a NULL string </param>
    /// <param name="lKeep">TRUE specifies that the file should remain open after creating. FALSE closes the file.</param>
    /// <param name="lJustOpen">TRUE specifies that an existing database file be opened; FALSE specifies that that a new database file be opened.  The default is FALSE.  This can be used to open existing SDF and delimited files, which do not have a structure in the header — in which case, an empty aStruct should be used.</param>
    /// <returns>TRUE when succesfull, otherwise FALSE. When an error has occurred then you can retrieve that error from RuntimeState.LastRddError.</returns>
FUNCTION VODBCreate( cName AS STRING, aStruct AS IList<RddFieldInfo>, rddList AS _RddList, lNew AS LOGIC, cAlias AS STRING, cDelim AS STRING, lKeep AS LOGIC, lJustOpen AS LOGIC ) AS LOGIC
    TRY
        LOCAL oRdd := NULL  AS RegisteredRDD
        FOREACH VAR name IN rddList:atomRddName
            oRdd := RegisteredRDD.Find(name)
            oRdd:Load()
        NEXT
        IF (oRdd != NULL_OBJECT)
            RETURN VODBCreate(cName, aStruct, oRdd:RddType, lNew, cAlias, cDelim, lKeep, lJustOpen)
        ENDIF
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    RETURN FALSE
    
    /// <summary>
    /// Create new file through the specified RDDs
    /// </summary>
    /// <param name="cName">Name of the file to create. When no extension is specified then the default extension for the RDD will be used.</param>
    /// <param name="aStruct">Structure to use when creating the file.</param>
    /// <param name="rddType">Type of the class that must be used to work with the RDD.</param>
    /// <param name="lNew">TRUE opens the database file in a new work area (first available).  FALSE opens it in the current work area.  <lNew> is useful only when <lOpen> has a value of TRUE. The default is FALSE.</param>
    /// <param name="cAlias">The alias to be associated with the work area where the file is opened.  Within a single thread, X# will not accept duplicate aliases.  cAlias is useful only when lOpen has a value of TRUE.  The default alias is the filename without extension</param>
    /// <param name="cDelim">The delimiter for fields within a delimited database file. The default is a NULL string </param>
    /// <param name="lKeep">TRUE specifies that the file should remain open after creating. FALSE closes the file.</param>
    /// <param name="lJustOpen">TRUE specifies that an existing database file be opened; FALSE specifies that that a new database file be opened.  The default is FALSE.  This can be used to open existing SDF and delimited files, which do not have a structure in the header — in which case, an empty aStruct should be used.</param>
    /// <returns>TRUE when succesfull, otherwise FALSE. When an error has occurred then you can retrieve that error from RuntimeState.LastRddError.</returns>
FUNCTION VODBCreate( cName AS STRING, aStruct AS IList<RddFieldInfo>, rddType AS System.Type, lNew AS LOGIC, cAlias AS STRING, cDelim AS STRING, lKeep AS LOGIC, lJustOpen AS LOGIC ) AS LOGIC
    TRY
        RETURN VoDbCreate(cName, aStruct:ToArray(), rddType, lNew, cAlias, cDelim, lKeep, lJustOpen)
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    RETURN FALSE
    /// <summary>
    /// Create new file through the specified RDDs
    /// </summary>
    /// <param name="cName">Name of the file to create. When no extension is specified then the default extension for the RDD will be used.</param>
    /// <param name="aStruct">Structure to use when creating the file.</param>
    /// <param name="rddType">Type of the class that must be used to work with the RDD.</param>
    /// <param name="lNew">TRUE opens the database file in a new work area (first available).  FALSE opens it in the current work area.  <lNew> is useful only when <lOpen> has a value of TRUE. The default is FALSE.</param>
    /// <param name="cAlias">The alias to be associated with the work area where the file is opened.  Within a single thread, X# will not accept duplicate aliases.  cAlias is useful only when lOpen has a value of TRUE.  The default alias is the filename without extension</param>
    /// <param name="cDelim">The delimiter for fields within a delimited database file. The default is a NULL string </param>
    /// <param name="lKeep">TRUE specifies that the file should remain open after creating. FALSE closes the file.</param>
    /// <param name="lJustOpen">TRUE specifies that an existing database file be opened; FALSE specifies that that a new database file be opened.  The default is FALSE.  This can be used to open existing SDF and delimited files, which do not have a structure in the header — in which case, an empty aStruct should be used.</param>
    /// <returns>TRUE when succesfull, otherwise FALSE. When an error has occurred then you can retrieve that error from RuntimeState.LastRddError.</returns>
FUNCTION VODBCreate( cName AS STRING, aStruct AS RddFieldInfo[], rddType AS System.Type, lNew AS LOGIC, cAlias AS STRING, cDelim AS STRING, lKeep AS LOGIC, lJustOpen AS LOGIC ) AS LOGIC
    TRY
        LOCAL uiOldArea := 0 AS DWORD
        LOCAL uiNewArea := 0 AS DWORD
        LOCAL ret   := FALSE   AS LOGIC
        RuntimeState.LastRddError := NULL
        IF String.IsNullOrEmpty( cName )
            PostArgumentError( "VODBCreate", EDB_USE, nameof(cName), 1, <OBJECT>{ cName } )
        ELSEIF aStruct == NULL
            PostArgumentError( "VODBCreate", EDB_USE, nameof(aStruct), 2 ,NULL)
        ELSEIF lNew && ! ( ret := VODBSelect( 0, REF uiOldArea ) )
            PostError( "VODBCreate", EG_CREATE, EDB_NOAREAS )
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
        IF ret && String.IsNullOrEmpty( cAlias ) && ! ( ret := _VoDbAliasFromFilename( cName, cAlias ) )
            PostArgumentError( "VODBCreate", EDB_BADALIAS, nameof(cAlias), 5, <OBJECT>{ cAlias } )
        ENDIF   
        IF ret && ! ( ret := _VoDbIsAliasUnused( cAlias ) )
            PostArgumentError( "VODBCreate", EDB_DUPALIAS, nameof(cAlias), 5, <OBJECT>{ cAlias } )
        ENDIF
        // Now all arguments are valid. So lets create the RDD Object and try to create the file
        LOCAL oRDD AS IRDD
        oRDD := _VoDbCreateRDDInstance(rddType, cAlias)
        IF oRDD == NULL
            PostArgumentError( "VODBCreate", EDB_DRIVERLOAD, nameof(rddType), 3, <OBJECT>{ rddType } )
            ret := FALSE
        ELSEIF ! _VoDbIsAliasUnused( cAlias )
            PostArgumentError( "VODBCreate", EDB_DUPALIAS, nameof(cAlias), 4, <OBJECT>{ cAlias } )
            ret := FALSE
        ELSE
            ret := RuntimeState.Workareas:SetArea(uiNewArea, oRDD)
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
                dboi:WorkArea  := RuntimeState.Workareas:FindEmptyArea(TRUE)
                oRDD:Alias := cAlias
                ret := RuntimeState.Workareas:SetArea(uiNewArea, oRdd)
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
    CATCH e AS Exception
        RuntimeState.LastRddError := e
    END TRY
    RETURN FALSE
    
    /// <summary>
    /// Mark the current record for deletion.
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBDelete() AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBDelete") AS IRDD
        RETURN oRDD:Delete()
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    RETURN FALSE
    
    /// <summary>
    /// Return the deleted status of the current record.
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBDeleted() AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBDeleted") AS IRDD
        RETURN oRDD:Deleted
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    RETURN FALSE
    
    /// <summary>
    /// Determine when end-of-file is encountered.
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBEof() AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBEof") AS IRDD
        RETURN oRDD:EoF
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    RETURN FALSE
    
    /// <summary>
    /// Evaluate a code block for each record that matches a specified scope and/or condition.
    /// </summary>
    /// <param name="uBlock"></param>
    /// <param name="uCobFor"></param>
    /// <param name="uCobWhile"></param>
    /// <param name="uNext"></param>
    /// <param name="nRecno"></param>
    /// <param name="lRest"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBEval(uBlock AS ICodeBlock,uCobFor AS ICodeBlock,uCobWhile AS ICodeBlock,uNext AS OBJECT,nRecno AS OBJECT,lRest AS LOGIC) AS LOGIC
    TRY
        LOCAL nNext AS LONG
        IF uBlock == NULL
            THROW Error.ArgumentError("VODBEVal", nameof(uBlock),1, <OBJECT>{uBlock})
        ELSEIF uCobFor == NULL
            THROW Error.ArgumentError("VODBEVal", nameof(uCobFor),2, <OBJECT>{uCobFor})        
        ELSEIF uCobWhile == NULL    
            THROW Error.ArgumentError("VODBEVal", nameof(uCobWhile),3, <OBJECT>{uCobWhile})
        ELSE
            TRY
                IF uNext != NULL
                    nNext := Convert.ToInt32(uNext)
                ELSE
                    nNext := 0
                ENDIF
            CATCH AS Exception
                THROW Error.ArgumentError("VODBEVal", nameof(uNext),4, <OBJECT>{uNext})
            END TRY
        ENDIF
        LOCAL oRDD := RDDHelpers.CWA("VODBEval") AS IRDD
        LOCAL oInfo AS DbEvalInfo
        oInfo := DbEvalInfo{}
        oInfo:Block := uBlock
        oInfo:ScopeInfo:ForBlock := uCobFor
        oInfo:ScopeInfo:WhileBlock := uCobWhile
        oInfo:ScopeInfo:NextCount  := nNext
        oInfo:ScopeInfo:RecId      := nRecno
        oInfo:ScopeInfo:Rest       := lRest
        RETURN oRDD:DbEval(oInfo)
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    RETURN FALSE
    
    /// <summary>
    /// Retrieve the value of a specified database field.
    /// </summary>
    /// <param name="nPos"></param>
    /// <param name="ptrRet"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBFieldGet(nPos AS DWORD,ptrRet REF OBJECT) AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBFieldGet") AS IRDD
        ptrRet := oRDD:GetValue((INT) nPos-1)
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
    /// <param name="ptrRet"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBFieldInfo(nOrdinal AS DWORD,nPos AS DWORD,ptrRet REF OBJECT) AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBFieldInfo") AS IRDD
        ptrRet := oRDD:FieldInfo((INT) nPos-1, (INT) nOrdinal, ptrRet)
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
    /// <returns>
    /// </returns>
FUNCTION VODBFieldPut(nPos AS DWORD,xValue AS OBJECT) AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBFieldPut") AS IRDD
        RETURN oRDD:PutValue((INT) nPos-1, xValue)
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    RETURN FALSE
    
    
    /// <summary>
    /// </summary>
    /// <param name="nPos"></param>
    /// <param name="cFile"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBFileGet(nPos AS DWORD,cFile AS STRING) AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBFileGet") AS IRDD
        RETURN oRDD:GetValueFile((INT) nPos, cFile)
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    RETURN FALSE
    
    /// <summary>
    /// </summary>
    /// <param name="nPos"></param>
    /// <param name="cFile"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBFilePut(nPos AS DWORD,cFile AS STRING) AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBFilePut") AS IRDD
        RETURN oRDD:PutValueFile((INT) nPos, cFile)
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    RETURN FALSE
    
    /// <summary>
    /// Return a filter.
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBFilter() AS STRING
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBFilter") AS IRDD
        RETURN oRDD:FilterText
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    
    RETURN string.Empty   
    
    /// <summary>
    /// Lock an opened and shared database file.
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBFlock() AS LOGIC
    TRY    
        VAR oRDD := RDDHelpers.CWA("VODBFlock")
        LOCAL dbli AS DbLockInfo
        dbli := DbLockInfo{}
        dbli:Result := FALSE
        dbli:@@METHOD := DbLockInfo.LockMethod.File
        RETURN oRDD:Lock(dbli)
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    RETURN FALSE
    
    /// <summary>
    /// Determine if the previous search operation succeeded.
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBFound() AS LOGIC
    TRY
        VAR oRDD := RDDHelpers.CWA("VODBFound")
        RETURN oRDD:Found
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    RETURN FALSE
    
    
    
    /// <summary>
    /// Return the work area number.
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBGetSelect() AS DWORD
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBGetSelect") AS IRDD
        RETURN oRDD:Area
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    RETURN 0
    
    /// <summary>
    /// Move to the last logical record.
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBGoBottom() AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBGoBottom") AS IRDD
        RETURN oRDD:GoBottom()
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    RETURN FALSE
    
    /// <summary>
    /// Move to a record specified by record number.
    /// </summary>
    /// <param name="uRecId"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBGoto(uRecId AS OBJECT) AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBGoto") AS IRDD
        RETURN oRDD:GoToId(uRecID)
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    RETURN FALSE
    
    /// <summary>
    /// Move to the first logical record.
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBGoTop() AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBGoTop") AS IRDD
        RETURN oRDD:GoTop()
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    RETURN FALSE
    
    
    /// <summary>
    /// Retrieve information about a work area.
    /// </summary>
    /// <param name="nOrdinal"></param>
    /// <param name="ptrRet"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBInfo(nOrdinal AS DWORD,ptrRet REF OBJECT) AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBInfo") AS IRDD
        IF (nOrdinal == DBI_RDD_OBJECT)
            ptrRet := oRDD
        ELSE
            ptrRet := oRDD:Info((INT) nOrdinal, ptrRet)
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
    /// <returns>
    /// </returns>
FUNCTION VODBJoinAppend(nSelect AS DWORD,struList AS DbJoinList) AS LOGIC
    LOCAL result := FALSE AS LOGIC
    TRY
        LOCAL nCount AS LONG
        LOCAL nDestSel AS DWORD
        LOCAL nFld AS LONG
        LOCAL oRDDDest AS IRDD
        LOCAL oRDDSrc AS IRDD
        LOCAL oValue  AS OBJECT
        nCount := struList:Count
        nDestSel := struList:DestSel
        oRDDDest := RuntimeState.Workareas.GetRDD(nDestSel)
        IF oRDDDest == NULL
            PostNoTableError("VODBJoinAppend")
        ELSE
            FOR nFld := 0 TO nCount-1
                oRDDSrc := RuntimeState.Workareas.GetRDD(struList:Fields[nFld]:Area)
                IF oRDDSrc == NULL_OBJECT
                    PostNoTableError("VODBJoinAppend")
                ENDIF
                oValue := oRDDSrc:GetValue((INT) struList:Fields[nFld]:Pos)
                result := oRDDDest:PutValue(nFld, oValue)
                IF ! result
                    EXIT
                ENDIF
            NEXT
        ENDIF
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    RETURN result
    
    /// <summary>
    /// Return the number of the last record in a database file.
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBLastRec() AS LONG
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBLastRec") AS IRDD
        RETURN oRDD:RecCount
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    
    RETURN 0
    
    
    /// <summary>
    /// Search for the first record that matches a specified condition and scope.
    /// </summary>
    /// <param name="uCobFor"></param>
    /// <param name="uCobWhile"></param>
    /// <param name="nNext"></param>
    /// <param name="uRecId"></param>
    /// <param name="lRest"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBLocate(uCobFor AS ICodeBlock,uCobWhile AS ICodeBlock,nNext AS LONG,uRecId AS OBJECT,lRest AS LOGIC) AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODbMemoExt") AS IRDD
        LOCAL scopeinfo := DBSCOPEINFO{} AS DBSCOPEINFO
        scopeinfo:ForBlock := uCobFor
        scopeinfo:WhileBlock := uCobWhile
        scopeinfo:Rest:= lRest
        scopeinfo:RecId := uRecID
        scopeinfo:NextCount := nNext
        IF oRDD:SetScope(scopeinfo)
            RETURN oRDD:SkipScope(1)
        ENDIF
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    RETURN FALSE
    
    
    /// <summary>
    /// </summary>
    /// <param name="cDriver"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBMemoExt(cDriver AS STRING) AS STRING
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODbMemoExt") AS IRDD
        RETURN (STRING) oRDD:Info(DBI_MEMOEXT, NULL)
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    
    RETURN String.Empty
    /// <summary>
    /// Return the default index file extension for a work area as defined by the its RDD.
    /// </summary>
    /// <returns>Default extension for the current workarea, or an empty string when no table is open in the current workarea.
    /// </returns>
FUNCTION VODBOrdBagExt() AS STRING
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBOrdBagExt") AS IRDD
        VAR info := DbOrderInfo{}
        RETURN (STRING) oRDD:OrderInfo( DBOI_BAGEXT, info)
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    
    RETURN String.Empty
    
    /// <summary>
    /// Set the condition and scope for an order.
    /// </summary>
    /// <param name="ptrCondInfo"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBOrdCondSet(ordCondInfo AS DbOrderCondInfo) AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBOrdCondSet") AS IRDD
        RETURN oRDD:OrderCondition(ordCondInfo)
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    
    RETURN FALSE
    
    /// <summary>
    /// Create or replace an order in an index file.
    /// </summary>
    /// <param name="cBagName">Orderbag name (index filename) to create the order in.</param>
    /// <param name="uOrder">Ordername to create.</param>
    /// <param name="cExpr">The order key expression specified as a string</param>
    /// <param name="uCobExpr">The order key expression specified as a codeblock.</param>
    /// <param name="lUnique">TRUE creates a unique order by including only those records with unique key values; FALSE uses all records in the database file. </param>
    /// <param name="ordCondInfo">Object that describes other (optional) settings for the order creation, such as FOR condition, WHILE condition, STEP and EVAL.</param>
    /// <returns>
    /// </returns>
FUNCTION VODBOrdCreate(cBagName AS STRING,oOrder AS OBJECT,cExpr AS STRING,oCodeBlock AS ICodeBlock,lUnique AS LOGIC,ordCondInfo AS DbOrderCondInfo) AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBOrdCreate") AS IRDD
        VAR info := DbOrderCreateInfo{}
        info:BagName 		:= cBagName
        info:Order			:= oOrder
        info:Expression 	:= cExpr
        info:Block      	:= oCodeBlock
        info:Unique			:= lUnique
        info:OrdCondInfo 	:= ordCondInfo		
        RETURN oRDD:OrderCreate(info)
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    
    RETURN FALSE
    
    /// <summary>
    /// Remove an order from an open index file.
    /// </summary>
    /// <param name="cBagName"></param>
    /// <param name="oOrder"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBOrdDestroy(cBagName AS STRING,oOrder AS OBJECT) AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBOrdDestroy") AS IRDD
        VAR info := DbOrderInfo{}
        info:BagName := cBagName
        info:Order   := oOrder
        RETURN oRDD:OrderDestroy(info)
        
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    
    RETURN FALSE
    
    /// <summary>
    /// Return information about index files and the orders in them.
    /// </summary>
    /// <param name="nOrdinal"></param>
    /// <param name="cBagName"></param>
    /// <param name="uOrder"></param>
    /// <param name="oValue"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBOrderInfo(nOrdinal AS DWORD,cBagName AS STRING,oOrder AS OBJECT,oValue AS OBJECT) AS LOGIC
    TRY
        RETURN VoDbOrderInfo(nOrdinal, cBagName, oOrder, REF oValue)
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    RETURN FALSE
    /// <summary>
    /// Return information about index files and the orders in them.
    /// </summary>
    /// <param name="nOrdinal"></param>
    /// <param name="cBagName"></param>
    /// <param name="uOrder"></param>
    /// <param name="oValue"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBOrderInfo(nOrdinal AS DWORD,cBagName AS STRING,oOrder AS OBJECT,oValue REF OBJECT) AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBOrdDestroy") AS IRDD
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
    /// <returns>
    /// </returns>
FUNCTION VODBOrdListAdd(cBagName AS STRING,oOrder AS OBJECT) AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBOrdListAdd") AS IRDD
        VAR info := DbOrderInfo{}
        info:BagName := cBagName
        IF oOrder == NULL
            info:AllTags := TRUE
        ELSE
            info:Order   := oOrder
        ENDIF
        RETURN oRDD:OrderListAdd(info)
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    
    RETURN FALSE
    
    /// <summary>
    /// Remove orders from the order list in a work area and close associated index files.
    /// </summary>
    /// <param name="cBagName"></param>
    /// <param name="oOrder"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBOrdListClear(cBagName AS STRING,oOrder AS OBJECT) AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBOrdListAdd",FALSE) AS IRDD
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
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    RETURN FALSE    
    
    /// <summary>
    /// Rebuild all orders in the order list of a work area.
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBOrdListRebuild() AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBOrdListRebuild") AS IRDD
        RETURN oRDD:OrderListRebuild()
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    
    RETURN FALSE
    
    /// <summary>
    /// Set the controlling order for a work area.
    /// </summary>
    /// <param name="cBagName"></param>
    /// <param name="oOrder"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBOrdSetFocus(cBagName AS STRING,oOrder AS OBJECT, strPreviousOrder REF STRING) AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBOrdSetFocus") AS IRDD
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
    /// <returns>
    /// </returns>
FUNCTION VODBPack() AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBPack") AS IRDD
        RETURN oRDD:Pack()
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    
    RETURN FALSE
    
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
    TRY
        RETURN (DWORD) VODBRddList():Length
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    RETURN 0
    /// <summary>
    /// </summary>
    /// <param name="nOrdinal"></param>
    /// <param name="ptrRet"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBRDDInfo(nOrdinal AS DWORD,oRet REF OBJECT) AS LOGIC
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
    
    
FUNCTION VODBRDDInfo(nOrdinal AS DWORD,oRet AS OBJECT) AS LOGIC
    TRY
        LOCAL oValue AS OBJECT
        oValue := RuntimeState.GetValue<OBJECT> ((INT) nOrdinal)
        RuntimeState.SetValue((INT) nOrdinal, oRet)
        RETURN TRUE
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    RETURN FALSE
    
    
[Obsolete( "'VODBRddList( rddList, nRddType )' is not supported, use VODBRddList() instead", TRUE )];
FUNCTION VODBRddList(rddList AS RddList,nRddType AS DWORD) AS LOGIC
    THROW  NotImplementedException{}
    
FUNCTION VODBRddList() AS STRING[]
    TRY
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
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    RETURN NULL
    
    
    /// <summary>
    /// Return an RDD name.                  
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBRddName() AS STRING
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBRddName") AS IRDD
        RETURN oRDD:SysName
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    
    RETURN String.Empty
    
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
    /// <returns>
    /// </returns>
FUNCTION VODBRecall() AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBRecall") AS IRDD
        RETURN oRDD:Recall()
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    
    RETURN FALSE
    
    /// <summary>
    /// Return the current record number.
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBRecno() AS OBJECT
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBRecno") AS IRDD
        RETURN oRDD:Recno
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    
    RETURN NULL
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBRecordGet() AS BYTE[]
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBRecordGet") AS IRDD
        RETURN oRDD:GetRec()
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    
    RETURN NULL       
    
    /// <summary>
    /// Retrieve information about a record.
    /// </summary>
    /// <param name="nOrdinal"></param>
    /// <param name="oRecID"></param>
    /// <param name="oRet"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBRecordInfo(nOrdinal AS DWORD,oRecID AS OBJECT,oRet REF OBJECT) AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBRecordInfo") AS IRDD
        oRDD:RecInfo(oRecID, (INT) nOrdinal, oRet )
        RETURN TRUE
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    
    RETURN FALSE       
    
    /// <summary>
    /// </summary>
    /// <param name="pszRecord"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBRecordPut(aRecord AS BYTE[]) AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBRecordPut") AS IRDD
        RETURN oRDD:PutRec(aRecord)
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    
    RETURN FALSE       
    
    /// <summary>
    /// Return the linking expression of a specified relation.
    /// </summary>
    /// <param name="nPos"></param>
    /// <param name="pszRel"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBRelation(nPos AS DWORD,sRel REF STRING) AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBRecordPut") AS IRDD
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
    /// <returns>
    /// </returns>
FUNCTION VODBRlock(uRecId AS OBJECT) AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBRlock") AS IRDD
        LOCAL lockInfo AS DbLockInfo
        lockInfo := DbLockInfo{}
        lockInfo:RecId := uRecID
        lockInfo:@@METHOD  := DbLockInfo.LockMethod.Multiple
        RETURN oRDD:Lock(lockInfo)
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    
    RETURN FALSE 
    /// <summary>
    /// Return the work area number of a relation.
    /// </summary>
    /// <param name="nPos"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBRSelect(nPos AS DWORD) AS DWORD
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBRSelect") AS IRDD
        RETURN oRDD:RelArea(nPos)
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    RETURN 0
    
    /// <summary>
    /// Move to the record having the specified key value.
    /// </summary>
    /// <param name="oValue"></param>
    /// <param name="lSoftSeek"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBSeek(oValue AS OBJECT,lSoftSeek AS LOGIC) AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBSeek") AS IRDD
        VAR info 		:= DbSeekInfo{}
        info:Value 		:= oValue          
        info:SoftSeek 	:= lSoftSeek
        //info:Last		:= lLast 
        RETURN oRDD:Seek(info)
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    
    RETURN FALSE       
    
    /// <summary>
    /// Select a new work area and retrieve the current work area.
    /// </summary>
    /// <param name="nNew"></param>
    /// <param name="nOld"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBSelect(nNew AS DWORD,nOld REF DWORD ) AS LOGIC
    TRY
        nOld := (DWORD) RuntimeState.Workareas:CurrentWorkAreaNO
        IF nNew != nOld
            IF nNew == 0
                nNew := (DWORD) RuntimeState.Workareas:FindEmptyArea(TRUE)
            ENDIF
            IF nNew > WorkAreas.MaxWorkareas
                PostArgumentError( "VODBSelect", EDB_SELECT, "nNew", 1, <OBJECT>{ nNew } )
            ELSE
                RuntimeState.Workareas:CurrentWorkAreaNO :=  nNew
            ENDIF
        ENDIF
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    
    RETURN TRUE
    
    /// <summary>
    /// Set a filter condition.
    /// </summary>
    /// <param name="oBlock"></param>
    /// <param name="cFilter"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBSetFilter(oBlock AS ICodeBlock,cFilter AS STRING) AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBSetFilter") AS IRDD
        VAR info 		 := DbFilterInfo{}
        info:FilterBlock := oBlock         
        info:FilterText  := cFilter
        RETURN oRDD:SetFilter(info)
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    
    RETURN FALSE       
    
    /// <summary>
    /// Set the found flag.
    /// </summary>
    /// <param name="lFound"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBSetFound(lFound AS LOGIC) AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBSetFound") AS IRDD
        oRDD:Found := TRUE
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    
    RETURN FALSE   
    
    /// <summary>
    /// Specify the code block for a locate condition.
    /// </summary>
    /// <param name="oBlock"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBSetLocate(oBlock AS ICodeBlock) AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBSetLocate") AS IRDD
        VAR scope := oRDD:GetScope()
        scope:ForBlock := oBlock
        oRDD:SetScope(scope)    
        RETURN TRUE
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    
    RETURN FALSE                            
    
    /// <summary>
    /// Relate a specified work area to the current work area.
    /// </summary>
    /// <param name="cAlias"></param>
    /// <param name="uCobKey"></param>
    /// <param name="cKey"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBSetRelation(cAlias AS STRING,oKey  AS ICodeBlock,cKey AS STRING) AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBSetRelation") AS IRDD
        LOCAL nDest := RuntimeState.Workareas.FindAlias(cAlias) AS DWORD
        IF nDest == 0
            PostArgumentError("VODBSetRelation",EDB_SETRELATION, nameof(cAlias), 1, <OBJECT>{cAlias})
        ENDIF
        LOCAL oDest := RuntimeState.Workareas.GetRDD(nDest) AS IRDD
        IF oDest == NULL_OBJECT
            PostArgumentError("VODBSetRelation",EDB_SETRELATION, nameof(cAlias), 1, <OBJECT>{cAlias})
        ENDIF
        LOCAL oRelInfo AS DbRelInfo
        oRelInfo := DbRelInfo{}
        oRelInfo:Parent := oRDD
        oRelInfo:Child  := oDest
        oRelInfo:Key    := cKey
        oRelInfo:Block  := oKey
        RETURN oRDD:SetRel(oRelInfo)
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    RETURN FALSE
    
    
    /// <summary>
    /// </summary>
    /// <param name="scope"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBSetScope(scope AS DBSCOPEINFO) AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBSetLocate") AS IRDD
        RETURN oRDD:SetScope(scope)
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    
    RETURN FALSE
    /// <summary>
    /// Select a new work area.
    /// </summary>
    /// <param name="siNew"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBSetSelect(siNew AS INT) AS DWORD
    TRY
        IF siNew == -1
            siNew := (INT) RuntimeState.Workareas:FindEmptyArea(FALSE)
        ELSEIF siNEw == 0
            siNew := (INT) RuntimeState.Workareas:FindEmptyArea(TRUE)
        ENDIF
        IF siNew == 0 || siNew > Workareas.MaxWorkAreas
            PostArgumentError( "VODBSetSelect", EDB_SELECT, nameof(siNew), 1, <OBJECT>{siNew})
        ELSE      
            RuntimeState.CurrentWorkarea := (DWORD) siNew   
        ENDIF
        RETURN (DWORD) siNew
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    RETURN 0
    
    /// <summary>
    /// Move the record pointer relative to the current record.
    /// </summary>
    /// <param name="nRecords"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBSkip(nRecords AS LONG) AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBSkip") AS IRDD
        RETURN oRDD:Skip(nRecords)
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    
    RETURN FALSE   
    
    /// <summary>
    /// </summary>
    /// <param name="nRecords"></param>
    /// <param name="scope"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBSkipScope(nRecords AS LONG,scope AS DBSCOPEINFO) AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBSkipScope") AS IRDD
        oRDD:SetScope(scope)    
        RETURN oRDD:Skip(nRecords)
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    
    RETURN FALSE   
    
INTERNAL FUNCTION _VODBBuildTrans( dbti AS DbTransInfo, lpfn AS DbFieldNames, src AS IRDD, dst AS IRDD ) AS LOGIC
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
FUNCTION VODBSort(nDest AS DWORD,fieldNames AS DbFieldNames,uCobFor AS ICodeBlock,uCobWhile AS ICodeBlock,;
nNext AS OBJECT,nRecno AS OBJECT,lRest AS LOGIC,sortNames AS DbFieldNames) AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBSort") AS IRDD
        LOCAL info AS DbSortInfo
        info := DbSortInfo{fieldNames:fieldCount, sortNames:fieldCount}
        _VoDbTransSetInfo(oRDD, info:TransInfo, "VODBSort", nDest, fieldNames,uCobFor, uCobWhile, nNext, nRecno, lRest)        
        // Now process the fieldnames in the sortnames list
        LOCAL nFld AS INT
        FOR nFld := 0 TO sortNames:fieldCount -1
            // Could be FieldName / ACDB to indicate the sort method
            VAR parts := sortNames:fields[nFld]:Split('/')
            IF parts:Length > 1
                VAR part = parts[1]
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
                PostArgumentError( "VODBSort", EDB_FIELDNAME, nameof(sortNames), 8, <OBJECT>{ sortNames:fields[nFld] } )
            ENDIF
            info:Items[nFld]:FieldNo := iField
        NEXT
        RETURN oRDD:Sort( info )
        
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    
    RETURN FALSE   
    
INTERNAL FUNCTION _VoDbTransSetInfo(oRdd AS IRDD, info AS DbTransInfo, cFunc AS STRING,nDest AS DWORD, fldNames AS DbFieldNames,;
uCobFor AS ICodeBlock ,uCobWhile AS ICodeBlock ,;
nNext AS OBJECT,nRecno AS OBJECT,lRest AS LOGIC) AS VOID
    LOCAL oDest := RUntimeState.Workareas.GetRDD(nDest) AS IRDD
    IF oDest == NULL
        PostNoTableError("VODBTrans")
    ENDIF
    info:Destination := oDest
    IF !_VODBBuildTrans( info, fldNames, oRDD, oDest )
        PostArgumentError( cFunc, EDB_DBSTRUCT, nameof(fldNames), 2, <OBJECT>{fldNames} )
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
    /// <summary>
    /// </summary>
    /// <param name="nDest"></param>
    /// <param name="fldNames"></param>
    /// <param name="uCobFor"></param>
    /// <param name="uCobWhile"></param>
    /// <param name="nNext"></param>
    /// <param name="nRecno"></param>
    /// <param name="lRest"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBTrans(nDest AS DWORD,fldNames AS DbFieldNames,uCobFor AS ICodeBlock,uCobWhile AS ICodeBlock,;
nNext AS OBJECT,nRecno AS OBJECT,lRest AS LOGIC) AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBTrans") AS IRDD
        LOCAL info AS DbTransInfo
        info := DbTransInfo{fldNames:fieldCount}
        _VoDbTransSetInfo(oRDD, info, "VODBTrans", nDest, fldNames, uCobFor, uCobWhile, nNext, nRecno, lRest)
        RETURN oRDD:Trans( info )
        
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    RETURN FALSE      
    /// <summary>
    /// </summary>
    /// <param name="nDest"></param>
    /// <param name="fldNames"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBTransRec(nDest AS DWORD,fldNames AS DbFieldNames) AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBUnlock") AS IRDD
        LOCAL dbti := DbTransInfo{ fldNames:fieldCount} AS DBTRANSINFO
        LOCAL oDest := RUntimeState.Workareas.GetRDD(nDest) AS IRDD
        IF oDest == NULL_OBJECT
            PostNoTableError("VODBTransRec")
        ENDIF
        dbti:Destination := oDest
        dbti:ItemCount := fldNames:FieldCount
        IF _VODBBuildTrans( dbti, fldNames, oRDD, oDest )
            LOCAL oCanPutRec AS OBJECT
            dbti:Flags |= DBTRANSINFO.Match
            oCanPutRec := oRDD:Info( DBInfo.DBI_CANPUTREC, NULL )
            IF oCanPutRec != NULL .AND. (LOGIC) oCanPutRec
                oCanPutRec := oDest:Info(DBInfo.DBI_CANPUTREC, NULL )
                IF oCanPutRec != NULL .AND. (LOGIC) oCanPutRec
                    dbti:Flags |= DBTRANSINFO.PutRec
                ENDIF
            ENDIF
        ENDIF
        RETURN oRDD:TransRec( dbti )

    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    
    RETURN FALSE  
    
    
    
    /// <summary>
    /// Release all locks for a work area.
    /// </summary>
    /// <param name="uRecno"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBUnlock(uRecno AS OBJECT) AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBUnlock") AS IRDD
        RETURN oRDD:UnLock(uRecno)    
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    
    RETURN FALSE   
    
    
    /// <summary>
    /// Release all locks for all work areas.
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBUnlockAll() AS LOGIC
    TRY
        RuntimeState.Workareas:UnlockAll()
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    
    RETURN FALSE   
    
    /// <summary>
    /// Open a database file.
    /// </summary>
    /// <param name="lNew"></param>
    /// <param name="rddList"></param>
    /// <param name="cName"></param>
    /// <param name="cAlias"></param>
    /// <param name="lShare"></param>
    /// <param name="lReadOnly"></param>
    /// <returns>
    /// </returns>
    
FUNCTION VODBUseArea(lNew AS LOGIC,rddList AS _RDDLIST,cName AS STRING,cAlias AS STRING,lShare AS LOGIC,lReadOnly AS LOGIC) AS LOGIC
    TRY
        LOCAL oRdd := NULL  AS RegisteredRDD
        FOREACH VAR name IN rddList:atomRddName
            oRdd := RegisteredRDD.Find(name)
            oRdd:Load()
        NEXT
        IF oRdd != NULL_OBJECT
            RETURN VODBUseArea(lNew, oRdd:RddType, cName, cAlias, lShare, lReadOnly)
        ENDIF
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    RETURN FALSE    
FUNCTION VODBUseArea(lNew AS LOGIC,rddName AS STRING,cName AS STRING,cAlias AS STRING,lShare AS LOGIC,lReadOnly AS LOGIC) AS LOGIC
    TRY
        LOCAL rddType AS Type
        IF ( rddType := _VoDbRddNameToType( rddName ) ) == NULL
            PostArgumentError( "VODBUseArea", EDB_RDDNOTFOUND, "rddName", 3, <OBJECT>{ rddName } )
        ELSE
            RETURN VODBUseArea( lNew, rddType, cName, cAlias, lShare, lReadOnly )
        ENDIF
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    
    RETURN FALSE
    
    
    
FUNCTION VODBUseArea(lNew AS LOGIC,rddType AS System.Type,cName AS STRING,cAlias AS STRING,lShare AS LOGIC,lReadOnly AS LOGIC) AS LOGIC
    TRY
        LOCAL ret   := FALSE AS LOGIC
        LOCAL area  := 0    AS DWORD
        IF String.IsNullOrEmpty( cName )
            PostArgumentError( "VODBUseArea", EDB_USE, nameof(cName), 3 , <OBJECT>{NULL})
        ELSE
            ret := TRUE
            NetErr( FALSE )
            cName := cName:Trim() // :ToUpperInvariant()
            
            IF String.IsNullOrEmpty( cAlias )
                TRY
                    cAlias := Path.GetFileNameWithoutExtension( cName )
                CATCH  AS ArgumentException
                    PostArgumentError( "VODBUseArea", EDB_USE, nameof(cName), 3, <OBJECT>{cName} ) 
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
                LOCAL rdd :=_VoDbCreateRDDInstance( rddType, cAlias ) AS IRDD
                
                IF rdd == NULL
                    PostArgumentError( "VODBUseArea", EDB_DRIVERLOAD, nameof(rddType), 3, <OBJECT>{ rddType } )
                    ret := FALSE
                ELSEIF ! _VoDbIsAliasUnused( cAlias )
                    PostArgumentError( "VODBUseArea", EDB_DUPALIAS, nameof(cAlias), 4, <OBJECT>{ cAlias } )
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
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    RETURN FALSE 
    
    
    //	LOCAL oRDD := RDDHelpers.CWA("VODBUseArea") AS IRDD
    //	IF oRDD != NULL
    //		
    //	ENDIF                            
    
    /// <summary>
    /// Remove all records from open files.
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBZap() AS LOGIC
    TRY
        LOCAL oRDD := RDDHelpers.CWA("VODBZap") AS IRDD
        RETURN oRDD:Zap()
    CATCH e AS Exception
        RuntimeState.LastRDDError := e
    END TRY
    
    RETURN FALSE
    
    
INTERNAL FUNCTION PostArgumentError( funcName AS STRING, subcode AS DWORD, argName AS STRING, argNum AS DWORD, args AS OBJECT[] ) AS VOID
    LOCAL e AS Error
    e := Error.ArgumentError(funcName, argname,argNum, args)
    e:SubSystem := "DBCMD"
    e:Severity := ES_ERROR
    e:GenCode := EG_ARG
    e:SubCode := subcode
    THROW e
    
INTERNAL FUNCTION PostError( funcName AS STRING, gencode AS DWORD, subcode AS DWORD ) AS VOID
    LOCAL e AS Error
    e := Error{}
    e:SubSystem := "DBCMD"
    e:Severity := ES_ERROR
    e:GenCode := gencode
    e:SubCode := subcode
    e:FuncSym := funcName
    THROW e
    
    
    // Returns false if alias is illegal
INTERNAL FUNCTION _VoDbAliasFromFilename( cFilename AS STRING, cAlias REF STRING ) AS LOGIC
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
INTERNAL FUNCTION _VoDbIsAliasUnused( cAlias AS STRING ) AS LOGIC
    RETURN RuntimeState.Workareas:FindAlias(cAlias) == 0
    
    // Create RDD Object from RDD Type
INTERNAL FUNCTION _VoDbCreateRDDInstance( rddType AS Type , cAlias AS STRING) AS IRDD
    LOCAL ret    AS IRDD
    TRY
        ret := (IRDD) rddType:InvokeMember( NULL, BindingFlags.DeclaredOnly | BindingFlags.Public | BindingFlags.Instance | BindingFlags.CreateInstance, NULL, NULL, <OBJECT>{} )
        ret:Alias := cAlias:ToUpperInvariant()
    CATCH
        ret := NULL
    END TRY
    RETURN ret 
    
    
    
    
INTERNAL FUNCTION _VoDbRddNameToType( cRDDName AS STRING ) AS Type
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
