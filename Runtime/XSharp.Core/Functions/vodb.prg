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
    RETURN RuntimeState.Workareas:GetAlias(nArea)
    
    /// <summary>
    /// Add a new record.
    /// </summary>
    /// <param name="lReleaseLocks"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBAppend(lReleaseLocks AS LOGIC) AS LOGIC
    LOCAL oRDD := RDDHelpers.CWA("VODBAppend") AS IRDD
    IF (oRDD != NULL)
        RETURN oRDD:Append(lReleaseLocks)
    ENDIF
    RETURN FALSE   
    
    /// <summary>
    /// </summary>
    /// <param name="nOrdinal"></param>
    /// <param name="nPos"></param>
    /// <param name="ptrRet"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBBlobInfo(nOrdinal AS DWORD,nPos AS DWORD,ptrRet REF OBJECT) AS LOGIC
    LOCAL oRDD := RDDHelpers.CWA("VODBBlobInfo") AS IRDD
    IF (oRDD != NULL)
        ptrRet := oRDD:BlobInfo(nOrdinal, nPos)
        RETURN TRUE
    ENDIF
    RETURN FALSE   
    
    /// <summary>
    /// Determine when beginning-of-file is encountered.
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBBof() AS LOGIC
    LOCAL oRDD := RDDHelpers.CWA("VODBBof") AS IRDD
    IF (oRDD != NULL)
        RETURN oRDD:BoF
    ENDIF
    
    RETURN FALSE   
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBBuffRefresh() AS LOGIC
    LOCAL oRDD := RDDHelpers.CWA("VODBBuffRefresh") AS IRDD
    IF (oRDD != NULL)
        oRDD:RecInfo(0, DbRecordInfo.DBRI_Updated,NULL)
        RETURN TRUE
    ENDIF
    RETURN FALSE   
    
    /// <summary>
    /// Clear a logical filter condition.
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBClearFilter() AS LOGIC
    LOCAL oRDD := RDDHelpers.CWA("VODBClearFilter") AS IRDD
    IF (oRDD != NULL)
        RETURN oRDD:ClearFilter()
    ENDIF
    RETURN FALSE   
    
    /// <summary>
    /// Clear a locate condition by deleting the locate code block.
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBClearLocate() AS LOGIC
    LOCAL oRDD := RDDHelpers.CWA("VODBClearLocate") AS IRDD
    IF (oRDD != NULL)
        RETURN oRDD:ClearScope()
    ENDIF
    RETURN FALSE   
    
    /// <summary>
    /// Clear any active relations.
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBClearRelation() AS LOGIC
    LOCAL oRDD := RDDHelpers.CWA("VODBClearRelation") AS IRDD
    IF (oRDD != NULL)
        RETURN oRDD:ClearRel()
    ENDIF
    RETURN FALSE   
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBClearScope() AS LOGIC
    LOCAL oRDD := RDDHelpers.CWA("VODBClearScope") AS IRDD
    IF (oRDD != NULL)
        RETURN oRDD:ClearScope()
    ENDIF
    RETURN FALSE   
    
    /// <summary>
    /// Close all files in all work areas.
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBCloseAll() AS LOGIC
    RETURN RuntimeState.Workareas:CloseAll()
    
    /// <summary>
    /// Close all files in a work area.
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBCloseArea() AS LOGIC
    LOCAL oRDD := RDDHelpers.CWA("VODBFlock") AS IRDD
    IF (oRDD != NULL)
        RETURN oRDD:Close()
    ENDIF
    RETURN FALSE   
    
    /// <summary>
    /// Flush pending updates in one work area.
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBCommit() AS LOGIC
    LOCAL oRDD := RDDHelpers.CWA("VODBCommit") AS IRDD
    IF (oRDD != NULL)
        RETURN oRDD:Flush()
    ENDIF
    RETURN FALSE   
    
    /// <summary>
    /// Flush pending updates in all work areas.
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBCommitAll() AS LOGIC
    RETURN RuntimeState.Workareas:CommitAll()
    
    /// <summary>
    /// Resume a pending locate condition.
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBContinue() AS LOGIC
    LOCAL oRDD := RDDHelpers.CWA("VODBContinue") AS IRDD
    IF (oRDD != NULL)
        RETURN oRDD:Continue()
    ENDIF
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
    LOCAL ret := FALSE AS LOGIC
    LOCAL rddType AS Type
    IF ( rddType := _VoDbRddNameToType( cRddName ) ) == NULL
        PostArgumentError( "VODBCreate", EDB_RDDNOTFOUND, nameof(cRddName), 3, <OBJECT>{ cRddName } )
    ELSE
        ret := VODBCreate( cName, aStruct, rddType, lNew, cAlias, cDelim, lKeep, lJustOpen )
    ENDIF
    RETURN ret
    
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
    LOCAL oRdd := NULL  AS RegisteredRDD
    LOCAL ret := FALSE AS LOGIC
    FOREACH VAR name IN rddList:atomRddName
        oRdd := RegisteredRDD.Find(name)
        oRdd:Load()
    NEXT
    IF (oRdd != NULL_OBJECT)
        ret := VODBCreate(cName, aStruct, oRdd:RddType, lNew, cAlias, cDelim, lKeep, lJustOpen)
    ENDIF
    RETURN ret  
    
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
    RETURN VoDbCreate(cName, aStruct:ToArray(), rddType, lNew, cAlias, cDelim, lKeep, lJustOpen)
    
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
    LOCAL uiOldArea := 0 AS DWORD
    LOCAL uiNewArea := 0 AS DWORD
    LOCAL ret   := FALSE   AS LOGIC
    TRY
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
    LOCAL oRDD := RDDHelpers.CWA("VODBDelete") AS IRDD
    IF (oRDD != NULL)
        RETURN oRDD:Delete()
    ENDIF
    RETURN FALSE   
    
    /// <summary>
    /// Return the deleted status of the current record.
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBDeleted() AS LOGIC
    LOCAL oRDD := RDDHelpers.CWA("VODBDeleted") AS IRDD
    IF (oRDD != NULL)
        RETURN oRDD:Deleted
    ENDIF
    RETURN FALSE   
    
    /// <summary>
    /// Determine when end-of-file is encountered.
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBEof() AS LOGIC
    LOCAL oRDD := RDDHelpers.CWA("VODBEof") AS IRDD
    IF (oRDD != NULL)
        RETURN oRDD:EoF
    ENDIF
    RETURN FALSE   
    
    /// <summary>
    /// Evaluate a code block for each record that matches a specified scope and/or condition.
    /// </summary>
    /// <param name="uBlock"></param>
    /// <param name="uCobFor"></param>
    /// <param name="uCobWhile"></param>
    /// <param name="nNext"></param>
    /// <param name="nRecno"></param>
    /// <param name="lRest"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBEval(uBlock AS OBJECT,uCobFor AS OBJECT,uCobWhile AS OBJECT,nNext AS OBJECT,nRecno AS OBJECT,lRest AS LOGIC) AS LOGIC
    THROW  NotImplementedException{}
    //RETURN FALSE   
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBExit() AS INT
    THROW  NotImplementedException{}
    //RETURN 0   
    
    /// <summary>
    /// Retrieve the value of a specified database field.
    /// </summary>
    /// <param name="nPos"></param>
    /// <param name="ptrRet"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBFieldGet(nPos AS DWORD,ptrRet REF OBJECT) AS LOGIC
    LOCAL oRDD := RDDHelpers.CWA("VODBFieldGet") AS IRDD
    IF (oRDD != NULL)
        ptrRet := oRDD:GetValue((INT) nPos)
        RETURN TRUE
    ENDIF
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
    LOCAL oRDD := RDDHelpers.CWA("VODBFieldInfo") AS IRDD
    IF (oRDD != NULL)
        ptrRet := oRDD:FieldInfo((INT) nPos, (INT) nOrdinal, ptrRet)
        RETURN TRUE
    ENDIF
    RETURN FALSE   
    
    /// <summary>
    /// Set the value of a specified database field.
    /// </summary>
    /// <param name="nPos"></param>
    /// <param name="xValue"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBFieldPut(nPos AS DWORD,xValue AS OBJECT) AS LOGIC
    LOCAL oRDD := RDDHelpers.CWA("VODBFieldPut") AS IRDD
    IF (oRDD != NULL)
        RETURN oRDD:PutValue((INT) nPos, xValue)
    ENDIF
    RETURN FALSE   
    
    /// <summary>
    /// </summary>
    /// <param name="nPos"></param>
    /// <param name="cFile"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBFileGet(nPos AS DWORD,cFile AS STRING) AS LOGIC
    LOCAL oRDD := RDDHelpers.CWA("VODBFileGet") AS IRDD
    IF (oRDD != NULL)
        RETURN oRDD:GetValueFile((INT) nPos, cFile)
    ENDIF
    RETURN FALSE   
    
    /// <summary>
    /// </summary>
    /// <param name="nPos"></param>
    /// <param name="cFile"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBFilePut(nPos AS DWORD,cFile AS STRING) AS LOGIC
    LOCAL oRDD := RDDHelpers.CWA("VODBFilePut") AS IRDD
    IF (oRDD != NULL)
        RETURN oRDD:PutValueFile((INT) nPos, cFile)
    ENDIF
    RETURN FALSE   
    
    /// <summary>
    /// Return a filter.
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBFilter() AS STRING
    LOCAL oRDD := RDDHelpers.CWA("VODBFilter") AS IRDD
    IF (oRDD != NULL)
        RETURN oRDD:FilterText
    ENDIF
    
    RETURN string.Empty   
    
    /// <summary>
    /// Lock an opened and shared database file.
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBFlock() AS LOGIC
    VAR oRDD := RDDHelpers.CWA("VODBFlock")
    IF (oRDD != NULL)
        LOCAL dbli AS DbLockInfo
        dbli := DbLockInfo{}
        dbli:Result := FALSE
        dbli:@@METHOD := DbLockInfo.LockMethod.File
        RETURN oRDD:Lock(dbli)
    ENDIF
    RETURN FALSE   
    
    /// <summary>
    /// Determine if the previous search operation succeeded.
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBFound() AS LOGIC
    VAR oRDD := RDDHelpers.CWA("VODBFound")
    IF (oRDD != NULL)
        RETURN oRDD:Found
    ENDIF
    RETURN FALSE   
    
    
    /// <summary>
    /// Return the work area number.
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBGetSelect() AS DWORD
    LOCAL oRDD := RDDHelpers.CWA("VODBGetSelect") AS IRDD
    IF oRDD != NULL
        RETURN oRDD:Area
    ENDIF                            
    RETURN 0
    
    /// <summary>
    /// Move to the last logical record.
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBGoBottom() AS LOGIC
    LOCAL oRDD := RDDHelpers.CWA("VODBGoBottom") AS IRDD
    IF (oRDD != NULL)
        RETURN oRDD:GoBottom()
    ENDIF
    RETURN FALSE   
    
    /// <summary>
    /// Move to a record specified by record number.
    /// </summary>
    /// <param name="uRecId"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBGoto(uRecId AS OBJECT) AS LOGIC
    LOCAL oRDD := RDDHelpers.CWA("VODBGoto") AS IRDD
    IF (oRDD != NULL)
        RETURN oRDD:GoToId(uRecID)
    ENDIF
    RETURN FALSE   
    
    /// <summary>
    /// Move to the first logical record.
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBGoTop() AS LOGIC
    LOCAL oRDD := RDDHelpers.CWA("VODBGoTop") AS IRDD
    IF (oRDD != NULL)
        RETURN oRDD:GoTop()
    ENDIF
    RETURN FALSE   
    
    /// <summary>
    /// Retrieve information about a work area.
    /// </summary>
    /// <param name="nOrdinal"></param>
    /// <param name="ptrRet"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBInfo(nOrdinal AS DWORD,ptrRet REF OBJECT) AS LOGIC
    LOCAL oRDD := RDDHelpers.CWA("VODBInfo") AS IRDD
    IF (oRDD != NULL)
        ptrRet := oRDD:Info((INT) nOrdinal, ptrRet)
        RETURN TRUE
    ENDIF
    RETURN FALSE   
    
    /// <summary>
    /// </summary>
    /// <param name="nSelect"></param>
    /// <param name="struList"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBJoinAppend(nSelect AS DWORD,struList AS DbJOINLIST) AS LOGIC
    THROW  NotImplementedException{}
    //RETURN FALSE   
    
    /// <summary>
    /// Return the number of the last record in a database file.
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBLastRec() AS LONG
    LOCAL oRDD := RDDHelpers.CWA("VODBLastRec") AS IRDD
    IF (oRDD != NULL)
        RETURN oRDD:RecCount
    ENDIF
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
FUNCTION VODBLocate(uCobFor AS OBJECT,uCobWhile AS OBJECT,nNext AS LONG,uRecId AS OBJECT,lRest AS LOGIC) AS LOGIC
    THROW  NotImplementedException{}
    //RETURN FALSE   
    
    /// <summary>
    /// </summary>
    /// <param name="cDriver"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBMemoExt(cDriver AS STRING) AS STRING
    LOCAL oRDD := RDDHelpers.CWA("VODbMemoExt") AS IRDD
    IF oRDD != NULL
        RETURN (STRING) oRDD:Info(DBI_MEMOEXT, NULL)
    ENDIF                            
    RETURN String.Empty
    /// <summary>
    /// Return the default index file extension for a work area as defined by the its RDD.
    /// </summary>
    /// <returns>Default extension for the current workarea, or an empty string when no table is open in the current workarea.
    /// </returns>
FUNCTION VODBOrdBagExt() AS STRING
    LOCAL oRDD := RDDHelpers.CWA("VODBOrdBagExt") AS IRDD
    IF oRDD != NULL
        VAR info := DbOrderInfo{}
        RETURN (STRING) oRDD:OrderInfo( DBOI_BAGEXT, info)
    ENDIF                            
    RETURN String.Empty
    
    /// <summary>
    /// Set the condition and scope for an order.
    /// </summary>
    /// <param name="ptrCondInfo"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBOrdCondSet(ordCondInfo AS DbOrderCondInfo) AS LOGIC
    LOCAL oRDD := RDDHelpers.CWA("VODBOrdCondSet") AS IRDD
    IF oRDD != NULL
        RETURN oRDD:OrderCondition(ordCondInfo)
    ENDIF                            
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
    LOCAL oRDD := RDDHelpers.CWA("VODBOrdCreate") AS IRDD
    IF oRDD != NULL                  
        VAR info := DbOrderCreateInfo{}
        info:BagName 		:= cBagName
        info:Order			:= oOrder
        info:Expression 	:= cExpr
        info:Block      	:= oCodeBlock
        info:Unique			:= lUnique
        info:OrdCondInfo 	:= ordCondInfo		
        RETURN oRDD:OrderCreate(info)
    ENDIF                            
    RETURN FALSE
    
    /// <summary>
    /// Remove an order from an open index file.
    /// </summary>
    /// <param name="cBagName"></param>
    /// <param name="oOrder"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBOrdDestroy(cBagName AS STRING,oOrder AS OBJECT) AS LOGIC
    LOCAL oRDD := RDDHelpers.CWA("VODBOrdDestroy") AS IRDD
    IF oRDD != NULL                  
        VAR info := DbOrderInfo{}
        info:BagName := cBagName
        info:Order   := oOrder
        RETURN oRDD:OrderDestroy(info)
    ENDIF                            
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
    RETURN VoDbOrderInfo(nOrdinal, cBagName, oOrder, REF oValue)
    
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
    LOCAL oRDD := RDDHelpers.CWA("VODBOrdDestroy") AS IRDD
    IF oRDD != NULL                  
        VAR info := DbOrderInfo{}
        info:BagName := cBagName
        info:Order   := oOrder
        oValue :=  oRDD:OrderInfo(nOrdinal, info)
        RETURN TRUE
    ENDIF                            
    RETURN FALSE
    
    
    /// <summary>
    /// Open an index file and add specified orders to the order list in a work area.
    /// </summary>
    /// <param name="cBagName"></param>
    /// <param name="oOrder"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBOrdListAdd(cBagName AS STRING,oOrder AS OBJECT) AS LOGIC
    LOCAL oRDD := RDDHelpers.CWA("VODBOrdListAdd") AS IRDD
    IF oRDD != NULL                  
        VAR info := DbOrderInfo{}
        info:BagName := cBagName
        IF oOrder == NULL
            info:AllTags := TRUE
        ELSE
            info:Order   := oOrder
        ENDIF
        RETURN oRDD:OrderListAdd(info)
    ENDIF                            
    RETURN FALSE
    
    /// <summary>
    /// Remove orders from the order list in a work area and close associated index files.
    /// </summary>
    /// <param name="cBagName"></param>
    /// <param name="oOrder"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBOrdListClear(cBagName AS STRING,oOrder AS OBJECT) AS LOGIC
    LOCAL oRDD := RDDHelpers.CWA("VODBOrdListAdd") AS IRDD
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
    
    
    /// <summary>
    /// Rebuild all orders in the order list of a work area.
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBOrdListRebuild() AS LOGIC
    LOCAL oRDD := RDDHelpers.CWA("VODBOrdListRebuild") AS IRDD
    IF oRDD != NULL                  
        RETURN oRDD:OrderListRebuild()
    ENDIF                            
    RETURN FALSE
    
    /// <summary>
    /// Set the controlling order for a work area.
    /// </summary>
    /// <param name="cBagName"></param>
    /// <param name="oOrder"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBOrdSetFocus(cBagName AS STRING,oOrder AS OBJECT, strPreviousOrder REF STRING) AS LOGIC
    LOCAL oRDD := RDDHelpers.CWA("VODBOrdSetFocus") AS IRDD
    IF oRDD != NULL                     
        VAR info     := DbOrderInfo{}
        info:BagName := cBagName
        info:Order   := oOrder
        strPreviousOrder := String.Empty
        VAR result := oRDD:OrderListFocus(info)
        IF result .AND. info:Result IS STRING
            strPreviousOrder := (STRING)info:Result
        ENDIF
        RETURN result
    ENDIF                            
    RETURN FALSE
    
    /// <summary>
    /// Remove all records that have been marked for deletion from a database file.
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBPack() AS LOGIC
    LOCAL oRDD := RDDHelpers.CWA("VODBPack") AS IRDD
    IF oRDD != NULL                     
        RETURN oRDD:Pack()
    ENDIF                            
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
    RETURN (DWORD) VODBRddList():Length
    
    /// <summary>
    /// </summary>
    /// <param name="nOrdinal"></param>
    /// <param name="ptrRet"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBRDDInfo(nOrdinal AS DWORD,oRet REF OBJECT) AS LOGIC
    THROW  NotImplementedException{}

FUNCTION VODBRDDInfo(nOrdinal AS DWORD,oRet AS OBJECT) AS LOGIC
    THROW  NotImplementedException{}


[Obsolete( "'VODBRddList( rddList, nRddType )' is not supported, use VODBRddList() instead", TRUE )];
FUNCTION VODBRddList(rddList AS RddList,nRddType AS DWORD) AS LOGIC
    THROW  NotImplementedException{}
    
FUNCTION VODBRddList() AS STRING[]
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
    
    
    
    /// <summary>
    /// Return an RDD name.                  
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBRddName() AS STRING
    LOCAL oRDD := RDDHelpers.CWA("VODBRddName") AS IRDD
    IF oRDD != NULL                     
        RETURN oRDD:SysName
    ENDIF                            
    RETURN String.Empty
    
    /// <summary>
    /// Return and optionally change the default RDD for the application.
    /// </summary>
    /// <param name="cDrv"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBRddSetDefault(cDrv AS STRING) AS STRING
    THROW  NotImplementedException{}
    
    /// <summary>
    /// Restore the current record if it is marked for deletion.
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBRecall() AS LOGIC
    LOCAL oRDD := RDDHelpers.CWA("VODBRecall") AS IRDD
    IF oRDD != NULL                     
        RETURN oRDD:Recall()
    ENDIF                            
    RETURN FALSE
    
    /// <summary>
    /// Return the current record number.
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBRecno() AS OBJECT
    LOCAL oRDD := RDDHelpers.CWA("VODBRecno") AS IRDD
    IF oRDD != NULL                     
        RETURN oRDD:Recno
    ENDIF                            
    RETURN NULL
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBRecordGet() AS BYTE[]
    LOCAL oRDD := RDDHelpers.CWA("VODBRecordGet") AS IRDD
    IF oRDD != NULL                     
        RETURN oRDD:GetRec()
    ENDIF                            
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
    LOCAL oRDD := RDDHelpers.CWA("VODBRecordInfo") AS IRDD
    IF oRDD != NULL                     
        oRDD:RecInfo(oRecID, (INT) nOrdinal, oRet )
        RETURN TRUE
    ENDIF                            
    RETURN FALSE       
    
    /// <summary>
    /// </summary>
    /// <param name="pszRecord"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBRecordPut(aRecord AS BYTE[]) AS LOGIC
    LOCAL oRDD := RDDHelpers.CWA("VODBRecordPut") AS IRDD
    IF oRDD != NULL                     
        RETURN oRDD:PutRec(aRecord)
    ENDIF                            
    RETURN FALSE       
    
    /// <summary>
    /// Return the linking expression of a specified relation.
    /// </summary>
    /// <param name="nPos"></param>
    /// <param name="pszRel"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBRelation(nPos AS DWORD,sRel REF STRING) AS LOGIC
    LOCAL oRDD := RDDHelpers.CWA("VODBRecordPut") AS IRDD
    IF oRDD != NULL                     
        sRel :=  oRDD:RelText(nPos)
        RETURN TRUE
    ENDIF                            
    RETURN FALSE       
    
    /// <summary>
    /// Lock the current record.
    /// </summary>
    /// <param name="uRecId"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBRlock(uRecId AS OBJECT) AS LOGIC
    LOCAL oRDD := RDDHelpers.CWA("VODBRlock") AS IRDD
    IF oRDD != NULL
        LOCAL lockInfo AS DbLockInfo
        lockInfo := DbLockInfo{}
        lockInfo:RecId := uRecID
        lockInfo:@@METHOD  := DbLockInfo.LockMethod.Multiple
        RETURN oRDD:Lock(lockInfo)
    ENDIF                            
    RETURN FALSE 
    /// <summary>
    /// Return the work area number of a relation.
    /// </summary>
    /// <param name="nPos"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBRSelect(nPos AS DWORD) AS DWORD
    THROW  NotImplementedException{}
    
    /// <summary>
    /// Move to the record having the specified key value.
    /// </summary>
    /// <param name="oValue"></param>
    /// <param name="lSoftSeek"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBSeek(oValue AS OBJECT,lSoftSeek AS LOGIC) AS LOGIC
    LOCAL oRDD := RDDHelpers.CWA("VODBSeek") AS IRDD
    IF oRDD != NULL           
        VAR info 		:= DbSeekInfo{}
        info:Value 		:= oValue          
        info:SoftSeek 	:= lSoftSeek
        //info:Last		:= lLast 
        RETURN oRDD:Seek(info)
    ENDIF                            
    RETURN FALSE       
    
    /// <summary>
    /// Select a new work area and retrieve the current work area.
    /// </summary>
    /// <param name="nNew"></param>
    /// <param name="nOld"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBSelect(nNew AS DWORD,nOld REF DWORD ) AS LOGIC
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
    RETURN TRUE
    
    /// <summary>
    /// Set a filter condition.
    /// </summary>
    /// <param name="oBlock"></param>
    /// <param name="cFilter"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBSetFilter(oBlock AS ICodeBlock,cFilter AS STRING) AS LOGIC
    LOCAL oRDD := RDDHelpers.CWA("VODBSetFilter") AS IRDD
    IF oRDD != NULL           
        VAR info 		 := DbFilterInfo{}
        info:FilterBlock := oBlock         
        info:FilterText  := cFilter
        RETURN oRDD:SetFilter(info)
    ENDIF                            
    RETURN FALSE       
    
    /// <summary>
    /// Set the found flag.
    /// </summary>
    /// <param name="lFound"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBSetFound(lFound AS LOGIC) AS LOGIC
    LOCAL oRDD := RDDHelpers.CWA("VODBSetFilter") AS IRDD
    IF oRDD != NULL           
        oRDD:Found := TRUE
    ENDIF                            
    RETURN FALSE   
    
    /// <summary>
    /// Specify the code block for a locate condition.
    /// </summary>
    /// <param name="oBlock"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBSetLocate(oBlock AS ICodeBlock) AS LOGIC
    LOCAL oRDD := RDDHelpers.CWA("VODBSetLocate") AS IRDD
    IF oRDD != NULL        
        VAR scope := oRDD:GetScope()
        scope:ForBlock := oBlock
        oRDD:SetScope(scope)    
        RETURN TRUE
    ENDIF
    RETURN FALSE                            
    
    /// <summary>
    /// Relate a specified work area to the current work area.
    /// </summary>
    /// <param name="cAlias"></param>
    /// <param name="uCobKey"></param>
    /// <param name="cKey"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBSetRelation(cAlias AS STRING,uCobKey AS OBJECT,cKey AS STRING) AS LOGIC
    THROW  NotImplementedException{}
    
    /// <summary>
    /// </summary>
    /// <param name="scope"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBSetScope(scope AS DBSCOPEINFO) AS LOGIC
    LOCAL oRDD := RDDHelpers.CWA("VODBSetLocate") AS IRDD
    IF oRDD != NULL        
        RETURN oRDD:SetScope(scope)
    ENDIF                            
    RETURN FALSE
    /// <summary>
    /// Select a new work area.
    /// </summary>
    /// <param name="siNew"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBSetSelect(siNew AS INT) AS DWORD
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
    
    /// <summary>
    /// Move the record pointer relative to the current record.
    /// </summary>
    /// <param name="nRecords"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBSkip(nRecords AS LONG) AS LOGIC
    LOCAL oRDD := RDDHelpers.CWA("VODBSkip") AS IRDD
    IF oRDD != NULL        
        RETURN oRDD:Skip(nRecords)
    ENDIF                            
    RETURN FALSE   
    
    /// <summary>
    /// </summary>
    /// <param name="nRecords"></param>
    /// <param name="scope"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBSkipScope(nRecords AS LONG,scope AS DBSCOPEINFO) AS LOGIC
    LOCAL oRDD := RDDHelpers.CWA("VODBSkipScope") AS IRDD
    IF oRDD != NULL    
        oRDD:SetScope(scope)    
        RETURN oRDD:Skip(nRecords)
    ENDIF                            
    RETURN FALSE   
    
    /// <summary>
    /// </summary>
    /// <param name="nDest"></param>
    /// <param name="fnNames"></param>
    /// <param name="uCobFor"></param>
    /// <param name="uCobWhile"></param>
    /// <param name="nNext"></param>
    /// <param name="nRecno"></param>
    /// <param name="lRest"></param>
    /// <param name="fnSortNames"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBSort(nDest AS DWORD,fnNames AS DbFIELDNAMES,uCobFor AS OBJECT,uCobWhile AS OBJECT,;
nNext AS OBJECT,nRecno AS OBJECT,lRest AS LOGIC,fnSortNames AS DbFIELDNAMES) AS LOGIC
    THROW  NotImplementedException{}
    
    
    
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
FUNCTION VODBTrans(nDest AS DWORD,fldNames AS DbFieldNames,uCobFor AS OBJECT,uCobWhile AS OBJECT,;
nNext AS OBJECT,nRecno AS OBJECT,lRest AS LOGIC) AS LOGIC
    THROW  NotImplementedException{}
    
    /// <summary>
    /// </summary>
    /// <param name="nDest"></param>
    /// <param name="fldNames"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBTransRec(nDest AS DWORD,fldNames AS DbFieldNames) AS LOGIC
    THROW  NotImplementedException{}
    
    /// <summary>
    /// Release all locks for a work area.
    /// </summary>
    /// <param name="uRecno"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBUnlock(uRecno AS OBJECT) AS LOGIC
    THROW  NotImplementedException{}
    
    /// <summary>
    /// Release all locks for all work areas.
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION VODBUnlockAll() AS LOGIC
    THROW  NotImplementedException{}
    
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

    LOCAL oRdd := NULL  AS RegisteredRDD
    LOCAL ret := FALSE AS LOGIC
    FOREACH VAR name IN rddList:atomRddName
        oRdd := RegisteredRDD.Find(name)
        oRdd:Load()
    NEXT
    IF oRdd != NULL_OBJECT
        ret := VODBUseArea(lNew, oRdd:RddType, cName, cAlias, lShare, lReadOnly)
    ENDIF
    RETURN ret
FUNCTION VODBUseArea(lNew AS LOGIC,rddName AS STRING,cName AS STRING,cAlias AS STRING,lShare AS LOGIC,lReadOnly AS LOGIC) AS LOGIC
    LOCAL ret  := FALSE   AS LOGIC
    LOCAL rddType AS Type
    
    IF ( rddType := _VoDbRddNameToType( rddName ) ) == NULL
        PostArgumentError( "VODBUseArea", EDB_RDDNOTFOUND, "rddName", 3, <OBJECT>{ rddName } )
    ELSE
        ret := VODBUseArea( lNew, rddType, cName, cAlias, lShare, lReadOnly )
    ENDIF
    
    RETURN ret  
    
    
    
FUNCTION VODBUseArea(lNew AS LOGIC,rddType AS System.Type,cName AS STRING,cAlias AS STRING,lShare AS LOGIC,lReadOnly AS LOGIC) AS LOGIC
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
                    ret := rdd:Open( dboi ) 
                ENDIF
                IF ! ret
                    RuntimeState.Workareas:CloseArea(uiArea)
                ENDIF
                RuntimeState.Workareas:CurrentWorkAreaNO := uiArea
            ENDIF   
        ENDIF
    ENDIF
    
    RETURN ret
    
    
    
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
    LOCAL oRDD := RDDHelpers.CWA("VODBZap") AS IRDD
    IF oRDD != NULL
        RETURN oRDD:Zap()
    ENDIF                            
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
    IF (oRdd != NULL)   
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
