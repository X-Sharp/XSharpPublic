//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
#ifdef NOTDEFINED
USING XSharp
USING XSharp.RDD
USING XSharp.RDD.Enums
USING System.Collections.Generic
/// <summary>
/// Return the full path of the file
/// </summary>
/// <returns>
/// </returns>
FUNCTION DBF() AS STRING
	LOCAL oWA := RDDHelpers.CWA("DBF") AS IRDD
	IF oWA != NULL
		RETURN (STRING) oWA:Info(DBI_FULLPATH, NULL)
	ENDIF                            
	RETURN String.Empty

/// <summary>
/// Return the number of fields in the current database file.
/// </summary>
/// <returns>
/// </returns>
FUNCTION FCount() AS DWORD
	LOCAL oWA := RDDHelpers.CWA() AS IRDD
	IF (oWA != NULL)
		RETURN (DWORD) oWA:FieldCount
	ENDIF
	RETURN 0


/// <summary>
/// Return the name of a field as a string.
/// </summary>
/// <param name="dwFieldPos"></param>
/// <returns>
/// </returns>
FUNCTION FieldName(dwFieldPos AS DWORD) AS STRING
	LOCAL oWA := RDDHelpers.CWA("FieldName") AS IRDD
	IF (oWA != NULL)
		RETURN oWA:FieldName((INT) dwFieldPos)
	ENDIF
	RETURN String.Empty   



/// <summary>
/// Return the position of a field.
/// </summary>
/// <param name="sFieldName"></param>
/// <returns>
/// </returns>
FUNCTION FieldPos(sFieldName AS STRING) AS DWORD
	LOCAL oWA := RDDHelpers.CWA("FieldPos") AS IRDD
	IF (oWA != NULL)
		RETURN (DWORD) oWA:FieldIndex(sFieldName) 
	ENDIF
	RETURN 0   




/// <summary>
/// Return the alias of a specified work area as a string.
/// </summary>
/// <param name="nArea"></param>
/// <returns>
/// </returns>
FUNCTION VODBAlias(nArea AS DWORD) AS STRING
	LOCAL oWAS AS WorkAreas
	oWAS := RddHelpers.WAS()
	RETURN oWAS:GetAlias((INT) nArea)

/// <summary>
/// Add a new record.
/// </summary>
/// <param name="lReleaseLocks"></param>
/// <returns>
/// </returns>
FUNCTION VODBAppend(lReleaseLocks AS LOGIC) AS LOGIC
	LOCAL oWA := RDDHelpers.CWA("VODBAppend") AS IRDD
	IF (oWA != NULL)
		RETURN oWA:Append(lReleaseLocks)
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
	LOCAL oWA := RDDHelpers.CWA("VODBBlobInfo") AS IRDD
	IF (oWA != NULL)
		ptrRet := oWA:BlobInfo(nOrdinal, nPos)
		RETURN TRUE
	ENDIF
	RETURN FALSE   

/// <summary>
/// Determine when beginning-of-file is encountered.
/// </summary>
/// <returns>
/// </returns>
FUNCTION VODBBof() AS LOGIC
	LOCAL oWA := RDDHelpers.CWA("VODBBof") AS IRDD
	IF (oWA != NULL)
		RETURN oWA:BoF
	ENDIF
	
	RETURN FALSE   

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION VODBBuffRefresh() AS LOGIC
	LOCAL oWA := RDDHelpers.CWA("VODBBuffRefresh") AS IRDD
	IF (oWA != NULL)
		oWA:RecInfo(0, DbRecordInfo.DBRI_Updated,NULL)
		RETURN TRUE
	ENDIF
	RETURN FALSE   

/// <summary>
/// Clear a logical filter condition.
/// </summary>
/// <returns>
/// </returns>
FUNCTION VODBClearFilter() AS LOGIC
	LOCAL oWA := RDDHelpers.CWA("VODBClearFilter") AS IRDD
	IF (oWA != NULL)
		RETURN oWA:ClearFilter()
	ENDIF
	RETURN FALSE   

/// <summary>
/// Clear a locate condition by deleting the locate code block.
/// </summary>
/// <returns>
/// </returns>
FUNCTION VODBClearLocate() AS LOGIC
	LOCAL oWA := RDDHelpers.CWA("VODBClearLocate") AS IRDD
	IF (oWA != NULL)
		RETURN oWA:ClearScope()
	ENDIF
	RETURN FALSE   

/// <summary>
/// Clear any active relations.
/// </summary>
/// <returns>
/// </returns>
FUNCTION VODBClearRelation() AS LOGIC
	LOCAL oWA := RDDHelpers.CWA("VODBClearRelation") AS IRDD
	IF (oWA != NULL)
		RETURN oWA:ClearRel()
	ENDIF
	RETURN FALSE   

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION VODBClearScope() AS LOGIC
	LOCAL oWA := RDDHelpers.CWA("VODBClearScope") AS IRDD
	IF (oWA != NULL)
		RETURN oWA:ClearScope()
	ENDIF
	RETURN FALSE   

/// <summary>
/// Close all files in all work areas.
/// </summary>
/// <returns>
/// </returns>
FUNCTION VODBCloseAll() AS LOGIC
	LOCAL oWAS AS WorkAreas
	oWAS := RddHelpers.WAS()
	RETURN oWAS:CloseAll()

/// <summary>
/// Close all files in a work area.
/// </summary>
/// <returns>
/// </returns>
FUNCTION VODBCloseArea() AS LOGIC
	LOCAL oWA := RDDHelpers.CWA("VODBFlock") AS IRDD
	IF (oWA != NULL)
		RETURN oWA:Close()
	ENDIF
	RETURN FALSE   

/// <summary>
/// Flush pending updates in one work area.
/// </summary>
/// <returns>
/// </returns>
FUNCTION VODBCommit() AS LOGIC
	LOCAL oWA := RDDHelpers.CWA("VODBCommit") AS IRDD
	IF (oWA != NULL)
		RETURN oWA:Flush()
	ENDIF
	RETURN FALSE   

/// <summary>
/// Flush pending updates in all work areas.
/// </summary>
/// <returns>
/// </returns>
FUNCTION VODBCommitAll() AS LOGIC
	LOCAL oWAS AS WorkAreas
	oWAS := RddHelpers.WAS()
	RETURN oWAS:CommitAll()

/// <summary>
/// Resume a pending locate condition.
/// </summary>
/// <returns>
/// </returns>
FUNCTION VODBContinue() AS LOGIC
	LOCAL oWA := RDDHelpers.CWA("VODBContinue") AS IRDD
	IF (oWA != NULL)
		RETURN oWA:Continue()
	ENDIF
	RETURN FALSE   

FUNCTION VODBCreate( cName AS STRING, aStru AS List<RddFieldInfo>, rddName AS STRING, lNew AS LOGIC, cAlias AS STRING, cDelim AS STRING, lKeep AS LOGIC, lJustOpen AS LOGIC ) AS LOGIC
   LOCAL i           AS DWORD
   LOCAL oRdd        AS RegisteredRDD
   LOCAL ret     AS LOGIC
   FOR i := 1 TO rddList:atomRddName:Length 
        oRdd := RegisteredRDD.Find(rddList:atomRddName[i])
        oRdd:Load()
   NEXT
   IF (oRdd != NULL_OBJECT)
        ret := VODBCreate(cName, aStru, oRdd:RddType, lNew, cAlias, cDelim, lKeep, lJustOpen)
   ENDIF
   RETURN ret  


FUNCTION VODBCreate( cName AS STRING, aStru AS List<RddFieldInfo>, rddName AS System.Type, lNew AS LOGIC, cAlias AS STRING, cDelim AS STRING, lKeep AS LOGIC, lJustOpen AS LOGIC ) AS LOGIC

   RETURN FALSE

/// <summary>
/// Mark the current record for deletion.
/// </summary>
/// <returns>
/// </returns>
FUNCTION VODBDelete() AS LOGIC
	LOCAL oWA := RDDHelpers.CWA("VODBDelete") AS IRDD
	IF (oWA != NULL)
		RETURN oWA:Delete()
	ENDIF
	RETURN FALSE   

/// <summary>
/// Return the deleted status of the current record.
/// </summary>
/// <returns>
/// </returns>
FUNCTION VODBDeleted() AS LOGIC
	LOCAL oWA := RDDHelpers.CWA("VODBDeleted") AS IRDD
	IF (oWA != NULL)
		RETURN oWA:Deleted
	ENDIF
	RETURN FALSE   

/// <summary>
/// Determine when end-of-file is encountered.
/// </summary>
/// <returns>
/// </returns>
FUNCTION VODBEof() AS LOGIC
	LOCAL oWA := RDDHelpers.CWA("VODBEof") AS IRDD
	IF (oWA != NULL)
		RETURN oWA:EoF
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
	LOCAL oWA := RDDHelpers.CWA("VODBFieldGet") AS IRDD
	IF (oWA != NULL)
		ptrRet := oWA:GetValue((INT) nPos)
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
	LOCAL oWA := RDDHelpers.CWA("VODBFieldInfo") AS IRDD
	IF (oWA != NULL)
		ptrRet := oWA:FieldInfo((INT) nPos, (INT) nOrdinal, ptrRet)
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
	LOCAL oWA := RDDHelpers.CWA("VODBFieldPut") AS IRDD
	IF (oWA != NULL)
		RETURN oWA:PutValue((INT) nPos, xValue)
	ENDIF
	RETURN FALSE   

/// <summary>
/// </summary>
/// <param name="nPos"></param>
/// <param name="cFile"></param>
/// <returns>
/// </returns>
FUNCTION VODBFileGet(nPos AS DWORD,cFile AS STRING) AS LOGIC
	LOCAL oWA := RDDHelpers.CWA("VODBFileGet") AS IRDD
	IF (oWA != NULL)
		RETURN oWA:GetValueFile((INT) nPos, cFile)
	ENDIF
	RETURN FALSE   

/// <summary>
/// </summary>
/// <param name="nPos"></param>
/// <param name="cFile"></param>
/// <returns>
/// </returns>
FUNCTION VODBFilePut(nPos AS DWORD,cFile AS STRING) AS LOGIC
	LOCAL oWA := RDDHelpers.CWA("VODBFilePut") AS IRDD
	IF (oWA != NULL)
		RETURN oWA:PutValueFile((INT) nPos, cFile)
	ENDIF
	RETURN FALSE   

/// <summary>
/// Return a filter.
/// </summary>
/// <returns>
/// </returns>
FUNCTION VODBFilter() AS STRING
	LOCAL oWA := RDDHelpers.CWA("VODBFilter") AS IRDD
	IF (oWA != NULL)
		RETURN oWA:FilterText
	ENDIF
	
	RETURN string.Empty   

/// <summary>
/// Lock an opened and shared database file.
/// </summary>
/// <returns>
/// </returns>
FUNCTION VODBFlock() AS LOGIC
	VAR oWA := RDDHelpers.CWA("VODBFlock")
	IF (oWA != NULL)
		 LOCAL dbli AS DbLockInfo
         dbli := DbLockInfo{}
         dbli:Result := FALSE
         dbli:@@METHOD := DbLockInfo.LockMethod.File
		 RETURN oWA:Lock(dbli)
	ENDIF
	RETURN FALSE   

/// <summary>
/// Determine if the previous search operation succeeded.
/// </summary>
/// <returns>
/// </returns>
FUNCTION VODBFound() AS LOGIC
	VAR oWA := RDDHelpers.CWA("VODBFound")
	IF (oWA != NULL)
		RETURN oWA:Found
	ENDIF
	RETURN FALSE   


/// <summary>
/// Return the work area number.
/// </summary>
/// <returns>
/// </returns>
FUNCTION VODBGetSelect() AS INT
	LOCAL oWA := RDDHelpers.CWA("VODBGetSelect") AS IRDD
	IF oWA != NULL
		RETURN oWA:Area
	ENDIF                            
	RETURN 0

/// <summary>
/// Move to the last logical record.
/// </summary>
/// <returns>
/// </returns>
FUNCTION VODBGoBottom() AS LOGIC
	LOCAL oWA := RDDHelpers.CWA("VODBGoBottom") AS IRDD
	IF (oWA != NULL)
		RETURN oWA:GoBottom()
	ENDIF
	RETURN FALSE   

/// <summary>
/// Move to a record specified by record number.
/// </summary>
/// <param name="uRecId"></param>
/// <returns>
/// </returns>
FUNCTION VODBGoto(uRecId AS OBJECT) AS LOGIC
	LOCAL oWA := RDDHelpers.CWA("VODBGoto") AS IRDD
	IF (oWA != NULL)
		RETURN oWA:GoToId(uRecID)
	ENDIF
	RETURN FALSE   

/// <summary>
/// Move to the first logical record.
/// </summary>
/// <returns>
/// </returns>
FUNCTION VODBGoTop() AS LOGIC
	LOCAL oWA := RDDHelpers.CWA("VODBGoTop") AS IRDD
	IF (oWA != NULL)
		RETURN oWA:GoTop()
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
	LOCAL oWA := RDDHelpers.CWA("VODBInfo") AS IRDD
	IF (oWA != NULL)
		ptrRet := oWA:Info((INT) nOrdinal, ptrRet)
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
	LOCAL oWA := RDDHelpers.CWA("VODBLastRec") AS IRDD
	IF (oWA != NULL)
		RETURN oWA:RecCount
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
	LOCAL oWA := RDDHelpers.CWA("VODbMemoExt") AS IRDD
	IF oWA != NULL
		RETURN (STRING) oWA:Info(DBI_MEMOEXT, NULL)
	ENDIF                            
	RETURN String.Empty
/// <summary>
/// Return the default index file extension for a work area as defined by the its RDD.
/// </summary>
/// <returns>Default extension for the current workarea, or an empty string when no table is open in the current workarea.
/// </returns>
FUNCTION VODBOrdBagExt() AS STRING
	LOCAL oWA := RDDHelpers.CWA("VODBOrdBagExt") AS IRDD
	IF oWA != NULL
        VAR info := XSharp.RDD.DbOrderInfo{}
		RETURN (STRING) oWA:OrderInfo( DBOI_BAGEXT, info)
	ENDIF                            
	RETURN String.Empty

/// <summary>
/// Set the condition and scope for an order.
/// </summary>
/// <param name="ptrCondInfo"></param>
/// <returns>
/// </returns>
FUNCTION VODBOrdCondSet(ordCondInfo AS DbOrderCondInfo) AS LOGIC
	LOCAL oWA := RDDHelpers.CWA("VODBOrdCondSet") AS IRDD
	IF oWA != NULL
		RETURN oWA:OrderCondition(ordCondInfo)
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
	LOCAL oWA := RDDHelpers.CWA("VODBOrdCreate") AS IRDD
	IF oWA != NULL                  
		VAR info := DbOrderCreateInfo{}
		info:BagName 		:= cBagName
		info:Order			:= oOrder
		info:Expression 	:= cExpr
		info:Block      	:= oCodeBlock
		info:Unique			:= lUnique
		info:OrdCondInfo 	:= ordCondInfo		
		RETURN oWA:OrderCreate(info)
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
	LOCAL oWA := RDDHelpers.CWA("VODBOrdDestroy") AS IRDD
	IF oWA != NULL                  
		VAR info := XSharp.RDD.DbOrderInfo{}
		info:BagName := cBagName
		info:Order   := oOrder
		RETURN oWA:OrderDestroy(info)
	ENDIF                            
	RETURN FALSE

/// <summary>
/// Return information about index files and the orders in them.
/// </summary>
/// <param name="nOrdinal"></param>
/// <param name="cBagName"></param>
/// <param name="uOrder"></param>
/// <param name="ptrRet"></param>
/// <returns>
/// </returns>
FUNCTION VODBOrderInfo(nOrdinal AS DWORD,cBagName AS STRING,oOrder AS OBJECT,ptrRet REF OBJECT) AS LOGIC
	LOCAL oWA := RDDHelpers.CWA("VODBOrdDestroy") AS IRDD
	IF oWA != NULL                  
		VAR info := XSharp.RDD.DbOrderInfo{}
		info:BagName := cBagName
		info:Order   := oOrder
		ptrRet :=  oWA:OrderInfo(nOrdinal, info)
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
	LOCAL oWA := RDDHelpers.CWA("VODBOrdListAdd") AS IRDD
	IF oWA != NULL                  
		VAR info := XSharp.RDD.DbOrderInfo{}
		info:BagName := cBagName
        IF oOrder == NULL
            info:AllTags := TRUE
        ELSE
		    info:Order   := oOrder
        ENDIF
		RETURN oWA:OrderListAdd(info)
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
	LOCAL oWA := RDDHelpers.CWA("VODBOrdListAdd") AS IRDD
	IF oWA == NULL
        RETURN TRUE // not logical but compatible with VO
    ELSE
		VAR info := XSharp.RDD.DbOrderInfo{}
		info:BagName := cBagName
        IF oOrder == NULL
            info:AllTags := TRUE
        ELSE
		    info:Order   := oOrder
        ENDIF
        RETURN oWA:OrderListDelete(info) 
    ENDIF


/// <summary>
/// Rebuild all orders in the order list of a work area.
/// </summary>
/// <returns>
/// </returns>
FUNCTION VODBOrdListRebuild() AS LOGIC
	LOCAL oWA := RDDHelpers.CWA("VODBOrdListRebuild") AS IRDD
	IF oWA != NULL                  
		RETURN oWA:OrderListRebuild()
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
	LOCAL oWA := RDDHelpers.CWA("VODBOrdSetFocus") AS IRDD
	IF oWA != NULL                     
		VAR info := XSharp.RDD.DbOrderInfo{}
		info:BagName := cBagName
		info:Order   := oOrder
        strPreviousOrder := String.Empty
        VAR result := oWA:OrderListFocus(info)
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
	LOCAL oWA := RDDHelpers.CWA("VODBPack") AS IRDD
	IF oWA != NULL                     
		RETURN oWA:Pack()
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

[Obsolete( "'VODBRddList( rddList, nRddType )' is not supported, use VODBRddList() instead", TRUE )];
FUNCTION VODBRddList(rddList AS RddList,nRddType AS DWORD) AS LOGIC
	THROW  NotImplementedException{}

FUNCTION VODBRddList() AS STRING[]
	LOCAL aList AS List<STRING>
    
    aList := List<STRING>{}
    LOCAL oWAs := Workareas.GetInstance() AS Workareas
    FOR VAR i := 1 TO WorkAreas.MaxWorkAreas
        VAR oRDD := oWAs.GetRDD(i)
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
	LOCAL oWA := RDDHelpers.CWA("VODBRddName") AS IRDD
	IF oWA != NULL                     
		RETURN oWA:SysName
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
	LOCAL oWA := RDDHelpers.CWA("VODBRecall") AS IRDD
	IF oWA != NULL                     
		RETURN oWA:Recall()
	ENDIF                            
	RETURN FALSE

/// <summary>
/// Return the current record number.
/// </summary>
/// <returns>
/// </returns>
FUNCTION VODBRecno() AS OBJECT
	LOCAL oWA := RDDHelpers.CWA("VODBRecno") AS IRDD
	IF oWA != NULL                     
		RETURN oWA:RecId
	ENDIF                            
	RETURN NULL

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION VODBRecordGet() AS BYTE[]
	LOCAL oWA := RDDHelpers.CWA("VODBRecordGet") AS IRDD
	IF oWA != NULL                     
		RETURN oWA:GetRec()
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
	LOCAL oWA := RDDHelpers.CWA("VODBRecordInfo") AS IRDD
	IF oWA != NULL                     
		oWA:RecInfo(oRecID, (INT) nOrdinal, oRet )
		RETURN TRUE
	ENDIF                            
	RETURN FALSE       

/// <summary>
/// </summary>
/// <param name="pszRecord"></param>
/// <returns>
/// </returns>
FUNCTION VODBRecordPut(aRecord AS BYTE[]) AS LOGIC
	LOCAL oWA := RDDHelpers.CWA("VODBRecordPut") AS IRDD
	IF oWA != NULL                     
		RETURN oWA:PutRec(aRecord)
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
	LOCAL oWA := RDDHelpers.CWA("VODBRecordPut") AS IRDD
	IF oWA != NULL                     
		sRel :=  oWA:RelText(nPos)
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
	LOCAL oWA := RDDHelpers.CWA("VODBRlock") AS IRDD
	IF oWA != NULL
        LOCAL lockInfo AS DbLockInfo
        lockInfo := DbLockInfo{}
        lockInfo:RecId := uRecID
        lockInfo:@@METHOD  := DbLockInfo.LockMethod.Multiple
		RETURN oWA:Lock(lockInfo)
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
	LOCAL oWA := RDDHelpers.CWA("VODBSeek") AS IRDD
	IF oWA != NULL           
		VAR info 		:= DbSeekInfo{}
		info:Value 		:= oValue          
		info:SoftSeek 	:= lSoftSeek
		//info:Last		:= lLast 
		RETURN oWA:Seek(info)
	ENDIF                            
	RETURN FALSE       

/// <summary>
/// Select a new work area and retrieve the current work area.
/// </summary>
/// <param name="nNew"></param>
/// <param name="riOld"></param>
/// <returns>
/// </returns>
FUNCTION VODBSelect(nNew AS DWORD,riOld AS OBJECT) AS LOGIC
	THROW  NotImplementedException{}

/// <summary>
/// Set a filter condition.
/// </summary>
/// <param name="oBlock"></param>
/// <param name="cFilter"></param>
/// <returns>
/// </returns>
FUNCTION VODBSetFilter(oBlock AS ICodeBlock,cFilter AS STRING) AS LOGIC
	LOCAL oWA := RDDHelpers.CWA("VODBSetFilter") AS IRDD
	IF oWA != NULL           
		VAR info 		 := DbFilterInfo{}
		info:FilterBlock := oBlock         
		info:FilterText  := cFilter
		RETURN oWA:SetFilter(info)
	ENDIF                            
	RETURN FALSE       

/// <summary>
/// Set the found flag.
/// </summary>
/// <param name="lFound"></param>
/// <returns>
/// </returns>
FUNCTION VODBSetFound(lFound AS LOGIC) AS LOGIC
	LOCAL oWA := RDDHelpers.CWA("VODBSetFilter") AS IRDD
	IF oWA != NULL           
		oWA:Found := TRUE
	ENDIF                            
RETURN FALSE   

/// <summary>
/// Specify the code block for a locate condition.
/// </summary>
/// <param name="oBlock"></param>
/// <returns>
/// </returns>
FUNCTION VODBSetLocate(oBlock AS ICodeBlock) AS LOGIC
	LOCAL oWA := RDDHelpers.CWA("VODBSetLocate") AS IRDD
	IF oWA != NULL        
		VAR scope := oWA:GetScope()
		scope:ForBlock := oBlock
		oWA:SetScope(scope)    
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
	LOCAL oWA := RDDHelpers.CWA("VODBSetLocate") AS IRDD
	IF oWA != NULL        
		RETURN oWA:SetScope(scope)
	ENDIF                            
	RETURN FALSE
/// <summary>
/// Select a new work area.
/// </summary>
/// <param name="siNew"></param>
/// <returns>
/// </returns>
FUNCTION VODBSetSelect(siNew AS INT) AS INT
	THROW  NotImplementedException{}

/// <summary>
/// Move the record pointer relative to the current record.
/// </summary>
/// <param name="nRecords"></param>
/// <returns>
/// </returns>
FUNCTION VODBSkip(nRecords AS LONG) AS LOGIC
	LOCAL oWA := RDDHelpers.CWA("VODBSkip") AS IRDD
	IF oWA != NULL        
		RETURN oWA:Skip(nRecords)
	ENDIF                            
	RETURN FALSE   

/// <summary>
/// </summary>
/// <param name="nRecords"></param>
/// <param name="scope"></param>
/// <returns>
/// </returns>
FUNCTION VODBSkipScope(nRecords AS LONG,scope AS DBSCOPEINFO) AS LOGIC
	LOCAL oWA := RDDHelpers.CWA("VODBSkipScope") AS IRDD
	IF oWA != NULL    
		oWA:SetScope(scope)    
		RETURN oWA:Skip(nRecords)
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

FUNCTION VODBUseArea(lNew AS LOGIC,rddList AS RDDLIST,cName AS STRING,cAlias AS STRING,lShare AS LOGIC,lReadOnly AS LOGIC) AS LOGIC
	THROW  NotImplementedException{}
//	LOCAL oWA := RDDHelpers.CWA("VODBUseArea") AS IRDD
//	IF oWA != NULL
//		
//	ENDIF                            

/// <summary>
/// Remove all records from open files.
/// </summary>
/// <returns>
/// </returns>
FUNCTION VODBZap() AS LOGIC
	LOCAL oWA := RDDHelpers.CWA("VODBZap") AS IRDD
	IF oWA != NULL
		RETURN oWA:Zap()
	ENDIF                            
	RETURN FALSE

#endif
