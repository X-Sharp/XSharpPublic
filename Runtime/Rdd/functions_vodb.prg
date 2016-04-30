//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
begin namespace XSharp.RDD
	#region functions
	/// <summary>
	/// Return the alias of a specified work area as a string.
	/// </summary>
	/// <param name="nArea"></param>
	/// <returns>
	/// </returns>
	FUNCTION VODBAlias(nArea AS DWORD) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Return the alias of a specified work area as a symbol.
	/// </summary>
	/// <param name="nArea"></param>
	/// <returns>
	/// </returns>
	FUNCTION VODBAliasSym(nArea AS DWORD) AS SYMBOL
		/// THROW NotImplementedException{}
	RETURN NULL_SYMBOL   

	/// <summary>
	/// Add a new record.
	/// </summary>
	/// <param name="lRlsLcks"></param>
	/// <returns>
	/// </returns>
	FUNCTION VODBAppend(lRlsLcks AS LOGIC) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <param name="nOrdinal"></param>
	/// <param name="nPos"></param>
	/// <param name="ptrRet"></param>
	/// <returns>
	/// </returns>
	FUNCTION VODBBlobInfo(nOrdinal AS DWORD,nPos AS DWORD,ptrRet AS PTR) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Determine when beginning-of-file is encountered.
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION VODBBof() AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION VODBBuffRefresh() AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Clear a logical filter condition.
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION VODBClearFilter() AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Clear a locate condition by deleting the locate code block.
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION VODBClearLocate() AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Clear any active relations.
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION VODBClearRelation() AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION VODBClearScope() AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Close all files in all work areas.
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION VODBCloseAll() AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Close all files in a work area.
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION VODBCloseArea() AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Flush pending updates in one work area.
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION VODBCommit() AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Flush pending updates in all work areas.
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION VODBCommitAll() AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Resume a pending locate condition.
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION VODBContinue() AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <param name="cName"></param>
	/// <param name="aStru"></param>
	/// <param name="rddL"></param>
	/// <param name="lNew"></param>
	/// <param name="cAlias"></param>
	/// <param name="cDelim"></param>
	/// <param name="lKeep"></param>
	/// <param name="lOpen"></param>
	/// <returns>
	/// </returns>
	FUNCTION VODBCreate(cName AS STRING,aStru AS ARRAY,rddL AS _RDDLIST,lNew AS LOGIC,cAlias AS STRING,cDelim AS STRING,lKeep AS LOGIC,lOpen AS LOGIC) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Mark the current record for deletion.
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION VODBDelete() AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Return the deleted status of the current record.
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION VODBDeleted() AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Determine when end-of-file is encountered.
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION VODBEof() AS LOGIC
		/// THROW NotImplementedException{}
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
	FUNCTION VODBEval(uBlock AS USUAL,uCobFor AS USUAL,uCobWhile AS USUAL,nNext AS USUAL,nRecno AS USUAL,lRest AS LOGIC) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION VODBExit() AS INT
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Retrieve the value of a specified database field.
	/// </summary>
	/// <param name="nPos"></param>
	/// <param name="ptrRet"></param>
	/// <returns>
	/// </returns>
	FUNCTION VODBFieldGet(nPos AS DWORD,ptrRet AS PTR) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Retrieve field definition information about a field.
	/// </summary>
	/// <param name="nOrdinal"></param>
	/// <param name="nPos"></param>
	/// <param name="ptrRet"></param>
	/// <returns>
	/// </returns>
	FUNCTION VODBFieldInfo(nOrdinal AS DWORD,nPos AS DWORD,ptrRet AS PTR) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Set the value of a specified database field.
	/// </summary>
	/// <param name="nPos"></param>
	/// <param name="xValue"></param>
	/// <returns>
	/// </returns>
	FUNCTION VODBFieldPut(nPos AS DWORD,xValue AS USUAL) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <param name="nPos"></param>
	/// <param name="cFile"></param>
	/// <returns>
	/// </returns>
	FUNCTION VODBFileGet(nPos AS DWORD,cFile AS STRING) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <param name="nPos"></param>
	/// <param name="cFile"></param>
	/// <returns>
	/// </returns>
	FUNCTION VODBFilePut(nPos AS DWORD,cFile AS STRING) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Return a filter.
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION VODBFilter() AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Lock an opened and shared database file.
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION VODBFlock() AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Determine if the previous search operation succeeded.
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION VODBFound() AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION VODBFreeDriver() AS VOID
		/// THROW NotImplementedException{}
	RETURN

	/// <summary>
	/// Return the work area number.
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION VODBGetSelect() AS INT
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Move to the last logical record.
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION VODBGoBottom() AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Move to a record specified by record number.
	/// </summary>
	/// <param name="uRecId"></param>
	/// <returns>
	/// </returns>
	FUNCTION VODBGoto(uRecId AS USUAL) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Move to the first logical record.
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION VODBGoTop() AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Retrieve information about a work area.
	/// </summary>
	/// <param name="nOrdinal"></param>
	/// <param name="ptrRet"></param>
	/// <returns>
	/// </returns>
	FUNCTION VODBInfo(nOrdinal AS DWORD,ptrRet AS PTR) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <param name="nSelect"></param>
	/// <param name="struList"></param>
	/// <returns>
	/// </returns>
	FUNCTION VODBJoinAppend(nSelect AS DWORD,struList AS _JOINLIST) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Return the number of the last record in a database file.
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION VODBLastRec() AS LONG
		/// THROW NotImplementedException{}
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
	FUNCTION VODBLocate(uCobFor AS USUAL,uCobWhile AS USUAL,nNext AS LONG,uRecId AS USUAL,lRest AS LOGIC) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <param name="cDriver"></param>
	/// <returns>
	/// </returns>
	FUNCTION VODBMemoExt(cDriver AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Return the default index file extension for a work area as defined by the its RDD.
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION VODBOrdBagExt() AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Set the condition and scope for an order.
	/// </summary>
	/// <param name="ptrCondInfo"></param>
	/// <returns>
	/// </returns>
	FUNCTION VODBOrdCondSet(ptrCondInfo AS PTR) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Create or replace an order in an index file.
	/// </summary>
	/// <param name="cBagName"></param>
	/// <param name="uOrder"></param>
	/// <param name="cExpr"></param>
	/// <param name="uCobExpr"></param>
	/// <param name="lUnique"></param>
	/// <param name="ptrCondInfo"></param>
	/// <returns>
	/// </returns>
	FUNCTION VODBOrdCreate(cBagName AS STRING,uOrder AS USUAL,cExpr AS STRING,uCobExpr AS USUAL,lUnique AS LOGIC,ptrCondInfo AS PTR) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Remove an order from an open index file.
	/// </summary>
	/// <param name="cOrdBag"></param>
	/// <param name="uOrder"></param>
	/// <returns>
	/// </returns>
	FUNCTION VODBOrdDestroy(cOrdBag AS STRING,uOrder AS USUAL) AS LOGIC
		/// THROW NotImplementedException{}
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
	FUNCTION VODBOrderInfo(nOrdinal AS DWORD,cBagName AS STRING,uOrder AS USUAL,ptrRet AS PTR) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <param name="ost"></param>
	/// <returns>
	/// </returns>
	FUNCTION VODBOrderStatus(ost AS _ORDERSTATUS) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Open an index file and add specified orders to the order list in a work area.
	/// </summary>
	/// <param name="cOrdBag"></param>
	/// <param name="uOrder"></param>
	/// <returns>
	/// </returns>
	FUNCTION VODBOrdListAdd(cOrdBag AS STRING,uOrder AS USUAL) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Remove orders from the order list in a work area and close associated index files.
	/// </summary>
	/// <param name="cOrdBag"></param>
	/// <param name="uOrder"></param>
	/// <returns>
	/// </returns>
	FUNCTION VODBOrdListClear(cOrdBag AS STRING,uOrder AS USUAL) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Rebuild all orders in the order list of a work area.
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION VODBOrdListRebuild() AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Set the controlling order for a work area.
	/// </summary>
	/// <param name="cOrdBag"></param>
	/// <param name="uOrder"></param>
	/// <param name="pszOrder"></param>
	/// <returns>
	/// </returns>
	FUNCTION VODBOrdSetFocus(cOrdBag AS STRING,uOrder AS USUAL,pszOrder AS USUAL) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Remove all records that have been marked for deletion from a database file.
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION VODBPack() AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <param name="nRddType"></param>
	/// <returns>
	/// </returns>
	FUNCTION VODBRddCount(nRddType AS DWORD) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// </summary>
	/// <param name="nOrdinal"></param>
	/// <param name="ptrRet"></param>
	/// <returns>
	/// </returns>
	FUNCTION VODBRDDInfo(nOrdinal AS DWORD,ptrRet AS PTR) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Get a list of RDDs in use.
	/// </summary>
	/// <param name="rddList"></param>
	/// <param name="nRddType"></param>
	/// <returns>
	/// </returns>
	FUNCTION VODBRddList(rddList AS _RDDLIST,nRddType AS DWORD) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Return an RDD name.
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION VODBRddName() AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Return and optionally change the default RDD for the application.
	/// </summary>
	/// <param name="cDrv"></param>
	/// <returns>
	/// </returns>
	FUNCTION VODBRddSetDefault(cDrv AS STRING) AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Restore the current record if it is marked for deletion.
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION VODBRecall() AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Return the current record number.
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION VODBRecno() AS USUAL
		/// THROW NotImplementedException{}
	RETURN NIL   

	/// <summary>
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION VODBRecordGet() AS STRING
		/// THROW NotImplementedException{}
	RETURN NULL_STRING   

	/// <summary>
	/// Retrieve information about a record.
	/// </summary>
	/// <param name="nOrdinal"></param>
	/// <param name="uRecId"></param>
	/// <param name="ptrRet"></param>
	/// <returns>
	/// </returns>
	FUNCTION VODBRecordInfo(nOrdinal AS DWORD,uRecId AS USUAL,ptrRet AS PTR) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <param name="pszRecord"></param>
	/// <returns>
	/// </returns>
	FUNCTION VODBRecordPut(pszRecord AS PSZ) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Return the linking expression of a specified relation.
	/// </summary>
	/// <param name="nPos"></param>
	/// <param name="pszRel"></param>
	/// <returns>
	/// </returns>
	FUNCTION VODBRelation(nPos AS DWORD,pszRel AS USUAL) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Lock the current record.
	/// </summary>
	/// <param name="uRecId"></param>
	/// <returns>
	/// </returns>
	FUNCTION VODBRlock(uRecId AS USUAL) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Return the work area number of a relation.
	/// </summary>
	/// <param name="nPos"></param>
	/// <returns>
	/// </returns>
	FUNCTION VODBRSelect(nPos AS DWORD) AS DWORD
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Move to the record having the specified key value.
	/// </summary>
	/// <param name="xValue"></param>
	/// <param name="lSoft"></param>
	/// <returns>
	/// </returns>
	FUNCTION VODBSeek(xValue AS USUAL,lSoft AS LOGIC) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Select a new work area and retrieve the current work area.
	/// </summary>
	/// <param name="nNew"></param>
	/// <param name="riOld"></param>
	/// <returns>
	/// </returns>
	FUNCTION VODBSelect(nNew AS DWORD,riOld AS USUAL) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Set a filter condition.
	/// </summary>
	/// <param name="uCobFilter"></param>
	/// <param name="cFilter"></param>
	/// <returns>
	/// </returns>
	FUNCTION VODBSetFilter(uCobFilter AS USUAL,cFilter AS STRING) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Set the found flag.
	/// </summary>
	/// <param name="lFound"></param>
	/// <returns>
	/// </returns>
	FUNCTION VODBSetFound(lFound AS LOGIC) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Specify the code block for a locate condition.
	/// </summary>
	/// <param name="uCobFor"></param>
	/// <returns>
	/// </returns>
	FUNCTION VODBSetLocate(uCobFor AS USUAL) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Relate a specified work area to the current work area.
	/// </summary>
	/// <param name="cAlias"></param>
	/// <param name="uCobKey"></param>
	/// <param name="cKey"></param>
	/// <returns>
	/// </returns>
	FUNCTION VODBSetRelation(cAlias AS STRING,uCobKey AS USUAL,cKey AS STRING) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <param name="ptrdbsci"></param>
	/// <returns>
	/// </returns>
	FUNCTION VODBSetScope(ptrdbsci AS _DBSCOPEINFO) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Select a new work area.
	/// </summary>
	/// <param name="siNew"></param>
	/// <returns>
	/// </returns>
	FUNCTION VODBSetSelect(siNew AS INT) AS INT
		/// THROW NotImplementedException{}
	RETURN 0   

	/// <summary>
	/// Move the record pointer relative to the current record.
	/// </summary>
	/// <param name="nRecords"></param>
	/// <returns>
	/// </returns>
	FUNCTION VODBSkip(nRecords AS LONG) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <param name="nRecords"></param>
	/// <param name="ptrdbsci"></param>
	/// <returns>
	/// </returns>
	FUNCTION VODBSkipScope(nRecords AS LONG,ptrdbsci AS _DBSCOPEINFO) AS LOGIC
		/// THROW NotImplementedException{}
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
	FUNCTION VODBSort(nDest AS DWORD,fnNames AS _FIELDNAMES,uCobFor AS USUAL,uCobWhile AS USUAL,nNext AS USUAL,nRecno AS USUAL,lRest AS LOGIC,fnSortNames AS _FIELDNAMES) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <param name="wst"></param>
	/// <returns>
	/// </returns>
	FUNCTION VODBStatus(wst AS _WORKAREASTATUS) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Select a new work area by specifying its alias as a symbol and return the number of the current work area.
	/// </summary>
	/// <param name="sAlias"></param>
	/// <returns>
	/// </returns>
	FUNCTION VODBSymSelect(sAlias AS SYMBOL) AS INT
		/// THROW NotImplementedException{}
	RETURN 0   

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
	FUNCTION VODBTrans(nDest AS DWORD,fldNames AS _FIELDNAMES,uCobFor AS USUAL,uCobWhile AS USUAL,nNext AS USUAL,nRecno AS USUAL,lRest AS LOGIC) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// </summary>
	/// <param name="nDest"></param>
	/// <param name="fldNames"></param>
	/// <returns>
	/// </returns>
	FUNCTION VODBTransRec(nDest AS DWORD,fldNames AS _FIELDNAMES) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Release all locks for a work area.
	/// </summary>
	/// <param name="uRecno"></param>
	/// <returns>
	/// </returns>
	FUNCTION VODBUnlock(uRecno AS USUAL) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Release all locks for all work areas.
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION VODBUnlockAll() AS LOGIC
		/// THROW NotImplementedException{}
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
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Remove all records from open files.
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION VODBZap() AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	#endregion
end namespace