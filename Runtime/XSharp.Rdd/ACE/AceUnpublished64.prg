USING System
USING System.Runtime.InteropServices

BEGIN NAMESPACE XSharp.ADS
 
	PUBLIC CLASS ACEUNPUB64
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsBuildKeyFromRecord(hTag AS Intptr, mpucRecBuffer AS STRING , ulRecordLen AS DWORD , pucKey AS CHAR[], pusKeyLen REF WORD ) AS DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsClearLastError() AS DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsDeleteFile(hConnect AS Intptr, pucFileName AS STRING ) AS DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsDeleteTable(hTable AS IntPtr ) AS DWORD
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD  AdsMemCompare(hConnect AS Intptr, pucStr1 AS STRING , ulStr1Len AS DWORD , pucStr2 AS STRING , ulStr2Len AS DWORD , usCharSet AS WORD , psResult OUT SHORT ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsMemCompare90(hConnect AS Intptr, pucStr1 AS STRING , ulStr1Len AS DWORD , pucStr2 AS STRING , ulStr2Len AS DWORD , usCharSet AS WORD , ulCollationID AS DWORD , psResult OUT SHORT ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsMemICompare(hConnect AS Intptr, pucStr1 AS STRING , ulStr1Len AS DWORD , pucStr2 AS STRING , ulStr2Len AS DWORD , usCharSet AS WORD , psResult OUT SHORT ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsMemICompare90(hConnect AS Intptr, pucStr1 AS STRING , ulStr1Len AS DWORD , pucStr2 AS STRING , ulStr2Len AS DWORD , usCharSet AS WORD , ulCollationID AS DWORD , psResult OUT SHORT ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsMemLwr(hConnect AS Intptr, pucStr AS STRING , usStrLen AS WORD , usCharSet AS WORD ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsMemLwr90(hConnect AS Intptr, pucStr AS STRING , usStrLen AS WORD , usCharSet AS WORD , ulCollationID AS DWORD ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsGetIndexFlags(hIndex AS Intptr, pulFlags OUT DWORD ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsConvertKeyToDouble(pucKey AS STRING , pdValue OUT double ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsGetNumSegments(hTag AS Intptr, usSegments OUT WORD ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsGetSegmentFieldname(hTag AS Intptr, usSegmentNum AS WORD , pucFieldname AS CHAR[], pusFldnameLen REF WORD ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsGetSegmentOffset(hTag AS Intptr, usSegmentNum AS WORD , usOffset OUT WORD ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsIsSegmentDescending(hTag AS Intptr, usSegmentNum AS WORD , pbDescending OUT WORD ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsGetSegmentFieldNumbers(hTag AS Intptr, pusNumSegments OUT WORD , pusSegFieldNumbers AS WORD[] )AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsSetFieldRaw(hObj AS Intptr, pucFldName AS STRING , pucBuf AS BYTE[] , ulLen AS DWORD ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD  AdsSetFieldRaw(hObj AS Intptr, lFieldOrdinal AS DWORD , pucBuf AS BYTE[] , ulLen AS DWORD ) AS DWORD
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsGetFieldRaw(hTbl AS Intptr, pucFldName AS STRING , pucBuf AS BYTE[] , pulLen REF DWORD ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsGetFieldRaw(hTbl AS Intptr, lFieldOrdinal AS DWORD , pucBuf AS BYTE[] , pulLen REF DWORD ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsSetFlushFlag(hConnect AS Intptr, usFlushEveryUpdate AS WORD ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsSetTimeStampRaw(hObj AS Intptr, pucFldName AS STRING , pucBuf REF UINT64 , ulLen AS DWORD ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsSetTimeStampRaw(hObj AS Intptr, lFieldOrdinal AS DWORD , pucBuf REF UINT64 , ulLen AS DWORD ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsSetInternalError(ulErrCode AS DWORD , pucFile AS STRING , ulLine AS DWORD ) AS DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsValidateThread() AS DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsSetLastError(ulErrCode AS DWORD , pucDetails AS STRING ) AS DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
        PUBLIC STATIC EXTERN METHOD AdsSetTableCharType(hTbl AS Intptr, usCharType AS WORD ) AS DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsConvertJulianToString(dJulian AS double , pucJulian AS CHAR[] , pusLen REF WORD ) AS DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsConvertStringToJulian(pucJulian AS STRING , usLen AS WORD , pdJulian OUT double ) AS DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsConvertStringToJulian(pucJulian AS CHAR[] , usLen AS WORD , pdJulian OUT double ) AS DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsConvertMillisecondsToString(ulMSeconds AS DWORD , pucTime AS CHAR[] , pusLen REF WORD ) AS DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsConvertStringToMilliseconds(pucTime AS STRING , usLen AS WORD , pulMSeconds OUT DWORD ) AS DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsSetCollationSequence(pucSequence AS STRING ) AS DWORD

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsSetBOFFlag(hTbl AS Intptr, usBOF AS WORD )AS DWORD

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsDBFDateToString(pucDBFDate AS STRING , pucFormattedDate AS STRING )AS DWORD

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsActivateAOF(hTable AS IntPtr )AS DWORD

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsDeactivateAOF(hTable AS IntPtr )AS DWORD

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsExtractPathPart(usPart AS WORD , pucFile AS STRING , pucPart AS CHAR[] , pusPartLen REF WORD )AS DWORD

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsExpressionLongToShort(hTable AS Intptr, pucLongExpr AS STRING ,  pucShortExpr AS CHAR[] , pusBufferLen REF WORD )AS DWORD

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsExpressionShortToLong(hTable AS Intptr, pucShortExpr AS STRING ,  pucLongExpr AS CHAR[] , pusBufferLen REF WORD )AS DWORD

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsExpressionLongToShort90(hTable AS Intptr, pucLongExpr AS STRING ,  pucShortExpr AS CHAR[] , pulBufferLen REF DWORD ) AS DWORD

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsExpressionShortToLong90(hTable AS Intptr, pucShortExpr AS STRING ,  pucLongExpr AS CHAR[] , pulBufferLen REF DWORD ) AS DWORD

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsSqlPeekStatement(hCursor AS IntPtr, IsLive OUT BYTE ) AS DWORD

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsSetCursorAOF(hTable AS Intptr, pucFilter AS STRING , usResolve AS WORD ) AS DWORD

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsGetCursorAOF(hCursor AS IntPtr,  pucFilter AS CHAR[] , pusFilterLen REF WORD ) AS DWORD

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsClearCursorAOF(hTable AS IntPtr ) AS DWORD

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsInternalCloseCachedTables(hConnect AS Intptr, usOpen AS WORD ) AS DWORD

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsClearRecordBuffer(hTbl AS Intptr, pucBuf AS STRING , ulLen AS DWORD ) AS DWORD

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD ObsAdsEncryptBuffer(pucPassword AS STRING , pucBuffer AS STRING , usLen AS WORD ) AS DWORD

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD ObsAdsDecryptBuffer(pucPassword AS STRING , pucBuffer AS STRING , usLen AS WORD ) AS DWORD

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsEvalExpr(hTable AS Intptr, pucPCode AS STRING,  pucResult AS CHAR[] , pusLen REF WORD ) AS DWORD

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsFreeExpr(hTable AS Intptr, pucPCode AS STRING) AS DWORD

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsIsIndexExprValid(hTbl AS Intptr, pucExpr AS STRING , pbValid OUT WORD ) AS DWORD

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsStepIndexKey(hIndex AS Intptr, pucKey AS STRING , usLen AS WORD,  sDirection AS SHORT ) AS DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsPrepareSQLNow(hStatement AS Intptr, pucSQL AS STRING ,  pucFieldInfo AS CHAR[] , pusFieldInfoLen REF WORD ) AS DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsGetPreparedFields(hStatement AS Intptr,  pucBuffer AS CHAR[] , pulBufferLen REF DWORD , ulOptions AS DWORD ) AS DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsEcho(hConnect AS Intptr, pucData AS STRING , usLen AS WORD ) AS DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsReadRecords(hObj AS Intptr, ulRecordNum AS DWORD , cDirection AS BYTE ) AS DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsReadRecordNumbers(hObj AS Intptr, ulRecordNum AS DWORD , ucDirection AS BYTE , pulRecords OUT DWORD , pulArrayLen REF DWORD , pusHitEOF OUT WORD ) AS DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsMergeAOF(hTable AS IntPtr ) AS DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsSetupRI(hConnection AS IntPtr , lTableID AS INT , ucOpen AS BYTE , ulServerWAN AS DWORD ) AS DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsPerformRI(hTable AS Intptr, ulRecNum AS DWORD , pucRecBuffer AS STRING ) AS DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsLockRecordImplicitly(hTbl AS Intptr, ulRec AS DWORD ) AS DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsGetBaseFieldNum(hCursor AS IntPtr , pucColumnName AS STRING , pusBaseFieldNum OUT WORD ) AS DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsGetBaseFieldName(hTbl AS Intptr, usFld AS WORD ,  pucName AS CHAR[] , pusBufLen REF WORD ) AS DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsDDOpen( pucDictionaryPath AS STRING, pucPassword AS STRING , phDictionary OUT IntPtr ) AS DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsDDClose(hDictionary AS IntPtr ) AS DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsRefreshView(phCursor OUT IntPtr ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsDDExecuteProcedure(hDictionary AS Intptr, pucProcName AS STRING , pucInput AS STRING , pucOutput AS STRING , pulRowsAffected OUT DWORD ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsSetPacketSize(hConnect AS Intptr, usPacketLength AS WORD ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsVerifyRI(hConnect AS Intptr, usExclusive AS WORD ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsAddToAOF(hTable AS Intptr, pucFilter AS STRING , usOperation AS WORD , usWhichAOF AS WORD ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsDDVerifyUserRights(hObject AS IntPtr , pucTableName AS STRING , pulUserRights OUT DWORD ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsGetIndexPageSize(hIndex AS Intptr, pulPageSize OUT DWORD ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsDDAutoCreateTable(hConnect AS Intptr, pucTableName AS STRING , pucCollation AS STRING ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsDDAutoCreateIndex(hConnect AS Intptr, pucTableName AS STRING , pucIndexName AS STRING , pucCollation AS STRING ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsGetROWIDPrefix(hTable AS Intptr, pucRowIDPrefix AS STRING , usBufferLen AS WORD ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsGetColumnPermissions(hTable AS Intptr, usColumnNum AS WORD , pucPermissions AS STRING ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsGetSQLStmtParams(pucStatement AS STRING ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsRemoveSQLComments(pucStatement AS STRING ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsSetBaseTableAccess(hTbl AS Intptr, usAccessBase AS WORD ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsCopyTableTop(hObj AS Intptr, hDestTbl AS IntPtr , ulNumTopRecords AS DWORD) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsGetNullRecord(hTbl AS Intptr, pucBuf AS STRING , ulLen AS DWORD ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsSetProperty(hObj AS Intptr, ulOperation AS DWORD , ulValue AS DWORD ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsSetProperty90(hObj AS Intptr, ulOperation AS DWORD , uqValue AS UINT64 ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsCloseCachedTrigStatements(hConnection AS IntPtr , lTableID AS INT ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsDDCreateASA(hConnect AS Intptr, pucDictionaryPath AS STRING , usEncrypt AS WORD , pucDescription AS STRING , pucPassword AS STRING ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsGotoBOF(hObj AS IntPtr ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsCreateMemTable(hConnection AS IntPtr , pucName AS STRING , usTableType AS WORD , usCharType AS WORD , pucFields AS STRING , ulSize AS DWORD , phTable OUT IntPtr ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsCreateMemTable90(hConnection AS IntPtr , pucName AS STRING , usTableType AS WORD , usCharType AS WORD , pucFields AS STRING , ulSize AS DWORD , pucCollation AS STRING , phTable OUT IntPtr ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsGetTableWAN(hTable AS Intptr, pusWAN OUT WORD ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsGetFTSScore(hIndex AS Intptr, ulRecord AS DWORD , pucKey AS STRING , usKeyLen AS WORD , usDataType AS WORD , usSeekType AS WORD , pulScore OUT DWORD ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsConvertDateToJulian(hConnect AS Intptr, pucDate AS STRING , usLen AS WORD , pdJulian OUT double ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsDDSetActiveDictionary(hConnect AS Intptr, pucLinkName AS STRING , phDictionary OUT IntPtr ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsDDCreateLinkPre71(hDBConn AS IntPtr , pucLinkAlias AS STRING , pucLinkedDDPath AS STRING , pucUserName AS STRING , pucPassword AS STRING , ulOptions AS DWORD ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsDDDropLinkPre71(hDBConn AS IntPtr , pucLinkedDD AS STRING , usDropGlobal AS WORD ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsDDDisableTriggers(hDictionary AS Intptr, pucObjectName AS STRING , pucParent AS STRING , ulOptions AS DWORD ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsDDEnableTriggers(hDictionary AS Intptr, pucObjectName AS STRING , pucParent AS STRING , ulOptions AS DWORD ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsCreateCriticalSection(hObj AS Intptr, ulOptions AS DWORD ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsWaitForObject(hObj AS Intptr, ulOptions AS DWORD ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsReleaseObject(hObj AS IntPtr ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsBackupDatabase(hConnect AS Intptr, hOutputTable AS IntPtr , pucSourcePath AS STRING , pucSourceMask AS STRING , pucDestPath AS STRING , pucOptions AS STRING , pucFreeTablePasswords AS STRING , usCharType AS WORD , usLockingMode AS WORD , usCheckRights AS WORD , usTableType AS WORD , pucCollation AS STRING , ucDDConn AS BYTE ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsRestoreDatabase(hConnect AS Intptr, hOutputTable AS IntPtr , pucSourcePath AS STRING , pucSourcePassword AS STRING , pucDestPath AS STRING , pucDestPassword AS STRING , pucOptions AS STRING , pucFreeTablePasswords AS STRING , usCharType AS WORD , usLockingMode AS WORD , usCheckRights AS WORD , usTableType AS WORD , pucCollation AS STRING , ucDDConn AS BYTE ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsSetRecordPartial(hObj AS Intptr, pucRec AS STRING , ulLen AS DWORD ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsDDSetTriggerProperty(hDictionary AS Intptr,  pucTriggerName AS STRING, usPropertyID AS WORD , pucProperty AS STRING , usPropertyLen AS WORD ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsDDCreateFunction(hDictionary AS Intptr, pucName AS STRING , pucReturnType AS STRING , usInputParamCnt AS WORD , pucInputParams AS STRING , pucFuncBody AS STRING , pucComments AS STRING ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsDDDropFunction(hDictionary AS Intptr, pucName AS STRING ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsDDGetObjectProperty(hDictionary AS Intptr, usObjectType AS WORD , pucParent AS STRING , pucName AS STRING , usPropertyID AS WORD ,  pvProperty AS BYTE[] , pusPropertyLen REF WORD ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsDDGetObjectProperty(hDictionary AS Intptr, usObjectType AS WORD , pucParent AS STRING , pucName AS STRING , usPropertyID AS WORD ,  pucProperty AS CHAR[] , pusPropertyLen REF WORD ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsDDGetObjectProperty(hDictionary AS Intptr, usObjectType AS WORD , pucParent AS STRING , pucName AS STRING , usPropertyID AS WORD , pusProperty REF WORD , pusPropertyLen REF WORD ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsDDSetObjectProperty(hDictionary AS Intptr, usObjectType AS WORD , pucParent AS STRING , pucName AS STRING , usPropertyID AS WORD ,  pvProperty AS BYTE[] , usPropertyLen AS WORD ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsDDSetObjectProperty(hDictionary AS Intptr, usObjectType AS WORD , pucParent AS STRING , pucName AS STRING , usPropertyID AS WORD ,  pucProperty AS CHAR[] , usPropertyLen AS WORD ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsDDSetObjectProperty(hDictionary AS Intptr, usObjectType AS WORD , pucParent AS STRING , pucName AS STRING , usPropertyID AS WORD , pusProperty REF WORD , usPropertyLen AS WORD ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsDDCreatePackage(hDictionary AS Intptr, pucName AS STRING , pucComments AS STRING ) AS DWORD 
        
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsDDDropPackage(hDictionary AS Intptr, pucName AS STRING ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsCopyTableStructure81(hTable AS Intptr, pucFile AS STRING , ulOptions AS DWORD ) AS DWORD
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsAccessVfpSystemField(hTable AS Intptr, pucFldName AS STRING , pucBuffer AS STRING , ulOptions AS DWORD , puFlag OUT WORD ) AS DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		PUBLIC STATIC EXTERN METHOD AdsAccessVfpSystemField(hTable AS Intptr, lFieldOrdinal AS DWORD , pucBuffer AS STRING , ulOptions AS DWORD , puFlag OUT WORD ) AS DWORD 
		
	END CLASS
END NAMESPACE
