using System
using System.Runtime.InteropServices

BEGIN namespace XSharp.Ads

	INTERNAL CLASS ACEUNPUB64
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsBuildKeyFromRecord(hTag as Intptr, mpucRecBuffer as string , ulRecordLen as DWORD , pucKey as char[], pusKeyLen ref WORD ) as DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsClearLastError() as DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsDeleteFile(hConnect as Intptr, pucFileName as string ) as DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsDeleteTable(hTable as IntPtr ) as DWORD
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD  AdsMemCompare(hConnect as Intptr, pucStr1 as string , ulStr1Len as DWORD , pucStr2 as string , ulStr2Len as DWORD , usCharSet as WORD , psResult out short ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsMemCompare90(hConnect as Intptr, pucStr1 as string , ulStr1Len as DWORD , pucStr2 as string , ulStr2Len as DWORD , usCharSet as WORD , ulCollationID as DWORD , psResult out short ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsMemICompare(hConnect as Intptr, pucStr1 as string , ulStr1Len as DWORD , pucStr2 as string , ulStr2Len as DWORD , usCharSet as WORD , psResult out short ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsMemICompare90(hConnect as Intptr, pucStr1 as string , ulStr1Len as DWORD , pucStr2 as string , ulStr2Len as DWORD , usCharSet as WORD , ulCollationID as DWORD , psResult out short ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsMemLwr(hConnect as Intptr, pucStr as string , usStrLen as WORD , usCharSet as WORD ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsMemLwr90(hConnect as Intptr, pucStr as string , usStrLen as WORD , usCharSet as WORD , ulCollationID as DWORD ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsGetIndexFlags(hIndex as Intptr, pulFlags out DWORD ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsConvertKeyToDouble(pucKey as string , pdValue out double ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsGetNumSegments(hTag as Intptr, usSegments out WORD ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsGetSegmentFieldname(hTag as Intptr, usSegmentNum as WORD , pucFieldname as char[], pusFldnameLen ref WORD ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsGetSegmentOffset(hTag as Intptr, usSegmentNum as WORD , usOffset out WORD ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsIsSegmentDescending(hTag as Intptr, usSegmentNum as WORD , pbDescending out WORD ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsGetSegmentFieldNumbers(hTag as Intptr, pusNumSegments out WORD , pusSegFieldNumbers as WORD[] )as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsSetFieldRaw(hObj as Intptr, pucFldName as string , pucBuf as byte[] , ulLen as DWORD ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD  AdsSetFieldRaw(hObj as Intptr, lFieldOrdinal as DWORD , pucBuf as byte[] , ulLen as DWORD ) as DWORD
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsGetFieldRaw(hTbl as Intptr, pucFldName as string , pucBuf as byte[] , pulLen ref DWORD ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsGetFieldRaw(hTbl as Intptr, lFieldOrdinal as DWORD , pucBuf as byte[] , pulLen ref DWORD ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsSetFlushFlag(hConnect as Intptr, usFlushEveryUpdate as WORD ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsSetTimeStampRaw(hObj as Intptr, pucFldName as string , pucBuf ref UINT64 , ulLen as DWORD ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsSetTimeStampRaw(hObj as Intptr, lFieldOrdinal as DWORD , pucBuf ref UINT64 , ulLen as DWORD ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsSetInternalError(ulErrCode as DWORD , pucFile as string , ulLine as DWORD ) as DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsValidateThread() as DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsSetLastError(ulErrCode as DWORD , pucDetails as string ) as DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
        public static extern METHOD AdsSetTableCharType(hTbl as Intptr, usCharType as WORD ) as DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsConvertJulianToString(dJulian as double , pucJulian as char[] , pusLen ref WORD ) as DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsConvertStringToJulian(pucJulian as string , usLen as WORD , pdJulian out double ) as DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsConvertStringToJulian(pucJulian as char[] , usLen as WORD , pdJulian out double ) as DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsConvertMillisecondsToString(ulMSeconds as DWORD , pucTime as char[] , pusLen ref WORD ) as DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsConvertStringToMilliseconds(pucTime as string , usLen as WORD , pulMSeconds out DWORD ) as DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsSetCollationSequence(pucSequence as string ) as DWORD

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsSetBOFFlag(hTbl as Intptr, usBOF as WORD )as DWORD

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsDBFDateToString(pucDBFDate as string , pucFormattedDate as string )as DWORD

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsActivateAOF(hTable as IntPtr )as DWORD

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsDeactivateAOF(hTable as IntPtr )as DWORD

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsExtractPathPart(usPart as WORD , pucFile as string , pucPart as char[] , pusPartLen ref WORD )as DWORD

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsExpressionLongToShort(hTable as Intptr, pucLongExpr as string ,  pucShortExpr as char[] , pusBufferLen ref WORD )as DWORD

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsExpressionShortToLong(hTable as Intptr, pucShortExpr as string ,  pucLongExpr as char[] , pusBufferLen ref WORD )as DWORD

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsExpressionLongToShort90(hTable as Intptr, pucLongExpr as string ,  pucShortExpr as char[] , pulBufferLen ref DWORD ) as DWORD

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsExpressionShortToLong90(hTable as Intptr, pucShortExpr as string ,  pucLongExpr as char[] , pulBufferLen ref DWORD ) as DWORD

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsSqlPeekStatement(hCursor as IntPtr, IsLive out byte ) as DWORD

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsSetCursorAOF(hTable as Intptr, pucFilter as string , usResolve as WORD ) as DWORD

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsGetCursorAOF(hCursor as IntPtr,  pucFilter as char[] , pusFilterLen ref WORD ) as DWORD

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsClearCursorAOF(hTable as IntPtr ) as DWORD

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsInternalCloseCachedTables(hConnect as Intptr, usOpen as WORD ) as DWORD

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsClearRecordBuffer(hTbl as Intptr, pucBuf as string , ulLen as DWORD ) as DWORD

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD ObsAdsEncryptBuffer(pucPassword as string , pucBuffer as string , usLen as WORD ) as DWORD

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD ObsAdsDecryptBuffer(pucPassword as string , pucBuffer as string , usLen as WORD ) as DWORD

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsEvalExpr(hTable as Intptr, pucPCode as string,  pucResult as char[] , pusLen ref WORD ) as DWORD

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsFreeExpr(hTable as Intptr, pucPCode as string) as DWORD

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsIsIndexExprValid(hTbl as Intptr, pucExpr as string , pbValid out WORD ) as DWORD

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsStepIndexKey(hIndex as Intptr, pucKey as string , usLen as WORD,  sDirection as short ) as DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsPrepareSQLNow(hStatement as Intptr, pucSQL as string ,  pucFieldInfo as char[] , pusFieldInfoLen ref WORD ) as DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsGetPreparedFields(hStatement as Intptr,  pucBuffer as char[] , pulBufferLen ref DWORD , ulOptions as DWORD ) as DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsEcho(hConnect as Intptr, pucData as string , usLen as WORD ) as DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsReadRecords(hObj as Intptr, ulRecordNum as DWORD , cDirection as byte ) as DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsReadRecordNumbers(hObj as Intptr, ulRecordNum AS DWORD , ucDirection as byte , pulRecords out DWORD , pulArrayLen ref DWORD , pusHitEOF out WORD ) as DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsMergeAOF(hTable as IntPtr ) as DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsSetupRI(hConnection as IntPtr , lTableID as int , ucOpen as byte , ulServerWAN as DWORD ) as DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsPerformRI(hTable as Intptr, ulRecNum as DWORD , pucRecBuffer as string ) as DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsLockRecordImplicitly(hTbl as Intptr, ulRec as DWORD ) as DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsGetBaseFieldNum(hCursor as IntPtr , pucColumnName as string , pusBaseFieldNum out WORD ) as DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsGetBaseFieldName(hTbl as Intptr, usFld as WORD ,  pucName as char[] , pusBufLen ref WORD ) as DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsDDOpen( pucDictionaryPath as string, pucPassword as string , phDictionary out IntPtr ) as DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsDDClose(hDictionary as IntPtr ) as DWORD 

		[DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsRefreshView(phCursor out IntPtr ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsDDExecuteProcedure(hDictionary as Intptr, pucProcName as string , pucInput as string , pucOutput as string , pulRowsAffected out DWORD ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsSetPacketSize(hConnect as Intptr, usPacketLength as WORD ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsVerifyRI(hConnect as Intptr, usExclusive as WORD ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsAddToAOF(hTable as Intptr, pucFilter as string , usOperation as WORD , usWhichAOF as WORD ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsDDVerifyUserRights(hObject as IntPtr , pucTableName as string , pulUserRights out DWORD ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsGetIndexPageSize(hIndex as Intptr, pulPageSize out DWORD ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsDDAutoCreateTable(hConnect as Intptr, pucTableName as string , pucCollation as string ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsDDAutoCreateIndex(hConnect as Intptr, pucTableName as string , pucIndexName as string , pucCollation as string ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsGetROWIDPrefix(hTable as Intptr, pucRowIDPrefix as string , usBufferLen as WORD ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsGetColumnPermissions(hTable as Intptr, usColumnNum as WORD , pucPermissions as string ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsGetSQLStmtParams(pucStatement as string ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsRemoveSQLComments(pucStatement as string ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsSetBaseTableAccess(hTbl as Intptr, usAccessBase as WORD ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsCopyTableTop(hObj as Intptr, hDestTbl as IntPtr , ulNumTopRecords as DWORD) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsGetNullRecord(hTbl as Intptr, pucBuf as string , ulLen as DWORD ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsSetProperty(hObj as Intptr, ulOperation as DWORD , ulValue as DWORD ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsSetProperty90(hObj as Intptr, ulOperation as DWORD , uqValue as UINT64 ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsCloseCachedTrigStatements(hConnection as IntPtr , lTableID as int ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsDDCreateASA(hConnect as Intptr, pucDictionaryPath as string , usEncrypt as WORD , pucDescription as string , pucPassword as string ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsGotoBOF(hObj as IntPtr ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsCreateMemTable(hConnection as IntPtr , pucName as string , usTableType as WORD , usCharType as WORD , pucFields as string , ulSize as DWORD , phTable out IntPtr ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsCreateMemTable90(hConnection as IntPtr , pucName as string , usTableType as WORD , usCharType as WORD , pucFields as string , ulSize as DWORD , pucCollation as string , phTable out IntPtr ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsGetTableWAN(hTable as Intptr, pusWAN out WORD ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsGetFTSScore(hIndex as Intptr, ulRecord as DWORD , pucKey as string , usKeyLen as WORD , usDataType as WORD , usSeekType as WORD , pulScore out DWORD ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsConvertDateToJulian(hConnect as Intptr, pucDate as string , usLen as WORD , pdJulian out double ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsDDSetActiveDictionary(hConnect as Intptr, pucLinkName as string , phDictionary out IntPtr ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsDDCreateLinkPre71(hDBConn as IntPtr , pucLinkAlias as string , pucLinkedDDPath as string , pucUserName as string , pucPassword as string , ulOptions as DWORD ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsDDDropLinkPre71(hDBConn as IntPtr , pucLinkedDD as string , usDropGlobal as WORD ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsDDDisableTriggers(hDictionary as Intptr, pucObjectName as string , pucParent as string , ulOptions as DWORD ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsDDEnableTriggers(hDictionary as Intptr, pucObjectName as string , pucParent as string , ulOptions as DWORD ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsCreateCriticalSection(hObj as Intptr, ulOptions as DWORD ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsWaitForObject(hObj as Intptr, ulOptions as DWORD ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsReleaseObject(hObj as IntPtr ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsBackupDatabase(hConnect as Intptr, hOutputTable as IntPtr , pucSourcePath as string , pucSourceMask as string , pucDestPath as string , pucOptions as string , pucFreeTablePasswords as string , usCharType as WORD , usLockingMode as WORD , usCheckRights as WORD , usTableType as WORD , pucCollation as string , ucDDConn as byte ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsRestoreDatabase(hConnect as Intptr, hOutputTable as IntPtr , pucSourcePath as string , pucSourcePassword as string , pucDestPath as string , pucDestPassword as string , pucOptions as string , pucFreeTablePasswords as string , usCharType as WORD , usLockingMode as WORD , usCheckRights as WORD , usTableType as WORD , pucCollation as string , ucDDConn as byte ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsSetRecordPartial(hObj as Intptr, pucRec as string , ulLen as DWORD ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsDDSetTriggerProperty(hDictionary as Intptr,  pucTriggerName as string, usPropertyID as WORD , pucProperty as string , usPropertyLen as WORD ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsDDCreateFunction(hDictionary as Intptr, pucName as string , pucReturnType as string , usInputParamCnt as WORD , pucInputParams as string , pucFuncBody as string , pucComments as string ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsDDDropFunction(hDictionary as Intptr, pucName as string ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsDDGetObjectProperty(hDictionary as Intptr, usObjectType as WORD , pucParent as string , pucName as string , usPropertyID as WORD ,  pvProperty as byte[] , pusPropertyLen ref WORD ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsDDGetObjectProperty(hDictionary as Intptr, usObjectType as WORD , pucParent as string , pucName as string , usPropertyID as WORD ,  pucProperty as char[] , pusPropertyLen ref WORD ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsDDGetObjectProperty(hDictionary as Intptr, usObjectType as WORD , pucParent as string , pucName as string , usPropertyID as WORD , pusProperty ref WORD , pusPropertyLen ref WORD ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsDDSetObjectProperty(hDictionary as Intptr, usObjectType as WORD , pucParent as string , pucName as string , usPropertyID as WORD ,  pvProperty as byte[] , usPropertyLen as WORD ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsDDSetObjectProperty(hDictionary as Intptr, usObjectType as WORD , pucParent as string , pucName as string , usPropertyID as WORD ,  pucProperty as char[] , usPropertyLen as WORD ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsDDSetObjectProperty(hDictionary as Intptr, usObjectType as WORD , pucParent as string , pucName as string , usPropertyID as WORD , pusProperty ref WORD , usPropertyLen as WORD ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsDDCreatePackage(hDictionary as Intptr, pucName as string , pucComments as string ) as DWORD 
        
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsDDDropPackage(hDictionary as Intptr, pucName as string ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsCopyTableStructure81(hTable as Intptr, pucFile as string , ulOptions as DWORD ) as DWORD
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsAccessVfpSystemField(hTable as Intptr, pucFldName as string , pucBuffer as string , ulOptions as DWORD , puFlag out WORD ) as DWORD 
		
        [DllImport("ace64dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsAccessVfpSystemField(hTable as Intptr, lFieldOrdinal as DWORD , pucBuffer as string , ulOptions as DWORD , puFlag out WORD ) as DWORD 
		
	end class
END NAMESPACE