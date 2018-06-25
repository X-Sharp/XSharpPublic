using System
using System.Runtime.InteropServices

BEGIN namespace XSharp.ADS

	INTERNAL CLASS ACEUNPUB
		PRIVATE ENUM PathInfo
		
			MEMBER PATH_DRIVE := 1
			MEMBER PATH_SERVER
			MEMBER PATH_VOLUME
			MEMBER PATH_PATH
			MEMBER PATH_BASENAME
			MEMBER PATH_FILENAME
			MEMBER PATH_EXTENSION
		END ENUM
        #region Constants
		public const MAX_STRING_CHECK := 5000 as WORD
		public const ERROR_FILE_NOT_FOUND := 2 as WORD
		public const ADS_FOXGENERAL := 51 as WORD
		public const ADS_FOXPICTURE := 52 as WORD
		public const ADS_AQE_SERVER := 32768 as WORD
		public const ADS_DICTIONARY_SERVER := 16384 as WORD
		public const ADS_INTERNAL_CONNECTION := 8192 as WORD
		public const ADS_LOCAL_ASA_CONNECTION := -1 as short
		public const ADS_READ_NEXT := 1 as WORD
		public const ADS_READ_PREV := 2 as WORD
		public const ADS_DD_INDEX_FILE_PATH_UNPUB := 6900 as WORD
		public const ADS_DD_INDEX_FILE_TYPE_UNPUB := 6901 as WORD
		public const ADS_DD_USER_KEYS := 6902 as WORD
		public const ADS_DD_INDEX_FILE_RELATIVE_PATH_UNPUB := 6903 as WORD
		public const ADS_DD_USER_KEYS_AND_UPDATE := 6904 as WORD
		public const ADS_DD_DATABASE_ID := 6905 as WORD
		public const ADS_DD_INDEX_FILE_PAGESIZE_UNPUB := 6906 as WORD
		public const ADS_DD_DEPLOY_PASSWORD := 6907 as WORD
		public const ADS_DD_USER_ENABLE_INTERNET := 6908 as WORD
		public const ADS_DD_LINK_AUTHENTICATE := 6909 as WORD
		public const ADS_DD_PROC_EXECUTE_PRIV := 6910 as WORD
		public const ADS_DD_REFRESH_TABLE_PASSWORD := 6911 as WORD
		public const ADS_DD_UPDATE_ADMIN_KEY := 6912 as WORD
		public const ADS_DD_TABLE_OPTIONS := 6913 as WORD
		public const ADS_DD_FIELD_NAME_UNPUB := 6914 as WORD
		public const ADS_DD_USER_PASSWORD_ENCRYPTED := 6931 as WORD
		public const ADS_DD_ADMIN_PASSWORD_ENCRYPTED := 6932 as WORD
		public const ADS_DD_ENCRYPT_TABLE_PASSWORD_ENCRYPTED := 6933 as WORD
		public const ADS_DD_DISABLE_USER_INTERNET := 6934 as WORD
		public const ADS_DD_USER_LOGINS_DISABLED := 6935 as WORD
		public const ADS_DD_SUBSCR_PASSWORD_ENCRYPTED := 6936 as WORD
		public const ADS_DD_READ_OBJECT_NAME := 6937 as WORD
		public const ADS_DD_FUNCTION_INPUT_PARAM_COUNT := 6938 as WORD
		public const ADS_DD_FUNCTION_INPUT := 6939 as WORD
		public const ADS_DD_FUNCTION_OUTPUT := 6940 as WORD
		public const ADS_DD_FUNCTION_SCRIPT := 6941 as WORD
		public const ADS_DD_FUNCTION_EVAL_PERMISSION := 6942 as WORD
		public const ADS_DD_LOCK_OBJECT := 6943 as WORD
		public const ADS_DD_UNLOCK_OBJECT := 6944 as WORD
		public const ADS_DD_UNLOCK_OBJECT_CANCEL_UPDATE := 6945 as WORD
		public const ADS_DD_DATABASE_ID_UPDATE := 6946 as WORD
		public const ADS_DD_USER_DB_ROLES := 6947 as WORD
		public const ADS_DD_USER_UPDATED_DB_ROLES := 6948 as WORD
		public const ADS_DD_VERIFY_DEBUG_PERMISSION := 6949 as WORD
		public const ADS_DD_VERIFY_MISSING_PERMISSIONS := 6950 as WORD
		public const ADS_DD_VERIFY_ALTDROP_PERMISSIONS := 6951 as WORD
		public const ADS_DD_DELETE_INDEX := 6952 as WORD
		public const ADS_DD_ADD_INDEX := 6953 as WORD
		public const ADS_DD_OBJ_RIGHT_WRITE := 2u as DWORD
		public const ADS_DD_TABLE_ENCRYPTED := 1u as DWORD
		public const ADS_DD_TABLE_IS_OEM := 2u as DWORD
		public const ADS_DD_TABLE_ALLOW_SEARCH_NRC := 4u as DWORD
		public const ADS_DD_TABLE_STATIC_CURSOR_ONLY := 8u as DWORD
		public const ADS_ALTER_OBJECT := 2147483648u as DWORD
		public const ADS_LOGICAL_AND := 1 as WORD
		public const ADS_LOGICAL_OR := 2 as WORD
		public const AOF_NORMAL_TYPE := 1 as WORD
		public const AOF_CURSOR_TYPE := 2 as WORD
		public const ADS_LINK_TABLE_DELIMITOR := ':' as char
		public const ADS_PACKAGE_DELIMITOR := ':' as char
		public const ADS_QUALIFIED_NAME_DELIMITOR := ':' as char
		public const TRIG_BEFORE_UPDATE := 1 as WORD
		public const TRIG_AFTER_UPDATE := 2 as WORD
		public const TRIG_CONFLICT_UPDATE := 3 as WORD
		public const AQE_STMT_NO_JOIN_ORDER_OPT := "NOJOINORDEROPT" as string
		public const AQE_STMT_NO_PUSHDOWN_SORT_OPT := "NOPUSHDOWNSORTOPT" as string
		public const AQE_STMT_NO_EXECUTION_OPT := "NOEXECUTIONOPT" as string
		public const AQE_STMT_NO_MISC_OPT := "NOMISCOPT" as string
		public const AQE_STMT_NO_SUBQUERY_OPT := "NOSUBQUERYOPT" as string
		public const ADS_ROWID_PREFIX_LEN := 12 as WORD
		public const AQE_NO_TOPX := 4294967295u as DWORD
		public const ADS_DD_DISABLE_READ_PROP_CHK := 1u as DWORD
		public const ADS_DD_DISABLE_SET_PROP_CHK := 2u as DWORD
		public const ADS_DD_DISABLE_CREATE_RIGHTS_CHK := 4u as DWORD
		public const ADS_SET_LARGE_BLOCK_READS := 1 as WORD
		public const ADS_SET_BATCH_INSERTS := 2 as WORD
		public const ADS_UNIQUE_KEY_ENFORCEMENT := 3 as WORD
		public const ADS_RI_ENFORCEMENT := 4 as WORD
		public const ADS_AUTO_INCREMENT_ENFORCEMENT := 5 as WORD
		public const ADS_MOVE_SERVER_OP_COUNT := 6 as WORD
		public const ADS_SET_TRIG_IGNORE_EVENT := 7 as WORD
		public const ADS_VERIFY_FTS_INDEXES := 8 as WORD
		public const ADS_SET_FORCE_CLOSED := 9 as WORD
		public const ADS_SET_NOTRANS_TABLE := 10 as WORD
		public const ADS_SET_DDCONN_TRUEUSER := 11 as WORD
		public const ADS_SET_OUTPUT_TABLE := 12 as WORD
		public const ADS_GET_QUERY_ELEMENTS := 13 as WORD
		public const ADS_SKIP_BEGIN_TRIGS := 14 as WORD
		public const ADS_SET_DISABLE_PERMISSION := 16 as WORD
		public const ADS_ADD_DISABLE_PERMISSION := 17 as WORD
		public const ADS_DEBUG_CONNECTION := 18 as WORD
		public const ADS_GET_KEY_VALUES := 19 as WORD
		public const ADS_FREE_KEY_VALUES := 20 as WORD
		public const ADS_SET_DEBUG_MASK := 21 as WORD
		public const ADS_VERIFY_ADT := 22 as WORD
		public const ADS_SET_NO_RI := 23 as WORD
		public const ADS_VERIFY_MEMORY_TABLE := 24 as WORD
		public const ADS_ALLOW_SMC_CONNECTIONS := 25 as WORD
		public const ADS_SET_CONNINFO_INDEX := 26 as WORD
		public const ADS_RETRY_ADS_CONNECTS := 27 as WORD
		public const ADS_PUSH_ERROR_STACK := 28 as WORD
		public const ADS_POP_ERROR_STACK := 29 as WORD
		public const ADS_USE_PTHREAD_FOR_KA := 30 as WORD
		public const ADS_SET_REPLICATION_INFO := 31 as WORD
		public const ADS_GET_SERVER_USER_ID := 32 as WORD
		public const ADS_IGNORE_CS_TESTS := 33 as WORD
		public const ADS_ENFORCE_MODTIME_VALUES := 34 as WORD
		public const ADS_ENFORCE_ROWVERSION_VALUES := 35 as WORD
		public const ADS_DUMP_REPLICATION_CACHE := 36 as WORD
		public const ADS_GET_VIEW_STMT := 37 as WORD
		public const ADS_IN_PROC_OR_TRIG := 38 as WORD
		public const ADS_VERIFY_SCRIPT := 39 as WORD
		public const ADS_SET_MAX_CACHE_MEMORY := 40 as WORD
		public const ADS_PERSIST_CACHED_TABLE := 41 as WORD
		public const ADS_SET_VALUES_TABLE_FLAG := 42 as WORD
		public const ADS_GET_VALUES_TABLE_FLAG := 43 as WORD
		public const ADS_VERIFY_EXTERNAL_CALL_HANDLE := 44 as WORD
		public const ADS_GET_SERVER_RELEASE_BUILD := 45 as WORD
		public const ADS_SET_AVAIL_REINDEX_RAM := 46 as WORD
		public const ADS_SET_AVAIL_REINDEX_HDD := 47 as WORD
		public const ADS_GET_REINDEX_GROUP_NUM := 48 as WORD
		public const ADS_SET_REINDEX_SORTBUFFER_LEN := 49 as WORD
		public const ADS_SET_PULLING_DEBUG_TRIGGER := 50 as WORD
		public const ADS_FLIP_DESCEND_FLAG := 51 as WORD
		public const ADS_GET_ACE_ID_STR := 52 as WORD
		public const ADS_GET_SERVER_COLLATION_ID := 53 as WORD
		public const ADS_SET_REPLICATION_STMT := 54 as WORD
		public const ADS_GET_REPLICATION_STMT := 55 as WORD
		public const TRIG_NOT_VALUE_TABLE := 0 as WORD
		public const TRIG_OLD_TABLE := 1 as WORD
		public const TRIG_NEW_TABLE := 2 as WORD
		public const TRIG_ERROR_TABLE := 3 as WORD
		public const BACKUP_SEVERITY_HIGH := 10 as WORD
		public const BACKUP_SEVERITY_MEDHIGH := 7 as WORD
		public const BACKUP_SEVERITY_MED := 5 as WORD
		public const BACKUP_SEVERITY_LOW := 1 as WORD
		public const BACKUP_SEVERITY_NONE := 0 as WORD
		public const BACKUP_FREEPASSWDALL := "__AllFreeTablePassword" as string
		public const ADS_NORMAL_RA_CACHE_SIZE := 10 as WORD
		public const ADS_AGGRESSIVE_RA_CACHE_SIZE := 100 as WORD
		public const ADS_BEFORE_INSERT_TRIG := 1u as DWORD
		public const ADS_INSTEADOF_INSERT_TRIG := 2u as DWORD
		public const ADS_AFTER_INSERT_TRIG := 4u as DWORD
		public const ADS_BEFORE_UPDATE_TRIG := 8u as DWORD
		public const ADS_INSTEADOF_UPDATE_TRIG := 16u as DWORD
		public const ADS_AFTER_UPDATE_TRIG := 32u as DWORD
		public const ADS_BEFORE_DELETE_TRIG := 64u as DWORD
		public const ADS_INSTEADOF_DELETE_TRIG := 128u as DWORD
		public const ADS_AFTER_DELETE_TRIG := 256u as DWORD
		public const ADS_CONFLICTON_UPDATE_TRIG := 512u as DWORD
		public const ADS_CONFLICTON_DELETE_TRIG := 1024u as DWORD
		public const ADS_DCM_ALL := 4294967295u as DWORD
		public const ADS_DCM_ADT_HEADER := 1u as DWORD
		public const ADS_DCM_ADT_RECYCLE := 2u as DWORD
		public const ADS_DCM_ADM := 4u as DWORD
		public const ADS_CREATE_MEMTABLE := 1024u as DWORD
		public const ADS_DONT_ADD_TO_MEMTABLE_LIST := 2048u as DWORD
		public const ADS_RETURN_ID_DROP_TABLE := 32768u as DWORD
		public const ADS_TEMP_CURSOR_TABLE := 524288u as DWORD
		public const ADS_TEMP_TABLE_ANY_NAME := 1048576u as DWORD
		public const ADS_OPEN_TABLE_USING_SQL := 2097152u as DWORD
		public const ADS_NO_TRANSACTION := 4194304u as DWORD
		public const ADS_CREATE_TABLE_OVERWRITE := 8388608u as DWORD
		public const ADS_DONT_ENFORCE_RI := 16777216u as DWORD
		public const ADS_CREATE_ADD_FILE := 33554432u as DWORD
		public const ADS_DONT_OPEN_VIEW := 67108864u as DWORD
		public const ADS_LINK_FULLPATH_IN_PARAM := 16777216u as DWORD
		public const ADS_LINK_MODIFY := 33554432u as DWORD
		public const ADS_DISABLE_CONNECTION_CONCURRENCY := 1u as DWORD
		public const ADS_CURRENT_USER := 1u as DWORD
		public const ADS_ALL_USERS := 2u as DWORD
		public const ADS_CACHE_NEW_TABLE := 1u as DWORD
		public const ADS_RETRIEVE_NULL_FLAG := 1u as DWORD
		public const ADS_UPDATE_NULL_FLAG := 2u as DWORD
		public const ADS_UPDATE_FULL_FLAG := 3u as DWORD
		public const ADS_RETRIEVE_FULL_FLAG := 4u as DWORD
		public const BASE64CODE := "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" as string
		public const ADS_DONT_ADD_TO_DD := 69 as WORD
		#endregion

    #region Constructor
        STATIC PRIVATE Is32 as LOGIC
        STATIC CONSTRUCTOR  
            Is32 := IntPtr.Size == 4
        RETURN
    #endregion
    #region Method Dispatch Table
		PUBLIC STATIC METHOD AdsConvertStringToJulian(pucJulian as string , usLen as WORD , pdJulian out double ) as DWORD 
            IF Is32
                return ACEUNPUB32.AdsConvertStringToJulian(pucJulian, usLen, out pdJulian)
            ELSE
                return ACEUNPUB64.AdsConvertStringToJulian(pucJulian, usLen, out pdJulian)
            ENDIF
		PUBLIC STATIC METHOD AdsConvertStringToJulian(pucJulian as char[] , usLen as WORD , pdJulian out double ) as DWORD 
            IF Is32
                return ACEUNPUB32.AdsConvertStringToJulian(pucJulian, usLen, out pdJulian)
            ELSE
                return ACEUNPUB64.AdsConvertStringToJulian(pucJulian, usLen, out pdJulian)
            ENDIF

		PUBLIC STATIC METHOD AdsSqlPeekStatement(hCursor as IntPtr, IsLive out byte ) as DWORD
            IF Is32
                return ACEUNPUB32.AdsSqlPeekStatement(hCursor, out IsLive)
            ELSE
                return ACEUNPUB64.AdsSqlPeekStatement(hCursor, out IsLive)
            ENDIF
    #endregion
        
        // the methods below are defined in the 32 bit and 64 bit branches but not used


        /*
		PUBLIC STATIC EXTERN METHOD  AdsMemCompare(hConnect as Intptr, pucStr1 as string , ulStr1Len as DWORD , pucStr2 as string , ulStr2Len as DWORD , usCharSet as WORD , psResult out short ) as DWORD 
		PUBLIC STATIC EXTERN METHOD  AdsSetFieldRaw(hObj as Intptr, lFieldOrdinal as DWORD , pucBuf as byte[] , ulLen as DWORD ) as DWORD
		PUBLIC STATIC EXTERN METHOD AdsAccessVfpSystemField(hTable as Intptr, lFieldOrdinal as DWORD , pucBuffer as string , ulOptions as DWORD , puFlag out WORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsAccessVfpSystemField(hTable as Intptr, pucFldName as string , pucBuffer as string , ulOptions as DWORD , puFlag out WORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsActivateAOF(hTable as IntPtr )as DWORD
		PUBLIC STATIC EXTERN METHOD AdsAddToAOF(hTable as Intptr, pucFilter as string , usOperation as WORD , usWhichAOF as WORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsBackupDatabase(hConnect as Intptr, hOutputTable as IntPtr , pucSourcePath as string , pucSourceMask as string , pucDestPath as string , pucOptions as string , pucFreeTablePasswords as string , usCharType as WORD , usLockingMode as WORD , usCheckRights as WORD , usTableType as WORD , pucCollation as string , ucDDConn as byte ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsBuildKeyFromRecord(hTag as Intptr, mpucRecBuffer as string , ulRecordLen as DWORD , pucKey as char[], pusKeyLen ref WORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsClearCursorAOF(hTable as IntPtr ) as DWORD
		PUBLIC STATIC EXTERN METHOD AdsClearLastError() as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsClearRecordBuffer(hTbl as Intptr, pucBuf as string , ulLen as DWORD ) as DWORD
		PUBLIC STATIC EXTERN METHOD AdsCloseCachedTrigStatements(hConnection as IntPtr , lTableID as int ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsConvertDateToJulian(hConnect as Intptr, pucDate as string , usLen as WORD , pdJulian out double ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsConvertJulianToString(dJulian as double , pucJulian as char[] , pusLen ref WORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsConvertKeyToDouble(pucKey as string , pdValue out double ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsConvertMillisecondsToString(ulMSeconds as DWORD , pucTime as char[] , pusLen ref WORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsConvertStringToJulian(pucJulian as char[] , usLen as WORD , pdJulian out double ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsConvertStringToJulian(pucJulian as string , usLen as WORD , pdJulian out double ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsConvertStringToMilliseconds(pucTime as string , usLen as WORD , pulMSeconds out DWORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsCopyTableStructure81(hTable as Intptr, pucFile as string , ulOptions as DWORD ) as DWORD
		PUBLIC STATIC EXTERN METHOD AdsCopyTableTop(hObj as Intptr, hDestTbl as IntPtr , ulNumTopRecords as DWORD) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsCreateCriticalSection(hObj as Intptr, ulOptions as DWORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsCreateMemTable(hConnection as IntPtr , pucName as string , usTableType as WORD , usCharType as WORD , pucFields as string , ulSize as DWORD , phTable out IntPtr ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsCreateMemTable90(hConnection as IntPtr , pucName as string , usTableType as WORD , usCharType as WORD , pucFields as string , ulSize as DWORD , pucCollation as string , phTable out IntPtr ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsDBFDateToString(pucDBFDate as string , pucFormattedDate as string )as DWORD
		PUBLIC STATIC EXTERN METHOD AdsDDAutoCreateIndex(hConnect as Intptr, pucTableName as string , pucIndexName as string , pucCollation as string ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsDDAutoCreateTable(hConnect as Intptr, pucTableName as string , pucCollation as string ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsDDClose(hDictionary as IntPtr ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsDDCreateASA(hConnect as Intptr, pucDictionaryPath as string , usEncrypt as WORD , pucDescription as string , pucPassword as string ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsDDCreateFunction(hDictionary as Intptr, pucName as string , pucReturnType as string , usInputParamCnt as WORD , pucInputParams as string , pucFuncBody as string , pucComments as string ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsDDCreateLinkPre71(hDBConn as IntPtr , pucLinkAlias as string , pucLinkedDDPath as string , pucUserName as string , pucPassword as string , ulOptions as DWORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsDDCreatePackage(hDictionary as Intptr, pucName as string , pucComments as string ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsDDDisableTriggers(hDictionary as Intptr, pucObjectName as string , pucParent as string , ulOptions as DWORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsDDDropFunction(hDictionary as Intptr, pucName as string ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsDDDropLinkPre71(hDBConn as IntPtr , pucLinkedDD as string , usDropGlobal as WORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsDDDropPackage(hDictionary as Intptr, pucName as string ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsDDEnableTriggers(hDictionary as Intptr, pucObjectName as string , pucParent as string , ulOptions as DWORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsDDExecuteProcedure(hDictionary as Intptr, pucProcName as string , pucInput as string , pucOutput as string , pulRowsAffected out DWORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsDDGetObjectProperty(hDictionary as Intptr, usObjectType as WORD , pucParent as string , pucName as string , usPropertyID as WORD ,  pucProperty as char[] , pusPropertyLen ref WORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsDDGetObjectProperty(hDictionary as Intptr, usObjectType as WORD , pucParent as string , pucName as string , usPropertyID as WORD ,  pvProperty as byte[] , pusPropertyLen ref WORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsDDGetObjectProperty(hDictionary as Intptr, usObjectType as WORD , pucParent as string , pucName as string , usPropertyID as WORD , pusProperty ref WORD , pusPropertyLen ref WORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsDDOpen( pucDictionaryPath as string, pucPassword as string , phDictionary out IntPtr ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsDDSetActiveDictionary(hConnect as Intptr, pucLinkName as string , phDictionary out IntPtr ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsDDSetObjectProperty(hDictionary as Intptr, usObjectType as WORD , pucParent as string , pucName as string , usPropertyID as WORD ,  pucProperty as char[] , usPropertyLen as WORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsDDSetObjectProperty(hDictionary as Intptr, usObjectType as WORD , pucParent as string , pucName as string , usPropertyID as WORD ,  pvProperty as byte[] , usPropertyLen as WORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsDDSetObjectProperty(hDictionary as Intptr, usObjectType as WORD , pucParent as string , pucName as string , usPropertyID as WORD , pusProperty ref WORD , usPropertyLen as WORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsDDSetTriggerProperty(hDictionary as Intptr,  pucTriggerName as string, usPropertyID as WORD , pucProperty as string , usPropertyLen as WORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsDDVerifyUserRights(hObject as IntPtr , pucTableName as string , pulUserRights out DWORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsDeactivateAOF(hTable as IntPtr )as DWORD
		PUBLIC STATIC EXTERN METHOD AdsDeleteFile(hConnect as Intptr, pucFileName as string ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsDeleteTable(hTable as IntPtr ) as DWORD
		PUBLIC STATIC EXTERN METHOD AdsEcho(hConnect as Intptr, pucData as string , usLen as WORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsEvalExpr(hTable as Intptr, pucPCode as string,  pucResult as char[] , pusLen ref WORD ) as DWORD
		PUBLIC STATIC EXTERN METHOD AdsExpressionLongToShort(hTable as Intptr, pucLongExpr as string ,  pucShortExpr as char[] , pusBufferLen ref WORD )as DWORD
		PUBLIC STATIC EXTERN METHOD AdsExpressionLongToShort90(hTable as Intptr, pucLongExpr as string ,  pucShortExpr as char[] , pulBufferLen ref DWORD ) as DWORD
		PUBLIC STATIC EXTERN METHOD AdsExpressionShortToLong(hTable as Intptr, pucShortExpr as string ,  pucLongExpr as char[] , pusBufferLen ref WORD )as DWORD
		PUBLIC STATIC EXTERN METHOD AdsExpressionShortToLong90(hTable as Intptr, pucShortExpr as string ,  pucLongExpr as char[] , pulBufferLen ref DWORD ) as DWORD
		PUBLIC STATIC EXTERN METHOD AdsExtractPathPart(usPart as WORD , pucFile as string , pucPart as char[] , pusPartLen ref WORD )as DWORD
		PUBLIC STATIC EXTERN METHOD AdsFreeExpr(hTable as Intptr, pucPCode as string) as DWORD
		PUBLIC STATIC EXTERN METHOD AdsGetBaseFieldName(hTbl as Intptr, usFld as WORD ,  pucName as char[] , pusBufLen ref WORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsGetBaseFieldNum(hCursor as IntPtr , pucColumnName as string , pusBaseFieldNum out WORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsGetColumnPermissions(hTable as Intptr, usColumnNum as WORD , pucPermissions as string ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsGetCursorAOF(hCursor as IntPtr,  pucFilter as char[] , pusFilterLen ref WORD ) as DWORD
		PUBLIC STATIC EXTERN METHOD AdsGetFTSScore(hIndex as Intptr, ulRecord as DWORD , pucKey as string , usKeyLen as WORD , usDataType as WORD , usSeekType as WORD , pulScore out DWORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsGetFieldRaw(hTbl as Intptr, lFieldOrdinal as DWORD , pucBuf as byte[] , pulLen ref DWORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsGetFieldRaw(hTbl as Intptr, pucFldName as string , pucBuf as byte[] , pulLen ref DWORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsGetIndexFlags(hIndex as Intptr, pulFlags out DWORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsGetIndexPageSize(hIndex as Intptr, pulPageSize out DWORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsGetNullRecord(hTbl as Intptr, pucBuf as string , ulLen as DWORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsGetNumSegments(hTag as Intptr, usSegments out WORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsGetPreparedFields(hStatement as Intptr,  pucBuffer as char[] , pulBufferLen ref DWORD , ulOptions as DWORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsGetROWIDPrefix(hTable as Intptr, pucRowIDPrefix as string , usBufferLen as WORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsGetSQLStmtParams(pucStatement as string ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsGetSegmentFieldNumbers(hTag as Intptr, pusNumSegments out WORD , pusSegFieldNumbers as WORD[] )as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsGetSegmentFieldname(hTag as Intptr, usSegmentNum as WORD , pucFieldname as char[], pusFldnameLen ref WORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsGetSegmentOffset(hTag as Intptr, usSegmentNum as WORD , usOffset out WORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsGetTableWAN(hTable as Intptr, pusWAN out WORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsGotoBOF(hObj as IntPtr ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsInternalCloseCachedTables(hConnect as Intptr, usOpen as WORD ) as DWORD
		PUBLIC STATIC EXTERN METHOD AdsIsIndexExprValid(hTbl as Intptr, pucExpr as string , pbValid out WORD ) as DWORD
		PUBLIC STATIC EXTERN METHOD AdsIsSegmentDescending(hTag as Intptr, usSegmentNum as WORD , pbDescending out WORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsLockRecordImplicitly(hTbl as Intptr, ulRec as DWORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsMemCompare90(hConnect as Intptr, pucStr1 as string , ulStr1Len as DWORD , pucStr2 as string , ulStr2Len as DWORD , usCharSet as WORD , ulCollationID as DWORD , psResult out short ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsMemICompare(hConnect as Intptr, pucStr1 as string , ulStr1Len as DWORD , pucStr2 as string , ulStr2Len as DWORD , usCharSet as WORD , psResult out short ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsMemICompare90(hConnect as Intptr, pucStr1 as string , ulStr1Len as DWORD , pucStr2 as string , ulStr2Len as DWORD , usCharSet as WORD , ulCollationID as DWORD , psResult out short ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsMemLwr(hConnect as Intptr, pucStr as string , usStrLen as WORD , usCharSet as WORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsMemLwr90(hConnect as Intptr, pucStr as string , usStrLen as WORD , usCharSet as WORD , ulCollationID as DWORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsMergeAOF(hTable as IntPtr ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsPerformRI(hTable as Intptr, ulRecNum as DWORD , pucRecBuffer as string ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsPrepareSQLNow(hStatement as Intptr, pucSQL as string ,  pucFieldInfo as char[] , pusFieldInfoLen ref WORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsReadRecordNumbers(hObj as Intptr, ulRecordNum AS DWORD , ucDirection as byte , pulRecords out DWORD , pulArrayLen ref DWORD , pusHitEOF out WORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsReadRecords(hObj as Intptr, ulRecordNum as DWORD , cDirection as byte ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsRefreshView(phCursor out IntPtr ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsReleaseObject(hObj as IntPtr ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsRemoveSQLComments(pucStatement as string ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsRestoreDatabase(hConnect as Intptr, hOutputTable as IntPtr , pucSourcePath as string , pucSourcePassword as string , pucDestPath as string , pucDestPassword as string , pucOptions as string , pucFreeTablePasswords as string , usCharType as WORD , usLockingMode as WORD , usCheckRights as WORD , usTableType as WORD , pucCollation as string , ucDDConn as byte ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsSetBOFFlag(hTbl as Intptr, usBOF as WORD )as DWORD
		PUBLIC STATIC EXTERN METHOD AdsSetBaseTableAccess(hTbl as Intptr, usAccessBase as WORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsSetCollationSequence(pucSequence as string ) as DWORD
		PUBLIC STATIC EXTERN METHOD AdsSetCursorAOF(hTable as Intptr, pucFilter as string , usResolve as WORD ) as DWORD
		PUBLIC STATIC EXTERN METHOD AdsSetFieldRaw(hObj as Intptr, pucFldName as string , pucBuf as byte[] , ulLen as DWORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsSetFlushFlag(hConnect as Intptr, usFlushEveryUpdate as WORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsSetInternalError(ulErrCode as DWORD , pucFile as string , ulLine as DWORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsSetLastError(ulErrCode as DWORD , pucDetails as string ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsSetPacketSize(hConnect as Intptr, usPacketLength as WORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsSetProperty(hObj as Intptr, ulOperation as DWORD , ulValue as DWORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsSetProperty90(hObj as Intptr, ulOperation as DWORD , uqValue as UINT64 ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsSetRecordPartial(hObj as Intptr, pucRec as string , ulLen as DWORD ) as DWORD 
        PUBLIC STATIC EXTERN METHOD AdsSetTableCharType(hTbl as Intptr, usCharType as WORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsSetTimeStampRaw(hObj as Intptr, lFieldOrdinal as DWORD , pucBuf ref UINT64 , ulLen as DWORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsSetTimeStampRaw(hObj as Intptr, pucFldName as string , pucBuf ref UINT64 , ulLen as DWORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsSetupRI(hConnection as IntPtr , lTableID as int , ucOpen as byte , ulServerWAN as DWORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsSqlPeekStatement(hCursor as IntPtr, IsLive out byte ) as DWORD
		PUBLIC STATIC EXTERN METHOD AdsStepIndexKey(hIndex as Intptr, pucKey as string , usLen as WORD,  sDirection as short ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsValidateThread() as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsVerifyRI(hConnect as Intptr, usExclusive as WORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsWaitForObject(hObj as Intptr, ulOptions as DWORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD ObsAdsDecryptBuffer(pucPassword as string , pucBuffer as string , usLen as WORD ) as DWORD
		PUBLIC STATIC EXTERN METHOD ObsAdsEncryptBuffer(pucPassword as string , pucBuffer as string , usLen as WORD ) as DWORD
		*/
		
	end class
END NAMESPACE