USING System
USING System.Runtime.InteropServices

BEGIN NAMESPACE XSharp.ADS

	PUBLIC CLASS ACEUNPUB
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
		PUBLIC CONST MAX_STRING_CHECK := 5000 AS WORD
		PUBLIC CONST ERROR_FILE_NOT_FOUND := 2 AS WORD
		PUBLIC CONST ADS_FOXGENERAL := 51 AS WORD
		PUBLIC CONST ADS_FOXPICTURE := 52 AS WORD
		PUBLIC CONST ADS_AQE_SERVER := 32768 AS WORD
		PUBLIC CONST ADS_DICTIONARY_SERVER := 16384 AS WORD
		PUBLIC CONST ADS_INTERNAL_CONNECTION := 8192 AS WORD
		PUBLIC CONST ADS_LOCAL_ASA_CONNECTION := -1 AS SHORT
		PUBLIC CONST ADS_READ_NEXT := 1 AS WORD
		PUBLIC CONST ADS_READ_PREV := 2 AS WORD
		PUBLIC CONST ADS_DD_INDEX_FILE_PATH_UNPUB := 6900 AS WORD
		PUBLIC CONST ADS_DD_INDEX_FILE_TYPE_UNPUB := 6901 AS WORD
		PUBLIC CONST ADS_DD_USER_KEYS := 6902 AS WORD
		PUBLIC CONST ADS_DD_INDEX_FILE_RELATIVE_PATH_UNPUB := 6903 AS WORD
		PUBLIC CONST ADS_DD_USER_KEYS_AND_UPDATE := 6904 AS WORD
		PUBLIC CONST ADS_DD_DATABASE_ID := 6905 AS WORD
		PUBLIC CONST ADS_DD_INDEX_FILE_PAGESIZE_UNPUB := 6906 AS WORD
		PUBLIC CONST ADS_DD_DEPLOY_PASSWORD := 6907 AS WORD
		PUBLIC CONST ADS_DD_USER_ENABLE_INTERNET := 6908 AS WORD
		PUBLIC CONST ADS_DD_LINK_AUTHENTICATE := 6909 AS WORD
		PUBLIC CONST ADS_DD_PROC_EXECUTE_PRIV := 6910 AS WORD
		PUBLIC CONST ADS_DD_REFRESH_TABLE_PASSWORD := 6911 AS WORD
		PUBLIC CONST ADS_DD_UPDATE_ADMIN_KEY := 6912 AS WORD
		PUBLIC CONST ADS_DD_TABLE_OPTIONS := 6913 AS WORD
		PUBLIC CONST ADS_DD_FIELD_NAME_UNPUB := 6914 AS WORD
		PUBLIC CONST ADS_DD_USER_PASSWORD_ENCRYPTED := 6931 AS WORD
		PUBLIC CONST ADS_DD_ADMIN_PASSWORD_ENCRYPTED := 6932 AS WORD
		PUBLIC CONST ADS_DD_ENCRYPT_TABLE_PASSWORD_ENCRYPTED := 6933 AS WORD
		PUBLIC CONST ADS_DD_DISABLE_USER_INTERNET := 6934 AS WORD
		PUBLIC CONST ADS_DD_USER_LOGINS_DISABLED := 6935 AS WORD
		PUBLIC CONST ADS_DD_SUBSCR_PASSWORD_ENCRYPTED := 6936 AS WORD
		PUBLIC CONST ADS_DD_READ_OBJECT_NAME := 6937 AS WORD
		PUBLIC CONST ADS_DD_FUNCTION_INPUT_PARAM_COUNT := 6938 AS WORD
		PUBLIC CONST ADS_DD_FUNCTION_INPUT := 6939 AS WORD
		PUBLIC CONST ADS_DD_FUNCTION_OUTPUT := 6940 AS WORD
		PUBLIC CONST ADS_DD_FUNCTION_SCRIPT := 6941 AS WORD
		PUBLIC CONST ADS_DD_FUNCTION_EVAL_PERMISSION := 6942 AS WORD
		PUBLIC CONST ADS_DD_LOCK_OBJECT := 6943 AS WORD
		PUBLIC CONST ADS_DD_UNLOCK_OBJECT := 6944 AS WORD
		PUBLIC CONST ADS_DD_UNLOCK_OBJECT_CANCEL_UPDATE := 6945 AS WORD
		PUBLIC CONST ADS_DD_DATABASE_ID_UPDATE := 6946 AS WORD
		PUBLIC CONST ADS_DD_USER_DB_ROLES := 6947 AS WORD
		PUBLIC CONST ADS_DD_USER_UPDATED_DB_ROLES := 6948 AS WORD
		PUBLIC CONST ADS_DD_VERIFY_DEBUG_PERMISSION := 6949 AS WORD
		PUBLIC CONST ADS_DD_VERIFY_MISSING_PERMISSIONS := 6950 AS WORD
		PUBLIC CONST ADS_DD_VERIFY_ALTDROP_PERMISSIONS := 6951 AS WORD
		PUBLIC CONST ADS_DD_DELETE_INDEX := 6952 AS WORD
		PUBLIC CONST ADS_DD_ADD_INDEX := 6953 AS WORD
		PUBLIC CONST ADS_DD_OBJ_RIGHT_WRITE := 2 AS WORD
		PUBLIC CONST ADS_DD_TABLE_ENCRYPTED := 1 AS WORD
		PUBLIC CONST ADS_DD_TABLE_IS_OEM := 2 AS WORD
		PUBLIC CONST ADS_DD_TABLE_ALLOW_SEARCH_NRC := 4 AS WORD
		PUBLIC CONST ADS_DD_TABLE_STATIC_CURSOR_ONLY := 8 AS WORD
		PUBLIC CONST ADS_ALTER_OBJECT := 0x80000000 AS DWORD
		PUBLIC CONST ADS_LOGICAL_AND := 1 AS WORD
		PUBLIC CONST ADS_LOGICAL_OR := 2 AS WORD
		PUBLIC CONST AOF_NORMAL_TYPE := 1 AS WORD
		PUBLIC CONST AOF_CURSOR_TYPE := 2 AS WORD
		PUBLIC CONST ADS_LINK_TABLE_DELIMITOR := ':' AS CHAR
		PUBLIC CONST ADS_PACKAGE_DELIMITOR := ':' AS CHAR
		PUBLIC CONST ADS_QUALIFIED_NAME_DELIMITOR := ':' AS CHAR
		PUBLIC CONST TRIG_BEFORE_UPDATE := 1 AS WORD
		PUBLIC CONST TRIG_AFTER_UPDATE := 2 AS WORD
		PUBLIC CONST TRIG_CONFLICT_UPDATE := 3 AS WORD
		PUBLIC CONST AQE_STMT_NO_JOIN_ORDER_OPT := "NOJOINORDEROPT" AS STRING
		PUBLIC CONST AQE_STMT_NO_PUSHDOWN_SORT_OPT := "NOPUSHDOWNSORTOPT" AS STRING
		PUBLIC CONST AQE_STMT_NO_EXECUTION_OPT := "NOEXECUTIONOPT" AS STRING
		PUBLIC CONST AQE_STMT_NO_MISC_OPT := "NOMISCOPT" AS STRING
		PUBLIC CONST AQE_STMT_NO_SUBQUERY_OPT := "NOSUBQUERYOPT" AS STRING
		PUBLIC CONST ADS_ROWID_PREFIX_LEN := 12 AS WORD
		PUBLIC CONST AQE_NO_TOPX := 0xFFFFFFFF AS DWORD
		PUBLIC CONST ADS_DD_DISABLE_READ_PROP_CHK := 1 AS WORD
		PUBLIC CONST ADS_DD_DISABLE_SET_PROP_CHK := 2 AS WORD
		PUBLIC CONST ADS_DD_DISABLE_CREATE_RIGHTS_CHK := 4 AS WORD
		PUBLIC CONST ADS_SET_LARGE_BLOCK_READS := 1 AS WORD
		PUBLIC CONST ADS_SET_BATCH_INSERTS := 2 AS WORD
		PUBLIC CONST ADS_UNIQUE_KEY_ENFORCEMENT := 3 AS WORD
		PUBLIC CONST ADS_RI_ENFORCEMENT := 4 AS WORD
		PUBLIC CONST ADS_AUTO_INCREMENT_ENFORCEMENT := 5 AS WORD
		PUBLIC CONST ADS_MOVE_SERVER_OP_COUNT := 6 AS WORD
		PUBLIC CONST ADS_SET_TRIG_IGNORE_EVENT := 7 AS WORD
		PUBLIC CONST ADS_VERIFY_FTS_INDEXES := 8 AS WORD
		PUBLIC CONST ADS_SET_FORCE_CLOSED := 9 AS WORD
		PUBLIC CONST ADS_SET_NOTRANS_TABLE := 10 AS WORD
		PUBLIC CONST ADS_SET_DDCONN_TRUEUSER := 11 AS WORD
		PUBLIC CONST ADS_SET_OUTPUT_TABLE := 12 AS WORD
		PUBLIC CONST ADS_GET_QUERY_ELEMENTS := 13 AS WORD
		PUBLIC CONST ADS_SKIP_BEGIN_TRIGS := 14 AS WORD
		PUBLIC CONST ADS_SET_DISABLE_PERMISSION := 16 AS WORD
		PUBLIC CONST ADS_ADD_DISABLE_PERMISSION := 17 AS WORD
		PUBLIC CONST ADS_DEBUG_CONNECTION := 18 AS WORD
		PUBLIC CONST ADS_GET_KEY_VALUES := 19 AS WORD
		PUBLIC CONST ADS_FREE_KEY_VALUES := 20 AS WORD
		PUBLIC CONST ADS_SET_DEBUG_MASK := 21 AS WORD
		PUBLIC CONST ADS_VERIFY_ADT := 22 AS WORD
		PUBLIC CONST ADS_SET_NO_RI := 23 AS WORD
		PUBLIC CONST ADS_VERIFY_MEMORY_TABLE := 24 AS WORD
		PUBLIC CONST ADS_ALLOW_SMC_CONNECTIONS := 25 AS WORD
		PUBLIC CONST ADS_SET_CONNINFO_INDEX := 26 AS WORD
		PUBLIC CONST ADS_RETRY_ADS_CONNECTS := 27 AS WORD
		PUBLIC CONST ADS_PUSH_ERROR_STACK := 28 AS WORD
		PUBLIC CONST ADS_POP_ERROR_STACK := 29 AS WORD
		PUBLIC CONST ADS_USE_PTHREAD_FOR_KA := 30 AS WORD
		PUBLIC CONST ADS_SET_REPLICATION_INFO := 31 AS WORD
		PUBLIC CONST ADS_GET_SERVER_USER_ID := 32 AS WORD
		PUBLIC CONST ADS_IGNORE_CS_TESTS := 33 AS WORD
		PUBLIC CONST ADS_ENFORCE_MODTIME_VALUES := 34 AS WORD
		PUBLIC CONST ADS_ENFORCE_ROWVERSION_VALUES := 35 AS WORD
		PUBLIC CONST ADS_DUMP_REPLICATION_CACHE := 36 AS WORD
		PUBLIC CONST ADS_GET_VIEW_STMT := 37 AS WORD
		PUBLIC CONST ADS_IN_PROC_OR_TRIG := 38 AS WORD
		PUBLIC CONST ADS_VERIFY_SCRIPT := 39 AS WORD
		PUBLIC CONST ADS_SET_MAX_CACHE_MEMORY := 40 AS WORD
		PUBLIC CONST ADS_PERSIST_CACHED_TABLE := 41 AS WORD
		PUBLIC CONST ADS_SET_VALUES_TABLE_FLAG := 42 AS WORD
		PUBLIC CONST ADS_GET_VALUES_TABLE_FLAG := 43 AS WORD
		PUBLIC CONST ADS_VERIFY_EXTERNAL_CALL_HANDLE := 44 AS WORD
		PUBLIC CONST ADS_GET_SERVER_RELEASE_BUILD := 45 AS WORD
		PUBLIC CONST ADS_SET_AVAIL_REINDEX_RAM := 46 AS WORD
		PUBLIC CONST ADS_SET_AVAIL_REINDEX_HDD := 47 AS WORD
		PUBLIC CONST ADS_GET_REINDEX_GROUP_NUM := 48 AS WORD
		PUBLIC CONST ADS_SET_REINDEX_SORTBUFFER_LEN := 49 AS WORD
		PUBLIC CONST ADS_SET_PULLING_DEBUG_TRIGGER := 50 AS WORD
		PUBLIC CONST ADS_FLIP_DESCEND_FLAG := 51 AS WORD
		PUBLIC CONST ADS_GET_ACE_ID_STR := 52 AS WORD
		PUBLIC CONST ADS_GET_SERVER_COLLATION_ID := 53 AS WORD
		PUBLIC CONST ADS_SET_REPLICATION_STMT := 54 AS WORD
		PUBLIC CONST ADS_GET_REPLICATION_STMT := 55 AS WORD
		PUBLIC CONST TRIG_NOT_VALUE_TABLE := 0 AS WORD
		PUBLIC CONST TRIG_OLD_TABLE := 1 AS WORD
		PUBLIC CONST TRIG_NEW_TABLE := 2 AS WORD
		PUBLIC CONST TRIG_ERROR_TABLE := 3 AS WORD
		PUBLIC CONST BACKUP_SEVERITY_HIGH := 10 AS WORD
		PUBLIC CONST BACKUP_SEVERITY_MEDHIGH := 7 AS WORD
		PUBLIC CONST BACKUP_SEVERITY_MED := 5 AS WORD
		PUBLIC CONST BACKUP_SEVERITY_LOW := 1 AS WORD
		PUBLIC CONST BACKUP_SEVERITY_NONE := 0 AS WORD
		PUBLIC CONST BACKUP_FREEPASSWDALL := "__AllFreeTablePassword" AS STRING
		PUBLIC CONST ADS_NORMAL_RA_CACHE_SIZE := 10 AS WORD
		PUBLIC CONST ADS_AGGRESSIVE_RA_CACHE_SIZE := 100 AS WORD
		PUBLIC CONST ADS_BEFORE_INSERT_TRIG := 1 AS WORD
		PUBLIC CONST ADS_INSTEADOF_INSERT_TRIG := 2 AS WORD
		PUBLIC CONST ADS_AFTER_INSERT_TRIG := 4 AS WORD
		PUBLIC CONST ADS_BEFORE_UPDATE_TRIG := 8 AS WORD
		PUBLIC CONST ADS_INSTEADOF_UPDATE_TRIG := 16 AS WORD
		PUBLIC CONST ADS_AFTER_UPDATE_TRIG := 32 AS WORD
		PUBLIC CONST ADS_BEFORE_DELETE_TRIG := 64 AS WORD
		PUBLIC CONST ADS_INSTEADOF_DELETE_TRIG := 128 AS WORD
		PUBLIC CONST ADS_AFTER_DELETE_TRIG := 256 AS WORD
		PUBLIC CONST ADS_CONFLICTON_UPDATE_TRIG := 512 AS WORD
		PUBLIC CONST ADS_CONFLICTON_DELETE_TRIG := 1024 AS WORD
		PUBLIC CONST ADS_DCM_ALL := 0xFFFFFFFF AS DWORD
		PUBLIC CONST ADS_DCM_ADT_HEADER := 1 AS WORD
		PUBLIC CONST ADS_DCM_ADT_RECYCLE := 2 AS WORD
		PUBLIC CONST ADS_DCM_ADM := 4 AS WORD
		PUBLIC CONST ADS_CREATE_MEMTABLE := 1024 AS WORD
		PUBLIC CONST ADS_DONT_ADD_TO_MEMTABLE_LIST := 2048 AS WORD
		PUBLIC CONST ADS_RETURN_ID_DROP_TABLE := 0x8000 AS WORD
		PUBLIC CONST ADS_TEMP_CURSOR_TABLE := 0x80000 AS DWORD
		PUBLIC CONST ADS_TEMP_TABLE_ANY_NAME := 0x100000 AS DWORD
		PUBLIC CONST ADS_OPEN_TABLE_USING_SQL := 0x200000 AS DWORD
		PUBLIC CONST ADS_NO_TRANSACTION := 0x400000 AS DWORD
		PUBLIC CONST ADS_CREATE_TABLE_OVERWRITE := 0x800000 AS DWORD
		PUBLIC CONST ADS_DONT_ENFORCE_RI := 0x1000000 AS DWORD
		PUBLIC CONST ADS_CREATE_ADD_FILE := 0x2000000 AS DWORD
		PUBLIC CONST ADS_DONT_OPEN_VIEW := 0x4000000 AS DWORD
		PUBLIC CONST ADS_LINK_FULLPATH_IN_PARAM := 0x1000000 AS DWORD
		PUBLIC CONST ADS_LINK_MODIFY := 0x2000000 AS DWORD
		PUBLIC CONST ADS_DISABLE_CONNECTION_CONCURRENCY := 1 AS WORD
		PUBLIC CONST ADS_CURRENT_USER := 1 AS WORD
		PUBLIC CONST ADS_ALL_USERS := 2 AS WORD
		PUBLIC CONST ADS_CACHE_NEW_TABLE := 1 AS WORD
		PUBLIC CONST ADS_RETRIEVE_NULL_FLAG := 1 AS WORD
		PUBLIC CONST ADS_UPDATE_NULL_FLAG := 2 AS WORD
		PUBLIC CONST ADS_UPDATE_FULL_FLAG := 3 AS WORD
		PUBLIC CONST ADS_RETRIEVE_FULL_FLAG := 4 AS WORD
		PUBLIC CONST BASE64CODE := "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" AS STRING
		PUBLIC CONST ADS_DONT_ADD_TO_DD := 69 AS WORD
		#endregion

    #region Constructor
        STATIC PRIVATE Is32 AS LOGIC
        STATIC CONSTRUCTOR  
            Is32 := IntPtr.Size == 4
        RETURN
    #endregion
    #region Method Dispatch Table
		PUBLIC STATIC METHOD AdsConvertStringToJulian(pucJulian AS STRING , usLen AS WORD , pdJulian OUT double ) AS DWORD 
            IF Is32
                RETURN ACEUNPUB32.AdsConvertStringToJulian(pucJulian, usLen, OUT pdJulian)
            ELSE
                RETURN ACEUNPUB64.AdsConvertStringToJulian(pucJulian, usLen, OUT pdJulian)
            ENDIF
		PUBLIC STATIC METHOD AdsConvertStringToJulian(pucJulian AS CHAR[] , usLen AS WORD , pdJulian OUT double ) AS DWORD 
            IF Is32
                RETURN ACEUNPUB32.AdsConvertStringToJulian(pucJulian, usLen, OUT pdJulian)
            ELSE
                RETURN ACEUNPUB64.AdsConvertStringToJulian(pucJulian, usLen, OUT pdJulian)
            ENDIF

		PUBLIC STATIC METHOD AdsSqlPeekStatement(hCursor AS IntPtr, IsLive OUT BYTE ) AS DWORD
            IF Is32
                RETURN ACEUNPUB32.AdsSqlPeekStatement(hCursor, OUT IsLive)
            ELSE
                RETURN ACEUNPUB64.AdsSqlPeekStatement(hCursor, OUT IsLive)
            ENDIF

        PUBLIC STATIC METHOD AdsConvertJulianToString(dJulian AS double , pucJulian AS CHAR[] , pusLen REF WORD ) AS DWORD 
            IF Is32
                RETURN ACEUNPUB32.AdsConvertJulianToString(dJulian, pucJulian, REF pusLen)
            ELSE
                RETURN ACEUNPUB64.AdsConvertJulianToString(dJulian, pucJulian, REF pusLen)
            ENDIF

		PUBLIC STATIC METHOD AdsSetProperty90(hObj AS Intptr, ulOperation AS DWORD , uqValue AS UINT64 ) AS DWORD 
            IF Is32
                RETURN ACEUNPUB32.AdsSetProperty90(hObj, ulOperation, uqValue)
            ELSE
                RETURN ACEUNPUB64.AdsSetProperty90(hObj, ulOperation, uqValue)
            ENDIF
		PUBLIC STATIC METHOD AdsSetLastError(ulErrCode as DWORD , pucDetails as string ) as DWORD 
            IF Is32
                RETURN ACEUNPUB32.AdsSetLastError(ulErrCode, pucDetails)
            ELSE
                RETURN ACEUNPUB64.AdsSetLastError(ulErrCode, pucDetails)
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
		PUBLIC STATIC EXTERN METHOD AdsSetPacketSize(hConnect as Intptr, usPacketLength as WORD ) as DWORD 
		PUBLIC STATIC EXTERN METHOD AdsSetProperty(hObj as Intptr, ulOperation as DWORD , ulValue as DWORD ) as DWORD 
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
		
	END CLASS
END NAMESPACE
