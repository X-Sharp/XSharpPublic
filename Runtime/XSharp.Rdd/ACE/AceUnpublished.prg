USING System
USING System.Runtime.InteropServices

BEGIN NAMESPACE XSharp.ADS

	PUBLIC CLASS ACEUNPUB
		PRIVATE ENUM PathInfo
		
			MEMBER PATH_DRIVE := 1
			MEMBER PATH_SERVER := 2
			MEMBER PATH_VOLUME := 3
			MEMBER PATH_PATH := 4
			MEMBER PATH_BASENAME := 5
			MEMBER PATH_FILENAME := 6
			MEMBER PATH_EXTENSION := 7
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
		PUBLIC CONST ADS_DD_SET_TABLE_TXN_FREE_UNPUB := 6954 AS WORD
		PUBLIC CONST ADS_DD_FUNCTION_SCRIPT_W := 6955 AS WORD
		
		PUBLIC CONST ADS_DD_OBJ_RIGHT_WRITE := 2 AS WORD
		PUBLIC CONST ADS_DD_TABLE_ENCRYPTED := 1 AS WORD
		PUBLIC CONST ADS_DD_TABLE_IS_OEM := 2 AS WORD
		PUBLIC CONST ADS_DD_TABLE_ALLOW_SEARCH_NRC := 4 AS WORD
		PUBLIC CONST ADS_DD_TABLE_STATIC_CURSOR_ONLY := 8 AS WORD
		PUBLIC CONST ADS_DD_TABLE_IGNORES_TRANS := 16 AS WORD
		
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
		PUBLIC CONST ADS_GET_AOF_PLAN := 56 AS WORD
		PUBLIC CONST ADS_GET_REP_DBID := 57 AS WORD
		PUBLIC CONST ADS_SET_REP_DBID := 58 AS WORD
		PUBLIC CONST ADS_GET_ADD_PATH := 59 AS WORD
		PUBLIC CONST ADS_NONEXCLUSIVE_PROPRIETARY := 60 AS WORD
		PUBLIC CONST ADS_GET_TRIGGER_PULL_INFO := 61 AS WORD
		PUBLIC CONST ADS_SET_LPDBC_SP_TYPE_PTR := 62 AS WORD
		PUBLIC CONST ADS_USE_MEMO_READ_LOCKS := 63 AS WORD
		PUBLIC CONST ADS_USE_MEMO_READ_LOCKS_WA := 64 AS WORD
		PUBLIC CONST ADS_USE_MEMO_READ_LOCKS_TABLE := 65 AS WORD
		PUBLIC CONST ADS_USE_MEMO_READ_LOCKS_USER := 66 AS WORD
		PUBLIC CONST ADS_CANCEL_USER_QUERY_EXACT := 67 AS WORD
		PUBLIC CONST ADS_GET_FILE_SIZE := 68 AS WORD
		PUBLIC CONST ADS_GET_RAWFILE_CACHE_STATS := 69 AS WORD
		PUBLIC CONST ADS_DUMP_DLL_CACHE := 70 AS WORD
		PUBLIC CONST ADS_GET_TRANSACTION_COUNT := 71 AS WORD
		PUBLIC CONST ADS_DUMP_TMP_FILE_POOL := 72 AS WORD
		PUBLIC CONST ADS_SET_EQ_WINDOW_SIZE := 73 AS WORD
		PUBLIC CONST ADS_SET_EQ_MAX_THREADS := 74 AS WORD
		PUBLIC CONST ADS_GET_CODEPAGE := 75 AS WORD
		PUBLIC CONST ADS_GET_ICU_LIB_HANDLE := 76 AS WORD
		PUBLIC CONST ADS_GET_CURSOR_AOF_OPT_LEVEL := 77 AS WORD
		PUBLIC CONST ADS_GET_VIEW_STMT_ENCODING := 78 AS WORD
		PUBLIC CONST ADS_GET_REP_CONN_TYPE := 79 AS WORD
		PUBLIC CONST ADS_SET_ENCRYPTION_TYPE := 80 AS WORD
		PUBLIC CONST ADS_GET_COLLATION_INFO := 81 AS WORD
		PUBLIC CONST ADS_DOES_TABLE_SUPPORT_AES := 82 AS WORD
		PUBLIC CONST ADS_GET_ENCRYPTION_TYPE := 83 AS WORD
		PUBLIC CONST ADS_SET_RANDOM_KEY := 84 AS WORD
		PUBLIC CONST ADS_DUMP_AES_KEY_STORE := 85 AS WORD
		PUBLIC CONST ADS_SET_DEFAULT_FIPS_MODE := 86 AS WORD
		PUBLIC CONST ADS_STRONG_ENCRYPTION_SUPPORT := 87 AS WORD
		PUBLIC CONST ADS_SET_DBCAPI_BUFFER := 88 AS WORD
		PUBLIC CONST ADS_GET_DBCAPI_BUFFER := 89 AS WORD
		PUBLIC CONST ADS_IS_FIRST_FETCH := 90 AS WORD
		PUBLIC CONST ADS_DBCAPI_CANCEL := 91 AS WORD
		PUBLIC CONST ADS_GET_HEADER_LENGTH := 92 AS WORD
		
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
		PUBLIC CONST ADS_DONT_GO_TOP := 0x8000000 AS DWORD
		PUBLIC CONST ADS_DONT_OPEN_INDEXES := 0x10000000 AS DWORD
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
        STATIC PRIVATE Is32Bits AS LOGIC
        STATIC CONSTRUCTOR  
            Is32Bits := IntPtr.Size == 4
        RETURN
    #endregion
    #region Method Dispatch Table
		PUBLIC STATIC METHOD AdsConvertStringToJulian(pucJulian AS STRING , usLen AS WORD , pdJulian OUT System.Double ) AS DWORD 
            IF Is32Bits
                RETURN ACEUNPUB32.AdsConvertStringToJulian(pucJulian, usLen, OUT pdJulian)
            ELSE
                RETURN ACEUNPUB64.AdsConvertStringToJulian(pucJulian, usLen, OUT pdJulian)
            ENDIF

		PUBLIC STATIC METHOD AdsConvertStringToJulian(pucJulian AS CHAR[] , usLen AS WORD , pdJulian OUT System.Double ) AS DWORD 
            IF Is32Bits
                RETURN ACEUNPUB32.AdsConvertStringToJulian(pucJulian, usLen, OUT pdJulian)
            ELSE
                RETURN ACEUNPUB64.AdsConvertStringToJulian(pucJulian, usLen, OUT pdJulian)
            ENDIF

		PUBLIC STATIC METHOD AdsSqlPeekStatement(hCursor AS IntPtr, IsLive OUT BYTE ) AS DWORD
            IF Is32Bits
                RETURN ACEUNPUB32.AdsSqlPeekStatement(hCursor, OUT IsLive)
            ELSE
                RETURN ACEUNPUB64.AdsSqlPeekStatement(hCursor, OUT IsLive)
            ENDIF

        PUBLIC STATIC METHOD AdsConvertJulianToString(dJulian AS System.Double , pucJulian AS CHAR[] , pusLen REF WORD ) AS DWORD 
            IF Is32Bits
                RETURN ACEUNPUB32.AdsConvertJulianToString(dJulian, pucJulian, REF pusLen)
            ELSE
                RETURN ACEUNPUB64.AdsConvertJulianToString(dJulian, pucJulian, REF pusLen)
            ENDIF

		PUBLIC STATIC METHOD AdsSetLastError(ulErrCode AS DWORD , pucDetails AS STRING ) AS DWORD 
            IF Is32Bits
                RETURN ACEUNPUB32.AdsSetLastError(ulErrCode, pucDetails)
            ELSE
                RETURN ACEUNPUB64.AdsSetLastError(ulErrCode, pucDetails)
            ENDIF

		PUBLIC STATIC METHOD AdsMemCompare(hConnect AS IntPtr, pucStr1 AS STRING , ulStr1Len AS DWORD , pucStr2 AS STRING , ulStr2Len AS DWORD , usCharSet AS WORD , psResult OUT SHORT ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsMemCompare(hConnect , pucStr1 , ulStr1Len , pucStr2 , ulStr2Len , usCharSet , OUT psResult ) 
			ELSE
				RETURN ACEUNPUB64.AdsMemCompare(hConnect , pucStr1 , ulStr1Len , pucStr2 , ulStr2Len , usCharSet , OUT psResult ) 
			ENDIF

		PUBLIC STATIC METHOD  AdsSetFieldRaw(hObj AS IntPtr, lFieldOrdinal AS DWORD , pucBuf AS BYTE[] , ulLen AS DWORD ) AS DWORD
			IF Is32Bits
				RETURN ACEUNPUB32.AdsSetFieldRaw(hObj , lFieldOrdinal , pucBuf , ulLen ) 
			ELSE
				RETURN ACEUNPUB64.AdsSetFieldRaw(hObj , lFieldOrdinal , pucBuf , ulLen ) 
			ENDIF

		PUBLIC STATIC METHOD AdsAccessVfpSystemField(hTable AS IntPtr, lFieldOrdinal AS DWORD , pucBuffer AS STRING , ulOptions AS DWORD , puFlag OUT WORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsAccessVfpSystemField(hTable , lFieldOrdinal , pucBuffer , ulOptions ,  OUT puFlag ) 
			ELSE
				RETURN ACEUNPUB64.AdsAccessVfpSystemField(hTable , lFieldOrdinal , pucBuffer , ulOptions ,  OUT puFlag ) 
			ENDIF

		PUBLIC STATIC METHOD AdsAccessVfpSystemField(hTable AS IntPtr, pucFldName AS STRING , pucBuffer AS STRING , ulOptions AS DWORD , puFlag OUT WORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsAccessVfpSystemField(hTable , pucFldName , pucBuffer , ulOptions ,  OUT puFlag ) 
			ELSE
				RETURN ACEUNPUB64.AdsAccessVfpSystemField(hTable , pucFldName , pucBuffer , ulOptions ,  OUT puFlag ) 
			ENDIF

		PUBLIC STATIC METHOD AdsActivateAOF(hTable AS IntPtr )AS DWORD
			IF Is32Bits
				RETURN ACEUNPUB32.AdsActivateAOF(hTable  )
			ELSE
				RETURN ACEUNPUB64.AdsActivateAOF(hTable  )
			ENDIF

		PUBLIC STATIC METHOD AdsAddToAOF(hTable AS IntPtr, pucFilter AS STRING , usOperation AS WORD , usWhichAOF AS WORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsAddToAOF(hTable , pucFilter , usOperation , usWhichAOF ) 
			ELSE
				RETURN ACEUNPUB64.AdsAddToAOF(hTable , pucFilter , usOperation , usWhichAOF ) 
			ENDIF

		PUBLIC STATIC METHOD AdsBackupDatabase(hConnect AS IntPtr, hOutputTable AS IntPtr , pucSourcePath AS STRING , pucSourceMask AS STRING , pucDestPath AS STRING , pucOptions AS STRING , pucFreeTablePasswords AS STRING , usCharType AS WORD , usLockingMode AS WORD , usCheckRights AS WORD , usTableType AS WORD , pucCollation AS STRING , ucDDConn AS BYTE ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsBackupDatabase(hConnect , hOutputTable  , pucSourcePath , pucSourceMask , pucDestPath , pucOptions , pucFreeTablePasswords , usCharType , usLockingMode , usCheckRights , usTableType , pucCollation , ucDDConn ) 
			ELSE
				RETURN ACEUNPUB64.AdsBackupDatabase(hConnect , hOutputTable  , pucSourcePath , pucSourceMask , pucDestPath , pucOptions , pucFreeTablePasswords , usCharType , usLockingMode , usCheckRights , usTableType , pucCollation , ucDDConn ) 
			ENDIF

		PUBLIC STATIC METHOD AdsBuildKeyFromRecord(hTag AS IntPtr, mpucRecBuffer AS STRING , ulRecordLen AS DWORD , pucKey AS CHAR[], pusKeyLen REF WORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsBuildKeyFromRecord(hTag , mpucRecBuffer , ulRecordLen , pucKey , REF pusKeyLen ) 
			ELSE
				RETURN ACEUNPUB64.AdsBuildKeyFromRecord(hTag , mpucRecBuffer , ulRecordLen , pucKey , REF pusKeyLen ) 
			ENDIF

		PUBLIC STATIC METHOD AdsClearCursorAOF(hTable AS IntPtr ) AS DWORD
			IF Is32Bits
				RETURN ACEUNPUB32.AdsClearCursorAOF(hTable  ) 
			ELSE
				RETURN ACEUNPUB64.AdsClearCursorAOF(hTable  ) 
			ENDIF

		PUBLIC STATIC METHOD AdsClearLastError() AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsClearLastError() 
			ELSE
				RETURN ACEUNPUB64.AdsClearLastError() 
			ENDIF

		PUBLIC STATIC METHOD AdsClearRecordBuffer(hTbl AS IntPtr, pucBuf AS STRING , ulLen AS DWORD ) AS DWORD
			IF Is32Bits
				RETURN ACEUNPUB32.AdsClearRecordBuffer(hTbl , pucBuf , ulLen ) 
			ELSE
				RETURN ACEUNPUB64.AdsClearRecordBuffer(hTbl , pucBuf , ulLen ) 
			ENDIF

		PUBLIC STATIC METHOD AdsCloseCachedTrigStatements(hConnection AS IntPtr , lTableID AS INT ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsCloseCachedTrigStatements(hConnection  , lTableID ) 
			ELSE
				RETURN ACEUNPUB64.AdsCloseCachedTrigStatements(hConnection  , lTableID ) 
			ENDIF

		PUBLIC STATIC METHOD AdsConvertDateToJulian(hConnect AS IntPtr, pucDate AS STRING , usLen AS WORD , pdJulian OUT System.Double ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsConvertDateToJulian(hConnect , pucDate , usLen , OUT pdJulian ) 
			ELSE
				RETURN ACEUNPUB64.AdsConvertDateToJulian(hConnect , pucDate , usLen , OUT pdJulian ) 
			ENDIF

		PUBLIC STATIC METHOD AdsConvertKeyToDouble(pucKey AS STRING , pdValue OUT System.Double ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsConvertKeyToDouble(pucKey , OUT pdValue ) 
			ELSE
				RETURN ACEUNPUB64.AdsConvertKeyToDouble(pucKey , OUT pdValue ) 
			ENDIF

		PUBLIC STATIC METHOD AdsConvertMillisecondsToString(ulMSeconds AS DWORD, pucTime AS CHAR[] , pusLen REF WORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsConvertMillisecondsToString(ulMSeconds  , pucTime  , REF pusLen ) 
			ELSE
				RETURN ACEUNPUB64.AdsConvertMillisecondsToString(ulMSeconds  , pucTime  , REF pusLen ) 
			ENDIF

		PUBLIC STATIC METHOD AdsConvertStringToMilliseconds(pucTime AS STRING , usLen AS WORD , pulMSeconds OUT DWORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsConvertStringToMilliseconds(pucTime , usLen ,  OUT pulMSeconds ) 
			ELSE
				RETURN ACEUNPUB64.AdsConvertStringToMilliseconds(pucTime , usLen ,  OUT pulMSeconds ) 
			ENDIF

		PUBLIC STATIC METHOD AdsCopyTableStructure81(hTable AS IntPtr, pucFile AS STRING , ulOptions AS DWORD ) AS DWORD
			IF Is32Bits
				RETURN ACEUNPUB32.AdsCopyTableStructure81(hTable , pucFile , ulOptions ) 
			ELSE
				RETURN ACEUNPUB64.AdsCopyTableStructure81(hTable , pucFile , ulOptions ) 
			ENDIF

		PUBLIC STATIC METHOD AdsCopyTableTop(hObj AS IntPtr, hDestTbl AS IntPtr , ulNumTopRecords AS DWORD) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsCopyTableTop(hObj , hDestTbl  , ulNumTopRecords ) 
			ELSE
				RETURN ACEUNPUB64.AdsCopyTableTop(hObj , hDestTbl  , ulNumTopRecords ) 
			ENDIF

		PUBLIC STATIC METHOD AdsCreateCriticalSection(hObj AS IntPtr, ulOptions AS DWORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsCreateCriticalSection(hObj , ulOptions ) 
			ELSE
				RETURN ACEUNPUB64.AdsCreateCriticalSection(hObj , ulOptions ) 
			ENDIF

		PUBLIC STATIC METHOD AdsCreateMemTable(hConnection AS IntPtr , pucName AS STRING , usTableType AS WORD , usCharType AS WORD , pucFields AS STRING , ulSize AS DWORD , phTable OUT IntPtr ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsCreateMemTable(hConnection  , pucName , usTableType , usCharType , pucFields , ulSize ,  OUT phTable ) 
			ELSE
				RETURN ACEUNPUB64.AdsCreateMemTable(hConnection  , pucName , usTableType , usCharType , pucFields , ulSize ,  OUT phTable ) 
			ENDIF

		PUBLIC STATIC METHOD AdsCreateMemTable90(hConnection AS IntPtr , pucName AS STRING , usTableType AS WORD , usCharType AS WORD , pucFields AS STRING , ulSize AS DWORD , pucCollation AS STRING , phTable OUT IntPtr ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsCreateMemTable90(hConnection  , pucName , usTableType , usCharType , pucFields , ulSize , pucCollation , OUT phTable ) 
			ELSE
				RETURN ACEUNPUB64.AdsCreateMemTable90(hConnection  , pucName , usTableType , usCharType , pucFields , ulSize , pucCollation , OUT phTable ) 
			ENDIF

		PUBLIC STATIC METHOD AdsDBFDateToString(pucDBFDate AS STRING , pucFormattedDate AS STRING )AS DWORD
			IF Is32Bits
				RETURN ACEUNPUB32.AdsDBFDateToString(pucDBFDate , pucFormattedDate )
			ELSE
				RETURN ACEUNPUB64.AdsDBFDateToString(pucDBFDate , pucFormattedDate )
			ENDIF

		PUBLIC STATIC METHOD AdsDDAutoCreateIndex(hConnect AS IntPtr, pucTableName AS STRING , pucIndexName AS STRING , pucCollation AS STRING ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsDDAutoCreateIndex(hConnect , pucTableName , pucIndexName , pucCollation ) 
			ELSE
				RETURN ACEUNPUB64.AdsDDAutoCreateIndex(hConnect , pucTableName , pucIndexName , pucCollation ) 
			ENDIF

		PUBLIC STATIC METHOD AdsDDAutoCreateTable(hConnect AS IntPtr, pucTableName AS STRING , pucCollation AS STRING ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsDDAutoCreateTable(hConnect , pucTableName , pucCollation ) 
			ELSE
				RETURN ACEUNPUB64.AdsDDAutoCreateTable(hConnect , pucTableName , pucCollation ) 
			ENDIF

		PUBLIC STATIC METHOD AdsDDClose(hDictionary AS IntPtr ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsDDClose(hDictionary  ) 
			ELSE
				RETURN ACEUNPUB64.AdsDDClose(hDictionary  ) 
			ENDIF

		PUBLIC STATIC METHOD AdsDDCreateASA(hConnect AS IntPtr, pucDictionaryPath AS STRING , usEncrypt AS WORD , pucDescription AS STRING , pucPassword AS STRING ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsDDCreateASA(hConnect , pucDictionaryPath , usEncrypt , pucDescription , pucPassword ) 
			ELSE
				RETURN ACEUNPUB64.AdsDDCreateASA(hConnect , pucDictionaryPath , usEncrypt , pucDescription , pucPassword ) 
			ENDIF

		PUBLIC STATIC METHOD AdsDDCreateFunction(hDictionary AS IntPtr, pucName AS STRING , pucReturnType AS STRING , usInputParamCnt AS WORD , pucInputParams AS STRING , pucFuncBody AS STRING , pucComments AS STRING ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsDDCreateFunction(hDictionary , pucName , pucReturnType , usInputParamCnt , pucInputParams , pucFuncBody , pucComments ) 
			ELSE
				RETURN ACEUNPUB64.AdsDDCreateFunction(hDictionary , pucName , pucReturnType , usInputParamCnt , pucInputParams , pucFuncBody , pucComments ) 
			ENDIF

		PUBLIC STATIC METHOD AdsDDCreateLinkPre71(hDBConn AS IntPtr , pucLinkAlias AS STRING , pucLinkedDDPath AS STRING , pucUserName AS STRING , pucPassword AS STRING , ulOptions AS DWORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsDDCreateLinkPre71(hDBConn  , pucLinkAlias , pucLinkedDDPath , pucUserName , pucPassword , ulOptions  ) 
			ELSE
				RETURN ACEUNPUB64.AdsDDCreateLinkPre71(hDBConn  , pucLinkAlias , pucLinkedDDPath , pucUserName , pucPassword , ulOptions  ) 
			ENDIF

		PUBLIC STATIC METHOD AdsDDCreatePackage(hDictionary AS IntPtr, pucName AS STRING , pucComments AS STRING ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsDDCreatePackage(hDictionary , pucName , pucComments ) 
			ELSE
				RETURN ACEUNPUB64.AdsDDCreatePackage(hDictionary , pucName , pucComments ) 
			ENDIF

		PUBLIC STATIC METHOD AdsDDDisableTriggers(hDictionary AS IntPtr, pucObjectName AS STRING , pucParent AS STRING , ulOptions AS DWORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsDDDisableTriggers(hDictionary , pucObjectName , pucParent , ulOptions  ) 
			ELSE
				RETURN ACEUNPUB64.AdsDDDisableTriggers(hDictionary , pucObjectName , pucParent , ulOptions  ) 
			ENDIF

		PUBLIC STATIC METHOD AdsDDDropFunction(hDictionary AS IntPtr, pucName AS STRING ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsDDDropFunction(hDictionary , pucName ) 
			ELSE
				RETURN ACEUNPUB64.AdsDDDropFunction(hDictionary , pucName ) 
			ENDIF

		PUBLIC STATIC METHOD AdsDDDropLinkPre71(hDBConn AS IntPtr , pucLinkedDD AS STRING , usDropGlobal AS WORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsDDDropLinkPre71(hDBConn  , pucLinkedDD , usDropGlobal ) 
			ELSE
				RETURN ACEUNPUB64.AdsDDDropLinkPre71(hDBConn  , pucLinkedDD , usDropGlobal ) 
			ENDIF

		PUBLIC STATIC METHOD AdsDDDropPackage(hDictionary AS IntPtr, pucName AS STRING ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsDDDropPackage(hDictionary , pucName ) 
			ELSE
				RETURN ACEUNPUB64.AdsDDDropPackage(hDictionary , pucName ) 
			ENDIF

		PUBLIC STATIC METHOD AdsDDEnableTriggers(hDictionary AS IntPtr, pucObjectName AS STRING , pucParent AS STRING , ulOptions AS DWORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsDDEnableTriggers(hDictionary , pucObjectName , pucParent , ulOptions  ) 
			ELSE
				RETURN ACEUNPUB64.AdsDDEnableTriggers(hDictionary , pucObjectName , pucParent , ulOptions  ) 
			ENDIF

		PUBLIC STATIC METHOD AdsDDExecuteProcedure(hDictionary AS IntPtr, pucProcName AS STRING , pucInput AS STRING , pucOutput AS STRING , pulRowsAffected OUT DWORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsDDExecuteProcedure(hDictionary , pucProcName , pucInput , pucOutput , OUT pulRowsAffected ) 
			ELSE
				RETURN ACEUNPUB64.AdsDDExecuteProcedure(hDictionary , pucProcName , pucInput , pucOutput , OUT pulRowsAffected ) 
			ENDIF

		PUBLIC STATIC METHOD AdsDDGetObjectProperty(hDictionary AS IntPtr, usObjectType AS WORD , pucParent AS STRING , pucName AS STRING , usPropertyID AS WORD ,  pucProperty AS CHAR[] , pusPropertyLen REF WORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsDDGetObjectProperty(hDictionary , usObjectType , pucParent , pucName , usPropertyID ,  pucProperty  , REF pusPropertyLen ) 
			ELSE
				RETURN ACEUNPUB64.AdsDDGetObjectProperty(hDictionary , usObjectType , pucParent , pucName , usPropertyID ,  pucProperty  , REF pusPropertyLen ) 
			ENDIF

		PUBLIC STATIC METHOD AdsDDGetObjectProperty(hDictionary AS IntPtr, usObjectType AS WORD , pucParent AS STRING , pucName AS STRING , usPropertyID AS WORD ,  pvProperty AS BYTE[] , pusPropertyLen REF WORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsDDGetObjectProperty(hDictionary , usObjectType , pucParent , pucName , usPropertyID ,  pvProperty  , REF pusPropertyLen ) 
			ELSE
				RETURN ACEUNPUB64.AdsDDGetObjectProperty(hDictionary , usObjectType , pucParent , pucName , usPropertyID ,  pvProperty  , REF pusPropertyLen ) 
			ENDIF

		PUBLIC STATIC METHOD AdsDDGetObjectProperty(hDictionary AS IntPtr, usObjectType AS WORD , pucParent AS STRING , pucName AS STRING , usPropertyID AS WORD , pusProperty REF WORD , pusPropertyLen REF WORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsDDGetObjectProperty(hDictionary , usObjectType , pucParent , pucName , usPropertyID , REF pusProperty , REF pusPropertyLen ) 
			ELSE
				RETURN ACEUNPUB64.AdsDDGetObjectProperty(hDictionary , usObjectType , pucParent , pucName , usPropertyID , REF pusProperty , REF pusPropertyLen ) 
			ENDIF

		PUBLIC STATIC METHOD AdsDDOpen( pucDictionaryPath AS STRING, pucPassword AS STRING , phDictionary OUT IntPtr ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsDDOpen( pucDictionaryPath , pucPassword , OUT phDictionary ) 
			ELSE
				RETURN ACEUNPUB64.AdsDDOpen( pucDictionaryPath , pucPassword , OUT phDictionary ) 
			ENDIF

		PUBLIC STATIC METHOD AdsDDSetActiveDictionary(hConnect AS IntPtr, pucLinkName AS STRING , phDictionary OUT IntPtr ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsDDSetActiveDictionary(hConnect , pucLinkName , OUT phDictionary ) 
			ELSE
				RETURN ACEUNPUB64.AdsDDSetActiveDictionary(hConnect , pucLinkName , OUT phDictionary ) 
			ENDIF

		PUBLIC STATIC METHOD AdsDDSetObjectProperty(hDictionary AS IntPtr, usObjectType AS WORD , pucParent AS STRING , pucName AS STRING , usPropertyID AS WORD ,  pucProperty AS CHAR[] , usPropertyLen AS WORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsDDSetObjectProperty(hDictionary , usObjectType , pucParent , pucName , usPropertyID ,  pucProperty  , usPropertyLen ) 
			ELSE
				RETURN ACEUNPUB64.AdsDDSetObjectProperty(hDictionary , usObjectType , pucParent , pucName , usPropertyID ,  pucProperty  , usPropertyLen ) 
			ENDIF

		PUBLIC STATIC METHOD AdsDDSetObjectProperty(hDictionary AS IntPtr, usObjectType AS WORD , pucParent AS STRING , pucName AS STRING , usPropertyID AS WORD ,  pvProperty AS BYTE[] , usPropertyLen AS WORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsDDSetObjectProperty(hDictionary , usObjectType , pucParent , pucName , usPropertyID ,  pvProperty  , usPropertyLen ) 
			ELSE
				RETURN ACEUNPUB64.AdsDDSetObjectProperty(hDictionary , usObjectType , pucParent , pucName , usPropertyID ,  pvProperty  , usPropertyLen ) 
			ENDIF

		PUBLIC STATIC METHOD AdsDDSetObjectProperty(hDictionary AS IntPtr, usObjectType AS WORD , pucParent AS STRING , pucName AS STRING , usPropertyID AS WORD , pusProperty REF WORD , usPropertyLen AS WORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsDDSetObjectProperty(hDictionary , usObjectType , pucParent , pucName , usPropertyID , REF pusProperty , usPropertyLen ) 
			ELSE
				RETURN ACEUNPUB64.AdsDDSetObjectProperty(hDictionary , usObjectType , pucParent , pucName , usPropertyID , REF pusProperty , usPropertyLen ) 
			ENDIF

		PUBLIC STATIC METHOD AdsDDSetTriggerProperty(hDictionary AS IntPtr,  pucTriggerName AS STRING, usPropertyID AS WORD , pucProperty AS STRING , usPropertyLen AS WORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsDDSetTriggerProperty(hDictionary ,  pucTriggerName , usPropertyID , pucProperty , usPropertyLen ) 
			ELSE
				RETURN ACEUNPUB64.AdsDDSetTriggerProperty(hDictionary ,  pucTriggerName , usPropertyID , pucProperty , usPropertyLen ) 
			ENDIF

		PUBLIC STATIC METHOD AdsDDVerifyUserRights(hObject AS IntPtr , pucTableName AS STRING , pulUserRights OUT DWORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsDDVerifyUserRights(hObject  , pucTableName , OUT pulUserRights ) 
			ELSE
				RETURN ACEUNPUB64.AdsDDVerifyUserRights(hObject  , pucTableName , OUT pulUserRights ) 
			ENDIF

		PUBLIC STATIC METHOD AdsDeactivateAOF(hTable AS IntPtr )AS DWORD
			IF Is32Bits
				RETURN ACEUNPUB32.AdsDeactivateAOF(hTable  )
			ELSE
				RETURN ACEUNPUB64.AdsDeactivateAOF(hTable  )
			ENDIF

		PUBLIC STATIC METHOD AdsDeleteFile(hConnect AS IntPtr, pucFileName AS STRING ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsDeleteFile(hConnect , pucFileName ) 
			ELSE
				RETURN ACEUNPUB64.AdsDeleteFile(hConnect , pucFileName ) 
			ENDIF

		PUBLIC STATIC METHOD AdsDeleteTable(hTable AS IntPtr ) AS DWORD
			IF Is32Bits
				RETURN ACEUNPUB32.AdsDeleteTable(hTable  ) 
			ELSE
				RETURN ACEUNPUB64.AdsDeleteTable(hTable  ) 
			ENDIF

		PUBLIC STATIC METHOD AdsEcho(hConnect AS IntPtr, pucData AS STRING , usLen AS WORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsEcho(hConnect , pucData , usLen ) 
			ELSE
				RETURN ACEUNPUB64.AdsEcho(hConnect , pucData , usLen ) 
			ENDIF

		PUBLIC STATIC METHOD AdsEvalExpr(hTable AS IntPtr, pucPCode AS STRING,  pucResult AS CHAR[] , pusLen REF WORD ) AS DWORD
			IF Is32Bits
				RETURN ACEUNPUB32.AdsEvalExpr(hTable , pucPCode ,  pucResult  , REF pusLen ) 
			ELSE
				RETURN ACEUNPUB64.AdsEvalExpr(hTable , pucPCode ,  pucResult  , REF pusLen ) 
			ENDIF

		PUBLIC STATIC METHOD AdsExpressionLongToShort(hTable AS IntPtr, pucLongExpr AS STRING ,  pucShortExpr AS CHAR[] , pusBufferLen REF WORD )AS DWORD
			IF Is32Bits
				RETURN ACEUNPUB32.AdsExpressionLongToShort(hTable , pucLongExpr ,  pucShortExpr  , REF pusBufferLen )
			ELSE
				RETURN ACEUNPUB64.AdsExpressionLongToShort(hTable , pucLongExpr ,  pucShortExpr  , REF pusBufferLen )
			ENDIF

		PUBLIC STATIC METHOD AdsExpressionLongToShort90(hTable AS IntPtr, pucLongExpr AS STRING ,  pucShortExpr AS CHAR[] , pulBufferLen REF DWORD ) AS DWORD
			IF Is32Bits
				RETURN ACEUNPUB32.AdsExpressionLongToShort90(hTable , pucLongExpr ,  pucShortExpr  , REF pulBufferLen ) 
			ELSE
				RETURN ACEUNPUB64.AdsExpressionLongToShort90(hTable , pucLongExpr ,  pucShortExpr  , REF pulBufferLen ) 
			ENDIF

		PUBLIC STATIC METHOD AdsExpressionShortToLong(hTable AS IntPtr, pucShortExpr AS STRING ,  pucLongExpr AS CHAR[] , pusBufferLen REF WORD )AS DWORD
			IF Is32Bits
				RETURN ACEUNPUB32.AdsExpressionShortToLong(hTable , pucShortExpr ,  pucLongExpr  , REF pusBufferLen )
			ELSE
				RETURN ACEUNPUB64.AdsExpressionShortToLong(hTable , pucShortExpr ,  pucLongExpr  , REF pusBufferLen )
			ENDIF

		PUBLIC STATIC METHOD AdsExpressionShortToLong90(hTable AS IntPtr, pucShortExpr AS STRING ,  pucLongExpr AS CHAR[] , pulBufferLen REF DWORD ) AS DWORD
			IF Is32Bits
				RETURN ACEUNPUB32.AdsExpressionShortToLong90(hTable , pucShortExpr ,  pucLongExpr  , REF pulBufferLen ) 
			ELSE
				RETURN ACEUNPUB64.AdsExpressionShortToLong90(hTable , pucShortExpr ,  pucLongExpr  , REF pulBufferLen )
            ENDIF

		PUBLIC STATIC METHOD AdsExtractPathPart(usPart AS WORD , pucFile AS STRING , pucPart AS CHAR[] , pusPartLen REF WORD )AS DWORD
			IF Is32Bits
				RETURN ACEUNPUB32.AdsExtractPathPart(usPart , pucFile , pucPart  , REF pusPartLen )
			ELSE
				RETURN ACEUNPUB64.AdsExtractPathPart(usPart , pucFile , pucPart  , REF pusPartLen  )
			ENDIF

		PUBLIC STATIC METHOD AdsFreeExpr(hTable AS IntPtr, pucPCode AS STRING) AS DWORD
			IF Is32Bits
				RETURN ACEUNPUB32.AdsFreeExpr(hTable , pucPCode ) 
			ELSE
				RETURN ACEUNPUB64.AdsFreeExpr(hTable , pucPCode ) 
			ENDIF

		PUBLIC STATIC METHOD AdsGetBaseFieldName(hTbl AS IntPtr, usFld AS WORD ,  pucName AS CHAR[] , pusBufLen REF WORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsGetBaseFieldName(hTbl , usFld ,  pucName  , REF pusBufLen ) 
			ELSE
				RETURN ACEUNPUB64.AdsGetBaseFieldName(hTbl , usFld ,  pucName  , REF pusBufLen ) 
			ENDIF

		PUBLIC STATIC METHOD AdsGetBaseFieldNum(hCursor AS IntPtr , pucColumnName AS STRING , pusBaseFieldNum OUT WORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsGetBaseFieldNum(hCursor  , pucColumnName ,OUT  pusBaseFieldNum ) 
			ELSE
				RETURN ACEUNPUB64.AdsGetBaseFieldNum(hCursor  , pucColumnName ,OUT  pusBaseFieldNum ) 
			ENDIF

		PUBLIC STATIC METHOD AdsGetColumnPermissions(hTable AS IntPtr, usColumnNum AS WORD , pucPermissions AS STRING ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsGetColumnPermissions(hTable , usColumnNum , pucPermissions ) 
			ELSE
				RETURN ACEUNPUB64.AdsGetColumnPermissions(hTable , usColumnNum , pucPermissions ) 
			ENDIF

		PUBLIC STATIC METHOD AdsGetCursorAOF(hCursor AS IntPtr,  pucFilter AS CHAR[] , pusFilterLen REF WORD ) AS DWORD
			IF Is32Bits
				RETURN ACEUNPUB32.AdsGetCursorAOF(hCursor ,  pucFilter  , REF pusFilterLen ) 
			ELSE
				RETURN ACEUNPUB64.AdsGetCursorAOF(hCursor ,  pucFilter  , REF pusFilterLen ) 
			ENDIF

		PUBLIC STATIC METHOD AdsGetFTSScore(hIndex AS IntPtr, ulRecord AS DWORD , pucKey AS STRING , usKeyLen AS WORD , usDataType AS WORD , usSeekType AS WORD , pulScore OUT DWORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsGetFTSScore(hIndex , ulRecord , pucKey , usKeyLen , usDataType , usSeekType , OUT pulScore ) 
			ELSE
				RETURN ACEUNPUB64.AdsGetFTSScore(hIndex , ulRecord , pucKey , usKeyLen , usDataType , usSeekType , OUT pulScore ) 
			ENDIF

		PUBLIC STATIC METHOD AdsGetFieldRaw(hTbl AS IntPtr, lFieldOrdinal AS DWORD , pucBuf AS BYTE[] , pulLen REF DWORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsGetFieldRaw(hTbl , lFieldOrdinal , pucBuf  , REF pulLen ) 
			ELSE
				RETURN ACEUNPUB64.AdsGetFieldRaw(hTbl , lFieldOrdinal , pucBuf  , REF pulLen ) 
			ENDIF

		PUBLIC STATIC METHOD AdsGetFieldRaw(hTbl AS IntPtr, pucFldName AS STRING , pucBuf AS BYTE[] , pulLen REF DWORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsGetFieldRaw(hTbl , pucFldName , pucBuf  , REF pulLen ) 
			ELSE
				RETURN ACEUNPUB64.AdsGetFieldRaw(hTbl , pucFldName , pucBuf  , REF pulLen ) 
			ENDIF

		PUBLIC STATIC METHOD AdsGetIndexFlags(hIndex AS IntPtr, pulFlags OUT DWORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsGetIndexFlags(hIndex , OUT pulFlags ) 
			ELSE
				RETURN ACEUNPUB64.AdsGetIndexFlags(hIndex , OUT pulFlags ) 
			ENDIF

		PUBLIC STATIC METHOD AdsGetIndexPageSize(hIndex AS IntPtr, pulPageSize OUT DWORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsGetIndexPageSize(hIndex , OUT pulPageSize ) 
			ELSE
				RETURN ACEUNPUB64.AdsGetIndexPageSize(hIndex , OUT pulPageSize ) 
			ENDIF

		PUBLIC STATIC METHOD AdsGetNullRecord(hTbl AS IntPtr, pucBuf AS STRING , ulLen AS DWORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsGetNullRecord(hTbl , pucBuf , ulLen ) 
			ELSE
				RETURN ACEUNPUB64.AdsGetNullRecord(hTbl , pucBuf , ulLen ) 
			ENDIF

		PUBLIC STATIC METHOD AdsGetNumSegments(hTag AS IntPtr, usSegments OUT WORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsGetNumSegments(hTag , OUT usSegments ) 
			ELSE
				RETURN ACEUNPUB64.AdsGetNumSegments(hTag , OUT usSegments ) 
			ENDIF

		PUBLIC STATIC METHOD AdsGetPreparedFields(hStatement AS IntPtr,  pucBuffer AS CHAR[] , pulBufferLen REF DWORD , ulOptions AS DWORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsGetPreparedFields(hStatement ,  pucBuffer  , REF pulBufferLen , ulOptions ) 
			ELSE
				RETURN ACEUNPUB64.AdsGetPreparedFields(hStatement ,  pucBuffer  , REF pulBufferLen , ulOptions ) 
			ENDIF

		PUBLIC STATIC METHOD AdsGetROWIDPrefix(hTable AS IntPtr, pucRowIDPrefix AS STRING , usBufferLen AS WORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsGetROWIDPrefix(hTable , pucRowIDPrefix , usBufferLen ) 
			ELSE
				RETURN ACEUNPUB64.AdsGetROWIDPrefix(hTable , pucRowIDPrefix , usBufferLen ) 
			ENDIF

		PUBLIC STATIC METHOD AdsGetSQLStmtParams(pucStatement AS STRING ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsGetSQLStmtParams(pucStatement ) 
			ELSE
				RETURN ACEUNPUB64.AdsGetSQLStmtParams(pucStatement ) 
			ENDIF

		PUBLIC STATIC METHOD AdsGetSegmentFieldNumbers(hTag AS IntPtr, pusNumSegments OUT WORD , pusSegFieldNumbers AS WORD[] )AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsGetSegmentFieldNumbers(hTag , OUT pusNumSegments , pusSegFieldNumbers )
			ELSE
				RETURN ACEUNPUB64.AdsGetSegmentFieldNumbers(hTag , OUT pusNumSegments , pusSegFieldNumbers )
			ENDIF

		PUBLIC STATIC METHOD AdsGetSegmentFieldname(hTag AS IntPtr, usSegmentNum AS WORD , pucFieldname AS CHAR[], pusFldnameLen REF WORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsGetSegmentFieldname(hTag , usSegmentNum , pucFieldname , REF pusFldnameLen ) 
			ELSE
				RETURN ACEUNPUB64.AdsGetSegmentFieldname(hTag , usSegmentNum , pucFieldname , REF pusFldnameLen ) 
			ENDIF

		PUBLIC STATIC METHOD AdsGetSegmentOffset(hTag AS IntPtr, usSegmentNum AS WORD , usOffset OUT WORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsGetSegmentOffset(hTag , usSegmentNum , OUT usOffset ) 
			ELSE
				RETURN ACEUNPUB64.AdsGetSegmentOffset(hTag , usSegmentNum , OUT usOffset ) 
			ENDIF

		PUBLIC STATIC METHOD AdsGetTableWAN(hTable AS IntPtr, pusWAN OUT WORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsGetTableWAN(hTable , OUT pusWAN ) 
			ELSE
				RETURN ACEUNPUB64.AdsGetTableWAN(hTable , OUT pusWAN ) 
			ENDIF

		PUBLIC STATIC METHOD AdsGotoBOF(hObj AS IntPtr ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsGotoBOF(hObj  ) 
			ELSE
				RETURN ACEUNPUB64.AdsGotoBOF(hObj  ) 
			ENDIF

		PUBLIC STATIC METHOD AdsInternalCloseCachedTables(hConnect AS IntPtr, usOpen AS WORD ) AS DWORD
			IF Is32Bits
				RETURN ACEUNPUB32.AdsInternalCloseCachedTables(hConnect , usOpen ) 
			ELSE
				RETURN ACEUNPUB64.AdsInternalCloseCachedTables(hConnect , usOpen ) 
			ENDIF

		PUBLIC STATIC METHOD AdsIsIndexExprValid(hTbl AS IntPtr, pucExpr AS STRING , pbValid OUT WORD ) AS DWORD
			IF Is32Bits
				RETURN ACEUNPUB32.AdsIsIndexExprValid(hTbl , pucExpr , OUT pbValid ) 
			ELSE
				RETURN ACEUNPUB64.AdsIsIndexExprValid(hTbl , pucExpr , OUT pbValid ) 
			ENDIF

		PUBLIC STATIC METHOD AdsIsSegmentDescending(hTag AS IntPtr, usSegmentNum AS WORD , pbDescending OUT WORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsIsSegmentDescending(hTag , usSegmentNum , OUT pbDescending ) 
			ELSE
				RETURN ACEUNPUB64.AdsIsSegmentDescending(hTag , usSegmentNum , OUT pbDescending ) 
			ENDIF

		PUBLIC STATIC METHOD AdsLockRecordImplicitly(hTbl AS IntPtr, ulRec AS DWORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsLockRecordImplicitly(hTbl , ulRec ) 
			ELSE
				RETURN ACEUNPUB64.AdsLockRecordImplicitly(hTbl , ulRec ) 
			ENDIF

		PUBLIC STATIC METHOD AdsMemCompare90(hConnect AS IntPtr, pucStr1 AS STRING , ulStr1Len AS DWORD , pucStr2 AS STRING , ulStr2Len AS DWORD , usCharSet AS WORD , ulCollationID AS DWORD , psResult OUT SHORT ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsMemCompare90(hConnect , pucStr1 , ulStr1Len , pucStr2 , ulStr2Len , usCharSet , ulCollationID , OUT psResult ) 
			ELSE
				RETURN ACEUNPUB64.AdsMemCompare90(hConnect , pucStr1 , ulStr1Len , pucStr2 , ulStr2Len , usCharSet , ulCollationID , OUT psResult ) 
			ENDIF

		PUBLIC STATIC METHOD AdsMemICompare(hConnect AS IntPtr, pucStr1 AS STRING , ulStr1Len AS DWORD , pucStr2 AS STRING , ulStr2Len AS DWORD , usCharSet AS WORD , psResult OUT SHORT ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsMemICompare(hConnect , pucStr1 , ulStr1Len , pucStr2 , ulStr2Len , usCharSet , OUT psResult ) 
			ELSE
				RETURN ACEUNPUB64.AdsMemICompare(hConnect , pucStr1 , ulStr1Len , pucStr2 , ulStr2Len , usCharSet , OUT psResult ) 
			ENDIF

		PUBLIC STATIC METHOD AdsMemICompare90(hConnect AS IntPtr, pucStr1 AS STRING , ulStr1Len AS DWORD , pucStr2 AS STRING , ulStr2Len AS DWORD , usCharSet AS WORD , ulCollationID AS DWORD , psResult OUT SHORT ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsMemICompare90(hConnect , pucStr1 , ulStr1Len, pucStr2 , ulStr2Len , usCharSet , ulCollationID , OUT psResult ) 
			ELSE
				RETURN ACEUNPUB64.AdsMemICompare90(hConnect , pucStr1 , ulStr1Len, pucStr2 , ulStr2Len , usCharSet , ulCollationID , OUT psResult ) 
			ENDIF

		PUBLIC STATIC METHOD AdsMemLwr(hConnect AS IntPtr, pucStr AS STRING , usStrLen AS WORD , usCharSet AS WORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsMemLwr(hConnect , pucStr , usStrLen , usCharSet ) 
			ELSE
				RETURN ACEUNPUB64.AdsMemLwr(hConnect , pucStr , usStrLen , usCharSet ) 
			ENDIF

		PUBLIC STATIC METHOD AdsMemLwr90(hConnect AS IntPtr, pucStr AS STRING , usStrLen AS WORD , usCharSet AS WORD , ulCollationID AS DWORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsMemLwr90(hConnect , pucStr , usStrLen , usCharSet , ulCollationID ) 
			ELSE
				RETURN ACEUNPUB64.AdsMemLwr90(hConnect , pucStr , usStrLen , usCharSet , ulCollationID ) 
			ENDIF

		PUBLIC STATIC METHOD AdsMergeAOF(hTable AS IntPtr ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsMergeAOF(hTable  ) 
			ELSE
				RETURN ACEUNPUB64.AdsMergeAOF(hTable  ) 
			ENDIF

		PUBLIC STATIC METHOD AdsPerformRI(hTable AS IntPtr, ulRecNum AS DWORD , pucRecBuffer AS STRING ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsPerformRI(hTable , ulRecNum , pucRecBuffer ) 
			ELSE
				RETURN ACEUNPUB64.AdsPerformRI(hTable , ulRecNum , pucRecBuffer ) 
			ENDIF

		PUBLIC STATIC METHOD AdsPrepareSQLNow(hStatement AS IntPtr, pucSQL AS STRING ,  pucFieldInfo AS CHAR[] , pusFieldInfoLen REF WORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsPrepareSQLNow(hStatement , pucSQL ,  pucFieldInfo  , REF pusFieldInfoLen ) 
			ELSE
				RETURN ACEUNPUB64.AdsPrepareSQLNow(hStatement , pucSQL ,  pucFieldInfo  , REF pusFieldInfoLen ) 
			ENDIF

		PUBLIC STATIC METHOD AdsReadRecordNumbers(hObj AS IntPtr, ulRecordNum AS DWORD , ucDirection AS BYTE , pulRecords OUT DWORD , pulArrayLen REF DWORD , pusHitEOF OUT WORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsReadRecordNumbers(hObj , ulRecordNum , ucDirection , OUT pulRecords , REF pulArrayLen , OUT pusHitEOF ) 
			ELSE
				RETURN ACEUNPUB64.AdsReadRecordNumbers(hObj , ulRecordNum , ucDirection , OUT pulRecords , REF pulArrayLen , OUT pusHitEOF ) 
			ENDIF

		PUBLIC STATIC METHOD AdsReadRecords(hObj AS IntPtr, ulRecordNum AS DWORD , cDirection AS BYTE ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsReadRecords(hObj , ulRecordNum , cDirection ) 
			ELSE
				RETURN ACEUNPUB64.AdsReadRecords(hObj , ulRecordNum , cDirection ) 
			ENDIF

		PUBLIC STATIC METHOD AdsRefreshView(phCursor OUT IntPtr ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsRefreshView(OUT phCursor ) 
			ELSE
				RETURN ACEUNPUB64.AdsRefreshView(OUT phCursor ) 
			ENDIF

		PUBLIC STATIC METHOD AdsReleaseObject(hObj AS IntPtr ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsReleaseObject(hObj  ) 
			ELSE
				RETURN ACEUNPUB64.AdsReleaseObject(hObj  ) 
			ENDIF

		PUBLIC STATIC METHOD AdsRemoveSQLComments(pucStatement AS STRING ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsRemoveSQLComments(pucStatement ) 
			ELSE
				RETURN ACEUNPUB64.AdsRemoveSQLComments(pucStatement ) 
			ENDIF

		PUBLIC STATIC METHOD AdsRestoreDatabase(hConnect AS IntPtr, hOutputTable AS IntPtr , pucSourcePath AS STRING , pucSourcePassword AS STRING , pucDestPath AS STRING , pucDestPassword AS STRING , pucOptions AS STRING , pucFreeTablePasswords AS STRING , usCharType AS WORD , usLockingMode AS WORD , usCheckRights AS WORD , usTableType AS WORD , pucCollation AS STRING , ucDDConn AS BYTE ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsRestoreDatabase(hConnect , hOutputTable  , pucSourcePath , pucSourcePassword , pucDestPath , pucDestPassword , pucOptions , pucFreeTablePasswords , usCharType , usLockingMode , usCheckRights , usTableType , pucCollation , ucDDConn ) 
			ELSE
				RETURN ACEUNPUB64.AdsRestoreDatabase(hConnect , hOutputTable  , pucSourcePath , pucSourcePassword , pucDestPath , pucDestPassword , pucOptions , pucFreeTablePasswords , usCharType , usLockingMode , usCheckRights , usTableType , pucCollation , ucDDConn ) 
			ENDIF

		PUBLIC STATIC METHOD AdsSetBOFFlag(hTbl AS IntPtr, usBOF AS WORD )AS DWORD
			IF Is32Bits
				RETURN ACEUNPUB32.AdsSetBOFFlag(hTbl , usBOF )
			ELSE
				RETURN ACEUNPUB64.AdsSetBOFFlag(hTbl , usBOF )
			ENDIF

		PUBLIC STATIC METHOD AdsSetBaseTableAccess(hTbl AS IntPtr, usAccessBase AS WORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsSetBaseTableAccess(hTbl , usAccessBase ) 
			ELSE
				RETURN ACEUNPUB64.AdsSetBaseTableAccess(hTbl , usAccessBase ) 
			ENDIF

		PUBLIC STATIC METHOD AdsSetCollationSequence(pucSequence AS STRING ) AS DWORD
			IF Is32Bits
				RETURN ACEUNPUB32.AdsSetCollationSequence(pucSequence ) 
			ELSE
				RETURN ACEUNPUB64.AdsSetCollationSequence(pucSequence ) 
			ENDIF

		PUBLIC STATIC METHOD AdsSetCursorAOF(hTable AS IntPtr, pucFilter AS STRING , usResolve AS WORD ) AS DWORD
			IF Is32Bits
				RETURN ACEUNPUB32.AdsSetCursorAOF(hTable , pucFilter , usResolve ) 
			ELSE
				RETURN ACEUNPUB64.AdsSetCursorAOF(hTable , pucFilter , usResolve ) 
			ENDIF

		PUBLIC STATIC METHOD AdsSetFieldRaw(hObj AS IntPtr, pucFldName AS STRING , pucBuf AS BYTE[] , ulLen AS DWORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsSetFieldRaw(hObj , pucFldName , pucBuf  , ulLen ) 
			ELSE
				RETURN ACEUNPUB64.AdsSetFieldRaw(hObj , pucFldName , pucBuf  , ulLen ) 
			ENDIF

		PUBLIC STATIC METHOD AdsSetFlushFlag(hConnect AS IntPtr, usFlushEveryUpdate AS WORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsSetFlushFlag(hConnect , usFlushEveryUpdate ) 
			ELSE
				RETURN ACEUNPUB64.AdsSetFlushFlag(hConnect , usFlushEveryUpdate ) 
			ENDIF

		PUBLIC STATIC METHOD AdsSetInternalError(ulErrCode AS DWORD , pucFile AS STRING , ulLine AS DWORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsSetInternalError(ulErrCode , pucFile , ulLine ) 
			ELSE
				RETURN ACEUNPUB64.AdsSetInternalError(ulErrCode , pucFile , ulLine ) 
			ENDIF

		PUBLIC STATIC METHOD AdsSetPacketSize(hConnect AS IntPtr, usPacketLength AS WORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsSetPacketSize(hConnect , usPacketLength ) 
			ELSE
				RETURN ACEUNPUB64.AdsSetPacketSize(hConnect , usPacketLength ) 
			ENDIF

		PUBLIC STATIC METHOD AdsSetProperty(hObj AS IntPtr, ulOperation AS DWORD , ulValue AS DWORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsSetProperty(hObj , ulOperation , ulValue ) 
			ELSE
				RETURN ACEUNPUB64.AdsSetProperty(hObj , ulOperation , ulValue ) 
			ENDIF

		PUBLIC STATIC METHOD AdsSetRecordPartial(hObj AS IntPtr, pucRec AS STRING , ulLen AS DWORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsSetRecordPartial(hObj , pucRec , ulLen ) 
			ELSE
				RETURN ACEUNPUB64.AdsSetRecordPartial(hObj , pucRec , ulLen ) 
			ENDIF

		PUBLIC STATIC METHOD AdsSetTableCharType(hTbl AS IntPtr, usCharType AS WORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsSetTableCharType(hTbl , usCharType ) 
			ELSE
				RETURN ACEUNPUB64.AdsSetTableCharType(hTbl , usCharType ) 
			ENDIF

		PUBLIC STATIC METHOD AdsSetTimeStampRaw(hObj AS IntPtr, lFieldOrdinal AS DWORD , pucBuf REF UINT64 , ulLen AS DWORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsSetTimeStampRaw(hObj , lFieldOrdinal , REF pucBuf , ulLen ) 
			ELSE
				RETURN ACEUNPUB64.AdsSetTimeStampRaw(hObj , lFieldOrdinal , REF pucBuf , ulLen ) 
			ENDIF

		PUBLIC STATIC METHOD AdsSetTimeStampRaw(hObj AS IntPtr, pucFldName AS STRING , pucBuf REF UINT64 , ulLen AS DWORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsSetTimeStampRaw(hObj , pucFldName , REF pucBuf , ulLen ) 
			ELSE
				RETURN ACEUNPUB64.AdsSetTimeStampRaw(hObj , pucFldName , REF pucBuf , ulLen ) 
			ENDIF

		PUBLIC STATIC METHOD AdsSetupRI(hConnection AS IntPtr , lTableID AS INT , ucOpen AS BYTE , ulServerWAN AS DWORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsSetupRI(hConnection  , lTableID , ucOpen , ulServerWAN ) 
			ELSE
				RETURN ACEUNPUB64.AdsSetupRI(hConnection  , lTableID , ucOpen , ulServerWAN ) 
			ENDIF

		PUBLIC STATIC METHOD AdsStepIndexKey(hIndex AS IntPtr, pucKey AS STRING , usLen AS WORD,  sDirection AS SHORT ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsStepIndexKey(hIndex , pucKey , usLen ,  sDirection ) 
			ELSE
				RETURN ACEUNPUB64.AdsStepIndexKey(hIndex , pucKey , usLen ,  sDirection ) 
			ENDIF

		PUBLIC STATIC METHOD AdsValidateThread() AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsValidateThread() 
			ELSE
				RETURN ACEUNPUB64.AdsValidateThread() 
			ENDIF

		PUBLIC STATIC METHOD AdsVerifyRI(hConnect AS IntPtr, usExclusive AS WORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsVerifyRI(hConnect , usExclusive ) 
			ELSE
				RETURN ACEUNPUB64.AdsVerifyRI(hConnect , usExclusive ) 
			ENDIF

		PUBLIC STATIC METHOD AdsWaitForObject(hObj AS IntPtr, ulOptions AS DWORD ) AS DWORD 
			IF Is32Bits
				RETURN ACEUNPUB32.AdsWaitForObject(hObj , ulOptions ) 
			ELSE
				RETURN ACEUNPUB64.AdsWaitForObject(hObj , ulOptions ) 
			ENDIF

		PUBLIC STATIC METHOD ObsAdsDecryptBuffer(pucPassword AS STRING , pucBuffer AS STRING , usLen AS WORD ) AS DWORD
			IF Is32Bits
				RETURN ACEUNPUB32.ObsAdsDecryptBuffer(pucPassword , pucBuffer , usLen ) 
			ELSE
				RETURN ACEUNPUB64.ObsAdsDecryptBuffer(pucPassword , pucBuffer , usLen ) 
			ENDIF

		PUBLIC STATIC METHOD ObsAdsEncryptBuffer(pucPassword AS STRING , pucBuffer AS STRING , usLen AS WORD ) AS DWORD
			IF Is32Bits
				RETURN ACEUNPUB32.ObsAdsEncryptBuffer(pucPassword , pucBuffer , usLen ) 
			ELSE
				RETURN ACEUNPUB64.ObsAdsEncryptBuffer(pucPassword , pucBuffer , usLen ) 
			ENDIF

    #endregion
		
	END CLASS
END NAMESPACE
