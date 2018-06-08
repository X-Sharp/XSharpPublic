 //
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System
USING System.Runtime.InteropServices

BEGIN NAMESPACE XSharp.ADS
STATIC Class ACE
 
    #region Constants
	public const ADS_FALSE := 0 as WORD
	public const ADS_TRUE := 1 as WORD
	public const ADS_DEFAULT := 0 as WORD 
	public const ADS_ANSI := 1 as WORD
	public const ADS_OEM := 2 as WORD
	public const ADS_MAX_CHAR_SETS := 68 as WORD 
	public const ADS_CHECKRIGHTS := 1 as WORD 
	public const ADS_IGNORERIGHTS := 2 as WORD 
	public const ADS_INC_USERCOUNT := 1u as DWORD
	public const ADS_STORED_PROC_CONN := 2u as DWORD 
	public const ADS_COMPRESS_ALWAYS := 4u as DWORD 
	public const ADS_COMPRESS_NEVER := 8u as DWORD 
	public const ADS_COMPRESS_INTERNET := 12u as DWORD 
	public const ADS_REPLICATION_CONNECTION := 16u as DWORD 
	public const ADS_UDP_IP_CONNECTION := 32u as DWORD 
	public const ADS_IPX_CONNECTION := 64u as DWORD 
	public const ADS_TCP_IP_CONNECTION := 128u as DWORD 
	public const ADS_TCP_IP_V6_CONNECTION := 256u as DWORD 
	public const ADS_NOTIFICATION_CONNECTION := 512u as DWORD 
	public const ADS_EXCLUSIVE := 1u as DWORD 
	public const ADS_READONLY := 2u as DWORD 
	public const ADS_SHARED := 4u as DWORD 
	public const ADS_CLIPPER_MEMOS := 8u as DWORD 
	public const ADS_TABLE_PERM_READ := 16u as DWORD 
	public const ADS_TABLE_PERM_UPDATE := 32u as DWORD 
	public const ADS_TABLE_PERM_INSERT := 64u as DWORD 
	public const ADS_TABLE_PERM_DELETE := 128u as DWORD 
	public const ADS_REINDEX_ON_COLLATION_MISMATCH := 256u as DWORD 
	public const ADS_IGNORE_COLLATION_MISMATCH := 512u as DWORD 
	public const ADS_FREE_TABLE := 4096u as DWORD 
	public const ADS_TEMP_TABLE := 8192u as DWORD 
	public const ADS_DICTIONARY_BOUND_TABLE := 16384u as DWORD 
	public const ADS_ASCENDING := 0u as DWORD 
	public const ADS_UNIQUE := 1u as DWORD 
	public const ADS_COMPOUND := 2u as DWORD 
	public const ADS_CUSTOM := 4u as DWORD 
	public const ADS_DESCENDING := 8u as DWORD 
	public const ADS_USER_DEFINED := 16u as DWORD 
	public const ADS_NOT_AUTO_OPEN := 1024u as DWORD 
	public const ADS_CANDIDATE := 2048u as DWORD 
	public const ADS_FTS_INDEX := 32u as DWORD 
	public const ADS_FTS_FIXED := 64u as DWORD 
	public const ADS_FTS_CASE_SENSITIVE := 128u as DWORD 
	public const ADS_FTS_KEEP_SCORE := 256u as DWORD 
	public const ADS_FTS_PROTECT_NUMBERS := 512u as DWORD 
	public const ADS_NONE := 0 as WORD 
	public const ADS_LTRIM := 1 as WORD 
	public const ADS_RTRIM := 2 as WORD 
	public const ADS_TRIM := 3 as WORD 
	public const ADS_COMPATIBLE_LOCKING := 0 as WORD 
	public const ADS_PROPRIETARY_LOCKING := 1 as WORD 
	public const ADS_SOFTSEEK := 1 as WORD 
	public const ADS_HARDSEEK := 2 as WORD 
	public const ADS_SEEKGT := 4 as WORD 
	public const ADS_RAWKEY := 1 as WORD 
	public const ADS_STRINGKEY := 2 as WORD 
	public const ADS_DOUBLEKEY := 4 as WORD 
	public const ADS_TOP := 1 as WORD 
	public const ADS_BOTTOM := 2 as WORD 
	public const ADS_RESPECTFILTERS := 1 as WORD 
	public const ADS_IGNOREFILTERS := 2 as WORD 
	public const ADS_RESPECTSCOPES := 3 as WORD 
	public const ADS_REFRESHCOUNT := 4 as WORD 
	public const ADS_LOCAL_SERVER := 1 as WORD 
	public const ADS_REMOTE_SERVER := 2 as WORD 
	public const ADS_AIS_SERVER := 4 as WORD 
	public const ADS_CONNECTION := 1 as WORD 
	public const ADS_TABLE := 2 as WORD 
	public const ADS_INDEX_ORDER := 3 as WORD 
	public const ADS_STATEMENT := 4 as WORD 
	public const ADS_CURSOR := 5 as WORD 
	public const ADS_DATABASE_CONNECTION := 6 as WORD 
	public const ADS_FTS_INDEX_ORDER := 8 as WORD 
	public const ADS_CURSOR_READONLY := 1 as WORD 
	public const ADS_CURSOR_READWRITE := 2 as WORD 
	public const ADS_CONSTRAIN := 1 as WORD 
	public const ADS_NO_CONSTRAIN := 2 as WORD 
	public const ADS_READ_ALL_COLUMNS := 1 as WORD 
	public const ADS_READ_SELECT_COLUMNS := 2 as WORD 
	public const ADS_NO_OPTIMIZATION := 1 as WORD 
	public const ADS_NO_VALIDATE := 0 as WORD 
	public const ADS_VALIDATE_NO_SAVE := 1 as WORD 
	public const ADS_VALIDATE_WRITE_FAIL := 2 as WORD 
	public const ADS_VALIDATE_APPEND_FAIL := 3 as WORD 
	public const ADS_VALIDATE_RETURN_ERROR := 4 as WORD 
	public const ADS_CMP_LESS := -1 as INT
	public const ADS_CMP_EQUAL := 0 as INT
	public const ADS_CMP_GREATER := 1 as INT
	public const ADS_CONNECTIONPROP_USERNAME := 0 as WORD 
	public const ADS_CONNECTIONPROP_PASSWORD := 1 as WORD 
	public const ADS_CRC_LOCALLY := 1 as WORD 
	public const ADS_CRC_IGNOREMEMOPAGES := 2 as WORD 
	public const ADS_EVENT_ASYNC := 1 as WORD 
	public const ADS_PRESERVE_ERR := 1 as WORD 
	public const AE_SUCCESS := 0 as WORD 
	public const AE_ALLOCATION_FAILED := 5001 as WORD 
	public const AE_COMM_MISMATCH := 5002 as WORD 
	public const AE_DATA_TOO_LONG := 5003 as WORD 
	public const AE_FILE_NOT_FOUND := 5004 as WORD 
	public const AE_INSUFFICIENT_BUFFER := 5005 as WORD 
	public const AE_INVALID_BOOKMARK := 5006 as WORD 
	public const AE_INVALID_CALLBACK := 5007 as WORD 
	public const AE_INVALID_CENTURY := 5008 as WORD 
	public const AE_INVALID_DATEFORMAT := 5009 as WORD 
	public const AE_INVALID_DECIMALS := 5010 as WORD 
	public const AE_INVALID_EXPRESSION := 5011 as WORD 
	public const AE_INVALID_FIELDDEF := 5012 as WORD 
	public const AE_INVALID_FILTER_OPTION := 5013 as WORD 
	public const AE_INVALID_INDEX_HANDLE := 5014 as WORD 
	public const AE_INVALID_INDEX_NAME := 5015 as WORD 
	public const AE_INVALID_INDEX_ORDER_NAME := 5016 as WORD 
	public const AE_INVALID_INDEX_TYPE := 5017 as WORD 
	public const AE_INVALID_HANDLE := 5018 as WORD 
	public const AE_INVALID_OPTION := 5019 as WORD 
	public const AE_INVALID_PATH := 5020 as WORD 
	public const AE_INVALID_POINTER := 5021 as WORD 
	public const AE_INVALID_RECORD_NUMBER := 5022 as WORD 
	public const AE_INVALID_TABLE_HANDLE := 5023 as WORD 
	public const AE_INVALID_CONNECTION_HANDLE := 5024 as WORD 
	public const AE_INVALID_TABLETYPE := 5025 as WORD 
	public const AE_INVALID_WORKAREA := 5026 as WORD 
	public const AE_INVALID_CHARSETTYPE := 5027 as WORD 
	public const AE_INVALID_LOCKTYPE := 5028 as WORD 
	public const AE_INVALID_RIGHTSOPTION := 5029 as WORD 
	public const AE_INVALID_FIELDNUMBER := 5030 as WORD 
	public const AE_INVALID_KEY_LENGTH := 5031 as WORD 
	public const AE_INVALID_FIELDNAME := 5032 as WORD 
	public const AE_NO_DRIVE_CONNECTION := 5033 as WORD 
	public const AE_FILE_NOT_ON_SERVER := 5034 as WORD 
	public const AE_LOCK_FAILED := 5035 as WORD 
	public const AE_NO_CONNECTION := 5036 as WORD 
	public const AE_NO_FILTER := 5037 as WORD 
	public const AE_NO_SCOPE := 5038 as WORD 
	public const AE_NO_TABLE := 5039 as WORD 
	public const AE_NO_WORKAREA := 5040 as WORD 
	public const AE_NOT_FOUND := 5041 as WORD 
	public const AE_NOT_IMPLEMENTED := 5042 as WORD 
	public const AE_MAX_THREADS_EXCEEDED := 5043 as WORD 
	public const AE_START_THREAD_FAIL := 5044 as WORD 
	public const AE_TOO_MANY_INDEXES := 5045 as WORD 
	public const AE_TOO_MANY_TAGS := 5046 as WORD 
	public const AE_TRANS_OUT_OF_SEQUENCE := 5047 as WORD 
	public const AE_UNKNOWN_ERRCODE := 5048 as WORD 
	public const AE_UNSUPPORTED_LANGUAGE := 5049 as WORD 
	public const AE_NAME_TOO_LONG := 5050 as WORD 
	public const AE_DUPLICATE_ALIAS := 5051 as WORD 
	public const AE_TABLE_CLOSED_IN_TRANSACTION := 5053 as WORD 
	public const AE_PERMISSION_DENIED := 5054 as WORD 
	public const AE_STRING_NOT_FOUND := 5055 as WORD 
	public const AE_UNKNOWN_CHAR_SET := 5056 as WORD 
	public const AE_INVALID_OEM_CHAR_FILE := 5057 as WORD 
	public const AE_INVALID_MEMO_BLOCK_SIZE := 5058 as WORD 
	public const AE_NO_FILE_FOUND := 5059 as WORD 
	public const AE_NO_INF_LOCK := 5060 as WORD 
	public const AE_INF_FILE_ERROR := 5061 as WORD 
	public const AE_RECORD_NOT_LOCKED := 5062 as WORD 
	public const AE_ILLEGAL_COMMAND_DURING_TRANS := 5063 as WORD 
	public const AE_TABLE_NOT_SHARED := 5064 as WORD 
	public const AE_INDEX_ALREADY_OPEN := 5065 as WORD 
	public const AE_INVALID_FIELD_TYPE := 5066 as WORD 
	public const AE_TABLE_NOT_EXCLUSIVE := 5067 as WORD 
	public const AE_NO_CURRENT_RECORD := 5068 as WORD 
	public const AE_PRECISION_LOST := 5069 as WORD 
	public const AE_INVALID_DATA_TYPE := 5070 as WORD 
	public const AE_DATA_TRUNCATED := 5071 as WORD 
	public const AE_TABLE_READONLY := 5072 as WORD 
	public const AE_INVALID_RECORD_LENGTH := 5073 as WORD 
	public const AE_NO_ERROR_MESSAGE := 5074 as WORD 
	public const AE_INDEX_SHARED := 5075 as WORD 
	public const AE_INDEX_EXISTS := 5076 as WORD 
	public const AE_CYCLIC_RELATION := 5077 as WORD 
	public const AE_INVALID_RELATION := 5078 as WORD 
	public const AE_INVALID_DAY := 5079 as WORD 
	public const AE_INVALID_MONTH := 5080 as WORD 
	public const AE_CORRUPT_TABLE := 5081 as WORD 
	public const AE_INVALID_BINARY_OFFSET := 5082 as WORD 
	public const AE_BINARY_FILE_ERROR := 5083 as WORD 
	public const AE_INVALID_DELETED_BYTE_VALUE := 5084 as WORD 
	public const AE_NO_PENDING_UPDATE := 5085 as WORD 
	public const AE_PENDING_UPDATE := 5086 as WORD 
	public const AE_TABLE_NOT_LOCKED := 5087 as WORD 
	public const AE_CORRUPT_INDEX := 5088 as WORD 
	public const AE_AUTOOPEN_INDEX := 5089 as WORD 
	public const AE_SAME_TABLE := 5090 as WORD 
	public const AE_INVALID_IMAGE := 5091 as WORD 
	public const AE_COLLATION_SEQUENCE_MISMATCH := 5092 as WORD 
	public const AE_INVALID_INDEX_ORDER := 5093 as WORD 
	public const AE_TABLE_CACHED := 5094 as WORD 
	public const AE_INVALID_DATE := 5095 as WORD 
	public const AE_ENCRYPTION_NOT_ENABLED := 5096 as WORD 
	public const AE_INVALID_PASSWORD := 5097 as WORD 
	public const AE_TABLE_ENCRYPTED := 5098 as WORD 
	public const AE_SERVER_MISMATCH := 5099 as WORD 
	public const AE_INVALID_USERNAME := 5100 as WORD 
	public const AE_INVALID_VALUE := 5101 as WORD 
	public const AE_INVALID_CONTINUE := 5102 as WORD 
	public const AE_UNRECOGNIZED_VERSION := 5103 as WORD 
	public const AE_RECORD_ENCRYPTED := 5104 as WORD 
	public const AE_UNRECOGNIZED_ENCRYPTION := 5105 as WORD 
	public const AE_INVALID_SQLSTATEMENT_HANDLE := 5106 as WORD 
	public const AE_INVALID_SQLCURSOR_HANDLE := 5107 as WORD 
	public const AE_NOT_PREPARED := 5108 as WORD 
	public const AE_CURSOR_NOT_CLOSED := 5109 as WORD 
	public const AE_INVALID_SQL_PARAM_NUMBER := 5110 as WORD 
	public const AE_INVALID_SQL_PARAM_NAME := 5111 as WORD 
	public const AE_INVALID_COLUMN_NUMBER := 5112 as WORD 
	public const AE_INVALID_COLUMN_NAME := 5113 as WORD 
	public const AE_INVALID_READONLY_OPTION := 5114 as WORD
	public const AE_IS_CURSOR_HANDLE := 5115 as WORD
	public const AE_INDEX_EXPR_NOT_FOUND := 5116 as WORD
	public const AE_NOT_DML := 5117 as WORD
	public const AE_INVALID_CONSTRAIN_TYPE := 5118 as WORD
	public const AE_INVALID_CURSORHANDLE := 5119 as WORD
	public const AE_OBSOLETE_FUNCTION := 5120 as WORD
	public const AE_TADSDATASET_GENERAL := 5121 as WORD
	public const AE_UDF_OVERWROTE_BUFFER := 5122 as WORD
	public const AE_INDEX_UDF_NOT_SET := 5123 as WORD
	public const AE_CONCURRENT_PROBLEM := 5124 as WORD
	public const AE_INVALID_DICTIONARY_HANDLE := 5125 as WORD
	public const AE_INVALID_PROPERTY_ID := 5126 as WORD
	public const AE_INVALID_PROPERTY := 5127 as WORD
	public const AE_DICTIONARY_ALREADY_EXISTS := 5128 as WORD
	public const AE_INVALID_FIND_HANDLE := 5129 as WORD
	public const AE_DD_REQUEST_NOT_COMPLETED := 5130 as WORD
	public const AE_INVALID_OBJECT_ID := 5131 as WORD
	public const AE_INVALID_OBJECT_NAME := 5132 as WORD
	public const AE_INVALID_PROPERTY_LENGTH := 5133 as WORD
	public const AE_INVALID_KEY_OPTIONS := 5134 as WORD
	public const AE_CONSTRAINT_VALIDATION_ERROR := 5135 as WORD
	public const AE_INVALID_OBJECT_TYPE := 5136 as WORD
	public const AE_NO_OBJECT_FOUND := 5137 as WORD
	public const AE_PROPERTY_NOT_SET := 5138 as WORD
	public const AE_NO_PRIMARY_KEY_EXISTS := 5139 as WORD
	public const AE_LOCAL_CONN_DISABLED := 5140 as WORD
	public const AE_RI_RESTRICT := 5141 as WORD
	public const AE_RI_CASCADE := 5142 as WORD
	public const AE_RI_FAILED := 5143 as WORD
	public const AE_RI_CORRUPTED := 5144 as WORD
	public const AE_RI_UNDO_FAILED := 5145 as WORD
	public const AE_RI_RULE_EXISTS := 5146 as WORD
	public const AE_COLUMN_CANNOT_BE_NULL := 5147 as WORD
	public const AE_MIN_CONSTRAINT_VIOLATION := 5148 as WORD
	public const AE_MAX_CONSTRAINT_VIOLATION := 5149 as WORD
	public const AE_RECORD_CONSTRAINT_VIOLATION := 5150 as WORD
	public const AE_CANNOT_DELETE_TEMP_INDEX := 5151 as WORD
	public const AE_RESTRUCTURE_FAILED := 5152 as WORD
	public const AE_INVALID_STATEMENT := 5153 as WORD
	public const AE_STORED_PROCEDURE_FAILED := 5154 as WORD
	public const AE_INVALID_DICTIONARY_FILE := 5155 as WORD
	public const AE_NOT_MEMBER_OF_GROUP := 5156 as WORD
	public const AE_ALREADY_MEMBER_OF_GROUP := 5157 as WORD
	public const AE_INVALID_OBJECT_RIGHT := 5158 as WORD
	public const AE_INVALID_OBJECT_PERMISSION := 5158 as WORD
	public const AE_CANNOT_OPEN_DATABASE_TABLE := 5159 as WORD
	public const AE_INVALID_CONSTRAINT := 5160 as WORD
	public const AE_NOT_ADMINISTRATOR := 5161 as WORD
	public const AE_NO_TABLE_ENCRYPTION_PASSWORD := 5162 as WORD
	public const AE_TABLE_NOT_ENCRYPTED := 5163 as WORD
	public const AE_INVALID_ENCRYPTION_VERSION := 5164 as WORD
	public const AE_NO_STORED_PROC_EXEC_RIGHTS := 5165 as WORD
	public const AE_DD_UNSUPPORTED_DEPLOYMENT := 5166 as WORD
	public const AE_INFO_AUTO_CREATION_OCCURRED := 5168 as WORD
	public const AE_INFO_COPY_MADE_BY_CLIENT := 5169 as WORD
	public const AE_DATABASE_REQUIRES_NEW_SERVER := 5170 as WORD
	public const AE_COLUMN_PERMISSION_DENIED := 5171 as WORD
	public const AE_DATABASE_REQUIRES_NEW_CLIENT := 5172 as WORD
	public const AE_INVALID_LINK_NUMBER := 5173 as WORD
	public const AE_LINK_ACTIVATION_FAILED := 5174 as WORD
	public const AE_INDEX_COLLATION_MISMATCH := 5175 as WORD
	public const AE_ILLEGAL_USER_OPERATION := 5176 as WORD
	public const AE_TRIGGER_FAILED := 5177 as WORD
	public const AE_NO_ASA_FUNCTION_FOUND := 5178 as WORD
	public const AE_VALUE_OVERFLOW := 5179 as WORD
	public const AE_UNRECOGNIZED_FTS_VERSION := 5180 as WORD
	public const AE_TRIG_CREATION_FAILED := 5181 as WORD
	public const AE_MEMTABLE_SIZE_EXCEEDED := 5182 as WORD
	public const AE_OUTDATED_CLIENT_VERSION := 5183 as WORD
	public const AE_FREE_TABLE := 5184 as WORD
	public const AE_LOCAL_CONN_RESTRICTED := 5185 as WORD
	public const AE_OLD_RECORD := 5186 as WORD
	public const AE_QUERY_NOT_ACTIVE := 5187 as WORD
	public const AE_KEY_EXCEEDS_PAGE_SIZE := 5188 as WORD
	public const AE_TABLE_FOUND := 5189 as WORD
	public const AE_TABLE_NOT_FOUND := 5190 as WORD
	public const AE_LOCK_OBJECT := 5191 as WORD
	public const AE_INVALID_REPLICATION_IDENT := 5192 as WORD
	public const AE_ILLEGAL_COMMAND_DURING_BACKUP := 5193 as WORD
	public const AE_NO_MEMO_FILE := 5194 as WORD
	public const AE_SUBSCRIPTION_QUEUE_NOT_EMPTY := 5195 as WORD
	public const AE_UNABLE_TO_DISABLE_TRIGGERS := 5196 as WORD
	public const AE_UNABLE_TO_ENABLE_TRIGGERS := 5197 as WORD
	public const AE_BACKUP := 5198 as WORD
	public const AE_FREETABLEFAILED := 5199 as WORD
	public const AE_BLURRY_SNAPSHOT := 5200 as WORD
	public const AE_INVALID_VERTICAL_FILTER := 5201 as WORD
	public const AE_INVALID_USE_OF_HANDLE_IN_AEP := 5202 as WORD
	public const AE_COLLATION_NOT_RECOGNIZED := 5203 as WORD
	public const AE_INVALID_COLLATION := 5204 as WORD
	public const AE_NOT_VFP_NULLABLE_FIELD := 5205 as WORD
	public const AE_NOT_VFP_VARIABLE_FIELD := 5206 as WORD
	public const AE_ILLEGAL_EVENT_COMMAND := 5207 as WORD
	public const AE_KEY_CANNOT_BE_NULL := 5208 as WORD
	public const ADS_DATABASE_TABLE := 0 as WORD
	public const ADS_NTX := 1 as WORD
	public const ADS_CDX := 2 as WORD
	public const ADS_ADT := 3 as WORD
	public const ADS_VFP := 4 as WORD
	public const ADS_BASENAME := 1 as WORD
	public const ADS_BASENAMEANDEXT := 2 as WORD
	public const ADS_FULLPATHNAME := 3 as WORD
	public const ADS_DATADICTIONARY_NAME := 4 as WORD
	public const ADS_TABLE_OPEN_NAME := 5 as WORD
	public const ADS_OPTIMIZED_FULL := 1 as WORD
	public const ADS_OPTIMIZED_PART := 2 as WORD
	public const ADS_OPTIMIZED_NONE := 3 as WORD
	public const ADS_DYNAMIC_AOF := 0u as DWORD 
	public const ADS_RESOLVE_IMMEDIATE := 1u as DWORD 
	public const ADS_RESOLVE_DYNAMIC := 2u as DWORD 
	public const ADS_KEYSET_AOF := 4u as DWORD 
	public const ADS_FIXED_AOF := 8u as DWORD 
	public const ADS_AOF_ADD_RECORD := 1 as WORD
	public const ADS_AOF_REMOVE_RECORD := 2 as WORD
	public const ADS_AOF_TOGGLE_RECORD := 3 as WORD
	public const ADS_STORED_PROC := 1u  as DWORD 
	public const ADS_COMSTORED_PROC := 2u as DWORD 
	public const ADS_SCRIPT_PROC := 4u as DWORD 
	public const ADS_MAX_DATEMASK := 12 as WORD
	public const ADS_MAX_ERROR_LEN := 600 as WORD
	public const ADS_MAX_INDEX_EXPR_LEN := 510 as WORD
	public const ADS_MAX_KEY_LENGTH := 4082 as WORD
	public const ADS_MAX_FIELD_NAME := 128 as WORD
	public const ADS_MAX_DBF_FIELD_NAME := 10 as WORD
	public const ADS_MAX_INDEXES := 15 as WORD
	public const ADS_MAX_PATH := 260 as WORD
	public const ADS_MAX_TABLE_NAME := 255 as WORD
	public const ADS_MAX_TAG_NAME := 128 as WORD
	public const ADS_MAX_TAGS := 50 as WORD
	public const ADS_MAX_OBJECT_NAME := 200 as WORD
	public const ADS_MAX_TABLE_AND_PATH := 515 as WORD
	public const ADS_MIN_ADI_PAGESIZE := 512 as WORD
	public const ADS_MAX_ADI_PAGESIZE := 8192 as WORD
	public const ADS_TYPE_UNKNOWN := 0 as WORD
	public const ADS_LOGICAL := 1 as WORD
	public const ADS_NUMERIC := 2 as WORD
	public const ADS_DATE := 3 as WORD
	public const ADS_STRING := 4 as WORD
	public const ADS_MEMO := 5 as WORD
	public const ADS_BINARY := 6 as WORD
	public const ADS_IMAGE := 7 as WORD
	public const ADS_VARCHAR := 8 as WORD
	public const ADS_COMPACTDATE := 9 as WORD
	public const ADS_DOUBLE := 10 as WORD
	public const ADS_INTEGER := 11 as WORD
	public const ADS_SHORTINT := 12 as WORD
	public const ADS_TIME := 13 as WORD
	public const ADS_TIMESTAMP := 14 as WORD
	public const ADS_AUTOINC := 15 as WORD
	public const ADS_RAW := 16 as WORD
	public const ADS_CURDOUBLE := 17 as WORD
	public const ADS_MONEY := 18 as WORD
	public const ADS_INT64INT64 := 19 as WORD
	public const ADS_CISTRING := 20 as WORD
	public const ADS_ROWVERSION := 21 as WORD
	public const ADS_MODTIME := 22 as WORD
	public const ADS_VARCHAR_FOX := 23 as WORD
	public const ADS_VARBINARY_FOX := 24 as WORD
	public const ADS_SYSTEM_FIELD := 25 as WORD
	public const ADS_INDEX_UDF := 1 as WORD
	public const ADS_MAX_CFG_PATH := 256 as WORD
	public const ADS_MGMT_NETWARE_SERVER := 1 as WORD
	public const ADS_MGMT_NETWARE4_OR_OLDER_SERVER := 1 as WORD
	public const ADS_MGMT_NT_SERVER := 2 as WORD
	public const ADS_MGMT_LOCAL_SERVER := 3 as WORD
	public const ADS_MGMT_WIN9X_SERVER := 4 as WORD
	public const ADS_MGMT_NETWARE5_OR_NEWER_SERVER := 5 as WORD
	public const ADS_MGMT_LINUX_SERVER := 6 as WORD
	public const ADS_MGMT_NT_SERVER_64_BIT := 7 as WORD
	public const ADS_MGMT_LINUX_SERVER_64_BIT := 8 as WORD
	public const ADS_MGMT_NO_LOCK := 1 as WORD
	public const ADS_MGMT_RECORD_LOCK := 2 as WORD
	public const ADS_MGMT_FILE_LOCK := 3 as WORD
	public const ADS_REG_OWNER_LEN := 36 as WORD
	public const ADS_REVISION_LEN := 16 as WORD
	public const ADS_INST_DATE_LEN := 16 as WORD
	public const ADS_OEM_CHAR_NAME_LEN := 16 as WORD
	public const ADS_ANSI_CHAR_NAME_LEN := 16 as WORD
	public const ADS_SERIAL_NUM_LEN := 16 as WORD
	public const ADS_MGMT_MAX_PATH := 260 as WORD
	public const ADS_MGMT_PROPRIETARY_LOCKING := 1 as WORD
	public const ADS_MGMT_CDX_LOCKING := 2 as WORD
	public const ADS_MGMT_NTX_LOCKING := 3 as WORD
	public const ADS_MGMT_ADT_LOCKING := 4 as WORD
	public const ADS_MGMT_COMIX_LOCKING := 5 as WORD
	public const ADS_MAX_USER_NAME := 50 as WORD
	public const ADS_MAX_ADDRESS_SIZE := 30 as WORD
	public const ADS_DD_PROPERTY_NOT_AVAIL := 65535 as WORD
	public const ADS_DD_MAX_PROPERTY_LEN := 65534 as WORD
	public const ADS_DD_MAX_OBJECT_NAME_LEN := 200 as WORD
	public const ADS_DD_UNKNOWN_OBJECT := 0 as WORD
	public const ADS_DD_TABLE_OBJECT := 1 as WORD
	public const ADS_DD_RELATION_OBJECT := 2 as WORD
	public const ADS_DD_INDEX_FILE_OBJECT := 3 as WORD
	public const ADS_DD_FIELD_OBJECT := 4 as WORD
	public const ADS_DD_COLUMN_OBJECT := 4 as WORD
	public const ADS_DD_INDEX_OBJECT := 5 as WORD
	public const ADS_DD_VIEW_OBJECT := 6 as WORD
	public const ADS_DD_VIEW_OR_TABLE_OBJECT := 7 as WORD
	public const ADS_DD_USER_OBJECT := 8 as WORD
	public const ADS_DD_USER_GROUP_OBJECT := 9 as WORD
	public const ADS_DD_PROCEDURE_OBJECT := 10 as WORD
	public const ADS_DD_DATABASE_OBJECT := 11 as WORD
	public const ADS_DD_LINK_OBJECT := 12 as WORD
	public const ADS_DD_TABLE_VIEW_OR_LINK_OBJECT := 13 as WORD
	public const ADS_DD_TRIGGER_OBJECT := 14 as WORD
	public const ADS_DD_PUBLICATION_OBJECT := 15 as WORD
	public const ADS_DD_ARTICLE_OBJECT := 16 as WORD
	public const ADS_DD_SUBSCRIPTION_OBJECT := 17 as WORD
	public const ADS_DD_FUNCTION_OBJECT := 18 as WORD
	public const ADS_DD_PACKAGE_OBJECT := 19 as WORD
	public const ADS_DD_QUALIFIED_TRIGGER_OBJ := 20 as WORD
	public const ADS_DD_COMMENT := 1 as WORD
	public const ADS_DD_VERSION := 2 as WORD
	public const ADS_DD_USER_DEFINED_PROP := 3 as WORD
	public const ADS_DD_OBJECT_NAME := 4 as WORD
	public const ADS_DD_TRIGGERS_DISABLED := 5 as WORD
	public const ADS_DD_OBJECT_ID := 6 as WORD
	public const ADS_DD_OPTIONS := 7 as WORD
	public const ADS_DD_DEFAULT_TABLE_PATH := 100 as WORD
	public const ADS_DD_ADMIN_PASSWORD := 101 as WORD
	public const ADS_DD_TEMP_TABLE_PATH := 102 as WORD
	public const ADS_DD_LOG_IN_REQUIRED := 103 as WORD
	public const ADS_DD_VERIFY_ACCESS_RIGHTS := 104 as WORD
	public const ADS_DD_ENCRYPT_TABLE_PASSWORD := 105 as WORD
	public const ADS_DD_ENCRYPT_NEW_TABLE := 106 as WORD
	public const ADS_DD_ENABLE_INTERNET := 107 as WORD
	public const ADS_DD_INTERNET_SECURITY_LEVEL := 108 as WORD
	public const ADS_DD_MAX_FAILED_ATTEMPTS := 109 as WORD
	public const ADS_DD_ALLOW_ADSSYS_NET_ACCESS := 110 as WORD
	public const ADS_DD_VERSION_MAJOR := 111 as WORD
	public const ADS_DD_VERSION_MINOR := 112 as WORD
	public const ADS_DD_LOGINS_DISABLED := 113 as WORD
	public const ADS_DD_LOGINS_DISABLED_ERRSTR := 114 as WORD
	public const ADS_DD_FTS_DELIMITERS := 115 as WORD
	public const ADS_DD_FTS_NOISE := 116 as WORD
	public const ADS_DD_FTS_DROP_CHARS := 117 as WORD
	public const ADS_DD_FTS_CONDITIONAL_CHARS := 118 as WORD
	public const ADS_DD_ENCRYPTED := 119 as WORD
	public const ADS_DD_ENCRYPT_INDEXES := 120 as WORD
	public const ADS_DD_QUERY_LOG_TABLE := 121 as WORD
	public const ADS_DD_ENCRYPT_COMMUNICATION := 122 as WORD
	public const ADS_DD_DEFAULT_TABLE_RELATIVE_PATH := 123 as WORD
	public const ADS_DD_TEMP_TABLE_RELATIVE_PATH := 124 as WORD
	public const ADS_DD_DISABLE_DLL_CACHING := 125 as WORD
	public const ADS_DD_TABLE_VALIDATION_EXPR := 200 as WORD
	public const ADS_DD_TABLE_VALIDATION_MSG := 201 as WORD
	public const ADS_DD_TABLE_PRIMARY_KEY := 202 as WORD
	public const ADS_DD_TABLE_AUTO_CREATE := 203 as WORD
	public const ADS_DD_TABLE_TYPE := 204 as WORD
	public const ADS_DD_TABLE_PATH := 205 as WORD
	public const ADS_DD_TABLE_FIELD_COUNT := 206 as WORD
	public const ADS_DD_TABLE_RI_GRAPH := 207 as WORD
	public const ADS_DD_TABLE_OBJ_ID := 208 as WORD
	public const ADS_DD_TABLE_RI_XY := 209 as WORD
	public const ADS_DD_TABLE_IS_RI_PARENT := 210 as WORD
	public const ADS_DD_TABLE_RELATIVE_PATH := 211 as WORD
	public const ADS_DD_TABLE_CHAR_TYPE := 212 as WORD
	public const ADS_DD_TABLE_DEFAULT_INDEX := 213 as WORD
	public const ADS_DD_TABLE_ENCRYPTION := 214 as WORD
	public const ADS_DD_TABLE_MEMO_BLOCK_SIZE := 215 as WORD
	public const ADS_DD_TABLE_PERMISSION_LEVEL := 216 as WORD
	public const ADS_DD_TABLE_TRIGGER_TYPES := 217 as WORD
	public const ADS_DD_TABLE_TRIGGER_OPTIONS := 218 as WORD
	public const ADS_DD_FIELD_OPT_VFP_BINARY := 1u as DWORD 
	public const ADS_DD_FIELD_OPT_VFP_NULLABLE := 2u as DWORD 
	public const ADS_DD_FIELD_DEFAULT_VALUE := 300 as WORD
	public const ADS_DD_FIELD_CAN_NULL := 301 as WORD
	public const ADS_DD_FIELD_MIN_VALUE := 302 as WORD
	public const ADS_DD_FIELD_MAX_VALUE := 303 as WORD
	public const ADS_DD_FIELD_VALIDATION_MSG := 304 as WORD
	public const ADS_DD_FIELD_DEFINITION := 305 as WORD
	public const ADS_DD_FIELD_TYPE := 306 as WORD
	public const ADS_DD_FIELD_LENGTH := 307 as WORD
	public const ADS_DD_FIELD_DECIMAL := 308 as WORD
	public const ADS_DD_FIELD_NUM := 309 as WORD
	public const ADS_DD_FIELD_OPTIONS := 310 as WORD
	public const ADS_DD_INDEX_FILE_NAME := 400 as WORD
	public const ADS_DD_INDEX_EXPRESSION := 401 as WORD
	public const ADS_DD_INDEX_CONDITION := 402 as WORD
	public const ADS_DD_INDEX_OPTIONS := 403 as WORD
	public const ADS_DD_INDEX_KEY_LENGTH := 404 as WORD
	public const ADS_DD_INDEX_KEY_TYPE := 405 as WORD
	public const ADS_DD_INDEX_FTS_MIN_LENGTH := 406 as WORD
	public const ADS_DD_INDEX_FTS_DELIMITERS := 407 as WORD
	public const ADS_DD_INDEX_FTS_NOISE := 408 as WORD
	public const ADS_DD_INDEX_FTS_DROP_CHARS := 409 as WORD
	public const ADS_DD_INDEX_FTS_CONDITIONAL_CHARS := 410 as WORD
	public const ADS_DD_INDEX_COLLATION := 411 as WORD
	public const ADS_DD_RI_PARENT_GRAPH := 500 as WORD
	public const ADS_DD_RI_PRIMARY_TABLE := 501 as WORD
	public const ADS_DD_RI_PRIMARY_INDEX := 502 as WORD
	public const ADS_DD_RI_FOREIGN_TABLE := 503 as WORD
	public const ADS_DD_RI_FOREIGN_INDEX := 504 as WORD
	public const ADS_DD_RI_UPDATERULE := 505 as WORD
	public const ADS_DD_RI_DELETERULE := 506 as WORD
	public const ADS_DD_RI_NO_PKEY_ERROR := 507 as WORD
	public const ADS_DD_RI_CASCADE_ERROR := 508 as WORD
	public const ADS_DD_USER_GROUP_NAME := 600 as WORD
	public const ADS_DD_VIEW_STMT := 700 as WORD
	public const ADS_DD_VIEW_STMT_LEN := 701 as WORD
	public const ADS_DD_VIEW_TRIGGER_TYPES := 702 as WORD
	public const ADS_DD_VIEW_TRIGGER_OPTIONS := 703 as WORD
	public const ADS_DD_PROC_INPUT := 800 as WORD
	public const ADS_DD_PROC_OUTPUT := 801 as WORD
	public const ADS_DD_PROC_DLL_NAME := 802 as WORD
	public const ADS_DD_PROC_DLL_FUNCTION_NAME := 803 as WORD
	public const ADS_DD_PROC_INVOKE_OPTION := 804 as WORD
	public const ADS_DD_PROC_SCRIPT := 805 as WORD
	public const ADS_DD_INDEX_FILE_PATH := 900 as WORD
	public const ADS_DD_INDEX_FILE_PAGESIZE := 901 as WORD
	public const ADS_DD_INDEX_FILE_RELATIVE_PATH := 902 as WORD
	public const ADS_DD_INDEX_FILE_TYPE := 903 as WORD
	public const ADS_DD_TABLES_RIGHTS := 1001 as WORD
	public const ADS_DD_VIEWS_RIGHTS := 1002 as WORD
	public const ADS_DD_PROCS_RIGHTS := 1003 as WORD
	public const ADS_DD_OBJECTS_RIGHTS := 1004 as WORD
	public const ADS_DD_FREE_TABLES_RIGHTS := 1005 as WORD
	public const ADS_DD_USER_PASSWORD := 1101 as WORD
	public const ADS_DD_USER_GROUP_MEMBERSHIP := 1102 as WORD
	public const ADS_DD_USER_BAD_LOGINS := 1103 as WORD
	public const ADS_DD_LINK_PATH := 1300 as WORD
	public const ADS_DD_LINK_OPTIONS := 1301 as WORD
	public const ADS_DD_LINK_USERNAME := 1302 as WORD
	public const ADS_DD_LINK_RELATIVE_PATH := 1303 as WORD
	public const ADS_DD_TRIG_TABLEID := 1400 as WORD
	public const ADS_DD_TRIG_EVENT_TYPE := 1401 as WORD
	public const ADS_DD_TRIG_TRIGGER_TYPE := 1402 as WORD
	public const ADS_DD_TRIG_CONTAINER_TYPE := 1403 as WORD
	public const ADS_DD_TRIG_CONTAINER := 1404 as WORD
	public const ADS_DD_TRIG_FUNCTION_NAME := 1405 as WORD
	public const ADS_DD_TRIG_PRIORITY := 1406 as WORD
	public const ADS_DD_TRIG_OPTIONS := 1407 as WORD
	public const ADS_DD_TRIG_TABLENAME := 1408 as WORD
	public const ADS_DD_PUBLICATION_OPTIONS := 1500 as WORD
	public const ADS_DD_ARTICLE_FILTER := 1600 as WORD
	public const ADS_DD_ARTICLE_ID_COLUMNS := 1601 as WORD
	public const ADS_DD_ARTICLE_ID_COLUMN_NUMBERS := 1602 as WORD
	public const ADS_DD_ARTICLE_FILTER_SHORT := 1603 as WORD
	public const ADS_DD_ARTICLE_INCLUDE_COLUMNS := 1604 as WORD
	public const ADS_DD_ARTICLE_EXCLUDE_COLUMNS := 1605 as WORD
	public const ADS_DD_ARTICLE_INC_COLUMN_NUMBERS := 1606 as WORD
	public const ADS_DD_ARTICLE_INSERT_MERGE := 1607 as WORD
	public const ADS_DD_ARTICLE_UPDATE_MERGE := 1608 as WORD
	public const ADS_DD_SUBSCR_PUBLICATION_NAME := 1700 as WORD
	public const ADS_DD_SUBSCR_TARGET := 1701 as WORD
	public const ADS_DD_SUBSCR_USERNAME := 1702 as WORD
	public const ADS_DD_SUBSCR_PASSWORD := 1703 as WORD
	public const ADS_DD_SUBSCR_FORWARD := 1704 as WORD
	public const ADS_DD_SUBSCR_ENABLED := 1705 as WORD
	public const ADS_DD_SUBSCR_QUEUE_NAME := 1706 as WORD
	public const ADS_DD_SUBSCR_OPTIONS := 1707 as WORD
	public const ADS_DD_SUBSCR_QUEUE_NAME_RELATIVE := 1708 as WORD
	public const ADS_DD_SUBSCR_PAUSED := 1709 as WORD
	public const ADS_DD_LEVEL_0 := 0 as WORD
	public const ADS_DD_LEVEL_1 := 1 as WORD
	public const ADS_DD_LEVEL_2 := 2 as WORD
	public const ADS_DD_RI_CASCADE := 1 as WORD
	public const ADS_DD_RI_RESTRICT := 2 as WORD
	public const ADS_DD_RI_SETNULL := 3 as WORD
	public const ADS_DD_RI_SETDEFAULT := 4 as WORD
	public const ADS_DD_DFV_UNKNOWN := 1 as WORD
	public const ADS_DD_DFV_NONE := 2 as WORD
	public const ADS_DD_DFV_VALUES_STORED := 3 as WORD
	public const ADS_PERMISSION_NONE := 0u as DWORD 
	public const ADS_PERMISSION_READ := 1u as DWORD 
	public const ADS_PERMISSION_UPDATE := 2u as DWORD 
	public const ADS_PERMISSION_EXECUTE := 4u as DWORD 
	public const ADS_PERMISSION_INHERIT := 8u as DWORD 
	public const ADS_PERMISSION_INSERT := 16u as DWORD 
	public const ADS_PERMISSION_DELETE := 32u as DWORD 
	public const ADS_PERMISSION_LINK_ACCESS := 64u as DWORD 
	public const ADS_PERMISSION_CREATE := 128u as DWORD 
	public const ADS_PERMISSION_ALTER := 256u as DWORD 
	public const ADS_PERMISSION_DROP := 512u as DWORD 
	public const ADS_PERMISSION_WITH_GRANT := 2147483648u as DWORD 
	public const ADS_PERMISSION_ALL_WITH_GRANT := 2415919103u as DWORD 
	public const ADS_PERMISSION_ALL := 4294967295u as DWORD 
	public const ADS_GET_PERMISSIONS_WITH_GRANT := 2147549183u as DWORD 
	public const ADS_GET_PERMISSIONS_CREATE := 4294901888u as DWORD 
	public const ADS_GET_PERMISSIONS_CREATE_WITH_GRANT := 2415918991u as DWORD 
	public const ADS_LINK_GLOBAL := 1u as DWORD 
	public const ADS_LINK_AUTH_ACTIVE_USER := 2u as DWORD 
	public const ADS_LINK_PATH_IS_STATIC := 4u as DWORD 
	public const ADS_TRIGEVENT_INSERT := 1 as WORD
	public const ADS_TRIGEVENT_UPDATE := 2 as WORD
	public const ADS_TRIGEVENT_DELETE := 3 as WORD
	public const ADS_TRIGTYPE_BEFORE := 1u as DWORD 
	public const ADS_TRIGTYPE_INSTEADOF := 2u as DWORD 
	public const ADS_TRIGTYPE_AFTER := 4u as DWORD 
	public const ADS_TRIGTYPE_CONFLICTON := 8u as DWORD 
	public const ADS_TRIG_WIN32DLL := 1 as WORD
	public const ADS_TRIG_COM := 2 as WORD
	public const ADS_TRIG_SCRIPT := 3 as WORD
	public const ADS_TRIGOPTIONS_NO_VALUES := 0u as DWORD 
	public const ADS_TRIGOPTIONS_WANT_VALUES := 1u as DWORD 
	public const ADS_TRIGOPTIONS_WANT_MEMOS_AND_BLOBS := 2u as DWORD 
	public const ADS_TRIGOPTIONS_DEFAULT := 3u as DWORD 
	public const ADS_TRIGOPTIONS_NO_TRANSACTION := 4u as DWORD 
	public const ADS_DD_TABLE_PERMISSION_LEVEL_1 := 1 as WORD
	public const ADS_DD_TABLE_PERMISSION_LEVEL_2 := 2 as WORD
	public const ADS_DD_TABLE_PERMISSION_LEVEL_3 := 3 as WORD
	public const ADS_KEEP_TABLE_FILE_NAME := 1u as DWORD 
	public const ADS_IDENTIFY_BY_PRIMARY := 1u as DWORD 
	public const ADS_IDENTIFY_BY_ALL := 2u as DWORD 
	public const ADS_SUBSCR_QUEUE_IS_STATIC := 1u as DWORD 
	public const ADS_SUBSCR_AIS_TARGET := 2u as DWORD 
	public const ADS_SUBSCR_IGNORE_FAILED_REP := 4u as DWORD 
	public const ADS_SUBSCR_LOG_FAILED_REP_DATA := 8u as DWORD 


    public const AX_BEGIN_TRANSACTION  := 1 as int
    public const AX_COMMIT_TRANSACTION  := 2 as int
    public const AX_ISACTIVE_TRANSACTION  := 4 as int
    public const AX_ROLLBACK_TRANSACTION  := 3 as int


    #endregion Constants

    #region Constructor
        STATIC PRIVATE Is32Bits as LOGIC
        STATIC CONSTRUCTOR  
            Is32Bits := IntPtr.Size == 4
        RETURN
    #endregion
    #region Method Dispatch Table

    PUBLIC STATIC METHOD AdsAddCustomKey(hIndex as IntPtr ) as DWORD 
        IF Is32Bits
            RETURN ACE32.AdsAddCustomKey(hIndex)
        ELSE
            RETURN ACE64.AdsAddCustomKey(hIndex)
        ENDIF


    PUBLIC STATIC METHOD AdsAppendRecord(hTable as IntPtr ) as DWORD 
        IF Is32Bits
            RETURN ACE32.AdsAppendRecord(hTable)
        ELSE
            RETURN ACE64.AdsAppendRecord(hTable)
        ENDIF


    PUBLIC STATIC METHOD AdsApplicationExit() as DWORD 
            IF Is32Bits
                RETURN ACE32.AdsApplicationExit()
            ELSE
                RETURN ACE64.AdsApplicationExit()
            ENDIF


    PUBLIC STATIC METHOD AdsAtBOF(hTable as IntPtr , pbBof out WORD ) as DWORD 
            IF Is32Bits
                RETURN ACE32.AdsAtBOF(hTable, out pbBOF)
            ELSE
                RETURN ACE64.AdsAtBOF(hTable, out pbBOF)
            ENDIF


    PUBLIC STATIC METHOD AdsAtEOF(hTable as IntPtr , pbEof out WORD ) as DWORD 
            IF Is32Bits
                RETURN ACE32.AdsAtEOF(hTable, out pbEof)
            ELSE
                RETURN ACE64.AdsAtEOF(hTable, out pbEof)
            ENDIF


    PUBLIC STATIC METHOD AdsBeginTransaction(hConnect as IntPtr) as DWORD 
            IF Is32Bits
                RETURN ACE32.AdsBeginTransaction(hConnect)
            ELSE
                RETURN ACE64.AdsBeginTransaction(hConnect)
            ENDIF


    PUBLIC STATIC METHOD AdsBinaryToFile(hTable as IntPtr , strFldName as string , strFileName as string ) as DWORD 
            IF Is32Bits
                RETURN ACE32.AdsBinaryToFile(hTable, strFldName, strFileName)
            ELSE
                RETURN ACE64.AdsBinaryToFile(hTable, strFldName, strFileName)
            ENDIF

    PUBLIC STATIC METHOD AdsBinaryToFile(hTable as IntPtr , lFieldOrdinal as DWORD, strFileName as string ) as DWORD 
            IF Is32Bits
                RETURN ACE32.AdsBinaryToFile(hTable, lFieldOrdinal, strFileName)
            ELSE
                RETURN ACE64.AdsBinaryToFile(hTable, lFieldOrdinal, strFileName)
            ENDIF

    PUBLIC STATIC METHOD AdsCacheOpenCursors(usOpen as WORD) as DWORD 
            IF Is32Bits
                RETURN ACE32.AdsCacheOpenCursors(usOpen)
            ELSE
                RETURN ACE64.AdsCacheOpenCursors(usOpen)
            ENDIF


    PUBLIC STATIC METHOD AdsCacheOpenTables(usOpen as WORD) as DWORD 
            IF Is32Bits
                RETURN ACE32.AdsCacheOpenTables(usOpen)
            ELSE
                RETURN ACE64.AdsCacheOpenTables(usOpen)
            ENDIF


    PUBLIC STATIC METHOD AdsCacheRecords(hTable as IntPtr , usNumRecords as WORD ) as DWORD 
            IF Is32Bits
                RETURN ACE32.AdsCacheRecords(hTable, usNumRecords)
            ELSE
                RETURN ACE64.AdsCacheRecords(hTable, usNumRecords)
            ENDIF


    PUBLIC STATIC METHOD AdsCancelUpdate(hTable as IntPtr ) as DWORD 
            IF Is32Bits
                RETURN ACE32.AdsCancelUpdate(hTable)
            ELSE
                RETURN ACE64.AdsCancelUpdate(hTable)
            ENDIF



PUBLIC STATIC METHOD AdsClearAllScopes(hTable as IntPtr ) as DWORD 
            IF Is32Bits
                RETURN ACE32.AdsClearAllScopes(hTable)
            ELSE
                RETURN ACE64.AdsClearAllScopes(hTable)
            ENDIF

	
	    PUBLIC STATIC METHOD AdsClearDefault() as DWORD 
            IF Is32Bits
                RETURN ACE32.AdsClearDefault()
            ELSE
                RETURN ACE64.AdsClearDefault()
            ENDIF

	
	PUBLIC STATIC METHOD AdsClearFilter(hTable as IntPtr ) as DWORD 
            IF Is32Bits
                RETURN ACE32.AdsClearFilter(hTable)
            ELSE
                RETURN ACE64.AdsClearFilter(hTable)
            ENDIF


	
	PUBLIC STATIC METHOD AdsClearRelation(hTableParent as IntPtr ) as DWORD 
            IF Is32Bits
                RETURN ACE32.AdsClearFilter(hTableParent)
            ELSE
                RETURN ACE64.AdsClearFilter(hTableParent)
            ENDIF

	
	PUBLIC STATIC METHOD AdsClearScope(hIndex as IntPtr , usScopeOption as WORD) as DWORD 
         IF Is32Bits
                RETURN ACE32.AdsClearScope(hIndex, usScopeOption)
            ELSE
                RETURN ACE64.AdsClearScope(hIndex, usScopeOption)
            ENDIF

	
	PUBLIC STATIC METHOD AdsCloneTable(hTable as IntPtr , phClone out IntPtr ) as DWORD 
            IF Is32Bits
                RETURN ACE32.AdsCloneTable(hTable, out phClone)
            ELSE
                RETURN ACE64.AdsCloneTable(hTable, out phClone)
            ENDIF

	
	PUBLIC STATIC METHOD AdsCloseAllIndexes(hTable as IntPtr ) as DWORD 
           IF Is32Bits
                RETURN ACE32.AdsCloseAllIndexes(hTable)
            ELSE
                RETURN ACE64.AdsCloseAllIndexes(hTable)
            ENDIF

	
	PUBLIC STATIC METHOD AdsCloseAllTables() as DWORD 
          IF Is32Bits
                RETURN ACE32.AdsCloseAllTables()
            ELSE
                RETURN ACE64.AdsCloseAllTables()
            ENDIF

	
	PUBLIC STATIC METHOD AdsCloseIndex(hIndex as IntPtr ) as DWORD 
         IF Is32Bits
                RETURN ACE32.AdsCloseIndex(hIndex)
            ELSE
                RETURN ACE64.AdsCloseIndex(hIndex)
            ENDIF


	
	PUBLIC STATIC METHOD AdsCloseTable(hTable as IntPtr ) as DWORD 
            IF Is32Bits
                RETURN ACE32.AdsCloseTable(hTable)
            ELSE
                RETURN ACE64.AdsCloseTable(hTable)
            ENDIF


	
	PUBLIC STATIC METHOD AdsCloseCachedTables(hConnection as IntPtr) as DWORD 
            IF Is32Bits
                RETURN ACE32.AdsCloseCachedTables(hConnection)
            ELSE
                RETURN ACE64.AdsCloseCachedTables(hConnection)
            ENDIF

	
	PUBLIC STATIC METHOD AdsCommitTransaction(hConnect as IntPtr) as DWORD 
            IF Is32Bits
                RETURN ACE32.AdsCommitTransaction(hConnect)
            ELSE
                RETURN ACE64.AdsCommitTransaction(hConnect)
            ENDIF

	

	
	PUBLIC STATIC METHOD AdsContinue(hTable as IntPtr , pbFound out WORD) as DWORD 
           IF Is32Bits
                RETURN ACE32.AdsContinue(hTable, out pbFound)
            ELSE
                RETURN ACE64.AdsContinue(hTable, out pbFound)
            ENDIF

	

	
	PUBLIC STATIC METHOD AdsCopyTableStructure(hTable as IntPtr , strFile as string ) as DWORD 
           IF Is32Bits
                RETURN ACE32.AdsCopyTableStructure(hTable, strFile)
            ELSE
                RETURN ACE64.AdsCopyTableStructure(hTable, strFile)
            ENDIF

	
	PUBLIC STATIC METHOD AdsCreateFTSIndex(hTable as IntPtr , strFileName as string , strTag as string , strField as string , ulPageSize as DWORD , ulMinWordLen as DWORD , ulMaxWordLen as DWORD , usUseDefaultDelim as WORD , strDelimiters as string , usUseDefaultNoise as WORD , strNoiseWords as string , usUseDefaultDrop as WORD , strDropChars as string , usUseDefaultConditionals as WORD , strConditionalChars as string , strReserved1 as string , strReserved2 as string , ulOptions as DWORD) as DWORD 
           IF Is32Bits
                RETURN ACE32.AdsCreateFTSIndex(hTable, strFileName, strTag, strField, ulPageSize , ulMinWordLen , ulMaxWordLen , usUseDefaultDelim , strDelimiters , usUseDefaultNoise , strNoiseWords , usUseDefaultDrop , strDropChars , usUseDefaultConditionals , strConditionalChars , strReserved1 , strReserved2 , ulOptions )
            ELSE
                RETURN ACE64.AdsCreateFTSIndex(hTable, strFileName, strTag, strField, ulPageSize , ulMinWordLen , ulMaxWordLen , usUseDefaultDelim , strDelimiters , usUseDefaultNoise , strNoiseWords , usUseDefaultDrop , strDropChars , usUseDefaultConditionals , strConditionalChars , strReserved1 , strReserved2 , ulOptions )
            ENDIF

	    

	
	PUBLIC STATIC METHOD AdsDecryptRecord(hTable as IntPtr ) as DWORD 
               IF Is32Bits
                RETURN ACE32.AdsDecryptRecord(hTable)
            ELSE
                RETURN ACE64.AdsDecryptRecord(hTable)
            ENDIF

	

	
	PUBLIC STATIC METHOD AdsDecryptTable(hTable as IntPtr ) as DWORD 
           IF Is32Bits
                RETURN ACE32.AdsDecryptTable(hTable)
            ELSE
                RETURN ACE64.AdsDecryptTable(hTable)
            ENDIF

	
	PUBLIC STATIC METHOD AdsDeleteCustomKey(hIndex as IntPtr ) as DWORD 
         IF Is32Bits
                RETURN ACE32.AdsDeleteCustomKey(hIndex)
            ELSE
                RETURN ACE64.AdsDeleteCustomKey(hIndex)
            ENDIF

	
	PUBLIC STATIC METHOD AdsDeleteIndex(hIndex as IntPtr ) as DWORD 
             IF Is32Bits
                RETURN ACE32.AdsDeleteIndex(hIndex)
            ELSE
                RETURN ACE64.AdsDeleteIndex(hIndex)
            ENDIF

	
	PUBLIC STATIC METHOD AdsDeleteRecord(hTable as IntPtr ) as DWORD 
            IF Is32Bits
                RETURN ACE32.AdsDeleteRecord(hTable)
            ELSE
                RETURN ACE64.AdsDeleteRecord(hTable)
            ENDIF

	

	
	PUBLIC STATIC METHOD AdsDisableEncryption(hTable as IntPtr ) as DWORD 
              IF Is32Bits
                RETURN ACE32.AdsDisableEncryption(hTable)
            ELSE
                RETURN ACE64.AdsDisableEncryption(hTable)
            ENDIF

	
	PUBLIC STATIC METHOD AdsDisableLocalConnections() as DWORD 
          IF Is32Bits
                RETURN ACE32.AdsDisableLocalConnections()
            ELSE
                RETURN ACE64.AdsDisableLocalConnections()
            ENDIF

	
	PUBLIC STATIC METHOD AdsDisconnect(hConnect as IntPtr) as DWORD 
           IF Is32Bits
                RETURN ACE32.AdsDisconnect(hConnect)
            ELSE
                RETURN ACE64.AdsDisconnect(hConnect)
            ENDIF

	
	PUBLIC STATIC METHOD AdsEnableEncryption(hTable as IntPtr , strPassword as string ) as DWORD 
             IF Is32Bits
                RETURN ACE32.AdsEnableEncryption(hTable, strPassword)
            ELSE
                RETURN ACE64.AdsEnableEncryption(hTable,strPassword)
            ENDIF

	
	PUBLIC STATIC METHOD AdsEncryptRecord(hTable as IntPtr ) as DWORD 
             IF Is32Bits
                RETURN ACE32.AdsEncryptRecord(hTable)
            ELSE
                RETURN ACE64.AdsEncryptRecord(hTable)
            ENDIF


	
	PUBLIC STATIC METHOD AdsEncryptTable(hTable as IntPtr ) as DWORD 
             IF Is32Bits
                RETURN ACE32.AdsEncryptTable(hTable)
            ELSE
                RETURN ACE64.AdsEncryptTable(hTable)
            ENDIF

	
	PUBLIC STATIC METHOD AdsEvalLogicalExpr(hTable as IntPtr , strExpr as string , pbResult out WORD ) as DWORD 
            IF Is32Bits
                RETURN ACE32.AdsEvalLogicalExpr(hTable, strExpr, out pbResult)
            ELSE
                RETURN ACE64.AdsEvalLogicalExpr(hTable, strExpr, out pbResult)
            ENDIF

	
	PUBLIC STATIC METHOD AdsEvalNumericExpr(hTable as IntPtr , strExpr as string , pdResult out double ) as DWORD 
            IF Is32Bits
                RETURN ACE32.AdsEvalNumericExpr(hTable, strExpr, out pdResult)
            ELSE
                RETURN ACE64.AdsEvalNumericExpr(hTable, strExpr, out pdResult)
            ENDIF

	
	PUBLIC STATIC METHOD AdsEvalStringExpr(hTable as IntPtr , strExpr as string , [In] [Out] strResult as char[] , wLen ref WORD ) as DWORD 
            IF Is32Bits
                RETURN ACE32.AdsEvalStringExpr(hTable, strExpr, strResult, ref wLen)
            ELSE
                RETURN ACE64.AdsEvalStringExpr(hTable, strExpr, strResult, ref wLen)
            ENDIF

	
	PUBLIC STATIC METHOD AdsEvalTestExpr(hTable as IntPtr , strExpr as string , pusType out WORD ) as DWORD 
            IF Is32Bits
                RETURN ACE32.AdsEvalTestExpr(hTable, strExpr, out pusType)
            ELSE
                RETURN ACE64.AdsEvalTestExpr(hTable, strExpr, out pusType)
            ENDIF

	
	
	PUBLIC STATIC METHOD AdsFileToBinary(hTable as IntPtr , strFldName as string , usBinaryType as WORD , strFileName as string ) as DWORD 
            IF Is32Bits
                RETURN ACE32.AdsFileToBinary(hTable, strFldName, usBinaryType, strFileName)
            ELSE
                RETURN ACE64.AdsFileToBinary(hTable, strFldName, usBinaryType, strFileName)
            ENDIF

	
	PUBLIC STATIC METHOD AdsFileToBinary(hTable as IntPtr , lFieldOrdinal as DWORD, usBinaryType as WORD , strFileName as string ) as DWORD 
            IF Is32Bits
                RETURN ACE32.AdsFileToBinary(hTable, lFieldOrdinal, usBinaryType, strFileName)
            ELSE
                RETURN ACE64.AdsFileToBinary(hTable, lFieldOrdinal, usBinaryType, strFileName)
            ENDIF

    PUBLIC STATIC METHOD AdsFindConnection(strServerName as string , phConnect out IntPtr ) as DWORD 
            IF Is32Bits
                RETURN ACE32.AdsFindConnection(strServerName, out phConnect)
            ELSE
                RETURN ACE64.AdsFindConnection(strServerName, out phConnect)
            ENDIF

	
	PUBLIC STATIC METHOD AdsGetAllIndexes(hTable as IntPtr , [In] [Out] ahIndex as IntPtr[] , pusArrayLen ref WORD ) as DWORD 
        IF Is32Bits
                RETURN ACE32.AdsGetAllIndexes(hTable, ahIndex, ref pusArrayLen)
            ELSE
                RETURN ACE64.AdsGetAllIndexes(hTable, ahIndex, ref pusArrayLen)
            ENDIF

	
	PUBLIC STATIC METHOD AdsGetFTSIndexes(hTable as IntPtr , [In] [Out] ahIndex as IntPtr[] , pusArrayLen ref WORD ) as DWORD 
        IF Is32Bits
                RETURN ACE32.AdsGetFTSIndexes(hTable, ahIndex, ref pusArrayLen)
            ELSE
                RETURN ACE64.AdsGetFTSIndexes(hTable, ahIndex, ref pusArrayLen)
            ENDIF

	
	PUBLIC STATIC METHOD AdsGetAllLocks(hTable as IntPtr , [In] [Out] aulLocks as DWORD[] , pusArrayLen ref WORD ) as DWORD 
        IF Is32Bits
                RETURN ACE32.AdsGetAllLocks(hTable, aulLocks, ref pusArrayLen)
            ELSE
                RETURN ACE64.AdsGetAllLocks(hTable, aulLocks, ref pusArrayLen)
            ENDIF

	
	PUBLIC STATIC METHOD AdsGetAllTables([In] [Out] ahTable as IntPtr[] , pusArrayLen ref WORD ) as DWORD 
      IF Is32Bits
                RETURN ACE32.AdsGetAllTables(ahTable, ref pusArrayLen)
            ELSE
                RETURN ACE64.AdsGetAllTables(ahTable, ref pusArrayLen)
            ENDIF


	
	PUBLIC STATIC METHOD AdsGetBinary(hTable as IntPtr , strFldName as string , ulOffset as DWORD , [In] [Out] strBuf as byte[] , pulLen ref DWORD ) as DWORD 
        IF Is32Bits
            RETURN ACE32.AdsGetBinary(hTable, strFldName, ulOffSet, strBuf, ref pulLen)
        ELSE
            RETURN ACE64.AdsGetBinary(hTable, strFldName, ulOffSet, strBuf, ref pulLen)
        ENDIF

	
	PUBLIC STATIC METHOD AdsGetBinary(hTable as IntPtr , lFieldOrdinal as DWORD, ulOffset as DWORD , [In] [Out] strBuf as byte[] , pulLen ref DWORD ) as DWORD 
        IF Is32Bits
            RETURN ACE32.AdsGetBinary(hTable, lFieldOrdinal, ulOffSet, strBuf, ref pulLen)
        ELSE
            RETURN ACE64.AdsGetBinary(hTable, lFieldOrdinal, ulOffSet, strBuf, ref pulLen)
        ENDIF


	
	PUBLIC STATIC METHOD AdsGetBinaryLength(hTable as IntPtr , strFldName as string , pulLength out DWORD ) as DWORD 
        IF Is32Bits
            RETURN ACE32.AdsGetBinaryLength(hTable, strFldName, out pulLength)
        ELSE
            RETURN ACE64.AdsGetBinaryLength(hTable, strFldName, out pulLength)
        ENDIF

	
	PUBLIC STATIC METHOD AdsGetBinaryLength(hTable as IntPtr , lFieldOrdinal as DWORD, pulLength out DWORD ) as DWORD 
        IF Is32Bits
            RETURN ACE32.AdsGetBinaryLength(hTable, lFieldOrdinal, out pulLength)
        ELSE
            RETURN ACE64.AdsGetBinaryLength(hTable, lFieldOrdinal, out pulLength)
        ENDIF

	
	PUBLIC STATIC METHOD AdsGetBookmark(hTable as IntPtr , phBookmark out IntPtr ) as DWORD 
        IF Is32Bits
            RETURN ACE32.AdsGetBookmark(hTable, out phBookmark)
        ELSE
            RETURN ACE64.AdsGetBookmark(hTable, out phBookmark)
        ENDIF

	PUBLIC STATIC METHOD AdsGetConnectionType(hConnect as IntPtr, pusConnectType out WORD ) as DWORD 
        IF Is32Bits
            RETURN ACE32.AdsGetConnectionType(hConnect, out pusConnectType)
        ELSE
            RETURN ACE64.AdsGetConnectionType(hConnect, out pusConnectType)
        ENDIF
	
	PUBLIC STATIC METHOD AdsGetDate(hTable as IntPtr , strFldName as string , [In] [Out] strBuf as char[] , wLen ref WORD ) as DWORD 
        IF Is32Bits
            RETURN ACE32.AdsGetDate(hTable, strFldName, strBuf, ref wLen)
        ELSE
            RETURN ACE64.AdsGetDate(hTable, strFldName, strBuf, ref wLen)
        ENDIF

	
	PUBLIC STATIC METHOD AdsGetDate(hTable as IntPtr , lFieldOrdinal as DWORD, [In] [Out] strBuf as char[] , wLen ref WORD ) as DWORD 
        IF Is32Bits
            RETURN ACE32.AdsGetDate(hTable, lFieldOrdinal, strBuf, ref wLen)
        ELSE
            RETURN ACE64.AdsGetDate(hTable, lFieldOrdinal, strBuf, ref wLen)
        ENDIF

	PUBLIC STATIC METHOD AdsGetDateFormat([In] [Out] strFormat as char[] , wLen ref WORD ) as DWORD 
        IF Is32Bits
            RETURN ACE32.AdsGetDateFormat(strFormat, ref wLen)
        ELSE
            RETURN ACE64.AdsGetDateFormat(strFormat, ref wLen)
        ENDIF

	
	PUBLIC STATIC METHOD AdsGetDouble(hTable as IntPtr , strFldName as string , pdValue out Real8 ) as DWORD 
        IF Is32Bits
            RETURN ACE32.AdsGetDouble(hTable, strFldName, out pdValue)
        ELSE                                              
            RETURN ACE64.AdsGetDouble(hTable, strFldName, out pdValue)
        ENDIF

	
	PUBLIC STATIC METHOD AdsGetDouble(hTable as IntPtr , lFieldOrdinal as DWORD, pdValue out Real8 ) as DWORD 
        IF Is32Bits
            RETURN ACE32.AdsGetDouble(hTable, lFieldOrdinal, out pdValue)
        ELSE                                              
            RETURN ACE64.AdsGetDouble(hTable, lFieldOrdinal, out pdValue)
        ENDIF

	PUBLIC STATIC METHOD AdsGetFieldLength(hTable as IntPtr , strFldName as string , pulLength out DWORD ) as DWORD 
        IF Is32Bits
            RETURN ACE32.AdsGetFieldLength(hTable, strFldName, out pulLength)
        ELSE                                              
            RETURN ACE64.AdsGetFieldLength(hTable, strFldName, out pulLength)
        ENDIF

	
	PUBLIC STATIC METHOD AdsGetFieldLength(hTable as IntPtr , lFieldOrdinal as DWORD, pulLength out DWORD ) as DWORD 
        IF Is32Bits
            RETURN ACE32.AdsGetFieldLength(hTable, lFieldOrdinal, out pulLength)
        ELSE                                              
            RETURN ACE64.AdsGetFieldLength(hTable, lFieldOrdinal, out pulLength)
        ENDIF


	PUBLIC STATIC METHOD AdsGetFieldType(hTable as IntPtr , strFldName as string , pusType out WORD ) as DWORD 
        IF Is32Bits
            RETURN ACE32.AdsGetFieldType(hTable, strFldName, out pusType)
        ELSE                                              
            RETURN ACE64.AdsGetFieldType(hTable, strFldName, out pusType)
        ENDIF

	
	PUBLIC STATIC METHOD AdsGetFieldType(hTable as IntPtr , lFieldOrdinal as DWORD, pusType out WORD ) as DWORD 
        IF Is32Bits
            RETURN ACE32.AdsGetFieldType(hTable, lFieldOrdinal, out pusType)
        ELSE                                              
            RETURN ACE64.AdsGetFieldType(hTable, lFieldOrdinal, out pusType)
        ENDIF

	PUBLIC STATIC METHOD AdsGetHandleType(hObj as IntPtr , pusType out WORD ) as DWORD 
        IF Is32Bits
            RETURN ACE32.AdsGetHandleType(hObj, out pusType)
        ELSE                                     
            RETURN ACE64.AdsGetHandleType(hObj, out pusType)
        ENDIF

	PUBLIC STATIC METHOD AdsGetIndexHandle(hTable as IntPtr , strIndexOrder as string , phIndex out IntPtr ) as DWORD 
        IF Is32Bits
            RETURN ACE32.AdsGetIndexHandle(hTable, strIndexOrder, out phIndex)
        ELSE                                     
            RETURN ACE64.AdsGetIndexHandle(hTable, strIndexOrder, out phIndex)
        ENDIF

	PUBLIC STATIC METHOD AdsGetLastError(pulErrCode out DWORD , [In] [Out] strBuf as char[] , pusBufLen ref WORD ) as DWORD 
            IF Is32Bits
                RETURN ACE32.AdsGetLastError(out pulErrCode, strBuf, ref pusBufLen)
            ELSE
                RETURN ACE64.AdsGetLastError(out pulErrCode, strBuf, ref pusBufLen)
            ENDIF

	PUBLIC STATIC METHOD AdsGetMemoLength(hTable as IntPtr , strFldName as string , pulLength out DWORD ) as DWORD 
        IF Is32Bits
            RETURN ACE32.AdsGetMemoLength(hTable, strFldName, out pulLength)
        ELSE                                              
            RETURN ACE64.AdsGetMemoLength(hTable, strFldName, out pulLength)
        ENDIF

	
	PUBLIC STATIC METHOD AdsGetMemoLength(hTable as IntPtr , lFieldOrdinal as DWORD, pulLength out DWORD ) as DWORD 
        IF Is32Bits
            RETURN ACE32.AdsGetMemoLength(hTable, lFieldOrdinal, out pulLength)
        ELSE                                              
            RETURN ACE64.AdsGetMemoLength(hTable, lFieldOrdinal, out pulLength)
        ENDIF

	PUBLIC STATIC METHOD AdsGetMemoBlockSize(hTable as IntPtr , pusBlockSize out WORD ) as DWORD 
        IF Is32Bits
            RETURN ACE32.AdsGetMemoBlockSize(hTable, out pusBlockSize)
        ELSE                                      
            RETURN ACE64.AdsGetMemoBlockSize(hTable, out pusBlockSize)
        ENDIF

	PUBLIC STATIC METHOD AdsGetNumLocks(hTable as IntPtr , pusNum out WORD ) as DWORD 
        IF Is32Bits
            RETURN ACE32.AdsGetNumLocks(hTable, out pusNum)
        ELSE                                      
            RETURN ACE64.AdsGetNumLocks(hTable, out pusNum)
        ENDIF

	PUBLIC STATIC METHOD AdsGetRecordCount(hTable as IntPtr , usFilterOption as WORD, pulCount out DWORD) as DWORD 
        IF Is32Bits
            RETURN ACE32.AdsGetRecordCount(hTable, usFilterOption, out pulCount)
        ELSE                                      
            RETURN ACE64.AdsGetRecordCount(hTable, usFilterOption, out pulCount)
        ENDIF
	PUBLIC STATIC METHOD AdsGetRecordNum(hTable as IntPtr , usFilterOption as WORD, pulRec out DWORD ) as DWORD 
        IF Is32Bits
            RETURN ACE32.AdsGetRecordNum(hTable, usFilterOption , out pulRec)
        ELSE                                      
            RETURN ACE64.AdsGetRecordNum(hTable, usFilterOption , out pulRec)
        ENDIF
	PUBLIC STATIC METHOD AdsGetRecordCRC(hTable as IntPtr , pulCRC out DWORD , ulOptions as DWORD) as DWORD 
        IF Is32Bits
            RETURN ACE32.AdsGetRecordCRC(hTable, out pulCRC, ulOptions)
        ELSE                                      
            RETURN ACE64.AdsGetRecordCRC(hTable, out pulCRC, ulOptions)
        ENDIF
	PUBLIC STATIC METHOD AdsGetScope(hIndex as IntPtr , usScopeOption as WORD, [In] [Out] strScope as char[] , pusBufLen ref WORD ) as DWORD 
        IF Is32Bits
            RETURN ACE32.AdsGetScope(hIndex, usScopeOption, strScope, ref pusBufLen)
        ELSE                                      
            RETURN ACE64.AdsGetScope(hIndex, usScopeOption, strScope, ref pusBufLen)
        ENDIF

	PUBLIC STATIC METHOD AdsGetTableOpenOptions(hTable as IntPtr , pulOptions out DWORD ) as DWORD 
        IF Is32Bits
            RETURN ACE32.AdsGetTableOpenOptions(hTable, out pulOptions)
        ELSE                                      
            RETURN ACE64.AdsGetTableOpenOptions(hTable, out pulOptions)
        ENDIF

	PUBLIC STATIC METHOD AdsGetLastTableUpdate(hTable as IntPtr , [In] [Out] strDate as char[] , pusDateLen ref WORD ) as DWORD 
        IF Is32Bits
            RETURN ACE32.AdsGetLastTableUpdate(hTable, strDate, ref pusDateLen)
        ELSE                                      
            RETURN ACE64.AdsGetLastTableUpdate(hTable, strDate, ref pusDateLen)
        ENDIF

	PUBLIC STATIC METHOD AdsGotoBottom(hObj as IntPtr ) as DWORD 
       IF Is32Bits
            RETURN ACE32.AdsGotoBottom(hObj)
        ELSE                               
            RETURN ACE64.AdsGotoBottom(hObj)
        ENDIF

	PUBLIC STATIC METHOD AdsGotoRecord(hTable as IntPtr , ulRec as DWORD ) as DWORD 
       IF Is32Bits
            RETURN ACE32.AdsGotoRecord(hTable, ulRec)
        ELSE                               
            RETURN ACE64.AdsGotoRecord(hTable, ulRec)
        ENDIF

	PUBLIC STATIC METHOD AdsGotoTop(hObj as IntPtr ) as DWORD 
       IF Is32Bits
            RETURN ACE32.AdsGotoTop(hObj)
        ELSE                               
            RETURN ACE64.AdsGotoTop(hObj)
        ENDIF

	PUBLIC STATIC METHOD AdsInTransaction(hConnect as IntPtr, pbInTrans out WORD ) as DWORD 
       IF Is32Bits
            RETURN ACE32.AdsInTransaction(hConnect, out pbInTrans)
        ELSE                               
            RETURN ACE64.AdsInTransaction(hConnect, out pbInTrans)
        ENDIF

	PUBLIC STATIC METHOD AdsIsFound(hObj as IntPtr , pbFound out WORD) as DWORD 
       IF Is32Bits
            RETURN ACE32.AdsIsFound(hObj, out pbFound)
        ELSE                               
            RETURN ACE64.AdsIsFound(hObj, out pbFound)
        ENDIF

 	PUBLIC STATIC METHOD AdsIsTableLocked(hTable as IntPtr , pbLocked out WORD ) as DWORD 
       IF Is32Bits
            RETURN ACE32.AdsIsTableLocked(hTable, out pbLocked)
        ELSE                               
            RETURN ACE64.AdsIsTableLocked(hTable, out pbLocked)
        ENDIF

	PUBLIC STATIC METHOD AdsIsRecordLocked(hTable as IntPtr , ulRec as DWORD , pbLocked out WORD ) as DWORD 
       IF Is32Bits
            RETURN ACE32.AdsIsRecordLocked(hTable, ulRec, out pbLocked)
        ELSE                               
            RETURN ACE64.AdsIsRecordLocked(hTable, ulRec, out pbLocked)
        ENDIF

	
	PUBLIC STATIC METHOD AdsIsRecordVisible(hObj as IntPtr , pbVisible out WORD ) as DWORD 
       IF Is32Bits
            RETURN ACE32.AdsIsRecordVisible(hObj, out pbVisible)
        ELSE                               
            RETURN ACE64.AdsIsRecordVisible(hObj, out pbVisible)
        ENDIF

	
	PUBLIC STATIC METHOD AdsIsServerLoaded(strServer as string , pbLoaded out WORD ) as DWORD 
       IF Is32Bits
            RETURN ACE32.AdsIsServerLoaded(strServer, out pbLoaded)
        ELSE                               
            RETURN ACE64.AdsIsServerLoaded(strServer, out pbLoaded)
        ENDIF

 	PUBLIC STATIC METHOD AdsIsRecordDeleted(hTable as IntPtr , pbDeleted out WORD ) as DWORD 
          IF Is32Bits
                RETURN ACE32.AdsIsRecordDeleted(hTable, out pbDeleted)
            ELSE
                RETURN ACE64.AdsIsRecordDeleted(hTable, out pbDeleted)
            ENDIF

	PUBLIC STATIC METHOD AdsLockRecord(hTable as IntPtr , ulRec as DWORD ) as DWORD 
          IF Is32Bits
                RETURN ACE32.AdsLockRecord(hTable, ulRec)
            ELSE
                RETURN ACE64.AdsLockRecord(hTable, ulRec)
            ENDIF
	
	PUBLIC STATIC METHOD AdsLockTable(hTable as IntPtr ) as DWORD 
            IF Is32Bits
                RETURN ACE32.AdsLockTable(hTable)
            ELSE
                RETURN ACE64.AdsLockTable(hTable)
            ENDIF

	
	PUBLIC STATIC METHOD AdsPackTable(hTable as IntPtr ) as DWORD 
           IF Is32Bits
                RETURN ACE32.AdsPackTable(hTable)
            ELSE
                RETURN ACE64.AdsPackTable(hTable)
            ENDIF

	
	PUBLIC STATIC METHOD AdsRecallRecord(hTable as IntPtr ) as DWORD 
           IF Is32Bits
                RETURN ACE32.AdsRecallRecord(hTable)
            ELSE
                RETURN ACE64.AdsRecallRecord(hTable)
            ENDIF


	
	PUBLIC STATIC METHOD AdsRecallAllRecords(hTable as IntPtr ) as DWORD 
           IF Is32Bits
                RETURN ACE32.AdsRecallAllRecords(hTable)
            ELSE
                RETURN ACE64.AdsRecallAllRecords(hTable)
            ENDIF


	
	PUBLIC STATIC METHOD AdsRefreshRecord(hTable as IntPtr ) as DWORD 

           IF Is32Bits
                RETURN ACE32.AdsRefreshRecord(hTable)
            ELSE
                RETURN ACE64.AdsRefreshRecord(hTable)
            ENDIF

	
	PUBLIC STATIC METHOD AdsClearProgressCallback() as DWORD 
          IF Is32Bits
                RETURN ACE32.AdsClearProgressCallback()
            ELSE
                RETURN ACE64.AdsClearProgressCallback()
            ENDIF
	

	
	PUBLIC STATIC METHOD AdsClearCallbackFunction() as DWORD 
         IF Is32Bits
                RETURN ACE32.AdsClearCallbackFunction()
         ELSE
                RETURN ACE64.AdsClearCallbackFunction()
         ENDIF

	
	
	PUBLIC STATIC METHOD AdsResetConnection(hConnect as IntPtr) as DWORD 
                IF Is32Bits
                RETURN ACE32.AdsResetConnection(hConnect)
            ELSE
                RETURN ACE64.AdsResetConnection(hConnect)
            ENDIF

	
	PUBLIC STATIC METHOD AdsRollbackTransaction(hConnect as IntPtr) as DWORD 
            IF Is32Bits
                RETURN ACE32.AdsRollbackTransaction(hConnect)
            ELSE
                RETURN ACE64.AdsRollbackTransaction(hConnect)
            ENDIF

	PUBLIC STATIC METHOD AdsSeek(hIndex as IntPtr , strKey as string , usKeyLen AS WORD, usDataType as WORD, usSeekType as WORD, pbFound out WORD) as DWORD 
            IF Is32Bits
                RETURN ACE32.AdsSeek(hIndex, strKey, usKeyLen, usDataType, usSeekType, out pbFound)
            ELSE
                RETURN ACE64.AdsSeek(hIndex, strKey, usKeyLen, usDataType, usSeekType, out pbFound)
            ENDIF

	
	PUBLIC STATIC METHOD AdsSeek(hIndex as IntPtr , abKey as byte[] , usKeyLen AS WORD, usDataType as WORD, usSeekType as WORD, pbFound out WORD) as DWORD 
            IF Is32Bits
                RETURN ACE32.AdsSeek(hIndex, abKey, usKeyLen, usDataType, usSeekType, out pbFound)
            ELSE
                RETURN ACE64.AdsSeek(hIndex, abKey, usKeyLen, usDataType, usSeekType, out pbFound)
            ENDIF

	
	PUBLIC STATIC METHOD AdsSeekLast(hIndex as IntPtr , strKey as string , usKeyLen AS WORD, usDataType as WORD, pbFound out WORD) as DWORD 
            IF Is32Bits
                RETURN ACE32.AdsSeekLast(hIndex, strKey, usKeyLen, usDataType, out pbFound)
            ELSE
                RETURN ACE64.AdsSeekLast(hIndex, strKey, usKeyLen, usDataType, out pbFound)
            ENDIF

	
	PUBLIC STATIC METHOD AdsSeekLast(hIndex as IntPtr , abKey as byte[] , usKeyLen AS WORD, usDataType as WORD, pbFound out WORD) as DWORD 
            IF Is32Bits
                RETURN ACE32.AdsSeekLast(hIndex, abKey, usKeyLen, usDataType, out pbFound)
            ELSE
                RETURN ACE64.AdsSeekLast(hIndex, abKey, usKeyLen, usDataType, out pbFound)
            ENDIF

	PUBLIC STATIC METHOD AdsSetDateFormat(strFormat as string ) as DWORD 
        IF Is32Bits
                RETURN ACE32.AdsSetDateFormat(strFormat)
         ELSE
                RETURN ACE64.AdsSetDateFormat(strFormat)
         ENDIF

	
	PUBLIC STATIC METHOD AdsSetDecimals(usDecimals as WORD ) as DWORD 
        IF Is32Bits
                RETURN ACE32.AdsSetDecimals(usDecimals)
         ELSE
                RETURN ACE64.AdsSetDecimals(usDecimals)
         ENDIF

	PUBLIC STATIC METHOD AdsShowDeleted(bShowDeleted as WORD ) as DWORD 
            IF Is32Bits
                RETURN ACE32.AdsShowDeleted(bShowDeleted)
         ELSE
                RETURN ACE64.AdsShowDeleted(bShowDeleted)
         ENDIF

	
	PUBLIC STATIC METHOD AdsSetEpoch(usCentury as WORD ) as DWORD 
            IF Is32Bits
                RETURN ACE32.AdsSetEpoch(usCentury)
         ELSE
                RETURN ACE64.AdsSetEpoch(usCentury)
         ENDIF

	
	PUBLIC STATIC METHOD AdsSetExact(bExact as WORD ) as DWORD 
            IF Is32Bits
                RETURN ACE32.AdsSetExact(bExact)
         ELSE
                RETURN ACE64.AdsSetExact(bExact)
         ENDIF
 	
	PUBLIC STATIC METHOD AdsSetFilter(hTable as IntPtr , strFilter as string ) as DWORD 
            IF Is32Bits
                RETURN ACE32.AdsSetFilter(hTable, strFilter)
         ELSE
                RETURN ACE64.AdsSetFilter(hTable, strFilter)
         ENDIF
	PUBLIC STATIC METHOD AdsSetRelation(hTableParent as IntPtr , hIndexChild as IntPtr , strExpr as string ) as DWORD 
            IF Is32Bits
                RETURN ACE32.AdsSetRelation(hTableParent, hIndexChild, strExpr)
         ELSE
                RETURN ACE64.AdsSetRelation(hTableParent, hIndexChild, strExpr)
         ENDIF
	PUBLIC STATIC METHOD AdsSetServerType(usServerOptions as WORD ) as DWORD 
            IF Is32Bits
                RETURN ACE32.AdsSetServerType(usServerOptions)
         ELSE
                RETURN ACE64.AdsSetServerType(usServerOptions)
         ENDIF
	PUBLIC STATIC METHOD AdsSetScope(hIndex as IntPtr , usScopeOption as WORD, strScope as string , usScopeLen as WORD , usDataType as WORD) as DWORD 
            IF Is32Bits
                RETURN ACE32.AdsSetScope(hIndex, usScopeOption, strScope, usScopeLen, usDataType)
         ELSE
                RETURN ACE64.AdsSetScope(hIndex, usScopeOption, strScope, usScopeLen, usDataType)
         ENDIF
	PUBLIC STATIC METHOD AdsSetScope(hIndex as IntPtr , usScopeOption as WORD, abScope as byte[] , usScopeLen as WORD , usDataType as WORD) as DWORD 
            IF Is32Bits
                RETURN ACE32.AdsSetScope(hIndex, usScopeOption, abScope, usScopeLen, usDataType)
         ELSE
                RETURN ACE64.AdsSetScope(hIndex, usScopeOption, abScope, usScopeLen, usDataType)
         ENDIF

 	PUBLIC STATIC METHOD AdsSkip(hObj as IntPtr , lRecs as int) as DWORD 
            IF Is32Bits
                RETURN ACE32.AdsSkip(hObj, lRecs)
         ELSE
                RETURN ACE64.AdsSkip(hObj, lRecs)
         ENDIF
	
	PUBLIC STATIC METHOD AdsThreadExit() as DWORD 
         IF Is32Bits
                RETURN ACE32.AdsThreadExit()
         ELSE
                RETURN ACE64.AdsThreadExit()
         ENDIF

	
	PUBLIC STATIC METHOD AdsUnlockRecord(hTable as IntPtr , ulRec as DWORD) as DWORD 
           IF Is32Bits
                RETURN ACE32.AdsUnlockRecord(hTable, ulRec)
            ELSE
                RETURN ACE64.AdsUnlockRecord(hTable, ulRec)
            ENDIF

	
	PUBLIC STATIC METHOD AdsUnlockTable(hTable as IntPtr ) as DWORD 
           IF Is32Bits
                RETURN ACE32.AdsUnlockTable(hTable)
            ELSE
                RETURN ACE64.AdsUnlockTable(hTable)
            ENDIF

	
	
	PUBLIC STATIC METHOD AdsWriteAllRecords() as DWORD 
        IF Is32Bits
                RETURN ACE32.AdsWriteAllRecords()
         ELSE
                RETURN ACE64.AdsWriteAllRecords()
         ENDIF

	
	PUBLIC STATIC METHOD AdsWriteRecord(hTable as IntPtr ) as DWORD 
           IF Is32Bits
                RETURN ACE32.AdsWriteRecord(hTable)
            ELSE
                RETURN ACE64.AdsWriteRecord(hTable)
            ENDIF

	
	PUBLIC STATIC METHOD AdsZapTable(hTable as IntPtr ) as DWORD 
           IF Is32Bits
                RETURN ACE32.AdsZapTable(hTable)
            ELSE
                RETURN ACE64.AdsZapTable(hTable)
            ENDIF

	
	PUBLIC STATIC METHOD AdsSetAOF(hTable as IntPtr , strFilter as string , usOptions as WORD ) as DWORD 
            IF Is32Bits
                RETURN ACE32.AdsSetAOF(hTable, strFilter, usOptions)
            ELSE
                RETURN ACE64.AdsSetAOF(hTable, strFilter, usOptions)
            ENDIF

	
	
	PUBLIC STATIC METHOD AdsClearAOF(hTable as IntPtr ) as DWORD 
           IF Is32Bits
                RETURN ACE32.AdsClearAOF(hTable)
            ELSE
                RETURN ACE64.AdsClearAOF(hTable)
            ENDIF

	
	PUBLIC STATIC METHOD AdsRefreshAOF(hTable as IntPtr ) as DWORD 
           IF Is32Bits
                RETURN ACE32.AdsRefreshAOF(hTable)
            ELSE
                RETURN ACE64.AdsRefreshAOF(hTable)
            ENDIF

	

	
	PUBLIC STATIC METHOD AdsInitRawKey(hIndex as IntPtr ) as DWORD 
         IF Is32Bits
                RETURN ACE32.AdsInitRawKey(hIndex)
            ELSE
                RETURN ACE64.AdsInitRawKey(hIndex)
            ENDIF

	
	
	PUBLIC STATIC METHOD AdsExecuteSQL(hStatement as IntPtr , phCursor out IntPtr ) as DWORD 
        IF Is32Bits
                RETURN ACE32.AdsExecuteSQL(hStatement, out phCursor)
         ELSE
                RETURN ACE64.AdsExecuteSQL(hStatement, out phCursor)
         ENDIF

	

	
	PUBLIC STATIC METHOD AdsCloseSQLStatement(hStatement as IntPtr ) as DWORD 
        IF Is32Bits
                RETURN ACE32.AdsCloseSQLStatement(hStatement)
         ELSE
                RETURN ACE64.AdsCloseSQLStatement(hStatement)
         ENDIF


	
	PUBLIC STATIC METHOD AdsStmtDisableEncryption(hStatement as IntPtr ) as DWORD 
        IF Is32Bits
                RETURN ACE32.AdsStmtDisableEncryption(hStatement)
         ELSE
                RETURN ACE64.AdsStmtDisableEncryption(hStatement)
         ENDIF

	

	
	PUBLIC STATIC METHOD AdsStmtClearTablePasswords(hStatement as IntPtr ) as DWORD 
        IF Is32Bits
                RETURN ACE32.AdsStmtClearTablePasswords(hStatement)
         ELSE
                RETURN ACE64.AdsStmtClearTablePasswords(hStatement)
         ENDIF

	

	
	PUBLIC STATIC METHOD AdsClearSQLParams(hStatement as IntPtr ) as DWORD 
        IF Is32Bits
                RETURN ACE32.AdsClearSQLParams(hStatement)
         ELSE
                RETURN ACE64.AdsClearSQLParams(hStatement)
         ENDIF
	

	
	PUBLIC STATIC METHOD AdsClearSQLAbortFunc() as DWORD 
        IF Is32Bits
                RETURN ACE32.AdsClearSQLAbortFunc()
         ELSE
                RETURN ACE64.AdsClearSQLAbortFunc()
         ENDIF

	
	
	PUBLIC STATIC METHOD AdsFlushFileBuffers(hTable as IntPtr ) as DWORD 
           IF Is32Bits
                RETURN ACE32.AdsFlushFileBuffers(hTable)
            ELSE
                RETURN ACE64.AdsFlushFileBuffers(hTable)
            ENDIF

	
	
	PUBLIC STATIC METHOD AdsDisableUniqueEnforcement(hConnect as IntPtr) as DWORD 
            IF Is32Bits
                RETURN ACE32.AdsDisableUniqueEnforcement(hConnect)
            ELSE
                RETURN ACE64.AdsDisableUniqueEnforcement(hConnect)
            ENDIF

	
	PUBLIC STATIC METHOD AdsEnableUniqueEnforcement(hConnect as IntPtr) as DWORD 
            IF Is32Bits
                RETURN ACE32.AdsEnableUniqueEnforcement(hConnect)
            ELSE
                RETURN ACE64.AdsEnableUniqueEnforcement(hConnect)
            ENDIF

	
	PUBLIC STATIC METHOD AdsDisableRI(hConnect as IntPtr) as DWORD 
        IF Is32Bits
                RETURN ACE32.AdsDisableRI(hConnect)
         ELSE
                RETURN ACE64.AdsDisableRI(hConnect)
         ENDIF


	
	PUBLIC STATIC METHOD AdsEnableRI(hConnect as IntPtr) as DWORD 
        IF Is32Bits
                RETURN ACE32.AdsEnableRI(hConnect)
         ELSE
                RETURN ACE64.AdsEnableRI(hConnect)
         ENDIF

	
	PUBLIC STATIC METHOD AdsDisableAutoIncEnforcement(hConnection as IntPtr) as DWORD 
       IF Is32Bits
                RETURN ACE32.AdsDisableAutoIncEnforcement(hConnection)
         ELSE
                RETURN ACE64.AdsDisableAutoIncEnforcement(hConnection)
         ENDIF

	
	PUBLIC STATIC METHOD AdsEnableAutoIncEnforcement(hConnection as IntPtr) as DWORD 
      IF Is32Bits
                RETURN ACE32.AdsEnableAutoIncEnforcement(hConnection)
         ELSE
                RETURN ACE64.AdsEnableAutoIncEnforcement(hConnection)
         ENDIF
	
    #endregion

    // Unused methods
    /*
	PUBLIC STATIC METHOD AdsBuildRawKey(hIndex as IntPtr , [In] [Out] strKey as byte[] , pusKeyLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsCancelUpdate90(hTable as IntPtr , ulOptions as DWORD) as DWORD 
	PUBLIC STATIC METHOD AdsCheckExistence(hConnect as IntPtr, strFileName as string , pusOnDisk out WORD) as DWORD 
	PUBLIC STATIC METHOD AdsCompareBookmarks(strBookmark1 as string , strBookmark2 as string , plResult out int ) as DWORD 
	PUBLIC STATIC METHOD AdsConnect(strServerName as string , phConnect out IntPtr ) as DWORD 
	PUBLIC STATIC METHOD AdsConnect26(strServerName as string , usServerTypes as WORD, phConnect out IntPtr ) as DWORD 
	PUBLIC STATIC METHOD AdsConnect60(strServerPath as string , usServerTypes as WORD, strUserName as string , strPassword as string , ulOptions as DWORD, phConnect out IntPtr ) as DWORD 
	PUBLIC STATIC METHOD AdsConvertTable(hObj as IntPtr ,  usFilterOption as WORD, strFile as string , usTableType as WORD) as DWORD 
	PUBLIC STATIC METHOD AdsCopyTable(hObj as IntPtr , usFilterOption as WORD , strFile as string ) as DWORD 
	PUBLIC STATIC METHOD AdsCopyTableContents(hObjFrom as IntPtr , hTableTo as IntPtr , usFilterOption as WORD) as DWORD 
	PUBLIC STATIC METHOD AdsCreateIndex(hObj as IntPtr , strFileName as string , strTag as string , strExpr as string , strCondition as string , strWhile as string , ulOptions as DWORD, phIndex out IntPtr ) as DWORD 
	PUBLIC STATIC METHOD AdsCreateIndex61(hObj as IntPtr , strFileName as string , strTag as string , strExpr as string , strCondition as string , strWhile as string , ulOptions as DWORD, ulPageSize as DWORD , phIndex out IntPtr ) as DWORD 
	PUBLIC STATIC METHOD AdsCreateIndex90(hObj as IntPtr , strFileName as string , strTag as string , strExpr as string , strCondition as string , strWhile as string , ulOptions as DWORD, ulPageSize as DWORD , strCollation as string , phIndex out IntPtr ) as DWORD 
	PUBLIC STATIC METHOD AdsCreateSQLStatement(hConnect as IntPtr, phStatement out IntPtr ) as DWORD 
	PUBLIC STATIC METHOD AdsCreateSavepoint(hConnect as IntPtr, strSavepoint as string , ulOptions as DWORD) as DWORD 
	PUBLIC STATIC METHOD AdsCreateTable(hConnect as IntPtr, strName as string , strAlias as string , usTableType as WORD, usCharType as WORD , usLockType as WORD , usCheckRights as WORD , usMemoSize as WORD , strFields as string , phTable out IntPtr ) as DWORD 
	PUBLIC STATIC METHOD AdsCreateTable71(hConnect as IntPtr, strName as string , strDBObjName as string , usTableType as WORD, usCharType as WORD , usLockType as WORD , usCheckRights as WORD , usMemoSize as WORD , strFields as string , ulOptions as DWORD, phTable out IntPtr ) as DWORD 
	PUBLIC STATIC METHOD AdsCreateTable90(hConnect as IntPtr, strName as string , strDBObjName as string , usTableType as WORD, usCharType as WORD , usLockType as WORD , usCheckRights as WORD , usMemoSize as WORD , strFields as string , ulOptions as DWORD, strCollation as string , phTable out IntPtr ) as DWORD 
	PUBLIC STATIC METHOD AdsCustomizeAOF(hTable as IntPtr , ulNumRecords as DWORD , pulRecords out DWORD , usOption as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDAddIndexFile(hDictionary AS IntPtr , strTableName as string , strIndexFilePath as string , strComment as string ) as DWORD 
	PUBLIC STATIC METHOD AdsDDAddProcedure(hDictionary AS IntPtr , strName as string , strContainer as string , strProcName as string , ulInvokeOption as DWORD , strInParams as string , strOutParams as string , strComments as string ) as DWORD 
	PUBLIC STATIC METHOD AdsDDAddTable(hDictionary AS IntPtr , strTableName as string , strTablePath as string , usTableType as WORD, usCharType as WORD , strIndexFiles as string , strComments as string ) as DWORD 
	PUBLIC STATIC METHOD AdsDDAddTable90(hDictionary AS IntPtr , strTableName as string , strTablePath as string , usTableType as WORD, usCharType as WORD , strIndexFiles as string , strComments as string , strCollation as string ) as DWORD 
	PUBLIC STATIC METHOD AdsDDAddUserToGroup(hDictionary AS IntPtr , strGroupName as string , strUserName as string ) as DWORD 
	PUBLIC STATIC METHOD AdsDDAddView(hDictionary AS IntPtr , strName as string , strComments as string , strSQL as string ) as DWORD 
	PUBLIC STATIC METHOD AdsDDCreate(strDictionaryPath as string , usEncrypt as WORD, strDescription as string , phDictionary out IntPtr ) as DWORD 
	PUBLIC STATIC METHOD AdsDDCreateArticle(hDictionary AS IntPtr , strPublicationName as string , strObjectName as string , strRowIdentColumns as string , strFilter as string , ulOptions as DWORD) as DWORD 
	PUBLIC STATIC METHOD AdsDDCreateLink(hDBConn as IntPtr , strLinkAlias as string , strLinkedDDPath as string , strUserName as string , strPassword as string , ulOptions as DWORD) as DWORD 
	PUBLIC STATIC METHOD AdsDDCreatePublication(hDictionary AS IntPtr , strPublicationName as string , strComments as string , ulOptions as DWORD) as DWORD 
	PUBLIC STATIC METHOD AdsDDCreateRefIntegrity(hDictionary AS IntPtr , strRIName as string , strFailTable as string , strParentTableName as string , strParentTagName as string , strChildTableName as string , strChildTagName as string , usUpdateRule as WORD , usDeleteRule as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDCreateRefIntegrity62(hDictionary AS IntPtr , strRIName as string , strFailTable as string , strParentTableName as string , strParentTagName as string , strChildTableName as string , strChildTagName as string , usUpdateRule as WORD , usDeleteRule as WORD , strNoPrimaryError as string , strCascadeError as string ) as DWORD 
	PUBLIC STATIC METHOD AdsDDCreateSubscription(hDictionary AS IntPtr , strSubscriptionName as string , strPublicationName as string , strTarget as string , strUser as string , strPassword as string , strReplicationQueue as string , usForward as WORD , strComments as string , ulOptions as DWORD) as DWORD 
	PUBLIC STATIC METHOD AdsDDCreateTrigger(hDictionary AS IntPtr , strName as string , strTableName as string ,  ulTriggerType as DWORD, ulEventTypes as DWORD , ulContainerType as DWORD , strContainer as string , strFunctionName as string , ulPriority as DWORD , strComments as string , ulOptions as DWORD) as DWORD 
	PUBLIC STATIC METHOD AdsDDCreateUser(hDictionary AS IntPtr , strGroupName as string , strUserName as string , strPassword as string , strDescription as string ) as DWORD 
	PUBLIC STATIC METHOD AdsDDCreateUserGroup(hDictionary AS IntPtr , strGroupName as string , strDescription as string ) as DWORD 
	PUBLIC STATIC METHOD AdsDDDeleteArticle(hDictionary AS IntPtr , strPublicationName as string , strObjectName as string ) as DWORD 
	PUBLIC STATIC METHOD AdsDDDeleteIndex(hDictionary AS IntPtr , strTableName as string , strIndexName as string ) as DWORD 
	PUBLIC STATIC METHOD AdsDDDeletePublication(hDictionary AS IntPtr , strPublicationName as string ) as DWORD 
	PUBLIC STATIC METHOD AdsDDDeleteSubscription(hDictionary AS IntPtr , strSubscriptionName as string ) as DWORD 
	PUBLIC STATIC METHOD AdsDDDeleteUser(hDictionary AS IntPtr , strUserName as string ) as DWORD 
	PUBLIC STATIC METHOD AdsDDDeleteUserGroup(hDictionary AS IntPtr , strGroupName as string ) as DWORD 
	PUBLIC STATIC METHOD AdsDDDeployDatabase(strDestination as string , strDestinationPassword as string , strSource as string , strSourcePassword as string , usServerTypes as WORD, usValidateOption as WORD , usBackupFiles as WORD , ulOptions as DWORD) as DWORD 
	PUBLIC STATIC METHOD AdsDDDropLink(hDBConn as IntPtr , strLinkedDD as string , usDropGlobal as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDFindClose(hObject as IntPtr, hFindHandle as IntPtr ) as DWORD 
	PUBLIC STATIC METHOD AdsDDFindFirstObject(hObject as IntPtr, usFindObjectType as WORD , strParentName as string , [In] [Out] strObjectName as char[] , pusObjectNameLen ref WORD , phFindHandle out IntPtr ) as DWORD 
	PUBLIC STATIC METHOD AdsDDFindNextObject(hObject as IntPtr, hFindHandle as IntPtr , [In] [Out] strObjectName as char[] , pusObjectNameLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDFreeTable(strTableName as string , strPassword as string ) as DWORD 
	PUBLIC STATIC METHOD AdsDDGetArticleProperty(hObject as IntPtr, strPublicationName as string , strObjectName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , pusPropertyLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDGetArticleProperty(hObject as IntPtr, strPublicationName as string , strObjectName as string , usPropertyID as WORD, [In] [Out] strProperty as char[] , pusPropertyLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDGetArticleProperty(hObject as IntPtr, strPublicationName as string , strObjectName as string , usPropertyID as WORD, pusProperty ref WORD , pusPropertyLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDGetDatabaseProperty(hObject as IntPtr, usPropertyID as WORD, [In] [Out] pvProperty as byte[] , pusPropertyLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDGetDatabaseProperty(hObject as IntPtr, usPropertyID as WORD, [In] [Out] strProperty as char[] , pusPropertyLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDGetDatabaseProperty(hObject as IntPtr, usPropertyID as WORD, pusProperty ref WORD , pusPropertyLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDGetFieldProperty(hObject as IntPtr, strTableName as string , strFieldName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , pusPropertyLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDGetFieldProperty(hObject as IntPtr, strTableName as string , strFieldName as string , usPropertyID as WORD, [In] [Out] strProperty as char[] , pusPropertyLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDGetFieldProperty(hObject as IntPtr, strTableName as string , strFieldName as string , usPropertyID as WORD, pusProperty ref WORD , pusPropertyLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDGetIndexFileProperty(hObject as IntPtr, strTableName as string , strIndexFileName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , pusPropertyLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDGetIndexFileProperty(hObject as IntPtr, strTableName as string , strIndexFileName as string , usPropertyID as WORD, [In] [Out] strProperty as char[] , pusPropertyLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDGetIndexFileProperty(hObject as IntPtr, strTableName as string , strIndexFileName as string , usPropertyID as WORD, pusProperty ref WORD , pusPropertyLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDGetIndexProperty(hObject as IntPtr, strTableName as string , strIndexName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , pusPropertyLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDGetIndexProperty(hObject as IntPtr, strTableName as string , strIndexName as string , usPropertyID as WORD, [In] [Out] strProperty as char[] , pusPropertyLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDGetIndexProperty(hObject as IntPtr, strTableName as string , strIndexName as string , usPropertyID as WORD, pusProperty ref WORD , pusPropertyLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDGetLinkProperty(hConnect as IntPtr, strLinkName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , pusPropertyLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDGetLinkProperty(hConnect as IntPtr, strLinkName as string , usPropertyID as WORD, [In] [Out] strProperty as char[] , pusPropertyLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDGetLinkProperty(hConnect as IntPtr, strLinkName as string , usPropertyID as WORD, pusProperty ref WORD , pusPropertyLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDGetPermissions(hDBConn as IntPtr , strGrantee as string , usObjectType as WORD, strObjectName as string , strParentName as string , usGetInherited as WORD , pulPermissions out DWORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDGetProcedureProperty(hObject as IntPtr, strProcName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , pusPropertyLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDGetProcedureProperty(hObject as IntPtr, strProcName as string , usPropertyID as WORD, [In] [Out] strProperty as char[] , pusPropertyLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDGetProcedureProperty(hObject as IntPtr, strProcName as string , usPropertyID as WORD, pusProperty ref WORD , pusPropertyLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDGetPublicationProperty(hObject as IntPtr, strPublicationName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , pusPropertyLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDGetPublicationProperty(hObject as IntPtr, strPublicationName as string , usPropertyID as WORD, [In] [Out] strProperty as char[] , pusPropertyLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDGetPublicationProperty(hObject as IntPtr, strPublicationName as string , usPropertyID as WORD, pusProperty ref WORD , pusPropertyLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDGetRefIntegrityProperty(hObject as IntPtr, strRIName as string , usPropertyID as WORD, [In] [Out] strProperty as char[] , pusPropertyLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDGetSubscriptionProperty(hObject as IntPtr, strSubscriptionName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , pusPropertyLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDGetSubscriptionProperty(hObject as IntPtr, strSubscriptionName as string , usPropertyID as WORD, [In] [Out] strProperty as char[] , pusPropertyLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDGetSubscriptionProperty(hObject as IntPtr, strSubscriptionName as string , usPropertyID as WORD, pusProperty ref WORD , pusPropertyLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDGetTableProperty(hObject as IntPtr, strTableName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , pusPropertyLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDGetTableProperty(hObject as IntPtr, strTableName as string , usPropertyID as WORD, [In] [Out] strProperty as char[] , pusPropertyLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDGetTableProperty(hObject as IntPtr, strTableName as string , usPropertyID as WORD, pusProperty ref WORD , pusPropertyLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDGetTriggerProperty(hObject as IntPtr, strTriggerName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , pusPropertyLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDGetTriggerProperty(hObject as IntPtr, strTriggerName as string , usPropertyID as WORD, [In] [Out] strProperty as char[] , pusPropertyLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDGetTriggerProperty(hObject as IntPtr, strTriggerName as string , usPropertyID as WORD, pusProperty ref WORD , pusPropertyLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDGetUserGroupProperty(hObject as IntPtr, strUserGroupName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , pusPropertyLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDGetUserGroupProperty(hObject as IntPtr, strUserGroupName as string , usPropertyID as WORD, [In] [Out] strProperty as char[] , pusPropertyLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDGetUserGroupProperty(hObject as IntPtr, strUserGroupName as string , usPropertyID as WORD, pusProperty ref WORD , pusPropertyLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDGetUserProperty(hObject as IntPtr, strUserName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , pusPropertyLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDGetUserProperty(hObject as IntPtr, strUserName as string , usPropertyID as WORD, [In] [Out] strProperty as char[] , pusPropertyLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDGetUserProperty(hObject as IntPtr, strUserName as string , usPropertyID as WORD, pusProperty ref WORD , pusPropertyLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDGetViewProperty(hObject as IntPtr, strViewName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , pusPropertyLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDGetViewProperty(hObject as IntPtr, strViewName as string , usPropertyID as WORD, [In] [Out] strProperty as char[] , pusPropertyLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDGetViewProperty(hObject as IntPtr, strViewName as string , usPropertyID as WORD, pusProperty ref WORD , pusPropertyLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDGrantPermission(hAdminConn as IntPtr , usObjectType as WORD, strObjectName as string , strParentName as string , strGrantee as string , ulPermissions as DWORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDModifyLink(hDBConn as IntPtr , strLinkAlias as string , strLinkedDDPath as string , strUserName as string , strPassword as string , ulOptions as DWORD) as DWORD 
	PUBLIC STATIC METHOD AdsDDMoveObjectFile(hDictionary AS IntPtr , usObjectType as WORD, strObjectName as string , strNewPath as string , strIndexFiles as string , strParent as string , ulOptions as DWORD) as DWORD 
	PUBLIC STATIC METHOD AdsDDRemoveIndexFile(hDictionary AS IntPtr , strTableName as string , strIndexFileName as string , usDeleteFile as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDRemoveProcedure(hDictionary AS IntPtr , strName as string ) as DWORD 
	PUBLIC STATIC METHOD AdsDDRemoveRefIntegrity(hDictionary AS IntPtr , strRIName as string ) as DWORD 
	PUBLIC STATIC METHOD AdsDDRemoveTable(hObject as IntPtr, strTableName as string , usDeleteFiles as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDRemoveTrigger(hDictionary AS IntPtr , strName as string ) as DWORD 
	PUBLIC STATIC METHOD AdsDDRemoveUserFromGroup(hDictionary AS IntPtr , strGroupName as string , strUserName as string ) as DWORD 
	PUBLIC STATIC METHOD AdsDDRemoveView(hDictionary AS IntPtr , strName as string ) as DWORD 
	PUBLIC STATIC METHOD AdsDDRenameObject(hDictionary AS IntPtr , strObjectName as string , strNewObjectName as string , usObjectType as WORD, ulOptions as DWORD) as DWORD 
	PUBLIC STATIC METHOD AdsDDRevokePermission(hAdminConn as IntPtr , usObjectType as WORD, strObjectName as string , strParentName as string , strGrantee as string , ulPermissions as DWORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDSetArticleProperty(hDictionary AS IntPtr , strPublicationName as string , strObjectName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , usPropertyLen as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDSetArticleProperty(hDictionary AS IntPtr , strPublicationName as string , strObjectName as string , usPropertyID as WORD, [In] [Out] strProperty as char[] , usPropertyLen as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDSetArticleProperty(hDictionary AS IntPtr , strPublicationName as string , strObjectName as string , usPropertyID as WORD, pusProperty ref WORD , usPropertyLen as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDSetDatabaseProperty(hDictionary AS IntPtr , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , usPropertyLen as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDSetDatabaseProperty(hDictionary AS IntPtr , usPropertyID as WORD, [In] [Out] strProperty as char[] , usPropertyLen as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDSetDatabaseProperty(hDictionary AS IntPtr , usPropertyID as WORD, pusProperty ref WORD , usPropertyLen as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDSetFieldProperty(hDictionary AS IntPtr , strTableName as string , strFieldName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , usPropertyLen as WORD , usValidateOption as WORD , strFailTable as string ) as DWORD 
	PUBLIC STATIC METHOD AdsDDSetFieldProperty(hDictionary AS IntPtr , strTableName as string , strFieldName as string , usPropertyID as WORD, [In] [Out] strProperty as char[] , usPropertyLen as WORD , usValidateOption as WORD , strFailTable as string ) as DWORD 
	PUBLIC STATIC METHOD AdsDDSetFieldProperty(hDictionary AS IntPtr , strTableName as string , strFieldName as string , usPropertyID as WORD, pusProperty ref WORD , usPropertyLen as WORD , usValidateOption as WORD , strFailTable as string ) as DWORD 
	PUBLIC STATIC METHOD AdsDDSetIndexProperty(hAdminConn as IntPtr , strTableName as string , strIndexName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , usPropertyLen as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDSetIndexProperty(hAdminConn as IntPtr , strTableName as string , strIndexName as string , usPropertyID as WORD, [In] [Out] strProperty as char[] , usPropertyLen as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDSetIndexProperty(hAdminConn as IntPtr , strTableName as string , strIndexName as string , usPropertyID as WORD, pusProperty ref WORD , usPropertyLen as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDSetObjectAccessRights(hDictionary AS IntPtr , strObjectName as string , strAccessorName as string , strAllowedAccess as string ) as DWORD 
	PUBLIC STATIC METHOD AdsDDSetProcedureProperty(hDictionary AS IntPtr , strProcedureName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , usPropertyLen as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDSetProcedureProperty(hDictionary AS IntPtr , strProcedureName as string , usPropertyID as WORD, [In] [Out] strProperty as char[] , usPropertyLen as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDSetProcedureProperty(hDictionary AS IntPtr , strProcedureName as string , usPropertyID as WORD, pusProperty ref WORD , usPropertyLen as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDSetPublicationProperty(hDictionary AS IntPtr , strPublicationName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , usPropertyLen as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDSetPublicationProperty(hDictionary AS IntPtr , strPublicationName as string , usPropertyID as WORD, [In] [Out] strProperty as char[] , usPropertyLen as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDSetPublicationProperty(hDictionary AS IntPtr , strPublicationName as string , usPropertyID as WORD, pusProperty ref WORD , usPropertyLen as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDSetSubscriptionProperty(hDictionary AS IntPtr , strSubscriptionName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , usPropertyLen as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDSetSubscriptionProperty(hDictionary AS IntPtr , strSubscriptionName as string , usPropertyID as WORD, [In] [Out] strProperty as char[] , usPropertyLen as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDSetSubscriptionProperty(hDictionary AS IntPtr , strSubscriptionName as string , usPropertyID as WORD, pusProperty ref WORD , usPropertyLen as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDSetTableProperty(hDictionary AS IntPtr , strTableName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , usPropertyLen as WORD , usValidateOption as WORD , strFailTable as string ) as DWORD 
	PUBLIC STATIC METHOD AdsDDSetTableProperty(hDictionary AS IntPtr , strTableName as string , usPropertyID as WORD, [In] [Out] strProperty as char[] , usPropertyLen as WORD , usValidateOption as WORD , strFailTable as string ) as DWORD 
	PUBLIC STATIC METHOD AdsDDSetTableProperty(hDictionary AS IntPtr , strTableName as string , usPropertyID as WORD, pusProperty ref WORD , usPropertyLen as WORD , usValidateOption as WORD , strFailTable as string ) as DWORD 
	PUBLIC STATIC METHOD AdsDDSetUserGroupProperty(hDictionary AS IntPtr , strUserGroupName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , usPropertyLen as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDSetUserGroupProperty(hDictionary AS IntPtr , strUserGroupName as string , usPropertyID as WORD, [In] [Out] strProperty as char[] , usPropertyLen as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDSetUserGroupProperty(hDictionary AS IntPtr , strUserGroupName as string , usPropertyID as WORD, pusProperty ref WORD , usPropertyLen as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDSetUserProperty(hDictionary AS IntPtr , strUserName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , usPropertyLen as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDSetUserProperty(hDictionary AS IntPtr , strUserName as string , usPropertyID as WORD, [In] [Out] strProperty as char[] , usPropertyLen as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDSetUserProperty(hDictionary AS IntPtr , strUserName as string , usPropertyID as WORD, pusProperty ref WORD , usPropertyLen as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDSetViewProperty(hDictionary AS IntPtr , strViewName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , usPropertyLen as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDSetViewProperty(hDictionary AS IntPtr , strViewName as string , usPropertyID as WORD, [In] [Out] strProperty as char[] , usPropertyLen as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsDDSetViewProperty(hDictionary AS IntPtr , strViewName as string , usPropertyID as WORD, pusProperty ref WORD , usPropertyLen as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsEvalAOF(hTable as IntPtr , strFilter as string , pusOptLevel out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsExecuteSQLDirect(hStatement as IntPtr , strSQL as string , phCursor out IntPtr ) as DWORD 
	PUBLIC STATIC METHOD AdsExtractKey(hIndex as IntPtr , [In] [Out] strKey as char[] , wLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsFailedTransactionRecovery(strServer as string ) as DWORD 
	PUBLIC STATIC METHOD AdsFindClose(hConnect as IntPtr, lHandle as IntPtr ) as DWORD 
	PUBLIC STATIC METHOD AdsFindConnection25(strFullPath as string , phConnect out IntPtr ) as DWORD 
	PUBLIC STATIC METHOD AdsFindFirstTable(hConnect as IntPtr, strFileMask as string , [In] [Out] strFirstFile as char[] , pusFileLen ref WORD , plHandle out IntPtr ) as DWORD 
	PUBLIC STATIC METHOD AdsFindFirstTable62(hConnect as IntPtr, strFileMask as string , [In] [Out] strFirstDD as char[] , pusDDLen ref WORD , [In] [Out] strFirstFile as char[] , pusFileLen ref WORD , plHandle out IntPtr ) as DWORD 
	PUBLIC STATIC METHOD AdsFindNextTable(hConnect as IntPtr, lHandle as IntPtr , [In] [Out] strFileName as char[] , pusFileLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsFindNextTable62(hConnect as IntPtr, lHandle as IntPtr , [In] [Out] strDDName as char[] , pusDDLen ref WORD , [In] [Out] strFileName as char[] , pusFileLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetAOF(hTable as IntPtr , [In] [Out] strFilter as char[] , wLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetAOFOptLevel(hTable as IntPtr , pusOptLevel out WORD , [In] [Out] strNonOpt as char[] , wLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetActiveLinkInfo(hDBConn as IntPtr , usLinkNum as WORD , [In] [Out] strLinkInfo as char[] , pusBufferLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetBookmark60(hObj as IntPtr , [In] [Out] strBookmark as char[] , pulLength ref DWORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetBookmarkLength(hObj as IntPtr , pulLength ref DWORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetCollation(hConnect as IntPtr, [In] [Out] strCollation as char[] , wLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetCollationLang([In] [Out] strLang as char[] , wLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetConnectionPath(hConnect as IntPtr, [In] [Out] strConnectionPath as char[] , wLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetConnectionProperty(hConnect as IntPtr, usPropertyID as WORD, [In] [Out] pvProperty as byte[] , pulPropertyLen ref DWORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetDataLength(hTable as IntPtr , lFieldOrdinal as DWORD, ulOptions as DWORD, pulLength out DWORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetDataLength(hTable as IntPtr , strFldName as string , ulOptions as DWORD, pulLength out DWORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetDateFormat60(hConnect as IntPtr, [In] [Out] strFormat as char[] , wLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetDecimals(pusDecimals out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetDefault([In] [Out] strDefault as char[] , wLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetDeleted(pbUseDeleted out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetEpoch(pusCentury out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetErrorString(ulErrCode as DWORD , [In] [Out] strBuf as char[] , pusBufLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetExact(pbExact out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetExact22(hObj as IntPtr , pbExact out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetField(hTable as IntPtr , lFieldOrdinal as DWORD, [In] [Out] abBuf as byte[] , pulLen ref DWORD , usOption as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetField(hTable as IntPtr , lFieldOrdinal as DWORD, [In] [Out] strBuf as char[] , pulLen ref DWORD , usOption as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetField(hTable as IntPtr , strFldName as string , [In] [Out] abBuf as byte[] , pulLen ref DWORD , usOption as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetField(hTable as IntPtr , strFldName as string , [In] [Out] strBuf as char[] , pulLen ref DWORD , usOption as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetFieldDecimals(hTable as IntPtr , lFieldOrdinal as DWORD, pusDecimals out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetFieldDecimals(hTable as IntPtr , strFldName as string , pusDecimals out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetFieldName(hTable as IntPtr , usFld as WORD , [In] [Out] strName as char[] , pusBufLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetFieldNum(hTable as IntPtr , lFieldOrdinal as DWORD, pusNum out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetFieldNum(hTable as IntPtr , strFldName as string , pusNum out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetFieldOffset(hTable as IntPtr , lFieldOrdinal as DWORD, pulOffset out DWORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetFieldOffset(hTable as IntPtr , strFldName as string , pulOffset out DWORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetFilter(hTable as IntPtr , [In] [Out] strFilter as char[] , wLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetHandleINT64(hObj as IntPtr , pulVal out DWORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetINT64(hTable as IntPtr , lFieldOrdinal as DWORD, plValue out int ) as DWORD 
	PUBLIC STATIC METHOD AdsGetINT64(hTable as IntPtr , strFldName as string , plValue out int ) as DWORD 
	PUBLIC STATIC METHOD AdsGetINT64INT64(hTable as IntPtr , lFieldOrdinal as DWORD, pqValue out INT64 ) as DWORD 
	PUBLIC STATIC METHOD AdsGetINT64INT64(hTable as IntPtr , strFldName as string , pqValue out INT64 ) as DWORD 
	PUBLIC STATIC METHOD AdsGetIndexCollation(hIndex as IntPtr , [In] [Out] strCollation as char[] , wLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetIndexCondition(hIndex as IntPtr , [In] [Out] strExpr as char[] , wLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetIndexExpr(hIndex as IntPtr , [In] [Out] strExpr as char[] , wLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetIndexFilename(hIndex as IntPtr , usOption as WORD , [In] [Out] strName as char[] , wLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetIndexHandleByExpr(hTable as IntPtr , strExpr as string , ulDescending as DWORD , phIndex out IntPtr ) as DWORD 
	PUBLIC STATIC METHOD AdsGetIndexHandleByOrder(hTable as IntPtr , usOrderNum as WORD , phIndex out IntPtr ) as DWORD 
	PUBLIC STATIC METHOD AdsGetIndexName(hIndex as IntPtr , [In] [Out] strName as char[] , wLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetIndexOrderByHandle(hIndex as IntPtr , pusIndexOrder out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetJulian(hTable as IntPtr , lFieldOrdinal as DWORD, plDate out int ) as DWORD 
	PUBLIC STATIC METHOD AdsGetJulian(hTable as IntPtr , strFldName as string , plDate out int ) as DWORD 
	PUBLIC STATIC METHOD AdsGetKeyColumn(hCursor as IntPtr , [In] [Out] strKeyColumn as char[] , wLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetKeyCount(hIndex as IntPtr , usFilterOption as WORD, pulCount out DWORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetKeyLength(hIndex as IntPtr , pusKeyLength out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetKeyNum(hIndex as IntPtr , usFilterOption as WORD, pulKey out DWORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetKeyType(hIndex as IntPtr , usKeyType out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetLastAutoinc(hObj as IntPtr , pulAutoIncVal out DWORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetLogical(hTable as IntPtr , lFieldOrdinal as DWORD, pbValue out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetLogical(hTable as IntPtr , strFldName as string , pbValue out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetMemoDataType(hTable as IntPtr , lFieldOrdinal as DWORD, pusType out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetMemoDataType(hTable as IntPtr , strFldName as string , pusType out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetMilliseconds(hTable as IntPtr , lFieldOrdinal as DWORD, plTime out int ) as DWORD 
	PUBLIC STATIC METHOD AdsGetMilliseconds(hTable as IntPtr , strFldName as string , plTime out int ) as DWORD 
	PUBLIC STATIC METHOD AdsGetMoney(hTbl as IntPtr , lFieldOrdinal as DWORD, pqValue out INT64 ) as DWORD 
	PUBLIC STATIC METHOD AdsGetMoney(hTbl as IntPtr , strFldName as string , pqValue out INT64 ) as DWORD 
	PUBLIC STATIC METHOD AdsGetNumActiveLinks(hDBConn as IntPtr , pusNumLinks out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetNumFTSIndexes(hTable as IntPtr , pusNum out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetNumFields(hTable as IntPtr , pusCount out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetNumIndexes(hTable as IntPtr , pusNum out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetNumOpenTables(pusNum out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetNumParams(hStatement as IntPtr , pusNumParams out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetRecord(hTable as IntPtr , [In] [Out] strRec as byte[] , pulLen ref DWORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetRecordLength(hTable as IntPtr , pulLength out DWORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetRelKeyPos(hIndex as IntPtr , pdPos out double ) as DWORD 
	PUBLIC STATIC METHOD AdsGetSQLStatement(hStmt as IntPtr , [In] [Out] strSQL as char[] , wLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetSQLStatementHandle(hCursor as IntPtr , phStmt out IntPtr ) as DWORD 
	PUBLIC STATIC METHOD AdsGetSearchPath([In] [Out] strPath as char[] , wLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetServerName(hConnect as IntPtr, strName as char[] , wLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetServerTime(hConnect as IntPtr,  strDateBuf as char[], pusDateBufLen ref WORD , plTime out int , [In] [Out] strTimeBuf as char[] , pusTimeBufLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetShort(hTable as IntPtr , lFieldOrdinal as DWORD, psValue out short ) as DWORD 
	PUBLIC STATIC METHOD AdsGetShort(hTable as IntPtr , strFldName as string , psValue out short ) as DWORD 
	PUBLIC STATIC METHOD AdsGetString(hTable as IntPtr , lFieldOrdinal as DWORD, [In] [Out] strBuf as char[] , pulLen ref DWORD , usOption as WORD) as DWORD 
	PUBLIC STATIC METHOD AdsGetString(hTable as IntPtr , strFldName as string , [In] [Out] strBuf as char[] , pulLen ref DWORD , usOption as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetTableAlias(hTable as IntPtr , [In] [Out] strAlias as char[] , wLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetTableCharType(hTable as IntPtr , pusCharType out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetTableCollation(hTbl as IntPtr , [In] [Out] strCollation as char[] , wLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetTableConnection(hTable as IntPtr , phConnect out IntPtr ) as DWORD 
	PUBLIC STATIC METHOD AdsGetTableFilename(hTable as IntPtr , usOption as WORD , [In] [Out] strName as char[] , wLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetTableHandle(strName as string , phTable out IntPtr ) as DWORD 
	PUBLIC STATIC METHOD AdsGetTableHandle25(hConnect as IntPtr, strName as string , phTable out IntPtr ) as DWORD 
	PUBLIC STATIC METHOD AdsGetTableLockType(hTable as IntPtr , pusLockType out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetTableMemoSize(hTable as IntPtr , pusMemoSize out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetTableRights(hTable as IntPtr , pusRights out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetTableType(hTable as IntPtr , pusType out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetTime(hTable as IntPtr , lFieldOrdinal as DWORD, [In] [Out] strBuf as char[] , wLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetTime(hTable as IntPtr , strFldName as string , [In] [Out] strBuf as char[] , wLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGetVersion(pulMajor out DWORD , pulMinor out DWORD , strLetter as string , [In] [Out] strDesc as char[] , pusDescLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsGotoBookmark(hTable as IntPtr , hBookmark as IntPtr ) as DWORD 
	PUBLIC STATIC METHOD AdsGotoBookmark60(hObj as IntPtr , strBookmark as string ) as DWORD 
	PUBLIC STATIC METHOD AdsImageToClipboard(hTable as IntPtr , strFldName as string ) as DWORD 
	PUBLIC STATIC METHOD AdsIsConnectionAlive(hConnect as IntPtr, pbConnectionIsAlive out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsIsEmpty(hTable as IntPtr , lFieldOrdinal as DWORD, pbEmpty out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsIsEmpty(hTable as IntPtr , strFldName as string , pbEmpty out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsIsEncryptionEnabled(hTable as IntPtr , pusEnabled out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsIsExprValid(hTable as IntPtr , strExpr as string , pbValid out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsIsFieldBinary(hTable as IntPtr , lFieldOrdinal as DWORD, pbBinary out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsIsFieldBinary(hTable as IntPtr , strFldName as string , pbBinary out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsIsIndexCandidate(hIndex as IntPtr , pbCandidate out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsIsIndexCompound(hIndex as IntPtr , pbCompound out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsIsIndexCustom(hIndex as IntPtr , pbCustom out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsIsIndexDescending(hIndex as IntPtr , pbDescending out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsIsIndexFTS(hIndex as IntPtr , pbFTS out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsIsIndexNullable(hIndex as IntPtr , pbNullable out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsIsIndexPrimaryKey(hIndex as IntPtr , pbPrimaryKey out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsIsIndexUnique(hIndex as IntPtr , pbUnique out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsIsIndexUserDefined(hIndex as IntPtr , pbUserDefined out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsIsNull(hTable as IntPtr , lFieldOrdinal as DWORD, pbNull out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsIsNull(hTable as IntPtr , strFldName as string , pbNull out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsIsNullable(hTable as IntPtr , lFieldOrdinal as DWORD, pbNullable out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsIsNullable(hTable as IntPtr , strFldName as string , pbNullable out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsIsRecordEncrypted(hTable as IntPtr , pbEncrypted out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsIsRecordInAOF(hTable as IntPtr , ulRecordNum as DWORD , pusIsInAOF out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsIsTableEncrypted(hTable as IntPtr , pbEncrypted out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsLocate(hTable as IntPtr , strExpr as string , bForward as WORD , pbFound out WORD) as DWORD 
	PUBLIC STATIC METHOD AdsLookupKey(hIndex as IntPtr , strKey as string , usKeyLen AS WORD, usDataType as WORD, pbFound out WORD) as DWORD 
	PUBLIC STATIC METHOD AdsMgConnect(strServerName as string , strUserName as string , strPassword as string , phMgmtHandle out IntPtr ) as DWORD 
	PUBLIC STATIC METHOD AdsMgDisconnect(hMgmtHandle as IntPtr ) as DWORD 
	PUBLIC STATIC METHOD AdsMgDumpInternalTables(hMgmtHandle as IntPtr ) as DWORD 
	PUBLIC STATIC METHOD AdsMgGetServerType(hMgmtHandle as IntPtr , pusServerType out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsMgKillUser(hMgmtHandle as IntPtr , strUserName as string , usConnNumber as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsMgResetCommStats(hMgmtHandle as IntPtr ) as DWORD 
	PUBLIC STATIC METHOD AdsNullTerminateStrings(bNullTerminate as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsOpenIndex(hTable as IntPtr , strName as string , [In] [Out] ahIndex as IntPtr[] , pusArrayLen ref WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsOpenTable(hConnect as IntPtr, strName as string , strAlias as string , usTableType as WORD, usCharType as WORD , usLockType as WORD , usCheckRights as WORD , ulOptions as DWORD, phTable out IntPtr ) as DWORD 
	PUBLIC STATIC METHOD AdsOpenTable90(hConnect as IntPtr, strName as string , strAlias as string , usTableType as WORD, usCharType as WORD , usLockType as WORD , usCheckRights as WORD , ulOptions as DWORD, strCollation as string , phTable out IntPtr ) as DWORD 
	PUBLIC STATIC METHOD AdsPrepareSQL(hStatement as IntPtr , strSQL as string ) as DWORD 
	PUBLIC STATIC METHOD AdsRegisterCallbackFunction(pfn as CallbackFn , ulCallBackID as DWORD ) as DWORD 
	PUBLIC STATIC METHOD AdsReindex(hObject as IntPtr) as DWORD 
	PUBLIC STATIC METHOD AdsReindex61(hObject as IntPtr, ulPageSize as DWORD ) as DWORD 
	PUBLIC STATIC METHOD AdsReindexFTS(hObject as IntPtr, ulPageSize as DWORD ) as DWORD 
	PUBLIC STATIC METHOD AdsRestructureTable(hObj as IntPtr , strName as string , strPassword as string , usTableType as WORD, usCharType as WORD , usLockType as WORD , usCheckRights as WORD , strAddFields as string , strDeleteFields as string , strChangeFields as string ) as DWORD 
	PUBLIC STATIC METHOD AdsRestructureTable90(hObj as IntPtr , strName as string , strPassword as string , usTableType as WORD, usCharType as WORD , usLockType as WORD , usCheckRights as WORD , strAddFields as string , strDeleteFields as string , strChangeFields as string , strCollation as string ) as DWORD 
	PUBLIC STATIC METHOD AdsRollbackTransaction80(hConnect as IntPtr, strSavepoint as string , ulOptions as DWORD) as DWORD 
	PUBLIC STATIC METHOD AdsSetBinary(hTable as IntPtr , lFieldOrdinal as DWORD, usBinaryType as WORD , ulTotalLength as DWORD , ulOffset as DWORD , strBuf as byte[] , ulLen as DWORD ) as DWORD 
	PUBLIC STATIC METHOD AdsSetBinary(hTable as IntPtr , strFldName as string , usBinaryType as WORD , ulTotalLength as DWORD , ulOffset as DWORD , strBuf as byte[] , ulLen as DWORD ) as DWORD 
	PUBLIC STATIC METHOD AdsSetCollation(hConnect as IntPtr, strCollation as string ) as DWORD 
	PUBLIC STATIC METHOD AdsSetCollationLang(strLang as string ) as DWORD 
	PUBLIC STATIC METHOD AdsSetDate(hObj as IntPtr , lFieldOrdinal as DWORD, strValue as string , wLen as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsSetDate(hObj as IntPtr , strFldName as string , strValue as string , wLen as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsSetDateFormat60(hConnect as IntPtr, strFormat as string ) as DWORD 
	PUBLIC STATIC METHOD AdsSetDefault(strDefault as string ) as DWORD 
	PUBLIC STATIC METHOD AdsSetDouble(hObj as IntPtr , lFieldOrdinal as DWORD, dValue as real8) as DWORD 
	PUBLIC STATIC METHOD AdsSetDouble(hObj as IntPtr , strFldName as string , dValue as Real8) as DWORD 
	PUBLIC STATIC METHOD AdsSetEmpty(hObj as IntPtr , lFieldOrdinal as DWORD) as DWORD 
	PUBLIC STATIC METHOD AdsSetEmpty(hObj as IntPtr , strFldName as string ) as DWORD 
	PUBLIC STATIC METHOD AdsSetExact22(hObj as IntPtr , bExact as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsSetField(hObj as IntPtr , lFieldOrdinal as DWORD, abBuf as byte[] , ulLen as DWORD ) as DWORD 
	PUBLIC STATIC METHOD AdsSetField(hObj as IntPtr , lFieldOrdinal as DWORD, strBuf as string , ulLen as DWORD ) as DWORD 
	PUBLIC STATIC METHOD AdsSetField(hObj as IntPtr , strFldName as string , abBuf as byte[] , ulLen as DWORD ) as DWORD 
	PUBLIC STATIC METHOD AdsSetField(hObj as IntPtr , strFldName as string , strBuf as string , ulLen as DWORD ) as DWORD 
	PUBLIC STATIC METHOD AdsSetHandleINT64(hObj as IntPtr , ulVal as DWORD ) as DWORD 
	PUBLIC STATIC METHOD AdsSetINT64(hObj as IntPtr , lFieldOrdinal as DWORD, lValue as int ) as DWORD 
	PUBLIC STATIC METHOD AdsSetINT64(hObj as IntPtr , strFldName as string , lValue as int ) as DWORD 
	PUBLIC STATIC METHOD AdsSetINT64INT64(hObj as IntPtr , lFieldOrdinal as DWORD, qValue as INT64 ) as DWORD 
	PUBLIC STATIC METHOD AdsSetINT64INT64(hObj as IntPtr , strFldName as string , qValue as INT64 ) as DWORD 
	PUBLIC STATIC METHOD AdsSetIndexDirection(hIndex as IntPtr ,  usReverseDirection as WORD) as DWORD 
	PUBLIC STATIC METHOD AdsSetJulian(hObj as IntPtr , lFieldOrdinal as DWORD, lDate as int) as DWORD 
	PUBLIC STATIC METHOD AdsSetJulian(hObj as IntPtr , strFldName as string , lDate as int ) as DWORD 
	PUBLIC STATIC METHOD AdsSetLogical(hObj as IntPtr , lFieldOrdinal as DWORD, bValue as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsSetLogical(hObj as IntPtr , strFldName as string , bValue as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsSetMilliseconds(hObj as IntPtr , lFieldOrdinal as DWORD, lTime as int ) as DWORD 
	PUBLIC STATIC METHOD AdsSetMilliseconds(hObj as IntPtr , strFldName as string , lTime as int ) as DWORD 
	PUBLIC STATIC METHOD AdsSetMoney(hObj as IntPtr , lFieldOrdinal as DWORD, qValue as INT64 ) as DWORD 
	PUBLIC STATIC METHOD AdsSetMoney(hObj as IntPtr , strFldName as string , qValue as INT64 ) as DWORD 
	PUBLIC STATIC METHOD AdsSetNull(hTable as IntPtr , lFieldOrdinal as DWORD) as DWORD 
	PUBLIC STATIC METHOD AdsSetNull(hTable as IntPtr , strFldName as string ) as DWORD 
	PUBLIC STATIC METHOD AdsSetRecord(hObj as IntPtr , strRec as byte[] , ulLen as DWORD ) as DWORD 
	PUBLIC STATIC METHOD AdsSetRelKeyPos(hIndex as IntPtr , dPos as Real8) as DWORD 
	PUBLIC STATIC METHOD AdsSetScopedRelation(hTableParent as IntPtr , hIndexChild as IntPtr , strExpr as string ) as DWORD 
	PUBLIC STATIC METHOD AdsSetSearchPath(strPath as string ) as DWORD 
	PUBLIC STATIC METHOD AdsSetShort(hObj as IntPtr , lFieldOrdinal as DWORD, sValue as short ) as DWORD 
	PUBLIC STATIC METHOD AdsSetShort(hObj as IntPtr , strFldName as string , sValue as short ) as DWORD 
	PUBLIC STATIC METHOD AdsSetString(hObj as IntPtr , lFieldOrdinal as DWORD, strBuf as string , ulLen as DWORD ) as DWORD 
	PUBLIC STATIC METHOD AdsSetString(hObj as IntPtr , strFldName as string , strBuf as string , ulLen as DWORD ) as DWORD 
	PUBLIC STATIC METHOD AdsSetTime(hObj as IntPtr , lFieldOrdinal as DWORD, strValue as string , wLen as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsSetTime(hObj as IntPtr , strFldName as string , strValue as string , wLen as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsSetTimeStamp(hObj as IntPtr , lFieldOrdinal as DWORD, strBuf as string , ulLen as DWORD ) as DWORD 
	PUBLIC STATIC METHOD AdsSetTimeStamp(hObj as IntPtr , strFldName as string , strBuf as string , ulLen as DWORD ) as DWORD 
	PUBLIC STATIC METHOD AdsShowError(strTitle as string ) as DWORD 
	PUBLIC STATIC METHOD AdsSkipUnique(hIndex as IntPtr , lRecs as int) as DWORD 
	PUBLIC STATIC METHOD AdsStmtConstrainUpdates(hStatement as IntPtr , usConstrain as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsStmtEnableEncryption(hStatement as IntPtr , strPassword as string ) as DWORD 
	PUBLIC STATIC METHOD AdsStmtReadAllColumns(hStatement as IntPtr , usReadColumns as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsStmtSetTableCharType(hStatement as IntPtr , usCharType as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsStmtSetTableCollation(hStatement as IntPtr , strCollation as string ) as DWORD 
	PUBLIC STATIC METHOD AdsStmtSetTableLockType(hStatement as IntPtr , usLockType as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsStmtSetTablePassword(hStatement as IntPtr , strTableName as string , strPassword as string ) as DWORD 
	PUBLIC STATIC METHOD AdsStmtSetTableReadOnly(hStatement as IntPtr , usReadOnly as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsStmtSetTableRights(hStatement as IntPtr , usCheckRights as WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsStmtSetTableType(hStatement as IntPtr , usTableType as WORD) as DWORD 
	PUBLIC STATIC METHOD AdsVerifyPassword(hTable as IntPtr , pusEnabled out WORD ) as DWORD 
	PUBLIC STATIC METHOD AdsVerifySQL(hStatement as IntPtr , strSQL as string ) as DWORD 
    */
    END CLASS 
        
    public delegate CallbackFn(usPercentDone as WORD, ulCallbackID as DWORD) as DWORD 
    public delegate CallbackFn101(usPercentDone AS Word, qCallbackID AS Int64) as DWord

END NAMESPACE





