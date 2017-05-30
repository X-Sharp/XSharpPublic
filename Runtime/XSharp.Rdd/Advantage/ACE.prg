//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System
USING System.Collections.Generic
USING System.Text
USING System.Runtime.InteropServices

BEGIN NAMESPACE AdvantageClientEngine
Class ACE
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

    public const DBI_GET_ACE_STMT_HANDLE  := DBI_USER  + 111 as int
    public const DBI_GET_ACE_TABLE_HANDLE  := DBI_USER  + 110 as int
    public const DBOI_AXS_PERCENT_INDEXED  := 1805 as int
    public const DBOI_GET_ACE_INDEX_HANDLE  := 1806 as int

    public const AX_BEGIN_TRANSACTION  := 1 as int
    public const AX_COMMIT_TRANSACTION  := 2 as int
    public const AX_ISACTIVE_TRANSACTION  := 4 as int
    public const AX_ROLLBACK_TRANSACTION  := 3 as int

    public const SET_USER  := 200 as int
    public const _SET_AXSLOCKING  := SET_USER +1 as int
    public const _SET_RIGHTSCHECKING  := SET_USER + 2 as int
    public const _SET_CONNECTION_HANDLE  := SET_USER +3 as int
    public const _SET_EXACTKEYPOS  := SET_USER +4 as int
    public const _SET_SQL_QUERY  := SET_USER +5 as int
    public const _SET_SQL_TABLE_PASSWORDS  := SET_USER +6 as int
    public const _SET_COLLATION_NAME  := SET_USER +7 as int
    public const DBFAXS_ADS_AIS_SERVER        := 4 as int
    public const DBFAXS_ADS_LOCAL_SERVER      := 1 as int
    public const DBFAXS_ADS_REMOTE_SERVER     := 2 as int

    #endregion Constants

		[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsAddCustomKey(hIndex as IntPtr ) as DWORD 

		[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsAppendRecord(hTable as IntPtr ) as DWORD 

		[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsApplicationExit() as DWORD 

		[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsAtBOF(hTable as IntPtr , pbBof out WORD ) as DWORD 

		[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsAtEOF(hTable as IntPtr , pbEof out WORD ) as DWORD 

		[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsBeginTransaction(hConnect as IntPtr) as DWORD 

		[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsBinaryToFile(hTable as IntPtr , pucFldName as string , pucFileName as string ) as DWORD 

		[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsBinaryToFile(hTable as IntPtr , lFieldOrdinal as DWORD, pucFileName as string ) as DWORD 

		[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsCacheOpenCursors(usOpen as WORD) as DWORD 

		[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsCacheOpenTables(usOpen as WORD) as DWORD 

		[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsCacheRecords(hTable as IntPtr , usNumRecords as WORD ) as DWORD 

		[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsCancelUpdate(hTable as IntPtr ) as DWORD 

		[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsCancelUpdate90(hTable as IntPtr , ulOptions as DWORD) as DWORD 

		[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsCheckExistence(hConnect as IntPtr, pucFileName as string , pusOnDisk out WORD) as DWORD 

		[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
		public static extern METHOD AdsClearAllScopes(hTable as IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsClearDefault() as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsClearFilter(hTable as IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsClearRelation(hTableParent as IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsClearScope(hIndex as IntPtr , usScopeOption as WORD) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsCloneTable(hTable as IntPtr , phClone out IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsCloseAllIndexes(hTable as IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsCloseAllTables() as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsCloseIndex(hIndex as IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsCloseTable(hTable as IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsCloseCachedTables(hConnection as IntPtr) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsCommitTransaction(hConnect as IntPtr) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsConnect(pucServerName as string , phConnect out IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsConnect26(pucServerName as string , usServerTypes as WORD, phConnect out IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsConnect60(pucServerPath as string , usServerTypes as WORD, pucUserName as string , pucPassword as string , ulOptions as DWORD, phConnect out IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsIsConnectionAlive(hConnect as IntPtr, pbConnectionIsAlive out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsContinue(hTable as IntPtr , pbFound out WORD) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsConvertTable(hObj as IntPtr ,  usFilterOption as WORD, pucFile as string , usTableType as WORD) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsCopyTable(hObj as IntPtr , usFilterOption as WORD , pucFile as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsCopyTableContents(hObjFrom as IntPtr , hTableTo as IntPtr , usFilterOption as WORD) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsCopyTableStructure(hTable as IntPtr , pucFile as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsCreateIndex(hObj as IntPtr , pucFileName as string , pucTag as string , pucExpr as string , pucCondition as string , pucWhile as string , ulOptions as DWORD, phIndex out IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsCreateIndex61(hObj as IntPtr , pucFileName as string , pucTag as string , pucExpr as string , pucCondition as string , pucWhile as string , ulOptions as DWORD, ulPageSize as DWORD , phIndex out IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsCreateIndex90(hObj as IntPtr , pucFileName as string , pucTag as string , pucExpr as string , pucCondition as string , pucWhile as string , ulOptions as DWORD, ulPageSize as DWORD , pucCollation as string , phIndex out IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsCreateFTSIndex(hTable as IntPtr , pucFileName as string , pucTag as string , pucField as string , ulPageSize as DWORD , ulMinWordLen as DWORD , ulMaxWordLen as DWORD , usUseDefaultDelim as WORD , pucDelimiters as string , usUseDefaultNoise as WORD , pucNoiseWords as string , usUseDefaultDrop as WORD , pucDropChars as string , usUseDefaultConditionals as WORD , pucConditionalChars as string , pucReserved1 as string , pucReserved2 as string , ulOptions as DWORD) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsCreateTable(hConnect as IntPtr, pucName as string , pucAlias as string , usTableType as WORD, usCharType as WORD , usLockType as WORD , usCheckRights as WORD , usMemoSize as WORD , pucFields as string , phTable out IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsCreateTable71(hConnect as IntPtr, pucName as string , pucDBObjName as string , usTableType as WORD, usCharType as WORD , usLockType as WORD , usCheckRights as WORD , usMemoSize as WORD , pucFields as string , ulOptions as DWORD, phTable out IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsCreateTable90(hConnect as IntPtr, pucName as string , pucDBObjName as string , usTableType as WORD, usCharType as WORD , usLockType as WORD , usCheckRights as WORD , usMemoSize as WORD , pucFields as string , ulOptions as DWORD, pucCollation as string , phTable out IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDCreate(pucDictionaryPath as string , usEncrypt as WORD, pucDescription as string , phDictionary out IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDCreateRefIntegrity(hDictionary AS IntPtr , pucRIName as string , pucFailTable as string , pucParentTableName as string , pucParentTagName as string , pucChildTableName as string , pucChildTagName as string , usUpdateRule as WORD , usDeleteRule as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDCreateRefIntegrity62(hDictionary AS IntPtr , pucRIName as string , pucFailTable as string , pucParentTableName as string , pucParentTagName as string , pucChildTableName as string , pucChildTagName as string , usUpdateRule as WORD , usDeleteRule as WORD , pucNoPrimaryError as string , pucCascadeError as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDRemoveRefIntegrity(hDictionary AS IntPtr , pucRIName as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDGetDatabaseProperty(hObject as IntPtr, usPropertyID as WORD, [In] [Out] pvProperty as byte[] , pusPropertyLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDGetDatabaseProperty(hObject as IntPtr, usPropertyID as WORD, [In] [Out] pucProperty as char[] , pusPropertyLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDGetDatabaseProperty(hObject as IntPtr, usPropertyID as WORD, pusProperty ref WORD , pusPropertyLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDGetFieldProperty(hObject as IntPtr, pucTableName as string , pucFieldName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , pusPropertyLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDGetFieldProperty(hObject as IntPtr, pucTableName as string , pucFieldName as string , usPropertyID as WORD, [In] [Out] pucProperty as char[] , pusPropertyLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDGetFieldProperty(hObject as IntPtr, pucTableName as string , pucFieldName as string , usPropertyID as WORD, pusProperty ref WORD , pusPropertyLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDGetIndexFileProperty(hObject as IntPtr, pucTableName as string , pucIndexFileName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , pusPropertyLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDGetIndexFileProperty(hObject as IntPtr, pucTableName as string , pucIndexFileName as string , usPropertyID as WORD, [In] [Out] pucProperty as char[] , pusPropertyLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDGetIndexFileProperty(hObject as IntPtr, pucTableName as string , pucIndexFileName as string , usPropertyID as WORD, pusProperty ref WORD , pusPropertyLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDGetIndexProperty(hObject as IntPtr, pucTableName as string , pucIndexName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , pusPropertyLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDGetIndexProperty(hObject as IntPtr, pucTableName as string , pucIndexName as string , usPropertyID as WORD, [In] [Out] pucProperty as char[] , pusPropertyLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDGetIndexProperty(hObject as IntPtr, pucTableName as string , pucIndexName as string , usPropertyID as WORD, pusProperty ref WORD , pusPropertyLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDGetLinkProperty(hConnect as IntPtr, pucLinkName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , pusPropertyLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDGetLinkProperty(hConnect as IntPtr, pucLinkName as string , usPropertyID as WORD, [In] [Out] pucProperty as char[] , pusPropertyLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDGetLinkProperty(hConnect as IntPtr, pucLinkName as string , usPropertyID as WORD, pusProperty ref WORD , pusPropertyLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDGetTableProperty(hObject as IntPtr, pucTableName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , pusPropertyLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDGetTableProperty(hObject as IntPtr, pucTableName as string , usPropertyID as WORD, [In] [Out] pucProperty as char[] , pusPropertyLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDGetTableProperty(hObject as IntPtr, pucTableName as string , usPropertyID as WORD, pusProperty ref WORD , pusPropertyLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDGetUserGroupProperty(hObject as IntPtr, pucUserGroupName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , pusPropertyLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDGetUserGroupProperty(hObject as IntPtr, pucUserGroupName as string , usPropertyID as WORD, [In] [Out] pucProperty as char[] , pusPropertyLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDGetUserGroupProperty(hObject as IntPtr, pucUserGroupName as string , usPropertyID as WORD, pusProperty ref WORD , pusPropertyLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDGetUserProperty(hObject as IntPtr, pucUserName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , pusPropertyLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDGetUserProperty(hObject as IntPtr, pucUserName as string , usPropertyID as WORD, [In] [Out] pucProperty as char[] , pusPropertyLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDGetUserProperty(hObject as IntPtr, pucUserName as string , usPropertyID as WORD, pusProperty ref WORD , pusPropertyLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDGetViewProperty(hObject as IntPtr, pucViewName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , pusPropertyLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDGetViewProperty(hObject as IntPtr, pucViewName as string , usPropertyID as WORD, [In] [Out] pucProperty as char[] , pusPropertyLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDGetViewProperty(hObject as IntPtr, pucViewName as string , usPropertyID as WORD, pusProperty ref WORD , pusPropertyLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDGetTriggerProperty(hObject as IntPtr, pucTriggerName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , pusPropertyLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDGetTriggerProperty(hObject as IntPtr, pucTriggerName as string , usPropertyID as WORD, [In] [Out] pucProperty as char[] , pusPropertyLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDGetTriggerProperty(hObject as IntPtr, pucTriggerName as string , usPropertyID as WORD, pusProperty ref WORD , pusPropertyLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDGetProcedureProperty(hObject as IntPtr, pucProcName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , pusPropertyLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDGetProcedureProperty(hObject as IntPtr, pucProcName as string , usPropertyID as WORD, [In] [Out] pucProperty as char[] , pusPropertyLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDGetProcedureProperty(hObject as IntPtr, pucProcName as string , usPropertyID as WORD, pusProperty ref WORD , pusPropertyLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDGetRefIntegrityProperty(hObject as IntPtr, pucRIName as string , usPropertyID as WORD, [In] [Out] pucProperty as char[] , pusPropertyLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDGetPermissions(hDBConn as IntPtr , pucGrantee as string , usObjectType as WORD, pucObjectName as string , pucParentName as string , usGetInherited as WORD , pulPermissions out DWORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDGrantPermission(hAdminConn as IntPtr , usObjectType as WORD, pucObjectName as string , pucParentName as string , pucGrantee as string , ulPermissions as DWORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDRevokePermission(hAdminConn as IntPtr , usObjectType as WORD, pucObjectName as string , pucParentName as string , pucGrantee as string , ulPermissions as DWORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDSetDatabaseProperty(hDictionary AS IntPtr , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , usPropertyLen as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDSetDatabaseProperty(hDictionary AS IntPtr , usPropertyID as WORD, [In] [Out] pucProperty as char[] , usPropertyLen as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDSetDatabaseProperty(hDictionary AS IntPtr , usPropertyID as WORD, pusProperty ref WORD , usPropertyLen as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDSetFieldProperty(hDictionary AS IntPtr , pucTableName as string , pucFieldName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , usPropertyLen as WORD , usValidateOption as WORD , pucFailTable as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDSetFieldProperty(hDictionary AS IntPtr , pucTableName as string , pucFieldName as string , usPropertyID as WORD, [In] [Out] pucProperty as char[] , usPropertyLen as WORD , usValidateOption as WORD , pucFailTable as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDSetFieldProperty(hDictionary AS IntPtr , pucTableName as string , pucFieldName as string , usPropertyID as WORD, pusProperty ref WORD , usPropertyLen as WORD , usValidateOption as WORD , pucFailTable as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDSetProcedureProperty(hDictionary AS IntPtr , pucProcedureName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , usPropertyLen as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDSetProcedureProperty(hDictionary AS IntPtr , pucProcedureName as string , usPropertyID as WORD, [In] [Out] pucProperty as char[] , usPropertyLen as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDSetProcedureProperty(hDictionary AS IntPtr , pucProcedureName as string , usPropertyID as WORD, pusProperty ref WORD , usPropertyLen as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDSetTableProperty(hDictionary AS IntPtr , pucTableName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , usPropertyLen as WORD , usValidateOption as WORD , pucFailTable as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDSetTableProperty(hDictionary AS IntPtr , pucTableName as string , usPropertyID as WORD, [In] [Out] pucProperty as char[] , usPropertyLen as WORD , usValidateOption as WORD , pucFailTable as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDSetTableProperty(hDictionary AS IntPtr , pucTableName as string , usPropertyID as WORD, pusProperty ref WORD , usPropertyLen as WORD , usValidateOption as WORD , pucFailTable as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDSetUserGroupProperty(hDictionary AS IntPtr , pucUserGroupName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , usPropertyLen as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDSetUserGroupProperty(hDictionary AS IntPtr , pucUserGroupName as string , usPropertyID as WORD, [In] [Out] pucProperty as char[] , usPropertyLen as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDSetUserGroupProperty(hDictionary AS IntPtr , pucUserGroupName as string , usPropertyID as WORD, pusProperty ref WORD , usPropertyLen as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDSetUserProperty(hDictionary AS IntPtr , pucUserName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , usPropertyLen as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDSetUserProperty(hDictionary AS IntPtr , pucUserName as string , usPropertyID as WORD, [In] [Out] pucProperty as char[] , usPropertyLen as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDSetUserProperty(hDictionary AS IntPtr , pucUserName as string , usPropertyID as WORD, pusProperty ref WORD , usPropertyLen as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDSetViewProperty(hDictionary AS IntPtr , pucViewName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , usPropertyLen as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDSetViewProperty(hDictionary AS IntPtr , pucViewName as string , usPropertyID as WORD, [In] [Out] pucProperty as char[] , usPropertyLen as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDSetViewProperty(hDictionary AS IntPtr , pucViewName as string , usPropertyID as WORD, pusProperty ref WORD , usPropertyLen as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDSetObjectAccessRights(hDictionary AS IntPtr , pucObjectName as string , pucAccessorName as string , pucAllowedAccess as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDAddProcedure(hDictionary AS IntPtr , pucName as string , pucContainer as string , pucProcName as string , ulInvokeOption as DWORD , pucInParams as string , pucOutParams as string , pucComments as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDAddTable(hDictionary AS IntPtr , pucTableName as string , pucTablePath as string , usTableType as WORD, usCharType as WORD , pucIndexFiles as string , pucComments as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDAddTable90(hDictionary AS IntPtr , pucTableName as string , pucTablePath as string , usTableType as WORD, usCharType as WORD , pucIndexFiles as string , pucComments as string , pucCollation as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDAddView(hDictionary AS IntPtr , pucName as string , pucComments as string , pucSQL as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDCreateTrigger(hDictionary AS IntPtr , pucName as string , pucTableName as string ,  ulTriggerType as DWORD, ulEventTypes as DWORD , ulContainerType as DWORD , pucContainer as string , pucFunctionName as string , ulPriority as DWORD , pucComments as string , ulOptions as DWORD) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDRemoveTrigger(hDictionary AS IntPtr , pucName as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDAddIndexFile(hDictionary AS IntPtr , pucTableName as string , pucIndexFilePath as string , pucComment as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDCreateUser(hDictionary AS IntPtr , pucGroupName as string , pucUserName as string , pucPassword as string , pucDescription as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDAddUserToGroup(hDictionary AS IntPtr , pucGroupName as string , pucUserName as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDRemoveUserFromGroup(hDictionary AS IntPtr , pucGroupName as string , pucUserName as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDDeleteUser(hDictionary AS IntPtr , pucUserName as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDCreateUserGroup(hDictionary AS IntPtr , pucGroupName as string , pucDescription as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDDeleteUserGroup(hDictionary AS IntPtr , pucGroupName as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDDeleteIndex(hDictionary AS IntPtr , pucTableName as string , pucIndexName as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDRemoveIndexFile(hDictionary AS IntPtr , pucTableName as string , pucIndexFileName as string , usDeleteFile as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDRemoveProcedure(hDictionary AS IntPtr , pucName as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDRemoveTable(hObject as IntPtr, pucTableName as string , usDeleteFiles as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDRemoveView(hDictionary AS IntPtr , pucName as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDRenameObject(hDictionary AS IntPtr , pucObjectName as string , pucNewObjectName as string , usObjectType as WORD, ulOptions as DWORD) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDMoveObjectFile(hDictionary AS IntPtr , usObjectType as WORD, pucObjectName as string , pucNewPath as string , pucIndexFiles as string , pucParent as string , ulOptions as DWORD) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDFindFirstObject(hObject as IntPtr, usFindObjectType as WORD , pucParentName as string , [In] [Out] pucObjectName as char[] , pusObjectNameLen ref WORD , phFindHandle out IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDFindNextObject(hObject as IntPtr, hFindHandle as IntPtr , [In] [Out] pucObjectName as char[] , pusObjectNameLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDFindClose(hObject as IntPtr, hFindHandle as IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDCreateLink(hDBConn as IntPtr , pucLinkAlias as string , pucLinkedDDPath as string , pucUserName as string , pucPassword as string , ulOptions as DWORD) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDModifyLink(hDBConn as IntPtr , pucLinkAlias as string , pucLinkedDDPath as string , pucUserName as string , pucPassword as string , ulOptions as DWORD) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDDropLink(hDBConn as IntPtr , pucLinkedDD as string , usDropGlobal as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDCreatePublication(hDictionary AS IntPtr , pucPublicationName as string , pucComments as string , ulOptions as DWORD) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDGetPublicationProperty(hObject as IntPtr, pucPublicationName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , pusPropertyLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDGetPublicationProperty(hObject as IntPtr, pucPublicationName as string , usPropertyID as WORD, [In] [Out] pucProperty as char[] , pusPropertyLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDGetPublicationProperty(hObject as IntPtr, pucPublicationName as string , usPropertyID as WORD, pusProperty ref WORD , pusPropertyLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDSetPublicationProperty(hDictionary AS IntPtr , pucPublicationName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , usPropertyLen as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDSetPublicationProperty(hDictionary AS IntPtr , pucPublicationName as string , usPropertyID as WORD, [In] [Out] pucProperty as char[] , usPropertyLen as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDSetPublicationProperty(hDictionary AS IntPtr , pucPublicationName as string , usPropertyID as WORD, pusProperty ref WORD , usPropertyLen as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDDeletePublication(hDictionary AS IntPtr , pucPublicationName as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDCreateArticle(hDictionary AS IntPtr , pucPublicationName as string , pucObjectName as string , pucRowIdentColumns as string , pucFilter as string , ulOptions as DWORD) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDGetArticleProperty(hObject as IntPtr, pucPublicationName as string , pucObjectName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , pusPropertyLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDGetArticleProperty(hObject as IntPtr, pucPublicationName as string , pucObjectName as string , usPropertyID as WORD, [In] [Out] pucProperty as char[] , pusPropertyLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDGetArticleProperty(hObject as IntPtr, pucPublicationName as string , pucObjectName as string , usPropertyID as WORD, pusProperty ref WORD , pusPropertyLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDSetArticleProperty(hDictionary AS IntPtr , pucPublicationName as string , pucObjectName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , usPropertyLen as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDSetArticleProperty(hDictionary AS IntPtr , pucPublicationName as string , pucObjectName as string , usPropertyID as WORD, [In] [Out] pucProperty as char[] , usPropertyLen as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDSetArticleProperty(hDictionary AS IntPtr , pucPublicationName as string , pucObjectName as string , usPropertyID as WORD, pusProperty ref WORD , usPropertyLen as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDDeleteArticle(hDictionary AS IntPtr , pucPublicationName as string , pucObjectName as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDCreateSubscription(hDictionary AS IntPtr , pucSubscriptionName as string , pucPublicationName as string , pucTarget as string , pucUser as string , pucPassword as string , pucReplicationQueue as string , usForward as WORD , pucComments as string , ulOptions as DWORD) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDGetSubscriptionProperty(hObject as IntPtr, pucSubscriptionName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , pusPropertyLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDGetSubscriptionProperty(hObject as IntPtr, pucSubscriptionName as string , usPropertyID as WORD, [In] [Out] pucProperty as char[] , pusPropertyLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDGetSubscriptionProperty(hObject as IntPtr, pucSubscriptionName as string , usPropertyID as WORD, pusProperty ref WORD , pusPropertyLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDSetSubscriptionProperty(hDictionary AS IntPtr , pucSubscriptionName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , usPropertyLen as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDSetSubscriptionProperty(hDictionary AS IntPtr , pucSubscriptionName as string , usPropertyID as WORD, [In] [Out] pucProperty as char[] , usPropertyLen as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDSetSubscriptionProperty(hDictionary AS IntPtr , pucSubscriptionName as string , usPropertyID as WORD, pusProperty ref WORD , usPropertyLen as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDDeleteSubscription(hDictionary AS IntPtr , pucSubscriptionName as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDecryptRecord(hTable as IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDecryptTable(hTable as IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDeleteCustomKey(hIndex as IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDeleteIndex(hIndex as IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDeleteRecord(hTable as IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetKeyColumn(hCursor as IntPtr , [In] [Out] pucKeyColumn as char[] , pusLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDisableEncryption(hTable as IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDisableLocalConnections() as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDisconnect(hConnect as IntPtr) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsEnableEncryption(hTable as IntPtr , pucPassword as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsEncryptRecord(hTable as IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsEncryptTable(hTable as IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsEvalLogicalExpr(hTable as IntPtr , pucExpr as string , pbResult out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsEvalNumericExpr(hTable as IntPtr , pucExpr as string , pdResult out double ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsEvalStringExpr(hTable as IntPtr , pucExpr as string , [In] [Out] pucResult as char[] , pusLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsEvalTestExpr(hTable as IntPtr , pucExpr as string , pusType out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsExtractKey(hIndex as IntPtr , [In] [Out] pucKey as char[] , pusLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsFailedTransactionRecovery(pucServer as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsFileToBinary(hTable as IntPtr , pucFldName as string , usBinaryType as WORD , pucFileName as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsFileToBinary(hTable as IntPtr , lFieldOrdinal as DWORD, usBinaryType as WORD , pucFileName as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsFindConnection(pucServerName as string , phConnect out IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsFindConnection25(pucFullPath as string , phConnect out IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsFindClose(hConnect as IntPtr, lHandle as IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsFindFirstTable(hConnect as IntPtr, pucFileMask as string , [In] [Out] pucFirstFile as char[] , pusFileLen ref WORD , plHandle out IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsFindNextTable(hConnect as IntPtr, lHandle as IntPtr , [In] [Out] pucFileName as char[] , pusFileLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsFindFirstTable62(hConnect as IntPtr, pucFileMask as string , [In] [Out] pucFirstDD as char[] , pusDDLen ref WORD , [In] [Out] pucFirstFile as char[] , pusFileLen ref WORD , plHandle out IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsFindNextTable62(hConnect as IntPtr, lHandle as IntPtr , [In] [Out] pucDDName as char[] , pusDDLen ref WORD , [In] [Out] pucFileName as char[] , pusFileLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetAllIndexes(hTable as IntPtr , [In] [Out] ahIndex as IntPtr[] , pusArrayLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetFTSIndexes(hTable as IntPtr , [In] [Out] ahIndex as IntPtr[] , pusArrayLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetAllLocks(hTable as IntPtr , [In] [Out] aulLocks as DWORD[] , pusArrayLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetAllTables([In] [Out] ahTable as IntPtr[] , pusArrayLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetBinary(hTable as IntPtr , pucFldName as string , ulOffset as DWORD , [In] [Out] pucBuf as byte[] , pulLen ref DWORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetBinary(hTable as IntPtr , lFieldOrdinal as DWORD, ulOffset as DWORD , [In] [Out] pucBuf as byte[] , pulLen ref DWORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetBinaryLength(hTable as IntPtr , pucFldName as string , pulLength out DWORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetBinaryLength(hTable as IntPtr , lFieldOrdinal as DWORD, pulLength out DWORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetBookmark(hTable as IntPtr , phBookmark out IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetBookmark60(hObj as IntPtr , [In] [Out] pucBookmark as char[] , pulLength ref DWORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetBookmarkLength(hObj as IntPtr , pulLength ref DWORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsCompareBookmarks(pucBookmark1 as string , pucBookmark2 as string , plResult out int ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetCollationLang([In] [Out] pucLang as char[] , pusLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetCollation(hConnect as IntPtr, [In] [Out] pucCollation as char[] , pusLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetConnectionType(hConnect as IntPtr, pusConnectType out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetConnectionPath(hConnect as IntPtr, [In] [Out] pucConnectionPath as char[] , pusLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetConnectionProperty(hConnect as IntPtr, usPropertyID as WORD, [In] [Out] pvProperty as byte[] , pulPropertyLen ref DWORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetDate(hTable as IntPtr , pucFldName as string , [In] [Out] pucBuf as char[] , pusLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetDate(hTable as IntPtr , lFieldOrdinal as DWORD, [In] [Out] pucBuf as char[] , pusLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetDateFormat([In] [Out] pucFormat as char[] , pusLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetDateFormat60(hConnect as IntPtr, [In] [Out] pucFormat as char[] , pusLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetDecimals(pusDecimals out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetDefault([In] [Out] pucDefault as char[] , pusLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetDeleted(pbUseDeleted out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetDouble(hTable as IntPtr , pucFldName as string , pdValue out double ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetDouble(hTable as IntPtr , lFieldOrdinal as DWORD, pdValue out double ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetEpoch(pusCentury out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetErrorString(ulErrCode as DWORD , [In] [Out] pucBuf as char[] , pusBufLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetExact(pbExact out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetExact22(hObj as IntPtr , pbExact out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetField(hTable as IntPtr , pucFldName as string , [In] [Out] pucBuf as char[] , pulLen ref DWORD , usOption as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetField(hTable as IntPtr , lFieldOrdinal as DWORD, [In] [Out] pucBuf as char[] , pulLen ref DWORD , usOption as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetField(hTable as IntPtr , pucFldName as string , [In] [Out] abBuf as byte[] , pulLen ref DWORD , usOption as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetField(hTable as IntPtr , lFieldOrdinal as DWORD, [In] [Out] abBuf as byte[] , pulLen ref DWORD , usOption as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetFieldDecimals(hTable as IntPtr , pucFldName as string , pusDecimals out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetFieldDecimals(hTable as IntPtr , lFieldOrdinal as DWORD, pusDecimals out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetFieldLength(hTable as IntPtr , pucFldName as string , pulLength out DWORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetFieldLength(hTable as IntPtr , lFieldOrdinal as DWORD, pulLength out DWORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetFieldName(hTable as IntPtr , usFld as WORD , [In] [Out] pucName as char[] , pusBufLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetFieldNum(hTable as IntPtr , pucFldName as string , pusNum out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetFieldNum(hTable as IntPtr , lFieldOrdinal as DWORD, pusNum out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetFieldOffset(hTable as IntPtr , pucFldName as string , pulOffset out DWORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetFieldOffset(hTable as IntPtr , lFieldOrdinal as DWORD, pulOffset out DWORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetFieldType(hTable as IntPtr , pucFldName as string , pusType out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetFieldType(hTable as IntPtr , lFieldOrdinal as DWORD, pusType out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetFilter(hTable as IntPtr , [In] [Out] pucFilter as char[] , pusLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetHandleINT64(hObj as IntPtr , pulVal out DWORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetHandleType(hObj as IntPtr , pusType out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetIndexCondition(hIndex as IntPtr , [In] [Out] pucExpr as char[] , pusLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetIndexExpr(hIndex as IntPtr , [In] [Out] pucExpr as char[] , pusLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetIndexFilename(hIndex as IntPtr , usOption as WORD , [In] [Out] pucName as char[] , pusLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetIndexHandle(hTable as IntPtr , pucIndexOrder as string , phIndex out IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetIndexHandleByOrder(hTable as IntPtr , usOrderNum as WORD , phIndex out IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetIndexHandleByExpr(hTable as IntPtr , pucExpr as string , ulDescending as DWORD , phIndex out IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetIndexName(hIndex as IntPtr , [In] [Out] pucName as char[] , pusLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetIndexOrderByHandle(hIndex as IntPtr , pusIndexOrder out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetJulian(hTable as IntPtr , pucFldName as string , plDate out int ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetJulian(hTable as IntPtr , lFieldOrdinal as DWORD, plDate out int ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetKeyCount(hIndex as IntPtr , usFilterOption as WORD, pulCount out DWORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetKeyNum(hIndex as IntPtr , usFilterOption as WORD, pulKey out DWORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetKeyLength(hIndex as IntPtr , pusKeyLength out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetKeyType(hIndex as IntPtr , usKeyType out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetLastError(pulErrCode out DWORD , [In] [Out] pucBuf as char[] , pusBufLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetLastTableUpdate(hTable as IntPtr , [In] [Out] pucDate as char[] , pusDateLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetLogical(hTable as IntPtr , pucFldName as string , pbValue out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetLogical(hTable as IntPtr , lFieldOrdinal as DWORD, pbValue out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetINT64(hTable as IntPtr , pucFldName as string , plValue out int ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetINT64(hTable as IntPtr , lFieldOrdinal as DWORD, plValue out int ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetINT64INT64(hTable as IntPtr , pucFldName as string , pqValue out INT64 ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetINT64INT64(hTable as IntPtr , lFieldOrdinal as DWORD, pqValue out INT64 ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetMemoBlockSize(hTable as IntPtr , pusBlockSize out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetMemoLength(hTable as IntPtr , pucFldName as string , pulLength out DWORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetMemoLength(hTable as IntPtr , lFieldOrdinal as DWORD, pulLength out DWORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetMemoDataType(hTable as IntPtr , pucFldName as string , pusType out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetMemoDataType(hTable as IntPtr , lFieldOrdinal as DWORD, pusType out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetMilliseconds(hTable as IntPtr , pucFldName as string , plTime out int ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetMilliseconds(hTable as IntPtr , lFieldOrdinal as DWORD, plTime out int ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetMoney(hTbl as IntPtr , pucFldName as string , pqValue out INT64 ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetMoney(hTbl as IntPtr , lFieldOrdinal as DWORD, pqValue out INT64 ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetActiveLinkInfo(hDBConn as IntPtr , usLinkNum as WORD , [In] [Out] pucLinkInfo as char[] , pusBufferLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetNumActiveLinks(hDBConn as IntPtr , pusNumLinks out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetNumFields(hTable as IntPtr , pusCount out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetNumIndexes(hTable as IntPtr , pusNum out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetNumFTSIndexes(hTable as IntPtr , pusNum out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetNumLocks(hTable as IntPtr , pusNum out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetNumOpenTables(pusNum out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetRecord(hTable as IntPtr , [In] [Out] pucRec as byte[] , pulLen ref DWORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetRecordCount(hTable as IntPtr , usFilterOption as WORD, pulCount out DWORD) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetRecordNum(hTable as IntPtr , usFilterOption as WORD, pulRec out DWORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetRecordLength(hTable as IntPtr , pulLength out DWORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetRecordCRC(hTable as IntPtr , pulCRC out DWORD , ulOptions as DWORD) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetRelKeyPos(hIndex as IntPtr , pdPos out double ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetScope(hIndex as IntPtr , usScopeOption as WORD, [In] [Out] pucScope as char[] , pusBufLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetSearchPath([In] [Out] pucPath as char[] , pusLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetServerName(hConnect as IntPtr, pucName as char[] , pusLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetServerTime(hConnect as IntPtr,  pucDateBuf as char[], pusDateBufLen ref WORD , plTime out int , [In] [Out] pucTimeBuf as char[] , pusTimeBufLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetShort(hTable as IntPtr , pucFldName as string , psValue out short ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetShort(hTable as IntPtr , lFieldOrdinal as DWORD, psValue out short ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetString(hTable as IntPtr , pucFldName as string , [In] [Out] pucBuf as char[] , pulLen ref DWORD , usOption as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetString(hTable as IntPtr , lFieldOrdinal as DWORD, [In] [Out] pucBuf as char[] , pulLen ref DWORD , usOption as WORD) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetTableAlias(hTable as IntPtr , [In] [Out] pucAlias as char[] , pusLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetTableCharType(hTable as IntPtr , pusCharType out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetTableConnection(hTable as IntPtr , phConnect out IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetTableFilename(hTable as IntPtr , usOption as WORD , [In] [Out] pucName as char[] , pusLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetTableHandle(pucName as string , phTable out IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetTableHandle25(hConnect as IntPtr, pucName as string , phTable out IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetTableLockType(hTable as IntPtr , pusLockType out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetTableMemoSize(hTable as IntPtr , pusMemoSize out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetTableOpenOptions(hTable as IntPtr , pulOptions out DWORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetTableRights(hTable as IntPtr , pusRights out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetTableType(hTable as IntPtr , pusType out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetTime(hTable as IntPtr , pucFldName as string , [In] [Out] pucBuf as char[] , pusLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetTime(hTable as IntPtr , lFieldOrdinal as DWORD, [In] [Out] pucBuf as char[] , pusLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetVersion(pulMajor out DWORD , pulMinor out DWORD , pucLetter as string , [In] [Out] pucDesc as char[] , pusDescLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGotoBookmark(hTable as IntPtr , hBookmark as IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGotoBookmark60(hObj as IntPtr , pucBookmark as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGotoBottom(hObj as IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGotoRecord(hTable as IntPtr , ulRec as DWORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGotoTop(hObj as IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsImageToClipboard(hTable as IntPtr , pucFldName as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsInTransaction(hConnect as IntPtr, pbInTrans out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsIsEmpty(hTable as IntPtr , pucFldName as string , pbEmpty out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsIsEmpty(hTable as IntPtr , lFieldOrdinal as DWORD, pbEmpty out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsIsExprValid(hTable as IntPtr , pucExpr as string , pbValid out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsIsFound(hObj as IntPtr , pbFound out WORD) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsIsIndexCompound(hIndex as IntPtr , pbCompound out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsIsIndexCandidate(hIndex as IntPtr , pbCandidate out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsIsIndexNullable(hIndex as IntPtr , pbNullable out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsIsIndexCustom(hIndex as IntPtr , pbCustom out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsIsIndexDescending(hIndex as IntPtr , pbDescending out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsIsIndexPrimaryKey(hIndex as IntPtr , pbPrimaryKey out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsIsIndexFTS(hIndex as IntPtr , pbFTS out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsIsIndexUnique(hIndex as IntPtr , pbUnique out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsIsRecordDeleted(hTable as IntPtr , pbDeleted out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsIsRecordEncrypted(hTable as IntPtr , pbEncrypted out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsIsRecordLocked(hTable as IntPtr , ulRec as DWORD , pbLocked out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsIsRecordVisible(hObj as IntPtr , pbVisible out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsIsServerLoaded(pucServer as string , pbLoaded out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsIsTableEncrypted(hTable as IntPtr , pbEncrypted out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsIsTableLocked(hTable as IntPtr , pbLocked out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsLocate(hTable as IntPtr , pucExpr as string , bForward as WORD , pbFound out WORD) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsLockRecord(hTable as IntPtr , ulRec as DWORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsLockTable(hTable as IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsLookupKey(hIndex as IntPtr , pucKey as string , usKeyLen AS WORD, usDataType as WORD, pbFound out WORD) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsMgConnect(pucServerName as string , pucUserName as string , pucPassword as string , phMgmtHandle out IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsMgDisconnect(hMgmtHandle as IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsMgResetCommStats(hMgmtHandle as IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsMgDumpInternalTables(hMgmtHandle as IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsMgGetServerType(hMgmtHandle as IntPtr , pusServerType out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsMgKillUser(hMgmtHandle as IntPtr , pucUserName as string , usConnNumber as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsNullTerminateStrings(bNullTerminate as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsOpenIndex(hTable as IntPtr , pucName as string , [In] [Out] ahIndex as IntPtr[] , pusArrayLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsOpenTable(hConnect as IntPtr, pucName as string , pucAlias as string , usTableType as WORD, usCharType as WORD , usLockType as WORD , usCheckRights as WORD , ulOptions as DWORD, phTable out IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsOpenTable90(hConnect as IntPtr, pucName as string , pucAlias as string , usTableType as WORD, usCharType as WORD , usLockType as WORD , usCheckRights as WORD , ulOptions as DWORD, pucCollation as string , phTable out IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsPackTable(hTable as IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsRecallRecord(hTable as IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsRecallAllRecords(hTable as IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsRefreshRecord(hTable as IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsClearProgressCallback() as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsRegisterCallbackFunction(pfn as CallbackFn , ulCallBackID as DWORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsClearCallbackFunction() as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsReindex(hObject as IntPtr) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsReindex61(hObject as IntPtr, ulPageSize as DWORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsReindexFTS(hObject as IntPtr, ulPageSize as DWORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsResetConnection(hConnect as IntPtr) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsRollbackTransaction(hConnect as IntPtr) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSeek(hIndex as IntPtr , pucKey as string , usKeyLen AS WORD, usDataType as WORD, usSeekType as WORD, pbFound out WORD) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSeek(hIndex as IntPtr , abKey as byte[] , usKeyLen AS WORD, usDataType as WORD, usSeekType as WORD, pbFound out WORD) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSeekLast(hIndex as IntPtr , pucKey as string , usKeyLen AS WORD, usDataType as WORD, pbFound out WORD) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSeekLast(hIndex as IntPtr , abKey as byte[] , usKeyLen AS WORD, usDataType as WORD, pbFound out WORD) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetBinary(hTable as IntPtr , pucFldName as string , usBinaryType as WORD , ulTotalLength as DWORD , ulOffset as DWORD , pucBuf as byte[] , ulLen as DWORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetBinary(hTable as IntPtr , lFieldOrdinal as DWORD, usBinaryType as WORD , ulTotalLength as DWORD , ulOffset as DWORD , pucBuf as byte[] , ulLen as DWORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetCollationLang(pucLang as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetCollation(hConnect as IntPtr, pucCollation as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetDate(hObj as IntPtr , pucFldName as string , pucValue as string , usLen as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetDate(hObj as IntPtr , lFieldOrdinal as DWORD, pucValue as string , usLen as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetDateFormat(pucFormat as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetDateFormat60(hConnect as IntPtr, pucFormat as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetDecimals(usDecimals as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetDefault(pucDefault as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsShowDeleted(bShowDeleted as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetDouble(hObj as IntPtr , pucFldName as string , dValue as Real8) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetDouble(hObj as IntPtr , lFieldOrdinal as DWORD, dValue as real8) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetEmpty(hObj as IntPtr , pucFldName as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetEmpty(hObj as IntPtr , lFieldOrdinal as DWORD) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetEpoch(usCentury as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetExact(bExact as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetExact22(hObj as IntPtr , bExact as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetField(hObj as IntPtr , pucFldName as string , pucBuf as string , ulLen as DWORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetField(hObj as IntPtr , lFieldOrdinal as DWORD, pucBuf as string , ulLen as DWORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetField(hObj as IntPtr , pucFldName as string , abBuf as byte[] , ulLen as DWORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetField(hObj as IntPtr , lFieldOrdinal as DWORD, abBuf as byte[] , ulLen as DWORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetFilter(hTable as IntPtr , pucFilter as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetHandleINT64(hObj as IntPtr , ulVal as DWORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetJulian(hObj as IntPtr , pucFldName as string , lDate as int ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetJulian(hObj as IntPtr , lFieldOrdinal as DWORD, lDate as int) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetLogical(hObj as IntPtr , pucFldName as string , bValue as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetLogical(hObj as IntPtr , lFieldOrdinal as DWORD, bValue as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetINT64(hObj as IntPtr , pucFldName as string , lValue as int ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetINT64(hObj as IntPtr , lFieldOrdinal as DWORD, lValue as int ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetINT64INT64(hObj as IntPtr , pucFldName as string , qValue as INT64 ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetINT64INT64(hObj as IntPtr , lFieldOrdinal as DWORD, qValue as INT64 ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetMilliseconds(hObj as IntPtr , pucFldName as string , lTime as int ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetMilliseconds(hObj as IntPtr , lFieldOrdinal as DWORD, lTime as int ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetMoney(hObj as IntPtr , pucFldName as string , qValue as INT64 ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetMoney(hObj as IntPtr , lFieldOrdinal as DWORD, qValue as INT64 ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetRecord(hObj as IntPtr , pucRec as byte[] , ulLen as DWORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetRelation(hTableParent as IntPtr , hIndexChild as IntPtr , pucExpr as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetRelKeyPos(hIndex as IntPtr , dPos as Real8) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetScope(hIndex as IntPtr , usScopeOption as WORD, pucScope as string , usScopeLen as WORD , usDataType as WORD) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetScope(hIndex as IntPtr , usScopeOption as WORD, abScope as byte[] , usScopeLen as WORD , usDataType as WORD) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetScopedRelation(hTableParent as IntPtr , hIndexChild as IntPtr , pucExpr as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetSearchPath(pucPath as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetServerType(usServerOptions as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetShort(hObj as IntPtr , pucFldName as string , sValue as short ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetShort(hObj as IntPtr , lFieldOrdinal as DWORD, sValue as short ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetString(hObj as IntPtr , pucFldName as string , pucBuf as string , ulLen as DWORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetString(hObj as IntPtr , lFieldOrdinal as DWORD, pucBuf as string , ulLen as DWORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetTime(hObj as IntPtr , pucFldName as string , pucValue as string , usLen as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetTime(hObj as IntPtr , lFieldOrdinal as DWORD, pucValue as string , usLen as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsShowError(pucTitle as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSkip(hObj as IntPtr , lRecs as int) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSkipUnique(hIndex as IntPtr , lRecs as int) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsThreadExit() as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsUnlockRecord(hTable as IntPtr , ulRec as DWORD) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsUnlockTable(hTable as IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsVerifyPassword(hTable as IntPtr , pusEnabled out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsIsEncryptionEnabled(hTable as IntPtr , pusEnabled out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsWriteAllRecords() as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsWriteRecord(hTable as IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsZapTable(hTable as IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetAOF(hTable as IntPtr , pucFilter as string , usOptions as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsEvalAOF(hTable as IntPtr , pucFilter as string , pusOptLevel out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsClearAOF(hTable as IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsRefreshAOF(hTable as IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetAOF(hTable as IntPtr , [In] [Out] pucFilter as char[] , pusLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetAOFOptLevel(hTable as IntPtr , pusOptLevel out WORD , [In] [Out] pucNonOpt as char[] , pusLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsIsRecordInAOF(hTable as IntPtr , ulRecordNum as DWORD , pusIsInAOF out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsCustomizeAOF(hTable as IntPtr , ulNumRecords as DWORD , pulRecords out DWORD , usOption as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsInitRawKey(hIndex as IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsBuildRawKey(hIndex as IntPtr , [In] [Out] pucKey as byte[] , pusKeyLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsCreateSQLStatement(hConnect as IntPtr, phStatement out IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsPrepareSQL(hStatement as IntPtr , pucSQL as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsExecuteSQL(hStatement as IntPtr , phCursor out IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsExecuteSQLDirect(hStatement as IntPtr , pucSQL as string , phCursor out IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsCloseSQLStatement(hStatement as IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsStmtSetTableRights(hStatement as IntPtr , usCheckRights as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsStmtSetTableReadOnly(hStatement as IntPtr , usReadOnly as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsStmtSetTableLockType(hStatement as IntPtr , usLockType as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsStmtSetTableCharType(hStatement as IntPtr , usCharType as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsStmtSetTableType(hStatement as IntPtr , usTableType as WORD) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsStmtSetTableCollation(hStatement as IntPtr , pucCollation as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsStmtConstrainUpdates(hStatement as IntPtr , usConstrain as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsStmtEnableEncryption(hStatement as IntPtr , pucPassword as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsStmtDisableEncryption(hStatement as IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsStmtSetTablePassword(hStatement as IntPtr , pucTableName as string , pucPassword as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsStmtClearTablePasswords(hStatement as IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsStmtReadAllColumns(hStatement as IntPtr , usReadColumns as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsClearSQLParams(hStatement as IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetTimeStamp(hObj as IntPtr , pucFldName as string , pucBuf as string , ulLen as DWORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetTimeStamp(hObj as IntPtr , lFieldOrdinal as DWORD, pucBuf as string , ulLen as DWORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsClearSQLAbortFunc() as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetNumParams(hStatement as IntPtr , pusNumParams out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetLastAutoinc(hObj as IntPtr , pulAutoIncVal out DWORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsIsIndexUserDefined(hIndex as IntPtr , pbUserDefined out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsRestructureTable(hObj as IntPtr , pucName as string , pucPassword as string , usTableType as WORD, usCharType as WORD , usLockType as WORD , usCheckRights as WORD , pucAddFields as string , pucDeleteFields as string , pucChangeFields as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsRestructureTable90(hObj as IntPtr , pucName as string , pucPassword as string , usTableType as WORD, usCharType as WORD , usLockType as WORD , usCheckRights as WORD , pucAddFields as string , pucDeleteFields as string , pucChangeFields as string , pucCollation as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetSQLStatementHandle(hCursor as IntPtr , phStmt out IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetSQLStatement(hStmt as IntPtr , [In] [Out] pucSQL as char[] , pusLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsFlushFileBuffers(hTable as IntPtr ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDDeployDatabase(pucDestination as string , pucDestinationPassword as string , pucSource as string , pucSourcePassword as string , usServerTypes as WORD, usValidateOption as WORD , usBackupFiles as WORD , ulOptions as DWORD) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsVerifySQL(hStatement as IntPtr , pucSQL as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDisableUniqueEnforcement(hConnect as IntPtr) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsEnableUniqueEnforcement(hConnect as IntPtr) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDisableRI(hConnect as IntPtr) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsEnableRI(hConnect as IntPtr) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDisableAutoIncEnforcement(hConnection as IntPtr) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsEnableAutoIncEnforcement(hConnection as IntPtr) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsRollbackTransaction80(hConnect as IntPtr, pucSavepoint as string , ulOptions as DWORD) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsCreateSavepoint(hConnect as IntPtr, pucSavepoint as string , ulOptions as DWORD) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDFreeTable(pucTableName as string , pucPassword as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDSetIndexProperty(hAdminConn as IntPtr , pucTableName as string , pucIndexName as string , usPropertyID as WORD, [In] [Out] pvProperty as byte[] , usPropertyLen as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDSetIndexProperty(hAdminConn as IntPtr , pucTableName as string , pucIndexName as string , usPropertyID as WORD, [In] [Out] pucProperty as char[] , usPropertyLen as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsDDSetIndexProperty(hAdminConn as IntPtr , pucTableName as string , pucIndexName as string , usPropertyID as WORD, pusProperty ref WORD , usPropertyLen as WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsIsFieldBinary(hTable as IntPtr , pucFldName as string , pbBinary out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsIsFieldBinary(hTable as IntPtr , lFieldOrdinal as DWORD, pbBinary out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsIsNull(hTable as IntPtr , pucFldName as string , pbNull out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsIsNull(hTable as IntPtr , lFieldOrdinal as DWORD, pbNull out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsIsNullable(hTable as IntPtr , pucFldName as string , pbNullable out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsIsNullable(hTable as IntPtr , lFieldOrdinal as DWORD, pbNullable out WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetNull(hTable as IntPtr , pucFldName as string ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetNull(hTable as IntPtr , lFieldOrdinal as DWORD) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetTableCollation(hTbl as IntPtr , [In] [Out] pucCollation as char[] , pusLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetIndexCollation(hIndex as IntPtr , [In] [Out] pucCollation as char[] , pusLen ref WORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetDataLength(hTable as IntPtr , pucFldName as string , ulOptions as DWORD, pulLength out DWORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsGetDataLength(hTable as IntPtr , lFieldOrdinal as DWORD, ulOptions as DWORD, pulLength out DWORD ) as DWORD 

	[DllImport("ace32.dll", CharSet := CharSet.Ansi)];
	public static extern METHOD AdsSetIndexDirection(hIndex as IntPtr ,  usReverseDirection as WORD) as DWORD 
    END CLASS 
    
    public delegate CallbackFn(usPercentDone as WORD, ulCallbackID as DWORD) as DWORD 

END NAMESPACE





