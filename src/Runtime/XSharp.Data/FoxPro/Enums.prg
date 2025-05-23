
/// <summary>List of error codes used in the XSharp VTP support. <br />
/// Each error code represents a translatable string in the string table inside XSharp.Core.
/// </summary>


DELEGATE VFPErrorMessage( resid AS DWORD , args PARAMS OBJECT[]) AS STRING
GLOBAL oVFPErrorMessage AS VFPErrorMessage

ENUM XSharp.VFPErrors
    // Make sure that the member names do not collide with the member names inside the VOErrors Enum
    MEMBER VFP_BASE   := 1
    MEMBER VFP_INVALID_DATE
    MEMBER VFP_INVALID_MONTH
    MEMBER VFP_INVALID_FORMAT
    MEMBER VFP_INVALID_DB_OBJECT
    MEMBER VFP_INVALID_DB_PROPERTY_NAME
    MEMBER VFP_INVALID_FIELD_SPEC
    MEMBER VFP_INVALID_FIELDNAME
    MEMBER VFP_STATEMENT_HANDLE_INVALID
    MEMBER VFP_STATEMENT_HANDLE_NOTSHARED
    MEMBER VFP_COMMAND_PARAMETER_REQUIRED
    MEMBER VFP_SQLCOLUMNS_CTYPE
    MEMBER VFP_PROPERTY_UNKNOWN
    MEMBER VFP_INVALID_RANGE
    MEMBER VFP_WITH_STACK_EMPTY
    MEMBER VFP_VARIABLE_NOT_ARRAY
    MEMBER VFP_VARIABLE_DOES_NOT_EXIST
    MEMBER VFP_ONE_DIM_NO_COLUMNS
    MEMBER VFP_ATTRIBUTE_OUT_OF_RANGE
    MEMBER VFP_PROPERTY_NOT_FOUND
    MEMBER VFP_MULTI_DIM_EXPECTED
    MEMBER VFP_SUBARRAY_TOO_SMALL
    MEMBER VFP_COLLATION_NOT_FOUND
    MEMBER VFP_COLLECTION_INVALIDKEYSORT
    MEMBER VFP_COLLECTION_INVALID_BEFORE_AFTER
    MEMBER VFP_COLLECTION_MISSING_KEY
    MEMBER VFP_COLLECTION_DUPLICATE_KEY
    MEMBER VFP_COLLECTION_NO_KEY_ALLOWED
    MEMBER VFP_COLLECTION_INCORRECT_INDEX
    MEMBER VFP_COLLECTION_MEMBER_NOT_FOUND
    MEMBER VFP_COLLECTION_NOT_INDEXED_BY_KEY
    MEMBER VFP_COLLECTION_INCORRECT_KEYTYPE
    MEMBER VFP_COLLECTION_INCORRECT_KEYSORT
    MEMBER VFP_INVALID_PARAMETER
END ENUM
