//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

BEGIN NAMESPACE XSharp
    /// <summary>Error code Enum that matches the Visual Objecs and Vulcan Generic Error Codes</summary>
    /// <note type="tip">
    /// These enums are also available as DEFINES and can therefore also be used without the "Gencode." prefix.
    /// </note>
    ENUM Gencode AS DWORD
        MEMBER EG_UNKNOWN      := 0
        MEMBER EG_ARG          := 1
        MEMBER EG_BOUND        := 2
        MEMBER EG_STROVERFLOW  := 3
        MEMBER EG_NUMOVERFLOW  := 4
        MEMBER EG_ZERODIV      := 5
        MEMBER EG_NUMERR       := 6
        MEMBER EG_SYNTAX       := 7
        MEMBER EG_COMPLEXITY   := 8
        MEMBER EG_MEMOVERFLOW  := 9
        MEMBER EG_SEQUENCE	   := 10
        MEMBER EG_MEM          := 11
        MEMBER EG_NOFUNC       := 12
        MEMBER EG_NOMETHOD     := 13
        MEMBER EG_NOVAR        := 14
        MEMBER EG_NOALIAS      := 15
        MEMBER EG_NOVARMETHOD  := 16
        MEMBER EG_BADALIAS     := 17
        MEMBER EG_DUPALIAS     := 18
        MEMBER EG_NULLVAR      := 19
        MEMBER EG_CREATE       := 20
        MEMBER EG_OPEN         := 21
        MEMBER EG_CLOSE        := 22
        MEMBER EG_READ         := 23
        MEMBER EG_WRITE        := 24
        MEMBER EG_PRINT        := 25
        MEMBER EG_NOATOM       := 26
        MEMBER EG_NOCLASS      := 27
        MEMBER EG_WRONGCLASS   := 28
        MEMBER EG_REFERENCE    := 29
        MEMBER EG_UNSUPPORTED  := 30
        MEMBER EG_LIMIT        := 31
        MEMBER EG_CORRUPTION   := 32
        MEMBER EG_DATATYPE     := 33
        MEMBER EG_DATAWIDTH    := 34
        MEMBER EG_NOTABLE      := 35
        MEMBER EG_NOORDER      := 36
        MEMBER EG_SHARED       := 37
        MEMBER EG_UNLOCKED     := 38
        MEMBER EG_READONLY     := 39
        MEMBER EG_APPENDLOCK   := 40
        MEMBER EG_LOCK		   := 41
        MEMBER EG_TOOMANYTHREADS := 42
        MEMBER EG_DB           := 43
        MEMBER EG_RESERVED_44  := 44
        MEMBER EG_LOCK_ERROR   := 45
        MEMBER EG_LOCK_TIMEOUT := 46
        MEMBER EG_STACK        := 47
        MEMBER EG_EVALSTACK    := 48
        MEMBER EG_ERRORBLOCK   := 49
        MEMBER EG_PROTECTION   := 50

        MEMBER EG_BADPTR       := 51
        MEMBER EG_BADPAGEFAULT := 52
        MEMBER EG_ERRORBUILD   := 53
        MEMBER EG_DYNPTR       := 54

        // Vulcan errors
        MEMBER EG_INACCESSIBLETYPE := 55
        MEMBER EG_AMBIGUOUSMETHOD  := 56
        MEMBER EG_SEND_MISSINGARG  := 57
        MEMBER EG_SEND_TOOMANYARGS := 58
        MEMBER EG_EXCEPTION		   := 59
        MEMBER EG_NOACCESS         := 60
        // Last Member
        MEMBER EG_MAX          := 60

    END ENUM
    /// <summary>Error code Enum that matches the XSharp Error Severity codesFDesc</summary>
    /// <note type="tip">
    /// These enums are also available as DEFINES and can therefore also be used without the "Severity." prefix.
    /// </note>
    ENUM Severity
        MEMBER ES_WHOCARES     := 0
        MEMBER ES_WARNING      := 1
        MEMBER ES_ERROR        := 2
        MEMBER ES_CATASTROPHIC := 3
    END ENUM

    /// <summary>Error code Enum that matches the XSharp Error Sub Codes</summary>
    /// <note type="tip">
    /// These enums are also available as DEFINES and can therefore also be used without the "Subcodes." prefix.
    /// </note>
    ENUM Subcodes
        MEMBER ENOERROR := 0
        // Database errors - HOST side
        // Synchronize these error with the VOErrors enum for lookup of errors in the stringtables
        MEMBER EDB		:= 1000
        MEMBER EDB_SEEK := EDB + 1
        MEMBER EDB_SKIP :=EDB + 2
        MEMBER EDB_GOTO := EDB + 3
        MEMBER EDB_SETRELATION := EDB + 4
        MEMBER EDB_USE := EDB + 5
        MEMBER EDB_CREATEINDEX := EDB + 6
        MEMBER EDB_SETORDER := EDB + 7
        MEMBER EDB_SETINDEX := EDB + 8
        MEMBER EDB_FIELDNAME := EDB + 9
        MEMBER EDB_BADALIAS := EDB + 10
        MEMBER EDB_DUPALIAS := EDB + 11
        MEMBER EDB_SETFILTER := EDB + 12
        MEMBER EDB_CYCLICREL := EDB + 13
        MEMBER EDB_CREATETABLE := EDB + 14
        MEMBER EDB_RDDNOTFOUND := EDB + 15
        // 16
        MEMBER EDB_FIELDINDEX := EDB + 17
        MEMBER EDB_SELECT := EDB + 18
        MEMBER EDB_SYMSELECT := EDB + 19
        MEMBER EDB_TOTAL := EDB + 20
        MEMBER EDB_RECNO := EDB + 21
        MEMBER EDB_EXPRESSION := EDB + 22
        MEMBER EDB_EXPR_WIDTH := EDB + 23
        // 24-29
        MEMBER EDB_DRIVERLOAD := EDB + 30
        MEMBER EDB_PARAM := EDB + 31
        MEMBER EDB_NOAREAS := EDB + 32
        MEMBER EDB_NOMEM := EDB + 33
        MEMBER EDB_NOFIELDS := EDB + 35
        MEMBER EDB_BAD_ERROR_INFO := EDB + 36
        MEMBER EDB_WRONGFIELDNAME := EDB + 37
        MEMBER EDB_ORDDESTROY := EDB + 38
        MEMBER EDB_NOINITFUNCTION := EDB + 39
        MEMBER EDB_ERRORINIT := EDB + 40
        MEMBER EDB_DBSTRUCT := EDB + 41
        // 42-49
        // Generic no table error
        MEMBER EDB_NOTABLE := EDB + 50
        MEMBER EDB_NOORDER := EDB + 51
        MEMBER EDB_NODB    := EDB + 52
        MEMBER EDB_ASSERTION := EDB + 53



        MEMBER ERDD             := 1100
        MEMBER ERDD_OPEN_FILE   := ERDD + 01
        MEMBER ERDD_OPEN_MEMO   := ERDD + 02
        MEMBER ERDD_OPEN_ORDER  := ERDD + 03
        MEMBER ERDD_CREATE_FILE := ERDD + 04
        MEMBER ERDD_CREATE_MEMO := ERDD + 05
        MEMBER ERDD_CREATE_ORDER := ERDD + 06
        // 07-09
        MEMBER ERDD_READ := ERDD + 10
        MEMBER ERDD_WRITE := ERDD + 11
        MEMBER ERDD_CORRUPT := ERDD + 12
        MEMBER ERDD_CORRUPT_HEADER := ERDD + 13
        // 14
        MEMBER ERDD_RDDVERSION := ERDD + 15
        // 16-19
        MEMBER ERDD_DATATYPE := ERDD + 20
        MEMBER ERDD_DATAWIDTH := ERDD + 21
        MEMBER ERDD_UNLOCKED := ERDD + 22
        MEMBER ERDD_SHARED := ERDD + 23
        MEMBER ERDD_APPENDLOCK := ERDD + 24
        MEMBER ERDD_READONLY := ERDD + 25
        MEMBER ERDD_NULLKEY := ERDD + 26
        MEMBER ERDD_NTXLIMIT := ERDD + 27
        MEMBER ERDD_TAGLIMIT := ERDD + 28
        // 29
        MEMBER ERDD_INIT_LOCK := ERDD + 30
        MEMBER ERDD_READ_LOCK := ERDD + 31
        MEMBER ERDD_WRITE_LOCK := ERDD + 32
        MEMBER ERDD_READ_UNLOCK := ERDD + 33
        MEMBER ERDD_WRITE_UNLOCK := ERDD + 34
        MEMBER ERDD_READ_LOCK_TIMEOUT := ERDD + 35
        MEMBER ERDD_WRITE_LOCK_TIMEOUT := ERDD + 36
        MEMBER ERDD_READ_UNLOCK_TIMEOUT := ERDD + 37
        MEMBER ERDD_WRITE_UNLOCK_TIMEOUT := ERDD + 38
        // 39
        MEMBER ERDD_CLOSE_FILE := ERDD + 40
        MEMBER ERDD_CLOSE_MEMO := ERDD + 41
        MEMBER ERDD_CLOSE_ORDER := ERDD + 42

        MEMBER ERDD_INVALID_ORDER := ERDD + 50
        MEMBER ERDD_RECNO_MISSING := ERDD + 51
        MEMBER ERDD_NOORDER := ERDD + 52
        MEMBER ERDD_UNSUPPORTED := ERDD + 53
        // 54-99
        // Following errors are known as Clipper's internal errors
        MEMBER ERDD_WRITE_NTX := ERDD + 100
        MEMBER ERDD_KEY_NOT_FOUND := ERDD + 101
        MEMBER ERDD_KEY_EVAL := ERDD + 102
        MEMBER ERDD_VAR_TYPE := ERDD + 103
        // 104-105
        MEMBER ERDD_READ_BUFFER := ERDD + 106
        MEMBER ERDD_SORT_INIT := ERDD + 107
        MEMBER ERDD_SORT_ADVANCE := ERDD + 108
        MEMBER ERDD_SORT_SORT := ERDD + 109
        MEMBER ERDD_SORT_COMPLETE := ERDD + 110
        MEMBER ERDD_SORT_END := ERDD + 111
        MEMBER ERDD_READ_TEMPFILE := ERDD + 112
        MEMBER ERDD_STREAM_NUM := ERDD + 113
        MEMBER ERDD_CREATE_TEMPFILE := ERDD + 114
        // 115
        MEMBER DISKIO_WRITE := ERDD + 116
        MEMBER ERDD_WRONG_DRIVERTYPE := ERDD + 117
        MEMBER ERDD_OUTOFMEMORY := ERDD + 118
        MEMBER ERDD_INVALIDARG := ERDD + 119
        MEMBER ERDD_NOEVENTHANDLER := ERDD + 120

        // Start of user definable errors
        MEMBER ERDD_USER := 2000

        // These were originally in the CMX code
        MEMBER ERDD_SCOPETYPEMISMATCH := 8007     // ScopeTypeMismatch
        MEMBER ERDD_NOTCUSTOM := 8008     // NotCustom
        MEMBER ERDD_INVALIDRECORDLIST := 9001      // InvalidRecordList

    END ENUM
END NAMESPACE

#region Gencode Defines
/// <exclude />

DEFINE EG_UNKNOWN        := Gencode.EG_UNKNOWN
/// <exclude />
DEFINE EG_ARG            := Gencode.EG_ARG
/// <exclude />
DEFINE EG_BOUND          := Gencode.EG_BOUND
/// <exclude />
DEFINE EG_STROVERFLOW    := Gencode.EG_STROVERFLOW
/// <exclude />
DEFINE EG_NUMOVERFLOW    := Gencode.EG_NUMOVERFLOW
/// <exclude />
DEFINE EG_ZERODIV        := Gencode.EG_ZERODIV
/// <exclude />
DEFINE EG_NUMERR         := Gencode.EG_NUMERR
/// <exclude />
DEFINE EG_SYNTAX         := Gencode.EG_SYNTAX
/// <exclude />
DEFINE EG_COMPLEXITY     := Gencode.EG_COMPLEXITY
/// <exclude />
DEFINE EG_MEMOVERFLOW    := Gencode.EG_MEMOVERFLOW
/// <exclude />
DEFINE EG_SEQUENCE       := Gencode.EG_SEQUENCE
/// <exclude />
DEFINE EG_MEM            := Gencode.EG_MEM
/// <exclude />
DEFINE EG_NOFUNC         := Gencode.EG_NOFUNC
/// <exclude />
DEFINE EG_NOMETHOD       := Gencode.EG_NOMETHOD
/// <exclude />
DEFINE EG_NOVAR          := Gencode.EG_NOVAR
/// <exclude />
DEFINE EG_NOALIAS        := Gencode.EG_NOALIAS
/// <exclude />
DEFINE EG_NOVARMETHOD    := Gencode.EG_NOVARMETHOD
/// <exclude />
DEFINE EG_BADALIAS       := Gencode.EG_BADALIAS
/// <exclude />
DEFINE EG_DUPALIAS       := Gencode.EG_DUPALIAS
/// <exclude />
DEFINE EG_NULLVAR        := Gencode.EG_NULLVAR
/// <exclude />
DEFINE EG_CREATE         := Gencode.EG_CREATE
/// <exclude />
DEFINE EG_OPEN           := Gencode.EG_OPEN
/// <exclude />
DEFINE EG_CLOSE          := Gencode.EG_CLOSE
/// <exclude />
DEFINE EG_READ           := Gencode.EG_READ
/// <exclude />
DEFINE EG_WRITE          := Gencode.EG_WRITE
/// <exclude />
DEFINE EG_PRINT          := Gencode.EG_PRINT
/// <exclude />
DEFINE EG_NOATOM         := Gencode.EG_NOATOM
/// <exclude />
DEFINE EG_NOCLASS        := Gencode.EG_NOCLASS
/// <exclude />
DEFINE EG_WRONGCLASS     := Gencode.EG_WRONGCLASS
/// <exclude />
DEFINE EG_REFERENCE      := Gencode.EG_REFERENCE
/// <exclude />
DEFINE EG_UNSUPPORTED    := Gencode.EG_UNSUPPORTED
/// <exclude />
DEFINE EG_LIMIT          := Gencode.EG_LIMIT
/// <exclude />
DEFINE EG_CORRUPTION     := Gencode.EG_CORRUPTION
/// <exclude />
DEFINE EG_DATATYPE       := Gencode.EG_DATATYPE
/// <exclude />
DEFINE EG_DATAWIDTH      := Gencode.EG_DATAWIDTH
/// <exclude />
DEFINE EG_NOTABLE        := Gencode.EG_NOTABLE
/// <exclude />
DEFINE EG_NOORDER        := Gencode.EG_NOORDER
/// <exclude />
DEFINE EG_SHARED         := Gencode.EG_SHARED
/// <exclude />
DEFINE EG_UNLOCKED       := Gencode.EG_UNLOCKED
/// <exclude />
DEFINE EG_READONLY       := Gencode.EG_READONLY
/// <exclude />
DEFINE EG_APPENDLOCK     := Gencode.EG_APPENDLOCK
/// <exclude />
DEFINE EG_LOCK           := Gencode.EG_LOCK
/// <exclude />
DEFINE EG_LOCK_ERROR     := Gencode.EG_LOCK_ERROR
/// <exclude />
DEFINE EG_LOCK_TIMEOUT   := Gencode.EG_LOCK_TIMEOUT
/// <exclude />
DEFINE EG_STACK          := Gencode.EG_STACK
/// <exclude />
DEFINE EG_EVALSTACK      := Gencode.EG_EVALSTACK
/// <exclude />
DEFINE EG_ERRORBLOCK     := Gencode.EG_ERRORBLOCK
/// <exclude />
DEFINE EG_PROTECTION     := Gencode.EG_PROTECTION
/// <exclude />
DEFINE EG_BADPTR         := Gencode.EG_BADPTR
/// <exclude />
DEFINE EG_BADPAGEFAULT   := Gencode.EG_BADPAGEFAULT
/// <exclude />
DEFINE EG_ERRORBUILD     := Gencode.EG_ERRORBUILD
/// <exclude />
DEFINE EG_DYNPTR         := Gencode.EG_DYNPTR
/// <exclude />
DEFINE EG_INACCESSIBLETYPE	:= Gencode.EG_INACCESSIBLETYPE
/// <exclude />
DEFINE EG_AMBIGUOUSMETHOD	:= Gencode.EG_AMBIGUOUSMETHOD
/// <exclude />
DEFINE EG_SEND_MISSINGARG	:= Gencode.EG_SEND_MISSINGARG
/// <exclude />
DEFINE EG_SEND_TOOMANYARGS	:= Gencode.EG_SEND_TOOMANYARGS
/// <exclude />
DEFINE EG_EXCEPTION			:= Gencode.EG_EXCEPTION
/// <exclude />
DEFINE EG_DB                 := Gencode.EG_DB
/// <exclude />
DEFINE EG_NOACCESS			:= Gencode.EG_NOACCESS
/// <exclude />

DEFINE EG_MAX            := Gencode.EG_MAX


#endregion


#region Severity Defines
/// <exclude />
DEFINE ES_WHOCARES     := Severity.ES_WHOCARES
/// <exclude />
DEFINE ES_WARNING      := Severity.ES_WARNING
/// <exclude />
DEFINE ES_ERROR        := Severity.ES_ERROR
/// <exclude />
DEFINE ES_CATASTROPHIC := Severity.ES_CATASTROPHIC

#endregion


#region Other error defines

DEFINE E_BREAK		            := (0xFFFF)
DEFINE E_RETRY		            := (1)
DEFINE E_DEFAULT	            := (0)
DEFINE E_EXCEPTION              := 5333L
DEFINE E_ACCESSVIOLATION        := (5333L)
DEFINE E_DATATYPE_MISALIGNMENT  := (5334L)
DEFINE E_SINGLE_STEP            := (5335L)
DEFINE E_ARRAY_BOUNDS_EXCEEDED  := (5336L)
DEFINE E_FLT_DENORMAL_OPERAND   := (5337L)
DEFINE E_FLT_DIVIDE_BY_ZERO     := (5338L)
DEFINE E_FLT_INEXACT_RESULT     := (5339L)
DEFINE E_FLT_INVALID_OPERATION  := (5340L)
DEFINE E_FLT_OVERFLOW           := (5341L)
DEFINE E_FLT_STACK_CHECK        := (5342L)
DEFINE E_FLT_UNDERFLOW          := (5343L)
DEFINE E_INT_DIVIDE_BY_ZERO     := (5344L)
DEFINE E_INT_OVERFLOW           := (5345L)
DEFINE E_PRIV_INSTRUCTION       := (5346L)
DEFINE E_ILLEGAL_INSTRUCTION    := (5347L)
DEFINE E_NONCONTINUABLE_EXCEPTION := (5348L)
DEFINE E_STACK_OVERFLOW         := (5349L)
DEFINE E_INVALID_DISPOSITION    := (5350L)
DEFINE E_GUARD_PAGE             := (5351L)
#endregion
