//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

begin namespace XSharp
	/// <summary>Error code Enum that matches the Visual Objecs and Vulcan Generic Error Codes</summary>
	/// <note type="tip">
	/// These enums are also available as DEFINES and can therefore also be used without the "Gencode." prefix.
	/// </note>
	enum Gencode as dword
		member EG_UNKNOWN      := 0
		member EG_ARG          := 1
		member EG_BOUND        := 2
		member EG_STROVERFLOW  := 3
		member EG_NUMOVERFLOW  := 4
		member EG_ZERODIV      := 5
		member EG_NUMERR       := 6
		member EG_SYNTAX       := 7
		member EG_COMPLEXITY   := 8
		member EG_MEMOVERFLOW  := 9
		member EG_SEQUENCE	   := 10
		member EG_MEM          := 11
		member EG_NOFUNC       := 12
		member EG_NOMETHOD     := 13
		member EG_NOVAR        := 14
		member EG_NOALIAS      := 15
		member EG_NOVARMETHOD  := 16
		member EG_BADALIAS     := 17
		member EG_DUPALIAS     := 18
		member EG_NULLVAR      := 19
		member EG_CREATE       := 20
		member EG_OPEN         := 21
		member EG_CLOSE        := 22
		member EG_READ         := 23
		member EG_WRITE        := 24
		member EG_PRINT        := 25
		member EG_NOATOM       := 26
		member EG_NOCLASS      := 27
		member EG_WRONGCLASS   := 28
		member EG_REFERENCE    := 29
		member EG_UNSUPPORTED  := 30
		member EG_LIMIT        := 31
		member EG_CORRUPTION   := 32
		member EG_DATATYPE     := 33
		member EG_DATAWIDTH    := 34
		member EG_NOTABLE      := 35
		member EG_NOORDER      := 36
		member EG_SHARED       := 37
		member EG_UNLOCKED     := 38
		member EG_READONLY     := 39
		member EG_APPENDLOCK   := 40
		member EG_LOCK			:= 41
		// missing 42 .. 44                        
		member EG_LOCK_ERROR   := 45
		member EG_LOCK_TIMEOUT := 46
		member EG_STACK        := 47
		member EG_EVALSTACK    := 48
		member EG_ERRORBLOCK   := 49
		member EG_PROTECTION   := 50
		
		member EG_BADPTR       := 51
		member EG_BADPAGEFAULT := 52
		member EG_ERRORBUILD   := 53
		member EG_DYNPTR       := 54
		
		// Vulcan errors
		member EG_INACCESSIBLETYPE := 55
		member EG_AMBIGUOUSMETHOD  := 56
		member EG_SEND_MISSINGARG  := 57
		member EG_SEND_TOOMANYARGS := 58
		member EG_EXCEPTION		   := 59
		
		// Last Member                        
		member EG_MAX          := 59
		
	END ENUM
	/// <summary>Error code Enum that matches the Visual Objecs and Vulcan Error Severity codes</summary>
	/// <note type="tip">
	/// These enums are also available as DEFINES and can therefore also be used without the "Severity." prefix.
	/// </note>
	enum Severity
		member ES_WHOCARES     := 0
		member ES_WARNING      := 1
		member ES_ERROR        := 2
		member ES_CATASTROPHIC := 3
	end enum 
	
	/// <summary>Error code Enum that matches the Visual Objecs and Vulcan Error Sub Codes</summary>
	/// <note type="tip">
	/// These enums are also available as DEFINES and can therefore also be used without the "Subcodes." prefix.
	/// </note>
	enum Subcodes
		member ENOERROR := 0
		// Database errors - HOST side
		
		member EDB		:= 1000
		member EDB_SEEK := EDB + 1
		member EDB_SKIP :=EDB + 2
		member EDB_GOTO := EDB + 3
		member EDB_SETRELATION := EDB + 4
		member EDB_USE := EDB + 5
		member EDB_CREATEINDEX := EDB + 6
		member EDB_SETORDER := EDB + 7
		member EDB_SETINDEX := EDB + 8
		member EDB_FIELDNAME := EDB + 9
		member EDB_BADALIAS := EDB + 10
		member EDB_DUPALIAS := EDB + 11
		member EDB_SETFILTER := EDB + 12
		member EDB_CYCLICREL := EDB + 13
		member EDB_CREATETABLE := EDB + 14
		member EDB_RDDNOTFOUND := EDB + 15
		// 16
		member EDB_FIELDINDEX := EDB + 17
		member EDB_SELECT := EDB + 18
		member EDB_SYMSELECT := EDB + 19
		member EDB_TOTAL := EDB + 20
		member EDB_RECNO := EDB + 21
		member EDB_EXPRESSION := EDB + 22
		member EDB_EXPR_WIDTH := EDB + 23
		// 24-29		
		member EDB_DRIVERLOAD := EDB + 30
		member EDB_PARAM := EDB + 31
		member EDB_NOAREAS := EDB + 32
		member EDB_NOMEM := EDB + 33
		member EDB_NOFIELDS := EDB + 35
		member EDB_BAD_ERROR_INFO := EDB + 36
		member EDB_WRONGFIELDNAME := EDB + 37
		member EDB_ORDDESTROY := EDB + 38
		member EDB_NOINITFUNCTION := EDB + 39
		member EDB_ERRORINIT := EDB + 40
		member EDB_DBSTRUCT := EDB + 41
		// 42-49
		// Generic no table error
		member EDB_NOTABLE := EDB + 50
		member EDB_NOORDER := EDB + 51
		member EDB_ASSERTION := EDB + 53
		


		member ERDD := 1100
		
		member ERDD_OPEN_ORDER := ERDD + 03
		// 02		
		member ERDD_CREATE_FILE := ERDD + 04
		member ERDD_CREATE_MEMO := ERDD + 05
		member ERDD_CREATE_ORDER := ERDD + 06
		// 07-09
		member ERDD_READ := ERDD + 10
		member ERDD_WRITE := ERDD + 11
		member ERDD_CORRUPT := ERDD + 12
		member ERDD_CORRUPT_HEADER := ERDD + 13
		// 14		
		member ERDD_RDDVERSION := ERDD + 15
		// 16-19
		member ERDD_DATATYPE := ERDD + 20
		member ERDD_DATAWIDTH := ERDD + 21
		member ERDD_UNLOCKED := ERDD + 22
		member ERDD_SHARED := ERDD + 23
		member ERDD_APPENDLOCK := ERDD + 24
		member ERDD_READONLY := ERDD + 25
		member ERDD_NULLKEY := ERDD + 26
		member ERDD_NTXLIMIT := ERDD + 27
		member ERDD_TAGLIMIT := ERDD + 28
		// 29
		member ERDD_INIT_LOCK := ERDD + 30
		member ERDD_READ_LOCK := ERDD + 31
		member ERDD_WRITE_LOCK := ERDD + 32
		member ERDD_READ_UNLOCK := ERDD + 33
		member ERDD_WRITE_UNLOCK := ERDD + 34
		member ERDD_READ_LOCK_TIMEOUT := ERDD + 35
		member ERDD_WRITE_LOCK_TIMEOUT := ERDD + 36
		member ERDD_READ_UNLOCK_TIMEOUT := ERDD + 37
		member ERDD_WRITE_UNLOCK_TIMEOUT := ERDD + 38
		// 39
		member ERDD_CLOSE_FILE := ERDD + 40
		member ERDD_CLOSE_MEMO := ERDD + 41
		member ERDD_CLOSE_ORDER := ERDD + 42
		
		member ERDD_INVALID_ORDER := ERDD + 50
		member ERDD_RECNO_MISSING := ERDD + 51
		member ERDD_NOORDER := ERDD + 52
		member ERDD_UNSUPPORTED := ERDD + 53
		// 54-99
		// Following errors are known as Clipper's internal errors
		member ERDD_WRITE_NTX := ERDD + 100
		member ERDD_KEY_NOT_FOUND := ERDD + 101
		member ERDD_KEY_EVAL := ERDD + 102
		member ERDD_VAR_TYPE := ERDD + 103
		// 104-105		
		member ERDD_READ_BUFFER := ERDD + 106
		member ERDD_SORT_INIT := ERDD + 107
		member ERDD_SORT_ADVANCE := ERDD + 108
		member ERDD_SORT_SORT := ERDD + 109
		member ERDD_SORT_COMPLETE := ERDD + 110
		member ERDD_SORT_END := ERDD + 111
		member ERDD_READ_TEMPFILE := ERDD + 112
		member ERDD_STREAM_NUM := ERDD + 113
		member ERDD_CREATE_TEMPFILE := ERDD + 114
		// 115		
		member DISKIO_WRITE := ERDD + 116
		member ERDD_WRONG_DRIVERTYPE := ERDD + 117
		member ERDD_OUTOFMEMORY := ERDD + 118
		member ERDD_INVALIDARG := ERDD + 119
		member ERDD_NOEVENTHANDLER := ERDD + 120
				
		// Start of user definable errors
		member ERDD_USER := 2000
		
		// These were originally in the CMX code
		member ERDD_SCOPETYPEMISMATCH := 8007     // ScopeTypeMismatch
		member ERDD_NOTCUSTOM := 8008     // NotCustom
		member ERDD_INVALIDRECORDLIST := 9001      // InvalidRecordList
		
	end enum
end namespace

#region Gencode Defines
	
	define EG_UNKNOWN        := GenCode.EG_UNKNOWN
	define EG_ARG            := GenCode.EG_ARG          
	define EG_BOUND          := GenCode.EG_BOUND        
	define EG_STROVERFLOW    := GenCode.EG_STROVERFLOW  
	define EG_NUMOVERFLOW    := GenCode.EG_NUMOVERFLOW  
	define EG_ZERODIV        := GenCode.EG_ZERODIV      
	define EG_NUMERR         := GenCode.EG_NUMERR       
	define EG_SYNTAX         := GenCode.EG_SYNTAX       
	define EG_COMPLEXITY     := GenCode.EG_COMPLEXITY   
	define EG_MEMOVERFLOW    := GenCode.EG_MEMOVERFLOW  
	define EG_SEQUENCE       := GenCode.EG_SEQUENCE     
	define EG_MEM            := GenCode.EG_MEM          
	define EG_NOFUNC         := GenCode.EG_NOFUNC       
	define EG_NOMETHOD       := GenCode.EG_NOMETHOD     
	define EG_NOVAR          := GenCode.EG_NOVAR        
	define EG_NOALIAS        := GenCode.EG_NOALIAS      
	define EG_NOVARMETHOD    := GenCode.EG_NOVARMETHOD  
	define EG_BADALIAS       := GenCode.EG_BADALIAS     
	define EG_DUPALIAS       := GenCode.EG_DUPALIAS     
	define EG_NULLVAR        := GenCode.EG_NULLVAR      
	define EG_CREATE         := GenCode.EG_CREATE       
	define EG_OPEN           := GenCode.EG_OPEN         
	define EG_CLOSE          := GenCode.EG_CLOSE        
	define EG_READ           := GenCode.EG_READ         
	define EG_WRITE          := GenCode.EG_WRITE        
	define EG_PRINT          := GenCode.EG_PRINT        
	define EG_NOATOM         := GenCode.EG_NOATOM       
	define EG_NOCLASS        := GenCode.EG_NOCLASS      
	define EG_WRONGCLASS     := GenCode.EG_WRONGCLASS   
	define EG_REFERENCE      := GenCode.EG_REFERENCE    
	define EG_UNSUPPORTED    := GenCode.EG_UNSUPPORTED  
	define EG_LIMIT          := GenCode.EG_LIMIT        
	define EG_CORRUPTION     := GenCode.EG_CORRUPTION   
	define EG_DATATYPE       := GenCode.EG_DATATYPE     
	define EG_DATAWIDTH      := GenCode.EG_DATAWIDTH    
	define EG_NOTABLE        := GenCode.EG_NOTABLE      
	define EG_NOORDER        := GenCode.EG_NOORDER      
	define EG_SHARED         := GenCode.EG_SHARED       
	define EG_UNLOCKED       := GenCode.EG_UNLOCKED     
	define EG_READONLY       := GenCode.EG_READONLY     
	define EG_APPENDLOCK     := GenCode.EG_APPENDLOCK   
	define EG_LOCK           := GenCode.EG_LOCK         
	define EG_LOCK_ERROR     := GenCode.EG_LOCK_ERROR   
	define EG_LOCK_TIMEOUT   := GenCode.EG_LOCK_TIMEOUT 
	define EG_STACK          := GenCode.EG_STACK        
	define EG_EVALSTACK      := GenCode.EG_EVALSTACK    
	define EG_ERRORBLOCK     := GenCode.EG_ERRORBLOCK   
	define EG_PROTECTION     := GenCode.EG_PROTECTION   
	define EG_BADPTR         := GenCode.EG_BADPTR       
	define EG_BADPAGEFAULT   := GenCode.EG_BADPAGEFAULT 
	define EG_ERRORBUILD     := GenCode.EG_ERRORBUILD   
	define EG_DYNPTR         := GenCode.EG_DYNPTR     
	define EG_INACCESSIBLETYPE	:= GenCode.EG_INACCESSIBLETYPE
	define EG_AMBIGUOUSMETHOD	:= GenCode.EG_AMBIGUOUSMETHOD 
	define EG_SEND_MISSINGARG	:= GenCode.EG_SEND_MISSINGARG 
	define EG_SEND_TOOMANYARGS	:= GenCode.EG_SEND_TOOMANYARGS 
	define EG_EXCEPTION			:= GenCode.EG_EXCEPTION 
	
	define EG_MAX            := GenCode.EG_MAX    
	
	
#endregion


#region Severity Defines
	define ES_WHOCARES     := Severity.ES_WHOCARES    
	define ES_WARNING      := Severity.ES_WARNING     
	define ES_ERROR        := Severity.ES_ERROR       
	define ES_CATASTROPHIC := Severity.ES_CATASTROPHIC
	
#endregion
