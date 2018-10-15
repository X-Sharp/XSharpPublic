VOSTRUCT u1_win
	MEMBER LowPart AS DWORD
	MEMBER HighPart AS DWORD


VOSTRUCT LARGE_INTEGER_WIN			// RvdH 070411 added
	MEMBER LowPart  AS DWORD  
	MEMBER HighPart AS LONGINT
VOSTRUCT  _WINULARGE_INTEGER ALIGN 4
	MEMBER LowPart  AS DWORD
	MEMBER HighPart AS DWORD


VOSTRUCT _WINLUID               // RvdH 070411 added
	MEMBER LowPart 	AS DWORD
	MEMBER HighPart	AS LONGINT

VOSTRUCT _winLIST_ENTRY
	MEMBER Flink AS _winLIST_ENTRY
	MEMBER Blink AS _winLIST_ENTRY


VOSTRUCT _winSINGLE_LIST_ENTRY
	MEMBER structNext AS _winSINGLE_LIST_ENTRY



VOSTRUCT _WINGUID
	MEMBER Data1 AS DWORD
	MEMBER Data2 AS WORD
	MEMBER Data3 AS WORD
	MEMBER DIM Data4[8] AS BYTE



VOSTRUCT  _WINOBJECTID
	MEMBER Lineage IS _WINGUID
	MEMBER Uniquifier AS DWORD


VOSTRUCT _WINCONTEXT

	//
	// The flags values within this flag control the contents of
	// a CONTEXT record.
	//
	// If the context record is used as an input parameter, then
	// for each portion of the context record controlled by a flag
	// whose value is set, it is assumed that that portion of the
	// context record contains valid context. If the context record
	// is being used to modify a threads context, then only that
	// portion of the threads context will be modified.
	//
	// If the context record is used as an IN OUT parameter to capture
	// the context of a thread, then only those portions of the thread's
	// context corresponding to set flags will be returned.
	//
	// The context record is never used as an OUT only parameter.
	//

	MEMBER ContextFlags AS DWORD

	//
	// This section is specified/returned if CONTEXT_DEBUG_REGISTERS is
	// set in ContextFlags.  Note that CONTEXT_DEBUG_REGISTERS is NOT
	// included in CONTEXT_FULL.
	//

	MEMBER Dr0 AS DWORD
	MEMBER Dr1 AS DWORD
	MEMBER Dr2 AS DWORD
	MEMBER Dr3 AS DWORD
	MEMBER Dr6 AS DWORD
	MEMBER Dr7 AS DWORD

	//
	// This section is specified/returned if the
	// ContextFlags word contians the flag CONTEXT_FLOATING_POINT.
	//

	MEMBER FloatSave IS _winFLOATING_SAVE_AREA

	//
	// This section is specified/returned if the
	// ContextFlags word contians the flag CONTEXT_SEGMENTS.
	//

	MEMBER SegGs AS DWORD
	MEMBER SegFs AS DWORD
	MEMBER SegEs AS DWORD
	MEMBER SegDs AS DWORD

	//
	// This section is specified/returned if the
	// ContextFlags word contians the flag CONTEXT_INTEGER.
	//

	MEMBER Edi AS DWORD
	MEMBER Esi AS DWORD
	MEMBER Ebx AS DWORD
	MEMBER Edx AS DWORD
	MEMBER Ecx AS DWORD
	MEMBER Eax AS DWORD

	//
	// This section is specified/returned if the
	// ContextFlags word contians the flag CONTEXT_CONTROL.
	//

	MEMBER Ebp AS DWORD
	MEMBER Eip AS DWORD
	MEMBER SegCs AS DWORD              // MUST BE SANITIZED
	MEMBER EFlags AS DWORD             // MUST BE SANITIZED
	MEMBER Esp AS DWORD
	MEMBER SegSs AS DWORD   
	//
	// This section is specified/returned if the ContextFlags word
	// contains the flag CONTEXT_EXTENDED_REGISTERS.
	// The format and contexts are processor specific
	//
	// RvdH 070411 added
	MEMBER DIM ExtendedRegisters[MAXIMUM_SUPPORTED_EXTENSION] AS BYTE






VOSTRUCT _WINFLOATING_SAVE_AREA
	MEMBER ControlWord AS DWORD
	MEMBER StatusWord AS DWORD
	MEMBER TagWord AS DWORD
	MEMBER ErrorOffset AS DWORD
	MEMBER ErrorSelector AS DWORD
	MEMBER DataOffset AS DWORD
	MEMBER DataSelector AS DWORD
	MEMBER DIM RegisterArea[SIZE_OF_80387_REGISTERS] AS BYTE
	MEMBER Cr0NpxState AS DWORD
























































































































VOSTRUCT _WINEXCEPTION_RECORD

	MEMBER ExceptionCode AS DWORD

	MEMBER ExceptionFlags AS DWORD
	MEMBER ExceptionRecord AS _winEXCEPTION_RECORD
	MEMBER ExceptionAddress AS PTR
	MEMBER NumberParameters AS DWORD
	MEMBER DIM ExceptionInformation[EXCEPTION_MAXIMUM_PARAMETERS] AS DWORD



VOSTRUCT _WINEXCEPTION_POINTERS
	MEMBER ExceptionRecord AS _WINEXCEPTION_RECORD
	MEMBER ContextRecord AS _WINCONTEXT

VOSTRUCT _WINQUOTA_LIMITS
	MEMBER PagedPoolLimit AS DWORD
	MEMBER NonPagedPoolLimit AS DWORD
	MEMBER MinimumWorkingSetSize AS DWORD
	MEMBER MaximumWorkingSetSize AS DWORD
	MEMBER PagefileLimit AS DWORD
	MEMBER TimeLimit IS _WINLARGE_INTEGER  

VOSTRUCT _WINMEMORY_BASIC_INFORMATION
	MEMBER BaseAddress AS PTR
	MEMBER AllocationBase AS PTR
	MEMBER AllocationProtect AS DWORD
	MEMBER RegionSize AS DWORD
	MEMBER State AS DWORD
	MEMBER _Protect AS DWORD
	MEMBER Type AS DWORD

VOSTRUCT _WINGENERIC_MAPPING
	MEMBER GenericRead AS DWORD
	MEMBER GenericWrite AS DWORD
	MEMBER GenericExecute AS DWORD
	MEMBER GenericAll AS DWORD




	// PshPack 4

VOSTRUCT _WINLUID_AND_ATTRIBUTES ALIGN 4
	MEMBER Luid 		IS _WINLUID		// RvdH 070411 changed from LARGE_INTEGER to LUID
	MEMBER Attributes AS DWORD

	// PopPack (return to default)




VOSTRUCT _WINSID_IDENTIFIER_AUTHORITY
	MEMBER DIM  Value[6] AS BYTE


VOSTRUCT _WINSID
	MEMBER Revision AS BYTE
	MEMBER SubAuthorityCount AS BYTE
	MEMBER IdentifierAuthority IS _WINSID_IDENTIFIER_AUTHORITY
	MEMBER DIM SubAuthority[ANYSIZE_ARRAY] AS DWORD


VOSTRUCT _WINSID_AND_ATTRIBUTES
	MEMBER Sid AS PTR
	MEMBER Attributes AS DWORD






VOSTRUCT _WINACL
	MEMBER AclRevision AS BYTE
	MEMBER Sbz1 AS BYTE
	MEMBER AclSize AS WORD
	MEMBER AceCount AS WORD
	MEMBER Sbz2 AS WORD



VOSTRUCT _WINACE_HEADER
	MEMBER AceType AS BYTE
	MEMBER AceFlags AS BYTE
	MEMBER AceSize AS WORD


VOSTRUCT _WINACCESS_ALLOWED_ACE
	MEMBER Header IS _WINACE_HEADER
	MEMBER Mask AS DWORD
	MEMBER SidStart AS DWORD


VOSTRUCT _WINACCESS_DENIED_ACE
	MEMBER Header IS _WINACE_HEADER
	MEMBER Mask AS DWORD
	MEMBER SidStart AS DWORD

VOSTRUCT _WINSYSTEM_AUDIT_ACE
	MEMBER Header IS   _WINACE_HEADER
	MEMBER Mask AS DWORD
	MEMBER SidStart AS DWORD

VOSTRUCT _WINSYSTEM_ALARM_ACE
	MEMBER Header IS _WINACE_HEADER
	MEMBER Mask AS DWORD
	MEMBER SidStart AS DWORD





VOSTRUCT _WINACL_REVISION_INFORMATION
	MEMBER AclRevision AS DWORD


VOSTRUCT _WINACL_SIZE_INFORMATION
	MEMBER AceCount AS DWORD
	MEMBER AclBytesInUse AS DWORD
	MEMBER AclBytesFree AS DWORD




VOSTRUCT _WINSECURITY_DESCRIPTOR
	MEMBER Revision  AS BYTE
	MEMBER Sbz1 AS BYTE
	MEMBER Control AS WORD
	MEMBER Owner AS PTR
	MEMBER Group AS PTR
	MEMBER Sacl AS _WINACL	// RvdH 070411 changed from IS to AS
	MEMBER Dacl AS _WINACL	// RvdH 070411 changed from IS to AS








VOSTRUCT _WINPRIVILEGE_SET
	MEMBER PrivilegeCount AS DWORD
	MEMBER Control AS DWORD
	MEMBER DIM Privilege[ANYSIZE_ARRAY] IS _WINLUID_AND_ATTRIBUTES



VOSTRUCT _winSECURITY_QUALITY_OF_SERVICE
	MEMBER Length AS DWORD
	MEMBER ImpersonationLevel  AS LONGINT	// RvdH 070411 changed from WORD to LONG 
	MEMBER ContextTrackingMode AS BYTE
	MEMBER EffectiveOnly AS BYTE



VOSTRUCT _WINSE_IMPERSONATION_STATE
	MEMBER Token AS PTR
	MEMBER CopyOnOpen AS BYTE
	MEMBER EffectiveOnly AS BYTE
	MEMBER Level AS LONGINT					// RvdH 070411 changed from WORD to LONG





VOSTRUCT _WINTOKEN_USER
	MEMBER User IS _WINSID_AND_ATTRIBUTES


VOSTRUCT _WINTOKEN_GROUPS
	MEMBER GroupCount AS DWORD
	MEMBER DIM Groups[ANYSIZE_ARRAY] IS  _WINSID_AND_ATTRIBUTES  


VOSTRUCT _WINTOKEN_PRIVILEGES
	MEMBER PrivilegeCount AS DWORD
	MEMBER DIM Privileges[ANYSIZE_ARRAY] IS  _WINLUID_AND_ATTRIBUTES


VOSTRUCT _WINTOKEN_OWNER
	MEMBER Owner AS PTR


VOSTRUCT _WINTOKEN_PRIMARY_GROUP
	MEMBER PrimaryGroup AS PTR


VOSTRUCT _WINTOKEN_DEFAULT_DACL
	MEMBER DefaultDacl AS _WINACL		//RvdH 070412 changed from IS to AS



VOSTRUCT _WINTOKEN_SOURCE
	MEMBER DIM SourceName[TOKEN_SOURCE_LENGTH] AS BYTE
	MEMBER SourceIdentifier IS _WINLARGE_INTEGER





VOSTRUCT _WINTOKEN_CONTROL
	MEMBER TokenId IS _WINlARGE_INTEGER
	MEMBER AuthenticationId IS _WINLARGE_INTEGER
	MEMBER ModifiedId IS _WINLARGE_INTEGER
	MEMBER TokenSource_ IS _WINTOKEN_SOURCE



VOSTRUCT _WINIMAGE_DOS_HEADER  ALIGN 2
	MEMBER e_magic AS WORD
	MEMBER e_cblp AS WORD
	MEMBER e_cp AS WORD
	MEMBER e_crlc AS WORD
	MEMBER e_cparhdr AS WORD
	MEMBER e_minalloc AS WORD
	MEMBER e_maxalloc AS WORD
	MEMBER e_ss AS WORD
	MEMBER e_sp AS WORD
	MEMBER e_csum AS WORD
	MEMBER e_ip AS WORD
	MEMBER e_cs AS WORD
	MEMBER e_lfarlc AS WORD
	MEMBER e_ovno AS WORD
	MEMBER DIM e_res[4] AS WORD
	MEMBER e_oemid AS WORD
	MEMBER e_oeminfo AS WORD
	MEMBER DIM e_res2[10] AS WORD
	MEMBER e_lfanew AS LONGINT


VOSTRUCT _WINIMAGE_OS2_HEADER ALIGN 1	// RvdH 070411 changed alignment from 2 to 1
	MEMBER ne_magic  		AS WORD
	MEMBER ne_ver 			AS BYTE
	MEMBER ne_rev 			AS BYTE
	MEMBER ne_enttab 		AS WORD
	MEMBER ne_cbenttab 	AS WORD
	MEMBER ne_crc 			AS LONGINT
	MEMBER ne_flags  		AS WORD
	MEMBER ne_autodata 	AS WORD
	MEMBER ne_heap 		AS WORD
	MEMBER ne_stack 		AS WORD
	MEMBER ne_csip 		AS LONGINT
	MEMBER ne_sssp 		AS LONGINT
	MEMBER ne_cseg 		AS WORD
	MEMBER ne_cmod 		AS WORD
	MEMBER ne_cbnrestab 	AS WORD
	MEMBER ne_segtab 		AS WORD
	MEMBER ne_rsrctab 	AS WORD
	MEMBER ne_restab 		AS WORD
	MEMBER ne_modtab 		AS WORD
	MEMBER ne_imptab 		AS WORD
	MEMBER ne_nrestab 	AS LONGINT
	MEMBER ne_cmovent 	AS WORD
	MEMBER ne_align 		AS WORD
	MEMBER ne_cres 		AS WORD
	MEMBER ne_exetyp 		AS BYTE
	MEMBER ne_flagsothers AS BYTE
	MEMBER ne_pretthunks AS WORD
	MEMBER ne_psegrefbytes AS WORD
	MEMBER ne_swaparea 	AS WORD
	MEMBER ne_expver 		AS WORD


VOSTRUCT _WINIMAGE_VXD_HEADER ALIGN 1    // RvdH 070411 changed alignment from 2 to 1
	MEMBER e32_magic AS WORD
	MEMBER e32_border AS BYTE
	MEMBER e32_worder AS BYTE
	MEMBER e32_level AS DWORD
	MEMBER e32_cpu AS WORD
	MEMBER e32_os AS WORD
	MEMBER e32_ver AS DWORD
	MEMBER e32_mflags AS DWORD
	MEMBER e32_mpages AS DWORD
	MEMBER e32_startobj AS DWORD
	MEMBER e32_eip AS DWORD
	MEMBER e32_stackobj AS DWORD
	MEMBER e32_esp AS DWORD
	MEMBER e32_pagesize AS DWORD
	MEMBER e32_lastpagesize AS DWORD
	MEMBER e32_fixupsize AS DWORD
	MEMBER e32_fixupsum AS DWORD
	MEMBER e32_ldrsize AS DWORD
	MEMBER e32_ldrsum AS DWORD
	MEMBER e32_objtab AS DWORD
	MEMBER e32_objcnt AS DWORD
	MEMBER e32_objmap AS DWORD
	MEMBER e32_itermap AS DWORD
	MEMBER e32_rsrctab AS DWORD
	MEMBER e32_rsrccnt AS DWORD
	MEMBER e32_restab AS DWORD
	MEMBER e32_enttab AS DWORD
	MEMBER e32_dirtab AS DWORD
	MEMBER e32_dircnt AS DWORD
	MEMBER e32_fpagetab AS DWORD
	MEMBER e32_frectab AS DWORD
	MEMBER e32_impmod AS DWORD
	MEMBER e32_impmodcnt AS DWORD
	MEMBER e32_impproc AS DWORD
	MEMBER e32_pagesum AS DWORD
	MEMBER e32_datapage AS DWORD
	MEMBER e32_preload AS DWORD
	MEMBER e32_nrestab AS DWORD
	MEMBER e32_cbnrestab AS DWORD
	MEMBER e32_nressum AS DWORD
	MEMBER e32_autodata AS DWORD
	MEMBER e32_debuginfo AS DWORD
	MEMBER e32_debuglen AS DWORD
	MEMBER e32_instpreload AS DWORD
	MEMBER e32_instdemand AS DWORD
	MEMBER e32_heapsize AS DWORD
	MEMBER DIM e32_res3[12] AS BYTE   //RvdH 070412 Changed from DWORD to BYTE
	MEMBER e32_winresoff AS DWORD
	MEMBER e32_winreslen AS DWORD
	MEMBER e32_devid AS WORD
	MEMBER e32_ddkver AS WORD

	// PopPack (returns to 4)


VOSTRUCT _WINIMAGE_FILE_HEADER
	MEMBER Machine AS WORD
	MEMBER NumberOfSections AS WORD
	MEMBER TimeDateStamp AS DWORD
	MEMBER PointerToSymbolTable AS DWORD
	MEMBER NumberOfSymbols AS DWORD
	MEMBER SizeOfOptionalHeader AS WORD
	MEMBER Characteristics AS WORD

VOSTRUCT _WINIMAGE_DATA_DIRECTORY
	MEMBER VirtualAddress AS DWORD
	MEMBER Size AS DWORD

VOSTRUCT _WINIMAGE_OPTIONAL_HEADER
    //
    // Standard fields.
    //
	MEMBER Magic AS WORD
	MEMBER MajorLinkerVersion AS BYTE
	MEMBER MinorLinkerVersion AS BYTE
	MEMBER SizeOfCode AS DWORD
	MEMBER SizeOfInitializedData AS DWORD
	MEMBER SizeOfUninitializedData  AS DWORD
	MEMBER AddressOfEntryPoint  AS DWORD
	MEMBER BaseOfCode AS DWORD
	MEMBER BaseOfData AS DWORD

    //
    // NT additional fields.
    //
	MEMBER ImageBase AS DWORD
	MEMBER SectionAlignment AS DWORD
	MEMBER FileAlignment AS DWORD
	MEMBER MajorOperatingSystemVersion AS WORD
	MEMBER MinorOperatingSystemVersion AS WORD
	MEMBER MajorImageVersion AS WORD
	MEMBER MinorImageVersion AS WORD
	MEMBER MajorSubsystemVersion AS WORD
	MEMBER MinorSubsystemVersion AS WORD
	MEMBER Reserved1 AS DWORD
	MEMBER SizeOfImage AS DWORD
	MEMBER SizeOfHeaders AS DWORD
	MEMBER CheckSum AS DWORD
	MEMBER Subsystem AS WORD
	MEMBER DllCharacteristics AS WORD
	MEMBER SizeOfStackReserve AS DWORD
	MEMBER SizeOfStackCommit AS DWORD
	MEMBER SizeOfHeapReserve AS DWORD
	MEMBER SizeOfHeapCommit AS DWORD
	MEMBER LoaderFlags AS DWORD
	MEMBER NumberOfRvaAndSizes AS DWORD
	MEMBER DIM DataDirectory[IMAGE_NUMBEROF_DIRECTORY_ENTRIES] IS _WINIMAGE_DATA_DIRECTORY

VOSTRUCT _WINIMAGE_ROM_OPTIONAL_HEADER
	MEMBER Magic AS WORD
	MEMBER MajorLinkerVersion AS BYTE
	MEMBER MinorLinkerVersion AS BYTE
	MEMBER SizeOfCode AS DWORD
	MEMBER SizeOfInitializedData AS DWORD
	MEMBER SizeOfUninitializedData AS DWORD
	MEMBER AddressOfEntryPoint AS DWORD
	MEMBER BaseOfCode AS DWORD
	MEMBER BaseOfData AS DWORD
	MEMBER BaseOfBss AS DWORD
	MEMBER GprMask AS DWORD
	MEMBER DIM CprMask[4] AS DWORD
	MEMBER GpValue AS DWORD

VOSTRUCT _WINIMAGE_NT_HEADERS
	MEMBER Signature AS DWORD
	MEMBER FileHeader IS _WINIMAGE_FILE_HEADER
	MEMBER OptionalHeader IS _WINIMAGE_OPTIONAL_HEADER

VOSTRUCT _WINIMAGE_ROM_HEADERS
	MEMBER FileHeader IS _WINIMAGE_FILE_HEADER
	MEMBER OptionalHeader IS _WINIMAGE_ROM_OPTIONAL_HEADER






VOSTRUCT  _WINIMAGE_SECTION_HEADER    
	MEMBER DIM Name[IMAGE_SIZEOF_SHORT_NAME]  AS BYTE
	MEMBER Misc IS Misc_win_IMAGE_SECTION_HEADER
	MEMBER VirtualAddress AS DWORD
	MEMBER SizeOfRawData AS DWORD
	MEMBER PointerToRawData AS DWORD
	MEMBER PointerToRelocations AS DWORD
	MEMBER PointerToLinenumbers AS DWORD
	MEMBER NumberOfRelocations AS WORD
	MEMBER NumberOfLinenumbers AS WORD
	MEMBER Characteristics AS DWORD


VOSTRUCT Name_win
	MEMBER _Short AS DWORD
	MEMBER _Long AS DWORD

VOSTRUCT _winIMAGE_SYMBOL  ALIGN 1 // RvdH 070411 changed alignment from 2 to 1
	MEMBER N IS n_win
	MEMBER Value AS DWORD
	MEMBER SectionNumber AS SHORTINT
	MEMBER Type AS WORD
	MEMBER StorageClass AS BYTE
	MEMBER NumberOfAuxSymbols AS BYTE


VOSTRUCT LnSz_win ALIGN 2
	MEMBER Linenumber AS WORD
	MEMBER Size AS WORD

VOSTRUCT FUNCTION_win ALIGN 2
	MEMBER PointerToLinenumber AS DWORD
	MEMBER PointerToNextFunction AS DWORD

VOSTRUCT Array_win ALIGN 2
	MEMBER DIM Dimension[4] AS WORD

VOSTRUCT Sym_win  ALIGN 2
	MEMBER TagIndex AS DWORD
	MEMBER Misc IS Misc_win
	MEMBER FcnAry IS FcnAry_win
	MEMBER TvIndex AS WORD

VOSTRUCT File_win ALIGN 2
	MEMBER DIM   Name[IMAGE_SIZEOF_SYMBOL] AS BYTE

VOSTRUCT Section_win ALIGN 2
	MEMBER Length AS DWORD
	MEMBER NumberOfRelocations AS WORD
	MEMBER NumberOfLinenumbers AS WORD
	MEMBER CheckSum AS DWORD
	MEMBER Number AS SHORTINT
	MEMBER Selection AS BYTE


VOSTRUCT _WINIMAGE_AUX_SYMBOL_TOKEN_DEF ALIGN 2
    MEMBER bAuxType				AS BYTE
    MEMBER bReserved			 	AS BYTE
    MEMBER SymbolTableIndex 	AS DWORD
    MEMBER DIM rgbReserved[12] AS BYTE


VOSTRUCT _WINIMAGE_RELOCATION ALIGN 2
	MEMBER Address IS RELOCADDRESS
	MEMBER SymbolTableIndex AS DWORD
	MEMBER Type AS WORD

VOSTRUCT _WINIMAGE_LINENUMBER ALIGN 2
	MEMBER Type IS IMAGE_LINENUMBER_TYPE
	MEMBER Linenumber AS WORD

	// PopPack  (returns to 4)

VOSTRUCT _WINIMAGE_BASE_RELOCATION
	MEMBER VirtualAddress AS DWORD
	MEMBER SizeOfBlock AS DWORD
	//    MEMBER DIM  TypeOffset[1] AS WORD

VOSTRUCT _WINIMAGE_ARCHIVE_MEMBER_HEADER
	MEMBER DIM Name[16] AS BYTE
	MEMBER DIM _Date[12] AS BYTE
	MEMBER DIM UserID[6] AS BYTE
	MEMBER DIM GroupID[6] AS BYTE
	MEMBER DIM Mode[8] AS BYTE
	MEMBER DIM Size[10] AS BYTE
	MEMBER DIM EndHeader[2] AS BYTE

VOSTRUCT  _WINIMAGE_EXPORT_DIRECTORY
	MEMBER Characteristics AS DWORD
	MEMBER TimeDateStamp AS DWORD
	MEMBER MajorVersion AS WORD
	MEMBER MinorVersion AS WORD
	MEMBER Name AS DWORD
	MEMBER Base AS DWORD
	MEMBER NumberOfFunctions AS DWORD
	MEMBER NumberOfNames AS DWORD
	MEMBER AddressOfFunctions AS PTR
	MEMBER AddressOfNames AS PTR
	MEMBER AddressOfNameOrdinals AS PTR


VOSTRUCT _WINIMAGE_IMPORT_BY_NAME 
	MEMBER Hint AS WORD
	MEMBER DIM Name[1] AS BYTE

VOSTRUCT _WINIMAGE_THUNK_DATA
	MEMBER u1 IS u1_winIMAGE_THUNK_DATA

VOSTRUCT _WINIMAGE_IMPORT_DESCRIPTOR
	MEMBER u1 IS DESCRIPTOR_CHARACTERISTICS
	MEMBER TimeDateStamp AS DWORD
	MEMBER ForwarderChain AS DWORD
	MEMBER Name AS DWORD
	MEMBER FirstThunk AS _WINIMAGE_THUNK_DATA


VOSTRUCT _WINIMAGE_BOUND_IMPORT_DESCRIPTOR
	MEMBER TimeDateStamp AS DWORD
	MEMBER OffsetModuleName AS WORD
	MEMBER NumberOfModuleForwarderRefs AS WORD

VOSTRUCT _WINIMAGE_BOUND_FORWARDER_REF
	MEMBER TimeDateStamp AS DWORD
	MEMBER OffsetModuleName AS WORD
	MEMBER Reserved AS WORD



VOSTRUCT _WINIMAGE_TLS_DIRECTORY
	MEMBER StartAddressOfRawData AS DWORD
	MEMBER EndAddressOfRawData AS DWORD
	MEMBER AddressOfIndex AS DWORD PTR
	MEMBER AddressOfCallBacks AS PTR
	MEMBER SizeOfZeroFill AS DWORD
	MEMBER Characteristics AS DWORD




VOSTRUCT  _WINIMAGE_RESOURCE_DIRECTORY
	MEMBER Characteristics AS DWORD
	MEMBER TimeDateStamp AS DWORD
	MEMBER MajorVersion AS WORD		// RvdH 070411 changed from DWORD to WORD
	MEMBER MinorVersion AS WORD
	MEMBER NumberOfNamedEntries AS WORD
	MEMBER NumberOfIdEntries AS WORD

VOSTRUCT sIMAGE_RESOURCE_DIRECTORY_ENTRY1_win
	MEMBER bitField AS DWORD

VOSTRUCT sIMAGE_RESOURCE_DIRECTORY_ENTRY2_win
	MEMBER bitField AS DWORD

VOSTRUCT _WINIMAGE_RESOURCE_DIRECTORY_ENTRY
	MEMBER u1 IS uIMAGE_RESOURCE_DIRECTORY_ENTRY1_win
	MEMBER u2 IS uIMAGE_RESOURCE_DIRECTORY_ENTRY2_win


VOSTRUCT _WINIMAGE_RESOURCE_DIRECTORY_STRING
	MEMBER Length AS WORD
	MEMBER DIM NameString[ 1 ] AS BYTE


VOSTRUCT _WINIMAGE_RESOURCE_DIR_STRING_U
	MEMBER Length AS WORD
	MEMBER DIM NameString[ 1 ] AS WORD



VOSTRUCT _WINIMAGE_RESOURCE_DATA_ENTRY
	MEMBER OffsetToData AS DWORD
	MEMBER Size AS DWORD
	MEMBER CodePage AS DWORD
	MEMBER Reserved AS DWORD


VOSTRUCT _WINIMAGE_LOAD_CONFIG_DIRECTORY
	MEMBER Characteristics AS DWORD
	MEMBER TimeDateStamp AS DWORD
	MEMBER MajorVersion AS WORD
	MEMBER MinorVersion AS WORD
	MEMBER GlobalFlagsClear AS DWORD
	MEMBER GlobalFlagsSet AS DWORD
	MEMBER CriticalSectionDefaultTimeout AS DWORD
	MEMBER DeCommitFreeBlockThreshold AS DWORD
	MEMBER DeCommitTotalFreeThreshold AS DWORD
	MEMBER LockPrefixTable AS PTR
	MEMBER MaximumAllocationSize AS DWORD
	MEMBER VirtualMemoryThreshold AS DWORD
	MEMBER ProcessHeapFlags AS DWORD   
	// RvdH 070411 changed structure 
	MEMBER ProcessAffinityMask AS DWORD
	MEMBER CSDVersion AS WORD
	MEMBER Reserved1 AS WORD    
	//RvdH 070412 updated
	MEMBER EditList AS DWORD                   // VA
	MEMBER SecurityCookie AS DWORD            // VA
	MEMBER SEHandlerTable AS DWORD             // VA
	MEMBER SEHandlerCount AS DWORD



VOSTRUCT _WINIMAGE_RUNTIME_FUNCTION_ENTRY
	MEMBER BeginAddress AS DWORD
	MEMBER EndAddress AS DWORD
	MEMBER ExceptionHandler AS PTR
	//RvdH 070412 updated
//	MEMBER HandlerData AS PTR
//	MEMBER PrologEndAddress AS DWORD


VOSTRUCT _WINIMAGE_DEBUG_DIRECTORY
	MEMBER Characteristics AS DWORD
	MEMBER TimeDateStamp AS DWORD
	MEMBER MajorVersion AS WORD
	MEMBER MinorVersion AS WORD
	MEMBER Type AS DWORD
	MEMBER SizeOfData AS DWORD
	MEMBER AddressOfRawData AS DWORD
	MEMBER PointerToRawData AS DWORD

VOSTRUCT _WINIMAGE_COFF_SYMBOLS_HEADER
	MEMBER NumberOfSymbols AS DWORD
	MEMBER LvaToFirstSymbol AS DWORD
	MEMBER NumberOfLinenumbers AS DWORD
	MEMBER LvaToFirstLinenumber AS DWORD
	MEMBER RvaToFirstByteOfCode AS DWORD
	MEMBER RvaToLastByteOfCode AS DWORD
	MEMBER RvaToFirstByteOfData AS DWORD
	MEMBER RvaToLastByteOfData AS DWORD

VOSTRUCT _WINFPO_DATA
	MEMBER ulOffStart AS DWORD
	MEMBER cbProcSize AS DWORD
	MEMBER cdwLocals  AS DWORD
	MEMBER cdwParams  AS WORD	// RvdH 070411 changed from DWORD to WORD
	MEMBER uBitfield  AS WORD


VOSTRUCT _WINIMAGE_DEBUG_MISC
	MEMBER DataType AS DWORD
	MEMBER Length AS DWORD
	MEMBER Unicode AS LOGIC   // RvdH 070411 changed from BYTE to LOGIC
	MEMBER DIM Reserved[ 3 ] AS BYTE
	MEMBER DIM Data[ 1 ] AS BYTE



VOSTRUCT _WINIMAGE_FUNCTION_ENTRY
	MEMBER StartingAddress AS DWORD
	MEMBER EndingAddress AS DWORD
	MEMBER EndOfPrologue AS DWORD




VOSTRUCT _winIMAGE_SEPARATE_DEBUG_HEADER
	MEMBER Signature AS WORD
	MEMBER Flags AS WORD
	MEMBER Machine AS WORD
	MEMBER CharacteristicsAS AS WORD  // RvdH 070411 changed from DWORD to WORD
	MEMBER TimeDateStamp AS DWORD
	MEMBER CheckSum AS DWORD
	MEMBER ImageBase AS DWORD
	MEMBER SizeOfImage AS DWORD
	MEMBER NumberOfSections AS DWORD
	MEMBER ExportedNamesSize AS DWORD
	MEMBER DebugDirectorySize AS DWORD
	// RvdH 070411 Updated structure
	MEMBER SectionAlignment AS DWORD
	MEMBER DIM Reserved[ 2 ] AS DWORD

VOSTRUCT _WINMESSAGE_RESOURCE_ENTRY  
	MEMBER Length  AS WORD
	MEMBER Flags AS WORD
	MEMBER DIM  _Text[ 1 ] AS BYTE

VOSTRUCT _WINMESSAGE_RESOURCE_BLOCK
	MEMBER LowId AS DWORD
	MEMBER HighId AS DWORD
	MEMBER OffsetToEntries AS DWORD

VOSTRUCT _WINMESSAGE_RESOURCE_DATA
	MEMBER NumberOfBlocks AS DWORD
	MEMBER DIM Blocks[ 1 ] IS _WINMESSAGE_RESOURCE_BLOCK


VOSTRUCT _WINRTL_CRITICAL_SECTION_DEBUG
	//RvdH 070116 Corrected structure
	MEMBER Type AS WORD
	MEMBER CreatorBackTraceIndex AS WORD
	MEMBER CriticalSection AS _winRTL_CRITICAL_SECTION
	MEMBER ProcessLocksList IS _winLIST_ENTRY
	MEMBER EntryCount AS DWORD
	MEMBER ContentionCount AS DWORD
	MEMBER Flags AS DWORD
	MEMBER CreatorBackTraceIndexHigh AS WORD
	MEMBER Spare AS WORD

VOSTRUCT _winRTL_CRITICAL_SECTION ALIGN 4 // RvdH 070411 changed alignment from 8 to 4
	//RvdH 070116 Corrected structure
#ifdef __VULCAN__	
	MEMBER DebugInfo AS   PTR // Vulcan bug - can't compile because of circular reference
#else	
	MEMBER DebugInfo AS   _WINRTL_CRITICAL_SECTION_DEBUG
#endif	
	MEMBER LockCount AS LONGINT
	MEMBER RecursionCount AS LONGINT
	MEMBER OwningThread AS PTR
	MEMBER LockSemaphore AS PTR
	MEMBER SpinCount AS PTR
VOSTRUCT _WINEVENTLOGRECORD
	MEMBER Length AS DWORD
	MEMBER Reserved AS DWORD
	MEMBER RecordNumber AS DWORD
	MEMBER TimeGenerated AS DWORD
	MEMBER TimeWritten AS DWORD
	MEMBER EventID AS DWORD
	MEMBER EventType AS WORD
	MEMBER NumStrings AS WORD
	MEMBER EventCategory AS WORD
	MEMBER ReservedFlags AS WORD
	MEMBER ClosingRecordNumber AS DWORD
	MEMBER StringOffset AS DWORD
	MEMBER UserSidLength AS DWORD
	MEMBER UserSidOffset AS DWORD
	MEMBER DataLength AS DWORD
	MEMBER DataOffset AS DWORD












VOSTRUCT _winTAPE_ERASE
	MEMBER Type AS DWORD
	MEMBER Immediate AS LOGIC 	// RvdH 070411 changed from BYTE to LOGIC


VOSTRUCT _WINTAPE_PREPARE 
	MEMBER Operation AS DWORD
	MEMBER Immediate AS BYTE


VOSTRUCT _WINTAPE_WRITE_MARKS  
	MEMBER Type AS DWORD
	MEMBER Count AS DWORD
	MEMBER Immediate AS BYTE


VOSTRUCT _WINTAPE_GET_POSITION
	MEMBER Type AS DWORD
	MEMBER Partition AS DWORD
	MEMBER Offset IS _WINLARGE_INTEGER       // RvdH 070411 changed from AS to IS


VOSTRUCT _WINTAPE_SET_POSITION 
	MEMBER @@Method AS DWORD
	MEMBER Partition AS DWORD
	MEMBER Offset IS _WINLARGE_INTEGER     
	MEMBER Immediate AS BYTE



VOSTRUCT _WINTAPE_GET_DRIVE_PARAMETERS
	MEMBER ECC AS BYTE
	MEMBER Compression AS BYTE
	MEMBER DataPadding AS BYTE
	MEMBER ReportSetmarks AS BYTE
	MEMBER DefaultBlockSize AS DWORD
	MEMBER MaximumBlockSize AS DWORD
	MEMBER MinimumBlockSize AS DWORD
	MEMBER MaximumPartitionCount AS DWORD
	MEMBER FeaturesLow AS DWORD
	MEMBER FeaturesHigh AS DWORD
	MEMBER EOTWarningZoneSize AS DWORD


VOSTRUCT _WINTAPE_SET_DRIVE_PARAMETERS
	MEMBER ECC AS BYTE
	MEMBER Compression AS BYTE
	MEMBER DataPadding AS BYTE
	MEMBER ReportSetmarks AS BYTE
	MEMBER EOTWarningZoneSize AS DWORD


VOSTRUCT _WINTAPE_GET_MEDIA_PARAMETERS
	MEMBER Capacity IS _WINLARGE_INTEGER
	MEMBER Remaining IS _WINLARGE_INTEGER
	MEMBER BlockSize AS DWORD
	MEMBER PartitionCount AS DWORD
	MEMBER WriteProtected AS BYTE


VOSTRUCT _WINTAPE_SET_MEDIA_PARAMETERS
	MEMBER BlockSize AS DWORD


VOSTRUCT _WINTAPE_CREATE_PARTITION
	MEMBER _Method AS DWORD
	MEMBER Count AS DWORD
	MEMBER Size AS DWORD


UNION _WINLARGE_INTEGER				// RvdH 070411 changed from STRUCT to UNION 
	MEMBER u IS LARGE_INTEGER_WIN
	MEMBER r8 AS REAL8           // Actually a Int64, but VO does not have that
#ifdef __VULCAN__
    MEMBER i64 AS INT64
#endif
FUNCTION UInt32x32To64(a AS DWORD, b AS DWORD) AS REAL8
	LOCAL val1 AS DWORD
	LOCAL val2 AS DWORD

	val1 := DWORD(_CAST, a)
	val2 := DWORD(_CAST, b)
	RETURN (REAL8(_CAST, val1) * REAL8(_CAST, val2))



FUNCTION Int64ShllMod32(a AS DWORD, b AS DWORD) AS DWORD
	LOCAL val1 AS DWORD

	val1 :=DWORD(_CAST, a)
	RETURN (val1 << (b))

FUNCTION Int64ShraMod32(a AS DWORD, b AS DWORD) AS REAL8
	LOCAL val1 AS REAL8

	val1 := REAL8( _CAST, a)
	RETURN (val1 >>(b))

FUNCTION Int64ShrlMod32(a, b) AS REAL8
	LOCAL val1 AS REAL8

	val1 := REAL8( _CAST, a)
	RETURN (val1 >> (b))







































FUNCTION MAKELANGID(p, s) AS WORD
	RETURN ( _OR((WORD(_CAST, S) << 10), WORD(P)))

FUNCTION PRIMARYLANGID(lgid AS WORD) AS WORD
	RETURN (_AND(WORD(_CAST, lgid),  0x3ff))

FUNCTION SUBLANGID( lgid AS WORD) AS WORD
	RETURN (WORD(_CAST, lgid)  >>10)


FUNCTION MAKELCID(lgid AS WORD, srtid AS WORD) AS DWORD
	LOCAL val1 AS DWORD
	LOCAL val2 AS DWORD

	val1 := DWORD(_CAST, WORD(_CAST, srtid)) << 16
	val2 := DWORD(_CAST, WORD(_CAST, lgid))
	RETURN (_OR(val1, val2))


FUNCTION LANGIDFROMLCID(lcid AS DWORD) AS WORD
   RETURN WORD(_AND(lcid ,NLS_VALID_LOCALE_MASK))

FUNCTION SORTVERSIONFROMLCID(lcid AS DWORD) AS WORD
   RETURN WORD(_AND((lcid >> 20), 0x0F))

FUNCTION MAKESORTLCID(lgid AS DWORD, srtid AS DWORD, ver AS DWORD) AS DWORD
   ver := _AND(ver , NLS_VALID_LOCALE_MASK)
   RETURN _OR((ver << 20) , MAKELCID(lgid, srtid))
//
//  Default System and User IDs for language and locale.
//
UNION Misc_win_IMAGE_SECTION_HEADER   
	MEMBER PhysicalAddress AS DWORD
	MEMBER VirtualSize AS DWORD

UNION N_win
	MEMBER DIM ShortName[8] AS BYTE
	MEMBER Name IS Name_win
	MEMBER DIM LongName[2] AS BYTE PTR

	// PshPack 2

FUNCTION ISTAG(x AS DWORD) AS LOGIC STRICT
	RETURN(x=IMAGE_SYM_CLASS_STRUCT_TAG .OR. x=IMAGE_SYM_CLASS_UNION_TAG .OR. x=IMAGE_SYM_CLASS_ENUM_TAG)






UNION Misc_win
	MEMBER LnSz IS LnSz_win
	MEMBER TotalSize AS DWORD

UNION FcnAry_win
	MEMBER _FUNCTION IS function_win
	MEMBER _ARRAY IS Array_win

UNION _WINIMAGE_AUX_SYMBOL
	MEMBER Sym IS Sym_win
	MEMBER _File IS File_win
	MEMBER Section IS  Section_win


UNION RELOCADDRESS
	MEMBER VirtualAddress AS DWORD
	MEMBER RelocCount AS DWORD

UNION IMAGE_LINENUMBER_TYPE
	MEMBER SymbolTableIndex AS DWORD
	MEMBER VirtualAddress AS DWORD

UNION u1_winIMAGE_THUNK_DATA
	MEMBER ForwarderString AS BYTE PTR
	MEMBER _Function AS DWORD PTR
	MEMBER Ordinal AS DWORD
	MEMBER AddressOfData AS _WINIMAGE_IMPORT_BY_NAME



UNION DESCRIPTOR_CHARACTERISTICS
	MEMBER Characteristics AS DWORD
	MEMBER OriginalFirstThunk AS _WINIMAGE_THUNK_DATA


UNION uIMAGE_RESOURCE_DIRECTORY_ENTRY1_win
	MEMBER s1 IS sIMAGE_RESOURCE_DIRECTORY_ENTRY1_win
	MEMBER Name AS DWORD
	MEMBER Id AS WORD


UNION uIMAGE_RESOURCE_DIRECTORY_ENTRY2_win
	MEMBER OffsetToData AS DWORD
	MEMBER s2 IS sIMAGE_RESOURCE_DIRECTORY_ENTRY2_win

FUNCTION RtlMoveMemory(Destination AS PTR ,Source AS PTR,Length AS DWORD) AS PTR STRICT
	RETURN MemMove(Destination,Source,Length)

FUNCTION RtlCopyMemory(Destination AS PTR ,Source AS PTR,Length AS DWORD) AS PTR STRICT
	RETURN MemCopy(Destination,Source,Length)

FUNCTION RtlFillMemory(Destination AS PTR, Length AS INT, Fill AS DWORD) AS PTR STRICT
	RETURN MemSet((Destination), BYTE(Fill),(Length))

FUNCTION  RtlZeroMemory(Destination AS PTR, Length AS INT) AS PTR STRICT
	RETURN MemSet((Destination),0,(Length))

FUNCTION WorkItemFunc (pvContext AS PTR)  AS DWORD STRICT
	RETURN 0

_DLL FUNCTION QueueUserWorkItem(pFunc AS /* WorkItemFunc */ PTR, pContext AS PTR, ulFlags AS DWORD) AS LOGIC PASCAL:Kernel32.QueueUserWorkItem

_DLL FUNCTION CreateTimerQueueTimer(phNewTimer AS PTR, hTimerQueue AS PTR,;
	pfnCallback AS PTR,;
	pvContext   AS PTR,;
	DueTime     AS DWORD,;
	Period      AS DWORD,;
	Flags       AS DWORD ) AS LOGIC PASCAL:Kernel32.CreateTimerQueueTimer

FUNCTION WaitOrTimerCallback   (pvContext AS PTR, lTimerOrWaitFired AS LOGIC) AS VOID STRICT
	RETURN

_DLL FUNCTION DeleteTimerQueueTimer(hTimerQueue AS PTR,;
	hTimer      AS PTR,;
	hCompletionEvent AS PTR) AS LOGIC PASCAL:Kernel32.DeleteTimerQueueTimer


_DLL FUNCTION ChangeTimerQueueTimer(hTimerQueue AS PTR,;
	hTimer      AS PTR,;
	dwDueTime   AS DWORD,;
	dwPeriod    AS DWORD) AS LOGIC PASCAL:Kernel32.ChangeTimerQueueTimer


_DLL FUNCTION DeleteTimerQueueEx    (hTimerQueue AS PTR,;
	hCompletionEvent AS PTR) AS LOGIC PASCAL:Kernel32.DeleteTimerQueueEx


_DLL FUNCTION RegisterWaitForSingleObject(;
	phNewWaitObject AS PTR,;
	hObject AS PTR,;
	pfnCallback AS /* WaitOrTimerCallback */ PTR,;
	pvContext AS PTR,;
	dwMilliseconds AS DWORD,;
	dwFlags        AS DWORD) AS LOGIC PASCAL:Kernel32.RegisterWaitForSingleObject


_DLL FUNCTION UnregisterWaitEx      (hWaitHandle AS PTR,;
	hCompletionEvent AS PTR) AS LOGIC PASCAL:Kernel32.UnregisterWaitEx

_DLL FUNCTION BindIoCompletionCallback( hFile AS PTR,;
	pfn AS PTR,;
	dwFlags AS DWORD) AS LOGIC PASCAL:Kernel32.BindIoCompletionCallback
FUNCTION Int32x32To64(a, b)  AS REAL8
	LOCAL val1 AS LONGINT
	LOCAL val2 AS LONGINT

	val1 := LONGINT(_CAST, a)
	val2 := LONGINT(_CAST, b)
	RETURN (REAL8(_CAST, val1) * REAL8(_CAST, val2))


FUNCTION SORTIDFROMLCID(lcid AS DWORD) AS WORD
	LOCAL val1 AS DWORD
	LOCAL val2 AS DWORD

   val1  := DWORD(lcid)
	//PP-030924 correct 51422
	VAL2 := _AND(NLS_VALID_LOCALE_MASK, LONGINT(_CAST,VAL1))
	RETURN (WORD(_CAST, (val2 >> 16)))



#region defines
DEFINE ANYSIZE_ARRAY  :=1
DEFINE APPLICATION_ERROR_MASK       := 0x20000000
DEFINE ERROR_SEVERITY_SUCCESS       := 0x00000000
DEFINE ERROR_SEVERITY_INFORMATIONAL := 0x40000000
DEFINE ERROR_SEVERITY_WARNING       := 0x80000000
DEFINE ERROR_SEVERITY_ERROR         := 0xC0000000
DEFINE MINCHAR     := 0x80
DEFINE MAXCHAR     := 0x7f
DEFINE MINSHORT    := 0x8000
DEFINE MAXSHORT    := 0x7fff
DEFINE MINLONG     := 0x80000000
DEFINE MAXLONG     := 0x7fffffff
DEFINE MAXBYTE     := 0xff
DEFINE MAXWORD     := 0xffff
DEFINE MAXDWORD    := 0xffffffff
//
// RvdH 031103 Added New language defines and some new conversion functions
//
/*
#define VER_SERVER_NT                       0x80000000
#define VER_WORKSTATION_NT                  0x40000000
#define VER_SUITE_SMALLBUSINESS             0x00000001
#define VER_SUITE_ENTERPRISE                0x00000002
#define VER_SUITE_BACKOFFICE                0x00000004
#define VER_SUITE_COMMUNICATIONS            0x00000008
#define VER_SUITE_TERMINAL                  0x00000010
#define VER_SUITE_SMALLBUSINESS_RESTRICTED  0x00000020
#define VER_SUITE_EMBEDDEDNT                0x00000040
#define VER_SUITE_DATACENTER                0x00000080
#define VER_SUITE_SINGLEUSERTS              0x00000100
#define VER_SUITE_PERSONAL                  0x00000200
#define VER_SUITE_BLADE                     0x00000400
#define VER_SUITE_EMBEDDED_RESTRICTED       0x00000800
#define VER_SUITE_SECURITY_APPLIANCE        0x00001000
#define VER_SUITE_STORAGE_SERVER            0x00002000
#define VER_SUITE_COMPUTE_SERVER            0x00004000
#define VER_SUITE_WH_SERVER                 0x00008000
//
// Product types
// This list grows with each OS release.
//
// There is no ordering of values to ensure callers
// do an equality test i.e. greater-than and less-than
// comparisons are not useful.
//
// NOTE: Values in this list should never be deleted.
//       When a product-type 'X' gets dropped from a
//       OS release onwards, the value of 'X' continues
//       to be used in the mapping table of GetProductInfo.
//
#define PRODUCT_UNDEFINED                           0x00000000
#define PRODUCT_ULTIMATE                            0x00000001
#define PRODUCT_HOME_BASIC                          0x00000002
#define PRODUCT_HOME_PREMIUM                        0x00000003
#define PRODUCT_ENTERPRISE                          0x00000004
#define PRODUCT_HOME_BASIC_N                        0x00000005
#define PRODUCT_BUSINESS                            0x00000006
#define PRODUCT_STANDARD_SERVER                     0x00000007
#define PRODUCT_DATACENTER_SERVER                   0x00000008
#define PRODUCT_SMALLBUSINESS_SERVER                0x00000009
#define PRODUCT_ENTERPRISE_SERVER                   0x0000000A
#define PRODUCT_STARTER                             0x0000000B
#define PRODUCT_DATACENTER_SERVER_CORE              0x0000000C
#define PRODUCT_STANDARD_SERVER_CORE                0x0000000D
#define PRODUCT_ENTERPRISE_SERVER_CORE              0x0000000E
#define PRODUCT_ENTERPRISE_SERVER_IA64              0x0000000F
#define PRODUCT_BUSINESS_N                          0x00000010
#define PRODUCT_WEB_SERVER                          0x00000011
#define PRODUCT_CLUSTER_SERVER                      0x00000012
#define PRODUCT_HOME_SERVER                         0x00000013
#define PRODUCT_STORAGE_EXPRESS_SERVER              0x00000014
#define PRODUCT_STORAGE_STANDARD_SERVER             0x00000015
#define PRODUCT_STORAGE_WORKGROUP_SERVER            0x00000016
#define PRODUCT_STORAGE_ENTERPRISE_SERVER           0x00000017
#define PRODUCT_SERVER_FOR_SMALLBUSINESS            0x00000018
#define PRODUCT_SMALLBUSINESS_SERVER_PREMIUM        0x00000019
#define PRODUCT_HOME_PREMIUM_N                      0x0000001A
#define PRODUCT_ENTERPRISE_N                        0x0000001B
#define PRODUCT_ULTIMATE_N                          0x0000001C
#define PRODUCT_WEB_SERVER_CORE                     0x0000001D
#define PRODUCT_MEDIUMBUSINESS_SERVER_MANAGEMENT    0x0000001E
#define PRODUCT_MEDIUMBUSINESS_SERVER_SECURITY      0x0000001F
#define PRODUCT_MEDIUMBUSINESS_SERVER_MESSAGING     0x00000020
#define PRODUCT_SMALLBUSINESS_SERVER_PRIME          0x00000021
#define PRODUCT_HOME_PREMIUM_SERVER                 0x00000022
#define PRODUCT_SERVER_FOR_SMALLBUSINESS_V          0x00000023
#define PRODUCT_STANDARD_SERVER_V                   0x00000024
#define PRODUCT_DATACENTER_SERVER_V                 0x00000025
#define PRODUCT_ENTERPRISE_SERVER_V                 0x00000026
#define PRODUCT_DATACENTER_SERVER_CORE_V            0x00000027
#define PRODUCT_STANDARD_SERVER_CORE_V              0x00000028
#define PRODUCT_ENTERPRISE_SERVER_CORE_V            0x00000029
#define PRODUCT_HYPERV                              0x0000002A
#define PRODUCT_UNLICENSED                          0xABCDABCD
*/
//
//  Language IDs.
//
//  The following two combinations of primary language ID and
//  sublanguage ID have special semantics:
//
//    Primary Language ID   Sublanguage ID      Result
//    -------------------   ---------------     ------------------------
//    LANG_NEUTRAL          SUBLANG_NEUTRAL     Language neutral
//    LANG_NEUTRAL          SUBLANG_DEFAULT     User default language
//    LANG_NEUTRAL          SUBLANG_SYS_DEFAULT System default language
//    LANG_INVARIANT        SUBLANG_NEUTRAL     Invariant locale
//
//
//  Primary language IDs.
//
DEFINE LANG_NEUTRAL                   :=  0x00
DEFINE LANG_INVARIANT                 :=  0x7f
DEFINE LANG_AFRIKAANS                 :=  0x36
DEFINE LANG_ALBANIAN                  :=  0x1c
DEFINE LANG_ALSATIAN                  :=  0x84
DEFINE LANG_AMHARIC                   :=  0x5e
DEFINE LANG_ARABIC                    :=  0x01
DEFINE LANG_ARMENIAN                  :=  0x2b
DEFINE LANG_ASSAMESE                  :=  0x4d
DEFINE LANG_AZERI                     :=  0x2c
DEFINE LANG_BASHKIR                   :=  0x6d
DEFINE LANG_BASQUE                    :=  0x2d
DEFINE LANG_BELARUSIAN                :=  0x23
DEFINE LANG_BENGALI                   :=  0x45
DEFINE LANG_BRETON                    :=  0x7e
DEFINE LANG_BOSNIAN                   :=  0x1a   // Use with SUBLANG_BOSNIAN_* Sublanguage IDs
DEFINE LANG_BOSNIAN_NEUTRAL           :=0x781a   // Use with the ConvertDefaultLocale function
DEFINE LANG_BULGARIAN                 :=  0x02
DEFINE LANG_CATALAN                   :=  0x03
DEFINE LANG_CHINESE                   :=  0x04
DEFINE LANG_CHINESE_SIMPLIFIED        :=  0x04   // Use with the ConvertDefaultLocale function
DEFINE LANG_CHINESE_TRADITIONAL       :=0x7c04   // Use with the ConvertDefaultLocale function
DEFINE LANG_CORSICAN                  :=  0x83
DEFINE LANG_CROATIAN                  :=  0x1a
DEFINE LANG_CZECH                     :=  0x05
DEFINE LANG_DANISH                    :=  0x06
DEFINE LANG_DARI                      :=  0x8c
DEFINE LANG_DIVEHI                    :=  0x65
DEFINE LANG_DUTCH                     :=  0x13
DEFINE LANG_ENGLISH                   :=  0x09
DEFINE LANG_ESTONIAN                  :=  0x25
DEFINE LANG_FAEROESE                  :=  0x38
DEFINE LANG_FARSI                     :=  0x29
DEFINE LANG_FILIPINO                  :=  0x64
DEFINE LANG_FINNISH                   :=  0x0b
DEFINE LANG_FRENCH                    :=  0x0c
DEFINE LANG_FRISIAN                   :=  0x62
DEFINE LANG_GALICIAN                  :=  0x56
DEFINE LANG_GEORGIAN                  :=  0x37
DEFINE LANG_GERMAN                    :=  0x07
DEFINE LANG_GREEK                     :=  0x08
DEFINE LANG_GREENLANDIC               :=  0x6f
DEFINE LANG_GUJARATI                  :=  0x47
DEFINE LANG_HAUSA                     :=  0x68
DEFINE LANG_HEBREW                    :=  0x0d
DEFINE LANG_HINDI                     :=  0x39
DEFINE LANG_HUNGARIAN                 :=  0x0e
DEFINE LANG_ICELANDIC                 :=  0x0f
DEFINE LANG_IGBO                      :=  0x70
DEFINE LANG_INDONESIAN                :=  0x21
DEFINE LANG_INUKTITUT                 :=  0x5d
DEFINE LANG_IRISH                     :=  0x3c   // Use with the SUBLANG_IRISH_IRELAND Sublanguage ID
DEFINE LANG_ITALIAN                   :=  0x10
DEFINE LANG_JAPANESE                  :=  0x11
DEFINE LANG_KANNADA                   :=  0x4b
DEFINE LANG_KASHMIRI                  :=  0x60
DEFINE LANG_KAZAK                     :=  0x3f
DEFINE LANG_KHMER                     :=  0x53
DEFINE LANG_KICHE                     :=  0x86
DEFINE LANG_KINYARWANDA               :=  0x87
DEFINE LANG_KONKANI                   :=  0x57
DEFINE LANG_KOREAN                    :=  0x12
DEFINE LANG_KYRGYZ                    :=  0x40
DEFINE LANG_LAO                        := 0x54
DEFINE LANG_LATVIAN                   :=  0x26
DEFINE LANG_LITHUANIAN                :=  0x27
DEFINE LANG_LOWER_SORBIAN             :=  0x2e
DEFINE LANG_LUXEMBOURGISH             :=  0x6e
DEFINE LANG_MACEDONIAN                :=  0x2f   // the Former Yugoslav Republic of Macedonia
DEFINE LANG_MALAY                     :=  0x3e
DEFINE LANG_MALAYALAM                 :=  0x4c
DEFINE LANG_MALTESE                   :=  0x3a
DEFINE LANG_MANIPURI                  :=  0x58
DEFINE LANG_MAORI                     :=  0x81
DEFINE LANG_MAPUDUNGUN                :=  0x7a
DEFINE LANG_MARATHI                   :=  0x4e
DEFINE LANG_MOHAWK                    :=  0x7c
DEFINE LANG_MONGOLIAN                 :=  0x50
DEFINE LANG_NEPALI                    :=  0x61
DEFINE LANG_NORWEGIAN                 :=  0x14
DEFINE LANG_OCCITAN                   :=  0x82
DEFINE LANG_ORIYA                     :=  0x48
DEFINE LANG_PASHTO                    :=  0x63
DEFINE LANG_PERSIAN                   :=  0x29
DEFINE LANG_POLISH                    :=  0x15
DEFINE LANG_PORTUGUESE                :=  0x16
DEFINE LANG_PUNJABI                   :=  0x46
DEFINE LANG_QUECHUA                   :=  0x6b
DEFINE LANG_ROMANIAN                  :=  0x18
DEFINE LANG_ROMANSH                   :=  0x17
DEFINE LANG_RUSSIAN                   :=  0x19
DEFINE LANG_SAMI                      :=  0x3b
DEFINE LANG_SANSKRIT                  :=  0x4f
DEFINE LANG_SERBIAN                   :=  0x1a
DEFINE LANG_SERBIAN_NEUTRAL           := 0x7c1a   // Use with the ConvertDefaultLocale function
DEFINE LANG_SINDHI                    :=  0x59
DEFINE LANG_SINHALESE                 :=  0x5b
DEFINE LANG_SLOVAK                    :=  0x1b
DEFINE LANG_SLOVENIAN                 :=  0x24
DEFINE LANG_SOTHO                     :=  0x6c
DEFINE LANG_SPANISH                   :=  0x0a
DEFINE LANG_SWAHILI                   :=  0x41
DEFINE LANG_SWEDISH                   :=  0x1d
DEFINE LANG_SYRIAC                    :=  0x5a
DEFINE LANG_TAJIK                     :=  0x28
DEFINE LANG_TAMAZIGHT                 :=  0x5f
DEFINE LANG_TAMIL                     :=  0x49
DEFINE LANG_TATAR                     :=  0x44
DEFINE LANG_TELUGU                    :=  0x4a
DEFINE LANG_THAI                      :=  0x1e
DEFINE LANG_TIBETAN                   :=  0x51
DEFINE LANG_TIGRIGNA                  :=  0x73
DEFINE LANG_TSWANA                    :=  0x32
DEFINE LANG_TURKISH                   :=  0x1f
DEFINE LANG_TURKMEN                   :=  0x42
DEFINE LANG_UIGHUR                    :=  0x80
DEFINE LANG_UKRAINIAN                 :=  0x22
DEFINE LANG_UPPER_SORBIAN             :=  0x2e
DEFINE LANG_URDU                      :=  0x20
DEFINE LANG_UZBEK                     :=  0x43
DEFINE LANG_VIETNAMESE                :=  0x2a
DEFINE LANG_WELSH                     :=  0x52
DEFINE LANG_WOLOF                     :=  0x88
DEFINE LANG_XHOSA                     :=  0x34
DEFINE LANG_YAKUT                     :=  0x85
DEFINE LANG_YI                        :=  0x78
DEFINE LANG_YORUBA                    :=  0x6a
DEFINE LANG_ZULU                      :=  0x35
//
//  Sublanguage IDs.
//
//  The name immediately following SUBLANG_ dictates which primary
//  language ID that sublanguage ID can be combined with to form a
//  valid language ID.
//
DEFINE SUBLANG_NEUTRAL                :=  0x00    // language neutral
DEFINE SUBLANG_DEFAULT                :=  0x01    // user default
DEFINE SUBLANG_SYS_DEFAULT            :=  0x02    // system default
DEFINE SUBLANG_CUSTOM_DEFAULT         :=             0x03    // default custom language/locale
DEFINE SUBLANG_CUSTOM_UNSPECIFIED     :=             0x04    // custom language/locale
DEFINE SUBLANG_UI_CUSTOM_DEFAULT      :=             0x05    // Default custom MUI language/locale
DEFINE SUBLANG_AFRIKAANS_SOUTH_AFRICA   :=           0x01    // Afrikaans (South Africa) 0x0436 af-ZA
DEFINE SUBLANG_ALBANIAN_ALBANIA         :=           0x01    // Albanian (Albania) 0x041c sq-AL
DEFINE SUBLANG_ALSATIAN_FRANCE          :=           0x01    // Alsatian (France) 0x0484
DEFINE SUBLANG_AMHARIC_ETHIOPIA         :=           0x01    // Amharic (Ethiopia) 0x045e
DEFINE SUBLANG_ARABIC_SAUDI_ARABIA    :=  0x01    // Arabic (Saudi Arabia)
DEFINE SUBLANG_ARABIC_IRAQ            :=  0x02    // Arabic (Iraq)
DEFINE SUBLANG_ARABIC_EGYPT           :=  0x03    // Arabic (Egypt)
DEFINE SUBLANG_ARABIC_LIBYA           :=  0x04    // Arabic (Libya)
DEFINE SUBLANG_ARABIC_ALGERIA         :=  0x05    // Arabic (Algeria)
DEFINE SUBLANG_ARABIC_MOROCCO         :=  0x06    // Arabic (Morocco)
DEFINE SUBLANG_ARABIC_TUNISIA         :=  0x07    // Arabic (Tunisia)
DEFINE SUBLANG_ARABIC_OMAN            :=  0x08    // Arabic (Oman)
DEFINE SUBLANG_ARABIC_YEMEN           :=  0x09    // Arabic (Yemen)
DEFINE SUBLANG_ARABIC_SYRIA           :=  0x0a    // Arabic (Syria)
DEFINE SUBLANG_ARABIC_JORDAN          :=  0x0b    // Arabic (Jordan)
DEFINE SUBLANG_ARABIC_LEBANON         :=  0x0c    // Arabic (Lebanon)
DEFINE SUBLANG_ARABIC_KUWAIT          :=  0x0d    // Arabic (Kuwait)
DEFINE SUBLANG_ARABIC_UAE             :=  0x0e    // Arabic (U.A.E)
DEFINE SUBLANG_ARABIC_BAHRAIN         :=  0x0f    // Arabic (Bahrain)
DEFINE SUBLANG_ARABIC_QATAR           :=  0x10    // Arabic (Qatar)
DEFINE SUBLANG_ARMENIAN_ARMENIA        :=            0x01    // Armenian (Armenia) 0x042b hy-AM
DEFINE SUBLANG_ASSAMESE_INDIA          :=            0x01    // Assamese (India) 0x044d
DEFINE SUBLANG_AZERI_LATIN            :=  0x01    // Azeri (Latin)
DEFINE SUBLANG_AZERI_CYRILLIC         :=  0x02    // Azeri (Cyrillic)
DEFINE SUBLANG_BASHKIR_RUSSIA         :=             0x01    // Bashkir (Russia) 0x046d ba-RU
DEFINE SUBLANG_BASQUE_BASQUE          :=             0x01    // Basque (Basque) 0x042d eu-ES
DEFINE SUBLANG_BELARUSIAN_BELARUS     :=             0x01    // Belarusian (Belarus) 0x0423 be-BY
DEFINE SUBLANG_BENGALI_INDIA          :=             0x01    // Bengali (India)
DEFINE SUBLANG_BENGALI_BANGLADESH     :=             0x02    // Bengali (Bangladesh)
DEFINE SUBLANG_BOSNIAN_BOSNIA_HERZEGOVINA_LATIN    :=0x05    // Bosnian (Bosnia and Herzegovina - Latin) 0x141a bs-BA-Latn
DEFINE SUBLANG_BOSNIAN_BOSNIA_HERZEGOVINA_CYRILLIC :=0x08    // Bosnian (Bosnia and Herzegovina - Cyrillic) 0x201a bs-BA-Cyrl
DEFINE SUBLANG_BRETON_FRANCE           :=            0x01    // Breton (France) 0x047e
DEFINE SUBLANG_BULGARIAN_BULGARIA      :=            0x01    // Bulgarian (Bulgaria) 0x0402
DEFINE SUBLANG_CATALAN_CATALAN         :=            0x01    // Catalan (Catalan) 0x0403
DEFINE SUBLANG_CHINESE_TRADITIONAL    :=  0x01    // Chinese (Taiwan)
DEFINE SUBLANG_CHINESE_SIMPLIFIED     :=  0x02    // Chinese (PR China)
DEFINE SUBLANG_CHINESE_HONGKONG       :=  0x03    // Chinese (Hong Kong S.A.R., P.R.C.)
DEFINE SUBLANG_CHINESE_SINGAPORE      :=  0x04    // Chinese (Singapore)
DEFINE SUBLANG_CHINESE_MACAU          :=  0x05    // Chinese (Macau S.A.R.)
DEFINE SUBLANG_CORSICAN_FRANCE                     := 0x01    // Corsican (France) 0x0483
DEFINE SUBLANG_CZECH_CZECH_REPUBLIC                := 0x01    // Czech (Czech Republic) 0x0405
DEFINE SUBLANG_CROATIAN_CROATIA                    := 0x01    // Croatian (Croatia)
DEFINE SUBLANG_CROATIAN_BOSNIA_HERZEGOVINA_LATIN   := 0x04    // Croatian (Bosnia and Herzegovina - Latin) 0x101a hr-BA
DEFINE SUBLANG_DANISH_DENMARK                      := 0x01    // Danish (Denmark) 0x0406
DEFINE SUBLANG_DARI_AFGHANISTAN                    := 0x01    // Dari (Afghanistan)
DEFINE SUBLANG_DIVEHI_MALDIVES                     := 0x01    // Divehi (Maldives) 0x0465 div-MV
DEFINE SUBLANG_DUTCH                  :=  0x01    // Dutch
DEFINE SUBLANG_DUTCH_BELGIAN          :=  0x02    // Dutch (Belgian)
DEFINE SUBLANG_ENGLISH_US             :=  0x01    // English (USA)
DEFINE SUBLANG_ENGLISH_UK             :=  0x02    // English (UK)
DEFINE SUBLANG_ENGLISH_AUS            :=  0x03    // English (Australian)
DEFINE SUBLANG_ENGLISH_CAN            :=  0x04    // English (Canadian)
DEFINE SUBLANG_ENGLISH_NZ             :=  0x05    // English (New Zealand)
DEFINE SUBLANG_ENGLISH_EIRE           :=  0x06    // English (Irish)
DEFINE SUBLANG_ENGLISH_SOUTH_AFRICA   :=  0x07    // English (South Africa)
DEFINE SUBLANG_ENGLISH_JAMAICA        :=  0x08    // English (Jamaica)
DEFINE SUBLANG_ENGLISH_CARIBBEAN      :=  0x09    // English (Caribbean)
DEFINE SUBLANG_ENGLISH_BELIZE         :=  0x0a    // English (Belize)
DEFINE SUBLANG_ENGLISH_TRINIDAD       :=  0x0b    // English (Trinidad)
DEFINE SUBLANG_ENGLISH_ZIMBABWE       :=  0x0c    // English (Zimbabwe)
DEFINE SUBLANG_ENGLISH_PHILIPPINES    :=  0x0d    // English (Philippines)
DEFINE SUBLANG_ENGLISH_INDIA                       := 0x10    // English (India)
DEFINE SUBLANG_ENGLISH_MALAYSIA                    := 0x11    // English (Malaysia)
DEFINE SUBLANG_ENGLISH_SINGAPORE                   := 0x12    // English (Singapore)
DEFINE SUBLANG_ESTONIAN_ESTONIA                    := 0x01    // Estonian (Estonia) 0x0425 et-EE
DEFINE SUBLANG_FAEROESE_FAROE_ISLANDS              := 0x01    // Faroese (Faroe Islands) 0x0438 fo-FO
DEFINE SUBLANG_FILIPINO_PHILIPPINES                := 0x01    // Filipino (Philippines) 0x0464 fil-PH
DEFINE SUBLANG_FINNISH_FINLAND                     := 0x01    // Finnish (Finland) 0x040b
DEFINE SUBLANG_FRENCH                 :=  0x01    // French
DEFINE SUBLANG_FRENCH_BELGIAN         :=  0x02    // French (Belgian)
DEFINE SUBLANG_FRENCH_CANADIAN        :=  0x03    // French (Canadian)
DEFINE SUBLANG_FRENCH_SWISS           :=  0x04    // French (Swiss)
DEFINE SUBLANG_FRENCH_LUXEMBOURG      :=  0x05    // French (Luxembourg)
DEFINE SUBLANG_FRENCH_MONACO          :=  0x06    // French (Monaco)
DEFINE SUBLANG_FRISIAN_NETHERLANDS    :=             0x01    // Frisian (Netherlands) 0x0462 fy-NL
DEFINE SUBLANG_GALICIAN_GALICIAN      :=             0x01    // Galician (Galician) 0x0456 gl-ES
DEFINE SUBLANG_GEORGIAN_GEORGIA       :=             0x01    // Georgian (Georgia) 0x0437 ka-GE
DEFINE SUBLANG_GERMAN                 :=  0x01    // German
DEFINE SUBLANG_GERMAN_SWISS           :=  0x02    // German (Swiss)
DEFINE SUBLANG_GERMAN_AUSTRIAN        :=  0x03    // German (Austrian)
DEFINE SUBLANG_GERMAN_LUXEMBOURG      :=  0x04    // German (Luxembourg)
DEFINE SUBLANG_GERMAN_LIECHTENSTEIN   :=  0x05    // German (Liechtenstein)
DEFINE SUBLANG_GREEK_GREECE                       := 0x01    // Greek (Greece)
DEFINE SUBLANG_GREENLANDIC_GREENLAND              := 0x01    // Greenlandic (Greenland) 0x046f kl-GL
DEFINE SUBLANG_GUJARATI_INDIA                     := 0x01    // Gujarati (India (Gujarati Script)) 0x0447 gu-IN
DEFINE SUBLANG_HAUSA_NIGERIA_LATIN                := 0x01    // Hausa (Latin, Nigeria) 0x0468 ha-NG-Latn
DEFINE SUBLANG_HEBREW_ISRAEL                      := 0x01    // Hebrew (Israel) 0x040d
DEFINE SUBLANG_HINDI_INDIA                        := 0x01    // Hindi (India) 0x0439 hi-IN
DEFINE SUBLANG_HUNGARIAN_HUNGARY                  := 0x01    // Hungarian (Hungary) 0x040e
DEFINE SUBLANG_ICELANDIC_ICELAND                  := 0x01    // Icelandic (Iceland) 0x040f
DEFINE SUBLANG_IGBO_NIGERIA                       := 0x01    // Igbo (Nigeria) 0x0470 ig-NG
DEFINE SUBLANG_INDONESIAN_INDONESIA               := 0x01    // Indonesian (Indonesia) 0x0421 id-ID
DEFINE SUBLANG_INUKTITUT_CANADA                   := 0x01    // Inuktitut (Syllabics) (Canada) 0x045d iu-CA-Cans
DEFINE SUBLANG_INUKTITUT_CANADA_LATIN             := 0x02    // Inuktitut (Canada - Latin)
DEFINE SUBLANG_IRISH_IRELAND                      := 0x02    // Irish (Ireland)
DEFINE SUBLANG_ITALIAN                :=  0x01    // Italian
DEFINE SUBLANG_ITALIAN_SWISS          :=  0x02    // Italian (Swiss)
DEFINE SUBLANG_JAPANESE_JAPAN                      :=0x01    // Japanese (Japan) 0x0411
DEFINE SUBLANG_KANNADA_INDIA                       :=0x01    // Kannada (India (Kannada Script)) 0x044b kn-IN
DEFINE SUBLANG_KASHMIRI_SASIA         :=  0x02    // Kashmiri (South Asia)
DEFINE SUBLANG_KASHMIRI_INDIA         :=  0x02    // For app compatibility only
DEFINE SUBLANG_KAZAK_KAZAKHSTAN                    := 0x01    // Kazakh (Kazakhstan) 0x043f kk-KZ
DEFINE SUBLANG_KHMER_CAMBODIA                      := 0x01    // Khmer (Cambodia) 0x0453 kh-KH
DEFINE SUBLANG_KICHE_GUATEMALA                     := 0x01    // K'iche (Guatemala)
DEFINE SUBLANG_KINYARWANDA_RWANDA                  := 0x01    // Kinyarwanda (Rwanda) 0x0487 rw-RW
DEFINE SUBLANG_KONKANI_INDIA                       := 0x01    // Konkani (India) 0x0457 kok-IN
DEFINE SUBLANG_KOREAN                 :=  0x01    // Korean (Extended Wansung)
DEFINE SUBLANG_KYRGYZ_KYRGYZSTAN                   := 0x01    // Kyrgyz (Kyrgyzstan) 0x0440 ky-KG
DEFINE SUBLANG_LAO_LAO                             := 0x01    // Lao (Lao PDR) 0x0454 lo-LA
DEFINE SUBLANG_LATVIAN_LATVIA                      := 0x01    // Latvian (Latvia) 0x0426 lv-LV
DEFINE SUBLANG_LITHUANIAN             :=  0x01    // Lithuanian
DEFINE SUBLANG_LOWER_SORBIAN_GERMANY              :=  0x02    // Lower Sorbian (Germany) 0x082e wee-DE
DEFINE SUBLANG_LUXEMBOURGISH_LUXEMBOURG           :=  0x01    // Luxembourgish (Luxembourg) 0x046e lb-LU
DEFINE SUBLANG_MACEDONIAN_MACEDONIA               :=  0x01    // Macedonian (Macedonia (FYROM)) 0x042f mk-MK
DEFINE SUBLANG_MALAY_MALAYSIA         :=  0x01    // Malay (Malaysia)
DEFINE SUBLANG_MALAY_BRUNEI_DARUSSALAM:=  0x02    // Malay (Brunei Darussalam)
DEFINE SUBLANG_MALAYALAM_INDIA                  :=   0x01    // Malayalam (India (Malayalam Script) ) 0x044c ml-IN
DEFINE SUBLANG_MALTESE_MALTA                    :=   0x01    // Maltese (Malta) 0x043a mt-MT
DEFINE SUBLANG_MAORI_NEW_ZEALAND                :=   0x01    // Maori (New Zealand) 0x0481 mi-NZ
DEFINE SUBLANG_MAPUDUNGUN_CHILE                 :=   0x01    // Mapudungun (Chile) 0x047a arn-CL
DEFINE SUBLANG_MARATHI_INDIA                    :=   0x01    // Marathi (India) 0x044e mr-IN
DEFINE SUBLANG_MOHAWK_MOHAWK                    :=   0x01    // Mohawk (Mohawk) 0x047c moh-CA
DEFINE SUBLANG_MONGOLIAN_CYRILLIC_MONGOLIA      :=   0x01    // Mongolian (Cyrillic, Mongolia)
DEFINE SUBLANG_MONGOLIAN_PRC                    :=   0x02    // Mongolian (PRC)
DEFINE SUBLANG_NEPALI_INDIA           :=  0x02    // Nepali (India)
DEFINE SUBLANG_NEPALI_NEPAL                        := 0x01    // Nepali (Nepal) 0x0461 ne-NP
DEFINE SUBLANG_NORWEGIAN_BOKMAL       :=  0x01    // Norwegian (Bokmal)
DEFINE SUBLANG_NORWEGIAN_NYNORSK      :=  0x02    // Norwegian (Nynorsk)
DEFINE SUBLANG_OCCITAN_FRANCE                    :=   0x01    // Occitan (France) 0x0482 oc-FR
DEFINE SUBLANG_ORIYA_INDIA                       :=   0x01    // Oriya (India (Oriya Script)) 0x0448 or-IN
DEFINE SUBLANG_PASHTO_AFGHANISTAN                :=   0x01    // Pashto (Afghanistan)
DEFINE SUBLANG_PERSIAN_IRAN                      :=   0x01    // Persian (Iran) 0x0429 fa-IR
DEFINE SUBLANG_POLISH_POLAND                     :=   0x01    // Polish (Poland) 0x0415
DEFINE SUBLANG_PORTUGUESE             :=  0x02    // Portuguese
DEFINE SUBLANG_PORTUGUESE_BRAZILIAN   :=  0x01    // Portuguese (Brazilian)
DEFINE SUBLANG_PUNJABI_INDIA                    :=   0x01    // Punjabi (India (Gurmukhi Script)) 0x0446 pa-IN
DEFINE SUBLANG_QUECHUA_BOLIVIA                  :=   0x01    // Quechua (Bolivia)
DEFINE SUBLANG_QUECHUA_ECUADOR                  :=   0x02    // Quechua (Ecuador)
DEFINE SUBLANG_QUECHUA_PERU                     :=   0x03    // Quechua (Peru)
DEFINE SUBLANG_ROMANIAN_ROMANIA                 :=   0x01    // Romanian (Romania) 0x0418
DEFINE SUBLANG_ROMANSH_SWITZERLAND              :=   0x01    // Romansh (Switzerland) 0x0417 rm-CH
DEFINE SUBLANG_RUSSIAN_RUSSIA                   :=   0x01    // Russian (Russia) 0x0419
DEFINE SUBLANG_SAMI_NORTHERN_NORWAY             :=   0x01    // Northern Sami (Norway)
DEFINE SUBLANG_SAMI_NORTHERN_SWEDEN             :=   0x02    // Northern Sami (Sweden)
DEFINE SUBLANG_SAMI_NORTHERN_FINLAND            :=   0x03    // Northern Sami (Finland)
DEFINE SUBLANG_SAMI_LULE_NORWAY                 :=   0x04    // Lule Sami (Norway)
DEFINE SUBLANG_SAMI_LULE_SWEDEN                 :=   0x05    // Lule Sami (Sweden)
DEFINE SUBLANG_SAMI_SOUTHERN_NORWAY             :=   0x06    // Southern Sami (Norway)
DEFINE SUBLANG_SAMI_SOUTHERN_SWEDEN             :=   0x07    // Southern Sami (Sweden)
DEFINE SUBLANG_SAMI_SKOLT_FINLAND               :=   0x08    // Skolt Sami (Finland)
DEFINE SUBLANG_SAMI_INARI_FINLAND               :=   0x09    // Inari Sami (Finland)
DEFINE SUBLANG_SANSKRIT_INDIA                   :=   0x01    // Sanskrit (India) 0x044f sa-IN
DEFINE SUBLANG_SERBIAN_BOSNIA_HERZEGOVINA_LATIN :=   0x06    // Serbian (Bosnia and Herzegovina - Latin)
DEFINE SUBLANG_SERBIAN_BOSNIA_HERZEGOVINA_CYRILLIC := 0x07    // Serbian (Bosnia and Herzegovina - Cyrillic)
DEFINE SUBLANG_SERBIAN_CROATIA                     := 0x01    // Croatian (Croatia) 0x041a hr-HR
DEFINE SUBLANG_SERBIAN_LATIN          :=  0x02    // Serbian (Latin)
DEFINE SUBLANG_SERBIAN_CYRILLIC       :=  0x03    // Serbian (Cyrillic)
DEFINE SUBLANG_SINDHI_INDIA                      :=   0x01    // Sindhi (India) reserved 0x0459
DEFINE SUBLANG_SINDHI_PAKISTAN                   :=   0x02    // Sindhi (Pakistan) reserved 0x0859
DEFINE SUBLANG_SINDHI_AFGHANISTAN                :=   0x02    // For app compatibility only
DEFINE SUBLANG_SINHALESE_SRI_LANKA               :=   0x01    // Sinhalese (Sri Lanka)
DEFINE SUBLANG_SOTHO_NORTHERN_SOUTH_AFRICA       :=   0x01    // Northern Sotho (South Africa)
DEFINE SUBLANG_SLOVAK_SLOVAKIA                   :=   0x01    // Slovak (Slovakia) 0x041b sk-SK
DEFINE SUBLANG_SLOVENIAN_SLOVENIA                :=   0x01    // Slovenian (Slovenia) 0x0424 sl-SI
DEFINE SUBLANG_SPANISH                :=  0x01    // Spanish (Castilian)
DEFINE SUBLANG_SPANISH_MEXICAN        :=  0x02    // Spanish (Mexican)
DEFINE SUBLANG_SPANISH_MODERN         :=  0x03    // Spanish (Spain)
DEFINE SUBLANG_SPANISH_GUATEMALA      :=  0x04    // Spanish (Guatemala)
DEFINE SUBLANG_SPANISH_COSTA_RICA     :=  0x05    // Spanish (Costa Rica)
DEFINE SUBLANG_SPANISH_PANAMA         :=  0x06    // Spanish (Panama)
DEFINE SUBLANG_SPANISH_DOMINICAN_REPUBLIC:= 0x07  // Spanish (Dominican Republic)
DEFINE SUBLANG_SPANISH_VENEZUELA      :=  0x08    // Spanish (Venezuela)
DEFINE SUBLANG_SPANISH_COLOMBIA       :=  0x09    // Spanish (Colombia)
DEFINE SUBLANG_SPANISH_PERU           :=  0x0a    // Spanish (Peru)
DEFINE SUBLANG_SPANISH_ARGENTINA      :=  0x0b    // Spanish (Argentina)
DEFINE SUBLANG_SPANISH_ECUADOR        :=  0x0c    // Spanish (Ecuador)
DEFINE SUBLANG_SPANISH_CHILE          :=  0x0d    // Spanish (Chile)
DEFINE SUBLANG_SPANISH_URUGUAY        :=  0x0e    // Spanish (Uruguay)
DEFINE SUBLANG_SPANISH_PARAGUAY       :=  0x0f    // Spanish (Paraguay)
DEFINE SUBLANG_SPANISH_BOLIVIA        :=  0x10    // Spanish (Bolivia)
DEFINE SUBLANG_SPANISH_EL_SALVADOR    :=  0x11    // Spanish (El Salvador)
DEFINE SUBLANG_SPANISH_HONDURAS       :=  0x12    // Spanish (Honduras)
DEFINE SUBLANG_SPANISH_NICARAGUA      :=  0x13    // Spanish (Nicaragua)
DEFINE SUBLANG_SPANISH_PUERTO_RICO    :=  0x14    // Spanish (Puerto Rico)
DEFINE SUBLANG_SPANISH_US                          := 0x15    // Spanish (United States)
DEFINE SUBLANG_SWAHILI_KENYA                       := 0x01    // Swahili (Kenya) 0x0441 sw-KE
DEFINE SUBLANG_SWEDISH                :=  0x01    // Swedish
DEFINE SUBLANG_SWEDISH_FINLAND       :=   0x02    // Swedish (Finland)
DEFINE SUBLANG_SYRIAC_SYRIA                      :=   0x01    // Syriac (Syria) 0x045a syr-SY
DEFINE SUBLANG_TAJIK_TAJIKISTAN                  :=   0x01    // Tajik (Tajikistan) 0x0428 tg-TJ-Cyrl
DEFINE SUBLANG_TAMAZIGHT_ALGERIA_LATIN           :=   0x02    // Tamazight (Latin, Algeria) 0x085f tmz-DZ-Latn
DEFINE SUBLANG_TAMIL_INDIA                       :=   0x01    // Tamil (India)
DEFINE SUBLANG_TATAR_RUSSIA                      :=   0x01    // Tatar (Russia) 0x0444 tt-RU
DEFINE SUBLANG_TELUGU_INDIA                      :=   0x01    // Telugu (India (Telugu Script)) 0x044a te-IN
DEFINE SUBLANG_THAI_THAILAND                     :=   0x01    // Thai (Thailand) 0x041e th-TH
DEFINE SUBLANG_TIBETAN_PRC                       :=   0x01    // Tibetan (PRC)
DEFINE SUBLANG_TIGRIGNA_ERITREA                  :=   0x02    // Tigrigna (Eritrea)
DEFINE SUBLANG_TSWANA_SOUTH_AFRICA               :=   0x01    // Setswana / Tswana (South Africa) 0x0432 tn-ZA
DEFINE SUBLANG_TURKISH_TURKEY                    :=   0x01    // Turkish (Turkey) 0x041f tr-TR
DEFINE SUBLANG_TURKMEN_TURKMENISTAN              :=   0x01    // Turkmen (Turkmenistan) 0x0442 tk-TM
DEFINE SUBLANG_UIGHUR_PRC                        :=   0x01    // Uighur (PRC) 0x0480 ug-CN
DEFINE SUBLANG_UKRAINIAN_UKRAINE                 :=   0x01    // Ukrainian (Ukraine) 0x0422 uk-UA
DEFINE SUBLANG_UPPER_SORBIAN_GERMANY             :=   0x01    // Upper Sorbian (Germany) 0x042e wen-DE
DEFINE SUBLANG_URDU_PAKISTAN         :=   0x01    // Urdu (Pakistan)
DEFINE SUBLANG_URDU_INDIA            :=   0x02    // Urdu (India)
DEFINE SUBLANG_UZBEK_LATIN           :=   0x01    // Uzbek (Latin)
DEFINE SUBLANG_UZBEK_CYRILLIC        :=   0x02    // Uzbek (Cyrillic)
DEFINE SUBLANG_VIETNAMESE_VIETNAM               :=   0x01    // Vietnamese (Vietnam) 0x042a vi-VN
DEFINE SUBLANG_WELSH_UNITED_KINGDOM             :=   0x01    // Welsh (United Kingdom) 0x0452 cy-GB
DEFINE SUBLANG_WOLOF_SENEGAL                    :=   0x01    // Wolof (Senegal)
DEFINE SUBLANG_XHOSA_SOUTH_AFRICA               :=   0x01    // isiXhosa / Xhosa (South Africa) 0x0434 xh-ZA
DEFINE SUBLANG_YAKUT_RUSSIA                     :=   0x01    // Yakut (Russia) 0x0485 sah-RU
DEFINE SUBLANG_YI_PRC                           :=   0x01    // Yi (PRC)) 0x0478
DEFINE SUBLANG_YORUBA_NIGERIA                   :=   0x01    // Yoruba (Nigeria) 046a yo-NG
DEFINE SUBLANG_ZULU_SOUTH_AFRICA                :=   0x01    // isiZulu / Zulu (South Africa) 0x0435 zu-ZA
//
//  Sorting IDs.
//
DEFINE SORT_DEFAULT                  :=   0x0     // sorting default
DEFINE SORT_INVARIANT_MATH              := 0x1     // Invariant (Mathematical Symbols)
DEFINE SORT_JAPANESE_XJIS            :=   0x0     // Japanese XJIS order
DEFINE SORT_JAPANESE_UNICODE         :=   0x1     // Japanese Unicode order
DEFINE SORT_JAPANESE_RADICALSTROKE      := 0x4     // Japanese radical/stroke order
DEFINE SORT_CHINESE_BIG5              :=  0x0     // Chinese BIG5 order
DEFINE SORT_CHINESE_PRCP              :=  0x0     // PRC Chinese Phonetic order
DEFINE SORT_CHINESE_UNICODE           :=  0x1     // Chinese Unicode order
DEFINE SORT_CHINESE_PRC               :=  0x2     // PRC Chinese Stroke Count order
DEFINE SORT_CHINESE_BOPOMOFO          :=  0x3     // Traditional Chinese Bopomofo order
DEFINE SORT_KOREAN_KSC                :=  0x0     // Korean KSC order
DEFINE SORT_KOREAN_UNICODE            :=  0x1     // Korean Unicode order
DEFINE SORT_GERMAN_PHONE_BOOK         :=  0x1     // German Phone Book order
DEFINE SORT_HUNGARIAN_DEFAULT         :=  0x0     // Hungarian Default order
DEFINE SORT_HUNGARIAN_TECHNICAL       :=  0x1     // Hungarian Technical order
DEFINE SORT_GEORGIAN_TRADITIONAL      :=  0x0     // Georgian Traditional order
DEFINE SORT_GEORGIAN_MODERN           :=  0x1     // Georgian Modern order
// end_r_winnt
//
//  A language ID is a 16 bit value which is the combination of a
//  primary language ID and a secondary language ID.  The bits are
//  allocated as follows:
//
//       +-----------------------+-------------------------+
//       |     Sublanguage ID    |   Primary Language ID   |
//       +-----------------------+-------------------------+
//        15                   10 9                       0   bit
//
//
//  Language ID creation/extraction macros:
//
//    MAKELANGID    - construct language id from a primary language id and
//                    a sublanguage id.
//    PRIMARYLANGID - extract primary language id from a language id.
//    SUBLANGID     - extract sublanguage id from a language id.
//
DEFINE NLS_VALID_LOCALE_MASK  := 0x000fffffU
DEFINE LOCALE_NAME_MAX_LENGTH   := 85
//
//
//  Default System and User IDs for language and locale.
//
DEFINE LANG_SYSTEM_DEFAULT    := 0x0000002
DEFINE LANG_USER_DEFAULT      := 0x0000001
DEFINE LOCALE_SYSTEM_DEFAULT  := 0x0002
DEFINE LOCALE_USER_DEFAULT    := 0x400
DEFINE LOCALE_NEUTRAL         := 0x0000
DEFINE LOCALE_INVARIANT       := 0x007f
//
//  Other special IDs for language and locale.
//
DEFINE STATUS_WAIT_0                   :=   0x00000000L
DEFINE STATUS_ABANDONED_WAIT_0         :=   0x00000080L
DEFINE STATUS_USER_APC                 :=  DWORD (_CAST,  0x000000C0)
DEFINE STATUS_TIMEOUT                  :=  DWORD (_CAST, 0x00000102L)
DEFINE STATUS_PENDING                  :=  DWORD (_CAST, 0x00000103L)
DEFINE DBG_EXCEPTION_HANDLED           := DWORD(_CAST, 0x00010001L)    
DEFINE DBG_CONTINUE                   := DWORD(_CAST,0x00010002)
DEFINE STATUS_SEGMENT_NOTIFICATION     := DWORD(_CAST, 0x40000005L)    
DEFINE DBG_TERMINATE_THREAD           := DWORD(_CAST,0x40010003)
DEFINE DBG_TERMINATE_PROCESS          := DWORD(_CAST,0x40010004)
DEFINE DBG_CONTROL_C                  := DWORD(_CAST,0x40010005)
DEFINE DBG_CONTROL_BREAK              := DWORD(_CAST,0x40010008)
DEFINE DBG_COMMAND_EXCEPTION           := DWORD(_CAST, 0x40010009L)    
DEFINE STATUS_GUARD_PAGE_VIOLATION     :=  DWORD (_CAST, 0x80000001L)
DEFINE STATUS_DATATYPE_MISALIGNMENT    :=  DWORD (_CAST, 0x80000002L)
DEFINE STATUS_BREAKPOINT               :=  DWORD (_CAST, 0x80000003L)
DEFINE STATUS_SINGLE_STEP              :=  DWORD (_CAST, 0x80000004L)
DEFINE STATUS_LONGJUMP                  := DWORD(_CAST, 0x80000026L)    
DEFINE STATUS_UNWIND_CONSOLIDATE        := DWORD(_CAST, 0x80000029L)    
DEFINE DBG_EXCEPTION_NOT_HANDLED      := DWORD(_CAST,0x80010001)
DEFINE STATUS_ACCESS_VIOLATION         :=  DWORD (_CAST, 0xC0000005L)
DEFINE STATUS_IN_PAGE_ERROR            :=  DWORD (_CAST, 0xC0000006L)
DEFINE STATUS_INVALID_HANDLE           := DWORD(_CAST, 0xC0000008L)    
DEFINE STATUS_INVALID_PARAMETER        := DWORD(_CAST, 0xC000000DL)    
DEFINE STATUS_NO_MEMORY                :=  DWORD (_CAST, 0xC0000017L)
DEFINE STATUS_ILLEGAL_INSTRUCTION      :=  DWORD (_CAST, 0xC000001DL)
DEFINE STATUS_NONCONTINUABLE_EXCEPTION :=  DWORD (_CAST, 0xC0000025L)
DEFINE STATUS_INVALID_DISPOSITION      :=  DWORD (_CAST, 0xC0000026L)
DEFINE STATUS_ARRAY_BOUNDS_EXCEEDED    :=  DWORD (_CAST, 0xC000008CL)
DEFINE STATUS_FLOAT_DENORMAL_OPERAND   :=  DWORD (_CAST, 0xC000008DL)
DEFINE STATUS_FLOAT_DIVIDE_BY_ZERO     :=  DWORD (_CAST, 0xC000008EL)
DEFINE STATUS_FLOAT_INEXACT_RESULT     :=  DWORD (_CAST, 0xC000008FL)
DEFINE STATUS_FLOAT_INVALID_OPERATION  :=  DWORD (_CAST, 0xC0000090L)
DEFINE STATUS_FLOAT_OVERFLOW           :=  DWORD (_CAST, 0xC0000091L)
DEFINE STATUS_FLOAT_STACK_CHECK        :=  DWORD (_CAST, 0xC0000092L)
DEFINE STATUS_FLOAT_UNDERFLOW          :=  DWORD (_CAST, 0xC0000093L)
DEFINE STATUS_INTEGER_DIVIDE_BY_ZERO   :=  DWORD (_CAST, 0xC0000094L)
DEFINE STATUS_INTEGER_OVERFLOW         :=  DWORD (_CAST, 0xC0000095L)
DEFINE STATUS_PRIVILEGED_INSTRUCTION   :=  DWORD (_CAST, 0xC0000096L)
DEFINE STATUS_STACK_OVERFLOW           :=  DWORD (_CAST, 0xC00000FDL)
DEFINE STATUS_CONTROL_C_EXIT           :=  DWORD (_CAST, 0xC000013AL)  
DEFINE STATUS_POSSIBLE_DEADLOCK        :=  DWORD(_CAST, 0xC0000194L)
DEFINE STATUS_FLOAT_MULTIPLE_FAULTS     := DWORD(_CAST, 0xC00002B4L)    
DEFINE STATUS_FLOAT_MULTIPLE_TRAPS      := DWORD(_CAST, 0xC00002B5L)    
DEFINE STATUS_REG_NAT_CONSUMPTION       := DWORD(_CAST, 0xC00002C9L)    
DEFINE STATUS_STACK_BUFFER_OVERRUN      := DWORD(_CAST, 0xC0000409L)    
DEFINE STATUS_INVALID_CRUNTIME_PARAMETER := 0xC0000417U    
DEFINE STATUS_SXS_EARLY_DEACTIVATION    := 0xC015000FU    
DEFINE STATUS_SXS_INVALID_DEACTIVATION  := 0xC0150010U    
DEFINE MAXIMUM_WAIT_OBJECTS := 64
DEFINE MAXIMUM_SUSPEND_COUNT := MAXCHAR
DEFINE MAXIMUM_SUPPORTED_EXTENSION     := 512
DEFINE SIZE_OF_80387_REGISTERS      := 80
DEFINE CONTEXT_i386    := 0x00010000
DEFINE CONTEXT_i486    := 0x00010000
DEFINE CONTEXT_CONTROL         :=0x00010001L
DEFINE CONTEXT_INTEGER         :=0x00010002L
DEFINE CONTEXT_SEGMENTS        :=0x00010004L
DEFINE CONTEXT_FLOATING_POINT  :=0x00010008L
DEFINE CONTEXT_DEBUG_REGISTERS :=0x00010010L
DEFINE CONTEXT_FULL :=0x00010007L
DEFINE EXCEPTION_NONCONTINUABLE   := 0x1
DEFINE EXCEPTION_MAXIMUM_PARAMETERS  :=15
DEFINE PROCESS_TERMINATE                 :=  (0x0001)  
DEFINE PROCESS_CREATE_THREAD             :=  (0x0002)  
DEFINE PROCESS_VM_OPERATION              :=  (0x0008)  
DEFINE PROCESS_VM_READ                   :=  (0x0010)  
DEFINE PROCESS_VM_WRITE                  :=  (0x0020)  
DEFINE PROCESS_DUP_HANDLE                :=  (0x0040)  
DEFINE PROCESS_CREATE_PROCESS            :=  (0x0080)  
DEFINE PROCESS_SET_QUOTA                 :=  (0x0100)  
DEFINE PROCESS_SET_INFORMATION           :=  (0x0200)  
DEFINE PROCESS_QUERY_INFORMATION         :=  (0x0400)  
DEFINE PROCESS_ALL_ACCESS    := 0x001F0FFFl
DEFINE THREAD_TERMINATE               := 0x0001
DEFINE THREAD_SUSPEND_RESUME          := 0x0002
DEFINE THREAD_GET_CONTEXT             := 0x0008
DEFINE THREAD_SET_CONTEXT             := 0x0010
DEFINE THREAD_SET_INFORMATION         := 0x0020
DEFINE THREAD_QUERY_INFORMATION       := 0x0040
DEFINE THREAD_SET_THREAD_TOKEN        := 0x0080
DEFINE THREAD_IMPERSONATE             := 0x0100
DEFINE THREAD_DIRECT_IMPERSONATION    := 0x0200
DEFINE THREAD_ALL_ACCESS         := 0x001F03FFl
DEFINE TLS_MINIMUM_AVAILABLE       := 64
DEFINE THREAD_BASE_PRIORITY_LOWRT  := 15
DEFINE THREAD_BASE_PRIORITY_MAX    := 2
DEFINE THREAD_BASE_PRIORITY_MIN    := -2
DEFINE THREAD_BASE_PRIORITY_IDLE   := -15
DEFINE EVENT_MODIFY_STATE      := 0x0002
DEFINE EVENT_ALL_ACCESS        := 0x001F0003
DEFINE MUTANT_QUERY_STATE      := 0x0001
DEFINE MUTANT_ALL_ACCESS       := 0x001F0001
DEFINE SEMAPHORE_MODIFY_STATE  := 0x0002
DEFINE SEMAPHORE_ALL_ACCESS    := 0x001F0003
DEFINE TIME_ZONE_ID_UNKNOWN    := 0
DEFINE TIME_ZONE_ID_STANDARD   := 1
DEFINE TIME_ZONE_ID_DAYLIGHT   := 2
DEFINE PROCESSOR_INTEL_386     := 386
DEFINE PROCESSOR_INTEL_486     := 486
DEFINE PROCESSOR_INTEL_PENTIUM := 586
DEFINE PROCESSOR_MIPS_R4000    := 4000
DEFINE PROCESSOR_ALPHA_21064   := 21064
DEFINE PROCESSOR_ARCHITECTURE_INTEL := 0
DEFINE PROCESSOR_ARCHITECTURE_MIPS  := 1
DEFINE PROCESSOR_ARCHITECTURE_ALPHA := 2
DEFINE PROCESSOR_ARCHITECTURE_PPC   := 3
DEFINE PROCESSOR_ARCHITECTURE_UNKNOWN := 0xFFFF   
DEFINE PROCESSOR_ARCHITECTURE_AMD64    := 9
DEFINE PROCESSOR_ARCHITECTURE_IA64    := 6
DEFINE SECTION_QUERY       := 0x0001
DEFINE SECTION_MAP_WRITE   := 0x0002
DEFINE SECTION_MAP_READ    := 0x0004
DEFINE SECTION_MAP_EXECUTE := 0x0008
DEFINE SECTION_EXTEND_SIZE := 0x0010
DEFINE SECTION_ALL_ACCESS := 0x000F001Fl
DEFINE PAGE_NOACCESS          := 0x01
DEFINE PAGE_READONLY          := 0x02
DEFINE PAGE_READWRITE         := 0x04
DEFINE PAGE_WRITECOPY         := 0x08
DEFINE PAGE_EXECUTE           := 0x10
DEFINE PAGE_EXECUTE_READ      := 0x20
DEFINE PAGE_EXECUTE_READWRITE := 0x40
DEFINE PAGE_EXECUTE_WRITECOPY := 0x80
DEFINE PAGE_GUARD            := 0x100
DEFINE PAGE_NOCACHE          := 0x200
DEFINE MEM_COMMIT           := 0x1000
DEFINE MEM_RESERVE          := 0x2000
DEFINE MEM_DECOMMIT         := 0x4000
DEFINE MEM_RELEASE          := 0x8000
DEFINE MEM_FREE            := 0x10000
DEFINE MEM_PRIVATE         := 0x20000
DEFINE MEM_MAPPED          := 0x40000
DEFINE MEM_RESET           := 0x80000     
DEFINE MEM_TOP_DOWN       := 0x100000
DEFINE MEM_WRITE_WATCH    := 0x200000     
DEFINE MEM_PHYSICAL       := 0x400000     
DEFINE MEM_ROTATE         := 0x800000     
DEFINE MEM_LARGE_PAGES  := 0x20000000     
DEFINE MEM_4MB_PAGES    := 0x80000000     
DEFINE SEC_FILE           := 0x800000
DEFINE SEC_IMAGE         := 0x1000000
DEFINE SEC_PROTECTED_IMAGE  := 0x2000000     
DEFINE SEC_RESERVE       := 0x4000000
DEFINE SEC_COMMIT        := 0x8000000
DEFINE SEC_NOCACHE      := 0x10000000
DEFINE SEC_WRITECOMBINE := 0x40000000     
DEFINE SEC_LARGE_PAGES  := 0x80000000     
DEFINE MEM_IMAGE        :=  SEC_IMAGE
DEFINE WRITE_WATCH_FLAG_RESET := 0x01     
//
// Define access rights to files and directories
//
//
// The FILE_READ_DATA and FILE_WRITE_DATA constants are also defined in
// devioctl.h as FILE_READ_ACCESS and FILE_WRITE_ACCESS. The values for these
// constants *MUST* always be in sync.
// The values are redefined in devioctl.h because they must be available to
// both DOS and NT.
//
DEFINE FILE_READ_DATA            := 0x0001
DEFINE FILE_LIST_DIRECTORY       := 0x0001
DEFINE FILE_WRITE_DATA            := 0x0002
DEFINE FILE_ADD_FILE              := 0x0002
DEFINE FILE_APPEND_DATA           := 0x0004
DEFINE FILE_ADD_SUBDIRECTORY      := 0x0004
DEFINE FILE_CREATE_PIPE_INSTANCE  := 0x0004
DEFINE FILE_READ_EA               := 0x0008
DEFINE FILE_READ_PROPERTIES       := FILE_READ_EA
DEFINE FILE_WRITE_EA              := 0x0010
DEFINE FILE_WRITE_PROPERTIES      :=  FILE_WRITE_EA
DEFINE FILE_EXECUTE               := 0x0020
DEFINE FILE_TRAVERSE              := 0x0020
DEFINE FILE_DELETE_CHILD          := 0x0040
DEFINE FILE_READ_ATTRIBUTES       := 0x0080
DEFINE FILE_WRITE_ATTRIBUTES      := 0x0100
DEFINE FILE_ALL_ACCESS          := (STANDARD_RIGHTS_REQUIRED | SYNCHRONIZE | 0x1FF)
DEFINE FILE_GENERIC_READ       :=   (STANDARD_RIGHTS_READ     | ;
                                   FILE_READ_DATA           | ;
                                   FILE_READ_ATTRIBUTES     | ;
                                   FILE_READ_EA             | ;
                                   SYNCHRONIZE)
DEFINE FILE_GENERIC_WRITE        := (STANDARD_RIGHTS_WRITE    |;
                                   FILE_WRITE_DATA          |;
                                   FILE_WRITE_ATTRIBUTES    |;
                                   FILE_WRITE_EA            |;
                                   FILE_APPEND_DATA         |;
                                   SYNCHRONIZE)
DEFINE FILE_GENERIC_EXECUTE      := (STANDARD_RIGHTS_EXECUTE  |;
                                   FILE_READ_ATTRIBUTES     |;
                                   FILE_EXECUTE             |;
                                   SYNCHRONIZE)
DEFINE FILE_SHARE_READ                 := 0x00000001
DEFINE FILE_SHARE_WRITE                := 0x00000002
DEFINE FILE_SHARE_DELETE               := 0x00000004  
DEFINE FILE_ATTRIBUTE_READONLY         := 0x00000001
DEFINE FILE_ATTRIBUTE_HIDDEN           := 0x00000002
DEFINE FILE_ATTRIBUTE_SYSTEM           := 0x00000004
DEFINE FILE_ATTRIBUTE_DIRECTORY        := 0x00000010
DEFINE FILE_ATTRIBUTE_ARCHIVE          := 0x00000020
DEFINE FILE_ATTRIBUTE_DEVICE               := 0x00000040  
DEFINE FILE_ATTRIBUTE_NORMAL           := 0x00000080
DEFINE FILE_ATTRIBUTE_TEMPORARY        := 0x00000100
DEFINE FILE_ATTRIBUTE_SPARSE_FILE          := 0x00000200  
DEFINE FILE_ATTRIBUTE_REPARSE_POINT        := 0x00000400  
DEFINE FILE_ATTRIBUTE_COMPRESSED       := 0x00000800
DEFINE FILE_ATTRIBUTE_OFFLINE              := 0x00001000  
DEFINE FILE_ATTRIBUTE_NOT_CONTENT_INDEXED  := 0x00002000  
DEFINE FILE_ATTRIBUTE_ENCRYPTED            := 0x00004000  
DEFINE FILE_ATTRIBUTE_VIRTUAL              := 0x00010000  
DEFINE FILE_NOTIFY_CHANGE_FILE_NAME    := 0x00000001
DEFINE FILE_NOTIFY_CHANGE_DIR_NAME     := 0x00000002
DEFINE FILE_NOTIFY_CHANGE_ATTRIBUTES   := 0x00000004
DEFINE FILE_NOTIFY_CHANGE_SIZE         := 0x00000008
DEFINE FILE_NOTIFY_CHANGE_LAST_WRITE   := 0x00000010
DEFINE FILE_NOTIFY_CHANGE_LAST_ACCESS  := 0x00000020   
DEFINE FILE_NOTIFY_CHANGE_CREATION     := 0x00000040   
DEFINE FILE_NOTIFY_CHANGE_SECURITY     := 0x00000100
DEFINE FILE_ACTION_ADDED                   := 0x00000001   
DEFINE FILE_ACTION_REMOVED                 := 0x00000002   
DEFINE FILE_ACTION_MODIFIED                := 0x00000003   
DEFINE FILE_ACTION_RENAMED_OLD_NAME        := 0x00000004   
DEFINE FILE_ACTION_RENAMED_NEW_NAME        := 0x00000005   
DEFINE MAILSLOT_NO_MESSAGE             := -1
DEFINE MAILSLOT_WAIT_FOREVER           := -1
DEFINE FILE_CASE_SENSITIVE_SEARCH      := 0x00000001
DEFINE FILE_CASE_PRESERVED_NAMES       := 0x00000002
DEFINE FILE_UNICODE_ON_DISK            := 0x00000004
DEFINE FILE_PERSISTENT_ACLS            := 0x00000008
DEFINE FILE_FILE_COMPRESSION           := 0x00000010
DEFINE FILE_VOLUME_QUOTAS              := 0x00000020  
DEFINE FILE_SUPPORTS_SPARSE_FILES      := 0x00000040  
DEFINE FILE_SUPPORTS_REPARSE_POINTS    := 0x00000080  
DEFINE FILE_SUPPORTS_REMOTE_STORAGE    := 0x00000100  
DEFINE FILE_VOLUME_IS_COMPRESSED       := 0x00008000
DEFINE FILE_SUPPORTS_OBJECT_IDS        := 0x00010000  
DEFINE FILE_SUPPORTS_ENCRYPTION        := 0x00020000  
DEFINE FILE_NAMED_STREAMS              := 0x00040000  
DEFINE FILE_READ_ONLY_VOLUME           := 0x00080000  
DEFINE FILE_SEQUENTIAL_WRITE_ONCE      := 0x00100000  
DEFINE FILE_SUPPORTS_TRANSACTIONS      :=  0x00200000  
////////////////////////////////////////////////////////////////////////
//                                                                    //
//                             ACCESS MASK                            //
//                                                                    //
////////////////////////////////////////////////////////////////////////
//
//  Define the access mask as a longword sized structure divided up as
//  follows:
//
//       3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1
//       1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
//      +---------------+---------------+-------------------------------+
//      |G|G|G|G|Res'd|A| StandardRights|         SpecificRights        |
//      |R|W|E|A|     |S|               |                               |
//      +-+-------------+---------------+-------------------------------+
//
//      typedef struct _ACCESS_MASK {
//          WORD   SpecificRights;
//          BYTE  StandardRights;
//          BYTE  AccessSystemAcl : 1;
//          BYTE  Reserved : 3;
//          BYTE  GenericAll : 1;
//          BYTE  GenericExecute : 1;
//          BYTE  GenericWrite : 1;
//          BYTE  GenericRead : 1;
//      } ACCESS_MASK;
//      typedef ACCESS_MASK *PACCESS_MASK;
//
//  but to make life simple for programmer's we'll allow them to specify
//  a desired access mask by simply OR'ing together mulitple single rights
//  and treat an access mask as a DWORD.  For example
//
//      DesiredAccess = DELETE | READ_CONTROL
//
//  So we'll declare ACCESS_MASK as DWORD
//
// begin_wdm
////////////////////////////////////////////////////////////////////////
//                                                                    //
//                             ACCESS TYPES                           //
//                                                                    //
////////////////////////////////////////////////////////////////////////
// begin_wdm
//
//  The following are masks for the predefined standard access types
//
DEFINE DELETE                     :=  0x00010000L
DEFINE READ_CONTROL               :=  0x00020000L
DEFINE WRITE_DAC                  :=  0x00040000L
DEFINE WRITE_OWNER                :=  0x00080000L
DEFINE SYNCHRONIZE                :=  0x00100000L
DEFINE STANDARD_RIGHTS_REQUIRED   :=  0x000F0000L
DEFINE STANDARD_RIGHTS_READ       :=  (READ_CONTROL)
DEFINE STANDARD_RIGHTS_WRITE      :=  (READ_CONTROL)
DEFINE STANDARD_RIGHTS_EXECUTE    :=  (READ_CONTROL)
DEFINE STANDARD_RIGHTS_ALL       :=  0x001F0000L
DEFINE SPECIFIC_RIGHTS_ALL       :=    0x0000FFFFL
//
// AccessSystemAcl access type
//
DEFINE ACCESS_SYSTEM_SECURITY    :=0x01000000L
//
// MaximumAllowed access type
//
DEFINE MAXIMUM_ALLOWED          :=        0x02000000L
//
//  These are the generic rights.
//
DEFINE GENERIC_READ                     :=0x80000000L
DEFINE GENERIC_WRITE                    :=0x40000000L
DEFINE GENERIC_EXECUTE                 := 0x20000000L
DEFINE GENERIC_ALL                      :=0x10000000L
//
//  Define the generic mapping array.  This is used to denote the
//  mapping of each generic access right to a specific access mask.
//
DEFINE SID_REVISION                     := 1
DEFINE SID_MAX_SUB_AUTHORITIES          := 15
DEFINE SID_RECOMMENDED_SUB_AUTHORITIES  := 1
DEFINE SidTypeUser            := 1
DEFINE SidTypeGroup           := 2
DEFINE SidTypeDomain          := 3
DEFINE SidTypeAlias           := 4
DEFINE SidTypeWellKnownGroup  := 5
DEFINE SidTypeDeletedAccount  := 6
DEFINE SidTypeInvalid         := 7
DEFINE SidTypeUnknown        := 8
DEFINE SECURITY_NULL_RID               := 0x00000000L
DEFINE SECURITY_WORLD_RID              := 0x00000000L
DEFINE SECURITY_LOCAL_RID              := 0X00000000L
DEFINE SECURITY_CREATOR_OWNER_RID      := 0x00000000L
DEFINE SECURITY_CREATOR_GROUP_RID      := 0x00000001L
DEFINE SECURITY_DIALUP_RID             := 0x00000001L
DEFINE SECURITY_NETWORK_RID            := 0x00000002L
DEFINE SECURITY_BATCH_RID              := 0x00000003L
DEFINE SECURITY_INTERACTIVE_RID        := 0x00000004L
DEFINE SECURITY_SERVICE_RID            := 0x00000006L
DEFINE SECURITY_ANONYMOUS_LOGON_RID    := 0x00000007L
DEFINE SECURITY_LOGON_IDS_RID          := 0x00000005L
DEFINE SECURITY_LOGON_IDS_RID_COUNT    := 3L
DEFINE SECURITY_LOCAL_SYSTEM_RID       := 0x00000012L
DEFINE SECURITY_NT_NON_UNIQUE          := 0x00000015L
DEFINE SECURITY_BUILTIN_DOMAIN_RID     := 0x00000020L
DEFINE DOMAIN_USER_RID_ADMIN          :=0x000001F4L
DEFINE DOMAIN_USER_RID_GUEST          :=0x000001F5L
DEFINE DOMAIN_GROUP_RID_ADMINS        :=0x00000200L
DEFINE DOMAIN_GROUP_RID_USERS         :=0x00000201L
DEFINE DOMAIN_GROUP_RID_GUESTS       := 0x00000202L
DEFINE DOMAIN_ALIAS_RID_ADMINS       := 0x00000220L
DEFINE DOMAIN_ALIAS_RID_USERS         :=0x00000221L
DEFINE DOMAIN_ALIAS_RID_GUESTS        :=0x00000222L
DEFINE DOMAIN_ALIAS_RID_POWER_USERS  := 0x00000223L
DEFINE DOMAIN_ALIAS_RID_ACCOUNT_OPS   :=0x00000224L
DEFINE DOMAIN_ALIAS_RID_SYSTEM_OPS    :=0x00000225L
DEFINE DOMAIN_ALIAS_RID_PRINT_OPS     :=0x00000226L
DEFINE DOMAIN_ALIAS_RID_BACKUP_OPS    :=0x00000227L
DEFINE DOMAIN_ALIAS_RID_REPLICATOR    :=0x00000228L
DEFINE SE_GROUP_MANDATORY              := 0x00000001L
DEFINE SE_GROUP_ENABLED_BY_DEFAULT     := 0x00000002L
DEFINE SE_GROUP_ENABLED                := 0x00000004L
DEFINE SE_GROUP_OWNER                  := 0x00000008L
DEFINE SE_GROUP_LOGON_ID               := 0xC0000000L
DEFINE ACL_REVISION     := 2
DEFINE ACL_REVISION1   := 1
DEFINE ACL_REVISION2   := 2
DEFINE ACCESS_ALLOWED_ACE_TYPE         :=  0x0
DEFINE ACCESS_DENIED_ACE_TYPE          :=  0x1
DEFINE SYSTEM_AUDIT_ACE_TYPE           :=  0x2
DEFINE SYSTEM_ALARM_ACE_TYPE           :=  0x3
DEFINE OBJECT_INHERIT_ACE                := 0x1
DEFINE CONTAINER_INHERIT_ACE             := 0x2
DEFINE NO_PROPAGATE_INHERIT_ACE          := 0x4
DEFINE INHERIT_ONLY_ACE                  := 0x8
DEFINE VALID_INHERIT_FLAGS               := 0xF
DEFINE SUCCESSFUL_ACCESS_ACE_FLAG      := 0x40
DEFINE FAILED_ACCESS_ACE_FLAG          :=  0x80
DEFINE AclRevisionInformaton := 1
DEFINE AclSizeInformation := 2
DEFINE SECURITY_DESCRIPTOR_REVISION     := 1
DEFINE SECURITY_DESCRIPTOR_REVISION1    := 1
DEFINE SECURITY_DESCRIPTOR_MIN_LENGTH   := 20
DEFINE SE_OWNER_DEFAULTED               := 0x0001
DEFINE SE_GROUP_DEFAULTED               := 0x0002
DEFINE SE_DACL_PRESENT                  := 0x0004
DEFINE SE_DACL_DEFAULTED                := 0x0008
DEFINE SE_SACL_PRESENT                  := 0x0010
DEFINE SE_SACL_DEFAULTED                := 0x0020
DEFINE SE_SELF_RELATIVE                 := 0x8000
DEFINE SE_PRIVILEGE_ENABLED_BY_DEFAULT := 0x00000001
DEFINE SE_PRIVILEGE_ENABLED            := 0x00000002
DEFINE SE_PRIVILEGE_USED_FOR_ACCESS    := 0x80000000
DEFINE PRIVILEGE_SET_ALL_NECESSARY    := 1
DEFINE SE_CREATE_TOKEN_NAME := "SeCreateTokenPrivilege"
DEFINE SE_ASSIGNPRIMARYTOKEN_NAME   := "SeAssignPrimaryTokenPrivilege"
DEFINE SE_LOCK_MEMORY_NAME          := "SeLockMemoryPrivilege"
DEFINE SE_INCREASE_QUOTA_NAME       := "SeIncreaseQuotaPrivilege"
DEFINE SE_UNSOLICITED_INPUT_NAME    := "SeUnsolicitedInputPrivilege"
DEFINE SE_MACHINE_ACCOUNT_NAME      := "SeMachineAccountPrivilege"
DEFINE SE_TCB_NAME                  := "SeTcbPrivilege"
DEFINE SE_SECURITY_NAME             := "SeSecurityPrivilege"
DEFINE SE_TAKE_OWNERSHIP_NAME       := "SeTakeOwnershipPrivilege"
DEFINE SE_LOAD_DRIVER_NAME          := "SeLoadDriverPrivilege"
DEFINE SE_SYSTEM_PROFILE_NAME       := "SeSystemProfilePrivilege"
DEFINE SE_SYSTEMTIME_NAME           := "SeSystemtimePrivilege"
DEFINE SE_PROF_SINGLE_PROCESS_NAME  := "SeProfileSingleProcessPrivilege"
DEFINE SE_INC_BASE_PRIORITY_NAME    := "SeIncreaseBasePriorityPrivilege"
DEFINE SE_CREATE_PAGEFILE_NAME      := "SeCreatePagefilePrivilege"
DEFINE SE_CREATE_PERMANENT_NAME     := "SeCreatePermanentPrivilege"
DEFINE SE_BACKUP_NAME               := "SeBackupPrivilege"
DEFINE SE_RESTORE_NAME              := "SeRestorePrivilege"
DEFINE SE_SHUTDOWN_NAME             := "SeShutdownPrivilege"
DEFINE SE_DEBUG_NAME                := "SeDebugPrivilege"
DEFINE SE_AUDIT_NAME                := "SeAuditPrivilege"
DEFINE SE_SYSTEM_ENVIRONMENT_NAME   := "SeSystemEnvironmentPrivilege"
DEFINE SE_CHANGE_NOTIFY_NAME        := "SeChangeNotifyPrivilege"
DEFINE SE_REMOTE_SHUTDOWN_NAME      := "SeRemoteShutdownPrivilege"
DEFINE SecurityAnonymous := 0
DEFINE SecurityIdentification := 1
DEFINE SecurityImpersonation := 2
DEFINE SecurityDelegation :=3
DEFINE SECURITY_MAX_IMPERSONATION_LEVEL := SecurityDelegation
DEFINE DEFAULT_IMPERSONATION_LEVEL := SecurityImpersonation
DEFINE SECURITY_DYNAMIC_TRACKING      := TRUE
DEFINE SECURITY_STATIC_TRACKING       := FALSE
DEFINE TOKEN_ASSIGN_PRIMARY    := 0x0001
DEFINE TOKEN_DUPLICATE         := 0x0002
DEFINE TOKEN_IMPERSONATE       := 0x0004
DEFINE TOKEN_QUERY             := 0x0008
DEFINE TOKEN_QUERY_SOURCE      := 0x0010
DEFINE TOKEN_ADJUST_PRIVILEGES := 0x0020
DEFINE TOKEN_ADJUST_GROUPS     := 0x0040
DEFINE TOKEN_ADJUST_DEFAULT    := 0x0080
DEFINE TOKEN_ADJUST_SESSIONID  := (0x0100)
DEFINE TOKEN_ALL_ACCESS_P := (STANDARD_RIGHTS_REQUIRED  |;
                          TOKEN_ASSIGN_PRIMARY      |;
                          TOKEN_DUPLICATE           |;
                          TOKEN_IMPERSONATE         |;
                          TOKEN_QUERY               |;
                          TOKEN_QUERY_SOURCE        |;
                          TOKEN_ADJUST_PRIVILEGES   |;
                          TOKEN_ADJUST_GROUPS       |;
                          TOKEN_ADJUST_DEFAULT )
DEFINE TOKEN_ALL_ACCESS  := (TOKEN_ALL_ACCESS_P |;
                          TOKEN_ADJUST_SESSIONID )
DEFINE TOKEN_READ       := (STANDARD_RIGHTS_READ      |;
                          TOKEN_QUERY)
DEFINE TOKEN_WRITE      := (STANDARD_RIGHTS_WRITE     |;
                          TOKEN_ADJUST_PRIVILEGES   |;
                          TOKEN_ADJUST_GROUPS       |;
                          TOKEN_ADJUST_DEFAULT)
DEFINE TOKEN_EXECUTE    := (STANDARD_RIGHTS_EXECUTE)
//
//
// Token Types
//
DEFINE TokenPrimary    := 1
DEFINE TokenImpersonation  := 2
//
// Token elevation values describe the relative strength of a given token.
// A full token is a token with all groups and privileges to which the principal
// is authorized.  A limited token is one with some groups or privileges removed.
//
DEFINE TokenElevationTypeDefault := 1
DEFINE TokenElevationTypeFull   := 2
DEFINE TokenElevationTypeLimited := 3
//
// Token Information Classes.
//
DEFINE TokenUser                  :=   1
DEFINE TokenGroups                :=   2
DEFINE TokenPrivileges            :=   3
DEFINE TokenOwner                 :=   4
DEFINE TokenPrimaryGroup          :=   5
DEFINE TokenDefaultDacl           :=   6
DEFINE TokenSource                :=   7
DEFINE TokenType                  :=   8
DEFINE TokenImpersonationLevel    :=   9
DEFINE TokenStatistics            :=   10
DEFINE TokenRestrictedSids := 11
DEFINE TokenSessionId       := 12
DEFINE TokenGroupsAndPrivileges:= 13
DEFINE TokenSessionReference:= 14
DEFINE TokenSandBoxInert:= 15
DEFINE TokenAuditPolicy:= 16
DEFINE TokenOrigin:= 17
DEFINE TokenElevationType:= 18
DEFINE TokenLinkedToken:= 19
DEFINE TokenElevation:= 20
DEFINE TokenHasRestrictions:= 21
DEFINE TokenAccessInformation := 22
DEFINE TokenVirtualizationAllowed:= 23
DEFINE TokenVirtualizationEnabled:= 24
DEFINE TokenIntegrityLevel:= 25
DEFINE TokenUIAccess:= 26
DEFINE TokenMandatoryPolicy:= 27
DEFINE TokenLogonSid:= 28
DEFINE MaxTokenInfoClass:= 29  // MaxTokenInfoClass should always be the last enum
//
// Token information class structures
//
DEFINE TOKEN_SOURCE_LENGTH  := 8
DEFINE DISABLE_MAX_PRIVILEGE   := 0x1 
DEFINE SANDBOX_INERT           := 0x2 
DEFINE LUA_TOKEN               := 0x4 
DEFINE WRITE_RESTRICTED        := 0x8 
DEFINE OWNER_SECURITY_INFORMATION       := 0X00000001L
DEFINE GROUP_SECURITY_INFORMATION       := 0X00000002L
DEFINE DACL_SECURITY_INFORMATION        := 0X00000004L
DEFINE SACL_SECURITY_INFORMATION        := 0X00000008L
DEFINE LABEL_SECURITY_INFORMATION       := (0x00000010L)
DEFINE PROTECTED_DACL_SECURITY_INFORMATION     := (0x80000000L)
DEFINE PROTECTED_SACL_SECURITY_INFORMATION     := (0x40000000L)
DEFINE UNPROTECTED_DACL_SECURITY_INFORMATION   := (0x20000000L)
DEFINE UNPROTECTED_SACL_SECURITY_INFORMATION   := (0x10000000L)
DEFINE PROCESS_SET_SESSIONID             :=  (0x0004)  
DEFINE PROCESS_SUSPEND_RESUME            :=  (0x0800)  
DEFINE PROCESS_QUERY_LIMITED_INFORMATION :=  (0x1000)  
//
// Image Format
//
DEFINE IMAGE_DOS_SIGNATURE                 := 0x5A4D
DEFINE IMAGE_OS2_SIGNATURE                 := 0x454E
DEFINE IMAGE_OS2_SIGNATURE_LE              := 0x454C
DEFINE IMAGE_VXD_SIGNATURE                 := 0x454C
DEFINE IMAGE_NT_SIGNATURE                  := 0x00004550
// PshPack 2
DEFINE IMAGE_SIZEOF_FILE_HEADER             := 20
DEFINE IMAGE_FILE_RELOCS_STRIPPED           	:= 0x0001  // Relocation info stripped from file.                                   
DEFINE IMAGE_FILE_EXECUTABLE_IMAGE          	:= 0x0002  // File is executable  (i.e. no unresolved externel references).         
DEFINE IMAGE_FILE_LINE_NUMS_STRIPPED        	:= 0x0004  // Line nunbers stripped from file.                                      
DEFINE IMAGE_FILE_LOCAL_SYMS_STRIPPED       	:= 0x0008  // Local symbols stripped from file.                                     
DEFINE IMAGE_FILE_AGGRESIVE_WS_TRIM         	:= 0x0010  // Agressively trim working set                                          
DEFINE IMAGE_FILE_LARGE_ADDRESS_AWARE        := 0x0020  // App can handle >2gb addresses                                         
DEFINE IMAGE_FILE_BYTES_REVERSED_LO    		:= 0x0080  // Bytes of machine word are reversed.                                             
DEFINE IMAGE_FILE_32BIT_MACHINE              := 0x0100  // 32 bit word machine.                                                            
DEFINE IMAGE_FILE_DEBUG_STRIPPED           	:= 0x0200  // Debugging info stripped from file in .DBG file                                  
DEFINE IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP   	:= 0x0400  // If Image is on removable media, copy and run from the swap file.                 
DEFINE IMAGE_FILE_NET_RUN_FROM_SWAP         	:= 0x0800  // If Image is on Net, copy and run from the swap file.                            
DEFINE IMAGE_FILE_SYSTEM                     := 0x1000  // System File.                                                                    
DEFINE IMAGE_FILE_DLL                        := 0x2000  // File is a DLL.                                                                  
DEFINE IMAGE_FILE_UP_SYSTEM_ONLY            	:= 0x4000  // File should only be run on a UP machine                                         
DEFINE IMAGE_FILE_BYTES_REVERSED_HI     		:= 0x8000  // Bytes of machine word are reversed.                                             
DEFINE IMAGE_FILE_MACHINE_UNKNOWN        :=    0
DEFINE IMAGE_FILE_MACHINE_I386           :=    0x014c  // Intel 386.
DEFINE IMAGE_FILE_MACHINE_R3000          :=    0x0162  // MIPS little-endian, 0x160 big-endian
DEFINE IMAGE_FILE_MACHINE_R4000          :=    0x0166  // MIPS little-endian
DEFINE IMAGE_FILE_MACHINE_R10000         :=    0x0168  // MIPS little-endian      
DEFINE IMAGE_FILE_MACHINE_WCEMIPSV2      :=    0x0169  // MIPS little-endian WCE v2      
DEFINE IMAGE_FILE_MACHINE_ALPHA          :=    0x0184  // Alpha_AXP      
DEFINE IMAGE_FILE_MACHINE_SH3            :=    0x01a2  // SH3 little-endian      
DEFINE IMAGE_FILE_MACHINE_SH3DSP         :=    0x01a3      
DEFINE IMAGE_FILE_MACHINE_SH3E           :=    0x01a4  // SH3E little-endian      
DEFINE IMAGE_FILE_MACHINE_SH4            :=    0x01a6  // SH4 little-endian      
DEFINE IMAGE_FILE_MACHINE_SH5            :=    0x01a8  // SH5      
DEFINE IMAGE_FILE_MACHINE_ARM            :=    0x01c0  // ARM Little-Endian      
DEFINE IMAGE_FILE_MACHINE_THUMB          :=    0x01c2      
DEFINE IMAGE_FILE_MACHINE_AM33           :=    0x01d3      
DEFINE IMAGE_FILE_MACHINE_POWERPC        :=    0x01F0  // IBM PowerPC Little-Endian      
DEFINE IMAGE_FILE_MACHINE_POWERPCFP      :=    0x01f1      
DEFINE IMAGE_FILE_MACHINE_IA64           :=    0x0200  // Intel 64      
DEFINE IMAGE_FILE_MACHINE_MIPS16         :=    0x0266  // MIPS      
DEFINE IMAGE_FILE_MACHINE_ALPHA64        :=    0x0284  // ALPHA64      
DEFINE IMAGE_FILE_MACHINE_MIPSFPU        :=    0x0366  // MIPS      
DEFINE IMAGE_FILE_MACHINE_MIPSFPU16      :=    0x0466  // MIPS      
DEFINE IMAGE_FILE_MACHINE_AXP64          :=    IMAGE_FILE_MACHINE_ALPHA64      
DEFINE IMAGE_FILE_MACHINE_TRICORE        :=    0x0520  // Infineon      
DEFINE IMAGE_FILE_MACHINE_CEF            :=    0x0CEF      
DEFINE IMAGE_FILE_MACHINE_EBC            :=    0x0EBC  // EFI Byte Code
DEFINE IMAGE_FILE_MACHINE_AMD64          :=    0x8664  // AMD64 (K8)
DEFINE IMAGE_FILE_MACHINE_M32R           :=    0x9041  // M32R little-endian
DEFINE IMAGE_FILE_MACHINE_CEE            :=    0xC0EE
//
// Directory format.
//
DEFINE IMAGE_NUMBEROF_DIRECTORY_ENTRIES    :=16
//
// Optional header format.
//
DEFINE IMAGE_SIZEOF_ROM_OPTIONAL_HEADER      :=56
DEFINE IMAGE_SIZEOF_STD_OPTIONAL_HEADER      :=28
DEFINE IMAGE_SIZEOF_NT_OPTIONAL_HEADER      :=224
DEFINE IMAGE_NT_OPTIONAL_HDR32_MAGIC      := 0x10b
DEFINE IMAGE_NT_OPTIONAL_HDR64_MAGIC      := 0x20b
DEFINE IMAGE_ROM_OPTIONAL_HDR_MAGIC       :=0x107
DEFINE IMAGE_SUBSYSTEM_UNKNOWN            :=  0
DEFINE IMAGE_SUBSYSTEM_NATIVE             :=   1
DEFINE IMAGE_SUBSYSTEM_WINDOWS_GUI        :=   2
DEFINE IMAGE_SUBSYSTEM_WINDOWS_CUI        :=   3
DEFINE IMAGE_SUBSYSTEM_OS2_CUI            :=   5
DEFINE IMAGE_SUBSYSTEM_POSIX_CUI          :=   7
DEFINE IMAGE_SUBSYSTEM_NATIVE_WINDOWS       := 8   // image is a native Win9x driver.
DEFINE IMAGE_SUBSYSTEM_WINDOWS_CE_GUI       := 9   // Image runs in the Windows CE subsystem.
DEFINE IMAGE_SUBSYSTEM_EFI_APPLICATION      := 10  //
DEFINE IMAGE_SUBSYSTEM_EFI_BOOT_SERVICE_DRIVER  := 11   //
DEFINE IMAGE_SUBSYSTEM_EFI_RUNTIME_DRIVER   := 12  //
DEFINE IMAGE_SUBSYSTEM_EFI_ROM              := 13
DEFINE IMAGE_SUBSYSTEM_XBOX                 := 14
DEFINE IMAGE_SUBSYSTEM_WINDOWS_BOOT_APPLICATION := 16
// DllCharacteristics Entries
//      IMAGE_LIBRARY_PROCESS_INIT            0x0001     // Reserved.
//      IMAGE_LIBRARY_PROCESS_TERM            0x0002     // Reserved.
//      IMAGE_LIBRARY_THREAD_INIT             0x0004     // Reserved.
//      IMAGE_LIBRARY_THREAD_TERM             0x0008     // Reserved.
DEFINE IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE := 0x0040     // DLL can move.
DEFINE IMAGE_DLLCHARACTERISTICS_FORCE_INTEGRITY    := 0x0080     // Code Integrity Image
DEFINE IMAGE_DLLCHARACTERISTICS_NX_COMPAT    := 0x0100     // Image is NX compatible
DEFINE IMAGE_DLLCHARACTERISTICS_NO_ISOLATION := 0x0200     // Image understands isolation and doesn't want it
DEFINE IMAGE_DLLCHARACTERISTICS_NO_SEH       := 0x0400     // Image does not use SEH.  No SE handler may reside in this image
DEFINE IMAGE_DLLCHARACTERISTICS_NO_BIND      := 0x0800     // Do not bind this image.
//                                            := 0x1000     // Reserved.
DEFINE IMAGE_DLLCHARACTERISTICS_WDM_DRIVER   := 0x2000     // Driver uses WDM model
//                                            := 0x4000     // Reserved.
DEFINE IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE     := 0x8000
// Directory Entries
DEFINE IMAGE_DIRECTORY_ENTRY_EXPORT        :=  0
DEFINE IMAGE_DIRECTORY_ENTRY_IMPORT        :=  1
DEFINE IMAGE_DIRECTORY_ENTRY_RESOURCE      :=  2
DEFINE IMAGE_DIRECTORY_ENTRY_EXCEPTION     :=  3
DEFINE IMAGE_DIRECTORY_ENTRY_SECURITY      :=  4
DEFINE IMAGE_DIRECTORY_ENTRY_BASERELOC     :=  5
DEFINE IMAGE_DIRECTORY_ENTRY_DEBUG         :=  6
//      IMAGE_DIRECTORY_ENTRY_COPYRIGHT       7   // (X86 usage)
DEFINE IMAGE_DIRECTORY_ENTRY_ARCHITECTURE    := 7   // Architecture Specific Data
DEFINE IMAGE_DIRECTORY_ENTRY_GLOBALPTR     :=  8
DEFINE IMAGE_DIRECTORY_ENTRY_TLS           :=  9
DEFINE IMAGE_DIRECTORY_ENTRY_LOAD_CONFIG   := 10
DEFINE IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT  := 11
DEFINE IMAGE_DIRECTORY_ENTRY_IAT           := 12
DEFINE IMAGE_DIRECTORY_ENTRY_DELAY_IMPORT   := 13   // Delay Load Import Descriptors
DEFINE IMAGE_DIRECTORY_ENTRY_COM_DESCRIPTOR := 14   // COM Runtime descriptor
//
// Section header format.
//
DEFINE IMAGE_SIZEOF_SHORT_NAME             := 8
DEFINE IMAGE_SIZEOF_SECTION_HEADER          :=  40
//
// Section characteristics.
//
//      IMAGE_SCN_TYPE_REG                   0x00000000  // Reserved.
//      IMAGE_SCN_TYPE_DSECT                 0x00000001  // Reserved.
//      IMAGE_SCN_TYPE_NOLOAD                0x00000002  // Reserved.
//      IMAGE_SCN_TYPE_GROUP                 0x00000004  // Reserved.
DEFINE IMAGE_SCN_TYPE_NO_PAD                 :=  0x00000008
//      IMAGE_SCN_TYPE_COPY                  0x00000010  // Reserved.
DEFINE IMAGE_SCN_CNT_CODE                   :=  0x00000020
DEFINE IMAGE_SCN_CNT_INITIALIZED_DATA       :=  0x00000040
DEFINE IMAGE_SCN_CNT_UNINITIALIZED_DATA     :=  0x00000080
DEFINE IMAGE_SCN_LNK_OTHER                  :=  0x00000100
DEFINE IMAGE_SCN_LNK_INFO                   :=  0x00000200
//      IMAGE_SCN_TYPE_OVER                  0x00000400  // Reserved.
DEFINE IMAGE_SCN_LNK_REMOVE                 :=  0x00000800
DEFINE IMAGE_SCN_LNK_COMDAT                 :=  0x00001000
//                                           0x00002000  // Reserved.
//      IMAGE_SCN_MEM_PROTECTED - Obsolete   0x00004000
DEFINE IMAGE_SCN_NO_DEFER_SPEC_EXC          := 0x00004000  // Reset speculative exceptions handling bits in the TLB entries for this section.
DEFINE IMAGE_SCN_GPREL                      := 0x00008000  // Section content can be accessed relative to GP
DEFINE IMAGE_SCN_MEM_FARDATA                :=  0x00008000
//      IMAGE_SCN_MEM_SYSHEAP  - Obsolete    0x00010000
DEFINE IMAGE_SCN_MEM_PURGEABLE              :=  0x00020000
DEFINE IMAGE_SCN_MEM_16BIT                  :=  0x00020000
DEFINE IMAGE_SCN_MEM_LOCKED                 :=  0x00040000
DEFINE IMAGE_SCN_MEM_PRELOAD                :=  0x00080000
DEFINE IMAGE_SCN_ALIGN_1BYTES               :=  0x00100000
DEFINE IMAGE_SCN_ALIGN_2BYTES               :=  0x00200000
DEFINE IMAGE_SCN_ALIGN_4BYTES               :=  0x00300000
DEFINE IMAGE_SCN_ALIGN_8BYTES               :=  0x00400000
DEFINE IMAGE_SCN_ALIGN_16BYTES              :=  0x00500000
DEFINE IMAGE_SCN_ALIGN_32BYTES              :=  0x00600000
DEFINE IMAGE_SCN_ALIGN_64BYTES              :=  0x00700000
DEFINE IMAGE_SCN_ALIGN_128BYTES             := 0x00800000  //
DEFINE IMAGE_SCN_ALIGN_256BYTES             := 0x00900000  //
DEFINE IMAGE_SCN_ALIGN_512BYTES             := 0x00A00000  //
DEFINE IMAGE_SCN_ALIGN_1024BYTES            := 0x00B00000  //
DEFINE IMAGE_SCN_ALIGN_2048BYTES            := 0x00C00000  //
DEFINE IMAGE_SCN_ALIGN_4096BYTES            := 0x00D00000  //
DEFINE IMAGE_SCN_ALIGN_8192BYTES            := 0x00E00000  //
// Unused                                   :=  0x00F00000
DEFINE IMAGE_SCN_ALIGN_MASK                 :=  0x00F00000
DEFINE IMAGE_SCN_LNK_NRELOC_OVFL            :=  0x01000000
DEFINE IMAGE_SCN_MEM_DISCARDABLE            :=  0x02000000
DEFINE IMAGE_SCN_MEM_NOT_CACHED             :=  0x04000000
DEFINE IMAGE_SCN_MEM_NOT_PAGED              :=  0x08000000
DEFINE IMAGE_SCN_MEM_SHARED                 :=  0x10000000
DEFINE IMAGE_SCN_MEM_EXECUTE                :=  0x20000000
DEFINE IMAGE_SCN_MEM_READ                   :=  0x40000000
DEFINE IMAGE_SCN_MEM_WRITE                  :=  0x80000000
//
// TLS Chaacteristic Flags
//
DEFINE IMAGE_SCN_SCALE_INDEX                :=  0x00000001  // Tls index is scaled
DEFINE IMAGE_SIZEOF_SYMBOL                  := 18
//
// Section values.
//
// Symbols have a section number of the section in which they are
// defined. Otherwise, section numbers have the following meanings:
//
DEFINE IMAGE_SYM_UNDEFINED          := SHORT(_CAST,0)
DEFINE IMAGE_SYM_ABSOLUTE           := -1
DEFINE IMAGE_SYM_DEBUG              := -2
DEFINE IMAGE_SYM_SECTION_MAX         := 0xFEFF            // Values 0xFF00-0xFFFF are special
//
// Type (fundamental) values.
//
DEFINE IMAGE_SYM_TYPE_NULL                 := 0x0000
DEFINE IMAGE_SYM_TYPE_VOID                 := 0x0001
DEFINE IMAGE_SYM_TYPE_CHAR                 := 0x0002
DEFINE IMAGE_SYM_TYPE_SHORT                := 0x0003
DEFINE IMAGE_SYM_TYPE_INT                  := 0x0004
DEFINE IMAGE_SYM_TYPE_LONG                 := 0x0005
DEFINE IMAGE_SYM_TYPE_FLOAT                := 0x0006
DEFINE IMAGE_SYM_TYPE_DOUBLE               := 0x0007
DEFINE IMAGE_SYM_TYPE_STRUCT               := 0x0008
DEFINE IMAGE_SYM_TYPE_UNION                := 0x0009
DEFINE IMAGE_SYM_TYPE_ENUM                 := 0x000A
DEFINE IMAGE_SYM_TYPE_MOE                  := 0x000B
DEFINE IMAGE_SYM_TYPE_BYTE                 := 0x000C
DEFINE IMAGE_SYM_TYPE_WORD                 := 0x000D
DEFINE IMAGE_SYM_TYPE_UINT                 := 0x000E
DEFINE IMAGE_SYM_TYPE_DWORD                := 0x000F
DEFINE IMAGE_SYM_TYPE_PCODE                := 0x8000
//
// Type (derived) values.
//
DEFINE IMAGE_SYM_DTYPE_NULL               :=  0
DEFINE IMAGE_SYM_DTYPE_POINTER            :=  1
DEFINE IMAGE_SYM_DTYPE_FUNCTION           :=  2
DEFINE IMAGE_SYM_DTYPE_ARRAY              :=  3
//
// Storage classes.
//
DEFINE IMAGE_SYM_CLASS_END_OF_FUNCTION     := -1
DEFINE IMAGE_SYM_CLASS_NULL                := 0x0000
DEFINE IMAGE_SYM_CLASS_AUTOMATIC           := 0x0001
DEFINE IMAGE_SYM_CLASS_EXTERNAL            := 0x0002
DEFINE IMAGE_SYM_CLASS_STATIC              := 0x0003
DEFINE IMAGE_SYM_CLASS_REGISTER            := 0x0004
DEFINE IMAGE_SYM_CLASS_EXTERNAL_DEF        := 0x0005
DEFINE IMAGE_SYM_CLASS_LABEL               := 0x0006
DEFINE IMAGE_SYM_CLASS_UNDEFINED_LABEL     := 0x0007
DEFINE IMAGE_SYM_CLASS_MEMBER_OF_STRUCT    := 0x0008
DEFINE IMAGE_SYM_CLASS_ARGUMENT            := 0x0009
DEFINE IMAGE_SYM_CLASS_STRUCT_TAG          := 0x000A
DEFINE IMAGE_SYM_CLASS_MEMBER_OF_UNION     := 0x000B
DEFINE IMAGE_SYM_CLASS_UNION_TAG           := 0x000C
DEFINE IMAGE_SYM_CLASS_TYPE_DEFINITION     := 0x000D
DEFINE IMAGE_SYM_CLASS_UNDEFINED_STATIC    := 0x000E
DEFINE IMAGE_SYM_CLASS_ENUM_TAG            := 0x000F
DEFINE IMAGE_SYM_CLASS_MEMBER_OF_ENUM      := 0x0010
DEFINE IMAGE_SYM_CLASS_REGISTER_PARAM      := 0x0011
DEFINE IMAGE_SYM_CLASS_BIT_FIELD           := 0x0012
DEFINE IMAGE_SYM_CLASS_FAR_EXTERNAL        := 0x0044
DEFINE IMAGE_SYM_CLASS_BLOCK               := 0x0064
DEFINE IMAGE_SYM_CLASS_FUNCTION            := 0x0065
DEFINE IMAGE_SYM_CLASS_END_OF_STRUCT       := 0x0066
DEFINE IMAGE_SYM_CLASS_FILE                := 0x0067
// new
DEFINE IMAGE_SYM_CLASS_SECTION             := 0x0068
DEFINE IMAGE_SYM_CLASS_WEAK_EXTERNAL       := 0x0069
DEFINE IMAGE_SYM_CLASS_CLR_TOKEN           := 0x006B
// type packing constants
DEFINE N_BTMASK                            := 0x000F
DEFINE N_TMASK                             := 0x0030
DEFINE N_TMASK1                            := 0x00C0
DEFINE N_TMASK2                            := 0x00F0
DEFINE N_BTSHFT                            := 4
DEFINE N_TSHIFT                            := 2
DEFINE IMAGE_SIZEOF_AUX_SYMBOL            := 18
//
// Communal selection types.
//
DEFINE IMAGE_COMDAT_SELECT_NODUPLICATES   := 1
DEFINE IMAGE_COMDAT_SELECT_ANY            := 2
DEFINE IMAGE_COMDAT_SELECT_SAME_SIZE      := 3
DEFINE IMAGE_COMDAT_SELECT_EXACT_MATCH    := 4
DEFINE IMAGE_COMDAT_SELECT_ASSOCIATIVE    := 5
DEFINE IMAGE_COMDAT_SELECT_LARGEST        := 6
DEFINE IMAGE_COMDAT_SELECT_NEWEST         := 7
DEFINE IMAGE_WEAK_EXTERN_SEARCH_NOLIBRARY := 1
DEFINE IMAGE_WEAK_EXTERN_SEARCH_LIBRARY   := 2
DEFINE IMAGE_WEAK_EXTERN_SEARCH_ALIAS     := 3
//
// Relocation format.
//
DEFINE IMAGE_SIZEOF_RELOCATION              := 10
DEFINE IMAGE_REL_I386_ABSOLUTE         := 0x0000
DEFINE IMAGE_REL_I386_DIR16            := 0x0001
DEFINE IMAGE_REL_I386_REL16            := 0x0002
DEFINE IMAGE_REL_I386_DIR32            := 0x0006
DEFINE IMAGE_REL_I386_DIR32NB          := 0x0007
DEFINE IMAGE_REL_I386_SEG12            := 0x0009
DEFINE IMAGE_REL_I386_SECTION          := 0x000A
DEFINE IMAGE_REL_I386_SECREL           := 0x000B
DEFINE IMAGE_REL_I386_TOKEN            := 0x000C  // clr token
DEFINE IMAGE_REL_I386_SECREL7          := 0x000D  // 7 bit offset from base of section containing target
DEFINE IMAGE_REL_I386_REL32            := 0x0014
DEFINE IMAGE_REL_MIPS_ABSOLUTE         := 0x0000
DEFINE IMAGE_REL_MIPS_REFHALF          := 0x0001
DEFINE IMAGE_REL_MIPS_REFWORD          := 0x0002
DEFINE IMAGE_REL_MIPS_JMPADDR          := 0x0003
DEFINE IMAGE_REL_MIPS_REFHI            := 0x0004
DEFINE IMAGE_REL_MIPS_REFLO            := 0x0005
DEFINE IMAGE_REL_MIPS_GPREL            := 0x0006
DEFINE IMAGE_REL_MIPS_LITERAL          := 0x0007
DEFINE IMAGE_REL_MIPS_SECTION          := 0x000A
DEFINE IMAGE_REL_MIPS_SECREL           := 0x000B
DEFINE IMAGE_REL_MIPS_SECRELLO         := 0x000C
DEFINE IMAGE_REL_MIPS_SECRELHI         := 0x000D
DEFINE IMAGE_REL_MIPS_TOKEN            := 0x000E  // clr token
DEFINE IMAGE_REL_MIPS_JMPADDR16        := 0x0010
DEFINE IMAGE_REL_MIPS_REFWORDNB        := 0x0022
DEFINE IMAGE_REL_MIPS_PAIR             := 0x0025
//
// Alpha Relocation types.
//
DEFINE IMAGE_REL_ALPHA_ABSOLUTE        := 0x0000
DEFINE IMAGE_REL_ALPHA_REFLONG         := 0x0001
DEFINE IMAGE_REL_ALPHA_REFQUAD         := 0x0002
DEFINE IMAGE_REL_ALPHA_GPREL32         := 0x0003
DEFINE IMAGE_REL_ALPHA_LITERAL         := 0x0004
DEFINE IMAGE_REL_ALPHA_LITUSE          := 0x0005
DEFINE IMAGE_REL_ALPHA_GPDISP          := 0x0006
DEFINE IMAGE_REL_ALPHA_BRADDR          := 0x0007
DEFINE IMAGE_REL_ALPHA_HINT            := 0x0008
DEFINE IMAGE_REL_ALPHA_INLINE_REFLONG  := 0x0009
DEFINE IMAGE_REL_ALPHA_REFHI           := 0x000A
DEFINE IMAGE_REL_ALPHA_REFLO           := 0x000B
DEFINE IMAGE_REL_ALPHA_PAIR            := 0x000C
DEFINE IMAGE_REL_ALPHA_MATCH           := 0x000D
DEFINE IMAGE_REL_ALPHA_SECTION         := 0x000E
DEFINE IMAGE_REL_ALPHA_SECREL          := 0x000F
DEFINE IMAGE_REL_ALPHA_REFLONGNB       := 0x0010
DEFINE IMAGE_REL_ALPHA_SECRELLO        := 0x0011  // Low 16-bit section relative reference
DEFINE IMAGE_REL_ALPHA_SECRELHI        := 0x0012  // High 16-bit section relative reference
DEFINE IMAGE_REL_ALPHA_REFQ3           := 0x0013  // High 16 bits of 48 bit reference
DEFINE IMAGE_REL_ALPHA_REFQ2           := 0x0014  // Middle 16 bits of 48 bit reference
DEFINE IMAGE_REL_ALPHA_REFQ1           := 0x0015  // Low 16 bits of 48 bit reference
DEFINE IMAGE_REL_ALPHA_GPRELLO         := 0x0016  // Low 16-bit GP relative reference
DEFINE IMAGE_REL_ALPHA_GPRELHI         := 0x0017  // High 16-bit GP relative reference
//
// IBM PowerPC relocation types.
//
DEFINE IMAGE_REL_PPC_ABSOLUTE          := 0x0000
DEFINE IMAGE_REL_PPC_ADDR64            := 0x0001
DEFINE IMAGE_REL_PPC_ADDR32            := 0x0002
DEFINE IMAGE_REL_PPC_ADDR24            := 0x0003
DEFINE IMAGE_REL_PPC_ADDR16            := 0x0004
DEFINE IMAGE_REL_PPC_ADDR14            := 0x0005
DEFINE IMAGE_REL_PPC_REL24             := 0x0006
DEFINE IMAGE_REL_PPC_REL14             := 0x0007
DEFINE IMAGE_REL_PPC_TOCREL16          := 0x0008
DEFINE IMAGE_REL_PPC_TOCREL14          := 0x0009
DEFINE IMAGE_REL_PPC_ADDR32NB          := 0x000A
DEFINE IMAGE_REL_PPC_SECREL            := 0x000B
DEFINE IMAGE_REL_PPC_SECTION           := 0x000C
DEFINE IMAGE_REL_PPC_IFGLUE            := 0x000D
DEFINE IMAGE_REL_PPC_IMGLUE            := 0x000E
DEFINE IMAGE_REL_PPC_SECREL16          := 0x000F
DEFINE IMAGE_REL_PPC_REFHI             := 0x0010
DEFINE IMAGE_REL_PPC_REFLO             := 0x0011
DEFINE IMAGE_REL_PPC_PAIR              := 0x0012
DEFINE IMAGE_REL_PPC_SECRELLO         :=  0x0013  // Low 16-bit section relative reference (used for >32k TLS)
DEFINE IMAGE_REL_PPC_SECRELHI         :=  0x0014  // High 16-bit section relative reference (used for >32k TLS)
DEFINE IMAGE_REL_PPC_GPREL            :=  0x0015
DEFINE IMAGE_REL_PPC_TOKEN            :=  0x0016  // clr token
DEFINE IMAGE_REL_PPC_TYPEMASK         := 0x00FF
// Flag bits in IMAGE_RELOCATION.TYPE
DEFINE IMAGE_REL_PPC_NEG               := 0x0100
DEFINE IMAGE_REL_PPC_BRTAKEN           := 0x0200
DEFINE IMAGE_REL_PPC_BRNTAKEN          := 0x0400
DEFINE IMAGE_REL_PPC_TOCDEFN           := 0x0800
//
// Hitachi SH3 relocation types.
//
DEFINE IMAGE_REL_SH3_ABSOLUTE          := 0x0000  // No relocation
DEFINE IMAGE_REL_SH3_DIRECT16          := 0x0001  // 16 bit direct
DEFINE IMAGE_REL_SH3_DIRECT32          := 0x0002  // 32 bit direct
DEFINE IMAGE_REL_SH3_DIRECT8           := 0x0003  // 8 bit direct, -128..255
DEFINE IMAGE_REL_SH3_DIRECT8_WORD      := 0x0004  // 8 bit direct .W (0 ext.)
DEFINE IMAGE_REL_SH3_DIRECT8_LONG      := 0x0005  // 8 bit direct .L (0 ext.)
DEFINE IMAGE_REL_SH3_DIRECT4           := 0x0006  // 4 bit direct (0 ext.)
DEFINE IMAGE_REL_SH3_DIRECT4_WORD      := 0x0007  // 4 bit direct .W (0 ext.)
DEFINE IMAGE_REL_SH3_DIRECT4_LONG      := 0x0008  // 4 bit direct .L (0 ext.)
DEFINE IMAGE_REL_SH3_PCREL8_WORD       := 0x0009  // 8 bit PC relative .W
DEFINE IMAGE_REL_SH3_PCREL8_LONG       := 0x000A  // 8 bit PC relative .L
DEFINE IMAGE_REL_SH3_PCREL12_WORD      := 0x000B  // 12 LSB PC relative .W
DEFINE IMAGE_REL_SH3_STARTOF_SECTION   := 0x000C  // Start of EXE section
DEFINE IMAGE_REL_SH3_SIZEOF_SECTION    := 0x000D  // Size of EXE section
DEFINE IMAGE_REL_SH3_SECTION           := 0x000E  // Section table index
DEFINE IMAGE_REL_SH3_SECREL            := 0x000F  // Offset within section
DEFINE IMAGE_REL_SH3_DIRECT32_NB       := 0x0010  // 32 bit direct not based
DEFINE IMAGE_REL_SH3_GPREL4_LONG       := 0x0011  // GP-relative addressing
DEFINE IMAGE_REL_SH3_TOKEN             := 0x0012  // clr token
DEFINE IMAGE_REL_SHM_PCRELPT           := 0x0013  // Offset from current
                                                 //  instruction in longwords
                                                //  if not NOMODE, insert the
                                                //  inverse of the low bit at
                                                //  bit 32 to select PTA/PTB
DEFINE IMAGE_REL_SHM_REFLO             := 0x0014  // Low bits of 32-bit address
DEFINE IMAGE_REL_SHM_REFHALF           := 0x0015  // High bits of 32-bit address
DEFINE IMAGE_REL_SHM_RELLO             := 0x0016  // Low bits of relative reference
DEFINE IMAGE_REL_SHM_RELHALF           := 0x0017  // High bits of relative reference
DEFINE IMAGE_REL_SHM_PAIR              := 0x0018  // offset operand for relocation
DEFINE IMAGE_REL_SH_NOMODE             := 0x8000  // relocation ignores section mode
DEFINE IMAGE_REL_ARM_ABSOLUTE          := 0x0000  // No relocation required
DEFINE IMAGE_REL_ARM_ADDR32            := 0x0001  // 32 bit address
DEFINE IMAGE_REL_ARM_ADDR32NB          := 0x0002  // 32 bit address w/o image base
DEFINE IMAGE_REL_ARM_BRANCH24          := 0x0003  // 24 bit offset << 2 & sign ext.
DEFINE IMAGE_REL_ARM_BRANCH11          := 0x0004  // Thumb: 2 11 bit offsets
DEFINE IMAGE_REL_ARM_TOKEN             := 0x0005  // clr token
DEFINE IMAGE_REL_ARM_GPREL12           := 0x0006  // GP-relative addressing (ARM)
DEFINE IMAGE_REL_ARM_GPREL7            := 0x0007  // GP-relative addressing (Thumb)
DEFINE IMAGE_REL_ARM_BLX24             := 0x0008
DEFINE IMAGE_REL_ARM_BLX11             := 0x0009
DEFINE IMAGE_REL_ARM_SECTION           := 0x000E  // Section table index
DEFINE IMAGE_REL_ARM_SECREL            := 0x000F  // Offset within section
DEFINE IMAGE_REL_AM_ABSOLUTE           := 0x0000
DEFINE IMAGE_REL_AM_ADDR32             := 0x0001
DEFINE IMAGE_REL_AM_ADDR32NB           := 0x0002
DEFINE IMAGE_REL_AM_CALL32             := 0x0003
DEFINE IMAGE_REL_AM_FUNCINFO           := 0x0004
DEFINE IMAGE_REL_AM_REL32_1            := 0x0005
DEFINE IMAGE_REL_AM_REL32_2            := 0x0006
DEFINE IMAGE_REL_AM_SECREL             := 0x0007
DEFINE IMAGE_REL_AM_SECTION            := 0x0008
DEFINE IMAGE_REL_AM_TOKEN              := 0x0009
//
// x64 relocations
//
DEFINE IMAGE_REL_AMD64_ABSOLUTE       :=  0x0000  // Reference is absolute, no relocation is necessary
DEFINE IMAGE_REL_AMD64_ADDR64         :=  0x0001  // 64-bit address (VA).
DEFINE IMAGE_REL_AMD64_ADDR32         :=  0x0002  // 32-bit address (VA).
DEFINE IMAGE_REL_AMD64_ADDR32NB       :=  0x0003  // 32-bit address w/o image base (RVA).
DEFINE IMAGE_REL_AMD64_REL32          :=  0x0004  // 32-bit relative address from byte following reloc
DEFINE IMAGE_REL_AMD64_REL32_1        :=  0x0005  // 32-bit relative address from byte distance 1 from reloc
DEFINE IMAGE_REL_AMD64_REL32_2        :=  0x0006  // 32-bit relative address from byte distance 2 from reloc
DEFINE IMAGE_REL_AMD64_REL32_3         := 0x0007  // 32-bit relative address from byte distance 3 from reloc
DEFINE IMAGE_REL_AMD64_REL32_4         := 0x0008  // 32-bit relative address from byte distance 4 from reloc
DEFINE IMAGE_REL_AMD64_REL32_5         := 0x0009  // 32-bit relative address from byte distance 5 from reloc
DEFINE IMAGE_REL_AMD64_SECTION         := 0x000A  // Section index
DEFINE IMAGE_REL_AMD64_SECREL          := 0x000B  // 32 bit offset from base of section containing target
DEFINE IMAGE_REL_AMD64_SECREL7         := 0x000C  // 7 bit unsigned offset from base of section containing target
DEFINE IMAGE_REL_AMD64_TOKEN           := 0x000D  // 32 bit metadata token
DEFINE IMAGE_REL_AMD64_SREL32         := 0x000E  // 32 bit signed span-dependent value emitted into object
DEFINE IMAGE_REL_AMD64_PAIR           := 0x000F
DEFINE IMAGE_REL_AMD64_SSPAN32        := 0x0010  // 32 bit signed span-dependent value applied at link time
//
// IA64 relocation types.
//
DEFINE IMAGE_REL_IA64_ABSOLUTE         := 0x0000
DEFINE IMAGE_REL_IA64_IMM14            := 0x0001
DEFINE IMAGE_REL_IA64_IMM22            := 0x0002
DEFINE IMAGE_REL_IA64_IMM64            := 0x0003
DEFINE IMAGE_REL_IA64_DIR32            := 0x0004
DEFINE IMAGE_REL_IA64_DIR64            := 0x0005
DEFINE IMAGE_REL_IA64_PCREL21B         := 0x0006
DEFINE IMAGE_REL_IA64_PCREL21M         := 0x0007
DEFINE IMAGE_REL_IA64_PCREL21F         := 0x0008
DEFINE IMAGE_REL_IA64_GPREL22          := 0x0009
DEFINE IMAGE_REL_IA64_LTOFF22          := 0x000A
DEFINE IMAGE_REL_IA64_SECTION          := 0x000B
DEFINE IMAGE_REL_IA64_SECREL22         := 0x000C
DEFINE IMAGE_REL_IA64_SECREL64I        := 0x000D
DEFINE IMAGE_REL_IA64_SECREL32         := 0x000E
//
DEFINE IMAGE_REL_IA64_DIR32NB          := 0x0010
DEFINE IMAGE_REL_IA64_SREL14           := 0x0011
DEFINE IMAGE_REL_IA64_SREL22           := 0x0012
DEFINE IMAGE_REL_IA64_SREL32           := 0x0013
DEFINE IMAGE_REL_IA64_UREL32           := 0x0014
DEFINE IMAGE_REL_IA64_PCREL60X         := 0x0015  // This is always a BRL and never converted
DEFINE IMAGE_REL_IA64_PCREL60B         := 0x0016  // If possible, convert to MBB bundle with NOP.B in slot 1
DEFINE IMAGE_REL_IA64_PCREL60F         := 0x0017  // If possible, convert to MFB bundle with NOP.F in slot 1
DEFINE IMAGE_REL_IA64_PCREL60I         := 0x0018  // If possible, convert to MIB bundle with NOP.I in slot 1
DEFINE IMAGE_REL_IA64_PCREL60M         := 0x0019  // If possible, convert to MMB bundle with NOP.M in slot 1
DEFINE IMAGE_REL_IA64_IMMGPREL64       := 0x001A
DEFINE IMAGE_REL_IA64_TOKEN            := 0x001B  // clr token
DEFINE IMAGE_REL_IA64_GPREL32          := 0x001C
DEFINE IMAGE_REL_IA64_ADDEND           := 0x001F
//
// CEF relocation types.
//
DEFINE IMAGE_REL_CEF_ABSOLUTE          := 0x0000  // Reference is absolute, no relocation is necessary
DEFINE IMAGE_REL_CEF_ADDR32            := 0x0001  // 32-bit address (VA).
DEFINE IMAGE_REL_CEF_ADDR64            := 0x0002  // 64-bit address (VA).
DEFINE IMAGE_REL_CEF_ADDR32NB          := 0x0003  // 32-bit address w/o image base (RVA).
DEFINE IMAGE_REL_CEF_SECTION           := 0x0004  // Section index
DEFINE IMAGE_REL_CEF_SECREL            := 0x0005  // 32 bit offset from base of section containing target
DEFINE IMAGE_REL_CEF_TOKEN             := 0x0006  // 32 bit metadata token
//
// clr relocation types.
//
DEFINE IMAGE_REL_CEE_ABSOLUTE          := 0x0000  // Reference is absolute, no relocation is necessary
DEFINE IMAGE_REL_CEE_ADDR32            := 0x0001  // 32-bit address (VA).
DEFINE IMAGE_REL_CEE_ADDR64            := 0x0002  // 64-bit address (VA).
DEFINE IMAGE_REL_CEE_ADDR32NB          := 0x0003  // 32-bit address w/o image base (RVA).
DEFINE IMAGE_REL_CEE_SECTION           := 0x0004  // Section index
DEFINE IMAGE_REL_CEE_SECREL            := 0x0005  // 32 bit offset from base of section containing target
DEFINE IMAGE_REL_CEE_TOKEN             := 0x0006  // 32 bit metadata token
DEFINE IMAGE_REL_M32R_ABSOLUTE        :=  0x0000  // No relocation required
DEFINE IMAGE_REL_M32R_ADDR32           := 0x0001  // 32 bit address
DEFINE IMAGE_REL_M32R_ADDR32NB         := 0x0002  // 32 bit address w/o image base
DEFINE IMAGE_REL_M32R_ADDR24           := 0x0003  // 24 bit address
DEFINE IMAGE_REL_M32R_GPREL16          := 0x0004  // GP relative addressing
DEFINE IMAGE_REL_M32R_PCREL24          := 0x0005  // 24 bit offset << 2 & sign ext.
DEFINE IMAGE_REL_M32R_PCREL16          := 0x0006  // 16 bit offset << 2 & sign ext.
DEFINE IMAGE_REL_M32R_PCREL8           := 0x0007  // 8 bit offset << 2 & sign ext.
DEFINE IMAGE_REL_M32R_REFHALF          := 0x0008  // 16 MSBs
DEFINE IMAGE_REL_M32R_REFHI            := 0x0009  // 16 MSBs; adj for LSB sign ext.
DEFINE IMAGE_REL_M32R_REFLO            := 0x000A  // 16 LSBs
DEFINE IMAGE_REL_M32R_PAIR             := 0x000B  // Link HI and LO
DEFINE IMAGE_REL_M32R_SECTION          := 0x000C  // Section table index
DEFINE IMAGE_REL_M32R_SECREL32         := 0x000D  // 32 bit section relative reference
DEFINE IMAGE_REL_M32R_TOKEN            := 0x000E  // clr token
DEFINE IMAGE_REL_EBC_ABSOLUTE          := 0x0000  // No relocation required
DEFINE IMAGE_REL_EBC_ADDR32NB          := 0x0001  // 32 bit address w/o image base
DEFINE IMAGE_REL_EBC_REL32             := 0x0002  // 32-bit relative address from byte following reloc
DEFINE IMAGE_REL_EBC_SECTION           := 0x0003  // Section table index
DEFINE IMAGE_REL_EBC_SECREL            := 0x0004  // Offset within section
// define EXT_IMM64(Value, Address, Size, InstPos, ValPos)  /* Intel-IA64-Filler */           \
//     Value |= (((ULONGLONG)((*(Address) >> InstPos) & (((ULONGLONG)1 << Size) - 1))) << ValPos)  // Intel-IA64-Filler
// define INS_IMM64(Value, Address, Size, InstPos, ValPos)  /* Intel-IA64-Filler */\
//     *(PDWORD)Address = (*(PDWORD)Address & ~(((1 << Size) - 1) << InstPos)) | /* Intel-IA64-Filler */\
//           ((DWORD)((((ULONGLONG)Value >> ValPos) & (((ULONGLONG)1 << Size) - 1))) << InstPos)  // Intel-IA64-Filler
DEFINE EMARCH_ENC_I17_IMM7B_INST_WORD_X         := 3  // Intel-IA64-Filler
DEFINE EMARCH_ENC_I17_IMM7B_SIZE_X              := 7  // Intel-IA64-Filler
DEFINE EMARCH_ENC_I17_IMM7B_INST_WORD_POS_X     := 4  // Intel-IA64-Filler
DEFINE EMARCH_ENC_I17_IMM7B_VAL_POS_X           := 0  // Intel-IA64-Filler
DEFINE EMARCH_ENC_I17_IMM9D_INST_WORD_X         := 3  // Intel-IA64-Filler
DEFINE EMARCH_ENC_I17_IMM9D_SIZE_X              := 9  // Intel-IA64-Filler
DEFINE EMARCH_ENC_I17_IMM9D_INST_WORD_POS_X     := 18 // Intel-IA64-Filler
DEFINE EMARCH_ENC_I17_IMM9D_VAL_POS_X           := 7  // Intel-IA64-Filler
DEFINE EMARCH_ENC_I17_IMM5C_INST_WORD_X         := 3  // Intel-IA64-Filler
DEFINE EMARCH_ENC_I17_IMM5C_SIZE_X              := 5  // Intel-IA64-Filler
DEFINE EMARCH_ENC_I17_IMM5C_INST_WORD_POS_X     := 13 // Intel-IA64-Filler
DEFINE EMARCH_ENC_I17_IMM5C_VAL_POS_X           := 16 // Intel-IA64-Filler
DEFINE EMARCH_ENC_I17_IC_INST_WORD_X            := 3  // Intel-IA64-Filler
DEFINE EMARCH_ENC_I17_IC_SIZE_X                 := 1  // Intel-IA64-Filler
DEFINE EMARCH_ENC_I17_IC_INST_WORD_POS_X        := 12 // Intel-IA64-Filler
DEFINE EMARCH_ENC_I17_IC_VAL_POS_X              := 21 // Intel-IA64-Filler
DEFINE EMARCH_ENC_I17_IMM41a_INST_WORD_X        := 1  // Intel-IA64-Filler
DEFINE EMARCH_ENC_I17_IMM41a_SIZE_X             := 10 // Intel-IA64-Filler
DEFINE EMARCH_ENC_I17_IMM41a_INST_WORD_POS_X    := 14 // Intel-IA64-Filler
DEFINE EMARCH_ENC_I17_IMM41a_VAL_POS_X          := 22 // Intel-IA64-Filler
DEFINE EMARCH_ENC_I17_IMM41b_INST_WORD_X        := 1  // Intel-IA64-Filler
DEFINE EMARCH_ENC_I17_IMM41b_SIZE_X             := 8  // Intel-IA64-Filler
DEFINE EMARCH_ENC_I17_IMM41b_INST_WORD_POS_X    := 24 // Intel-IA64-Filler
DEFINE EMARCH_ENC_I17_IMM41b_VAL_POS_X          := 32 // Intel-IA64-Filler
DEFINE EMARCH_ENC_I17_IMM41c_INST_WORD_X        := 2  // Intel-IA64-Filler
DEFINE EMARCH_ENC_I17_IMM41c_SIZE_X             := 23 // Intel-IA64-Filler
DEFINE EMARCH_ENC_I17_IMM41c_INST_WORD_POS_X    := 0  // Intel-IA64-Filler
DEFINE EMARCH_ENC_I17_IMM41c_VAL_POS_X          := 40 // Intel-IA64-Filler
DEFINE EMARCH_ENC_I17_SIGN_INST_WORD_X          := 3  // Intel-IA64-Filler
DEFINE EMARCH_ENC_I17_SIGN_SIZE_X               := 1  // Intel-IA64-Filler
DEFINE EMARCH_ENC_I17_SIGN_INST_WORD_POS_X      := 27 // Intel-IA64-Filler
DEFINE EMARCH_ENC_I17_SIGN_VAL_POS_X            := 63 // Intel-IA64-Filler
DEFINE X3_OPCODE_INST_WORD_X                    := 3  // Intel-IA64-Filler
DEFINE X3_OPCODE_SIZE_X                         := 4  // Intel-IA64-Filler
DEFINE X3_OPCODE_INST_WORD_POS_X                := 28 // Intel-IA64-Filler
DEFINE X3_OPCODE_SIGN_VAL_POS_X                 := 0  // Intel-IA64-Filler
DEFINE X3_I_INST_WORD_X                         := 3  // Intel-IA64-Filler
DEFINE X3_I_SIZE_X                              := 1  // Intel-IA64-Filler
DEFINE X3_I_INST_WORD_POS_X                     := 27 // Intel-IA64-Filler
DEFINE X3_I_SIGN_VAL_POS_X                      := 59 // Intel-IA64-Filler
DEFINE X3_D_WH_INST_WORD_X                      := 3  // Intel-IA64-Filler
DEFINE X3_D_WH_SIZE_X                           := 3  // Intel-IA64-Filler
DEFINE X3_D_WH_INST_WORD_POS_X                  := 24 // Intel-IA64-Filler
DEFINE X3_D_WH_SIGN_VAL_POS_X                   := 0  // Intel-IA64-Filler
DEFINE X3_IMM20_INST_WORD_X                     := 3  // Intel-IA64-Filler
DEFINE X3_IMM20_SIZE_X                          := 20 // Intel-IA64-Filler
DEFINE X3_IMM20_INST_WORD_POS_X                 := 4  // Intel-IA64-Filler
DEFINE X3_IMM20_SIGN_VAL_POS_X                  := 0  // Intel-IA64-Filler
DEFINE X3_IMM39_1_INST_WORD_X                  :=  2  // Intel-IA64-Filler
DEFINE X3_IMM39_1_SIZE_X                       :=  23 // Intel-IA64-Filler
DEFINE X3_IMM39_1_INST_WORD_POS_X              :=  0  // Intel-IA64-Filler
DEFINE X3_IMM39_1_SIGN_VAL_POS_X               :=  36 // Intel-IA64-Filler
DEFINE X3_IMM39_2_INST_WORD_X                   := 1  // Intel-IA64-Filler
DEFINE X3_IMM39_2_SIZE_X                        := 16 // Intel-IA64-Filler
DEFINE X3_IMM39_2_INST_WORD_POS_X               := 16 // Intel-IA64-Filler
DEFINE X3_IMM39_2_SIGN_VAL_POS_X                := 20 // Intel-IA64-Filler
DEFINE X3_P_INST_WORD_X                         := 3  // Intel-IA64-Filler
DEFINE X3_P_SIZE_X                              := 4  // Intel-IA64-Filler
DEFINE X3_P_INST_WORD_POS_X                     := 0  // Intel-IA64-Filler
DEFINE X3_P_SIGN_VAL_POS_X                      := 0  // Intel-IA64-Filler
DEFINE X3_TMPLT_INST_WORD_X                     := 0  // Intel-IA64-Filler
DEFINE X3_TMPLT_SIZE_X                          := 4  // Intel-IA64-Filler
DEFINE X3_TMPLT_INST_WORD_POS_X                 := 0  // Intel-IA64-Filler
DEFINE X3_TMPLT_SIGN_VAL_POS_X                  := 0  // Intel-IA64-Filler
DEFINE X3_BTYPE_QP_INST_WORD_X                  := 2  // Intel-IA64-Filler
DEFINE X3_BTYPE_QP_SIZE_X                       := 9  // Intel-IA64-Filler
DEFINE X3_BTYPE_QP_INST_WORD_POS_X              := 23 // Intel-IA64-Filler
DEFINE X3_BTYPE_QP_INST_VAL_POS_X               := 0  // Intel-IA64-Filler
DEFINE X3_EMPTY_INST_WORD_X                     := 1  // Intel-IA64-Filler
DEFINE X3_EMPTY_SIZE_X                          := 2  // Intel-IA64-Filler
DEFINE X3_EMPTY_INST_WORD_POS_X                 := 14 // Intel-IA64-Filler
DEFINE X3_EMPTY_INST_VAL_POS_X                  := 0  // Intel-IA64-Filler
//
// Line number format.
//
DEFINE IMAGE_SIZEOF_BASE_RELOCATION         := 8
//
// Based relocation types.
//
DEFINE IMAGE_REL_BASED_ABSOLUTE             := 0
DEFINE IMAGE_REL_BASED_HIGH                 := 1
DEFINE IMAGE_REL_BASED_LOW                  := 2
DEFINE IMAGE_REL_BASED_HIGHLOW              := 3
DEFINE IMAGE_REL_BASED_HIGHADJ              := 4
DEFINE IMAGE_REL_BASED_MIPS_JMPADDR         := 5
DEFINE IMAGE_REL_BASED_MIPS_JMPADDR16       := 9
DEFINE IMAGE_REL_BASED_IA64_IMM64           :=  9
DEFINE IMAGE_REL_BASED_DIR64                 := 10
//
// Archive format.
//
DEFINE IMAGE_ARCHIVE_START_SIZE             := 8
DEFINE IMAGE_SIZEOF_ARCHIVE_MEMBER_HDR      := 60
//
// DLL support.
//
//
// Export Format
//
DEFINE IMAGE_ORDINAL_FLAG    := 0x80000000
//
// Thread Local Storage
//
DEFINE IMAGE_RESOURCE_NAME_IS_STRING        := 0x80000000
DEFINE IMAGE_RESOURCE_DATA_IS_DIRECTORY     := 0x80000000
//
// Each directory contains the 32-bit Name of the entry and an offset,
// relative to the beginning of the resource directory of the data associated
// with this directory entry.  If the name of the entry is an actual text
// string instead of an integer Id, then the high order bit of the name field
// is set to one and the low order 31-bits are an offset, relative to the
// beginning of the resource directory of the string, which is of type
// IMAGE_RESOURCE_DIRECTORY_STRING.  Otherwise the high bit is clear and the
// low-order 16-bits are the integer Id that identify this resource directory
// entry. If the directory entry is yet another resource directory (i.e. a
// subdirectory), then the high order bit of the offset field will be
// set to indicate this.  Otherwise the high bit is clear and the offset
// field points to a resource data entry.
//
DEFINE IMAGE_DEBUG_TYPE_UNKNOWN          := 0
DEFINE IMAGE_DEBUG_TYPE_COFF             := 1
DEFINE IMAGE_DEBUG_TYPE_CODEVIEW         := 2
DEFINE IMAGE_DEBUG_TYPE_FPO              := 3
DEFINE IMAGE_DEBUG_TYPE_MISC             := 4
DEFINE IMAGE_DEBUG_TYPE_EXCEPTION        := 5
DEFINE IMAGE_DEBUG_TYPE_FIXUP            := 6
DEFINE IMAGE_DEBUG_TYPE_OMAP_TO_SRC      := 7
DEFINE IMAGE_DEBUG_TYPE_OMAP_FROM_SRC    := 8
DEFINE IMAGE_DEBUG_TYPE_BORLAND          := 9
DEFINE IMAGE_DEBUG_TYPE_RESERVED10       := 10
DEFINE IMAGE_DEBUG_TYPE_CLSID            := 11
DEFINE FRAME_FPO       := 0
DEFINE FRAME_TRAP      := 1
DEFINE FRAME_TSS       := 2
DEFINE FRAME_NONFPO    := 3
DEFINE SIZEOF_RFPO_DATA := 16
DEFINE IMAGE_DEBUG_MISC_EXENAME    :=1
DEFINE IMAGE_SEPARATE_DEBUG_SIGNATURE := 0x4944
DEFINE NON_PAGED_DEBUG_SIGNATURE      := 0x494E
DEFINE IMAGE_SEPARATE_DEBUG_FLAGS_MASK := 0x8000
DEFINE IMAGE_SEPARATE_DEBUG_MISMATCH   := 0x8000  // when DBG was updated, the
                                                // old checksum didn't match.
//
//  The .arch section is made up of headers, each describing an amask position/value
//  pointing to an array of IMAGE_ARCHITECTURE_ENTRY's.  Each "array" (both the header
//  and entry arrays) are terminiated by a quadword of 0xffffffffL.
//
//  NOTE: There may be quadwords of 0 sprinkled around and must be skipped.
//
DEFINE HEAP_NO_SERIALIZE               := 0x00000001
DEFINE HEAP_GROWABLE                   := 0x00000002
DEFINE HEAP_GENERATE_EXCEPTIONS        := 0x00000004
DEFINE HEAP_ZERO_MEMORY                := 0x00000008
DEFINE HEAP_REALLOC_IN_PLACE_ONLY      := 0x00000010
DEFINE HEAP_TAIL_CHECKING_ENABLED      := 0x00000020
DEFINE HEAP_FREE_CHECKING_ENABLED      := 0x00000040
DEFINE HEAP_DISABLE_COALESCE_ON_FREE   := 0x00000080
DEFINE HEAP_CREATE_ALIGN_16            := 0x00010000
DEFINE HEAP_CREATE_ENABLE_TRACING      := 0x00020000
DEFINE HEAP_CREATE_ENABLE_EXECUTE      := 0x00040000      
DEFINE HEAP_MAXIMUM_TAG                := 0x0FFF
DEFINE HEAP_PSEUDO_TAG_FLAG            := 0x8000
DEFINE HEAP_TAG_SHIFT                  := 16
DEFINE IS_TEXT_UNICODE_ASCII16               := 0x0001
DEFINE IS_TEXT_UNICODE_REVERSE_ASCII16       := 0x0010
DEFINE IS_TEXT_UNICODE_STATISTICS            := 0x0002
DEFINE IS_TEXT_UNICODE_REVERSE_STATISTICS    := 0x0020
DEFINE IS_TEXT_UNICODE_CONTROLS              := 0x0004
DEFINE IS_TEXT_UNICODE_REVERSE_CONTROLS      := 0x0040
DEFINE IS_TEXT_UNICODE_SIGNATURE             := 0x0008
DEFINE IS_TEXT_UNICODE_REVERSE_SIGNATURE     := 0x0080
DEFINE IS_TEXT_UNICODE_ILLEGAL_CHARS         := 0x0100
DEFINE IS_TEXT_UNICODE_ODD_LENGTH            := 0x0200
DEFINE IS_TEXT_UNICODE_DBCS_LEADBYTE         := 0x0400
DEFINE IS_TEXT_UNICODE_NULL_BYTES            := 0x1000
DEFINE IS_TEXT_UNICODE_UNICODE_MASK          := 0x000F
DEFINE IS_TEXT_UNICODE_REVERSE_MASK          := 0x00F0
DEFINE IS_TEXT_UNICODE_NOT_UNICODE_MASK      := 0x0F00
DEFINE IS_TEXT_UNICODE_NOT_ASCII_MASK        := 0xF000
DEFINE COMPRESSION_FORMAT_NONE          := (0x0000)
DEFINE COMPRESSION_FORMAT_DEFAULT       := (0x0001)
DEFINE COMPRESSION_FORMAT_LZNT1         := (0x0002)
DEFINE COMPRESSION_ENGINE_STANDARD      := (0x0000)
DEFINE COMPRESSION_ENGINE_MAXIMUM       := (0x0100)
DEFINE COMPRESSION_ENGINE_HIBER         := (0x0200)   
DEFINE SEF_DACL_AUTO_INHERIT             := 0x01
DEFINE SEF_SACL_AUTO_INHERIT             := 0x02
DEFINE SEF_DEFAULT_DESCRIPTOR_FOR_OBJECT := 0x04
DEFINE SEF_AVOID_PRIVILEGE_CHECK         := 0x08
DEFINE SEF_AVOID_OWNER_CHECK             := 0x10
DEFINE SEF_DEFAULT_OWNER_FROM_PARENT     := 0x20
DEFINE SEF_DEFAULT_GROUP_FROM_PARENT     := 0x40
DEFINE SEF_MACL_NO_WRITE_UP              := 0x100
DEFINE SEF_MACL_NO_READ_UP               := 0x200
DEFINE SEF_MACL_NO_EXECUTE_UP            := 0x400
DEFINE SEF_AVOID_OWNER_RESTRICTION       := 0x1000
DEFINE SEF_MACL_VALID_FLAGS              := (SEF_MACL_NO_WRITE_UP   | ;
                                           SEF_MACL_NO_READ_UP    | ;
                                           SEF_MACL_NO_EXECUTE_UP)
DEFINE MESSAGE_RESOURCE_UNICODE :=0x0001
DEFINE VER_EQUAL                       := 1
DEFINE VER_GREATER                     := 2
DEFINE VER_GREATER_EQUAL               := 3
DEFINE VER_LESS                        := 4
DEFINE VER_LESS_EQUAL                  := 5
DEFINE VER_AND                         := 6
DEFINE VER_OR                          := 7
DEFINE VER_CONDITION_MASK              := 7
DEFINE VER_NUM_BITS_PER_CONDITION_MASK := 3
//
// RtlVerifyVersionInfo() type mask bits
//
DEFINE VER_MINORVERSION                := 0x0000001
DEFINE VER_MAJORVERSION                := 0x0000002
DEFINE VER_BUILDNUMBER                 := 0x0000004
DEFINE VER_PLATFORMID                  := 0x0000008
DEFINE VER_SERVICEPACKMINOR            := 0x0000010
DEFINE VER_SERVICEPACKMAJOR            := 0x0000020
DEFINE VER_SUITENAME                   := 0x0000040
DEFINE VER_PRODUCT_TYPE                := 0x0000080
//
// RtlVerifyVersionInfo() os product type values
//
//DEFINE VER_NT_WORKSTATION              := 0x0000001
//DEFINE VER_NT_DOMAIN_CONTROLLER        := 0x0000002
//DEFINE VER_NT_SERVER                   := 0x0000003
//
// dwPlatformId defines:
//
//DEFINE VER_PLATFORM_WIN32s             := 0
//DEFINE VER_PLATFORM_WIN32_WINDOWS      := 1
//DEFINE VER_PLATFORM_WIN32_NT           := 2
//
//
// VerifyVersionInfo() macro to set the condition mask
//
// For documentation sakes here's the old version of the macro that got
// changed to call an API
// define VER_SET_CONDITION(_m_,_t_,_c_)  _m_=(_m_|(_c_<<(1<<_t_)))
//
DEFINE RTL_CRITSECT_TYPE  := 0
DEFINE RTL_RESOURCE_TYPE := 1
//
// These flags define the upper byte of the critical section SpinCount field
//
DEFINE RTL_CRITICAL_SECTION_FLAG_NO_DEBUG_INFO         := 0x01000000
DEFINE RTL_CRITICAL_SECTION_FLAG_DYNAMIC_SPIN          := 0x02000000
DEFINE RTL_CRITICAL_SECTION_FLAG_STATIC_INIT           := 0x04000000
DEFINE RTL_CRITICAL_SECTION_ALL_FLAG_BITS              := 0xFF000000
DEFINE RTL_CRITICAL_SECTION_FLAG_RESERVED              := _AND(RTL_CRITICAL_SECTION_ALL_FLAG_BITS , ; 
                                                           (_NOT(RTL_CRITICAL_SECTION_FLAG_NO_DEBUG_INFO | ;
                                                                RTL_CRITICAL_SECTION_FLAG_DYNAMIC_SPIN | ;
                                                                RTL_CRITICAL_SECTION_FLAG_STATIC_INIT)))
//
// These flags define possible values stored in the Flags field of a critsec debuginfo.
//
DEFINE RTL_CRITICAL_SECTION_DEBUG_FLAG_STATIC_INIT     := 0x00000001
DEFINE DLL_PROCESS_ATTACH := 1
DEFINE DLL_THREAD_ATTACH  := 2
DEFINE DLL_THREAD_DETACH  := 3
DEFINE DLL_PROCESS_DETACH := 0
//
// Defines for the READ flags for Eventlogging
//
DEFINE EVENTLOG_SEQUENTIAL_READ  :=    0X0001
DEFINE EVENTLOG_SEEK_READ        :=    0X0002
DEFINE EVENTLOG_FORWARDS_READ    :=    0X0004
DEFINE EVENTLOG_BACKWARDS_READ   :=     0X0008
//
// The types of events that can be logged.
//
DEFINE EVENTLOG_SUCCESS                := 0X0000
DEFINE EVENTLOG_ERROR_TYPE             := 0x0001
DEFINE EVENTLOG_WARNING_TYPE           := 0x0002
DEFINE EVENTLOG_INFORMATION_TYPE       := 0x0004
DEFINE EVENTLOG_AUDIT_SUCCESS          := 0x0008
DEFINE EVENTLOG_AUDIT_FAILURE          := 0x0010
//
// Defines for the WRITE flags used by Auditing for paired events
// These are not implemented in Product 1
//
DEFINE EVENTLOG_START_PAIRED_EVENT    := 0x0001
DEFINE EVENTLOG_END_PAIRED_EVENT      := 0x0002
DEFINE EVENTLOG_END_ALL_PAIRED_EVENTS := 0x0004
DEFINE EVENTLOG_PAIRED_EVENT_ACTIVE   := 0x0008
DEFINE EVENTLOG_PAIRED_EVENT_INACTIVE := 0x0010
//
// Structure that defines the header of the Eventlog record. This is the
// fixed-sized portion before all the variable-length strings, binary
// data and pad bytes.
//
// TimeGenerated is the time it was generated at the client.
// TimeWritten is the time it was put into the log at the server end.
//
DEFINE KEY_QUERY_VALUE         := 0x0001
DEFINE KEY_SET_VALUE           := 0x0002
DEFINE KEY_CREATE_SUB_KEY      := 0x0004
DEFINE KEY_ENUMERATE_SUB_KEYS  := 0x0008
DEFINE KEY_NOTIFY              := 0x0010
DEFINE KEY_CREATE_LINK         := 0x0020
DEFINE KEY_WOW64_32KEY         := (0x0200)
DEFINE KEY_WOW64_64KEY         := (0x0100)
DEFINE KEY_WOW64_RES           := (0x0300)
DEFINE KEY_WRITE      := (STANDARD_RIGHTS_WRITE| KEY_SET_VALUE| KEY_CREATE_SUB_KEY)
DEFINE KEY_READ       := (STANDARD_RIGHTS_READ |KEY_QUERY_VALUE |KEY_ENUMERATE_SUB_KEYS|KEY_NOTIFY)
DEFINE KEY_ALL_ACCESS := (KEY_QUERY_VALUE| KEY_ENUMERATE_SUB_KEYS| KEY_NOTIFY| KEY_CREATE_SUB_KEY| KEY_CREATE_LINK| KEY_SET_VALUE| STANDARD_RIGHTS_REQUIRED)
//
// Open/Create Options
//
DEFINE REG_OPTION_RESERVED         := 0x00000000L
DEFINE REG_OPTION_NON_VOLATILE     := 0x00000000L
DEFINE REG_OPTION_VOLATILE         := 0x00000001L
DEFINE REG_OPTION_CREATE_LINK     := 0x00000002L
DEFINE REG_OPTION_BACKUP_RESTORE   := 0x00000004L
DEFINE REG_LEGAL_OPTION  :=0x00000007L
//
// Key creation/open disposition
//
DEFINE REG_CREATED_NEW_KEY         := 0x00000001L
DEFINE REG_OPENED_EXISTING_KEY     := 0x00000002L
//
// hive format to be used by Reg(Nt)SaveKeyEx
//
DEFINE REG_STANDARD_FORMAT     := 1
DEFINE REG_LATEST_FORMAT       := 2
DEFINE REG_NO_COMPRESSION      := 4
//
// Key restore & hive load flags
//
DEFINE REG_WHOLE_HIVE_VOLATILE     := 0x00000001L
DEFINE REG_REFRESH_HIVE            := 0x00000002L
DEFINE REG_NO_LAZY_FLUSH              := (0x00000004L)   // Never lazy flush this hive
DEFINE REG_FORCE_RESTORE              :=  (0x00000008L)   // Force the restore process even when we have open handles on subkeys
DEFINE REG_APP_HIVE                   :=  (0x00000010L)   // Loads the hive visible to the calling process
DEFINE REG_PROCESS_PRIVATE            :=  (0x00000020L)   // Hive cannot be mounted by any other process while in use
DEFINE REG_START_JOURNAL              :=  (0x00000040L)   // Starts Hive Journal
DEFINE REG_HIVE_EXACT_FILE_GROWTH     :=  (0x00000080L)   // Grow hive file in exact 4k increments
DEFINE REG_HIVE_NO_RM                 :=  (0x00000100L)   // No RM is started for this hive (no transactions)
DEFINE REG_HIVE_SINGLE_LOG            :=  (0x00000200L)   // Legacy single logging is used for this hive
//
// Unload Flags
//
DEFINE REG_FORCE_UNLOAD            := 1
//
// Notify filter values
//
DEFINE REG_NOTIFY_CHANGE_NAME          := 0x00000001L
DEFINE REG_NOTIFY_CHANGE_ATTRIBUTES    := 0x00000002L
DEFINE REG_NOTIFY_CHANGE_LAST_SET      := 0x00000004L
DEFINE REG_NOTIFY_CHANGE_SECURITY      := 0x00000008L
DEFINE REG_LEGAL_CHANGE_FILTER                 := ;
                (REG_NOTIFY_CHANGE_NAME          |;
                 REG_NOTIFY_CHANGE_ATTRIBUTES    |;
                 REG_NOTIFY_CHANGE_LAST_SET      |;
                 REG_NOTIFY_CHANGE_SECURITY)
// end_wdm 
//
//
// Predefined Value Types.
//
DEFINE REG_NONE                     := 0
DEFINE REG_SZ                       := 1
DEFINE REG_EXPAND_SZ                := 2
DEFINE REG_BINARY                   := 3
DEFINE REG_DWORD                    := 4
DEFINE REG_DWORD_LITTLE_ENDIAN      := 4
DEFINE REG_DWORD_BIG_ENDIAN         := 5
DEFINE REG_LINK                     := 6
DEFINE REG_MULTI_SZ                 := 7
DEFINE REG_RESOURCE_LIST            := 8
DEFINE REG_FULL_RESOURCE_DESCRIPTOR  := 9
DEFINE REG_RESOURCE_REQUIREMENTS_LIST  := 10
DEFINE REG_QWORD                   := ( 11 )  // 64-bit number
DEFINE REG_QWORD_LITTLE_ENDIAN     := ( 11 )  // 64-bit number (same as REG_QWORD)
// end_wdm
// begin_wdm
//
// Service Types (Bit Mask)
//
DEFINE SERVICE_KERNEL_DRIVER          := 0x00000001
DEFINE SERVICE_FILE_SYSTEM_DRIVER     := 0x00000002
DEFINE SERVICE_ADAPTER                := 0x00000004
DEFINE SERVICE_RECOGNIZER_DRIVER      := 0x00000008
DEFINE SERVICE_DRIVER                 := (SERVICE_KERNEL_DRIVER | ;
                                        SERVICE_FILE_SYSTEM_DRIVER | ;
                                        SERVICE_RECOGNIZER_DRIVER)
DEFINE SERVICE_WIN32_OWN_PROCESS      :=0x00000010
DEFINE SERVICE_WIN32_SHARE_PROCESS    :=0x00000020
DEFINE SERVICE_WIN32                  := (SERVICE_WIN32_OWN_PROCESS | ;
                                        SERVICE_WIN32_SHARE_PROCESS)
DEFINE SERVICE_INTERACTIVE_PROCESS   := 0x00000100
DEFINE SERVICE_TYPE_ALL               := (SERVICE_WIN32  | ;
                                        SERVICE_ADAPTER | ;
                                        SERVICE_DRIVER  | ;
                                        SERVICE_INTERACTIVE_PROCESS)
//
// Start Type
//
DEFINE SERVICE_BOOT_START             := 0x00000000
DEFINE SERVICE_SYSTEM_START           := 0x00000001
DEFINE SERVICE_AUTO_START             := 0x00000002
DEFINE SERVICE_DEMAND_START           := 0x00000003
DEFINE SERVICE_DISABLED               := 0x00000004
//
// Error control type
//
DEFINE SERVICE_ERROR_IGNORE           := 0x00000000
DEFINE SERVICE_ERROR_NORMAL           := 0x00000001
DEFINE SERVICE_ERROR_SEVERE           := 0x00000002
DEFINE SERVICE_ERROR_CRITICAL         := 0x00000003
//
//
// Define the registry driver node enumerations
//
DEFINE DriverType                 := 0x00000001
DEFINE FileSystemType             := 0x00000002
DEFINE Win32ServiceOwnProcess     := 0x00000010
DEFINE win32ServiceShareProcess   := 0x00000020
DEFINE AdapterTyper               := 0x00000004
DEFINE Recongnizertype            := 0x00000008
DEFINE BootLoad    := 0x00000000
DEFINE SystemLoad  := 0x00000001
DEFINE AutoLoad    := 0x00000002
DEFINE DemandLoad  := 0x00000003
DEFINE DisableLoad := 0x00000004
DEFINE IgnoreError := 0x00000000
DEFINE NormalError := 0x00000001
DEFINE SevereError := 0x00000002
DEFINE CriticalError := 0x00000003
//
// Service node Flags
//
//
// The following flag, if set, is used by the OS loader to promote
// a driver's start type to boot start when booting from a network.
//
DEFINE CM_SERVICE_NETWORK_BOOT_LOAD  := 0x00000001
//
// IOCTL_TAPE_ERASE definitions
//
DEFINE TAPE_ERASE_SHORT            := 0L
DEFINE TAPE_ERASE_LONG             := 1L
DEFINE TAPE_LOAD                   := 0L
DEFINE TAPE_UNLOAD                 := 1L
DEFINE TAPE_TENSION                := 2L
DEFINE TAPE_LOCK                   := 3L
DEFINE TAPE_UNLOCK                 := 4L
DEFINE TAPE_FORMAT                 :=5L
DEFINE TAPE_SETMARKS               := 0L
DEFINE TAPE_FILEMARKS              := 1L
DEFINE TAPE_SHORT_FILEMARKS        := 2L
DEFINE TAPE_LONG_FILEMARKS         := 3L
DEFINE TAPE_ABSOLUTE_POSITION       := 0L
DEFINE TAPE_LOGICAL_POSITION        := 1L
DEFINE TAPE_PSEUDO_LOGICAL_POSITION := 2L
DEFINE TAPE_REWIND                 := 0L
DEFINE TAPE_ABSOLUTE_BLOCK         := 1L
DEFINE TAPE_LOGICAL_BLOCK          := 2L
DEFINE TAPE_PSEUDO_LOGICAL_BLOCK   := 3L
DEFINE TAPE_SPACE_END_OF_DATA      := 4L
DEFINE TAPE_SPACE_RELATIVE_BLOCKS  := 5L
DEFINE TAPE_SPACE_FILEMARKS        := 6L
DEFINE TAPE_SPACE_SEQUENTIAL_FMKS  := 7L
DEFINE TAPE_SPACE_SETMARKS         := 8L
DEFINE TAPE_SPACE_SEQUENTIAL_SMKS  :=9L
DEFINE TAPE_DRIVE_FIXED            := 0x00000001
DEFINE TAPE_DRIVE_SELECT           := 0x00000002
DEFINE TAPE_DRIVE_INITIATOR        := 0x00000004
DEFINE TAPE_DRIVE_ERASE_SHORT      := 0x00000010
DEFINE TAPE_DRIVE_ERASE_LONG       := 0x00000020
DEFINE TAPE_DRIVE_ERASE_BOP_ONLY   := 0x00000040
DEFINE TAPE_DRIVE_ERASE_IMMEDIATE  := 0x00000080
DEFINE TAPE_DRIVE_TAPE_CAPACITY    := 0x00000100
DEFINE TAPE_DRIVE_TAPE_REMAINING   := 0x00000200
DEFINE TAPE_DRIVE_FIXED_BLOCK      := 0x00000400
DEFINE TAPE_DRIVE_VARIABLE_BLOCK   := 0x00000800
DEFINE TAPE_DRIVE_WRITE_PROTECT    := 0x00001000
DEFINE TAPE_DRIVE_EOT_WZ_SIZE      := 0x00002000
DEFINE TAPE_DRIVE_ECC              := 0x00010000
DEFINE TAPE_DRIVE_COMPRESSION      := 0x00020000
DEFINE TAPE_DRIVE_PADDING          := 0x00040000
DEFINE TAPE_DRIVE_REPORT_SMKS      := 0x00080000
DEFINE TAPE_DRIVE_GET_ABSOLUTE_BLK := 0x00100000
DEFINE TAPE_DRIVE_GET_LOGICAL_BLK  := 0x00200000
DEFINE TAPE_DRIVE_SET_EOT_WZ_SIZE  := 0x00400000
DEFINE TAPE_DRIVE_EJECT_MEDIA      := 0x01000000
DEFINE TAPE_DRIVE_CLEAN_REQUESTS   := 0x02000000
DEFINE TAPE_DRIVE_SET_CMP_BOP_ONLY := 0x04000000
DEFINE TAPE_DRIVE_RESERVED_BIT     := 0x80000000
//                                              //can't be a low features bit!
//                                              //reserved; high features only
//
// Definitions for FeaturesHigh parameter
//
DEFINE TAPE_DRIVE_LOAD_UNLOAD      := 0x80000001
DEFINE TAPE_DRIVE_TENSION          := 0x80000002
DEFINE TAPE_DRIVE_LOCK_UNLOCK      := 0x80000004
DEFINE TAPE_DRIVE_REWIND_IMMEDIATE := 0x80000008
DEFINE TAPE_DRIVE_SET_BLOCK_SIZE   := 0x80000010
DEFINE TAPE_DRIVE_LOAD_UNLD_IMMED  := 0x80000020
DEFINE TAPE_DRIVE_TENSION_IMMED    := 0x80000040
DEFINE TAPE_DRIVE_LOCK_UNLK_IMMED  := 0x80000080
DEFINE TAPE_DRIVE_SET_ECC          := 0x80000100
DEFINE TAPE_DRIVE_SET_COMPRESSION  := 0x80000200
DEFINE TAPE_DRIVE_SET_PADDING      := 0x80000400
DEFINE TAPE_DRIVE_SET_REPORT_SMKS  := 0x80000800
DEFINE TAPE_DRIVE_ABSOLUTE_BLK     := 0x80001000
DEFINE TAPE_DRIVE_ABS_BLK_IMMED    := 0x80002000
DEFINE TAPE_DRIVE_LOGICAL_BLK      := 0x80004000
DEFINE TAPE_DRIVE_LOG_BLK_IMMED    := 0x80008000
DEFINE TAPE_DRIVE_END_OF_DATA      := 0x80010000
DEFINE TAPE_DRIVE_RELATIVE_BLKS    := 0x80020000
DEFINE TAPE_DRIVE_FILEMARKS        := 0x80040000
DEFINE TAPE_DRIVE_SEQUENTIAL_FMKS  := 0x80080000
DEFINE TAPE_DRIVE_SETMARKS         := 0x80100000
DEFINE TAPE_DRIVE_SEQUENTIAL_SMKS  := 0x80200000
DEFINE TAPE_DRIVE_REVERSE_POSITION := 0x80400000
DEFINE TAPE_DRIVE_SPACE_IMMEDIATE  := 0x80800000
DEFINE TAPE_DRIVE_WRITE_SETMARKS   := 0x81000000
DEFINE TAPE_DRIVE_WRITE_FILEMARKS  := 0x82000000
DEFINE TAPE_DRIVE_WRITE_SHORT_FMKS := 0x84000000
DEFINE TAPE_DRIVE_WRITE_LONG_FMKS  := 0x88000000
DEFINE TAPE_DRIVE_WRITE_MARK_IMMED := 0x90000000
DEFINE TAPE_DRIVE_FORMAT           := 0xA0000000
DEFINE TAPE_DRIVE_FORMAT_IMMEDIATE := 0xC0000000
DEFINE TAPE_DRIVE_HIGH_FEATURES    := 0x80000000
DEFINE TAPE_FIXED_PARTITIONS       := 0L
DEFINE TAPE_SELECT_PARTITIONS      := 1L
DEFINE TAPE_INITIATOR_PARTITIONS   := 2L
DEFINE WT_EXECUTEDEFAULT      := 0x00000000
DEFINE WT_EXECUTEINIOTHREAD   := 0x00000001
DEFINE WT_EXECUTEINUITHREAD   := 0x00000002
DEFINE WT_EXECUTEINWAITTHREAD := 0x00000004
DEFINE WT_EXECUTEDELETEWAIT   := 0x00000008
DEFINE WT_EXECUTEINLONGTHREAD := 0x00000010
#endregion
