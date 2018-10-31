DEFINE   F_ERROR := PTR(_CAST,0xFFFFFFFF) // Error value (all functions)
// FERROR() returns, which are not reflected as DOSERROR()
DEFINE   FERROR_FULL    := 256   // disk full
DEFINE   FERROR_EOF     := 257   // eof was already reached, when a read was tried
DEFINE   FERROR_PARAM   := 258   // invalid parameter already detected before giving to DOS
// FSEEK(), _llseek() modes
DEFINE   FS_SET         := 0  // Seek from beginning of file
DEFINE   FS_RELATIVE    := 1  // Seek from current file position
DEFINE   FS_END         := 2  // Seek from end of file
// FOPEN() access modes
DEFINE   FO_READ        := 0  // Open for reading (default)
DEFINE   FO_WRITE       := 1  // Open for writing
DEFINE   FO_READWRITE   := 2  // Open for reading or writing
// FOPEN() sharing modes (combine with open mode using +)
DEFINE FO_COMPAT     := 0x00000000  // Compatibility mode (default)
DEFINE FO_EXCLUSIVE  := 0x00000010  // Exclusive
DEFINE FO_DENYWRITE  := 0x00000020  // Prevent other processes from writing
DEFINE FO_DENYREAD   := 0x00000030  // Prevent other processes from reading
DEFINE FO_DENYNONE   := 0x00000040  // (same as FO_SHARED)
DEFINE FO_SHARED     := 0x00000040  // Allow other processes to read or write
// FXOPEN() mode
DEFINE FXO_WILD      := 0x00010000  // Allow wildcards in file name
// FCREATE() file attribute modes (always opens with OF_READWRITE)
DEFINE FC_NORMAL     := 0x00000000  // normal read/write file (default for create)
DEFINE FC_READONLY   := 0x00000001  // read-only file
DEFINE FC_HIDDEN     := 0x00000002  // hidden file
DEFINE FC_SYSTEM     := 0x00000004  // system file
DEFINE FC_ARCHIVED   := 0x00000020
//
// additional file attribute for DIRECTORY(), FFIRST() and FFCOUNT()
//
DEFINE FA_VOLUME     := 0x00000008
DEFINE FA_DIRECTORY  := 0x00000010
// UH 01/09/1996
DEFINE FA_TEMPORARY  := 0x00000100
DEFINE FA_COMPRESSED := 0x00000800
DEFINE FA_OFFLINE    := 0x00001000
// !!! Sabo 12/05/1995
//
// subscripts for Directory() array
//
DEFINE F_NAME  := 1
DEFINE F_SIZE  := 2
DEFINE F_DATE  := 3
DEFINE F_TIME  := 4
DEFINE F_ATTR  := 5
DEFINE F_LEN   := 5   // length of array
//
// Standard math definitions
//
// RvdH 040122 Adjusted some constants and added a couple
DEFINE PI      :=    3.14159265358979323846      // Pi
DEFINE PI_2    :=    1.57079632679489661923      // Pi/2  ** NEW !
DEFINE PI_4    :=    0.785398163397448309616     // Pi/4  ** NEW !
DEFINE L2E     :=    1.44269504088896340736      // Log2 ( e)
DEFINE L2T     :=    3.32192809488736234781      // Log2 (10)
DEFINE L10E    :=    0.434294481903251827651     // Log10 (e) ** NEW !
DEFINE LG2     :=    0.301029995663981195226     // Log10( 2)
DEFINE LN2     :=    0.693147180559945309417     // LogE ( 2)
DEFINE LN10    :=    2.30258509299404568402      // LogE ( 10) ** NEW !
DEFINE REAL4_EPSILON := 1.192092896e-07          // smallest such that 1.0+REAL4_EPSILON != 1.0
DEFINE REAL4_MAX     := 3.402823466e+38          // Maximum representable number
DEFINE REAL8_EPSILON := 2.2204460492503131e-016  // smallest such that 1.0+REAL8_EPSILON != 1.0
DEFINE REAL8_MAX     := 1.7976931348623158e+308  // Maximum representable number
DEFINE TICK_FREQUENCY   :=    18.20647  // ticks per second 
//
// ASCII character equates
//
DEFINE ASC_BELL   :=     7    // Bell
DEFINE ASC_BS     :=     8    // BackSpace
DEFINE ASC_TAB    :=     9    // Tab
DEFINE ASC_LF     :=    10    // LineFeed
DEFINE ASC_FF     :=    12    // FormFeed
DEFINE ASC_CR     :=    13    // CarriageReturn
DEFINE ASC_SOFT_CR   :=    141      // softCarriageReturn
DEFINE ASC_EOF    :=    26    // EndOfFile
DEFINE ASC_ESC    :=    27    // Escape
DEFINE ASC_BLANK  :=    32
DEFINE ASC_0      :=    48
DEFINE ASC_1      :=    49
DEFINE ASC_9      :=    57
DEFINE ASC_A      :=    65
DEFINE ASC_Z      :=    90
#warning CRLF
//DEFINE CRLF       := _CHR(ASC_CR) +_CHR(ASC_LF)
//
// .INI group name
//
DEFINE INI_GROUP_RUNTIME :=   "CA-Visual Objects"
// Flag for second RegisterKid() parameter when
// called by Methods of Objects having an Axit()
DEFINE KID_IN_AXIT      := 0x20000000
// Globals from VO28RUN.DLL
// UH
// _DLL GLOBAL _lpKIDSP        AS PTR    :VO28RUN.lpKIDSP_RT
DEFINE MAX_ALLOC :=  65527  // 0xFFF7, biggest block size
DEFINE _MAX_PATH  := 260
DEFINE _MAX_DRIVE := 3
DEFINE _MAX_DIR   := 256
DEFINE _MAX_FNAME := 256
DEFINE _MAX_EXT   := 256
//FILE.C
DEFINE MEMORY_COLLECT               := -1
DEFINE MEMORY_SYSTEM_FREE           := 0
DEFINE MEMORY_SYSTEM_MAX            := 1
DEFINE MEMORY_DYNINFOFREE           := 2
DEFINE MEMORY_DYNINFOMAX            := 3
DEFINE MEMORY_KIDSTACK_SIZE         := 4
DEFINE MEMORY_KIDSTACK_FREE         := 5
DEFINE MEMORY_STACK_SIZE            := 6
DEFINE MEMORY_STACK_FREE            := 7
DEFINE MEMORY_MAXATOM               := 8
DEFINE MEMORY_ACTIVATION            := 9
DEFINE MEMORY_PUBLIC             := 10
DEFINE MEMORY_PRIVAT             := 11
DEFINE MEMORY_DYNINFOUSED           := 12
DEFINE MEMORY_MEMTOTAL              := 13
DEFINE MEMORY_DS_SIZE               := 14
DEFINE MEMORY_CS_SIZE               := 15
DEFINE MEMORY_REGISTEREXIT_COUNT    := 16
DEFINE MEMORY_REGCOLLNOTIFYSTART_COUNT := 17
DEFINE MEMORY_REGCOLLNOTIFYEND_COUNT   := 18
DEFINE MEMORY_REGISTERKID           := 19
DEFINE MEMORY_REGISTERAXIT          := 20
DEFINE MEMORY_COLLECTCOUNT          := 21
DEFINE MEMORY_DYNINFOSIZE           := 22
DEFINE MEMORY_RT_DGROUP             := 23
DEFINE MEMORY_RT_DS              := 24
DEFINE MEMORY_SS                 := 25
DEFINE MEMORY_DS                 := 26
DEFINE MEMORY_CS                 := 27
DEFINE MEMORY_SEQUENCE              := 28
DEFINE MEMORY_STACKKID              := 29
DEFINE MEMORY_SP                 := 30
DEFINE MEMORY_GLOBALSEL             := 31
DEFINE MEMORY_FUNCTIONCOUNT         := 32
DEFINE MEMORY_CLASSCOUNT            := 33
DEFINE MEMORY_DB_DS              := 34
DEFINE MEMORY_DB_DS_SIZE            := 35
DEFINE MEMORY_WINDOWS_SYSTEMRESOURCES  :=100
DEFINE MEMORY_WINDOWS_GDIRESOURCES     :=101
DEFINE MEMORY_WINDOWS_USERRESOURCES    :=102
//OSPRINTF.C
DEFINE AMERICAN := 1 // mm/dd/yy  mm/dd/yyyy
DEFINE ANSI    := 2  // yy.mm.dd  yyyy.mm.dd
DEFINE BRITISH := 3  // dd/mm/yy  dd/mm/yyyy
DEFINE FRENCH  := 4  // dd/mm/yy  dd/mm/yyyy
DEFINE GERMAN  := 5  // dd.mm.yy  dd.mm.yyyy
DEFINE ITALIAN := 6  // dd-mm-yy  dd-mm-yyyy
DEFINE JAPANESE := 7 // yy/mm/dd  yyyy/mm/dd
DEFINE USA     := 8  // mm-dd-yy  mm-dd-yyyy
DEFINE JUL_BASE := 1721060
//MCOMP.C
DEFINE CLASS_DEBUG_ALLOC	:= 0x01
DEFINE CLASS_DEBUG_INIT		:= 0x02
DEFINE CLASS_DEBUG_AXIT	 	:= 0x04
DEFINE CLASS_DEBUG_SEND	 	:= 0x08
DEFINE CLASS_DEBUG_IVARGET	:= 0x10
DEFINE CLASS_DEBUG_IVARPUT  	:= 0x20
DEFINE CLASS_DEBUG_DECLARE  	:= 0x40
DEFINE CLASS_DEBUG_UNDECL   	:= 0x80
DEFINE CLASS_DEBUG_METHDECL 	:= 0x100
DEFINE CLASS_DEBUG_NOIVARGET 	:= 0x200
DEFINE CLASS_DEBUG_NOIVARPUT 	:= 0x400
DEFINE CLASS_DEBUG_ALL 		:= 0xFFFFFFFF
