//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.


DEFINE F_ERROR := PTR(_CAST,0xFFFFFFFF) // Error value (all functions)

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
DEFINE FA_NORMAL	 := 0x00000080
DEFINE FA_TEMPORARY  := 0x00000100
DEFINE FA_COMPRESSED := 0x00000800
DEFINE FA_OFFLINE    := 0x00001000


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
DEFINE ASC_SOFT_CR :=   141      // softCarriageReturn
DEFINE ASC_EOF    :=    26    // EndOfFile
DEFINE ASC_ESC    :=    27    // Escape
DEFINE ASC_BLANK  :=    32
DEFINE ASC_0      :=    48
DEFINE ASC_1      :=    49
DEFINE ASC_9      :=    57
DEFINE ASC_A      :=    65
DEFINE ASC_Z      :=    90
#ifndef CRLF
DEFINE CRLF       := Chr(ASC_CR) +Chr(ASC_LF)
#endif
DEFINE INI_GROUP_RUNTIME :=   "CA-Visual Objects"
