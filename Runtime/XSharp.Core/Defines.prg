//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.

 

//
// subscripts for Directory() array
//
/// <summary>Position in Directory() sub-array for fileName (as a string).</summary>
/// <include file="CoreComments.xml" path="Comments/Directory/*" />
DEFINE F_NAME  := 1
/// <summary>Position in Directory() sub-array for filesize(as a numeric).</summary>
/// <include file="CoreComments.xml" path="Comments/Directory/*" />
DEFINE F_SIZE  := 2
/// <summary>Position in Directory() sub-array for last file write date(as a date).</summary>
/// <include file="CoreComments.xml" path="Comments/Directory/*" />
DEFINE F_DATE  := 3
/// <summary>Position in Directory() sub-array for last file write date(as a date).</summary>
/// <include file="CoreComments.xml" path="Comments/Directory/*" />
DEFINE F_WRITE_DATE  := 3
/// <summary>Position in Directory() sub-array for last file write time(as a string).</summary>
/// <include file="CoreComments.xml" path="Comments/Directory/*" />
DEFINE F_TIME  := 4
/// <summary>Position in Directory() sub-array for last file write time(as a string).</summary>
/// <include file="CoreComments.xml" path="Comments/Directory/*" />
DEFINE F_WRITE_TIME  := 4
/// <summary>Position in Directory() sub-array for file attributes(as a number).</summary>
/// <include file="CoreComments.xml" path="Comments/Directory/*" />
DEFINE F_ATTR  := 5
/// <summary>Position in Directory() sub-array for Size of extended attributes(as a string).</summary>
/// <include file="CoreComments.xml" path="Comments/Directory/*" />
DEFINE F_EA_SIZE := 6
/// <summary>Position in Directory() sub-array for creation date(as a date).</summary>
/// <include file="CoreComments.xml" path="Comments/Directory/*" />
DEFINE F_CREATION_DATE := 7
/// <summary>Position in Directory() sub-array for creation time(as a string).</summary>
/// <include file="CoreComments.xml" path="Comments/Directory/*" />
DEFINE F_CREATION_TIME := 8
/// <summary>Position in Directory() sub-array for last access date(as a date).</summary>
/// <include file="CoreComments.xml" path="Comments/Directory/*" />
DEFINE F_ACCESS_DATE := 9
/// <summary>Position in Directory() sub-array for last access time(as a string).</summary>
/// <include file="CoreComments.xml" path="Comments/Directory/*" />
DEFINE F_ACCESS_TIME := 10
/// <summary>Length of Directory() sub-array.</summary>
/// <include file="CoreComments.xml" path="Comments/Directory/*" />
DEFINE F_LEN   := 10   // length of




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
DEFINE ASC_TAB    :=     9    // Tab
DEFINE ASC_SOFT_CR :=   141      // softCarriageReturn
DEFINE ASC_EOF    :=    26    // EndOfFile
#ifndef CRLF
DEFINE	 CRLF       := e"\r\n"
#endif

DEFINE SE_ABORT := 0
DEFINE SE_IGNORE := 1
DEFINE SE_RETRY := 2
DEFINE SE_CANCEL := 3
DEFINE SE_OK := 4
DEFINE SE_YES := 5
DEFINE SE_NO := 6
DEFINE SE_CLOSE := 7
DEFINE SE_DEFAULT := 0x80000000
DEFINE EC_ALERT := 0
DEFINE EC_IGNORE := 1
DEFINE EC_RETRY := 2
DEFINE EC_BREAK := 3
DEFINE REG_BUFF_SIZE := 64
DEFINE INT_WINDOWS := 1
DEFINE INT_CLIPPER := 0
DEFINE INT_BINARY := 2


DEFINE INTERNET_ERROR_BASE := 12000
DEFINE VOVER_BUILDNUMBER := __VERSION__
DEFINE SUCCESS := 0
DEFINE FAILURE := -1
DEFINE DBOI_LOCK_ALL := 100
DEFINE DBOI_LOCK_FAIL := 101
DEFINE DBOI_HPLOCK_GATE := 102
DEFINE RDD_INFO := 100
DEFINE RDD_INFO_MAX := 114
DEFINE NEW_AREA := 0
DEFINE BUFF_SIZE := 0x00008000
DEFINE MAXFILENAME := 260
DEFINE MAX_EXT_NAME := 5
DEFINE DB_MAXAREAS := 1024
DEFINE MAXDRIVERNAME := 12
DEFINE MAX_KEY_LEN := 256
DEFINE DBF_ANSI := 0x04
DEFINE DBF_VER := 0x03
DEFINE DBF_MEMO := 0x80
DEFINE DBF_DB4MEMO := 0x88
DEFINE DBF_PRODINDEX := 0x01
DEFINE DBF_MEMOS := 0x02
DEFINE DBF_ISDATABASE := 0x04
DEFINE DBF_OLE := 0x80
DEFINE _MAX_DIR := 256
DEFINE _MAX_FNAME := 256
DEFINE _MAX_EXT := 256

DEFINE MAX_INST := 10
DEFINE ASC_BS := 8
DEFINE ASC_LF := 10
DEFINE ASC_FF := 12
DEFINE ASC_CR := 13
DEFINE ASC_ESC := 27
DEFINE ASC_BLANK := 32
DEFINE ASC_0 := 48
DEFINE ASC_1 := 49
DEFINE ASC_9 := 57
DEFINE ASC_A := 65
DEFINE ASC_Z := 90
DEFINE MAX_ALLOC := 65527
DEFINE _MAX_DRIVE := 3
