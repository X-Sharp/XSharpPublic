//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.



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
DEFINE	 CRLF       := e"\r\n"
#endif
DEFINE INI_GROUP_RUNTIME :=   "CA-Visual Objects"
