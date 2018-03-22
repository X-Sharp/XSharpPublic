//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.



//
// subscripts for Directory() array
//
define F_NAME  := 1
define F_SIZE  := 2
define F_DATE  := 3
define F_TIME  := 4
define F_ATTR  := 5
define F_LEN   := 5   // length of array


//
// Standard math definitions
//

define PI      :=    3.14159265358979323846      // Pi
define PI_2    :=    1.57079632679489661923      // Pi/2  ** NEW !
define PI_4    :=    0.785398163397448309616     // Pi/4  ** NEW !
define L2E     :=    1.44269504088896340736      // Log2 ( e)
define L2T     :=    3.32192809488736234781      // Log2 (10)
define L10E    :=    0.434294481903251827651     // Log10 (e) ** NEW !
define LG2     :=    0.301029995663981195226     // Log10( 2)
define LN2     :=    0.693147180559945309417     // LogE ( 2)
define LN10    :=    2.30258509299404568402      // LogE ( 10) ** NEW !

define REAL4_EPSILON := 1.192092896e-07          // smallest such that 1.0+REAL4_EPSILON != 1.0
define REAL4_MAX     := 3.402823466e+38          // Maximum representable number
define REAL8_EPSILON := 2.2204460492503131e-016  // smallest such that 1.0+REAL8_EPSILON != 1.0
define REAL8_MAX     := 1.7976931348623158e+308  // Maximum representable number


define TICK_FREQUENCY   :=    18.20647  // ticks per second 

//
// ASCII character equates
//

define ASC_BELL   :=     7    // Bell
define ASC_BS     :=     8    // BackSpace
define ASC_TAB    :=     9    // Tab
define ASC_LF     :=    10    // LineFeed
define ASC_FF     :=    12    // FormFeed
define ASC_CR     :=    13    // CarriageReturn
define ASC_SOFT_CR :=   141      // softCarriageReturn
define ASC_EOF    :=    26    // EndOfFile
define ASC_ESC    :=    27    // Escape
define ASC_BLANK  :=    32
define ASC_0      :=    48
define ASC_1      :=    49
define ASC_9      :=    57
define ASC_A      :=    65
define ASC_Z      :=    90
#ifndef CRLF
define	 CRLF       := Chr(ASC_CR) +Chr(ASC_LF)
#endif
define INI_GROUP_RUNTIME :=   "CA-Visual Objects"
