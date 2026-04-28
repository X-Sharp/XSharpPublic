//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

//  DELDBE.CH


DEFINE  DELDBE_MODE             := DBE_USER+1
DEFINE  DELDBE_RECORD_TOKEN     := DBE_USER+2
DEFINE  DELDBE_FIELD_TOKEN      := DBE_USER+3
DEFINE  DELDBE_DECIMAL_TOKEN    := DBE_USER+4
DEFINE  DELDBE_DELIMITER_TOKEN  := DBE_USER+5
DEFINE  DELDBE_LOGICAL_TOKEN    := DBE_USER+6
DEFINE  DELDBE_MAX_BUFFERSIZE   := DBE_USER+7
DEFINE  DELDBE_FIELD_TYPES      := DBE_USER+8
DEFINE  DELDBE_IGNORE_CTRL_CHAR := DBE_USER+9
DEFINE  DELDBE_SHARED_SUPPORT   := DBE_USER+10

DEFINE  DELDBO_MODE             := DBE_USER+1

DEFINE  DELDBE_SINGLEFIELD      := 1
DEFINE  DELDBE_MULTIFIELD       := 2
DEFINE  DELDBE_AUTOFIELD        := 3

//  DBFDBE.CH
DEFINE  DBFDBE_MEMOFILE_EXT    := DBE_USER+1
DEFINE  DBFDBE_MEMOBLOCKSIZE   := DBE_USER+2
DEFINE  DBFDBE_LOCKMODE        := DBE_USER+3
DEFINE  DBFDBE_LOCKOFFSET      := DBE_USER+4
DEFINE  DBFDBE_LOCKDELAY       := DBE_USER+5
DEFINE  DBFDBE_LOCKRETRY       := DBE_USER+6
DEFINE  DBFDBE_CACHESIZE       := DBE_USER+7
DEFINE  DBFDBE_LIFETIME        := DBE_USER+8
DEFINE  DBFDBE_LOCKDELAY_MIN   := DBE_USER+9
DEFINE  DBFDBE_LOCKRETRY_MIN   := DBE_USER+10
DEFINE  DBFDBE_ANSI            := DBE_USER+11

// DBO instances of DBFDBE can use:
//
DEFINE  DBFDBO_LOCKMODE        := DBE_USER+1
DEFINE  DBFDBO_LOCKOFFSET      := DBE_USER+2
DEFINE  DBFDBO_LOCKDELAY       := DBE_USER+3
DEFINE  DBFDBO_LOCKRETRY       := DBE_USER+4
DEFINE  DBFDBO_DBFHANDLE       := DBE_USER+5
DEFINE  DBFDBO_DBTHANDLE       := DBE_USER+6
DEFINE  DBFDBO_ANSI            := DBE_USER+7


DEFINE DBF_NOLOCK             := 1

// enhanced OS/2 mode performs autolocking for
// append, read/write, delete, recall
DEFINE DBF_AUTOLOCK           := 2



//  DBTYPES.CH
DEFINE XPP_BLOB           := 16384


//  NLS.CH

define NLS_ICOUNTRY            := 1

// separators
define NLS_SLIST               := 2
define NLS_SDECIMAL            := 3
define NLS_STHOUSAND           := 4

// currency
define NLS_SCURRENCY           := 5
define NLS_ICURRENCY           := 6
define NLS_ICURRDIGITS         := 8

// date/time
define NLS_SDATE             :=   9
define NLS_IDATE              := 10
define NLS_ICENTURY           := 11
define NLS_STIME              := 12
define NLS_ITIME              := 13
define NLS_S1159              := 14
define NLS_S2359              := 15

// logical values
define NLS_SYES               := 16
define NLS_SNO                := 17

// weekdays, depending on country settings of OS
define NLS_SDAYNAME1          := 18
define NLS_SDAYNAME2          := 19
define NLS_SDAYNAME3          := 20
define NLS_SDAYNAME4          := 21
define NLS_SDAYNAME5          := 22
define NLS_SDAYNAME6          := 23
define NLS_SDAYNAME7          := 24

// month names, depending on country settings of OS
define NLS_SMONTHNAME1        := 25
define NLS_SMONTHNAME2        := 26
define NLS_SMONTHNAME3        := 27
define NLS_SMONTHNAME4        := 28
define NLS_SMONTHNAME5        := 29
define NLS_SMONTHNAME6        := 30
define NLS_SMONTHNAME7        := 31
define NLS_SMONTHNAME8        := 32
define NLS_SMONTHNAME9        := 33
define NLS_SMONTHNAME10       := 34
define NLS_SMONTHNAME11       := 35
define NLS_SMONTHNAME12       := 36

define NLS_ICURRENCYEURO      := 37

// short names for weekdays, depending on country settings of OS
define NLS_SABBREVDAYNAME1    := 38
define NLS_SABBREVDAYNAME2    := 39
define NLS_SABBREVDAYNAME3    := 40
define NLS_SABBREVDAYNAME4    := 41
define NLS_SABBREVDAYNAME5    := 42
define NLS_SABBREVDAYNAME6    := 43
define NLS_SABBREVDAYNAME7    := 44

// short names for month names, depending on country settings of OS
define NLS_SABBREVMONTHNAME1  := 45
define NLS_SABBREVMONTHNAME2  := 46
define NLS_SABBREVMONTHNAME3  := 47
define NLS_SABBREVMONTHNAME4  := 48
define NLS_SABBREVMONTHNAME5  := 49
define NLS_SABBREVMONTHNAME6  := 50
define NLS_SABBREVMONTHNAME7  := 51
define NLS_SABBREVMONTHNAME8  := 52
define NLS_SABBREVMONTHNAME9  := 53
define NLS_SABBREVMONTHNAME10 := 54
define NLS_SABBREVMONTHNAME11 := 55
define NLS_SABBREVMONTHNAME12 := 56


// weekdays, fixed
define NLS_SFDAYNAME1        := 57
define NLS_SFDAYNAME2        := 58
define NLS_SFDAYNAME3        := 59
define NLS_SFDAYNAME4        := 60
define NLS_SFDAYNAME5        := 61
define NLS_SFDAYNAME6        := 62
define NLS_SFDAYNAME7        := 63



// month names, fixed
define NLS_SFMONTHNAME1       := 64
define NLS_SFMONTHNAME2       := 65
define NLS_SFMONTHNAME3       := 66
define NLS_SFMONTHNAME4       := 67
define NLS_SFMONTHNAME5       := 68
define NLS_SFMONTHNAME6       := 69
define NLS_SFMONTHNAME7       := 70
define NLS_SFMONTHNAME8       := 71
define NLS_SFMONTHNAME9       := 72
define NLS_SFMONTHNAME10      := 73
define NLS_SFMONTHNAME11      := 74
define NLS_SFMONTHNAME12      := 75

// the time zone string
define NLS_STIMEZONE          := 76
// the time zone bias local time to UTC (minutes) incl. NLS_ITZDAYLIGHTBIAS
define NLS_ITZBIAS            := 77
// additional bias during daylight saving time
define NLS_ITZDAYLIGHTBIAS    := 78

// code page id's
define NLS_IANSICP            := 79
define NLS_IOEMCP             := 80


define NLS_COUNT              := 80

// defines for LocaleConfigure()
define LOCALE_COLLATION       := 1000
define LOCALE_CHAR_TYPE       := 1001
define LOCALE_TO_UPPER        := 1002
define LOCALE_TO_LOWER        := 1003
define LOCALE_ANSI_TO_OEM     := 1004
define LOCALE_OEM_TO_ANSI     := 1005


// language defines
define LANG_DEFAULT            := 0x400   // user default language

