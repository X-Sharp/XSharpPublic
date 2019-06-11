//////////////////////////////////////////////////////////////////////
//
//  COLLAT_CH.PRG
//
//  
//  Contents:
//     define constants for the collation handling SET(_SET_COLLATION, ...)
//   
//  Remarks:
//     When calling the function set(_SET_COLLATION) you should not use 
//     the value defined here directly but only the defined names. The
//     values may change in future versions.
//   
//////////////////////////////////////////////////////////////////////

//  COLLAT.CH
DEFINE COLLAT_SYSTEM         :=   0
DEFINE COLLAT_GERMAN         :=   1
DEFINE COLLAT_BRITISH        :=   2
DEFINE COLLAT_AMERICAN       :=   2
DEFINE COLLAT_FINNISH        :=   3
DEFINE COLLAT_FRENCH         :=   4
DEFINE COLLAT_DANISH         :=   5
DEFINE COLLAT_GREEK437       :=   6
DEFINE COLLAT_GREEK851       :=   7
DEFINE COLLAT_ICELANDIC850   :=   8
DEFINE COLLAT_ICELANDIC861   :=   9
DEFINE COLLAT_ITALIAN        :=  10
DEFINE COLLAT_NORWEGIAN      :=  11
DEFINE COLLAT_PORTUGUESE     :=  12
DEFINE COLLAT_SPANISH        :=  13
DEFINE COLLAT_SWEDISH        :=  14
DEFINE COLLAT_DUTCH          :=  15
DEFINE COLLAT_USER           :=  16
DEFINE COLLAT_ASCII          :=  -1   
DEFINE COLLAT_COUNT          :=  17

DEFINE CHARSET_OEM       := 1
DEFINE CHARSET_ANSI      := 0
