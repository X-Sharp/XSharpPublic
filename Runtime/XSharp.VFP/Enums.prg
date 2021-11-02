//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
/// <summary>List of possible index collations for VFP DBF files. For each of these collations there is a weight table in the XSharp.VFP assembly.</summary>
ENUM XSharp.FoxCollations
 //MEMBER ARABIC
 /// <summary>CZech collation. Supported are codepages 852, 895 and 1250</summary>
 MEMBER CZECH
 /// <summary>Dutch collation. Supported are codepages 437, 850 and 1252</summary>
 MEMBER DUTCH
 /// <summary>General collation. Supported are codepages 437, 620, 850,852,857,861,865,895,1250,1252 and 1254</summary>
 MEMBER GENERAL
/// <summary>German phone book order (DIN) collation. Supported are codepages 437, and 1252<br/>
/// This collation is used for English, French, German, Modern Spanish, Portuguese, and other Western European languages</summary>
 MEMBER GERMAN
/// <summary>Greek collation. Supported are codepages 737, and 1253</summary>
 MEMBER GREEK
 //MEMBER HEBREW
 /// <summary>Hungarian collation. Supported are codepages 852, and 1250</summary>
 MEMBER HUNGARY
 /// <summary>Icelandic collation. Supported are codepages 437,, 850, 861 and 1252</summary>
 MEMBER ICELAND
 //MEMBER JAPANESE
 //MEMBER KOREAN
 /// <summary>Machine collation. Supported are codepages 437, 620, 850,852,857,861,865,895,1250,1251,1252,1253 and 1254</summary>
 MEMBER MACHINE
 /// <summary>Norwegian, Danish collation. Supported are codepages 437, 850, 865 and 1252.</summary>
 MEMBER NORDAN
 //MEMBER PINYIN
 /// <summary>Polish collation. Supported are codepages 620, 852 and 1250</summary>
 MEMBER POLISH
 /// <summary>Russian collation. Supported are codepages 866 and 1251</summary>
 MEMBER RUSSIAN
 /// <summary>Slovakian collation. Supported are codepages 852, 895 and 1250</summary>
 MEMBER SLOVAK
 /// <summary>Traditional Spanish collation. Supported are codepages 437, 850 and 1252</summary>
 MEMBER SPANISH
 //MEMBER STROKE
 /// <summary>Swedish/Finnish collation. Supported are codepages 437, 850, 865 and 1252</summary>
 MEMBER SWEFIN
 //MEMBER THAI
 /// <summary>Turkish collation. Supported are codepages 857 and 1254</summary>
 MEMBER TURKISH
 /// <summary>Unique Weight collation. Supported are codepages 437, 850 and 1252</summary>
 MEMBER UNIQWT
END ENUM

/// <exclude/>
ENUM XSharp.VFP.PropertyVisibility
   MEMBER Public    := 1
   MEMBER Protected := 2
   MEMBER Hidden    := 3
END ENUM

/// <summary>List of error codes used in the XSharp VTP support. <br />
/// Each error code represents a translatable string in the string table inside XSharp.Core.
/// </summary>

ENUM XSharp.VFPErrors
    // Make sure that the member names do not collide with the member names inside the VOErrors Enum
    MEMBER XS_VFP_BASE   := 1
    MEMBER INVALID_DATE
    MEMBER INVALID_MONTH
    MEMBER INVALID_FORMAT
    MEMBER INVALID_DB_OBJECT
    MEMBER INVALID_DB_PROPERTY_NAME
    MEMBER INVALID_FIELD_SPEC
    MEMBER INVALID_FIELDNAME
    MEMBER STATEMENT_HANDLE_INVALID
    MEMBER STATEMENT_HANDLE_NOTSHARED
    MEMBER COMMAND_PARAMETER_REQUIRED
    MEMBER SQLCOLUMNS_CTYPE
    MEMBER PROPERTY_UNKNOWN
    MEMBER INVALID_RANGE
    MEMBER WITH_STACK_EMPTY
    MEMBER VARIABLE_NOT_ARRAY
    MEMBER VARIABLE_DOES_NOT_EXIST
    MEMBER ONE_DIM_NO_COLUMNS
    MEMBER ATTRIBUTE_OUT_OF_RANGE
    MEMBER PROPERTY_NOT_FOUND
    MEMBER MULTI_DIM_EXPECTED
    MEMBER SUBARRAY_TOO_SMALL
    MEMBER COLLATION_NOT_FOUND

END ENUM

