//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING XSharp
BEGIN NAMESPACE XSharp
/// <summary>Values that match the XBase DateCountry defines. They are used to set the date format.</summary>
/// <remarks>Please note that although there are different values for British and French, the date formats are the same.</remarks>
PUBLIC ENUM DateCountry
    /// <include file="XSharp.CoreDefines.xml" path="members/DateCountry.American/*" />
	MEMBER American := 1
    /// <include file="XSharp.CoreDefines.xml" path="members/DateCountry.Ansi/*" />
	MEMBER Ansi     := 2
    /// <include file="XSharp.CoreDefines.xml" path="members/DateCountry.British/*" />
	MEMBER British  := 3
    /// <include file="XSharp.CoreDefines.xml" path="members/DateCountry.British/*" />
	MEMBER French   := 4
    /// <include file="XSharp.CoreDefines.xml" path="members/DateCountry.German/*" />
	MEMBER German   := 5
    /// <include file="XSharp.CoreDefines.xml" path="members/DateCountry.Italian/*" />
	MEMBER Italian  := 6
    /// <include file="XSharp.CoreDefines.xml" path="members/DateCountry.Italian/*" />
    MEMBER Dutch    := 6
    /// <include file="XSharp.CoreDefines.xml" path="members/DateCountry.Japanese/*" />
    MEMBER Japanese := 7
    /// <include file="XSharp.CoreDefines.xml" path="members/DateCountry.Japanese/*" />
	MEMBER USA      := 8
    /// <include file="XSharp.CoreDefines.xml" path="members/DateCountry.Taiwan/*" />
	MEMBER Taiwan   := 9
    /// <include file="XSharp.CoreDefines.xml" path="members/DateCountry.American/*" />
	MEMBER MDY      := 10
    /// <include file="XSharp.CoreDefines.xml" path="members/DateCountry.British/*" />
	MEMBER DMY      := 11
    /// <include file="XSharp.CoreDefines.xml" path="members/DateCountry.Taiwan/*" />
	MEMBER YMD      := 12
    /// <include file="XSharp.CoreDefines.xml" path="members/DateCountry.System/*" />
    MEMBER System   := 99
    /// <include file="XSharp.CoreDefines.xml" path="members/DateCountry.System/*" />
    MEMBER Windows  := 99
END ENUM

/// <summary>This enum specifies the collation mode used to create and update index strings.</summary>
ENUM CollationMode
	MEMBER Windows
	MEMBER Clipper
	MEMBER Unicode
	MEMBER Ordinal
    MEMBER Xpp
END ENUM

END NAMESPACE
#region defines
/// <include file="XSharp.CoreDefines.xml" path="members/DateCountry.American/*" />
DEFINE AMERICAN := DateCountry.American
/// <include file="XSharp.CoreDefines.xml" path="members/DateCountry.Ansi/*" />
DEFINE ANSI     := DateCountry.Ansi
/// <include file="XSharp.CoreDefines.xml" path="members/DateCountry.British/*" />
DEFINE BRITISH  := DateCountry.British
/// <include file="XSharp.CoreDefines.xml" path="members/DateCountry.British/*" />
DEFINE FRENCH   := DateCountry.French
/// <include file="XSharp.CoreDefines.xml" path="members/DateCountry.German/*" />
DEFINE GERMAN   := DateCountry.German
/// <include file="XSharp.CoreDefines.xml" path="members/DateCountry.Italian/*" />
DEFINE ITALIAN  := DateCountry.Italian
/// <include file="XSharp.CoreDefines.xml" path="members/DateCountry.Italian/*" />
DEFINE DUTCH    := DateCountry.Dutch
/// <include file="XSharp.CoreDefines.xml" path="members/DateCountry.Japanese/*" />
DEFINE JAPANESE := DateCountry.Japanese
/// <include file="XSharp.CoreDefines.xml" path="members/DateCountry.USA/*" />
DEFINE USA      := DateCountry.USA
/// <include file="XSharp.CoreDefines.xml" path="members/DateCountry.British/*" />
DEFINE DMY      := DateCountry.DMY
/// <include file="XSharp.CoreDefines.xml" path="members/DateCountry.American/*" />
DEFINE MDY      := DateCountry.MDY
/// <include file="XSharp.CoreDefines.xml" path="members/DateCountry.Japanese/*" />
DEFINE YMD      := DateCountry.YMD
/// <include file="XSharp.CoreDefines.xml" path="members/DateCountry.Taiwan/*" />
DEFINE TAIWAN   := DateCountry.Taiwan

#endregion


