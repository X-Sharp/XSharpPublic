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
    // <summary>mm/dd/yy  mm/dd/yyyy</summary>
	MEMBER American := 1   

    // <summary>yy.mm.dd  yyyy.mm.dd</summary>
	MEMBER Ansi     := 2    

    // <summary>dd/mm/yy  dd/mm/yyyy</summary>
	MEMBER British  := 3    

    // <summary>dd/mm/yy  dd/mm/yyyy</summary>
	MEMBER French   := 4
    
    // <summary>dd.mm.yy  dd.mm.yyyy</summary>
	MEMBER German   := 5    

    // <summary>dd-mm-yy  dd-mm-yyyy</summary>
	MEMBER Italian  := 6    

    // <summary>dd-mm-yy  dd-mm-yyyy</summary>
    MEMBER Dutch    := 6    

    // <summary>yy/mm/dd  yyyy/mm/dd</summary>
	MEMBER Japanese := 7    

    // <summary>mm-dd-yy  mm-dd-yyyy </summary>
	MEMBER USA      := 8    

    // <summary>yy/mm/dd  yyyy/mm/dd</summary>
	MEMBER Taiwan   := 9

    // <summary>mm/dd/yy  mm/dd/yyyy</summary>
	MEMBER MDY      := 10

    // <summary>dd/mm/yy  dd/mm/yyyy</summary>
	MEMBER DMY      := 11

    // <summary>yy/mm/dd  yyyy/mm/dd</summary>
	MEMBER YMD      := 12
END ENUM
END NAMESPACE
#region defines
/// <include file="CoreComments.xml" path="Comments/DateCountry/*" />
DEFINE AMERICAN := DateCountry.American
/// <include file="CoreComments.xml" path="Comments/DateCountry/*" />
DEFINE ANSI     := DateCountry.Ansi
/// <include file="CoreComments.xml" path="Comments/DateCountry/*" />
DEFINE BRITISH  := DateCountry.British
/// <include file="CoreComments.xml" path="Comments/DateCountry/*" />
DEFINE FRENCH   := DateCountry.French
/// <include file="CoreComments.xml" path="Comments/DateCountry/*" />
DEFINE GERMAN   := DateCountry.German
/// <include file="CoreComments.xml" path="Comments/DateCountry/*" />
DEFINE ITALIAN  := DateCountry.Italian
/// <include file="CoreComments.xml" path="Comments/DateCountry/*" />
DEFINE DUTCH    := DateCountry.Dutch
/// <include file="CoreComments.xml" path="Comments/DateCountry/*" />
DEFINE JAPANESE := DateCountry.Japanese
/// <include file="CoreComments.xml" path="Comments/DateCountry/*" />
DEFINE USA      := DateCountry.USA  
/// <include file="CoreComments.xml" path="Comments/DateCountry/*" />
DEFINE DMY      := DateCountry.DMY
/// <include file="CoreComments.xml" path="Comments/DateCountry/*" />
DEFINE MDY      := DateCountry.MDY
/// <include file="CoreComments.xml" path="Comments/DateCountry/*" />
DEFINE YMD      := DateCountry.YMD
/// <include file="CoreComments.xml" path="Comments/DateCountry/*" />
DEFINE TAIWAN   := DateCountry.TAIWAN

#endregion
