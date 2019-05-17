//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING XSharp
BEGIN NAMESPACE XSharp
/// <summary>Values that match the XBase DateCountry defines </summary>
/// <remarks>Please note that although there are different values for British and French, the date formats are the same.</remarks>
PUBLIC ENUM DateCountry
	MEMBER American := 1    // mm/dd/yy  mm/dd/yyyy
	MEMBER Ansi     := 2    // yy.mm.dd  yyyy.mm.dd
	MEMBER British  := 3    // dd/mm/yy  dd/mm/yyyy
	MEMBER French   := 4    // dd/mm/yy  dd/mm/yyyy
	MEMBER German   := 5    // dd.mm.yy  dd.mm.yyyy
	MEMBER Italian  := 6    // dd-mm-yy  dd-mm-yyyy
    MEMBER Dutch    := 6    // dd-mm-yy  dd-mm-yyyy
	MEMBER Japanese := 7    // yy/mm/dd  yyyy/mm/dd
	MEMBER USA      := 8    // mm-dd-yy  mm-dd-yyyy 
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
#endregion
