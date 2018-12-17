//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING XSharp
BEGIN NAMESPACE XSharp
/// <summary>Values that match the Visual Objects DateCountry defines </summary>
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
DEFINE AMERICAN := DateCountry.American
DEFINE ANSI     := DateCountry.Ansi
DEFINE BRITISH  := DateCountry.British
DEFINE FRENCH   := DateCountry.French
DEFINE GERMAN   := DateCountry.German
DEFINE ITALIAN  := DateCountry.Italian
DEFINE DUTCH    := DateCountry.Dutch
DEFINE JAPANESE := DateCountry.Japanese
DEFINE USA      := DateCountry.USA  
#endregion
