//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

PROCEDURE InitCollation INIT3
    SetCollation(XppCollations.System)
    SET(Set.CharSet, CHARSET_ANSI)
    RETURN


PROCEDURE InitDateCountry INIT3
   SetDateCountry(DateCountry.System)
    RETURN
