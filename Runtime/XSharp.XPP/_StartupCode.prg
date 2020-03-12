//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

PROCEDURE InitCollation INIT3
    SetCollation(XppCollations.System)
    SetAnsi(TRUE)       // This also sets the Set.CharSet
    RETURN


PROCEDURE InitDateCountry INIT3
   SetDateCountry(DateCountry.System)
    RETURN
