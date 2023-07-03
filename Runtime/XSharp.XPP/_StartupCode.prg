//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

procedure InitCollation init3
    SetCollation(XppCollations.System)
    SetAnsi(true)       // This also sets the Set.CharSet
    return


procedure InitDateCountry init3
    SetDateCountry(DateCountry.System)
    return

