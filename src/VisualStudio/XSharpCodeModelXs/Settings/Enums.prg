//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

NAMESPACE XSharp.Settings
ENUM DebuggerMode
    MEMBER Design := 0
    MEMBER Break := 1
    MEMBER Running := 2
    MEMBER EditAndContinue := 0x10000000
    MEMBER EditAndContinueMask := ~0x10000000
END ENUM
ENUM KeywordCase
    MEMBER None := 0
    MEMBER Upper:= 1
    MEMBER Lower:= 2
    MEMBER Title:= 3
END ENUM
ENUM PublicStyle
    MEMBER Public := 0
    MEMBER Export := 1
    MEMBER None   := 2
END ENUM

ENUM PrivateStyle
    MEMBER Private := 0
    MEMBER Hidden  := 1
END ENUM
