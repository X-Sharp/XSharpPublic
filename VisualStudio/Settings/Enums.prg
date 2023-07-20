// Enums.prg
// Created by    : robert
// Creation Date : 7/4/2023 7:44:11 PM
// Created for   :
// WorkStation   : NYX


BEGIN NAMESPACE XSharp.Settings
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

END NAMESPACE
