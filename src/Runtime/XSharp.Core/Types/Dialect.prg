//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//



BEGIN NAMESPACE XSharp
    /// <summary>This enum lists the various dialects possible in XSharp. The enum MUST match the dialect values inside the compiler </summary>
    ENUM XSharpDialect
        MEMBER Core     := 0
        MEMBER VO       := 1
        MEMBER Vulcan   := 2
        MEMBER Harbour  := 3
        MEMBER FoxPro   := 4
        MEMBER XPP      := 5
        MEMBER dBase    := 6
        MEMBER Last     := 6
    END ENUM

    STATIC CLASS DialectExtension
        STATIC METHOD IsVoLike( SELF dialect as XSharpDialect) AS LOGIC
            SWITCH dialect
            CASE XSharpDialect.VO
            CASE XSharpDialect.Vulcan
                RETURN TRUE
            OTHERWISE
                RETURN FALSE
            END SWITCH
    END CLASS
END NAMESPACE


