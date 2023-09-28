//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING XUnit
USING System.Globalization


BEGIN NAMESPACE XSharp.RT.Tests

CLASS TerminalTests

    [Fact, Trait("Category", "Terminal")];
    METHOD TextToFileTests() AS VOID
        var lConsole := SetConsole()
        var lAlte    := SetAlternate()
        SetConsole(FALSE)
        SetAlternate(FALSE)
        TEXT TO FILE TextToFileTests.Txt
        a
        b
        c
        ENDTEXT
        SetConsole(lConsole)
        SetAlternate(lAlte)
        var cText := MemoRead("TextToFileTests.txt")
        var nCount := MLCount(cText)
        Assert.Equal(nCount, 4U)
        Assert.Equal("a", MLine(cText,2):Trim())
        Assert.Equal("b", MLine(cText,3):Trim())
        Assert.Equal("c", MLine(cText,4):Trim())

END CLASS
END NAMESPACE

