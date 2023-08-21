USING XUnit
USING System
USING System.Collections.Generic
USING System.Text

BEGIN NAMESPACE XSharp.Harbour.Tests

CLASS GeneralTests


    [Fact, Trait("Category", "Settings")];
    METHOD SettingsTests() as VOID
        LOCAL nDecimals as DWORD
        InitHarbour()
        nDecimals := SetDecimal()
        Assert.Equal(0U, nDecimals) //
        local nNum as FLOAT
        nNum := 42.123
        Assert.Equal("        42", Str(nNum))
        Assert.Equal(10U, SLen(Str(nNum)))

    [Fact, Trait("Category", "Array")];
    METHOD ArrayTests() as VOID
        local aValues as Array
        InitHarbour()
        aValues := {1,2,3,4}
        Hb_ADel(aValues)
        Assert.True(Alen(aValues) == 4)
        Assert.True(AValues[1] == 2)
        Assert.True(aValues[4] == NIL)
        Hb_ADel(aValues,2)
        Assert.True(AValues[1] == 3)
        Assert.True(aValues[2] == 4)
        Assert.True(Alen(aValues) == 4)
        Hb_ADel(aValues,1, TRUE)
        Assert.True(Alen(aValues) == 3)
        Assert.True(aValues[1] == 4)
        Assert.True(aValues[2] == NIL)
        Assert.True(aValues[3] == NIL)





END CLASS
END NAMESPACE
