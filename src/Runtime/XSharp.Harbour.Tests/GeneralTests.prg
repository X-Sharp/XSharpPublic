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
        Assert.True(AValues[1] == 2)
        Assert.True(aValues[2] == 4)
        Assert.True(Alen(aValues) == 4)
        Hb_ADel(aValues,1, TRUE)
        Assert.True(Alen(aValues) == 3)
        Assert.True(aValues[1] == 4)
        Assert.True(aValues[2] == NIL)
        Assert.True(aValues[3] == NIL)

    [Fact, Trait("Category", "Array")];
    METHOD HbAInsTests() as VOID
        local aValues as Array
        InitHarbour()
        // Insert at beginning without auto-size: array length stays the same, last element is lost
        aValues := {1,2,3,4}
        Hb_AIns(aValues, 1, 99)
        Assert.Equal(4, (int) Alen(aValues))
        Assert.True(aValues[1] == 99)
        Assert.True(aValues[2] == 1)
        Assert.True(aValues[3] == 2)
        Assert.True(aValues[4] == 3)

        // Insert at middle position without auto-size
        aValues := {10,20,30,40}
        Hb_AIns(aValues, 2, 15)
        Assert.Equal(4, (int) Alen(aValues))
        Assert.True(aValues[1] == 10)
        Assert.True(aValues[2] == 15)
        Assert.True(aValues[3] == 20)
        Assert.True(aValues[4] == 30)

        // Insert with auto-size: array grows by one
        aValues := {1,2,3}
        Hb_AIns(aValues, 1, 0, TRUE)
        Assert.Equal(4, (int) Alen(aValues))
        Assert.True(aValues[1] == 0)
        Assert.True(aValues[2] == 1)
        Assert.True(aValues[3] == 2)
        Assert.True(aValues[4] == 3)

        // Insert at end with auto-size
        aValues := {10,20,30}
        Hb_AIns(aValues, 3, 25, TRUE)
        Assert.Equal(4, (int) Alen(aValues))
        Assert.True(aValues[3] == 25)
        Assert.True(aValues[4] == 30)

        // Insert NIL value
        aValues := {"a","b","c"}
        Hb_AIns(aValues, 2, NIL, TRUE)
        Assert.Equal(4, (int) Alen(aValues))
        Assert.True(aValues[2] == NIL)
        Assert.True(aValues[3] == "b")

    [Fact, Trait("Category", "Array")];
    METHOD HbADelEdgeCaseTests() as VOID
        local aValues as Array
        InitHarbour()
        // Single element array - delete without auto-size: element becomes NIL
        aValues := {42}
        Hb_ADel(aValues, 1)
        Assert.Equal(1, (int) Alen(aValues))
        Assert.True(aValues[1] == NIL)

        // Single element array - delete with auto-size: array becomes empty
        aValues := {42}
        Hb_ADel(aValues, 1, TRUE)
        Assert.Equal(0, (int) Alen(aValues))

        // Delete from string array with auto-size
        aValues := {"alpha","beta","gamma"}
        Hb_ADel(aValues, 1, TRUE)
        // Implementation always deletes position 1, so "alpha" is removed
        Assert.Equal(2, (int) Alen(aValues))
        Assert.Equal("beta",  aValues[1])
        Assert.Equal("gamma", aValues[2])

        // Delete default position (first element)
        aValues := {10,20,30}
        Hb_ADel(aValues)
        Assert.Equal(3, (int) Alen(aValues))
        Assert.True(aValues[1] == 20)
        Assert.True(aValues[2] == 30)
        Assert.True(aValues[3] == NIL)


END CLASS
END NAMESPACE
