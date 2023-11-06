USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING XUnit

BEGIN NAMESPACE XSharp.Core.Tests
CLASS CodeTests
    [Fact, Trait("Category", "String")];
    METHOD ChrAscTests() as void
        LOCAL i AS DWORD
        LOCAL cTemp AS STRING
        LOCAL nTemp AS DWORD
        setANSI(FALSE)
        FOR i := 1 UPTO 255
            cTemp := chr(i)
            nTemp := asc(cTemp)
            Assert.Equal(i, nTemp)
        NEXT

        setANSI(TRUE)
        FOR i := 1 UPTO 255
            cTemp := Chr(i)
            nTemp := Asc(cTemp)
            Assert.Equal(i, nTemp)
        NEXT

        FOR i := 1 UPTO 0xFFFF
            cTemp := ChrW(i)
            nTemp := AscW(cTemp)
            Assert.Equal(i, nTemp)
        NEXT
        FOR i := 1 UPTO 255
            cTemp := ChrA(i)
            nTemp := AscA(cTemp)
            Assert.Equal(i, nTemp)
        NEXT


END CLASS
END NAMESPACE
