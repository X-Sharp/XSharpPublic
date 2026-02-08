// AMembersTests.prg
// Created by    : irwin
// Creation Date : 1/9/2026 9:57:02 PM
// Created for   :
// WorkStation   : IRWINPC


USING System
USING System.Collections.Generic
USING System.Text
USING XUnit

BEGIN NAMESPACE XSharp.VFP.Tests
    // Guinea Pig class for testing
    CLASS TestObject
        PUBLIC MyField AS INT
        PROPERTY MyProp AS STRING AUTO
        EVENT MyEvent AS EventHandler
        METHOD MyMethod AS VOID
            NOP
        END METHOD
    END CLASS

    CLASS AMembersTests
        [Fact, Trait("Category", "AMembers")];
        METHOD BasicMembersTest AS VOID
            LOCAL o AS TestObject
            o := TestObject{}

            LOCAL ARRAY aInfo(1)
            VAR nCount := AMembers(aInfo, o, 0) // 1D array

            FOR EACH VAR cMember IN aInfo
                ? cMember
            NEXT

            Assert.True(nCount > 0)
            Assert.Equal(nCount, Alen(aInfo, 1))

            Assert.True(AScan(aInfo, "MYFIELD") > 0)
            Assert.True(AScan(aInfo, "MYPROP") > 0)
            Assert.True(AScan(aInfo, "MYMETHOD") > 0)
            Assert.True(AScan(aInfo, "MYEVENT") > 0)

            Assert.True(AScan(aInfo, "GET_MYPROP") == 0)
        END METHOD

        [Fact, Trait("Category", "AMembers")];
        METHOD DetailedMembersTest AS VOID
            LOCAL o AS TestObject
            o := TestObject{}
            LOCAL ARRAY aInfo(1)

            // 2D Array (Name and Type)
            VAR nCount := AMembers(aInfo, o, 1)

            Assert.True(nCount > 0)
            Assert.Equal(2, (INT)Alen(aInfo, 2))

            // Confirm method
            VAR nIndex := AScan(aInfo, "MYMETHOD")
            Assert.True(nIndex > 0)
            VAR nRow := ASubscript(aInfo, nIndex)
            Assert.Equal("Method", aInfo[nRow, 2])

            // Confirm property
            nIndex := AScan(aInfo, "MYPROP")
            Assert.True(nIndex > 0)
            nRow := ASubscript(aInfo, nIndex)
            Assert.Equal("Property", aInfo[nRow, 2])

            // Confirm event
            nIndex := AScan(aInfo, "MYEVENT")
            Assert.True(nIndex > 0)
            nRow := ASubscript(aInfo, nIndex)
            Assert.Equal("Event", aInfo[nRow, 2])
        END METHOD
    END CLASS
END NAMESPACE // XSharp.VFP.Tests
