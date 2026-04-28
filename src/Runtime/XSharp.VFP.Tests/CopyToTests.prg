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


// Array tests are not working correctly yet with the current build
BEGIN NAMESPACE XSharp.VFP.Tests

	CLASS CopyToTests
	    STATIC CONSTRUCTOR
            XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro

        [Fact, Trait("Category", "CopyTo")];
		METHOD CopyToTests1() AS VOID
            LOCAL aStruct AS ARRAY
            aStruct := { {"F1","N",10,0},{"F2","N",10,0},{"F3","N",10,0}}
            DbCreate("test.dbf", aStruct)
            DbUseArea(TRUE, NIL, "TEST")
            FOR VAR i := 1 TO 3
                DbAppend()
                FOR VAR nFld := 1 TO Alen(aStruct)
                    FieldPut(nFld, nFld * i)
                NEXT
            NEXT
            PUBLIC aValues(3,3)
            DBGoTop()
            DbCopyToArray(aValues)
            Assert.True(ALen(aValues,1) == 3)
            Assert.True(ALen(aValues,2) == 3)
            Assert.True(ALen(aValues) == 9)
            Assert.True(aValues[1,1] == 1)
            Assert.True(aValues[1,2] == 2)
            Assert.True(aValues[1,3] == 3)
            Assert.True(aValues[2,1] == 2)
            Assert.True(aValues[2,2] == 4)
            Assert.True(aValues[2,3] == 6)
            Assert.True(aValues[3,1] == 3)
            Assert.True(aValues[3,2] == 6)
            Assert.True(aValues[3,3] == 9)

            PUBLIC aValues(4,4)
            DBGoTop()
            DbCopyToArray(aValues)
            Assert.True(ALen(aValues,1) == 4)
            Assert.True(ALen(aValues,2) == 4)
            Assert.True(ALen(aValues) == 16)
            Assert.True(aValues[1,1] == 1)
            Assert.True(aValues[1,2] == 2)
            Assert.True(aValues[1,3] == 3)
            Assert.True(aValues[1,4] == NIL)
            Assert.True(aValues[2,1] == 2)
            Assert.True(aValues[2,2] == 4)
            Assert.True(aValues[2,3] == 6)
            Assert.True(aValues[2,4] == NIL)
            Assert.True(aValues[3,1] == 3)
            Assert.True(aValues[3,2] == 6)
            Assert.True(aValues[3,3] == 9)
            Assert.True(aValues[3,4] == NIL)
            Assert.True(aValues[4,1] == NIL)
            Assert.True(aValues[4,2] == NIL)
            Assert.True(aValues[4,3] == NIL)
            Assert.True(aValues[4,4] == NIL)
            PUBLIC aValues(2,2)
            DBGoTop()
            DbCopyToArray(aValues)
            Assert.True(ALen(aValues,1) == 2)
            Assert.True(ALen(aValues,2) == 2)
            Assert.True(ALen(aValues) == 4)
            Assert.True(aValues[1,1] == 1)
            Assert.True(aValues[1,2] == 2)
            Assert.True(aValues[2,1] == 2)
            Assert.True(aValues[2,2] == 4)
            DbCloseArea()

        [Fact, Trait("Category", "CopyTo")];
        METHOD CopyToArrayTest() AS VOID
            LOCAL aStruct AS ARRAY

            aStruct := {{"ID", "N", 4, 0}, {"NAME", "C", 10, 0}}
            DbCreate("test.dbf", aStruct)
            DbUseArea(TRUE, NIL, "TEST")

            DbAppend()
            FieldPut(1, 100)
            FieldPut(2, "John")

            DbAppend()
            FieldPut(1, 200)
            FieldPut(2, "Peter")

            PUBLIC aDest(2,2)
            aDest := DbCopyToArray(aDest)
            DBGoTop()

            Assert.True(IsArray(aDest), "Target should be an array")
            Assert.True(aDest IS __FoxArray, "Target should be a FOX array")

            Assert.Equal(2, (INT)ALen(aDest, 1)) // 2 Rows
            Assert.Equal(2, (INT)ALen(aDest, 2)) // 2 Columns

            Assert.Equal(100, (INT) aDest[1, 1])
            Assert.Equal("John      ", (STRING) aDest[1, 2])

            Assert.Equal(200, (INT) aDest[2, 1])
            Assert.Equal("Peter     ", (STRING) aDest[2, 2])

            DbCloseArea()
        END METHOD
    END CLASS
END NAMESPACE
