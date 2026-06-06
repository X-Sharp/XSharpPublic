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

        [Fact, Trait("Category", "CopyTo")];
        METHOD CopyToSdfWithDatesTest() AS VOID
            LOCAL aStruct AS ARRAY
            LOCAL cContent AS STRING

            aStruct := {{"F1","N",4,0}, {"F2","D",8,0}, {"F3","C",10,0}}
            DbCreate("test_sdf.dbf", aStruct)
            DbUseArea(TRUE, NIL, "TEST_SDF")

            DbAppend()
            FieldPut(1, 1)
            FieldPut(2, ConDate(2024, 6, 15))
            FieldPut(3, "Alice")

            DbAppend()
            FieldPut(1, 2)
            FieldPut(2, NULL_DATE)
            FieldPut(3, "Bob")

            DbAppend()
            FieldPut(1, 3)
            FieldPut(2, ConDate(2024, 12, 25))
            FieldPut(3, "Charlie")

            COPY TO "test_sdf.txt" TYPE SDF

            Assert.True(File("test_sdf.txt"), "SDF file should be created")

            cContent := FileToStr("test_sdf.txt")
            Assert.True(At("20240615", cContent) > 0, "Valid date YYYYMMDD")
            Assert.True(At("20241225", cContent) > 0, "Valid date YYYYMMDD")
            Assert.True(At("Bob", cContent) > 0, "Record with empty date")

            DbCloseArea()
            FErase("test_sdf.txt")
            FErase("test_sdf.dbf")
        END METHOD

        [Fact, Trait("Category", "CopyTo")];
        METHOD CopyToCsvWithDatesTest() AS VOID
            LOCAL aStruct AS ARRAY
            LOCAL cContent AS STRING

            aStruct := {{"F1","N",4,0}, {"F2","D",8,0}, {"F3","C",10,0}}
            DbCreate("test_csv.dbf", aStruct)
            DbUseArea(TRUE, NIL, "TEST_CSV")

            DbAppend()
            FieldPut(1, 1)
            FieldPut(2, ConDate(2024, 6, 15))
            FieldPut(3, "Alice")

            DbAppend()
            FieldPut(1, 2)
            FieldPut(2, NULL_DATE)
            FieldPut(3, "Bob")

            DbAppend()
            FieldPut(1, 3)
            FieldPut(2, ConDate(2024, 12, 25))
            FieldPut(3, "Charlie")

            COPY TO "test_csv" TYPE CSV

            Assert.True(File("test_csv.csv"), "CSV file should be created")

            cContent := FileToStr("test_csv.csv")
            Assert.True(At("20240615", cContent) > 0, "Valid date YYYYMMDD")
            Assert.True(At("20241225", cContent) > 0, "Valid date YYYYMMDD")
            Assert.True(At("Alice", cContent) > 0, "Character data should be present")
            Assert.True(At("Bob", cContent) > 0, "Record with empty date should be present")

            DbCloseArea()
            FErase("test_csv.csv")
            FErase("test_csv.dbf")
    END METHOD
    END CLASS
END NAMESPACE
