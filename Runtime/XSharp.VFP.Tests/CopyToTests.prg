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
    END CLASS
END NAMESPACE
