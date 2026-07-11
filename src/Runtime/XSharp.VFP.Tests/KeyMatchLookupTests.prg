//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System
USING System.Collections.Generic
USING System.Text
USING XUnit

BEGIN NAMESPACE XSharp.VFP.Tests

    CLASS KeyMatchLookupTests

        STATIC CONSTRUCTOR
            XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro
        END CONSTRUCTOR

        // --- KEYMATCH ---
        [Fact, Trait("Category", "KeyMatch")];
        METHOD KeyMatchExistingKeyReturnsTrue AS VOID
            CREATE CURSOR curTest (pCode C(10), pName C(50))
            INSERT INTO curTest VALUES ("A001", "Product 1")
            INSERT INTO curTest VALUES ("A002", "Product 2")
            INSERT INTO curTest VALUES ("A003", "Product 3")
            INDEX ON pCode TAG pCode

            Assert.True(KEYMATCH("A002"))
        END METHOD

        [Fact, Trait("Category", "KeyMatch")];
        METHOD KeyMatchNonExistingKeyReturnsFalse AS VOID
            CREATE CURSOR curTest (pCode C(10), pName C(50))
            INSERT INTO curTest VALUES ("A001", "Product 1")
            INSERT INTO curTest VALUES ("A002", "Product 2")
            INDEX ON pCode TAG pCode

            Assert.False(KEYMATCH("B999"))
        END METHOD

        [Fact, Trait("Category", "KeyMatch")];
        METHOD KeyMatchPreservesRecordPointer AS VOID
            CREATE CURSOR curTest (pCode C(10), pName C(50))
            INSERT INTO curTest VALUES ("A001", "Product 1")
            INSERT INTO curTest VALUES ("A002", "Product 2")
            INSERT INTO curTest VALUES ("A003", "Product 3")
            INDEX ON pCode TAG pCode

            GOTO 3
            VAR nRecBefore := RECNO()
            VAR lFound := KEYMATCH("A001")
            VAR nRecAfter := RECNO()

            Assert.True(lFound)
            Assert.Equal(nRecBefore, nRecAfter)
        END METHOD

        // --- LOOKUP ---
        [Fact, Trait("Category", "Lookup")];
        METHOD LookupFoundReturnsFieldValue AS VOID
            CREATE CURSOR curTest (pCode C(10), pName C(50))
            INSERT INTO curTest VALUES ("A001", "Product 1")
            INSERT INTO curTest VALUES ("A002", "Product 2")
            INSERT INTO curTest VALUES ("A003", "Product 3")
            INDEX ON pCode TAG pCode

            VAR cResult := LOOKUP("pName", "A002", "pCode", "pCode")
            Assert.Equal("Product 2", ALLTRIM(cResult))
        END METHOD

        [Fact, Trait("Category", "Lookup")];
        METHOD LookupNotFoundReturnsEmpty AS VOID
            CREATE CURSOR curTest (pCode C(10), pName C(50))
            INSERT INTO curTest VALUES ("A001", "Product 1")
            INDEX ON pCode TAG pCode

            VAR cResult := LOOKUP("pName", "B999", "pCode", "pCode")
            Assert.True(EMPTY(ALLTRIM(cResult)))
        END METHOD

        [Fact, Trait("Category", "Lookup")];
        METHOD LookupFoundMovesPointer AS VOID
            CREATE CURSOR curTest (pCode C(10), pName C(50))
            INSERT INTO curTest VALUES ("A001", "Product 1")
            INSERT INTO curTest VALUES ("A002", "Product 2")
            INDEX ON pCode TAG pCode

            GOTO 1
            LOOKUP("pName", "A002", "pCode", "pCode")
            Assert.Equal("A002", ALLTRIM(FIELDGET(FieldPos("pCode"))))
        END METHOD

        [Fact, Trait("Category", "Lookup")];
        METHOD LookupNotFoundEofTrue AS VOID
            CREATE CURSOR curTest (pCode C(10), pName C(50))
            INSERT INTO curTest VALUES ("A001", "Product 1")
            INDEX ON pCode TAG pCode

            LOOKUP("pName", "B999", "pCode", "pCode")
            Assert.True(EOF())
        END METHOD

    END CLASS

END NAMESPACE
