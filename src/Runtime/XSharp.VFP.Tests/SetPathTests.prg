//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
// SET PATH TO ... ADDITIVE and a path with several folders. The expected values
// were measured on VFP 9 SP2, except for the case of the letters: VFP returns
// the path in upper case and X# keeps it as it was written.

USING System
USING System.IO
USING XUnit

BEGIN NAMESPACE XSharp.VFP.Tests
    CLASS SetPathTests

        STATIC CONSTRUCTOR
            XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro
            RegisterFoxMemVarSupport()
        END CONSTRUCTOR

        [Fact, Trait("Category", "SetPath")];
        METHOD AdditiveAddsTheFolder AS VOID
            LOCAL cOld, cA, cB AS STRING
            cOld := SELF:SavePath()
            TRY
                cA := "c:\temp\gmA"
                cB := "c:\temp\gmB"
                SET PATH TO (cA)
                SET PATH TO (cB) ADDITIVE
                Assert.Equal("c:\temp\gmA;c:\temp\gmB", SetPath())
                // an empty path has nothing to separate
                SET PATH TO
                SET PATH TO (cA) ADDITIVE
                Assert.Equal("c:\temp\gmA", SetPath())
                // and an empty string adds nothing
                SET PATH TO ("") ADDITIVE
                Assert.Equal("c:\temp\gmA", SetPath())
            FINALLY
                SetPath(cOld)
            END TRY
        END METHOD

        [Fact, Trait("Category", "SetPath")];
        METHOD AdditiveSkipsAFolderOfThePath AS VOID
            LOCAL cOld, cA, cB AS STRING
            cOld := SELF:SavePath()
            TRY
                cA := "c:\temp\gmA"
                cB := "c:\temp\gmB"
                SET PATH TO (cA)
                SET PATH TO (cB) ADDITIVE
                SET PATH TO (cB) ADDITIVE
                SET PATH TO ("C:\TEMP\GMB") ADDITIVE
                SET PATH TO ("c:\temp\gmB\") ADDITIVE
                SET PATH TO (" c:\temp\gmA") ADDITIVE
                SET PATH TO ("c:\temp\gmA\..\gmA") ADDITIVE
                Assert.Equal("c:\temp\gmA;c:\temp\gmB", SetPath())
                // also when the path has commas and the folder is in the middle
                SET PATH TO ("c:\temp\gmA,c:\temp\gmB,c:\temp\gmC")
                SET PATH TO (cB) ADDITIVE
                Assert.Equal("c:\temp\gmA,c:\temp\gmB,c:\temp\gmC", SetPath())
            FINALLY
                SetPath(cOld)
            END TRY
        END METHOD

        [Fact, Trait("Category", "SetPath")];
        METHOD AdditiveWithAListOfFolders AS VOID
            LOCAL cOld AS STRING
            cOld := SELF:SavePath()
            TRY
                // the folders are added one by one, the ones of the path are skipped
                SET PATH TO ("c:\temp\gmA;c:\temp\gmB")
                SET PATH TO ("c:\temp\gmC,c:\temp\gmA;c:\temp\gmD") ADDITIVE
                Assert.Equal("c:\temp\gmA;c:\temp\gmB;c:\temp\gmC;c:\temp\gmD", SetPath())
                // empty elements are dropped
                SET PATH TO ("c:\temp\gmA")
                SET PATH TO ("c:\temp\gmB,,;c:\temp\gm C") ADDITIVE
                Assert.Equal("c:\temp\gmA;c:\temp\gmB;c:\temp\gm C", SetPath())
                // a folder that comes twice in the new list is added twice
                SET PATH TO ("c:\temp\gmA")
                SET PATH TO ("c:\temp\gmB;c:\temp\gmB") ADDITIVE
                Assert.Equal("c:\temp\gmA;c:\temp\gmB;c:\temp\gmB", SetPath())
                // the blanks of the new folder stay
                SET PATH TO ("c:\temp\gmA")
                SET PATH TO ("  c:\temp\gmB  ") ADDITIVE
                Assert.Equal("c:\temp\gmA;  c:\temp\gmB  ", SetPath())
            FINALLY
                SetPath(cOld)
            END TRY
        END METHOD

        [Fact, Trait("Category", "SetPath")];
        METHOD AdditiveKeepsThePathThatWasThere AS VOID
            LOCAL cOld, cB AS STRING
            cOld := SELF:SavePath()
            TRY
                cB := "c:\temp\gmB"
                SET PATH TO ("c:\temp\gmA,c:\temp\gmC")
                SET PATH TO (cB) ADDITIVE
                Assert.Equal("c:\temp\gmA,c:\temp\gmC;c:\temp\gmB", SetPath())
                // no second separator when the path ends with one
                SET PATH TO ("c:\temp\gmA;")
                SET PATH TO (cB) ADDITIVE
                Assert.Equal("c:\temp\gmA;c:\temp\gmB", SetPath())
                SET PATH TO ("c:\temp\gmA,")
                SET PATH TO (cB) ADDITIVE
                Assert.Equal("c:\temp\gmA,c:\temp\gmB", SetPath())
                SET PATH TO ("c:\temp\gmA ")
                SET PATH TO (cB) ADDITIVE
                Assert.Equal("c:\temp\gmA ;c:\temp\gmB", SetPath())
            FINALLY
                SetPath(cOld)
            END TRY
        END METHOD

        [Fact, Trait("Category", "SetPath")];
        METHOD AdditiveAfterAnExpression AS VOID
            LOCAL cOld, cA, cB AS STRING
            cOld := SELF:SavePath()
            TRY
                cA := "c:\temp\gmA"
                cB := "c:\temp\gmB"
                SET PATH TO (cA)
                SET PATH TO "c:\temp\gm C" ADDITIVE
                Assert.Equal("c:\temp\gmA;c:\temp\gm C", SetPath())
                SET PATH TO (cA)
                SET PATH TO [c:\temp\gm C] ADDITIVE
                Assert.Equal("c:\temp\gmA;c:\temp\gm C", SetPath())
                SET PATH TO (cA)
                SET PATH TO (cB) + "\sub" ADDITIVE
                Assert.Equal("c:\temp\gmA;c:\temp\gmB\sub", SetPath())
                SET PATH TO (cA)
                SET PATH TO "c:\temp\gmB" + "\sub" ADDITIVE
                Assert.Equal("c:\temp\gmA;c:\temp\gmB\sub", SetPath())
                SET PATH TO (cA)
                SET PATH TO JustPath(cB) ADDITIVE
                Assert.Equal("c:\temp\gmA;c:\temp", SetPath())
                SET PATH TO (cA)
                SET PATH TO (cB) additive
                Assert.Equal("c:\temp\gmA;c:\temp\gmB", SetPath())
                SET PATH TO (cA)
                SET PATH TO (cB) ADDI
                Assert.Equal("c:\temp\gmA;c:\temp\gmB", SetPath())
            FINALLY
                SetPath(cOld)
            END TRY
        END METHOD

        [Fact, Trait("Category", "SetPath")];
        METHOD AdditiveAfterANameIsPartOfThePath AS VOID
            // VFP does the same: without quotes or parentheses the word is not a clause
            LOCAL cOld, cA AS STRING
            PRIVATE cMacro
            cOld := SELF:SavePath()
            TRY
                cA := "c:\temp\gmA"
                SET PATH TO (cA)
                SET PATH TO data ADDITIVE
                Assert.Equal("data ADDITIVE", SetPath())
                SET PATH TO (cA)
                SET PATH TO c:\temp\gmB ADDITIVE
                Assert.Equal("c:\temp\gmB ADDITIVE", SetPath())
                // not like VFP: with a macro VFP also takes the word as part of
                // the path, here the folder is added
                cMacro := "c:\temp\gmM"
                SET PATH TO (cA)
                SET PATH TO &cMacro ADDITIVE
                Assert.Equal("c:\temp\gmA;c:\temp\gmM", SetPath())
            FINALLY
                SetPath(cOld)
            END TRY
        END METHOD

        [Fact, Trait("Category", "SetPath")];
        METHOD AdditiveTakesTheQuotesOfTheValueAway AS VOID
            // the help of VFP tells to write the macro with the quotes inside
            LOCAL cOld, cA, cQuoted AS STRING
            PRIVATE cMacro
            cOld := SELF:SavePath()
            TRY
                cA := "c:\temp\gmA"
                cMacro := '"c:\temp\gm B"'
                SET PATH TO (cA)
                SET PATH TO &cMacro ADDITIVE
                Assert.Equal("c:\temp\gmA;c:\temp\gm B", SetPath())
                cMacro := "[c:\temp\gmB]"
                SET PATH TO (cA)
                SET PATH TO &cMacro ADDITIVE
                Assert.Equal("c:\temp\gmA;c:\temp\gmB", SetPath())
                cQuoted := "'c:\temp\gmB'"
                SET PATH TO (cA)
                SET PATH TO (cQuoted) ADDITIVE
                Assert.Equal("c:\temp\gmA;c:\temp\gmB", SetPath())
            FINALLY
                SetPath(cOld)
            END TRY
        END METHOD

        [Fact, Trait("Category", "SetPath")];
        METHOD AdditiveNeedsAString AS VOID
            LOCAL cOld AS STRING
            LOCAL lError := FALSE AS LOGIC
            cOld := SELF:SavePath()
            TRY
                SET PATH TO ("c:\temp\gmA")
                TRY
                    SET PATH TO (123) ADDITIVE
                CATCH
                    lError := TRUE
                END TRY
                Assert.True(lError)
                Assert.Equal("c:\temp\gmA", SetPath())
            FINALLY
                SetPath(cOld)
            END TRY
        END METHOD

        [Fact, Trait("Category", "SetPath")];
        METHOD FoldersOfThePathAreSearched AS VOID
            LOCAL cOld, cRoot, cA, cB, cC AS STRING
            cOld := SELF:SavePath()
            cRoot := Path.Combine(Path.GetTempPath(), "VfpSetPath_" + Guid.NewGuid():ToString("N"))
            cA := Path.Combine(cRoot, "a")
            cB := Path.Combine(cRoot, "b")
            cC := Path.Combine(cRoot, "c d")
            TRY
                XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro
                System.IO.Directory.CreateDirectory(cA)
                System.IO.Directory.CreateDirectory(cB)
                System.IO.Directory.CreateDirectory(cC)
                System.IO.File.WriteAllText(Path.Combine(cB, "xs_setpath_b.txt"), "x")
                System.IO.File.WriteAllText(Path.Combine(cC, "xs_setpath_c.txt"), "x")

                SET PATH TO (cA)
                Assert.False(File("xs_setpath_b.txt"))
                SET PATH TO (cB) ADDITIVE
                Assert.True(File("xs_setpath_b.txt"))
                Assert.False(File("xs_setpath_c.txt"))
                SET PATH TO (cC) ADDITIVE
                Assert.True(File("xs_setpath_c.txt"))
            FINALLY
                SetPath(cOld)
                IF System.IO.Directory.Exists(cRoot)
                    System.IO.Directory.Delete(cRoot, TRUE)
                ENDIF
            END TRY
        END METHOD

        [Fact, Trait("Category", "SetPath")];
        METHOD FoldersWithACommaAreSearched AS VOID
            // a comma between the folders, and blanks around them
            LOCAL cOld, cRoot, cA, cB, cC AS STRING
            cOld := SELF:SavePath()
            cRoot := Path.Combine(Path.GetTempPath(), "VfpSetPath_" + Guid.NewGuid():ToString("N"))
            cA := Path.Combine(cRoot, "a")
            cB := Path.Combine(cRoot, "b")
            cC := Path.Combine(cRoot, "c d")
            TRY
                XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro
                System.IO.Directory.CreateDirectory(cA)
                System.IO.Directory.CreateDirectory(cB)
                System.IO.Directory.CreateDirectory(cC)
                System.IO.File.WriteAllText(Path.Combine(cB, "xs_setpath_b.txt"), "x")
                System.IO.File.WriteAllText(Path.Combine(cC, "xs_setpath_c.txt"), "x")

                SET PATH TO (cA + "," + cB)
                Assert.True(File("xs_setpath_b.txt"))
                SET PATH TO (cA + ", " + cB)
                Assert.True(File("xs_setpath_b.txt"))
                SET PATH TO ("  " + cA + "  ;  " + cC + "  ")
                Assert.True(File("xs_setpath_c.txt"))
                Assert.False(File("xs_setpath_b.txt"))
            FINALLY
                SetPath(cOld)
                IF System.IO.Directory.Exists(cRoot)
                    System.IO.Directory.Delete(cRoot, TRUE)
                ENDIF
            END TRY
        END METHOD

        PRIVATE METHOD SavePath() AS STRING
            LOCAL cPath AS STRING
            cPath := SetPath()
            IF cPath == NULL
                cPath := ""
            ENDIF
            RETURN cPath
        END METHOD

    END CLASS
END NAMESPACE
