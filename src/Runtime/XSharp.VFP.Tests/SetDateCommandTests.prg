//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING XUnit

BEGIN NAMESPACE XSharp.VFP.Tests
    CLASS SetDateCommandTests

        STATIC CONSTRUCTOR
            XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro
        END CONSTRUCTOR

        [Fact, Trait("Category", "SetDate")];
        METHOD SetDateAcceptsAParenthesizedVariable AS VOID
            LOCAL cFmt AS STRING
            VAR cOld := GetDateFormat()
            TRY
                SET CENTURY ON
                cFmt := "AMERICAN"
                SET DATE TO GERMAN
                SET DATE TO (cFmt)
                Assert.Equal("MM/DD/YYYY", GetDateFormat())
            FINALLY
                SetDateFormat(cOld)
            END TRY
        END METHOD

        [Fact, Trait("Category", "SetDate")];
        METHOD SetDateAcceptsAFunctionCall AS VOID
            LOCAL cFmt AS STRING
            VAR cOld := GetDateFormat()
            TRY
                SET CENTURY ON
                cFmt := "AMERICAN  "
                SET DATE TO GERMAN
                SET DATE TO AllTrim(cFmt)
                Assert.Equal("MM/DD/YYYY", GetDateFormat())
            FINALLY
                SetDateFormat(cOld)
            END TRY
        END METHOD

        [Fact, Trait("Category", "SetDate")];
        METHOD SetDateAcceptsACursorField AS VOID
            VAR cOld := GetDateFormat()
            TRY
                SET CENTURY ON
                CREATE CURSOR csrdate (cdatef C(10))
                INSERT INTO csrdate VALUES ("BRITISH   ")
                SET DATE TO GERMAN
                SET DATE TO AllTrim(csrdate->cdatef)
                Assert.Equal("DD/MM/YYYY", GetDateFormat())
            FINALLY
                SetDateFormat(cOld)
            END TRY
        END METHOD

        [Fact, Trait("Category", "SetDate")];
        METHOD SetDateAcceptsAMacro AS VOID
            LOCAL cFmt AS STRING
            VAR cOld := GetDateFormat()
            TRY
                SET CENTURY ON
                cFmt := "ITALIAN"
                SET DATE TO GERMAN
                SET DATE TO &cFmt
                Assert.Equal("DD-MM-YYYY", GetDateFormat())
            FINALLY
                SetDateFormat(cOld)
            END TRY
        END METHOD

        [Fact, Trait("Category", "SetDate")];
        METHOD SetDateAcceptsEveryKeywordAtRuntime AS VOID
            LOCAL cFmt AS STRING
            VAR cOld := GetDateFormat()
            TRY
                SET CENTURY ON
                FOREACH oPair AS STRING[] IN <STRING[]>{ ;
                        <STRING>{"AMERICAN", "MM/DD/YYYY"}, ;
                        <STRING>{"ANSI"    , "YYYY.MM.DD"}, ;
                        <STRING>{"BRITISH" , "DD/MM/YYYY"}, ;
                        <STRING>{"FRENCH"  , "DD/MM/YYYY"}, ;
                        <STRING>{"GERMAN"  , "DD.MM.YYYY"}, ;
                        <STRING>{"ITALIAN" , "DD-MM-YYYY"}, ;
                        <STRING>{"JAPAN"   , "YYYY/MM/DD"}, ;
                        <STRING>{"USA"     , "MM-DD-YYYY"}, ;
                        <STRING>{"MDY"     , "MM/DD/YYYY"}, ;
                        <STRING>{"DMY"     , "DD/MM/YYYY"}, ;
                        <STRING>{"YMD"     , "YYYY/MM/DD"}  ;
                    }
                    cFmt := oPair[1]
                    SET DATE TO GERMAN
                    SET DATE TO (cFmt)
                    Assert.Equal(oPair[2], GetDateFormat())
                NEXT
            FINALLY
                SetDateFormat(cOld)
            END TRY
        END METHOD

        [Fact, Trait("Category", "SetDate")];
        METHOD SetDateLiteralKeywordsStillWork AS VOID
            VAR cOld := GetDateFormat()
            TRY
                SET CENTURY ON
                SET DATE TO AMERICAN
                Assert.Equal("MM/DD/YYYY", GetDateFormat())
                SET DATE TO GERMAN
                Assert.Equal("DD.MM.YYYY", GetDateFormat())
                SET DATE TO ANSI
                Assert.Equal("YYYY.MM.DD", GetDateFormat())
                SET DATE TO ITALIAN
                Assert.Equal("DD-MM-YYYY", GetDateFormat())
                SET DATE TO JAPANESE
                Assert.Equal("YYYY/MM/DD", GetDateFormat())
                SET DATE TO USA
                Assert.Equal("MM-DD-YYYY", GetDateFormat())
            FINALLY
                SetDateFormat(cOld)
            END TRY
        END METHOD

        [Fact, Trait("Category", "SetDate")];
        METHOD SetDateFormatCommandStillWorks AS VOID
            VAR cOld := GetDateFormat()
            TRY
                SET DATE FORMAT TO "MM/DD/YY"
                Assert.Equal("MM/DD/YY", GetDateFormat())
            FINALLY
                SetDateFormat(cOld)
            END TRY
        END METHOD

        [Fact, Trait("Category", "SetDate")];
        METHOD SetDateAcceptsADatePictureSoTheSettingCanBeRestored AS VOID
            LOCAL cSaved AS STRING
            VAR cOld := GetDateFormat()
            TRY
                SET CENTURY ON
                SET DATE TO GERMAN
                cSaved := GetDateFormat()
                SET DATE TO AMERICAN
                SET DATE TO (cSaved)
                Assert.Equal("DD.MM.YYYY", GetDateFormat())
            FINALLY
                SetDateFormat(cOld)
            END TRY
        END METHOD

        [Fact, Trait("Category", "SetDate")];
        METHOD SetDateRejectsAnUnknownValue AS VOID
            LOCAL lThrown := FALSE AS LOGIC
            LOCAL cFmt AS STRING
            VAR cOld := GetDateFormat()
            TRY
                SET DATE TO GERMAN
                cFmt := "NOTAFORMAT"
                TRY
                    SET DATE TO (cFmt)
                CATCH AS Exception
                    lThrown := TRUE
                END TRY
                Assert.True(lThrown)
                Assert.Equal("DD.MM.YY", Left(GetDateFormat(), 8))
            FINALLY
                SetDateFormat(cOld)
            END TRY
        END METHOD

        [Fact, Trait("Category", "SetDate")];
        METHOD SetDateAcceptsAValueWithTrailingSpaces AS VOID
            LOCAL cFmt AS STRING
            VAR cOld := GetDateFormat()
            TRY
                SET CENTURY ON
                cFmt := "AMERICAN  "
                SET DATE TO GERMAN
                SET DATE TO (cFmt)
                Assert.Equal("MM/DD/YYYY", GetDateFormat())
            FINALLY
                SetDateFormat(cOld)
            END TRY
        END METHOD

        [Fact, Trait("Category", "SetDate")];
        METHOD SetDateQueryReturnsTheCountryKeywordNotThePicture AS VOID
            VAR cOld := GetDateFormat()
            TRY
                SET CENTURY ON
                SET DATE TO GERMAN
                Assert.Equal("GERMAN", (STRING) Set("DATE"))
                Assert.Equal("DD.MM.YYYY", (STRING) Set("DATEFORMAT"))
            FINALLY
                SetDateFormat(cOld)
            END TRY
        END METHOD

        [Fact, Trait("Category", "SetDate")];
        METHOD SetDateQueryReturnsEveryKeyword AS VOID
            LOCAL cKw AS STRING
            VAR cOld := GetDateFormat()
            TRY
                SET CENTURY ON
                FOREACH oPair AS STRING[] IN <STRING[]>{ ;
                        <STRING>{"AMERICAN", "AMERICAN"}, ;
                        <STRING>{"ANSI"    , "ANSI"    }, ;
                        <STRING>{"BRITISH" , "BRITISH" }, ;
                        <STRING>{"FRENCH"  , "FRENCH"  }, ;
                        <STRING>{"GERMAN"  , "GERMAN"  }, ;
                        <STRING>{"ITALIAN" , "ITALIAN" }, ;
                        <STRING>{"DUTCH"   , "ITALIAN" }, ;
                        <STRING>{"JAPAN"   , "JAPAN"   }, ;
                        <STRING>{"JAPANESE", "JAPAN"   }, ;
                        <STRING>{"USA"     , "USA"     }, ;
                        <STRING>{"SHORT"   , "SHORT"   }  ;
                    }
                    cKw := oPair[1]
                    SET DATE TO (cKw)
                    Assert.Equal(oPair[2], (STRING) Set("DATE"))
                NEXT
            FINALLY
                SetDateFormat(cOld)
            END TRY
        END METHOD

        [Fact, Trait("Category", "SetDate")];
        METHOD SetDateQueryCollapsesAliasKeywords_KnownLimitation AS VOID
            LOCAL cKw AS STRING
            VAR cOld := GetDateFormat()
            TRY
                SET CENTURY ON
                FOREACH oPair AS STRING[] IN <STRING[]>{ ;
                        <STRING>{"TAIWAN", "JAPAN"   }, ;
                        <STRING>{"MDY"   , "AMERICAN"}, ;
                        <STRING>{"DMY"   , "BRITISH" }, ;
                        <STRING>{"YMD"   , "JAPAN"   }  ;
                    }
                    cKw := oPair[1]
                    SET DATE TO (cKw)
                    Assert.Equal(oPair[2], (STRING) Set("DATE"))
                NEXT
            FINALLY
                SetDateFormat(cOld)
            END TRY
        END METHOD

        [Fact, Trait("Category", "SetDate")];
        METHOD SetDateQueryWorksForLiteralKeywordsToo AS VOID
            VAR cOld := GetDateFormat()
            TRY
                SET CENTURY ON
                SET DATE TO AMERICAN
                Assert.Equal("AMERICAN", (STRING) Set("DATE"))
                SET DATE TO ANSI
                Assert.Equal("ANSI", (STRING) Set("DATE"))
                SET DATE TO FRENCH
                Assert.Equal("FRENCH", (STRING) Set("DATE"))
                SET DATE TO ITALIAN
                Assert.Equal("ITALIAN", (STRING) Set("DATE"))
                SET DATE TO JAPANESE
                Assert.Equal("JAPAN", (STRING) Set("DATE"))
                SET DATE TO USA
                Assert.Equal("USA", (STRING) Set("DATE"))
            FINALLY
                SetDateFormat(cOld)
            END TRY
        END METHOD
    END CLASS

END NAMESPACE
