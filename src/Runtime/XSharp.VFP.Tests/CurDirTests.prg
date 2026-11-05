//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System
USING System.IO
USING XUnit

BEGIN NAMESPACE XSharp.VFP.Tests

    CLASS CurDirTests
        STATIC CONSTRUCTOR
            XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro
        END CONSTRUCTOR

        // VFP returns \FOLDER\SUB\ in upper case, without the drive (issue 2157)
        [Fact, Trait("Category", "File")];
        METHOD CurDirReturnsTheFolderInUpperCaseBetweenBackslashes() AS VOID
            LOCAL cCurrent AS STRING
            LOCAL cExpected AS STRING
            cCurrent := Environment.CurrentDirectory
            cExpected := cCurrent:Substring(cCurrent:IndexOf(Path.VolumeSeparatorChar) + 1)
            IF ! cExpected:EndsWith("\")
                cExpected += "\"
            ENDIF
            Assert.Equal(cExpected:ToUpperInvariant(), CurDir())
        END METHOD

        // at the root of the drive VFP returns a single backslash
        [Fact, Trait("Category", "File")];
        METHOD CurDirAtTheRootIsASingleBackslash() AS VOID
            LOCAL cSaved AS STRING
            cSaved := Environment.CurrentDirectory
            TRY
                Environment.CurrentDirectory := Path.GetPathRoot(cSaved)
                Assert.Equal("\", CurDir())
            FINALLY
                Environment.CurrentDirectory := cSaved
            END TRY
        END METHOD

        // with the current drive, with or without the colon, it is the current folder of that drive
        [Fact, Trait("Category", "File")];
        METHOD CurDirWithTheCurrentDrive() AS VOID
            LOCAL cDrive AS STRING
            cDrive := Environment.CurrentDirectory:Substring(0, 1)
            Assert.Equal(CurDir(), CurDir(cDrive))
            Assert.Equal(CurDir(), CurDir(cDrive + ":"))
        END METHOD

        // a drive that does not exist gives an empty string
        [Fact, Trait("Category", "File")];
        METHOD CurDirWithADriveThatDoesNotExist() AS VOID
            LOCAL cLetter AS STRING
            LOCAL aUsed AS System.Collections.Generic.List<STRING>
            aUsed := System.Collections.Generic.List<STRING>{}
            FOREACH VAR oDrive IN DriveInfo.GetDrives()
                aUsed:Add(oDrive:Name:Substring(0, 1):ToUpper())
            NEXT
            cLetter := ""
            FOREACH VAR c IN "ZYXWVUTSRQPONMLKJIHGFE"
                IF ! aUsed:Contains(c:ToString())
                    cLetter := c:ToString()
                    EXIT
                ENDIF
            NEXT
            IF cLetter:Length > 0
                Assert.Equal("", CurDir(cLetter))
            ENDIF
        END METHOD

    END CLASS

END NAMESPACE
