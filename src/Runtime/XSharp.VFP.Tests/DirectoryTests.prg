//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System
USING System.IO
USING XUnit

BEGIN NAMESPACE XSharp.VFP.Tests

    CLASS DirectoryTests
        STATIC CONSTRUCTOR
            XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro
        END CONSTRUCTOR

        STATIC PRIVATE METHOD NewTempDir() AS STRING
            LOCAL cDir AS STRING
            cDir := System.IO.Path.Combine(System.IO.Path.GetTempPath(), "VfpDirectoryTest_" + Guid.NewGuid():ToString("N"))
            System.IO.Directory.CreateDirectory(cDir)
            RETURN cDir
        END METHOD

        STATIC PRIVATE METHOD Hide(cDir AS STRING) AS VOID
            LOCAL oInfo AS System.IO.DirectoryInfo
            oInfo := System.IO.DirectoryInfo{cDir}
            oInfo:Attributes := (System.IO.FileAttributes) _OR((DWORD) System.IO.FileAttributes.Directory, ;
                                                               (DWORD) System.IO.FileAttributes.Hidden)
        END METHOD

        [Fact, Trait("Category", "File")];
        METHOD DirectoryFindsAnExistingDirectory() AS VOID
            LOCAL cDir AS STRING
            cDir := NewTempDir()
            TRY
                Assert.True(Directory(cDir))
            FINALLY
                System.IO.Directory.Delete(cDir)
            END TRY
        END METHOD

        [Fact, Trait("Category", "File")];
        METHOD DirectoryAcceptsATrailingSeparator() AS VOID
            LOCAL cDir AS STRING
            cDir := NewTempDir()
            TRY
                Assert.True(Directory(cDir + System.IO.Path.DirectorySeparatorChar:ToString()))
            FINALLY
                System.IO.Directory.Delete(cDir)
            END TRY
        END METHOD

        [Fact, Trait("Category", "File")];
        METHOD DirectoryReturnsFalseForAMissingDirectory() AS VOID
            Assert.False(Directory(System.IO.Path.Combine(System.IO.Path.GetTempPath(), "VfpDirectoryTest_" + Guid.NewGuid():ToString("N"))))
        END METHOD

        // a file is not a directory, VFP reports .F. for it
        [Fact, Trait("Category", "File")];
        METHOD DirectoryReturnsFalseForAFile() AS VOID
            LOCAL cDir AS STRING
            LOCAL cFile AS STRING
            cDir := NewTempDir()
            cFile := System.IO.Path.Combine(cDir, "afile.txt")
            System.IO.File.WriteAllText(cFile, "x")
            TRY
                Assert.False(Directory(cFile))
            FINALLY
                System.IO.File.Delete(cFile)
                System.IO.Directory.Delete(cDir)
            END TRY
        END METHOD

        [Fact, Trait("Category", "File")];
        METHOD DirectoryReturnsFalseForAnEmptyName() AS VOID
            Assert.False(Directory(""))
        END METHOD

        [Fact, Trait("Category", "File")];
        METHOD DirectoryHidesAHiddenDirectoryByDefault() AS VOID
            LOCAL cDir AS STRING
            cDir := NewTempDir()
            Hide(cDir)
            TRY
                Assert.False(Directory(cDir))
            FINALLY
                System.IO.Directory.Delete(cDir)
            END TRY
        END METHOD

        [Fact, Trait("Category", "File")];
        METHOD DirectoryFindsAHiddenDirectoryWithFlagOne() AS VOID
            LOCAL cDir AS STRING
            cDir := NewTempDir()
            Hide(cDir)
            TRY
                Assert.True(Directory(cDir, 1))
            FINALLY
                System.IO.Directory.Delete(cDir)
            END TRY
        END METHOD

        // only nFlags 1 ignores the attributes, 2 behaves like 0
        [Fact, Trait("Category", "File")];
        METHOD DirectoryTreatsFlagsOtherThanOneAsZero() AS VOID
            LOCAL cDir AS STRING
            cDir := NewTempDir()
            Hide(cDir)
            TRY
                Assert.False(Directory(cDir, 2))
            FINALLY
                System.IO.Directory.Delete(cDir)
            END TRY
        END METHOD

        // Windows marks a drive root Hidden and System, VFP still reports it
        [Fact, Trait("Category", "File")];
        METHOD DirectoryFindsADriveRoot() AS VOID
            Assert.True(Directory(System.IO.Path.GetPathRoot(Environment.SystemDirectory)))
        END METHOD

        [Fact, Trait("Category", "File")];
        METHOD DirectoryResolvesARelativeName() AS VOID
            LOCAL cName AS STRING
            LOCAL cDir AS STRING
            cName := "VfpDirectoryTest_" + Guid.NewGuid():ToString("N")
            cDir := System.IO.Path.Combine(Environment.CurrentDirectory, cName)
            System.IO.Directory.CreateDirectory(cDir)
            TRY
                Assert.True(Directory(cName))
            FINALLY
                System.IO.Directory.Delete(cDir)
            END TRY
        END METHOD

    END CLASS
END NAMESPACE
