USING System
USING System.Collections.Generic
USING System.Text
USING System.IO
USING XUnit

BEGIN NAMESPACE XSharp.VFP.Tests
	CLASS FileVersionTests
        [Fact, Trait("Category", "AGetFileVersion")];
        METHOD BasicSystemDllTest AS VOID
            VAR cFile := typeof(System.String):Assembly:Location

            DIMENSION aVer(1)
            VAR nCount := AGetFileVersion(aVer, cFile)

            Assert.Equal(15, (INT)nCount)
            Assert.Equal(15, (INT)ALen(aVer))

            // element 2: Company Name
            Assert.Contains("Microsoft", (String)aVer[2])

            // element 4: File version (should not be empty)
            Assert.False(String.IsNullOrEmpty((String)aVer[4]))

            // element 10: Product Name (Microsoft® .NET Framework or similar)
            Assert.False(String.IsNullOrEmpty((String)aVer[10]))
        END METHOD

        [Fact, Trait("Category", "AGetFileVersion")];
        METHOD FileNotFoundTest AS VOID
            DIMENSION aVer(1)
            VAR nCount := AGetFileVersion(aVer, "c:\non_existent_file.exe")
            Assert.Equal(0, (INT)nCount)
            Assert.Equal(1, (INT)ALen(aVer))
        END METHOD

        [Fact, Trait("Category", "FDate")];
        METHOD FDateFTimeTest() AS VOID
            VAR tempFile := Path.GetTempFileName()
            File.WriteAllText(tempFile, "XSharp VFP File Functions Test")

            VAR dFileDate := FDate(tempFile)
            Assert.True(IsDate(dFileDate))

            VAR dtFileDate := FDate(tempFile, 1)
            Assert.True(IsDateTime(dtFileDate))

            VAR cFileTime := FTime(tempFile)
            Assert.True(cFileTime:Length == 8) // Format HH:mm:ss

            File.Delete(tempFile)
        END METHOD

        [Fact, Trait("Category", "FDate")];
        METHOD FSizeCompatibleTest() AS VOID
            VAR tempFile := Path.GetTempFileName()
            File.WriteAllText(tempFile, "123456789012345")

            SET COMPATIBLE OFF
            VAR nSizeOff := FSize(tempFile)
            Assert.Equal(0, nSizeOff)

            SET COMPATIBLE ON
            VAR nSizeOn := FSize(tempFile)
            Assert.Equal(15, nSizeOn)

            // VFP states: if no extention is passed then assume DBF
            VAR tempDbf := Path.ChangeExtension(tempFile, ".dbf")
            File.Copy(tempFile, tempDbf)
            VAR cNoExtension := Path.Combine(Path.GetDirectoryName(tempDbf), Path.GetFileNameWithoutExtension(tempDbf))

            VAR nSizeDbf := FSize(cNoExtension)
            Assert.Equal(15, nSizeDbf)

            SET COMPATIBLE OFF
            File.Delete(tempFile)
            File.Delete(tempDbf)
        END METHOD

        [Fact];
        METHOD FSizeLowLevelTest() AS VOID
            VAR cFile := Path.GetTempFileName()
            VAR hFile := (IntPtr) FCreate(cFile)

            Assert.NotEqual(IntPtr.Zero, hFile)
            FWrite((INT64) hFile, "12345")

            Assert.Equal((INT64)5, FSize(hFile))
            FClose((INT64) hFile)

            FFirst(cFile, 0)
            Assert.Equal((DWORD)5, FSize())

            FErase(cFile)
        END METHOD

        [Fact];
        METHOD FCountOverloadsTests() AS VOID
            DbCloseAll()

            VAR aStruct := { {"FIELD1", "C", 10, 0}, {"FIELD2", "N", 5, 0}, {"FIELD3", "L", 1, 0} }
            VAR cFile := Path.GetTempFileName() + ".dbf"

            DbCreate(cFile, aStruct)
            DbUseArea(TRUE, "DBFVFP", cFile, "TESTFCOUNT")

            Assert.Equal((DWORD)3, FCount())

            Assert.Equal((DWORD)3, FCount("TESTFCOUNT"))
            Assert.Equal((DWORD)3, FCount(Select("TESTFCOUNT")))

            DbCloseArea()
            FErase(cFile)
        END METHOD

        [Fact];
        METHOD FDateFTimeParameterlessTests() AS VOID
            VAR cFile := Path.GetTempFileName()

            StrToFile("test", cFile)

            FFirst(cFile, 0)

            VAR dDate := FDate()
            Assert.True(NULL_DATE != dDate)

            VAR cTime := FTime()
            Assert.True(cTime:Length > 0)

            FErase(cFile)
        END METHOD

        [Fact, Trait("Category", "FileFunctions")];
        METHOD TestDisplayPath() AS VOID
            VAR cPath := "C:\xsharp\projects\vfp\runtime\source\data\mydata.dbf"
            VAR cResult := DISPLAYPATH(cPath, 30)

            Assert.True(cResult:Length <= 30)
            Assert.Equal("c:\...\source\data\mydata.dbf", cResult)

            Assert.True(cResult:Length <= 30)

            Assert.ThrowsAny<Exception>({ => DISPLAYPATH(cPath, 5)})

        END METHOD
	END CLASS
END NAMESPACE // XSharp.VFP.Tests
