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
	END CLASS
END NAMESPACE // XSharp.VFP.Tests
