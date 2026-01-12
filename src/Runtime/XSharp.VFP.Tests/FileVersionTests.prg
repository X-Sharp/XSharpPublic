USING System
USING System.Collections.Generic
USING System.Text
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

            // element 10: Product Name (Microsoft® .NET Framework o similar)
            Assert.False(String.IsNullOrEmpty((String)aVer[10]))
        END METHOD

        [Fact, Trait("Category", "AGetFileVersion")];
        METHOD FileNotFoundTest AS VOID
            DIMENSION aVer(1)
            VAR nCount := AGetFileVersion(aVer, "c:\non_existent_file.exe")
            Assert.Equal(0, (INT)nCount)
            Assert.Equal(1, (INT)ALen(aVer))
        END METHOD
	END CLASS
END NAMESPACE // XSharp.VFP.Tests
