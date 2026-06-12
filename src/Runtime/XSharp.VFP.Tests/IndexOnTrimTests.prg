using System
using System.IO
using XUnit

begin namespace XSharp.VFP.Tests
    class IndexOnTrimTests
        static constructor
            XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro
        end constructor

        [Fact];
        method TestIndexOnAllTrim() as void
            var cOldDir := System.IO.Directory.GetCurrentDirectory()
            var oDir := System.IO.Directory.CreateDirectory(;
                Path.Combine(Path.GetTempPath(), ;
                "IdxTrimTest_" + Guid.NewGuid():ToString("N")))
            var cTempPath := oDir:FullName

            try
                SET DEFAULT TO (cTempPath)
                CREATE TABLE TrimTest (Id int, Name c(30), City C(20))
                INSERT INTO TrimTest VALUES(1, "  Alpha  ", "Berlin")
                INSERT INTO TrimTest VALUES(2, "Beta", "Munich")
                INSERT INTO TrimTest VALUES(3, "  Gamma  ", "")
                INSERT INTO TrimTest VALUES(4, "", "Hamburg")

                INDEX ON ALLTRIM(Name) TAG NameIdx
                GO TOP
                Assert.Equal(4, (int)RecNo())
                SKIP
                Assert.Equal(1, (int)RecNo())

                INDEX ON LTRIM(Name) TAG LNameIdx
                GO TOP
                Assert.True(RecNo() > 0)

                INDEX ON RTRIM(Name) TAG RNameIdx
                GO TOP
                Assert.True(RecNo() > 0)

                INDEX ON UPPER(SUBSTR(LTRIM(Name), 1, 5)) TAG SubIdx
                GO TOP
                Assert.True(RecNo() > 0)

                INDEX ON ALLTRIM(Name) + ALLTRIM(City) TAG CombIdx
                GO TOP
                Assert.True(RecNo() > 0)
            finally
                XSharp.CoreDb.CloseAll()
                FErase("TrimTest.DBF")
                FErase("TrimTest.CDX")
                SET DEFAULT TO (cOldDir)
                System.IO.Directory.SetCurrentDirectory(cOldDir)
                try
                    System.IO.Directory.Delete(cTempPath)
                catch
                end try
            end try
        end method
    end class
end namespace
