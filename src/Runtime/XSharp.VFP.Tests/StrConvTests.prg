using System
using System.Text
using XUnit
using XSharp.VFP

begin namespace XSharp.VFP.Tests
    class StrConvTests
        [Fact, Trait("Category", "StrConv")];
        method Base64Tests() as void
            // String -> Base64
            Assert.Equal("QWJjZA==", StrConv("Abcd", 13))

            // Base64 -> Binary (14)
            var res := StrConv("QWJjZA==", 14)
            Assert.True(IsBinary(res))
            VAR bytes := (BINARY)res
            VAR content := Encoding.UTF8.GetString(bytes)
            Assert.Equal("Abcd", content)

            // Binary Input -> Base64 (13)
            var binInput := (BINARY) 0h010203
            Assert.Equal("AQID", StrConv(binInput, 13))
        end method

        [Fact, Trait("Category", "StrConv")];
        method HexTests() as void
            // String -> Hex (15)
            Assert.Equal("41626364", StrConv("Abcd", 15))

            // Hex -> Binary (16)
            var res := StrConv("41626364", 16)
            Assert.True(IsBinary(res))
            Assert.Equal("Abcd", (STRING)(BINARY)res)

            // Binary Input -> Hex (15)
            var binInput := (BINARY) 0h010203
            Assert.Equal("010203", StrConv(binInput, 15))

        end method

        [Fact, Trait("Category", "StrConv")];
        method CaseTests() as void
            // Lower (7)
            Assert.Equal("hello world", StrConv("HELLO WORLD", 7))

            // Upper (8)
            Assert.Equal("HELLO WORLD", StrConv("hello world", 8))
        end method

        [Fact, Trait("Category", "StrConv")];
        method UnicodeUTF8Tests() as void
            // Unicode String -> UTF8 Bytes (10)
            var cOriginal := "World"
            var resUTF8 := StrConv(cOriginal, 10)

            Assert.True(IsBinary(resUTF8))

            // "Pará" -> "á" is C3 A1 in UTF8
            var cWithAccent := "Pará"
            var resWithAccent := StrConv(cWithAccent, 10)


            var hexString := (STRING) resWithAccent
            Assert.Equal("0h506172C3A1", hexString)

            // UTF8 Bytes -> Unicode String (12)
            var resRevert := StrConv(resWithAccent, 12)
            Assert.Equal("Pará", resRevert)


        end method
    end class
end namespace
