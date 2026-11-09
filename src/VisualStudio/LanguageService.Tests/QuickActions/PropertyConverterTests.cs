using System;
using XSharp.LanguageService.Editors.LightBulb.QuickActions;
using XSharpModel;
using Xunit;

namespace XSharp.LanguageService.Tests.QuickActions
{
    public class PropertyConverterTests
    {
        private static AccessorSource Source(string body, string parameter = null, string[] index = null, string visibility = "", string[] indexNames = null)
        {
            return new AccessorSource(body, parameter, index ?? new string[0], visibility, indexNames);
        }

        [Fact]
        public void FinalReturnAfterSemicolonKeepsTheAssignment()
        {
            Assert.Equal("    PROPERTY Name AS STRING SET SELF:_c := VALUE\r\n", Build(null, Source("        SELF:_c := cValue ; RETURN", "cValue")));
        }

        [Fact]
        public void OneLineWithCommentUsesBlocks()
        {
            var text = Build(Source("        RETURN SELF:_c // cached"), null);
            Assert.Contains("// cached", text);
            Assert.EndsWith("    END PROPERTY\r\n", text);
        }

        [Fact]
        public void IndexNamesAreMapped()
        {
            var text = Build(Source("        RETURN SELF:aItems[nIndex]", index: new[] { "nIndex AS INT" }, indexNames: new[] { "nIndex" }),
                Source("        SELF:aItems[n] := c", "c", new[] { "n AS INT" }, indexNames: new[] { "n" }), "Item");
            Assert.Equal("    PROPERTY Item[nIndex AS INT] AS STRING GET SELF:aItems[nIndex] SET SELF:aItems[nIndex] := VALUE\r\n", text);
        }

        [Fact]
        public void BodyThatCannotBeRewrittenGivesNull()
        {
            Assert.Null(Build(Source("        RETURN SELF:_c"), Source("        RETURN a + SELF:Foo()", "c")));
        }

        private static CodeStyle Style(Func<string, string> keyword = null)
        {
            return new CodeStyle(XDialect.VO, keyword ?? (s => s.ToUpperInvariant()), "    ", "\r\n");
        }

        private static string Build(AccessorSource access, AccessorSource assign, string name = "Name", string type = "STRING", Func<string, string> keyword = null)
        {
            return PropertyConverter.Build(name, type, access, assign, "    ", Style(keyword), s => TestLexer.Lex(s, "dialect:VO"));
        }

        [Fact]
        public void OneLinePair()
        {
            var text = Build(Source("        RETURN SELF:_c"), Source("        SELF:_c := cValue", "cValue"));
            Assert.Equal("    PROPERTY Name AS STRING GET SELF:_c SET SELF:_c := VALUE\r\n", text);
        }

        [Fact]
        public void AccessOnly()
        {
            Assert.Equal("    PROPERTY Name AS STRING GET SELF:_c\r\n", Build(Source("        RETURN SELF:_c"), null));
        }

        [Fact]
        public void AssignOnly()
        {
            Assert.Equal("    PROPERTY Name AS STRING SET SELF:_c := VALUE\r\n", Build(null, Source("        SELF:_c := cValue\r\n        RETURN", "cValue")));
        }

        [Fact]
        public void MultiLineBodiesKeepComments()
        {
            var access = Source("        // cached\r\n        IF SELF:_c == NULL\r\n            SELF:_c := \"\"\r\n        ENDIF\r\n        RETURN SELF:_c");
            var text = Build(access, Source("        SELF:_c := cValue", "cValue"));
            Assert.Equal(
                "    PROPERTY Name AS STRING\r\n" +
                "        GET\r\n" +
                "            // cached\r\n" +
                "            IF SELF:_c == NULL\r\n" +
                "                SELF:_c := \"\"\r\n" +
                "            ENDIF\r\n" +
                "            RETURN SELF:_c\r\n" +
                "        END GET\r\n" +
                "        SET\r\n" +
                "            SELF:_c := VALUE\r\n" +
                "        END SET\r\n" +
                "    END PROPERTY\r\n", text);
        }

        [Fact]
        public void EmptyAssignUsesMultiLineForm()
        {
            var text = Build(Source("        RETURN SELF:_c"), Source("", "cValue"));
            Assert.Contains("        SET\r\n        END SET\r\n    END PROPERTY\r\n", text);
        }

        [Fact]
        public void ContinuedLineUsesMultiLineForm()
        {
            var text = Build(Source("        RETURN SELF:_c"), Source("        SELF:_c := ;\r\n            cValue", "cValue"));
            Assert.Contains("            SELF:_c := ;\r\n                VALUE\r\n", text);
            Assert.EndsWith("    END PROPERTY\r\n", text);
        }

        [Fact]
        public void IndexedPair()
        {
            var text = Build(Source("        RETURN SELF:aItems[n]", index: new[] { "n AS INT" }),
                Source("        SELF:aItems[n] := c", "c", new[] { "n AS INT" }), "Item");
            Assert.Equal("    PROPERTY Item[n AS INT] AS STRING GET SELF:aItems[n] SET SELF:aItems[n] := VALUE\r\n", text);
        }

        [Fact]
        public void NarrowerVisibilityOnAccessor()
        {
            var text = Build(Source("        RETURN SELF:_c"), Source("        SELF:_c := cValue", "cValue", visibility: "PROTECTED"));
            Assert.Equal("    PROPERTY Name AS STRING GET SELF:_c PROTECTED SET SELF:_c := VALUE\r\n", text);
        }

        [Fact]
        public void WiderVisibilityOnProperty()
        {
            var text = Build(Source("        RETURN SELF:_c", visibility: "PROTECTED"), Source("        SELF:_c := cValue", "cValue", visibility: "PROTECTED"));
            Assert.StartsWith("    PROTECTED PROPERTY Name AS STRING GET", text);
        }

        [Fact]
        public void StatementInSetUsesMultiLineForm()
        {
            var text = Build(Source("        RETURN SELF:_c"), Source("        IF cValue != NULL ; SELF:_c := cValue ; ENDIF", "cValue"));
            Assert.Contains("END PROPERTY", text);
        }

        [Fact]
        public void KeywordCase()
        {
            var text = Build(Source("        RETURN SELF:_c"), null, keyword: s => s.ToLowerInvariant());
            Assert.Equal("    property Name as STRING get SELF:_c\r\n", text);
        }
    }
}
