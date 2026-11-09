using XSharp.LanguageService.Editors.LightBulb.QuickActions;
using XSharpModel;
using Xunit;

namespace XSharp.LanguageService.Tests.QuickActions
{
    public class AccessorBodyTests
    {
        private static string Rename(string body, string parameter = "cValue")
        {
            return AccessorBody.RenameParameter(body, TestLexer.Lex(body, "dialect:VO"), parameter, "VALUE");
        }

        private static string Returns(string body)
        {
            var style = new CodeStyle(XDialect.VO, s => s.ToUpperInvariant(), "    ", "\r\n");
            return AccessorBody.RewriteReturns(body, TestLexer.Lex(body, "dialect:VO"), style);
        }

        [Fact]
        public void OnlyIdentifierTokensAreRenamed()
        {
            Assert.Equal("SELF:_c := VALUE // cValue\r\n? \"cValue\", cValueOld",
                Rename("SELF:_c := cValue // cValue\r\n? \"cValue\", cValueOld"));
        }

        [Fact]
        public void MemberAccessIsNotRenamed()
        {
            // VO: the parameter is often named like the instance variable
            Assert.Equal("SELF:oOwner := VALUE\r\n? oOther:oOwner, oOther.oOwner, _FIELD->oOwner",
                Rename("SELF:oOwner := oOwner\r\n? oOther:oOwner, oOther.oOwner, _FIELD->oOwner", "oOwner"));
        }

        [Fact]
        public void ReturnFieldIsDropped() => Assert.Equal("    RETURN", Returns("    RETURN SELF:_c"));

        [Fact]
        public void ReturnNegativeLiteralIsDropped() => Assert.Equal("    RETURN", Returns("    RETURN -1"));

        [Fact]
        public void ReturnCallIsKept() => Assert.Equal("    SELF:Changed()\r\n    RETURN", Returns("    RETURN SELF:Changed()"));

        [Fact]
        public void ReturnMixedIsRefused() => Assert.Null(Returns("    RETURN a + SELF:Foo()"));

        [Fact]
        public void ReturnAfterSemicolonOnTheLineIsRefused() => Assert.Null(Returns("    IF c != NIL ; RETURN SELF:_c := c ; ENDIF"));

        [Fact]
        public void RenameIgnoresCase()
        {
            Assert.Equal("SELF:_c := VALUE", Rename("SELF:_c := CVALUE"));
        }

        [Fact]
        public void ReturnWithLiteralOrIdentifierDropsIt()
        {
            Assert.Equal("    RETURN", Returns("    RETURN NIL"));
            Assert.Equal("    SELF:_c := c\r\n    RETURN", Returns("    SELF:_c := c\r\n    RETURN c"));
        }

        [Fact]
        public void ReturnWithAssignmentKeepsTheAssignment()
        {
            Assert.Equal("    SELF:_c := c\r\n    RETURN", Returns("    RETURN SELF:_c := c"));
        }

        [Fact]
        public void BareReturnStays()
        {
            Assert.Equal("    SELF:_c := c\r\n    RETURN", Returns("    SELF:_c := c\r\n    RETURN"));
        }

        [Fact]
        public void OtherCodeUnchanged()
        {
            const string body = "    SELF:_c := c\r\n    SELF:Changed()";
            Assert.Equal(body, Returns(body));
        }
    }
}
