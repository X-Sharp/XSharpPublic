using System.Linq;
using XSharp.LanguageService.Editors.LightBulb.QuickActions;
using Xunit;

namespace XSharp.LanguageService.Tests.QuickActions
{
    public class CallResultTests
    {
        /// <summary>How the call to Foo in <paramref name="text"/> uses its result.</summary>
        private static CallResult Of(string text)
        {
            var tokens = TestLexer.Lex(text);
            var name = tokens.First(t => t.Text == "Foo");
            return CallResult.Of(tokens, name);
        }

        [Theory]
        [InlineData("FUNCTION Test() AS VOID\r\n    Foo()\r\n    RETURN\r\n")]
        [InlineData("FUNCTION Test() AS VOID\r\n    SELF:Foo(1, 2)\r\n    RETURN\r\n")]
        [InlineData("FUNCTION Test() AS VOID\r\n    oCust:Foo()\r\n    RETURN\r\n")]
        [InlineData("FUNCTION Test() AS VOID\r\n    oCust:Owner:Foo(Bar(1))\r\n    RETURN\r\n")]
        public void StatementIsUnused(string text)
        {
            Assert.Equal(CallResultKind.Unused, Of(text).Kind);
        }

        [Theory]
        [InlineData("    IF Foo()\r\n    ENDIF\r\n")]
        [InlineData("    IF x > 1\r\n    ELSEIF SELF:Foo(1)\r\n    ENDIF\r\n")]
        [InlineData("    DO WHILE Foo()\r\n    ENDDO\r\n")]
        [InlineData("    IF !Foo()\r\n    ENDIF\r\n")]
        [InlineData("    IF .NOT. Foo()\r\n    ENDIF\r\n")]
        [InlineData("    IF x > 1 .AND. Foo()\r\n    ENDIF\r\n")]
        [InlineData("    IF Foo() || x\r\n    ENDIF\r\n")]
        public void ConditionIsLogic(string text)
        {
            var result = Of(text);
            Assert.Equal(CallResultKind.Type, result.Kind);
            Assert.Equal("LOGIC", result.Type);
        }

        [Theory]
        [InlineData("    LOCAL n := Foo() AS INT\r\n", "INT")]
        [InlineData("    LOCAL l := Foo() AS List<STRING>\r\n", "List<STRING>")]
        [InlineData("    LOCAL e := Foo() AS System.Text.Encoding\r\n", "System.Text.Encoding")]
        [InlineData("    LOCAL a := Foo() AS ARRAY OF STRING\r\n", "ARRAY OF STRING")]
        public void TypedDeclarationGivesItsType(string text, string type)
        {
            var result = Of(text);
            Assert.Equal(CallResultKind.Type, result.Kind);
            Assert.Equal(type, result.Type);
        }

        [Theory]
        [InlineData("    nCount := Foo()\r\n", "nCount")]
        [InlineData("    SELF:nCount := Foo(1)\r\n", "nCount")]
        public void AssignmentNamesTheVariable(string text, string variable)
        {
            var result = Of(text);
            Assert.Equal(CallResultKind.Variable, result.Kind);
            Assert.Equal(variable, result.Variable.Text);
        }

        [Fact]
        public void ReturnUsesTheMemberType()
        {
            Assert.Equal(CallResultKind.Return, Of("    RETURN Foo()\r\n").Kind);
        }

        /// <summary>The call name for the caret at '|' in <paramref name="text"/>.</summary>
        private static string NameAt(string text)
        {
            var position = text.IndexOf('|');
            return CallResult.NameAt(TestLexer.Lex(text.Remove(position, 1)), position)?.Text;
        }

        [Theory]
        [InlineData("    c:Test(|1, 2)\r\n", "Test")]
        [InlineData("    c:Test(1,| 2)\r\n", "Test")]
        [InlineData("    c:Test(1, \"a|b\")\r\n", "Test")]
        [InlineData("    c:Test(|)\r\n", "Test")]
        [InlineData("    c:Test()|\r\n", "Test")]
        [InlineData("    c:Test()   |\r\n", "Test")]
        [InlineData("    c:Test() // note|\r\n", "Test")]
        [InlineData("    c:Test(Bar(|1))\r\n", "Bar")]
        [InlineData("    c:Test(Bar(1) ,| 2)\r\n", "Test")]
        [InlineData("    c:Test(Bar(1))|\r\n", "Test")]
        [InlineData("    IF c:Test(1)|\r\n    ENDIF\r\n", "Test")]
        public void NameAtFindsTheCallAroundTheCaret(string text, string name)
        {
            Assert.Equal(name, NameAt(text));
        }

        [Theory]
        [InlineData("    x := {1,| 2}\r\n")]
        [InlineData("    x := Foo{1,| 2}\r\n")]
        [InlineData("    c:Test()\r\n    x := 1|\r\n")]
        [InlineData("    c:Test()\r\n|    x := 1\r\n")]
        [InlineData("    x := (1 +| 2)\r\n")]
        [InlineData("    x := 1|\r\n")]
        public void NameAtIsNullOutsideACall(string text)
        {
            Assert.Null(NameAt(text));
        }

        [Theory]
        [InlineData("    VAR x := Foo()\r\n")]
        [InlineData("    LOCAL x := Foo()\r\n")]
        [InlineData("    x := Foo() + 1\r\n")]
        [InlineData("    Bar(Foo())\r\n")]
        [InlineData("    ? Foo()\r\n")]
        [InlineData("    RETURN Foo() + 1\r\n")]
        [InlineData("    Foo(\r\n")]
        public void OtherUsesAreUnknown(string text)
        {
            Assert.Equal(CallResultKind.Unknown, Of(text).Kind);
        }
    }
}
