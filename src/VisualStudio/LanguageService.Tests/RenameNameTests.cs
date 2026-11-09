using XSharp.LanguageService.Rename;
using Xunit;

namespace XSharp.LanguageService.Tests
{
    public class RenameNameTests
    {
        [Theory]
        [InlineData("Close", "Shut", "Valid")]
        [InlineData("Close", "CLOSE", "CaseOnly")]
        [InlineData("Close", "Close", "Unchanged")]
        [InlineData("Close", "  ", "Empty")]
        [InlineData("Close", "1abc", "Invalid")]
        [InlineData("Close", "a b", "Invalid")]
        [InlineData("Close", "a.b", "Invalid")]
        [InlineData("Close", "CLASS", "Keyword")]
        [InlineData("Close", "@@CLASS", "Valid")]
        public void NewNameIsChecked(string oldName, string newName, string expected)
        {
            Assert.Equal(expected, RenameName.Check(oldName, newName, s => TestLexer.Lex(s)).ToString());
        }
    }
}
