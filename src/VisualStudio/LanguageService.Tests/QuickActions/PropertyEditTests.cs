using System.Collections.Generic;
using System.Linq;
using XSharp.LanguageService.Editors.LightBulb.QuickActions;
using Xunit;

namespace XSharp.LanguageService.Tests.QuickActions
{
    public class PropertyEditTests
    {
        //                                     0                  1                      2                            3
        private static readonly string[] Lines = { "CLASS Customer", "    // the name", "    ACCESS Name AS STRING", "        RETURN SELF:_c",
        //   4     5                      6                  7     8                           9                                     10
            "", "    METHOD Show()", "        RETURN", "", "    /// sets the name", "    ASSIGN Name(cValue AS STRING)", "        SELF:_c := cValue",
        //   11    12                      13
            "", "    // next comment", "    METHOD Hide()", "        RETURN", "END CLASS" };

        private static string Line(int i) => Lines[i];

        /// <summary>Applies the replacements bottom-up to <see cref="Lines"/>.</summary>
        private static List<string> Apply(IEnumerable<LineReplacement> replacements)
        {
            var result = Lines.ToList();
            foreach (var r in replacements.OrderByDescending(r => r.StartLine))
            {
                result.RemoveRange(r.StartLine, r.EndLine - r.StartLine + 1);
                if (r.Text.Length > 0)
                    result.InsertRange(r.StartLine, r.Text.TrimEnd('\n').Split('\n').Select(l => l.TrimEnd('\r')));
            }
            return result;
        }

        [Fact]
        public void MemberLinesIncludeCommentsAbove()
        {
            var lines = PropertyEdit.MemberLines(Line, Lines.Length, 2, 5);
            Assert.Equal(1, lines.StartLine);
            Assert.Equal(2, lines.HeaderLine);
            Assert.Equal(3, lines.EndLine);
        }

        [Fact]
        public void MemberLinesStopBeforeNextComment()
        {
            var lines = PropertyEdit.MemberLines(Line, Lines.Length, 9, 13);
            Assert.Equal(8, lines.StartLine);
            Assert.Equal(10, lines.EndLine);
        }

        [Fact]
        public void CommentsAboveTheRemovedMemberMove()
        {
            var access = PropertyEdit.MemberLines(Line, Lines.Length, 2, 5);
            var assign = PropertyEdit.MemberLines(Line, Lines.Length, 9, 13);
            var result = Apply(PropertyEdit.Replacements(Line, access, assign, "    PROPERTY Name AS STRING GET SELF:_c SET SELF:_c := VALUE\r\n"));
            Assert.Equal(new[] { "CLASS Customer", "    // the name", "    /// sets the name", "    PROPERTY Name AS STRING GET SELF:_c SET SELF:_c := VALUE",
                "", "    METHOD Show()", "        RETURN", "", "    // next comment", "    METHOD Hide()", "        RETURN", "END CLASS" }, result);
        }

        [Fact]
        public void MembersInBetweenStay()
        {
            var access = PropertyEdit.MemberLines(Line, Lines.Length, 2, 5);
            var assign = PropertyEdit.MemberLines(Line, Lines.Length, 9, 13);
            var result = Apply(PropertyEdit.Replacements(Line, access, assign, "    PROPERTY Name\r\n"));
            Assert.Contains("    METHOD Show()", result);
            Assert.Contains("    METHOD Hide()", result);
            Assert.DoesNotContain(result, l => l.Contains("ASSIGN") || l.Contains("ACCESS"));
        }

        [Fact]
        public void SingleMemberIsReplacedInPlace()
        {
            var access = PropertyEdit.MemberLines(Line, Lines.Length, 2, 5);
            var result = Apply(PropertyEdit.Replacements(Line, access, null, "    PROPERTY Name AS STRING GET SELF:_c\r\n"));
            Assert.Equal("    PROPERTY Name AS STRING GET SELF:_c", result[2]);
            Assert.Equal("", result[3]);
        }

        [Fact]
        public void ClassVariantDoesNotOverlap()
        {
            var a = PropertyEdit.MemberLines(Line, Lines.Length, 2, 5);
            var b = PropertyEdit.MemberLines(Line, Lines.Length, 9, 13);
            var all = PropertyEdit.Replacements(Line, a, null, "X\r\n").Concat(PropertyEdit.Replacements(Line, b, null, "Y\r\n")).ToList();
            Assert.True(PropertyEdit.NonOverlapping(all));
            Assert.False(PropertyEdit.NonOverlapping(all.Concat(new[] { new LineReplacement(3, 9, "") }).ToList()));
        }

        [Theory]
        [InlineData("    ACCESS Name AS STRING", "", true)]
        [InlineData("    PROTECTED ACCESS Name AS STRING", "PROTECTED", true)]
        [InlineData("    protected internal assign Name(c)", "PROTECTED INTERNAL", true)]
        [InlineData("    STATIC ACCESS Name", "", false)]
        public void HeaderVisibility(string header, string visibility, bool onlyVisibility)
        {
            Assert.Equal(onlyVisibility, PropertyEdit.HeaderVisibility(header, out var found));
            if (onlyVisibility)
                Assert.Equal(visibility, found);
        }
    }
}
