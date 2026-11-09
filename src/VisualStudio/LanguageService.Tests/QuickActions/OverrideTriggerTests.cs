using System.Collections.Generic;
using XSharp.LanguageService.Editors.LightBulb.QuickActions;
using Xunit;

namespace XSharp.LanguageService.Tests.QuickActions
{
    public class OverrideTriggerTests
    {
        //                                   0                                1                   2               3           4
        private static readonly string[] Lines = { "CLASS MyWindow INHERIT Window", "    METHOD Show()", "        nX := 1", "", "        RETURN",
        //   5     6                  7                   8           9      10
            "", "    // helper", "    METHOD Close()", "        RETURN", "", "END CLASS" };

        // the lines where a member of the class (or END CLASS) starts
        private static readonly HashSet<int> Declarations = new HashSet<int> { 1, 7, 10 };

        private static bool Applies(int caretLine, string[] lines = null)
        {
            lines = lines ?? Lines;
            return OverrideTrigger.Applies(i => lines[i], lines.Length, caretLine, 0, Declarations);
        }

        [Fact]
        public void OnTheClassHeader() => Assert.True(Applies(0));

        [Fact]
        public void OnABlankLineBeforeAMember() => Assert.True(Applies(5));

        [Fact]
        public void CommentsBeforeTheMemberAreSkipped() => Assert.True(Applies(5));

        [Fact]
        public void OnABlankLineBeforeEndClass() => Assert.True(Applies(9));

        [Fact]
        public void NotOnABlankLineInsideAMethodBody() => Assert.False(Applies(3));

        [Fact]
        public void NotOnCode() => Assert.False(Applies(2));

        [Fact]
        public void NotOnAMemberHeader() => Assert.False(Applies(7));

        [Fact]
        public void BlankLineAtTheEndOfTheFile()
        {
            // a VO-style class: its last method runs to the end of the file
            var lines = new[] { "CLASS Foo", "METHOD Bar() CLASS Foo", "    RETURN NIL", "" };
            Assert.True(OverrideTrigger.Applies(i => lines[i], lines.Length, 3, 0, new HashSet<int> { 1 }));
        }
    }
}
