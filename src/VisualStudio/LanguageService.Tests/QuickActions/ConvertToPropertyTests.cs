using System.Collections.Generic;
using System.Linq;
using XSharp.LanguageService.Editors.LightBulb.QuickActions;
using XSharpModel;
using Xunit;

namespace XSharp.LanguageService.Tests.QuickActions
{
    /// <summary>A whole pair through ConvertToPropertyAction.ForPair: line ranges, rewrite and property text together.</summary>
    public class ConvertToPropertyTests
    {
        private static readonly XFile File = new XFile(@"C:\Test\A.prg", null);

        private static XSourceMemberSymbol Member(Kind kind, string name, int header, string type, params (string Name, string Type)[] parameters)
        {
            var member = new XSourceMemberSymbol(name, kind, Modifiers.Public, new TextRange(header, 1, header + 1, 1), new TextInterval(header * 100, header * 100 + 50), type, null);
            member.File = File;
            foreach (var p in parameters)
                member.AddParameter(new XSourceParameterSymbol(member, p.Name, new TextRange(header, 1, header, 1), new TextInterval(header * 100, header * 100 + 1), p.Type));
            return member;
        }

        /// <summary>Converts the pair in <paramref name="lines"/> and returns the new lines, or null when refused.</summary>
        private static List<string> Convert(string[] lines, AccessAssignPair pair, params int[] boundaries)
        {
            var style = new CodeStyle(XDialect.VO, s => s.ToUpperInvariant(), "    ", "\r\n");
            var edits = ConvertToPropertyAction.ForPair(pair, i => lines[i], lines.Length, boundaries.ToList(), style, s => TestLexer.Lex(s, "dialect:VO"));
            if (edits == null)
                return null;
            var result = lines.ToList();
            foreach (var r in edits.OrderByDescending(r => r.StartLine))
            {
                result.RemoveRange(r.StartLine, r.EndLine - r.StartLine + 1);
                if (r.Text.Length > 0)
                    result.InsertRange(r.StartLine, r.Text.TrimEnd('\n').Split('\n').Select(l => l.TrimEnd('\r')));
            }
            return result;
        }

        [Fact]
        public void InstanceVariableNamedLikeTheParameter()
        {
            var lines = new[] { "CLASS C", "    ACCESS Owner AS OBJECT", "        RETURN SELF:oOwner", "",
                "    ASSIGN Owner(oOwner AS OBJECT)", "        SELF:oOwner := oOwner", "END CLASS" };
            var pair = new AccessAssignPair("Owner", Member(Kind.Access, "Owner", 1, "OBJECT"), Member(Kind.Assign, "Owner", 4, "", ("oOwner", "OBJECT")));
            var result = Convert(lines, pair, 1, 4, 6);
            Assert.Contains("    PROPERTY Owner AS OBJECT GET SELF:oOwner SET SELF:oOwner := VALUE", result);
        }

        [Fact]
        public void RegionLinesStay()
        {
            var lines = new[] { "CLASS C", "    #region properties", "    ACCESS Name AS STRING", "        RETURN SELF:_c", "",
                "    ASSIGN Name(c AS STRING)", "        SELF:_c := c", "    #endregion", "END CLASS" };
            var pair = new AccessAssignPair("Name", Member(Kind.Access, "Name", 2, "STRING"), Member(Kind.Assign, "Name", 5, "", ("c", "STRING")));
            var result = Convert(lines, pair, 2, 5, 8);
            Assert.Equal(new[] { "CLASS C", "    #region properties", "    PROPERTY Name AS STRING GET SELF:_c SET SELF:_c := VALUE",
                "    #endregion", "END CLASS" }, result);
        }

        [Fact]
        public void PreprocessorInsideABodyIsRefused()
        {
            var lines = new[] { "CLASS C", "    ACCESS Name AS STRING", "    #ifdef DEBUG", "        RETURN \"debug\"", "    #endif",
                "        RETURN SELF:_c", "END CLASS" };
            var pair = new AccessAssignPair("Name", Member(Kind.Access, "Name", 1, "STRING"), null);
            Assert.Null(Convert(lines, pair, 1, 6));
        }

        [Fact]
        public void DifferentIndexNamesAreMapped()
        {
            var lines = new[] { "CLASS C", "    ACCESS Item(nIndex AS INT) AS STRING", "        RETURN SELF:aItems[nIndex]",
                "    ASSIGN Item(c AS STRING, n AS INT)", "        SELF:aItems[n] := c", "END CLASS" };
            var pair = new AccessAssignPair("Item", Member(Kind.Access, "Item", 1, "STRING", ("nIndex", "INT")),
                Member(Kind.Assign, "Item", 3, "", ("c", "STRING"), ("n", "INT")));
            var result = Convert(lines, pair, 1, 3, 5);
            Assert.Contains("    PROPERTY Item[nIndex AS INT] AS STRING GET SELF:aItems[nIndex] SET SELF:aItems[nIndex] := VALUE", result);
        }
    }
}
