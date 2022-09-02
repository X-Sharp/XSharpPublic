using XSharpModel;
using Microsoft.VisualStudio.Text;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using LanguageService.SyntaxTree;
using System.Collections.Generic;

namespace XSharp.LanguageService
{

    /// <summary>
    /// This class keeps the first keyword or first two keywords for special lines in the editor
    /// These keywords are used for formatting the code
    /// </summary>
    internal class XSharpLineKeywords : XSharpLineInfo<XKeyword>
    {
        internal XSharpLineKeywords() : base()
        {
        }

        static bool IsInstanceVar(int keyword)
        {
            switch (keyword)
            {
                case XSharpLexer.EXPORT:
                case XSharpLexer.HIDDEN:
                case XSharpLexer.PUBLIC:
                case XSharpLexer.PRIVATE:
                case XSharpLexer.PROTECTED:
                    return true;
            }
            return false;
        }

        internal static XKeyword Tokens2Keyword(IList<IToken> tokens)
        {
            IToken firstkw = null, secondkw = null;
            bool startOfLine = true;
            XKeyword kw = default;
            if (tokens?.Count > 0)
            {
                foreach (var token in tokens)
                {
                    if (token.Type == XSharpLexer.WS)
                        continue;
                    if (XSharpLexer.IsComment(token.Type))
                        continue;

                    var include = IsInstanceVar(token.Type) || !XSharpLexer.IsModifier(token.Type);
                    if (include || token.Type == XSharpLexer.CLASS)
                    {
                        if (XSharpLexer.IsKeyword(token.Type))
                        {
                            if (firstkw == null)
                            {
                                firstkw = token;
                            }
                            else if (secondkw == null)
                            {
                                secondkw = token;
                            }
                            else
                            {
                                startOfLine = false;
                            }
                        }
                        else
                        {
                            startOfLine = false;
                        }
                    }
                    if (!startOfLine)
                        break;
                }
                // encode keyword
                if (firstkw != null)
                {
                    if (XFormattingRule.IsSingleKeyword(firstkw.Type))
                    {
                        kw = new XKeyword(firstkw.Type);
                    }
                    else if (secondkw != null)
                    {
                        kw = new XKeyword(firstkw.Type, secondkw.Type);
                    }
                    else
                    {
                        kw = new XKeyword(firstkw.Type);
                    }
                }
            }
            return kw;
        }

    }
}
