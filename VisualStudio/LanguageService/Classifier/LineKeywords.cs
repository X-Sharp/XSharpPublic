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

        internal static XKeyword Tokens2Keyword(IList<IToken> tokens)
        {
            IToken firstkw = null, secondkw = null;
            bool startOfLine = true;
            XKeyword kw = default;
            IToken lastModifier = null;
            if (tokens?.Count > 0)
            {
                foreach (var token in tokens)
                {
                    if (token.Type == XSharpLexer.WS)
                        continue;
                    if (XSharpLexer.IsComment(token.Type))
                        continue;
                    if (XSharpLexer.IsModifier(token.Type) && token.Type != XSharpLexer.CLASS)
                    {
                        lastModifier = token;
                        continue;
                    }
                    if (token.Type == XSharpLexer.ID && lastModifier != null)
                    {
                        firstkw = lastModifier;
                    }
                    else if (XSharpLexer.IsKeyword(token.Type))
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
                    lastModifier = null;
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
