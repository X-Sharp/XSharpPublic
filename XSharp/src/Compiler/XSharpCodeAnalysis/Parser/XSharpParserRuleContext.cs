/*
   Copyright 2016-2017 XSharp B.V.

Licensed under the X# compiler source code License, Version 1.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.xsharp.info/licenses

Unless required by applicable law or agreed to in writing, software
Distributed under the License is distributed on an "as is" basis,
without warranties or conditions of any kind, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/
using System.Collections.Generic;
using Microsoft.CodeAnalysis;
using Antlr4.Runtime;
using Antlr4.Runtime.Tree;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.CSharp;
using MCT= Microsoft.CodeAnalysis.Text;
namespace LanguageService.CodeAnalysis.XSharp.SyntaxParser
{
    public class XSharpParserRuleContext :
        Antlr4.Runtime.ParserRuleContext,
        IMessageSerializable,
        IXParseTree
    {
        public XSharpParserRuleContext() : base()
        {

        }
#if !TEST
        public SyntaxTriviaList GetLeadingTrivia(CompilationUnitSyntax cu)
        {
            var list = new SyntaxTriviaList();
            if (cu == null)
                return list;
            XSharpToken start = this.Start as XSharpToken;
            var tokens = ((BufferedTokenStream) cu.XTokens).GetTokens();
            // find offset of first token in the tokenlist
            int startindex = start.OriginalTokenIndex;
            if (startindex >= 0) 
            {
                startindex -= 1;
                var endindex = startindex;
                var sb = new System.Text.StringBuilder();
                while (startindex >= 0)
                {
                    switch (tokens[startindex].Channel )
                    {
                        case XSharpLexer.XMLDOCCHANNEL:
                            sb.Insert(0,tokens[startindex].Text + "\r\n");
                            break;
                        case XSharpLexer.DefaultTokenChannel:
                            // exit the loop
                            startindex = 0;
                            break;
                    }
                    startindex--;
                }
                if (sb.Length > 0)
                {
                    string text = sb.ToString();
                    var source = MCT.SourceText.From(text);
                    var lexer = new Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax.Lexer(source, CSharpParseOptions.Default);
                    list = lexer.LexSyntaxLeadingTrivia();
                    lexer.Dispose();
                }
            }
            return list;
        }
#endif
        public XSharpParserRuleContext(Antlr4.Runtime.ParserRuleContext parent, int state) : base(parent, state)
        {

        }

        public override IErrorNode AddErrorNode(IToken badToken)
        {
            var t = new XTerminalNodeImpl(badToken);
            AddChild(t);
            t.parent = this;
            return t;
        }

  
        public object CsNode { get; set; }
        public string SourceFileName { get { return (Start as XSharpToken).SourceName; } }
        public string MappedFileName { get { return (Start as XSharpToken).MappedFileName; } }

        internal List<ParseErrorData> ErrorData;

        internal bool HasErrors()
        {
            return (ErrorData != null) && ErrorData.Count > 0;
        }

        public Location GetLocation()
        {
            return new XSharpSourceLocation(this);
        }
        internal void AddError(ParseErrorData e)
        {
            if (ErrorData == null)
                ErrorData = new List<ParseErrorData>();
            ErrorData.Add(e);


        }
        public new IToken Stop
        {
            get
            {
                return this.stop;
            }
            set
            {
                this.stop = value;
            }
        }
        public new IToken Start
        {
            get
            {
                return this.start;
            }
            set
            {
                this.start = value;
            }
        }

        int iBPLength = -1;
        int iBpStart = -1;
        public bool IsHidden { get { return iBPLength == -1; } }
        public int Position
        {
            get
            {
                if (iBpStart >= 0)
                    return iBpStart;
                return Start.StartIndex;
            }
        }
        public int FullWidth
        {
            get
            {
                if (iBPLength > 0)
                    return iBPLength;
                if (Stop != null)
                    return Stop.StopIndex - Start.StartIndex + 1;
                else
                    return Start.StopIndex - Start.StartIndex + 1;

            }
        }
        public int MappedLine { get { return (Start as XSharpToken).MappedLine; } }
        public IToken SourceSymbol { get { return (Start as XSharpToken).SourceSymbol; } }
        public override string ToString()
        {
            /*return this.GetText();*/
            var s = this.GetType().ToString();
            return s.Substring(s.LastIndexOfAny(".+".ToCharArray()) + 1).Replace("Context", "");
        }
        public void SetSequencePoint(IToken start, IToken end)
        {
            if (end != null && start != null)
            {
                iBpStart = start.StartIndex;
                if (end.StopIndex >= start.StartIndex)
                {
                    iBPLength = end.StopIndex - start.StartIndex + 1;
                }
                else if (end.StartIndex >= start.StartIndex)
                {
                    iBPLength = end.StartIndex - start.StartIndex + 1;
                }
                else
                {
                    iBPLength = 1;
                }
            }

        }

        public void SetSequencePoint(IToken next)
        {
            if (next != null)
            {
                if (next.StartIndex > this.Start.StartIndex)
                    iBPLength = next.StartIndex - this.Start.StartIndex;
                else if (next.StopIndex > this.Start.StartIndex)
                    iBPLength = next.StopIndex - this.Start.StartIndex;
                else
                    iBPLength = 1;
                if (iBPLength < 0)
                    iBPLength = 1;
            }

        }

        public void SetSequencePoint()
        {
            SetSequencePoint(Start, Stop);
        }

        public void SetSequencePoint(ParserRuleContext end)
        {
            if (end is XSharpParser.EosContext)
            {
                SetSequencePoint(this.Start, end.Start);
                return;
            }
            else if (end != null)
            {
                if (end.Stop != null)
                {
                    SetSequencePoint(this.Start, end.Stop);
                    return;
                }
                else
                {
                    SetSequencePoint(this.Start, end.Start);
                    return;
                }
            }
            if (this.Stop != null)
            {
                SetSequencePoint(this.Start, this.Stop);
                return;
            }
            else
            {
                var last = this.Start;
                foreach (var child in children)
                {
                    var c = child as ParserRuleContext;
                    if (c != null)
                    {
                        if (c.Stop != null && c.Stop.StopIndex > last.StopIndex)
                        {
                            last = c.Stop;
                        }
                        else if (c.Start.StopIndex > last.StopIndex)
                        {
                            last = c.Start;
                        }
                    }
                }
                SetSequencePoint(this.Start, last);
            }
        }
        public string ParentName
        {
            get
            {
                string name = "";
                if (Parent is XSharpParser.IEntityContext)
                {
                    name = ((XSharpParser.IEntityContext)Parent).Name + ".";
                }
                else if (Parent.Parent is XSharpParser.IEntityContext)
                {
                    name = ((XSharpParser.IEntityContext)Parent.Parent).Name + ".";
                }
                else if (Parent is XSharpParser.Namespace_Context)
                {
                    name = ((XSharpParser.Namespace_Context)Parent).Name.GetText() + ".";
                }
                return name;
            }
        }
    }
}


