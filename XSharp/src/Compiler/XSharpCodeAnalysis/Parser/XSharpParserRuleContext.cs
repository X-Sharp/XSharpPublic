//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#nullable disable
using System.Collections.Generic;
using Microsoft.CodeAnalysis;
using Antlr4.Runtime;
using Antlr4.Runtime.Tree;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using InternalSyntax = Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax;
using Microsoft.CodeAnalysis.CSharp;
using System;
using MCT= Microsoft.CodeAnalysis.Text;
using Antlr4.Runtime.Misc;

namespace LanguageService.CodeAnalysis.XSharp.SyntaxParser
{
    public class XSharpParserRuleContext :
        Antlr4.Runtime.ParserRuleContext,
        IXParseTree
        , IFormattable
    {
        public XSharpParserRuleContext() : base()
        {

        }
#if !VSPARSER
        public SyntaxTriviaList ParseTrivia(String comments)
        {
            var source = MCT.SourceText.From(comments);
            using (var lexer = new InternalSyntax.Lexer(source, CSharpParseOptions.Default))
            {
                return lexer.LexSyntaxLeadingTrivia();
            }

        }
        public SyntaxTriviaList GetFunctionDoc(CSharpSyntaxNode node)
        {
            // We only generate the function doc for the default tree to avoid generating an empty line for every prg file.
            if (node is ClassDeclarationSyntax &&
                node.CsGreen.XDefaultTree)
            {
                return ParseTrivia("/// <summary></summary>");
            }
            return default;
        }
        public SyntaxTriviaList GetLeadingTrivia(CSharpSyntaxNode parent, CompilationUnitSyntax cu)
        {
            if (parent is MethodDeclarationSyntax mdecl && mdecl.Identifier.Text.Contains("$"))
            {
                return default;
            }
            if (cu != null)
            {
                var options = ((CSharpParseOptions)cu.SyntaxTree.Options);
                if (cu.HasDocComments || options.TargetDLL != XSharpTargetDLL.Other)
                {
                    XSharpToken start = this.Start as XSharpToken;
                    // we have stored the XML comments of an entity in the first token of the ContextNode
                    if (start.HasXmlComments)
                    {
                        return ParseTrivia(start.XmlComments);
                    }
                }
                if (parent is PropertyDeclarationSyntax propdecl)
                {
                    var acc = propdecl.AccessorList;
                    for (int i = 0; i < acc.Accessors.Count; i++)
                    {
                        var accessor = acc.Accessors[i];
                        var node = accessor.XNode as XSharpParserRuleContext;
                        if (node != null && node.Start is XSharpToken xtoken && xtoken.HasXmlComments)
                        {
                            return ParseTrivia(xtoken.XmlComments);
                        }
                    }
                }
            }
            if (parent is FieldDeclarationSyntax fdecl)
            {
                if (fdecl.XNode is XSharpParser.VodefineContext)
                {
                    return ParseTrivia("/// <exclude />");
                }
            }
            return default;
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
#if !VSPARSER

        internal InternalSyntax.CSharpSyntaxNode CSharpSyntaxNode
        {
            get
            {
                return CsNode as InternalSyntax.CSharpSyntaxNode;
            }
        }

        internal InternalSyntax.CompilationUnitSyntax CompilationUnit
        {
            get
            {
                var node = this;
                while (node.Parent != null)
                {
                    node = node.Parent as XSharpParserRuleContext;
                }
                if (node != null && node.CsNode != null)
                {
                    return node.CsNode as InternalSyntax.CompilationUnitSyntax;
                }
                return null;
            }
        }
        public string SourceText
        {
            get
            {
                var start = Start as XSharpToken;
                var stop = Stop as XSharpToken;
                var cu = this.CompilationUnit;
                if (cu != null)
                {
                    var result = new System.Text.StringBuilder();
                    var tokens = ((BufferedTokenStream)cu.XPPTokens).GetTokens();
                    var startpos = start.TokenIndex;
                    var stoppos = stop.TokenIndex;
                    if (tokens == null)
                    {
                        tokens = ((BufferedTokenStream)cu.XTokens).GetTokens();
                        startpos = start.OriginalTokenIndex;
                        stoppos = stop.OriginalTokenIndex;
                    }
                    if (startpos < tokens.Count && stoppos < tokens.Count)
                    {
                        for (int i = startpos; i <= stoppos; i++)
                        {
                            var token = tokens[i];
                            if (!XSharpLexer.IsComment(token.Type))
                            {
                                if (token.Type != XSharpLexer.LINE_CONT)
                                    result.Append(token.Text);
                            }
                        }
                        result = result.Replace('\t', ' ');
                        return result.ToString();
                    }
                    else
                    {
                        return this.GetText();
                    }
                }

                var text = start.InputStream.GetText(Interval.Of(start.Position, stop.Position));
                return text;
            }
        }
        public Location GetLocation()
        {
            return new XSharpSourceLocation(this);
        }
#endif
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
#if !VSPARSER
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
        public void ClearSequencePoint()
        {
            iBpStart = -1;
            iBPLength = -1;
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
                if (Parent is XSharpParser.IEntityContext entity)
                {
                    name = entity.Name + ".";
                }
                else if (Parent.Parent is XSharpParser.IEntityContext entity2)
                {
                    name = entity2.Name + ".";
                }
                else if (Parent is XSharpParser.Namespace_Context ns)
                {
                    name = ns.Name.GetText() + ".";
                }
                return name;
            }
        }
#endif
        public string ToString(string format, IFormatProvider formatProvider)
        {
            return ToString();
        }

    }
}


