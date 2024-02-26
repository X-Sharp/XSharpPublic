//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#nullable disable
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;
using InternalSyntax = Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using XP = LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser;

namespace Microsoft.CodeAnalysis.CSharp
{
    /// <summary>
    /// The parsed representation of a C# source document.
    /// </summary>
    public abstract partial class CSharpSyntaxTree : SyntaxTree
    {
        // NOTE:
        // The spans and positions the we get here (when calculating breakpoint positions mostly)
        // have nothing to do with the real sourcecode.
        // Generated Using lines and generated static functions class also are included in the positions.
        // So a function like this
        // FUNCTION Start as VOID
        // Console.ReadLine()
        // RETURN
        // will be represented by a tree like this
        // using  static Functions  ;
        // using  System  ;
        // partial  public static class Functions
        // {
        //   static public void Start()
        //   {
        //      Console.ReadLine();
        //      return;
        //   }
        // }
        // so where the actual position in the source for the function start is 0
        // in the tree it will be a much larger number !
        // therefore we find the C# node in the tree first
        // and then use the X# node attached to it to find the real source location
        internal bool Generated { get; set; }
        static CSharpSyntaxNode s_lastNode = null;
        static int s_lastPos = 0;
        static CSharpSyntaxNode s_lastResult = null;
        static readonly object s_gate = new();

        private static CSharpSyntaxNode GetNode(CSharpSyntaxNode root, int position)
        {
            lock (s_gate)
            {
                if (root == s_lastNode && position == s_lastPos)
                {
                    return s_lastResult;
                }
                if (root.XNode != null && position != 0)
                {
                    var node = (CSharpSyntaxNode)root.ChildThatContainsPosition(position);
                    while (!node.Green.IsToken && (position > node.Position || position < (node.Position + node.FullWidth)))
                    {
                        var n = (CSharpSyntaxNode)node.ChildThatContainsPosition(position);
                        // also exit for variabledeclation because the order of our declaration is quite different
                        if (n == null || n == node || n is VariableDeclarationSyntax)
                            break;
                        node = n;
                    }
                    s_lastNode = root;
                    s_lastPos = position;
                    s_lastResult = node;
                    return node;
                }
                return null;
            }
        }

        private static CSharpSyntaxNode GetStatement(CSharpSyntaxNode node)
        {
            if (node is ExpressionSyntax)
            {
                var original = node;
                while (node != null)
                {
                    if (node is StatementSyntax)
                        break;
                    if (node is SwitchSectionSyntax)
                        break;
                    if (node is SwitchLabelSyntax)
                        break;
                    if (node is ElseClauseSyntax)
                        break;
                    if (node is CatchClauseSyntax)
                        break;
                    if (node is FinallyClauseSyntax)
                        break;
                    if (node is QueryClauseSyntax)
                        break;
                    node = node.Parent;
                }
                if (node == null)
                {
                    node = original;
                }
            }
            return node;
        }

        static CompilationUnitSyntax GetEof(CSharpSyntaxNode root, out IXParseTree eof, out int eofPos)
        {
            eof = null;
            eofPos = 0;
            if (root is CompilationUnitSyntax cus)
            {
                var node = cus.EndOfFileToken.Node;
                if (node is InternalSyntax.SyntaxToken st)
                {
                    eof = st.XNode;
                }
                eofPos = cus.EndOfFileToken.Position;
                return cus;
            }
            return null;
        }
        private LineVisibility GetXNodeVisibility(int position)
        {
            var root = GetRoot();
            if (root.XGenerated)
            {
                return LineVisibility.Hidden;
            }
            GetEof(root, out var eof, out var eofPos);
            if (position >= eofPos)
            {
                return LineVisibility.Hidden;
            }

            if (eof == null)
            {
                var node = GetNode(root, position);
                if (node != null)
                {
                    node = GetStatement(node);
                }
                if (node != null && node.XGenerated)
                    return LineVisibility.Hidden;
            }
            return LineVisibility.Visible;
        }

        private LinePosition GetXNodePosition(int position)
        {
            var text = this.GetText();
            var root = GetRoot();
            GetEof(root, out var eof, out var eofPos);
            if (position >= eofPos)
            {
                position -= eofPos;
            }
            if (eof == null)
            {
                var node = GetNode(root, position);
                position = (position < node.Position + node.FullWidth) ?
                                node.XNode?.Position ?? 0 : node.XNode?.Position + node.XNode?.FullWidth ?? 0;
                string file = node.XNode?.SourceFileName;
                if (node.XNode?.SourceSymbol != null)
                {
                    position = node.XNode.SourceSymbol.StartIndex;
                    file = node.XNode.SourceFileName;
                    if (string.IsNullOrEmpty(file))
                    {
                        file = (node.XNode.SourceSymbol as XSharpToken).SourceName;
                    }
                }
                if (!string.IsNullOrEmpty(file) && (root as CompilationUnitSyntax).IncludedFiles.TryGetValue(file, out var ntext))
                    text = ntext;
            }
            return text.Lines.GetLinePosition(position);
        }

        private FileLinePositionSpan GetXNodeSpan(TextSpan span)
        {
            // Note that the span that we receive here is the location in the pseudo C# source code
            // we need to locate the C# node first and then return the File/Line position of the matching X# code

            string file = this.FilePath;
            var text = this.GetText();
            var root = GetRoot();
            var cs = GetEof(root, out var eof, out var eofPos);
            int line = -1;
            int column = -1;
            var length = 0;
            if (span.Start >= eofPos)
            {
                // this is mostly used when there are parser errors
                var start = span.Start - eofPos;
                length = span.Length;
                if (cs != null)
                {
                    foreach (var lead in cs.EndOfFileToken.LeadingTrivia)
                    {
                        if (lead.HasStructure)
                        {
                            var f = lead.GetStructure().Green.GetFirstTerminal() as InternalSyntax.CSharpSyntaxNode;
                            if (start > f.FullWidth)
                            {
                                start -= f.FullWidth;
                                continue;
                            }
                            string fn = f.XNode?.SourceFileName;
                            if (!string.IsNullOrEmpty(fn) && cs.IncludedFiles.TryGetValue(fn, out var ntext))
                            {
                                text = ntext;
                                file = fn;
                            }
                            break;
                        }
                    }
                }
                if (length < 0)
                    length = 0;
                if (start + length > text.Length)
                {
                    length = text.Length - start;
                }
                span = new TextSpan(start, length);
            }
            else if (root.XNode != null && eof == null && span.Start != 0 && span.End != 0)
            {
                var snode = (CSharpSyntaxNode)root.ChildThatContainsPosition(span.Start);
                while (!snode.Green.IsToken && (span.Start > snode.Position || span.Length < snode.FullWidth))
                {
                    var child = (CSharpSyntaxNode)snode.ChildThatContainsPosition(span.Start);
                    if (child == null || child == snode) // no child found
                        break;
                    if (span.Start == child.Position && span.Length > child.FullWidth)
                    {
                        var en = (CSharpSyntaxNode)snode.ChildThatContainsPosition(span.End - 1);
                        if (en != null)
                        {
                            snode = child;
                            break;
                        }
                    }
                    snode = child;
                }
                var start = 0;
                string fn = file;
                // correct several incorrect breakpoint positions due to different order of things
                if (snode.Parent is VariableDeclarationSyntax)
                {
                    snode = snode.Parent;
                    if (snode.Parent is LocalDeclarationStatementSyntax)
                        snode = snode.Parent;
                }
                if (snode.XNode != null)
                {
                    // Roslyn wants zero based lines !
                    // Our line numbers are 1 based and column numbers are zero based..

                    var xNode = snode.XNode as XSharpParserRuleContext;
                    if (xNode is not XP.StatementContext)
                    {
                        switch (xNode.Parent)
                        {
                            case XP.CommonLocalDeclContext:
                            case XP.VarLocalDeclContext:
                            case XP.MemvardeclContext:
                            case XP.FoxmemvardeclContext:
                            case XP.FoxlparametersContext:
                            case XP.FilewidevarContext:

                            case XP.ClassvarsContext:
                            case XP.VoglobalContext:
                            case XP.XppclassvarsContext:
                            case XP.FoxclassvarsContext:
                                // We want to set the error on the variable and not the statement
                                break;
                            case XP.CaseStmtContext:
                            case XP.IfStmtContext:
                                // sequence point on the block condition
                                // do not set the breakpoint on the top level
                                break;
                            case XP.IMultiElementContext:
                            case XP.AccessMemberContext:
                            case XP.AssignmentExpressionContext:
                            case XP.BinaryExpressionContext:
                            case XP.MethodCallContext:
                            case XP.CtorCallContext:
                            case XP.DelegateCtorCallContext:
                            case XP.GlobalAttributesContext:
                            case XP.StatementBlockContext:
                            case XP.StatementContext:
                            case XP.ClassmemberContext:
                            case XP.IifContext:
                            case XP.LinqQueryContext:
                                xNode = (XSharpParserRuleContext)xNode.Parent;
                                break;
                        }
                    }

                    start = xNode.Position;
                    length = xNode.FullWidth;
                    line = xNode.Start.Line - 1;
                    column = xNode.Start.Column;
                    fn = xNode.SourceFileName;
                    if (xNode.SourceSymbol is XSharpToken symbol)
                    {
                        // for a define or UDC we want the location in the source and not in the #include
                        start = symbol.StartIndex;
                        length = symbol.StopIndex - start + 1;
                        var pos = symbol.OriginalTokenIndex;
                        if (pos < 0)    // happens sometimes with tokens from header files. See #1213
                        {
                            pos = 0;
                        }
                        while (pos < cs.XTokens.Size)
                        {
                            var token = cs.XTokens.Get(pos);
                            if (token.Type == XSharpLexer.EOS || token.Type == XSharpLexer.Eof)
                            {
                                length = token.Column - symbol.Column + 1;
                                break;
                            }
                            pos += 1;
                        }

                        fn = symbol.InputStream.SourceName;
                        line = symbol.Line - 1;
                        column = symbol.Column;
                    }
                }
                if (length < 0)
                    length = 0;
                if (!string.IsNullOrEmpty(fn) && cs.IncludedFiles.TryGetValue(fn, out var ntext))
                {
                    text = ntext;
                    file = fn;
                }
                if (string.IsNullOrEmpty(file) && root.XNode is XSharpParser.ScriptContext)
                {
                    file = "Script";
                }
                if (start + length > text.Length)
                {
                    // this should not happen, but we fix it anyway to prevent a nasty crash when generating
                    // pdb information
                    if (start > text.Length)
                        start = text.Length;
                    length = text.Length - start;
                }
                span = new TextSpan(start, length);
            }
            LinePosition s, e;
            if (line > 0 && column >= 0)
            {
                s = new LinePosition(line, column);
                if (length > 0)
                    e = new LinePosition(line, column + length);
                else
                    e = new LinePosition(line, column + 1);
            }
            else
            {
                s = text.Lines.GetLinePosition(span.Start);
                e = text.Lines.GetLinePosition(span.End);
            }
            if (e.Line > s.Line + 1)
            {
                e = new LinePosition(s.Line, s.Character + 1);
            }
            //System.Diagnostics.Debug.WriteLine($" {span.Start} {file} {s.Line}");
            return new FileLinePositionSpan(file, s, e);
        }
    }
}
