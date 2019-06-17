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
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;
using Roslyn.Utilities;
using InternalSyntax = Microsoft.CodeAnalysis.CSharp.Syntax.InternalSyntax;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;

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
        //      Console.ReadLine(); return;
        //   }
        // }
        // so where the actual position in the source for the function start is 0
        // in the tree it will be a much larger number !
        // therefore we find the C# node in the tree first
        // and then use the X# node attached to it to find the real source location


        internal bool Generated { get; set; }
        private CSharpSyntaxNode GetNode(CSharpSyntaxNode root, int position)
        {
            if (root.XNode != null && position != 0)
            {
                var node = (CSharpSyntaxNode)GetRoot().ChildThatContainsPosition(position);
                while (!node.Green.IsToken && (position > node.Position || position < (node.Position + node.FullWidth)))
                {
                    var n = (CSharpSyntaxNode)node.ChildThatContainsPosition(position);
					// also exit for variabledeclation because the order of our declaration is quite different
                    if (n == null || n == node || n is VariableDeclarationSyntax)
                        break;
                    node = n;
                }
                return node;
            }
            return null;
        }

        private CSharpSyntaxNode GetStatement(CSharpSyntaxNode node )
        {
            if (node is ExpressionSyntax)
            {
                var original = node;
                while (node != null)
                {
                    if (node is StatementSyntax)
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
        private LineVisibility GetXNodeVisibility(int position)
        {
            var root = (CSharpSyntaxNode)GetRoot();
            var eof = ((root as CompilationUnitSyntax)?.EndOfFileToken.Node as InternalSyntax.SyntaxToken)?.XNode;
            var eofPos = (root as CompilationUnitSyntax)?.EndOfFileToken.Position;
            if (position >= eofPos && eofPos != null)
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
                if (node!= null && node.XGenerated)
                    return LineVisibility.Hidden;
            }
            return LineVisibility.Visible;
        }

        private LinePosition GetXNodePosition(int position)
        {
            var text = this.GetText();
            var root = (CSharpSyntaxNode)GetRoot();
            var eof = ((root as CompilationUnitSyntax)?.EndOfFileToken.Node as InternalSyntax.SyntaxToken)?.XNode;
            var eofPos = (root as CompilationUnitSyntax)?.EndOfFileToken.Position;
            if (position >= eofPos && eofPos != null)
            {
                position = position - (eofPos ?? 0);
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
                SourceText ntext;
                if (!string.IsNullOrEmpty(file) && (root as CompilationUnitSyntax).IncludedFiles.TryGetValue(file, out ntext))
                    text = ntext;
            }
            return text.Lines.GetLinePosition(position);
        }

        private FileLinePositionSpan GetXNodeSpan(TextSpan span)
        {
            string file = this.FilePath;
            var text = this.GetText();
            var root = (CSharpSyntaxNode)GetRoot();
            var cs = root as CompilationUnitSyntax;
            var eof = (cs?.EndOfFileToken.Node as InternalSyntax.SyntaxToken)?.XNode;
            var eofPos = cs?.EndOfFileToken.Position;
            if (span.Start >= eofPos && eofPos != null)
            {
                var start = span.Start - (eofPos ?? 0);
                var length = span.Length;
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
                            SourceText ntext;
                            if (!string.IsNullOrEmpty(fn) && cs.IncludedFiles.TryGetValue(fn, out ntext))
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
                if (start+ length > text.Length)
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
                    var sn = (CSharpSyntaxNode)snode.ChildThatContainsPosition(span.Start);
                    if (sn == null || sn == snode) // no child found
                        break;
                    if (span.Start == sn.Position && span.Length > sn.FullWidth)
                    {
                        var en = (CSharpSyntaxNode)snode.ChildThatContainsPosition(span.End - 1);
                        if (en != null)
                        {
                            snode = sn;
                            break;
                        }
                    }
                    snode = sn;
                }
                var start = 0;
                var length = 0;
                string fn = file;
                // correct several incorrect breakpoint positions due to different order of things
                if (snode.Parent is VariableDeclarationSyntax)
                {
                    snode = snode.Parent;
                }
                snode = GetStatement(snode);
                if (snode.XNode != null)
                {
                    var xNode = snode.XNode as XSharpParserRuleContext;
                    start = xNode.Position ;
                    length = xNode.FullWidth ;
                    fn = xNode.SourceFileName;
                    if (xNode.SourceSymbol is XSharpToken symbol)
                    {
                        // for a define or UDC we want the location in the source and not in the #include 
                        start = symbol.StartIndex;
                        length = symbol.StopIndex - start + 1;
                        fn = symbol.InputStream.SourceName;
                    }
                }
                if (length < 0)
                    length = 0;
                SourceText ntext;
                if (!string.IsNullOrEmpty(fn) && cs.IncludedFiles.TryGetValue(fn, out ntext))
                {
                    text = ntext;
                    file = fn;
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
            var s = text.Lines.GetLinePosition(span.Start);
            var e = text.Lines.GetLinePosition(span.End);
            //System.Diagnostics.Debug.WriteLine($" {span.Start} {file} {s.Line}");
            return new FileLinePositionSpan(file, s, e);
        }
    }
}
