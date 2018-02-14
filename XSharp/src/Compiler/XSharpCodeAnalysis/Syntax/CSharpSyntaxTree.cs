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
        internal bool Generated { get; set; }
        private CSharpSyntaxNode GetNode(CSharpSyntaxNode root, int position)
        {
            if (root.XNode != null && position != 0)
            {
                var node = (CSharpSyntaxNode)GetRoot().ChildThatContainsPosition(position);
                while (!node.Green.IsToken && (position > node.Position || position < (node.Position + node.FullWidth)))
                {
                    var n = (CSharpSyntaxNode)node.ChildThatContainsPosition(position);
                    if (n == null || n == node)
                        break;
                    node = n;
                }
                return node;
            }
            return null;
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
                if (node != null && node.XNode != null && node.XNode.IsHidden)
                {
                    return LineVisibility.Hidden;
                }
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
                var enode = snode;
                while (!snode.Green.IsToken && (span.Start > snode.Position || span.Length < snode.FullWidth))
                {
                    var sn = (CSharpSyntaxNode)snode.ChildThatContainsPosition(span.Start);
                    if (sn == null || sn == snode)
                        break;
                    if (span.Start == sn.Position && span.Length > sn.FullWidth)
                    {
                        var en = (CSharpSyntaxNode)snode.ChildThatContainsPosition(span.End - 1);
                        if (en != null)
                        {
                            snode = sn;
                            enode = en;
                            break;
                        }
                    }
                    snode = sn;
                    enode = sn;
                }
                var start = 0;
                var length = 0;
                string fn = file;
                if (snode.XNode != null)
                {
                    var xNode = snode.XNode as XSharpParserRuleContext;
                    start = xNode.Position ;
                    length = xNode.FullWidth ;
                    fn = xNode.SourceFileName;
                    var symbol = xNode.SourceSymbol as XSharpToken;
                    if (symbol != null )
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
            return new FileLinePositionSpan(file, s, e);
        }
    }
}