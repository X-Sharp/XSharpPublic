// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Diagnostics;
using Microsoft.CodeAnalysis.Text;
#if XSHARP
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
#endif
namespace Microsoft.CodeAnalysis.CSharp
{
    /// <summary>
    /// An enumerator for diagnostic lists.
    /// </summary>
    internal struct SyntaxTreeDiagnosticEnumerator
    {
        private readonly SyntaxTree _syntaxTree;
        private NodeIterationStack _stack;
        private Diagnostic _current;
        private int _position;
        private const int DefaultStackCapacity = 8;
#if XSHARP
        private GreenNode _node;
#endif

        internal SyntaxTreeDiagnosticEnumerator(SyntaxTree syntaxTree, GreenNode node, int position)
        {
            _syntaxTree = null;
            _current = null;
            _position = position;
#if XSHARP
            _node = node;
#endif
            if (node != null && node.ContainsDiagnostics)
            {
                _syntaxTree = syntaxTree;
                _stack = new NodeIterationStack(DefaultStackCapacity);
                _stack.PushNodeOrToken(node);
            }
            else
            {
                _stack = new NodeIterationStack();
            }
        }

        /// <summary>
        /// Moves the enumerator to the next diagnostic instance in the diagnostic list.
        /// </summary>
        /// <returns>Returns true if enumerator moved to the next diagnostic, false if the
        /// enumerator was at the end of the diagnostic list.</returns>
        public bool MoveNext()
        {
            while (_stack.Any())
            {
                var diagIndex = _stack.Top.DiagnosticIndex;
                var node = _stack.Top.Node;
                var diags = node.GetDiagnostics();
                if (diagIndex < diags.Length - 1)
                {
                    diagIndex++;
                    var sdi = (SyntaxDiagnosticInfo)diags[diagIndex];

#if XSHARP && false
                    var length = ((CSharpSyntaxTree)_syntaxTree).GetRoot().XNode?.FullWidth ?? 0;
                    int spanStart = Math.Min(((CSharp.Syntax.InternalSyntax.CSharpSyntaxNode)node).XNode?.Position + sdi.Offset ?? 0, length);
                    var spanWidth = Math.Min(spanStart + sdi.Width, length) - spanStart;
                    Debug.WriteLine("Diag span {0} .. {1}", spanStart, spanWidth);
#else
                    //for tokens, we've already seen leading trivia on the stack, so we have to roll back
                    //for nodes, we have yet to see the leading trivia
                    int leadingWidthAlreadyCounted = node.IsToken ? node.GetLeadingTriviaWidth() : 0;

                    // don't produce locations outside of tree span
                    var length = _syntaxTree.GetRoot().FullSpan.Length;
                    var spanStart = Math.Min(_position - leadingWidthAlreadyCounted + sdi.Offset, length);
                    var spanWidth = Math.Min(spanStart + sdi.Width, length) - spanStart;
#endif

                    _current = new CSDiagnostic(sdi, new SourceLocation(_syntaxTree, new TextSpan(spanStart, spanWidth)));
#if XSHARP
                    var n = node as Syntax.InternalSyntax.CSharpSyntaxNode;
                    if (n.XNode != null)
                    {
                        _current = new CSDiagnostic(sdi, n.XNode.GetLocation());
                    }
#endif
                    _stack.UpdateDiagnosticIndexForStackTop(diagIndex);
                    return true;
                }

                var slotIndex = _stack.Top.SlotIndex;
            tryAgain:
                if (slotIndex < node.SlotCount - 1)
                {
                    slotIndex++;
                    var child = node.GetSlot(slotIndex);
                    if (child == null)
                    {
                        goto tryAgain;
                    }

                    if (!child.ContainsDiagnostics)
                    {
                        _position += child.FullWidth;
                        goto tryAgain;
                    }

                    _stack.UpdateSlotIndexForStackTop(slotIndex);
                    _stack.PushNodeOrToken(child);
                }
                else
                {
                    if (node.SlotCount == 0)
                    {
                        _position += node.Width;
                    }

                    _stack.Pop();
                }
            }

            return false;
        }

        /// <summary>
        /// The current diagnostic that the enumerator is pointing at.
        /// </summary>
        public Diagnostic Current
        {
            get { return _current; }
        }

        private struct NodeIteration
        {
            internal readonly GreenNode Node;
            internal int DiagnosticIndex;
            internal int SlotIndex;

            internal NodeIteration(GreenNode node)
            {
                this.Node = node;
                this.SlotIndex = -1;
                this.DiagnosticIndex = -1;
            }
        }

        private struct NodeIterationStack
        {
            private NodeIteration[] _stack;
            private int _count;

            internal NodeIterationStack(int capacity)
            {
                Debug.Assert(capacity > 0);
                _stack = new NodeIteration[capacity];
                _count = 0;
            }

            internal void PushNodeOrToken(GreenNode node)
            {
                var token = node as Syntax.InternalSyntax.SyntaxToken;
                if (token != null)
                {
                    PushToken(token);
                }
                else
                {
                    Push(node);
                }
            }

            private void PushToken(Syntax.InternalSyntax.SyntaxToken token)
            {
                var trailing = token.GetTrailingTrivia();
                if (trailing != null)
                {
                    this.Push(trailing);
                }

                this.Push(token);
                var leading = token.GetLeadingTrivia();
                if (leading != null)
                {
                    this.Push(leading);
                }
            }

            private void Push(GreenNode node)
            {
                if (_count >= _stack.Length)
                {
                    var tmp = new NodeIteration[_stack.Length * 2];
                    Array.Copy(_stack, tmp, _stack.Length);
                    _stack = tmp;
                }

                _stack[_count] = new NodeIteration(node);
                _count++;
            }

            internal void Pop()
            {
                _count--;
            }

            internal bool Any()
            {
                return _count > 0;
            }

            internal NodeIteration Top
            {
                get
                {
                    return this[_count - 1];
                }
            }

            internal NodeIteration this[int index]
            {
                get
                {
                    Debug.Assert(_stack != null);
                    Debug.Assert(index >= 0 && index < _count);
                    return _stack[index];
                }
            }

            internal void UpdateSlotIndexForStackTop(int slotIndex)
            {
                Debug.Assert(_stack != null);
                Debug.Assert(_count > 0);
                _stack[_count - 1].SlotIndex = slotIndex;
            }

            internal void UpdateDiagnosticIndexForStackTop(int diagnosticIndex)
            {
                Debug.Assert(_stack != null);
                Debug.Assert(_count > 0);
                _stack[_count - 1].DiagnosticIndex = diagnosticIndex;
            }
        }
    }

#if XSHARP
    internal static class XNodeExtensions
    {
        // Extension to get safe line/column numbers
        internal static LinePosition GetLinePosition (this Antlr4.Runtime.IToken token, bool start)
        {
           int iLine = token.Line   > 1 ? token.Line-1 : 0;  // note Antlr Line = 1 based
           int iCol  = token.Column > 0 ? token.Column : 0;  // note Antlr Column = 0 based
           if (! start)
           {
               iCol += token.Text.Length;
           }
           return new LinePosition(iLine, iCol);
        }
        internal static Location GetLocation(this IXParseTree node)
        {

            TextSpan span = new TextSpan(0, 1);
            LinePositionSpan lspan = new LinePositionSpan(new LinePosition(0, 0), new LinePosition(0, 1));
            if (node is XSharpParserRuleContext)
            {
                var rule = node as XSharpParserRuleContext;
                span = new TextSpan(rule.Start.StartIndex, rule.FullWidth);     // Note Antlr StartIndex = 0 based
                lspan = new LinePositionSpan(rule.Start.GetLinePosition(true), rule.Stop.GetLinePosition(false));
            }
            else if (node is XTerminalNodeImpl)
            {
                var term = node as XTerminalNodeImpl;
                var sym = term.Symbol;
                if (term.SourceSymbol != null)
                {
                    sym = term.SourceSymbol;
                }
                if (sym != null)
                {
                    span = new TextSpan(sym.StartIndex, sym.StopIndex - sym.StartIndex+1);  // note Antlr StartIndex = 0 based
                    lspan = new LinePositionSpan(sym.GetLinePosition(true),sym.GetLinePosition(false));
                }
                else
                {
                    span = new TextSpan(term.Position, term.FullWidth);     // Position = 0 based
                    // default lSpan is set above
                }
            }

            return new ExternalFileLocation(node.SourceFileName, span, lspan);

        }
    }
#endif
}
