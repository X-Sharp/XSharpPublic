using Microsoft.VisualStudio.Text;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using XSharpModel;

namespace XSharp.LanguageService
{
    class DocFormatter
    {
        [DebuggerDisplay("{kw} {indent}")]
        internal struct blockindent
        {
            internal XKeyword kw;
            internal int indent;
            internal int line;
            internal blockindent(XKeyword parkw, int parindent, int line)
            {
                kw = parkw;
                indent = parindent;
                this.line = line;
            }
        }

        private readonly Stack<blockindent> blocks;
        private int firstDoccomment; // line number of start of doc comment block
        private readonly XDocument document;
        private readonly XSharpLineKeywords lineKeywords;
        private SourceCodeEditorSettings _settings;
        private int[] expectedIndent;
        private int indentSize;
        internal DocFormatter(XDocument document, SourceCodeEditorSettings settings)
        {
            blocks = new Stack<blockindent>();
            firstDoccomment = -1;
            this.document = document;
            lineKeywords = document.LineKeywords;
            _settings = settings;
        }

        private int IndentEntityStart(XKeyword kw, int lineNumber, int startIndent, bool singleLine)
        {
            while (blocks.Count > 0)
            {
                var top = blocks.Peek();
                if (!XFormattingRule.IsEntity(top.kw))
                {
                    blocks.Pop();
                }
                else
                {
                    break;
                }
            }
            if (blocks.Count > 0)
            {
                var block = blocks.Peek();
                indentSize = block.indent;
                if (_settings.IndentNamespace && block.kw.Kw2 == XTokenType.Namespace)
                {
                    indentSize += 1;
                }
                if (_settings.IndentTypeMembers)
                {
                    bool isMember = XFormattingRule.IsMember(kw) || singleLine;
                    if ( isMember && XFormattingRule.IsType(blocks.Peek().kw))
                    {
                        indentSize += 1;
                    }

                }

                // pop entities from the stack that are "Global"
                while (XFormattingRule.IsGlobalEntity(blocks.Peek().kw))
                {
                    blocks.Pop();
                    if (blocks.Count == 0)
                    {
                        break;
                    }
                }
            }
            else
            {
                indentSize = startIndent;
            }
            expectedIndent[lineNumber] = indentSize;
            if (firstDoccomment != -1)
            {
                for (int i = firstDoccomment; i < lineNumber; i++)
                {
                    if (!document.HasLineState(i, LineFlags.Preprocessor))
                    {
                        expectedIndent[i] = indentSize;
                    }
                }
            }
            while (lineNumber > 0 && document.HasLineState(lineNumber, LineFlags.Continued))
            {
                lineNumber -= 1;
                expectedIndent[lineNumber] = indentSize;
            }
            return indentSize;
        }

        void HandleStart(XKeyword kw, int lineNumber)
        {
            var rule = XFormattingRule.GetFirstRuleByStart(kw);
            var isEntity = rule.Flags.HasFlag(XFormattingFlags.Type) ||
                rule.Flags.HasFlag(XFormattingFlags.Member);

            blocks.Push(new blockindent(kw, indentSize, lineNumber));
            if (isEntity)
            {
                // Only indent statements when the option is enabled
                if (_settings.IndentStatements)
                    indentSize += 1;
            }
            else
            {
                indentSize += 1;
            }
        }

        void HandleMiddle(XKeyword kw, int lineNumber)
        {
            // This does not change the indent size
            // nut only adjusts the indent size for the current line

            expectedIndent[lineNumber] = indentSize;
            if (blocks.Count > 0)
            {
                var top = blocks.Peek();
                indentSize = top.indent;
                if (kw.Kw1 == XTokenType.Case || kw.Kw1 == XTokenType.Otherwise)
                {
                    if (_settings.IndentCaseLabel)
                        indentSize += 1;
                    expectedIndent[lineNumber] = indentSize;
                    if (_settings.IndentCaseContent)
                        indentSize += 1;
                }
                else
                {
                    expectedIndent[lineNumber] = indentSize;
                    indentSize += 1;
                }
            }
        }
        void HandleEnd(XKeyword kw, int lineNumber)
        {
            var isType = false;
            indentSize -= 1;
            expectedIndent[lineNumber] = indentSize;
            var rules = XFormattingRule.GetEndRules(kw);
            if (rules?.Count > 0)
            {
                var rule = rules.First();
                isType = rule.Flags.IsType();
            }
            if (isType)
            {
                while (blocks.Count > 0 && !XFormattingRule.IsType(blocks.Peek().kw))
                {
                    blocks.Pop();
                }
                if (blocks.Count > 0)
                {
                    var top = blocks.Pop();
                    indentSize = top.indent;
                    expectedIndent[lineNumber] = indentSize;
                }
            }
            else
            {
                if (blocks.Count > 0)
                {
                    var top = blocks.Pop();
                    indentSize = top.indent;
                    expectedIndent[lineNumber] = indentSize;
                }
            }

        }
      
        internal int[] GetIndentSizes(IEnumerable<ITextSnapshotLine> lines, int startLine, int endLine, int startIndent)
        {
            expectedIndent = new int[endLine + 1];
            indentSize = startIndent;
            ;
            foreach (var line in lines)
            {
                var lineNumber = line.LineNumber;
                if (lineNumber >= startLine && lineNumber <= endLine)
                {
                    if (document.HasLineState(lineNumber, LineFlags.Preprocessor))
                    {
                        if (!_settings.IndentPreprocessorLines)
                        {
                            expectedIndent[lineNumber] = 0;
                            continue;
                        }
                    }
                    if (document.HasLineState(lineNumber, LineFlags.DocComments))
                    {
                        if (firstDoccomment == -1)
                            firstDoccomment = lineNumber;
                        continue;

                    }
                    // Class variables are not easily recognizable.
                    // Check the SingleLineEntity flag for them
                    var singleLineEntityStart = document.HasLineState(lineNumber, LineFlags.SingleLineEntity);
                    expectedIndent[lineNumber] = indentSize;
                    if (!singleLineEntityStart && !lineKeywords.ContainsKey(lineNumber) )
                    {
                        // check for continuation
                        if (_settings.IndentContinuedLines && document.HasLineState(lineNumber, LineFlags.Continued))
                        {
                            expectedIndent[lineNumber] = indentSize + 1;
                        }
                        continue;
                    }
                    lineKeywords.Get(lineNumber, out var kw);

                    if (XFormattingRule.IsEntity(kw) || singleLineEntityStart)
                    {
                        indentSize = IndentEntityStart(kw, lineNumber, startIndent, singleLineEntityStart);
                        firstDoccomment = -1;
                    }
                    if (XFormattingRule.IsStartKeyword(kw))
                    {
                        HandleStart(kw, lineNumber);
                    }
                    else if (XFormattingRule.IsMiddleKeyword(kw))
                    {
                        HandleMiddle(kw, lineNumber);
                    }
                    else if (XFormattingRule.IsEndKeyword(kw))
                    {
                        HandleEnd(kw, lineNumber);
                    }
                }
            }
            return expectedIndent;
        }
    }
}
