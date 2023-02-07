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

        private readonly Stack<blockindent> _blocks;
        private int _firstDoccomment; // line number of start of doc comment block
        private readonly XDocument _document;
        private readonly XSharpLineKeywords _lineKeywords;
        private SourceCodeEditorSettings _settings;
        private int[] _expectedIndent;
        private int _indentSize;
        internal DocFormatter(XDocument document, SourceCodeEditorSettings settings)
        {
            _blocks = new Stack<blockindent>();
            _firstDoccomment = -1;
            _document = document;
            _lineKeywords = document.LineKeywords;
            _settings = settings;
        }

        private int IndentEntityStart(XKeyword kw, int lineNumber, int startIndent, bool singleLine)
        {
            while (_blocks.Count > 0)
            {
                var top = _blocks.Peek();
                if (!top.kw.IsEntity())
                {
                    _blocks.Pop();
                }
                else
                {
                    break;
                }
            }
            if (_blocks.Count > 0)
            {
                var block = _blocks.Peek();
                _indentSize = block.indent;
                if (_settings.IndentNamespace && block.kw.Kw2 == XTokenType.Namespace)
                {
                    _indentSize += 1;
                }
                if (_settings.IndentTypeMembers)
                {
                    bool isMember = kw.IsMember() && ! singleLine;
                    if ( isMember && _blocks.Peek().kw.IsType())
                    {
                        _indentSize += 1;
                    }
                    if (kw.IsAccessor() && _blocks.Peek().kw.IsMember())
                    {
                        _indentSize += 1;
                    }

                }

                if (_settings.IndentTypeFields)
                {
                    bool isMember = singleLine;
                    if (isMember && _blocks.Peek().kw.IsType())
                    {
                        _indentSize += 1;
                    }

                }

                // pop entities from the stack that are "Global"
                while (_blocks.Peek().kw.IsGlobal())
                {
                    _blocks.Pop();
                    if (_blocks.Count == 0)
                    {
                        break;
                    }
                }
            }
            else
            {
                _indentSize = startIndent;
            }
            _expectedIndent[lineNumber] = _indentSize;
            if (_firstDoccomment != -1)
            {
                for (int i = _firstDoccomment; i < lineNumber; i++)
                {
                    if (!_document.HasLineState(i, LineFlags.Preprocessor))
                    {
                        _expectedIndent[i] = _indentSize;
                    }
                }
            }
            //while (lineNumber > 0 && _document.HasLineState(lineNumber, LineFlags.Continued))
            //{
            //    lineNumber -= 1;
            //    _expectedIndent[lineNumber] = _indentSize;
            //}
            return _indentSize;
        }

        void HandleStart(XKeyword kw, int lineNumber)
        {
            var rule = XFormattingRule.GetFirstRuleByStart(kw);
            var isEntity = rule.Flags.HasFlag(XFormattingFlags.Type) ||
                rule.Flags.HasFlag(XFormattingFlags.Member);

            _blocks.Push(new blockindent(kw, _indentSize, lineNumber));
            if (isEntity)
            {
                // Only indent statements when the option is enabled
                if (_settings.IndentStatements)
                    _indentSize += 1;
            }
            else
            {
                _indentSize += 1;
            }
        }

        void HandleMiddle(XKeyword kw, int lineNumber)
        {
            // This does not change the indent size
            // nut only adjusts the indent size for the current line

            _expectedIndent[lineNumber] = _indentSize;
            if (_blocks.Count > 0)
            {
                var top = _blocks.Peek();
                _indentSize = top.indent;
                if (kw.Kw1 == XTokenType.Case || kw.Kw1 == XTokenType.Otherwise)
                {
                    if (_settings.IndentCaseLabel)
                        _indentSize += 1;
                    _expectedIndent[lineNumber] = _indentSize;
                    if (_settings.IndentCaseContent)
                        _indentSize += 1;
                }
                else
                {
                    _expectedIndent[lineNumber] = _indentSize;
                    _indentSize += 1;
                }
            }
        }
        void HandleEnd(XKeyword kw, int lineNumber)
        {
            var isType = false;
            _indentSize -= 1;
            _expectedIndent[lineNumber] = _indentSize;
            var rules = XFormattingRule.GetEndRules(kw);
            if (rules?.Count > 0)
            {
                var rule = rules.First();
                isType = rule.Flags.IsType();
            }
            if (isType)
            {
                while (_blocks.Count > 0 && !_blocks.Peek().kw.IsType())
                {
                    _blocks.Pop();
                }
                if (_blocks.Count > 0)
                {
                    var top = _blocks.Pop();
                    _indentSize = top.indent;
                    _expectedIndent[lineNumber] = _indentSize;
                }
            }
            else
            {
                if (_blocks.Count > 0)
                {
                    var top = _blocks.Pop();
                    _indentSize = top.indent;
                    _expectedIndent[lineNumber] = _indentSize;
                }
            }

        }

        internal int[] GetIndentSizes(IEnumerable<ITextSnapshotLine> lines, int startLine, int endLine, int startIndent)
        {
            _expectedIndent = new int[endLine + 1];
            _indentSize = startIndent;
            ;
            foreach (var line in lines)
            {
                var lineNumber = line.LineNumber;
                if (lineNumber >= startLine && lineNumber <= endLine)
                {
                    if (_document.HasLineState(lineNumber, LineFlags.Preprocessor))
                    {
                        if (!_settings.IndentPreprocessorLines)
                        {
                            _expectedIndent[lineNumber] = 0;
                            continue;
                        }
                    }
                    if (_document.HasLineState(lineNumber, LineFlags.DocComments))
                    {
                        if (_firstDoccomment == -1)
                            _firstDoccomment = lineNumber;
                        continue;

                    }
                    // Class variables are not easily recognizable.
                    // Check the SingleLineEntity flag for them
                    var singleLineEntityStart = _document.HasLineState(lineNumber, LineFlags.SingleLineEntity);
                    _expectedIndent[lineNumber] = _indentSize;
                    if (!singleLineEntityStart && !_lineKeywords.ContainsKey(lineNumber) )
                    {
                        // check for continuation
                        if (_settings.IndentContinuedLines && _document.HasLineState(lineNumber, LineFlags.IsContinued))
                        {
                            _expectedIndent[lineNumber] = _indentSize + 1;
                        }
                        continue;
                    }
                    _lineKeywords.Get(lineNumber, out var kw);

                    if (kw.IsEntity() || singleLineEntityStart)
                    {
                        _indentSize = IndentEntityStart(kw, lineNumber, startIndent, singleLineEntityStart);
                        _firstDoccomment = -1;
                    }
                    if (kw.IsStart())
                    {
                        HandleStart(kw, lineNumber);
                    }
                    else if (kw.IsMiddle())
                    {
                        HandleMiddle(kw, lineNumber);
                    }
                    else if (kw.IsStop())
                    {
                        HandleEnd(kw, lineNumber);
                    }
                }
            }
            return _expectedIndent;
        }
    }
}
