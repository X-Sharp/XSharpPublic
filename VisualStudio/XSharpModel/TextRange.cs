//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using LanguageService.SyntaxTree;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Diagnostics;
namespace XSharpModel
{
    /// <summary>
    /// 1 based TextRange
    /// </summary>
    [DebuggerDisplay("{StartLine}.{StartColumn}-{EndLine}.{EndColumn}")]
    public struct TextRange
    {
        private readonly int _StartLine;
        private readonly int _StartColumn;
        private readonly int _EndLine;
        private readonly int _EndColumn;

        public static TextRange Empty
        {
            get
            {
                return new TextRange(1, 1, 1, 1);
            }
        }

        public TextRange(ParserRuleContext context)
            : this(context.Start.Line, context.Start.Column,
                                            context.Stop.Line, context.Stop.Column)
        {

        }

        public TextRange(int sl, int sc, int el, int ec)
        {
            _StartLine = sl;
            _StartColumn = sc;
            _EndLine = el;
            _EndColumn = ec;
        }

        /// <summary>
        /// 1 based Start Line
        /// </summary>
        public int StartLine => _StartLine;
        /// <summary>
        /// 1 based End Line
        /// </summary>
        public int EndLine => _EndLine;
        /// <summary>
        /// 1 based Start Column
        /// </summary>
        public int StartColumn => _StartColumn;
        /// <summary>
        /// 1 based End Column
        /// </summary>
        public int EndColumn => _EndColumn;

        public bool ContainsExclusive(int line, int col)
        {
            if ((line > this._StartLine) && (line < this._EndLine))
            {
                return true;
            }
            if (line == this._StartLine)
            {
                if (col > this._StartColumn)
                {
                    if (line < this._EndLine)
                    {
                        return true;
                    }
                    if (line == this._EndLine)
                    {
                        return (col < this._EndColumn);
                    }
                }
                return false;
            }
            return ((line == this._EndLine) && (col < this._EndColumn));
        }

        public bool ContainsInclusive(int line, int col)
        {
            if ((line > this._StartLine) && (line < this._EndLine))
            {
                return true;
            }
            if (line == this._StartLine)
            {
                if (col >= this._StartColumn)
                {
                    if (line < this._EndLine)
                    {
                        return true;
                    }
                    if (line == this._EndLine)
                    {
                        return (col <= this._EndColumn);
                    }
                }
                return false;
            }
            return ((line == this._EndLine) && (col <= this._EndColumn));
        }


    }
    /// <summary>
    /// 0 based Text Interval
    /// </summary>
    [DebuggerDisplay("{Start}-{Stop}")]
    public struct TextInterval
    {
        private readonly int _StartIndex;
        private readonly int _StopIndex;

        public static TextInterval Empty
        {
            get
            {
                return new TextInterval();
            }
        }

        public TextInterval(ParserRuleContext context) :
            this(context.Start.StartIndex, context.Stop.StopIndex)
        {
        }

        public TextInterval(int start, int stop)
        {
            _StartIndex = start;
            _StopIndex = stop;
        }

        public bool IsEmpty() => _StartIndex == 0 && _StopIndex == 0;
        /// <summary>
        /// 0 based StartIndex
        /// </summary>
        public int Start => _StartIndex;

        /// <summary>
        /// 0 based StopIndex
        /// </summary>
        public int Stop => _StopIndex;

        public int Width => _StopIndex - _StartIndex + 1;

        public bool ContainsInclusive(int position)
        {
            if ((position >= this._StartIndex) && (position <= this._StopIndex))
            {
                return true;
            }
            return false;
        }

        public bool ContainsExclusive(int position)
        {
            if ((position > this._StartIndex) && (position < this._StopIndex))
            {
                return true;
            }
            return false;
        }
    }
}