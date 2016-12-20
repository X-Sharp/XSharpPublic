using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharpModel
{
    public struct TextRange
    {
        private int _StartLine;
        private int _StartColumn;
        private int _EndLine;
        private int _EndColumn;


        public TextRange(int sl, int sc, int el, int ec)
        {
            _StartLine = sl;
            _StartColumn = sc;
            _EndLine = el;
            _EndColumn = ec;
        }

        public int StartLine
        {
            get
            {
                return _StartLine;
            }
        }

        public int EndLine
        {
            get
            {
                return _EndLine;
            }
        }

        public int StartColumn
        {
            get
            {
                return _StartColumn;
            }
        }

        public int EndColumn
        {
            get
            {
                return _EndColumn;
            }
        }

        public bool ContainsExclusive( int line, int col)
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

        public bool ContainsInclusive( int line, int col)
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

    public struct TextInterval
    {
        private int _StartIndex;
        private int _StopIndex;


        public TextInterval(int start, int stop)
        {
            _StartIndex = start;
            _StopIndex = stop;
        }

        public int Start
        {
            get
            {
                return _StartIndex;
            }
        }

        public int Stop
        {
            get
            {
                return _StopIndex;
            }
        }

        public int Width
        {
            get
            {
                return (_StopIndex - _StartIndex) + 1;
            }
        }

        public bool ContainsInclusive(int position )
        {
            if ((position >= this._StartIndex) && (position <= this._StopIndex ))
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