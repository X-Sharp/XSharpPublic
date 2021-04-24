using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Diagnostics;

namespace XSharp.MacroCompiler
{
    public struct SourceLocation
    {
        int Offset;
        public int Line;
        public int Col;
        public string FileName;
        internal bool Valid { get { return Offset >= 0; } }

        internal static readonly SourceLocation None = new SourceLocation(-1);

        public SourceLocation(int offset)
        {
            FileName = null;
            Offset = offset;
            Line = 0;
            Col = 0;
        }

        public SourceLocation(string source, SourceLocation loc) : this(source, loc.Offset) { }

        public SourceLocation(string source, int offset)
        {
            FileName = null;
            Offset = offset;
            if (source != null)
            {
                Line = 1;
                Col = Offset + 1;
                bool ecr = false;
                for (int i = 0; i < Offset; i++)
                {
                    if (i >= source.Length)
                        break;
                    bool cr = source[i] == '\r';
                    bool lf = source[i] == '\n';
                    if (ecr && lf)
                    {
                        Col = Offset - i;
                    }
                    else if (cr || lf)
                    {
                        Line += 1;
                        Col = Offset - i;
                    }
                    ecr = cr;
                }
            }
            else
            {
                Line = 0;
                Col = 0;
            }
        }

        public override string ToString()
        {
            if (Valid)
            {
                if (FileName != null)
                    return String.Format("{0}:({1},{2})", FileName, Line, Col);
                return String.Format("({0},{1})", Line, Col);
            }
            return null;
        }
    }
}
