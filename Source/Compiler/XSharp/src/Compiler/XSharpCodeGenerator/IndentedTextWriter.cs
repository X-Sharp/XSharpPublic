using System;
using System.CodeDom.Compiler;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharp.CodeDom
{
    internal class XSharpIndentedTextWriter : IndentedTextWriter
    {
        internal bool SuppressNewLine = false;
        internal XSharpIndentedTextWriter(TextWriter writer, string tabString) : base(writer, tabString)
        {

        }
        protected override void OutputTabs()
        {
            base.OutputTabs();
        }
        public override void Write(string s)
        {
            base.Write(s);
        }
        public override void WriteLine(string s)
        {
            base.WriteLine(s);
        }
        public override void WriteLine()
        {
            if (!SuppressNewLine)
                base.WriteLine();
            else
                SuppressNewLine = false;
        }

    }
}
