using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharp.Debugger.UI
{
    internal interface IDebuggerToolWindow
    {
        void Refresh();
        void Clear();
    }
}
