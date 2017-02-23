using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharpModel
{
    public interface IXSharpProject
    {

        string RootNameSpace { get; }
        string Url { get; }
        void SetStatusBarText(string message);
        void OpenElement(string file, int line, int column);
    }
}
