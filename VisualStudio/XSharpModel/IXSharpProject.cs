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

        void SetStatusBarText(string message);
    }
}
