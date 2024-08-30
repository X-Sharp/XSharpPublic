using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using LanguageService.CodeAnalysis.XSharp;
using XSharpModel;
namespace XSharp.CodeDom
{
    public interface IProjectTypeHelper
    {
        XSharpModel.XSourceTypeSymbol ResolveXType(string name, IList<string> usings);
        [Obsolete("This method is obsolete and does the same as ResolveXType")]
        XSharpModel.XSourceTypeSymbol ResolveReferencedType(string name, IList<string> usings);
        XSharpModel.IXTypeSymbol ResolveExternalType(string name, IList<string> usings);

        XParseOptions ParseOptions { get; }
    }
}
