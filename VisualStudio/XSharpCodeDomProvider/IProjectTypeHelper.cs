using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using LanguageService.CodeAnalysis.XSharp;
namespace XSharp.CodeDom
{
    public interface IProjectTypeHelper
    {
        XSharpModel.XSourceTypeSymbol ResolveXType(string name, IList<string> usings);
        [Obsolete("This method is obsolete and does the same as ResolveXType")]
        XSharpModel.XSourceTypeSymbol ResolveReferencedType(string name, IList<string> usings);
        XSharpModel.IXTypeSymbol ResolveExternalType(string name, IList<string> usings);

        XSharpParseOptions ParseOptions { get; }
        string SynchronizeKeywordCase(string code, string fileName);
    }
}
