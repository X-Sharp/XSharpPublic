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
        XSharpModel.XTypeDefinition ResolveXType(string name, IReadOnlyList<string> usings);
        [Obsolete("This method is obsolete and does the same as ResolveXType")]
        XSharpModel.XTypeDefinition ResolveReferencedType(string name, IReadOnlyList<string> usings);
        XSharpModel.IXType ResolveExternalType(string name, IReadOnlyList<string> usings);

        XSharpParseOptions ParseOptions { get; }
        string SynchronizeKeywordCase(string code, string fileName);
    }
}
