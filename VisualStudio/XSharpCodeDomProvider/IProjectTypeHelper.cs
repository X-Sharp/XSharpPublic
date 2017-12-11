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
        System.Type ResolveType(string name, IReadOnlyList<string> usings);
        XSharpModel.XType ResolveXType(string name, IReadOnlyList<string> usings);
        XSharpModel.XType ResolveReferencedType(string name, IReadOnlyList<string> usings);
        //EnvDTE.CodeElement ResolveStrangerType(string name, IReadOnlyList<string> usings);

        XSharpParseOptions ParseOptions { get; }
    }
}
