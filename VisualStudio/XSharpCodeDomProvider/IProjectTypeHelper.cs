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
        System.Type ResolveType(string name, IList<string> usings);
        XSharpParseOptions ParseOptions { get; }
    }
}
