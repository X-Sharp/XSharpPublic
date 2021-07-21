using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharp.LanguageService
{
    [Flags]
    public enum CompletionState
    {
        None = 0,
        General = 1 << 0,
        Namespaces = 1 << 1,
        Types = 1 << 2,
        Interfaces = 1 << 3,
        StaticMembers = 1 << 4,
        InstanceMembers = 1 << 5,
        Constructors = 1 << 6,
        Brackets = 1 << 7,
        Keywords=1 << 8,
        Snippets=1 << 9,
    }
}
