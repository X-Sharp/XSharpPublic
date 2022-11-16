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
        Namespaces = 1 << 0,
        Types = 1 << 1,
        Interfaces = 1 << 2,
        StaticMembers = 1 << 3,
        InstanceMembers = 1 << 4,
        Members = StaticMembers | InstanceMembers,
        Constructors = 1 << 5,
        Brackets = 1 << 6,
        Keywords = 1 << 7,
        Snippets = 1 << 8,
        General = 1 << 9,
        Inherit = 1 << 10,
    }
}
