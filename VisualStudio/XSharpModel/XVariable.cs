using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using LanguageService.CodeAnalysis.Text;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using LanguageService.SyntaxTree;
using LanguageService.SyntaxTree.Misc;
using LanguageService.SyntaxTree.Tree;
namespace XSharpModel
{
    [DebuggerDisplay("{Prototype,nq}")]
    public class XVariable : XElement
    {
        private String _typeName;

        public XVariable(XElement parent, string name, Kind kind, Modifiers visibility, TextRange span, TextInterval position, string typeName)
            : base(name, kind, Modifiers.None, visibility, span, position)
        {
            _typeName = typeName;
            this.Parent = parent;
        }

        public string TypeName
        {
            get
            {
                return _typeName;
            }

            set
            {
                _typeName = value;
            }
        }

        public String Description
        {
            get
            {
                return this.Prototype + " as " + this.TypeName;
            }
        }

        public String Prototype
        {
            get
            {
                return this.Name;
            }
        }

    }
}
