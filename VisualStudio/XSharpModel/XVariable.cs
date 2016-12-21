using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharpModel
{
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
