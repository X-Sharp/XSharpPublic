using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using EnvDTE;
using LanguageService.CodeAnalysis.Text;
using System.Diagnostics;

namespace XSharpModel
{
    [DebuggerDisplay("{Prototype,nq}")]
    public class XTypeMember : XElement
    {

        private String _typeName;
        private List<XVariable> _parameters;
        private List<XVariable> _locals;
        private bool _isStatic;

        public XTypeMember(string name, Kind kind, Modifiers modifiers, Modifiers visibility, TextRange span, TextInterval position ) 
            : base(name, kind, modifiers, visibility, span, position)
        {
            this.Parent = null;
            this._parameters = new List<XVariable>();
            this._locals = new List<XVariable>();
            this._typeName = "";
            if ( modifiers == Modifiers.Static )
            {
                this._isStatic = true;
            }
        }

        public XTypeMember(string name, Kind kind, Modifiers modifiers, Modifiers visibility, TextRange span, TextInterval position, string typeName )
            : this(name, kind, modifiers, visibility, span, position)
        {
            _typeName = typeName;
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

        public bool IsStatic
        {
            get
            {
                return _isStatic;
            }

            set
            {
                _isStatic = value;
            }
        }

        new public XType Parent
        {
            get
            {
                return (XType)base.Parent;
            }

            set
            {
                base.Parent = value;
            }
        }

        public List<XVariable> Parameters
        {
            get
            {
                return _parameters;
            }

            set
            {
                _parameters = value;
            }
        }

        override public string FullName
        {
            get
            {
                if ( this.Parent != null )
                {

                    return this.Parent.FullName + "." + this.Name;
                }
                else
                {
                    return this.Name;
                }
            }
        }

        public override String Description
        {
            get
            {
                String modVis = "";
                if (this.Modifiers != Modifiers.None)
                {
                    modVis += this.Modifiers.ToString() + " ";
                }
                modVis += this.Visibility.ToString() + " ";
                //
                String desc = modVis;
                //
                if ( this.Kind != Kind.ClassVar )
                    desc += this.Kind.ToString() + " ";
                desc += this.Prototype;
                //
                if ((this.Kind == Kind.Method) || (this.Kind == Kind.Function) || (this.Kind == Kind.Property ) || (this.Kind == Kind.ClassVar) || (this.Kind == Kind.Event))
                {
                    desc += " as " + this.TypeName;
                }
                //
                return desc;
            }
        }

        public override String Prototype
        {
            get
            {
                if ( (this.Kind == Kind.Property) || 
                    (this.Kind == Kind.Access) || 
                    (this.Kind == Kind.ClassVar) ||
                    (this.Kind == Kind.EnumMember) ||
                    (this.Kind == Kind.Event))
                    return this.Name;
                //
                String vars = "";
                foreach (XVariable var in this.Parameters)
                {
                    if (vars.Length > 0)
                        vars += ", ";
                    vars += var.Name + " as " + var.TypeName;
                }
                //
                String desc = "";
                desc += this.Name;
                desc += "(";
                desc += vars;
                desc += ")";
                //

                return desc;
            }
        }

        public List<XVariable> Locals
        {
            get
            {
                return _locals;
            }

            set
            {
                _locals = value;
            }
        }
    }
}
