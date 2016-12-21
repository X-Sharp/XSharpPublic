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

    /// <summary>
    /// Model for Class or a Structure
    /// </summary>
    [DebuggerDisplay("{FullName:nq}")]
    public class XType : XElement
    {
        private List<XTypeMember> _members;
        private string _nameSpace;
        private bool _isPartial;
        private bool _isStatic;
        private string _parentName;

        public XType(string name, Kind kind, Modifiers modifiers, Modifiers visibility, TextRange span, TextInterval position)
            : base(name, kind, modifiers, visibility, span, position)
        {
            _members = new List<XTypeMember>();
            _nameSpace = "";
            if (modifiers.HasFlag(Modifiers.Static))
            {
                this._isStatic = true;
            }
            if (modifiers.HasFlag( Modifiers.Partial))
            {
                this._isPartial = true;
            }
        }

        public List<XTypeMember> Members
        {
            get
            {
                return _members;
            }

        }

        override public string FullName
        {
            get
            {
                if (!String.IsNullOrEmpty(_nameSpace))
                {

                    return this.NameSpace + "." + this.Name;
                }
                else
                {
                    return this.Name;
                }
            }
        }

        public string NameSpace
        {
            get
            {
                return _nameSpace;
            }

            set
            {
                _nameSpace = value;
            }
        }



        public bool IsPartial
        {
            get
            {
                return _isPartial;
            }

            set
            {
                _isPartial = value;
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


        /// <summary>
        /// Duplicate the current Object, so we have the same properties in another object
        /// </summary>
        /// <returns></returns>
        public XType Duplicate()
        {
            XType temp = new XType(this.Name, this.Kind, this.Modifiers, this.Visibility, this.Range, this.Interval);
            temp.IsPartial = this.IsPartial;
            temp.IsStatic = this.IsStatic;
            temp.File = new XFile(this.File.FullPath);
            foreach (XTypeMember mbr in this.Members)
            {
                temp.Members.Add(mbr);
            }
            //
            return temp;
        }


        /// <summary>
        /// Merge two XType Objects : Used to create the resulting  XType from partial classes
        /// </summary>
        /// <param name="otherType"></param>
        public void Merge(XType otherType)
        {
            this.IsPartial = true;
            if (otherType != null)
            {
                foreach (XTypeMember mbr in otherType.Members)
                {
                    this.Members.Add(mbr);
                }
                if ((this.Parent == null) && (otherType.Parent != null))
                {
                    this.Parent = otherType.Parent;
                }
                else if ((this.ParentName == null) && (otherType.ParentName != null))
                {
                    this.ParentName = otherType.ParentName;
                }
            }
            //
            return;
        }

        new public String ParentName
        {
            get
            {
                if (this.Parent != null)
                {
                    return this.Parent.Name;
                }
                else if ( this._parentName != null )
                {
                    return this._parentName;
                }
                return null;
            }

            set
            {
                if (this.Parent != null)
                {
                    throw new Exception( "Cannot ParentName if Parent is not null" );
                }
                this._parentName = value;
            }
        }

        public static XType CreateGlobalType()
        {
            return new XType("(Global Scope)", Kind.Class, Modifiers.Partial|Modifiers.Static, Modifiers.Public, new TextRange(), new TextInterval());
        }
    }
}
