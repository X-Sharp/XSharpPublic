//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
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
    /// Model for Namespace, Class, Interface, Structure, Enum
    /// </summary>
    [DebuggerDisplay("{FullName,nq}")]
    public class XType : XElement
    {
        private XTypeMemberList _members;
        private string _nameSpace;
        private bool _isPartial;
        private bool _isStatic;
        private string _parentName;

        public XType(string name, Kind kind, Modifiers modifiers, Modifiers visibility, TextRange span, TextInterval position)
            : base(name, kind, modifiers, visibility, span, position)
        {
            _members = new XTypeMemberList();
            _parentName = "System.Object";
            _nameSpace = "";
            if (modifiers.HasFlag(Modifiers.Static))
            {
                this._isStatic = true;
            }
            if (modifiers.HasFlag(Modifiers.Partial))
            {
                this._isPartial = true;
            }
        }

        public XTypeMemberList Members
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
            temp.Parent = this.Parent;
            temp.ParentName = this.ParentName;
            temp.IsPartial = this.IsPartial;
            temp.IsStatic = this.IsStatic;
            temp.File = this.File;
            temp.Members.AddRange(this.Members);
            //
            return temp;
        }

        /// <summary>
        /// If this XType is a Partial type, return a Copy of it, merged with all other informations
        /// coming from other files.
        /// </summary>
        public XType Clone
        {
            get
            {
                if (this.IsPartial)
                    return this.File.Project.LookupFullName(this.FullName, true);
                else
                    return this;
            }
        }

        /// <summary>
        /// Merge two XType Objects : Used to create the resulting  XType from partial classes
        /// </summary>
        /// <param name="otherType"></param>
        public XType Merge(XType otherType)
        {
            var clone = this.Duplicate();
            this.IsPartial = true;
            if (otherType != null)
            {
                clone.Members.AddRange(otherType.Members);
                if ((clone.Parent == null) && (otherType.Parent != null))
                {
                    clone.Parent = otherType.Parent;
                }
                else if ((clone.ParentName == null) && (otherType.ParentName != null))
                {
                    clone.ParentName = otherType.ParentName;
                }
            }
            //
            return clone;
        }

        public override String ParentName
        {
            get
            {
                if (this.Parent != null)
                {
                    return this.Parent.Name;
                }
                else if (this._parentName != null)
                {
                    return this._parentName;
                }
                return null;
            }

            set
            {
                if (this.Parent != null)
                {
                    throw new Exception("Cannot set ParentName if Parent is not null");
                }
                this._parentName = value;
            }
        }

        public override String Description
        {
            get
            {
                String modVis = "";
                if (this.Kind == Kind.Class)
                {
                    if (this.Modifiers != Modifiers.None)
                    {
                        modVis += this.Modifiers.ToString() + " ";
                    }
                    modVis += this.Visibility.ToString() + " ";
                }
                //
                String desc = modVis;
                //
                if (this.Kind == Kind.Keyword)
                {
                    desc = this.Name + " " + this.Kind.ToString();
                }
                else
                {
                    desc += this.Kind.ToString() + " ";
                    desc += this.Prototype;
                }
                //
                return desc;
            }
        }

        public static XType CreateGlobalType(XFile file)
        {
            XType globalType = new XType(XType.GlobalName, Kind.Class, Modifiers.None, Modifiers.Public, new TextRange(1, 1, 1, 1), new TextInterval());
            globalType.IsPartial = true;
            globalType.IsStatic = true;
            globalType.File = file;
            return globalType;
        }

        public const String GlobalName = "(Global Scope)";
    }
}
