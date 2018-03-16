//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Diagnostics;

namespace XSharpModel
{
    [DebuggerDisplay("{Prototype,nq}")]
    public class XTypeMember : XElement
    {
        public string Suffix { get; set; }
        private string _typeName;
        private List<XVariable> _parameters;
        private List<XVariable> _locals;

        public XTypeMember(string name, Kind kind, Modifiers modifiers, Modifiers visibility, TextRange span, TextInterval position, bool isStatic ) 
            : base(name, kind, modifiers, visibility, span, position)
        {
            this.Parent = null;
            this._parameters = new List<XVariable>();
            this._locals = new List<XVariable>();
            this._typeName = "";
            this._isStatic = isStatic;

        }

        public XTypeMember(string name, Kind kind, Modifiers modifiers, Modifiers visibility, TextRange span, TextInterval position, string typeName, bool isStatic )
            : this(name, kind, modifiers, visibility, span, position, isStatic)
        {
            _typeName = typeName;
        }

        public string TypeName
        {
            get
            {
                return _typeName;
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

        public bool IsArray { get; set; }
        public override string Description
        {
            get
            {
                string modVis = "";
                if (this.Modifiers != Modifiers.None)
                {
                    modVis += this.Modifiers.ToString() + " ";
                }
                modVis += this.Visibility.ToString() + " ";
                //
                string desc = modVis;
                //
                if ( this.Kind != Kind.Field)
                {
                    if (this.Kind == Kind.VODefine)
                    {
                        desc += "DEFINE" + " "+this.Name+this.Suffix;
                        return desc;
                    }
                    else if (this.Kind == Kind.VOGlobal)
                    {
                        desc += "GLOBAL" + " ";
                    }
                    else
                    {
                        desc += this.Kind.ToString() + " ";
                    }
                }
                desc += this.Prototype;
                //
                return desc;
            }
        }

        public override string Prototype
        {
            get
            {
                string vars = "";
                if ( this.Kind.HasParameters())
                {
                    vars = "(";
                    foreach (XVariable var in this.Parameters)
                    {
                        if (vars.Length > 1)
                            vars += ", ";
                        vars += var.Name + " as " + var.TypeName;
                    }
                    vars += ")";
                }
                //
                string desc = this.Name;
                desc += vars;
                //
                if (this.Kind.HasReturnType())
                {
                    desc += " AS " + this.TypeName;
                }
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
        }


        /// <summary>
        /// Fill a List of XTypeMember with Homonyms of the current one
        /// </summary>
        /// <returns></returns>
        public List<XTypeMember> Namesake()
        {
            List<XTypeMember> _namesake = new List<XTypeMember>();
            if ( Parent != null )
            {
                //Search in the parent members
                foreach( var member in Parent.Members )
                {
                    // For Homonyms
                    if ( string.Compare( member.FullName, this.FullName, true ) == 0 )
                    {
                        // But don't add the current one
                        if ( string.Compare( member.Prototype, this.Prototype, true) !=0 )
                        {
                            _namesake.Add(member);
                        }
                    }
                }
                // Hey, we should also walk the Parent's parents, no ?

            }
            return _namesake;
        }
    }
}
