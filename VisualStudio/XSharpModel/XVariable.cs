//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System;
using System.Diagnostics;
namespace XSharpModel
{
    [DebuggerDisplay("{Prototype,nq}")]
    public class XVariable : XElement
    {
        private string _typeName;
        private bool _isParameter;

        public XVariable(XElement parent, string name, Kind kind, Modifiers visibility, TextRange span, TextInterval position, string typeName, bool isParameter = false)
            : base(name, kind, Modifiers.None, visibility, span, position)
        {
            if (string.IsNullOrEmpty(typeName))
                typeName = "USUAL";
            _typeName = typeName;
            _isParameter = isParameter;
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
                if (string.IsNullOrEmpty(value))
                    value = "USUAL";
                _typeName = value;
            }
        }

        public override string Description
        {
            get
            {
                string prefix;
                if (_isParameter)
                    prefix = "PARAMETER ";
                else
                    prefix = "LOCAL ";
                return prefix +this.Prototype + " as " + this.TypeName + (IsArray ? "[]" : "");
            }
        }

        public override string Prototype
        {
            get
            {
                return this.Name;
            }
        }
        public bool IsArray { get; set; }

        public static readonly String VarType = "$VAR$";

    }
}
