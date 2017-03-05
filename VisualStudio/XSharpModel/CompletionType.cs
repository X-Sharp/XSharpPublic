//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharpModel
{
    /// <summary>
    /// Placeholder to store a type used in CompletionSet build.
    /// It can be one of our type, or one of the System type
    /// </summary>
    [DebuggerDisplay("{FullName,nq}")]
    public class CompletionType
    {
        private Type _stype = null;
        private XType _xtype = null;

        public CompletionType(XType xType)
        {
            this._xtype = xType;
        }

        public CompletionType(Type sType)
        {
            this._stype = sType;
        }

        public CompletionType()
        {
        }

        public CompletionType(XElement element)
        {
            //throw new NotImplementedException();
            XTypeMember member = element.Parent as XTypeMember;
            if (member != null)
            {
                CheckProjectType(member.TypeName, member.File, member.Parent.NameSpace);
            }
        }

        public CompletionType(XTypeMember element)
        {
            //throw new NotImplementedException();
            XType xType = element.Parent as XType;
            if (xType != null)
            {
                this._xtype = xType;
            }
        }

        public CompletionType(XVariable var)
        {
            // We know the context
            // var.Parent
            // We know the Type Name
            // var.TypeName
            // We need to lookup for the XType or System.Type
            // To check, we will need to know also "imported" types
            XTypeMember member = var.Parent as XTypeMember;
            if (member != null)
            {
                CheckProjectType(var.TypeName, member.File, member.Parent.NameSpace);
            }
        }

        public CompletionType(String typeName, List<String> usings)
        {
            CheckSystemType(typeName, usings);
        }

        //public CompletionType(String typeName, XFile xFile)
        //{
        //    CheckProjectType(typeName, xFile, "");
        //    if (!this.IsInitialized)
        //    {
        //        CheckSystemType(typeName, xFile.Usings);
        //    }
        //}

        public CompletionType(String typeName, XFile xFile, string defaultNS)
        {
            CheckProjectType(typeName, xFile, defaultNS);
            if (!this.IsInitialized)
            {
                CheckSystemType(typeName, xFile.Usings);
            }
        }


        private void CheckProjectType(string typeName, XFile xFile, string defaultNS )
        {
            // First, easy way..Use the simple name
            XType xType = xFile.Project.Lookup(typeName, true);
            if (xType == null)
            {
                // ?? Fullname maybe ?
                xType = xFile.Project.LookupFullName(typeName, true);
                if (xType == null)
                {
                    List<String> usings = new List<String>(xFile.Usings);
                    if (!String.IsNullOrEmpty(defaultNS))
                        usings.Add(defaultNS);
                    // Search using the USING statements in the File that contains the var
                    foreach (string usingStatement in usings)
                    {
                        String fqn = usingStatement + "." + typeName;
                        xType = xFile.Project.LookupFullName(fqn, true);
                        if (xType != null)
                            break;
                    }
                    if (xType == null)
                    {
                        // Ok, none of our own Type; can be a System/Referenced Type
                        CheckSystemType(typeName, usings);
                    }
                }
            }
            if (xType != null)
            {
                this._xtype = xType;
            }
        }


        private void CheckSystemType(string typeName, List<string> usings)
        {
            // Could it be a "simple" Type ?
            Type sType = SimpleTypeToSystemType(typeName);
            if (sType == null)
            {
                // When we have a TypeName as string, let's suppose it is a System Type
                sType = SystemTypeController.Lookup(typeName);
                if ((sType == null) && (usings != null))
                {
                    // Search using the USING statements in the File that contains the var
                    foreach (string usingStatement in usings)
                    {
                        String fqn = usingStatement + "." + typeName;
                        sType = SystemTypeController.Lookup(fqn);
                        if (sType != null)
                            break;
                    }
                }
            }
            if (sType != null)
            {
                this._stype = sType;
            }
        }

        public bool IsInitialized
        {
            get
            {
                return ((this._stype != null) || (this._xtype != null));
            }
        }

        public Type SType
        {
            get
            {
                return _stype;
            }

        }

        public XType XType
        {
            get
            {
                return _xtype;
            }

        }

        public String FullName
        {
            get
            {
                if (this._xtype != null)
                {
                    return this._xtype.FullName;
                }
                if (this._stype != null)
                {
                    return this._stype.FullName;
                }
                return null;
            }
        }

        internal Type SimpleTypeToSystemType(string kw)
        {
            if (kw != null)
            {
                kw = kw.ToLowerInvariant();
                switch (kw)
                {
                    case "string":
                        return typeof(string);
                    case "char":
                        return typeof(char);
                    case "byte":
                        return typeof(byte);
                    case "sbyte":
                        return typeof(sbyte);
                    case "int16":
                    case "short":
                    case "shortint":
                        return typeof(short);
                    case "uint16":
                    case "word":
                        return typeof(ushort);
                    case "dword":
                    case "uint32":
                        return typeof(uint);
                    case "int":
                    case "int32":
                    case "long":
                    case "longint":
                        return typeof(int);
                    case "int64":
                        return typeof(long);
                    case "uint64":
                        return typeof(ulong);
                    case "real8":
                        return typeof(double);
                    case "real4":
                        return typeof(float);
                    case "logic":
                        return typeof(bool);
                    case "void":
                        return typeof(void);
                }
            }
            return null;
        }
    }
}
