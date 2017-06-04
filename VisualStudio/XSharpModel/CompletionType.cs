//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using EnvDTE;
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
        // System Type
        private Type _stype = null;
        // XSharp Type
        private XType _xtype = null;
        // External project EnvDTE.CodeElement
        private CodeElement _codeElt = null;

        public CompletionType(XType xType)
        {
            this._xtype = xType;
        }

        public CompletionType(Type sType)
        {
            this._stype = sType;
        }

        public CompletionType(CodeElement elt)
        {
            this._codeElt = elt;
        }

        public CompletionType()
        {
        }

        public CompletionType(XElement element)
        {
            //throw new NotImplementedException();
            if (element is XType)
            {
                this._xtype = (XType)element;
            }
            else
            {
                XTypeMember member = element.Parent as XTypeMember;
                if (member != null)
                {
                    CheckType(member.TypeName, member.File, member.Parent.NameSpace);
                }
            }
        }

        public CompletionType(XTypeMember element)
        {
            //throw new NotImplementedException();
            if (element.Kind.HasReturnType())
            {
                // lookup type from Return type
                CheckType(element.TypeName, element.File, element.Parent.NameSpace);
            }
            else
            {
                this._xtype = element.Parent as XType;
            }
        }

        public CompletionType(XVariable var, string defaultNS)
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
                if (!String.IsNullOrEmpty(member.Parent.NameSpace))
                {
                    defaultNS = member.Parent.NameSpace;
                }
                CheckType(var.TypeName, member.File, defaultNS);
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
            CheckType(typeName, xFile, defaultNS);
        }

        public CompletionType(String typeName, XFile xFile, List<String> usings)
        {
            CheckType(typeName, xFile, usings);
        }


        /// <summary>
        /// Check/Lookup for typeName, in the project owning xFile, eventually looking at the Usings including the Default Namespace
        /// </summary>
        /// <param name="typeName"></param>
        /// <param name="xFile"></param>
        /// <param name="defaultNS"></param>
        private void CheckType(string typeName, XFile xFile, string defaultNS)
        {
            // Create the Usings List
            List<String> usings = new List<String>(xFile.Usings);
            if (!String.IsNullOrEmpty(defaultNS))
                usings.Add(defaultNS);
            //
            CheckType(typeName, xFile, usings);
        }

        private void CheckType(string typeName, XFile xFile, List<String> usings)
        {
            // First, check the XProject corresponding to the xFile
            CheckProjectType(typeName, xFile.Project, usings);
            if (!this.IsInitialized)
            {
                // Not Found ?
                // now try with System Types (External Dlls too)
                CheckSystemType(typeName, usings);
                if (!this.IsInitialized)
                {
                    // Not Found ? 
                    // now try with Referenced XSharp Projects
                    foreach (XProject prj in xFile.Project.ReferencedProjects)
                    {
                        CheckProjectType(typeName, prj, usings);
                        if (this.IsInitialized)
                            break;
                    }
                    if (!this.IsInitialized)
                    {
                        // Not Found ? 
                        // now try with Referenced Foreign Projects
                        CheckStrangerProjectType(typeName, xFile.Project, usings);
                    }
                }
            }
        }

        private void CheckProjectType(string typeName, XProject xprj, List<String> usings)
        {
            // First, easy way..Use the simple name
            XType xType = xprj.Lookup(typeName, true);
            if (xType == null)
            {
                // ?? Fullname maybe ?
                xType = xprj.LookupFullName(typeName, true);
                if (xType == null)
                {
                    // Search using the USING statements in the File that contains the var
                    foreach (string usingStatement in usings)
                    {
                        String fqn = usingStatement + "." + typeName;
                        xType = xprj.LookupFullName(fqn, true);
                        if (xType != null)
                            break;
                    }
                }
            }
            if (xType != null)
            {
                this._xtype = xType;
            }
        }

        private void CheckStrangerProjectType(string typeName, XProject xprj, List<String> usings)
        {
            // First, easy way..Use the simple name
            CodeElement codeElt = xprj.LookupForStranger(typeName, true);
            if (codeElt == null)
            {
                // Search using the USING statements in the File that contains the var
                foreach (string usingStatement in usings)
                {
                    String fqn = usingStatement + "." + typeName;
                    codeElt = xprj.LookupForStranger(fqn, true);
                    if (codeElt != null)
                        break;
                }
            }
            if (codeElt != null)
            {
                this._codeElt = codeElt;
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
                return ((this._stype != null) || (this._xtype != null) || (this._codeElt != null));
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

        public CompletionType ParentType
        {
            get
            {
                if (_stype != null)
                    return new CompletionType(_stype.BaseType);
                if (_xtype != null)
                {
                    if (_xtype.Parent != null)
                        return new CompletionType(_xtype.Parent);
                    if (_xtype.ParentName != null)
                        return new CompletionType(_xtype.ParentName, null);
                }
                return new CompletionType("System.Object",null);
            }
        }

        public CodeElement CodeElement
        {
            get
            {
                return _codeElt;
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
                if (this._codeElt != null)
                {
                    return this._codeElt.FullName;
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
    public static class CompletionTypeExtensions
    {
        public static bool IsEmpty(this CompletionType cType)
        {
            if (cType == null)
                return true;
            return !cType.IsInitialized;
        }
    }
}
