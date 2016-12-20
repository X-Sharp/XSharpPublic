using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharpModel
{
    /// <summary>
    /// Placeholder to store a type used in CompletionSet build.
    /// It can be one of our type, or one of the System type
    /// </summary>
    public class CompletionType
    {
        private Type _stype = null;
        private XType _xtype = null;

        public CompletionType(XType xType)
        {
            this._xtype = xType;
        }

        public CompletionType(XElement element)
        {
            //throw new NotImplementedException();
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
                // First, easy way..Use the simple name
                XType xType = var.File.Project.Lookup(var.TypeName, true);
                if (xType == null)
                {
                    // Search using the USING statements in the File that contains the var
                    foreach (string usingStatement in member.File.Usings)
                    {
                        String fqn = usingStatement + "." + var.TypeName;
                        xType = var.File.Project.Lookup(fqn, true);
                        if (xType != null)
                            break;
                    }
                    if (xType == null)
                    {
                        // Ok, none of our own Type; can be a System/Referenced Type
                        CheckSystemType(var.TypeName, member.File.Usings);
                    }
                }
                if (xType != null)
                {
                    this._xtype = xType;
                }
            }
        }

        public CompletionType( String typeName, List<String> usings )
        {
            CheckSystemType(typeName, usings);
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
                if ( this._xtype != null )
                {
                    return this._xtype.FullName;
                }
                if ( this._stype != null )
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
                if (kw == "string")
                {
                    return typeof(string);
                }
                if (kw == "char")
                {
                    return typeof(char);
                }
                if (kw == "byte")
                {
                    return typeof(byte);
                }
                if (kw == "short")
                {
                    return typeof(short);
                }
                if (kw == "word")
                {
                    return typeof(ushort);
                }
                if (kw == "dword")
                {
                    return typeof(uint);
                }
                if (kw == "long")
                {
                    return typeof(int);
                }
                if (kw == "longint")
                {
                    return typeof(int);
                }
                if (kw == "int")
                {
                    return typeof(int);
                }
                if (kw == "int64")
                {
                    return typeof(long);
                }
                if (kw == "uint64")
                {
                    return typeof(ulong);
                }
                if (kw == "real8")
                {
                    return typeof(double);
                }
                if (kw == "real4")
                {
                    return typeof(float);
                }
                if (kw == "logic")
                {
                    return typeof(bool);
                }
                if (kw == "void")
                {
                    return typeof(void);
                }
            }
            return null;
        }
    }
}
