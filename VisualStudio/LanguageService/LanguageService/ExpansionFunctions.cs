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
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Package;
using Microsoft.VisualStudio.TextManager.Interop;
using Microsoft.VisualStudio.OLE.Interop;
using System.Runtime.InteropServices;
namespace XSharp.LanguageService
{
    internal class ClassNameExpansionFunction : ExpansionFunction
    {
        int nameCount = 0;
        internal ClassNameExpansionFunction(ExpansionProvider provider, int counter) : base(provider)
        {
            nameCount = counter;
        }
        public override string GetCurrentValue()
        {
            string name = "MyClass";
            name += nameCount.ToString();
            return name;
        }
    }
    internal class SimpleTypeNameExpansionFunction : ExpansionFunction
    {
        internal SimpleTypeNameExpansionFunction(ExpansionProvider provider) : base(provider)
        {

        }
        public override string GetCurrentValue()
        {
            string typeName = GetArgument(0);
            if (typeName.ToLower().StartsWith("global::"))
            {
                typeName = typeName.Substring(8);
            }
            return typeName;
        }
    }
    internal class GenerateSwitchCasesExpansionFunction : ExpansionFunction
    {
        internal GenerateSwitchCasesExpansionFunction(ExpansionProvider provider) : base(provider)
        {
        }
        public override string GetCurrentValue()
        {
            string expansion = String.Empty;
            string classname;
            string varname;
            if (GetFieldValue("$expression$", out classname) && GetFieldValue("$var$", out varname))
            {
                System.Type type = Type.GetType(classname);
                if (type != null && type.IsEnum)
                {
                    var names = Enum.GetNames(type);
                    foreach (var s in names)
                    {
                        expansion += "CASE " + varname + " == " + s + "\r\n\r\n";
                    }
                }
                else
                {
                    expansion = "CASE " + varname + " == " + classname + "\r\n\r\n";
                }
            }
            return expansion;
        }
    }
    internal class InitProcTypeExpansionFunction : ExpansionFunction
    {
        string[] items = { "_INIT1","_INIT2","_INIT3" };
        internal InitProcTypeExpansionFunction(ExpansionProvider provider) : base(provider)
        {

        }
        public override int GetFunctionType(out uint pFuncType)
        {
            pFuncType = (int) _ExpansionFunctionType.eft_List;
            return 0;
        }
        public override int GetListCount(out int listCount)
        {
            listCount = items.Length;
            return 0;
        }
        public override String[] GetIntellisenseList()
        {
            return items;
        }
        public override string GetCurrentValue()
        {
            return items[0];
        }
        public override string GetDefaultValue()
        {
            return items[0];
        }
    }
}

