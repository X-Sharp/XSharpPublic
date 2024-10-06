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
    #region ExpansionFunction
    public abstract class ExpansionFunction : IVsExpansionFunction
    {
        private IVsExpansionClient provider;

        private string fieldName;

        private string[] args;

        private string[] list;

        public IVsExpansionClient ExpansionClient => provider;

        public string[] Arguments
        {
            get
            {
                return args;
            }
            set
            {
                args = value;
            }
        }

        public string FieldName
        {
            get
            {
                return fieldName;
            }
            set
            {
                fieldName = value;
            }
        }

        private ExpansionFunction()
        {
        }

        public ExpansionFunction(IVsExpansionClient provider)
        {
            this.provider = provider;
        }

        public abstract string GetCurrentValue();

        public virtual string GetDefaultValue()
        {
            return GetCurrentValue();
        }

        public virtual string[] GetIntellisenseList()
        {
            return null;
        }

        public string GetArgument(int index)
        {
            if (args == null || args.Length == 0 || index > args.Length)
            {
                return null;
            }

            string text = args[index];
            if (text == null)
            {
                return null;
            }

            int num = text.IndexOf('$');
            if (num >= 0)
            {
                StringBuilder stringBuilder = new StringBuilder();
                int length = text.Length;
                int num2 = 0;
                while (num >= 0 && num + 1 < length)
                {
                    stringBuilder.Append(text.Substring(num2, num - num2));
                    num2 = num;
                    num++;
                    if (text[num] == '$')
                    {
                        stringBuilder.Append('$');
                        num2 = num + 1;
                    }
                    else
                    {
                        int i;
                        for (i = num; i < length && char.IsLetterOrDigit(text[i]); i++)
                        {
                        }

                        if (i == length)
                        {
                            stringBuilder.Append('$');
                            num2 = num;
                            break;
                        }

                        if (text[i] == '$')
                        {
                            string text2 = text.Substring(num, i - num);
                            if (GetFieldValue(text2, out string value))
                            {
                                stringBuilder.Append(value);
                            }
                            else
                            {
                                stringBuilder.Append('$');
                                stringBuilder.Append(text2);
                                stringBuilder.Append('$');
                            }

                            num2 = i + 1;
                        }
                        else
                        {
                            stringBuilder.Append('$');
                            stringBuilder.Append(text.Substring(num, i - num));
                            num2 = i;
                        }
                    }

                    num = text.IndexOf('$', num2);
                }

                if (num2 < length)
                {
                    stringBuilder.Append(text.Substring(num2, length - num2));
                }

                text = stringBuilder.ToString();
            }

            if (text.Length > 2 && text[0] == '"' && text[text.Length - 1] == '"')
            {
                text = text.Substring(1, text.Length - 2);
            }
            else if (text.Length > 2 && text[0] == '\'' && text[text.Length - 1] == '\'')
            {
                text = text.Substring(1, text.Length - 2);
            }

            return text;
        }

        public bool GetFieldValue(string name, out string value)
        {
            value = null;
            //if (provider != null && provider.ExpansionSession != null)
            //{
            //    int fieldValue = ExpansionClient.GetFieldValue(name, out value);
            //    return NativeMethods.Succeeded(fieldValue);
            //}

            return false;
        }

        public TextSpan GetSelection()
        {
            TextSpan result = default(TextSpan);
            //IVsExpansionClient expansionProvider = IVsExpansionClient;
            //if (expansionProvider != null && expansionProvider.TextView != null)
            //{
            //    NativeMethods.ThrowOnFailure(expansionProvider.TextView.GetSelection(out result.iStartLine, out result.iStartIndex, out result.iEndLine, out result.iEndIndex));
            //}

            return result;
        }

        public virtual int FieldChanged(string bstrField, out int fRequeryValue)
        {
            if (args != null)
            {
                string b = "$" + bstrField + "$";
                string[] array = args;
                foreach (string a in array)
                {
                    if (a == b)
                    {
                        fRequeryValue = 1;
                        return 0;
                    }
                }
            }

            fRequeryValue = 0;
            return 0;
        }

        public int GetCurrentValue(out string bstrValue, out int hasDefaultValue)
        {
            try
            {
                bstrValue = GetCurrentValue();
            }
            catch
            {
                bstrValue = string.Empty;
            }

            hasDefaultValue = ((bstrValue != null) ? 1 : 0);
            return 0;
        }

        public int GetDefaultValue(out string bstrValue, out int hasCurrentValue)
        {
            try
            {
                bstrValue = GetDefaultValue();
            }
            catch
            {
                bstrValue = string.Empty;
            }

            hasCurrentValue = ((bstrValue != null) ? 1 : 0);
            return 0;
        }

        public virtual int GetFunctionType(out uint pFuncType)
        {
            if (list == null)
            {
                list = GetIntellisenseList();
            }

            pFuncType = ((list == null) ? 1u : 0u);
            return 0;
        }

        public virtual int GetListCount(out int iListCount)
        {
            if (list == null)
            {
                list = GetIntellisenseList();
            }

            if (list != null)
            {
                iListCount = list.Length;
            }
            else
            {
                iListCount = 0;
            }

            return 0;
        }

        public virtual int GetListText(int iIndex, out string ppszText)
        {
            if (list == null)
            {
                list = GetIntellisenseList();
            }

            if (list != null)
            {
                ppszText = list[iIndex];
            }
            else
            {
                ppszText = null;
            }

            return 0;
        }

        public virtual int ReleaseFunction()
        {
            provider = null;
            return 0;
        }
    }
    #endregion
    internal class SimpleTypeNameExpansionFunction : ExpansionFunction
    {
        string _fieldName;
        string _argument;
        internal SimpleTypeNameExpansionFunction(IVsExpansionClient provider, string fieldName, string param) : base(provider)
        {
            _fieldName = fieldName;
            _argument = param;
        }
        public override string GetCurrentValue()
        {
            string typeName = _argument;
            if (typeName.ToLower().StartsWith("global::"))
            {
                typeName = typeName.Substring(8);
            }
            return typeName;
        }
    }
    internal class InitProcTypeExpansionFunction : ExpansionFunction
    {
        string[] items = { "_INIT1", "_INIT2", "_INIT3" };
        internal InitProcTypeExpansionFunction(IVsExpansionClient provider) : base(provider)
        {

        }
        public override int GetFunctionType(out uint pFuncType)
        {
            pFuncType = (int)_ExpansionFunctionType.eft_List;
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

