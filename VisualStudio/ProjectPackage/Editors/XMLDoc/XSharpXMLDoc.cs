//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Operations;
using Microsoft.VisualStudio.Utilities;
using LanguageService.SyntaxTree;
using System.Reflection;
using System.Diagnostics;
using System.IO;
using System.Globalization;
using System.Linq;
using System.Xml;
using XSharpModel;
using Microsoft.VisualStudio.Shell.Interop;

namespace XSharp.Project
{
    // No need to do a complicated lookup for the reference assembly names
    // Our system type controller has these already
    static public class XSharpXMLDocTools
    {
        static Dictionary<string, IVsXMLMemberIndex> _memberIndexes = new Dictionary<string, IVsXMLMemberIndex>();
        static IVsXMLMemberIndexService _XMLMemberIndexService;
        static XSharpXMLDocTools()
        {
            _XMLMemberIndexService = (IVsXMLMemberIndexService)XSharpProjectPackage.GetGlobalService(typeof(SVsXMLMemberIndexService));
        }

        public static IVsXMLMemberIndex GetXmlDocFile(XAssembly assembly, XProject project)
        {
            IVsXMLMemberIndex index = null;
            var location = assembly.FileName;
            if (!string.IsNullOrWhiteSpace(location))
            {
                if (!_memberIndexes.TryGetValue(location, out index))
                {
                    _XMLMemberIndexService.CreateXMLMemberIndex(location, out index);
                    if (index != null)
                    {
                        _memberIndexes.Add(location, index);
                    }
                }
            }
            if (index == null)  // Sometimes we get a type in the Microsoft.Net folder and not the reference assemblies folder
            {
                string refasm = "";
                foreach (var asm in project.AssemblyReferences)
                {
                    if (asm.FullName == assembly.FullName)
                    {
                        refasm = asm.FileName;
                        break;
                    }
                    if (! string.IsNullOrEmpty(asm.FileName) &&  asm.FileName.EndsWith("System.DLL", StringComparison.OrdinalIgnoreCase))
                    {
                        if (assembly.FullName.Contains("mscorlib"))
                        {
                            refasm = Path.Combine(Path.GetDirectoryName(asm.FileName), "mscorlib.dll");
                            break;
                        }
                    }
                }

                if (refasm != location && !String.IsNullOrEmpty(refasm))
                {
                    if (!_memberIndexes.TryGetValue(refasm, out index))
                    {
                        _XMLMemberIndexService.CreateXMLMemberIndex(refasm, out index);
                        if (index != null)
                        {
                            if (!String.IsNullOrWhiteSpace(location))
                                _memberIndexes.Add(location, index);
                            _memberIndexes.Add(refasm, index);
                        }

                    }
                }
            }

            return index;
        }
    }


    static public class XSharpXMLDocMember
    {
        /// <summary>
        /// Build list of parameter names needed for lookup in the xml file
        /// </summary>
        /// <param name="parameters"></param>
        /// <returns></returns>
        static private string getParameterNames(IList<XParameter> parameters, MemberInfo member)
        {
            var pars = "";
            foreach (var p in parameters)
            {
                if (pars.Length > 0)
                    pars += ",";
                else
                    pars = "(";
                pars += p.TypeName;
            }
            if (pars.Length > 0)
                pars += ")";

            if (pars.Contains("&"))
                pars = pars.Replace("&", "@");
            return pars;
        }
        /// <summary>
        /// Get the summary of the type
        /// </summary>
        /// <param name="member"></param>
        /// <param name="project"></param>
        /// <returns></returns>
        static public string GetTypeSummary(IXType type, XProject project, out string returns, out string remarks)
        {
            string summary = null;
            XAssembly declarationAssembly = null;
            XTypeReference xtype;
            returns = remarks = "";

            if (type is XTypeReference)
            {
                xtype = (XTypeReference)type;
                declarationAssembly = xtype.Assembly;

            }
            else
                return "";
            var file = XSharpXMLDocTools.GetXmlDocFile(declarationAssembly, project);
            if (file == null)
                return null;
            string sig = xtype.XMLSignature;
            if (!string.IsNullOrEmpty(sig))
            {
                uint id = 0;
                string xml = "";
                IVsXMLMemberData data = null;
                var result = file.ParseMemberSignature(sig, out id);
                if (result >= 0 && id != 0)
                {
                    result = file.GetMemberXML(id, out xml);
                }
                if (result >= 0 && !String.IsNullOrEmpty(xml))
                {
                    result = file.GetMemberDataFromXML(xml, out data);
                }
                if (result >= 0 && data != null)
                {
                    result = data.GetSummaryText(out summary);
                    result = data.GetReturnsText(out returns);
                    result = data.GetRemarksText(out remarks);
                }
            }
            return summary;
        }


        static public string GetMemberSummary(IXMember member, XProject project, out string returns, out string remarks)
        {
            string summary = null;
            XAssembly declarationAssembly = null;
            XMemberReference xmember;
            returns = remarks = "";
            //
            if (member is XMemberReference)
            {
                xmember = (XMemberReference)member;
                declarationAssembly = xmember.Assembly;
            }
            else
                return summary;

            //
            var file = XSharpXMLDocTools.GetXmlDocFile(declarationAssembly, project);
            if (file == null)
                return null;
            var sig = xmember.XMLSignature;
            try
            {
                if (!string.IsNullOrEmpty(sig))
                {
                    uint id = 0;
                    string xml = "";
                    IVsXMLMemberData data = null;
                    var result = file.ParseMemberSignature(sig, out id);
                    if (result >= 0 && id != 0)
                    {
                        result = file.GetMemberXML(id, out xml);
                    }
                    if (result >= 0 && !String.IsNullOrEmpty(xml))
                    {
                        result = file.GetMemberDataFromXML(xml, out data);
                    }
                    if (result >= 0 && data != null)
                    {
                        result = data.GetSummaryText(out summary);
                        result = data.GetRemarksText(out remarks);
                        result = data.GetReturnsText(out returns);
                    }
                }
            }
            catch (Exception e)
            {
                XSharpProjectPackage.Instance.DisplayOutPutMessage("Exception in XSharpXMLDocMember.GetDocSummary");
                XSharpProjectPackage.Instance.DisplayException(e);
            }
            //
            return summary;
        }

        static public bool GetMemberParameters(IXMember member, XProject project,  IList<string> names, IList<string> descriptions)
        {
            XAssembly declarationAssembly = null;
            XMemberReference xmember;
            //
            if (member is XMemberReference)
            {
                xmember = (XMemberReference)member;
                declarationAssembly = xmember.Assembly;
            }
            else
                return false;

            //
            var file = XSharpXMLDocTools.GetXmlDocFile(declarationAssembly, project);
            if (file == null)
                return false;
            try
            {
                var sig  = xmember.XMLSignature;
                if (!string.IsNullOrEmpty(sig))
                {
                    uint id = 0;
                    string xml = "";
                    int numparams = 0;
                    IVsXMLMemberData data = null;
                    var result = file.ParseMemberSignature(sig, out id);
                    if (result >= 0 && id != 0)
                    {
                        result = file.GetMemberXML(id, out xml);
                    }
                    if (result >= 0 && !String.IsNullOrEmpty(xml))
                    {
                        result = file.GetMemberDataFromXML(xml, out data);
                    }
                    if (result >= 0 && data != null)
                    {
                        result = data.GetParamCount(out numparams);
                    }
                    if (result >= 0 && numparams != 0)
                    {
                        string paramName;
                        string paramDesc;
                        for (int i = 0; i < numparams; i++)
                        {
                            result = data.GetParamTextAt(i, out paramName, out paramDesc);
                            names.Add(paramName);
                            descriptions.Add(paramDesc);
                        }
                    }
                }
            }
            catch (Exception e)
            {
                XSharpProjectPackage.Instance.DisplayOutPutMessage("Exception in XSharpXMLDocMember.GetDocSummary");
                XSharpProjectPackage.Instance.DisplayException(e);
                return false;
            }
            return true;
        }
    }
}
