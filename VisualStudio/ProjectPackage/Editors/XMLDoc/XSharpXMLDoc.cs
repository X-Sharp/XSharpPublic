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

        public static IVsXMLMemberIndex GetXmlDocFile(Assembly assembly, XProject project)
        {
            IVsXMLMemberIndex index = null;
            var location = assembly.Location;
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
                    if (asm.FileName.EndsWith("System.DLL", StringComparison.OrdinalIgnoreCase))
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
        static private string getParameterNames(ParameterInfo[] parameters, MemberInfo member)
        {
            var pars = "";
            if (parameters.Length == 1)
            {
                if (parameters[0].ParameterType.FullName.EndsWith("Usual[]"))
                {
                    var atts = member.GetCustomAttributes(false);
                    if (atts != null)
                    {
                        foreach (var custattr in atts)
                        {
                            if (custattr.ToString().EndsWith("ClipperCallingConventionAttribute"))
                            {
                                string[] names = (string[])custattr.GetType().GetProperty("ParameterNames").GetValue(custattr, null);
                                var typeName = parameters[0].ParameterType.FullName.Replace("[]", "");
                                foreach (var n in names)
                                {
                                    if (pars.Length > 0)
                                        pars += ",";
                                    else
                                        pars = "(";
                                    pars += typeName;
                                }
                                if (pars.Length > 0)
                                    pars += ")";
                                return pars;
                            }
                        }
                    }
                }
            }
            foreach (var p in parameters)
            {
                if (pars.Length > 0)
                    pars += ",";
                else
                    pars = "(";
                pars += p.ParameterType.FullName;
            }
            if (pars.Length > 0)
                pars += ")";


            return pars;
        }
        /// <summary>
        /// Get the summary of the type
        /// </summary>
        /// <param name="member"></param>
        /// <param name="project"></param>
        /// <returns></returns>
        static public string GetTypeSummary(System.Type member, XProject project)
        {
            string summary = null;
            Assembly declarationAssembly = null;
            declarationAssembly = member.Assembly;
            var file = XSharpXMLDocTools.GetXmlDocFile(declarationAssembly, project);
            if (file == null)
                return null;
            string name = member.FullName;
            string prefix = "T:";
            if (!string.IsNullOrEmpty(name))
            {
                uint id = 0;
                string xml = "";
                IVsXMLMemberData data = null;
                var result = file.ParseMemberSignature(prefix + name, out id);
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
                }

            }
            return summary;
        }

        static public string GetMemberSummary(MemberInfo member, XProject project)
        {
            string summary = null;
            Assembly declarationAssembly = null;
            //
            if (member is Type)
                declarationAssembly = ((Type)member).Assembly;
            else if (member.DeclaringType != null)
                declarationAssembly = member.DeclaringType.Assembly;
            else
                return summary;

            //
            var file = XSharpXMLDocTools.GetXmlDocFile(declarationAssembly, project);
            if (file == null)
                return null;
            try
            {
                string prefix = "";
                string name = "";
                switch (member.MemberType)
                {
                    case MemberTypes.Field:
                        prefix = "F:";
                        name = member.Name;
                        break;
                    case MemberTypes.Property:
                        prefix = "P:";
                        name = member.Name;
                        break;

                    case MemberTypes.Method:
                        prefix = "M:";
                        name = member.Name;
                        var mi = member as MethodInfo;
                        name += getParameterNames(mi.GetParameters(), member);
                        break;
                    case MemberTypes.Constructor:
                        prefix = "M:";
                        name = "#ctor";
                        var ci = member as ConstructorInfo;
                        name += getParameterNames(ci.GetParameters(), member);
                        break;

                    case MemberTypes.Event:
                        prefix = "E:";
                        name = member.Name;
                        break;
                }
                if (!string.IsNullOrEmpty(name))
                {
                    uint id = 0;
                    string xml = "";
                    IVsXMLMemberData data = null;
                    var result = file.ParseMemberSignature(prefix + member.DeclaringType.FullName + "." + name, out id);
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
                    }
                }
                if (string.IsNullOrWhiteSpace(summary) && member is ConstructorInfo)
                    summary = GetTypeSummary(member.DeclaringType, project);
            }
            catch (Exception e)
            {
                XSharpProjectPackage.Instance.DisplayOutPutMessage("Exception in XSharpXMLDocMember.GetDocSummary");
                XSharpProjectPackage.Instance.DisplayException(e);
            }
            //
            return summary;
        }

        static public bool GetMemberParameters(MemberInfo member, XProject project,  IList<string> names, IList<string> descriptions)
        {
            Assembly declarationAssembly = null;
            //
            if (member is Type)
                declarationAssembly = ((Type)member).Assembly;
            else if (member.DeclaringType != null)
                declarationAssembly = member.DeclaringType.Assembly;
            else
                return false;

            //
            var file = XSharpXMLDocTools.GetXmlDocFile(declarationAssembly, project);
            if (file == null)
                return false;
            try
            {
                string prefix = "";
                string name = "";
                switch (member.MemberType)
                {

                    case MemberTypes.Method:
                        prefix = "M:";
                        name = member.Name;
                        var mi = member as MethodInfo;
                        name += getParameterNames(mi.GetParameters(), member);
                        break;
                    case MemberTypes.Constructor:
                        prefix = "M:";
                        name = "#ctor";
                        var ci = member as ConstructorInfo;
                        name += getParameterNames(ci.GetParameters(), member);
                        break;
                }
                if (!string.IsNullOrEmpty(name))
                {
                    uint id = 0;
                    string xml = "";
                    int numparams = 0;
                    IVsXMLMemberData data = null;
                    var result = file.ParseMemberSignature(prefix + member.DeclaringType.FullName + "." + name, out id);
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
