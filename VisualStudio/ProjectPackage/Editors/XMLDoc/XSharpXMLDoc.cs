//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.IO;
using XSharpModel;
using Microsoft.VisualStudio.Shell.Interop;
using System.Linq;

namespace XSharp.Project
{
    // No need to do a complicated lookup for the reference assembly names
    // Our system type controller has these already
    static public class XSharpXMLDocTools
    {
        static Dictionary<string, IVsXMLMemberIndex> _memberIndexes = new Dictionary<string, IVsXMLMemberIndex>();
        static IVsXMLMemberIndexService _XMLMemberIndexService;
        static string coreLoc = "";
        static IVsXMLMemberIndex coreIndex = null;
        static XSharpXMLDocTools()
        {
            _XMLMemberIndexService = (IVsXMLMemberIndexService)XSharpProjectPackage.GetGlobalService(typeof(SVsXMLMemberIndexService));
            // create default entry so our own xml lookup will work
            var node = @"HKEY_LOCAL_MACHINE\Software\XSharpBV\XSharp";
            var InstallPath = (string)Microsoft.Win32.Registry.GetValue(node, "XSharpPath", "");
            var assemblies = Path.Combine(InstallPath, "Assemblies");
            var location = Path.Combine(assemblies, "XSharp.Core.dll");
            IVsXMLMemberIndex index;
            _XMLMemberIndexService.CreateXMLMemberIndex(location, out index);
            if (index != null)
            {
                _memberIndexes.Add(location, index);
                coreLoc = location;
                coreIndex = index;
            }
        }

        public static void Close()
        {
            _memberIndexes.Clear();
            if (coreIndex != null)
            {
                _memberIndexes.Add(coreLoc, coreIndex);
            }
        }

        public static IVsXMLMemberIndex firstfile
        {
            get
            {
                return _memberIndexes.Values.FirstOrDefault();       
            }
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
            if (index == null && project != null)  // Sometimes we get a type in the Microsoft.Net folder and not the reference assemblies folder
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
        static private string getSummary(IVsXMLMemberIndex file, string xml,out string returns, out string remarks)
        {
            string summary = "";
            returns = remarks = "";
            IVsXMLMemberData data = null;
            var result = file.GetMemberDataFromXML(xml, out data);
            if (result >= 0 && data != null)
            {
                result = data.GetSummaryText(out summary);
                result = data.GetReturnsText(out returns);
                result = data.GetRemarksText(out remarks);
            }
            summary = CleanUpResult(summary);
            returns = CleanUpResult(returns);
            remarks = CleanUpResult(remarks);
            return summary;
        }
        static string CleanUpResult(string source)
        {
            if (! string.IsNullOrEmpty(source))
            {
                if (source.Contains("\t"))
                {
                    source = source.Replace("\t", " ");
                }
                while (source.Contains("  "))
                {
                    source = source.Replace("  ", " ");
                }
                source = source.Replace(". ", ".\r");
            }
            return source;
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
            returns = remarks = "";
            if (type == null)
                return "";
            if (type is XTypeDefinition)
            {
                var xdef = (XTypeDefinition)type;
                var xml = xdef.XmlComments;
                var xfile = XSharpXMLDocTools.firstfile;
                if (xfile != null && ! string.IsNullOrEmpty(xml))
                {
                    summary = getSummary(xfile, xml, out returns, out remarks);
                }
                return summary;

            }

            var xtype = (XTypeReference)type;
            var declarationAssembly = xtype.Assembly;

            var file = XSharpXMLDocTools.GetXmlDocFile(declarationAssembly, project);
            if (file == null)
                return null;
            string sig = xtype.XMLSignature;
            if (!string.IsNullOrEmpty(sig))
            {
                uint id = 0;
                string xml = "";
                var result = file.ParseMemberSignature(sig, out id);
                if (result >= 0 && id != 0)
                {
                    result = file.GetMemberXML(id, out xml);
                }
                summary = getSummary(file, xml, out returns, out remarks);
            }
            return summary;
        }

        static IVsXMLMemberIndex lastfile = null;
        static public string GetMemberSummary(IXMember member, XProject project, out string returns, out string remarks)
        {
            string summary = null;
            returns = remarks = "";
            if (member == null)
                return "";

            //
            if (member is XMemberDefinition)
            {
                var xdef = (XMemberDefinition)member;
                var xml = xdef.XmlComments;
                var xfile = XSharpXMLDocTools.firstfile;
                if (xfile != null && !string.IsNullOrEmpty(xml))
                {
                    summary = getSummary(xfile, xml, out returns, out remarks);
                }
                return summary;

            }
            if (!(member is XMemberReference))
                return "";

            var xmember = (XMemberReference)member;
            var declarationAssembly = xmember.Assembly;

                //
            var file = XSharpXMLDocTools.GetXmlDocFile(declarationAssembly, project);
            if (file == null)
                return null;
            lastfile = file;
            var sig = xmember.XMLSignature;
            try
            {
                if (!string.IsNullOrEmpty(sig))
                {
                    uint id = 0;
                    string xml = "";
                    var result = file.ParseMemberSignature(sig, out id);
                    if (result >= 0 && id != 0)
                    {
                        result = file.GetMemberXML(id, out xml);
                    }
                    if (! string.IsNullOrEmpty(xml))
                    {
                        summary = getSummary(file, xml, out returns, out remarks);
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


        static private bool getParameterInfo(IVsXMLMemberIndex file, string xml, IList<string> names, IList<string> descriptions)
        {
            IVsXMLMemberData data = null;
            var result = file.GetMemberDataFromXML(xml, out data);
            int numparams = 0;
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
                    paramDesc = CleanUpResult(paramDesc);
                    descriptions.Add(paramDesc);
                }
            }
            return true;

        }
        static public bool GetMemberParameters(IXMember member, XProject project,  IList<string> names, IList<string> descriptions)
        {
            if (member == null)
                return false;

            if (member is XMemberDefinition)
            {
                var xdef = (XMemberDefinition)member;
                var xml = xdef.XmlComments;
                var xfile = XSharpXMLDocTools.firstfile;
                if (xfile != null && !string.IsNullOrEmpty(xml))
                {
                    getParameterInfo(xfile, xml, names, descriptions);
                    return true;
                }
                return false;

            }
            if (!(member is XMemberReference))
                return false;

            var xmember = (XMemberReference)member;
            var declarationAssembly = xmember.Assembly;
            var file = XSharpXMLDocTools.GetXmlDocFile(declarationAssembly, project);
            if (file == null)
            {
                return false;
            }
            try
            {
                var sig  = xmember.XMLSignature;
                if (!string.IsNullOrEmpty(sig))
                {
                    uint id = 0;
                    string xml = "";
                    var result = file.ParseMemberSignature(sig, out id);
                    if (result >= 0 && id != 0)
                    {
                        result = file.GetMemberXML(id, out xml);
                    }
                    getParameterInfo(file, xml, names, descriptions);
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
