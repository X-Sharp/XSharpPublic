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
using System.Threading.Tasks;
using Microsoft.VisualStudio.Shell;
using Microsoft;
using Task = System.Threading.Tasks.Task;
using Community.VisualStudio.Toolkit;
using System.Text;
using System.Xml;

namespace XSharp.LanguageService
{
    // No need to do a complicated lookup for the reference assembly names
    // Our system type controller has these already
    static public class XSharpXMLDocTools
    {
        static readonly Dictionary<string, IVsXMLMemberIndex> _memberIndexes = new Dictionary<string, IVsXMLMemberIndex>();
        static IVsXMLMemberIndexService _XMLMemberIndexService;
        static string coreLoc = "";
        static IVsXMLMemberIndex coreIndex = null;
        static XSharpXMLDocTools()
        {
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                _XMLMemberIndexService = await VS.GetServiceAsync<SVsXMLMemberIndexService, IVsXMLMemberIndexService>();
            });
            Assumes.Present(_XMLMemberIndexService);
            return;
        }
        public static void Initialize()
        {
            ; // Do nothing. This just triggers the Class Constructor
        }
        private static async Task LoadCoreDllAsync()
        {
            var node = @"HKEY_LOCAL_MACHINE\Software\XSharpBV\XSharp";
            var InstallPath = (string)Microsoft.Win32.Registry.GetValue(node, "XSharpPath", "");
            var assemblies = Path.Combine(InstallPath, "Assemblies");
            coreLoc = Path.Combine(assemblies, "XSharp.Core.dll");
            coreIndex = null;
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
            _XMLMemberIndexService.CreateXMLMemberIndex(coreLoc, out IVsXMLMemberIndex index);
            if (index != null)
            {
                _memberIndexes.Add(coreLoc, index);
                coreIndex = index;
            }
            return;
        }

        private static void LoadCoreDLL()
        {
            ThreadHelper.JoinableTaskFactory.Run(() => LoadCoreDllAsync());
        }
        public static void Close()
        {
            _memberIndexes.Clear();
            if (coreIndex != null)
            {
                _memberIndexes.Add(coreLoc, coreIndex);
            }
        }

        public static IVsXMLMemberIndex Firstfile
        {
            get
            {
                if (_memberIndexes.Count == 0)
                {
                    LoadCoreDLL();
                }
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
                    ThreadHelper.JoinableTaskFactory.Run(async delegate
                    {
                        await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                        _XMLMemberIndexService.CreateXMLMemberIndex(location, out index);
                        if (index != null)
                        {
                            _memberIndexes.Add(location, index);
                        }
                    });
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
                    if (!string.IsNullOrEmpty(asm.FileName) && asm.FileName.EndsWith("System.DLL", StringComparison.OrdinalIgnoreCase))
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
                        ThreadHelper.JoinableTaskFactory.Run(async delegate
                        {
                            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                            _XMLMemberIndexService.CreateXMLMemberIndex(refasm, out index);
                        });
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
        public static IEnumerable<string> MemoLines(this string str, int maxLength)
        {
            str = str.Replace("\r\n", "\r");
            var tokens = str.Split(" \t".ToCharArray());
            var result = "";
            foreach (var token in tokens)
            {
                if (result.Length + token.Length + 1 > maxLength )
                { 
                    yield return result;
                    result = "";
                }
                if (result.Length > 0)
                    result += ' ';

                if (token.Contains("\r"))
                {
                    var lhs = token.Substring(0, token.IndexOf('\r'));
                    var rhs = token.Substring(token.IndexOf('\r') + 1);
                    result += lhs;
                    yield return result;
                    result = rhs;
                }
                else
                {
                    result += token;
                }
            }
            yield return result;
        }
        static string addXml(string token, string line)
        {
            var result = "";
            result += $"<{token}>";
            if (line.Length < 60)
            {
                result += line;
                result += $"<\\{token}>\r\n";
            }
            else
            {
                foreach (var element in MemoLines(line, 70))
                {
                    result += "\r\n" + element;
                }
                result += $"\r\n<\\{token}>\r\n";
            }
            return result;
        }
        public static string GetDoc(XAssembly asm, IXSymbol symbol)
        {
            var file = XSharpXMLDocTools.GetXmlDocFile(asm, null);
            string key=null;
            if (symbol is IXTypeSymbol type)
                key = type.XMLSignature;
            else if (symbol is IXMemberSymbol member)
                key = member.XMLSignature;
            if (file == null)
                return null;
            if (!string.IsNullOrEmpty(key))
            {
                uint id = 0;
                string xml = "";
                return ThreadHelper.JoinableTaskFactory.Run(async delegate
                {
                    await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                    StringBuilder sb = new StringBuilder();

                    var result = file.ParseMemberSignature(key, out id);
                    result = file.GetMemberXML(id, out xml);
                    var summary = GetSummary(xml, out var returns, out var remarks);
                    if (! String.IsNullOrWhiteSpace(summary))
                    {
                        sb.Append(addXml("summary", summary));
                    }
                    if (symbol is IXMemberSymbol member && member.Parameters.Count > 0)
                    {
                        var names = new List<string>();
                        var desc = new List<string>();
                        GetParameterInfo(file, xml, names, desc);
                        if (names.Count > 0 && desc.Count > 0 && names.Count == desc.Count)
                        {
                            for (int i = 0; i < names.Count; i++)
                            {
                                var temp = addXml("param", desc[i]);
                                temp = temp.Replace("<param>", $"<param name=\"{names[i]}\">");
                                sb.Append(temp);
                            }
                        }
                    }
                    if (!String.IsNullOrWhiteSpace(returns))
                    {
                        sb.Append(addXml("returns", returns));
                    }
                    if (!String.IsNullOrWhiteSpace(remarks))
                    {
                        sb.Append(addXml("remarks", remarks));
                    }
                    return sb.ToString();
                    
                });
            }
            return null;

        }
        static private string GetSummary(string xml, out string returns, out string remarks)
        {
            returns = remarks = "";
            string summary = "";
            string myreturns = "", myremarks = "";
            if (string.IsNullOrEmpty(xml))
                return "";
            try
            {
                var doc = new XmlDocument();
                doc.LoadXml(xml);
                var node = doc.FirstChild;
                foreach (XmlNode child in node.ChildNodes)
                {
                    switch (child.Name.ToLower())
                    {
                        case "summary":
                            summary = child.InnerXml;
                            break;
                        case "returns":
                            myreturns = child.InnerXml;
                            break;
                        case "remarks":
                            myremarks = child.InnerXml;
                            break;
                    }
                }
                summary = CleanUpResult(summary);
                returns = CleanUpResult(myreturns);
                remarks = CleanUpResult(myremarks);
            }
            catch (Exception e)
            {
                XSettings.DisplayOutputMessage("Exception in XSharpXMLDocMember.GetSummary");
                XSettings.DisplayException(e);

            }
            return summary;

        }
        static string CleanUpResult(string source)
        {
            if (!string.IsNullOrEmpty(source))
            {
                source = source.Replace("\n", "");
                source = source.Replace("\r", "");
                if (source.Contains("\t"))
                {
                    source = source.Replace("\t", " ");
                }
                while (source.Contains("  "))
                {
                    source = source.Replace("  ", " ");
                }
                source = source.Replace(". ", ".\r");
                
                source = source.Replace("<br />", "\r");
                source = source.Replace("<br/>", "\r");
                source = source.Replace("\r\r", "\r");
                source = source.Replace("\r ", "\r");
                source = source.TrimStart();
            }
            return source;
        }


        /// <summary>
        /// Get the summary of the type
        /// </summary>
        /// <param name="member"></param>
        /// <param name="project"></param>
        /// <returns></returns>
        static public string GetTypeSummary(IXTypeSymbol type, XProject project, out string returns, out string remarks)
        {
            string summary = null;
            returns = remarks = "";
            if (type == null)
                return "";
            if (type is XSourceTypeSymbol xdef)
            {
                var xml = xdef.XmlComments;
                var xfile = XSharpXMLDocTools.Firstfile;
                if (xfile != null && !string.IsNullOrEmpty(xml))
                {
                    summary = GetSummary(xml, out returns, out remarks);
                }
                return summary;

            }

            var xtype = (XPETypeSymbol)type;
            var declarationAssembly = xtype.Assembly;

            var file = XSharpXMLDocTools.GetXmlDocFile(declarationAssembly, project);
            if (file == null)
                return null;
            string sig = xtype.XMLSignature;
            if (!string.IsNullOrEmpty(sig))
            {
                uint id = 0;
                string xml = "";
                ThreadHelper.JoinableTaskFactory.Run(async delegate
                {
                    await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();

                    var result = file.ParseMemberSignature(sig, out id);
                    result = file.GetMemberXML(id, out xml);
                });
                summary = GetSummary(xml, out returns, out remarks);
            }
            return summary;
        }

        static public string GetMemberSummary(IXMemberSymbol member, XProject project, out string returns, out string remarks)
        {
            string summary = null;
            returns = remarks = "";
            if (member == null)
                return "";

            //
            if (member is XSourceMemberSymbol xdef)
            {
                var xml = xdef.XmlComments;
                var xfile = XSharpXMLDocTools.Firstfile;
                if (xfile != null && !string.IsNullOrEmpty(xml))
                {
                    summary = GetSummary(xml, out returns, out remarks);
                }
                return summary;

            }
            if (!(member is XPEMemberSymbol))
                return "";

            var xmember = (XPEMemberSymbol)member;
            var declarationAssembly = xmember.Assembly;

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
                    int result = 0;
                    ThreadHelper.JoinableTaskFactory.Run(async delegate
                    {
                        await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                        result = file.ParseMemberSignature(sig, out id);
                        result = file.GetMemberXML(id, out xml);
                    });
                    if (!string.IsNullOrEmpty(xml))
                    {
                        summary = GetSummary(xml, out returns, out remarks);
                    }
                }
            }
            catch (Exception e)
            {
                XSettings.DisplayOutputMessage("Exception in XSharpXMLDocMember.GetDocSummary");
                XSettings.DisplayException(e);
            }
            //
            return summary;
        }


        static private bool GetParameterInfo(IVsXMLMemberIndex file, string xml, IList<string> names, IList<string> descriptions)
        {
            //Microsoft.VisualStudio.Shell.ThreadHelper.ThrowIfNotOnUIThread();
            IVsXMLMemberData data = null;
            int result = 0;
            int numparams = 0;
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                result = file.GetMemberDataFromXML(xml, out data);
                if (result >= 0 && data != null)
                {
                    result = data.GetParamCount(out numparams);
                }
            });
            if (result >= 0 && numparams != 0)
            {
                string paramName = "";
                string paramDesc = "";
                for (int i = 0; i < numparams; i++)
                {
                    ThreadHelper.JoinableTaskFactory.Run(async delegate
                    {
                        await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();

                        result = data.GetParamTextAt(i, out paramName, out paramDesc);
                    });
                    names.Add(paramName);
                    paramDesc = CleanUpResult(paramDesc);
                    descriptions.Add(paramDesc);
                }
            }
            return true;

        }
        static public bool GetMemberParameters(IXMemberSymbol member, XProject project, IList<string> names, IList<string> descriptions)
        {
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                if (member == null)
                    return false;

                if (member is XSourceMemberSymbol xdef)
                {
                    var xml = xdef.XmlComments;
                    var xfile = XSharpXMLDocTools.Firstfile;
                    if (xfile != null && !string.IsNullOrEmpty(xml))
                    {
                        GetParameterInfo(xfile, xml, names, descriptions);
                        return true;
                    }
                    return false;

                }
                if (!(member is XPEMemberSymbol))
                    return false;

                var xmember = (XPEMemberSymbol)member;
                var declarationAssembly = xmember.Assembly;
                var file = XSharpXMLDocTools.GetXmlDocFile(declarationAssembly, project);
                if (file == null)
                {
                    return false;
                }
                try
                {
                    var sig = xmember.XMLSignature;
                    if (!string.IsNullOrEmpty(sig))
                    {
                        var result = file.ParseMemberSignature(sig, out uint id);
                        result = file.GetMemberXML(id, out string xml);
                        GetParameterInfo(file, xml, names, descriptions);
                    }
                }
                catch (Exception e)
                {
                    XSettings.DisplayOutputMessage("Exception in XSharpXMLDocMember.GetDocSummary");
                    XSettings.DisplayException(e);
                    return false;
                }
                return true;
            });
            return false;
        }
    }
}
