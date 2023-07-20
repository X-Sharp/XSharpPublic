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
using XSharp.Settings;
namespace XSharp.LanguageService
{
    // No need to do a complicated lookup for the reference assembly names
    // Our system type controller has these already
    static public class XSharpXMLDocTools
    {
        static readonly Dictionary<string, IVsXMLMemberIndex> _memberIndexes;
        static IVsXMLMemberIndexService _XMLMemberIndexService ;
        static string coreLoc ;
        static IVsXMLMemberIndex coreIndex ;
        static XSharpXMLDocTools()
        {
            _memberIndexes = new Dictionary<string, IVsXMLMemberIndex>();
            _XMLMemberIndexService = null;
            coreLoc = "";
            coreIndex = null;
        }

        static bool GetIndex()
        {
            if (_XMLMemberIndexService == null)
            {
                ThreadHelper.JoinableTaskFactory.Run(async ( )=>
                {
                    _XMLMemberIndexService = await VS.GetServiceAsync<SVsXMLMemberIndexService, IVsXMLMemberIndexService>();
                });
            }
            return _XMLMemberIndexService != null;

        }
        public static void Initialize()
        {
            ; // Do nothing. This just triggers the Class Constructor
        }
        private static async Task LoadCoreDllAsync()
        {
            var node = IntPtr.Size == 8 ? Constants.RegistryKey64 : Constants.RegistryKey;
            node = "HKEY_LOCAL_MACHINE\\" + node;
            var InstallPath = (string)Microsoft.Win32.Registry.GetValue(node, "XSharpPath", "");
            var assemblies = Path.Combine(InstallPath, "Assemblies");
            coreLoc = Path.Combine(assemblies, "XSharp.Core.dll");
            coreIndex = null;
            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
            if (GetIndex())
            {
                _XMLMemberIndexService.CreateXMLMemberIndex(coreLoc, out IVsXMLMemberIndex index);
                if (index != null)
                {
                    _memberIndexes.Add(coreLoc, index);
                    coreIndex = index;
                }
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
                    ThreadHelper.JoinableTaskFactory.Run(async ( )=>
                    {
                        await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                        if (GetIndex())
                        {
                            _XMLMemberIndexService.CreateXMLMemberIndex(location, out index);
                            if (index != null)
                            {
                                _memberIndexes.Add(location, index);
                            }
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
                        ThreadHelper.JoinableTaskFactory.Run(async ( )=>
                        {
                            await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                            if (GetIndex())
                            {
                                _XMLMemberIndexService.CreateXMLMemberIndex(refasm, out index);
                            }
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
            var tokens = str.Split(new char[]{' ','\t'});
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
                result += $"</{token}>\r\n";
            }
            else
            {
                foreach (var element in MemoLines(line, 70))
                {
                    result += "\r\n" + element;
                }
                result += $"\r\n</{token}>\r\n";
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
                return ThreadHelper.JoinableTaskFactory.Run(async ( )=>
                {
                    await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                    StringBuilder sb = new StringBuilder();

                    var result = file.ParseMemberSignature(key, out id);
                    result = file.GetMemberXML(id, out xml);
                    var summary = GetSummary(xml, out var returns, out var remarks);
                    if (! string.IsNullOrWhiteSpace(summary))
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
                            summary = decodeChildNodes(child);
                            break;
                        case "returns":
                            myreturns = decodeChildNodes(child);
                            break;
                        case "remarks":
                            myremarks = decodeChildNodes(child);
                            break;
                    }
                }
                summary = CleanUpResult(summary);
                returns = CleanUpResult(myreturns);
                remarks = CleanUpResult(myremarks);
            }
            catch (Exception e)
            {
                Logger.Exception(e, "Exception in XSharpXMLDocMember.GetSummary");
                summary = "** Invalid XML comment ** \r"+e.Message;

            }
            return summary;

        }
        static string decodeAttributes(XmlNode node)
        {

            if (node.Attributes.Count > 0)
            {
                var sb = new StringBuilder();
                foreach (XmlAttribute attribute in node.Attributes)
                {
                    switch (attribute.Name)
                    {
                        case "href":
                        case "cref":
                            var text = attribute.InnerText;
                            if (text.Length > 2 && text[1] == ':')
                            {
                                sb.Append(text.Substring(2));
                            }
                            else
                            {
                                sb.Append(text);
                            }
                            break;
                        default:
                            sb.Append(attribute.InnerText);
                            break;
                    }
                }
                return sb.ToString();

            }
            return node.InnerText;
        }
        static string decodeChildNodes(XmlNode node)
        {
            if (node.HasChildNodes)
            {
                var sb = new StringBuilder();
                foreach (var child in node.ChildNodes)
                {
                    if (child is XmlNode cnode)
                    {
                        switch (cnode.Name)
                        {
                            case "see":
                                if (cnode.InnerText.Length > 0)
                                    sb.Append(cnode.InnerText);
                                else
                                    sb.Append(decodeAttributes(cnode));
                                break;
                            case "cref":
                                break;
                            case "br":
                                sb.Append("<br />");
                                break;
                            case "paramref":
                                sb.Append("'"+cnode.Attributes.GetNamedItem("name").InnerText+"'");
                                break;
                            default:
                                sb.Append(decodeChildNodes(cnode));
                                break;
                        }
                    }
                    else if (child is XmlElement el)
                    {
                        if (el.InnerText.Length == 0)
                        {
                            sb.Append(decodeAttributes(el));
                        }
                        else
                        {
                            sb.Append(el.InnerText);
                        }
                    }
                }
                return sb.ToString();
            }
            if (node.InnerText.Length > 0)
                return node.InnerText;
            else
                return decodeAttributes(node);
        }
        /// <summary>
        /// Clean up HTML string: remove unneeded spaces and CR, LF, TAB chars.
        /// Change Dot space to Dot Space CRLF
        /// </summary>
        /// <param name="source"></param>
        /// <returns></returns>
        static string CleanUpResult(string source)
        {
            if (!string.IsNullOrEmpty(source))
            {
                source = source.Trim();
                var sb = new StringBuilder();
                var lastchar = '\0';
                foreach (var ch in source)
                {
                    switch (ch)
                    {
                        case ' ':
                            if (lastchar == '.')
                            {
                                sb.Append(ch);          // '.' ' ' will be followed by CRLF
                                lastchar = ch;
                                sb.Append("\r");
                            }
                            if (lastchar != ' ' && lastchar != '>')  // no space after '>' or after another space
                            {
                                lastchar = ' ';          
                                sb.Append(lastchar);
                            }
                            break;
                        case '\r':
                        case '\n':
                        case '\t':
                            if (lastchar != ' ')
                            {
                                lastchar = ' ';
                                sb.Append(lastchar);
                            }
                            break;
                        case '<':
                            if (lastchar == ' ')     // replace ' ' '<' with '<'
                            {
                                sb.Remove(sb.Length - 1, 1);
                            }
                            sb.Append(ch);
                            lastchar = ch;
                            break;
                        default:
                            sb.Append(ch);
                            lastchar = ch;
                            break;
                    }
                }
                sb.Replace("<br />", "\r");
                source = sb.ToString();
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
                ThreadHelper.JoinableTaskFactory.Run(async ( )=>
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
                    ThreadHelper.JoinableTaskFactory.Run(async ( )=>
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
                Logger.Exception(e, "Exception in XSharpXMLDocMember.GetDocSummary");
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
            ThreadHelper.JoinableTaskFactory.Run(async ( )=>
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
                    ThreadHelper.JoinableTaskFactory.Run(async ( )=>
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
            ThreadHelper.JoinableTaskFactory.Run(async ( )=>
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
                    Logger.Exception(e, "Exception in XSharpXMLDocMember.GetDocSummary");
                    return false;
                }
                return true;
            });
            return false;
        }
    }
}
