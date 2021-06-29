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
            var lang = XSharpLanguageService.Instance;
            object result = null;
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();

                result = await lang.GetServiceAsync(typeof(SVsXMLMemberIndexService));
                _XMLMemberIndexService = (IVsXMLMemberIndexService)result;
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
        static private string GetSummary(IVsXMLMemberIndex file, string xml, out string returns, out string remarks)
        {
            returns = remarks = "";
            string summary = "";
            IVsXMLMemberData data = null;
            int result = 0;
            string myreturns = "", myremarks = ""; 
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                result = file.GetMemberDataFromXML(xml, out data);
                if (result >= 0 && data != null)
                {
                    result = data.GetSummaryText(out summary);
                    result = data.GetReturnsText(out myreturns);
                    result = data.GetRemarksText(out myremarks);
                }
            });
            summary = CleanUpResult(summary);
            returns = CleanUpResult(myreturns);
            remarks = CleanUpResult(myremarks);
            return summary;
        }
        static string CleanUpResult(string source)
        {
            if (!string.IsNullOrEmpty(source))
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
                    summary = GetSummary(xfile, xml, out returns, out remarks);
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
                summary = GetSummary(file, xml, out returns, out remarks);
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
                    summary = GetSummary(xfile, xml, out returns, out remarks);
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
                        summary = GetSummary(file, xml, out returns, out remarks);
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
