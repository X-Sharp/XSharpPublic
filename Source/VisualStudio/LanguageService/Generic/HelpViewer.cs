//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Text.Editor;
using System;
using System.IO;
using System.Linq;
using System.Windows.Forms;
using XSharpModel;
namespace XSharp.LanguageService
{

    internal static class HelpViewer
    {
        internal static string HelpPath = "";
        internal static string GeneralHelp = "";
        internal static string RefHelp = "";
        internal static string GetXsPath()
        {
            string REG_KEY = @"HKEY_LOCAL_MACHINE\" + XSharp.Constants.RegistryKey;
            string InstallPath = (string)Microsoft.Win32.Registry.GetValue(REG_KEY, XSharp.Constants.RegistryValue, "");
            if (string.IsNullOrEmpty(InstallPath))
            {
                REG_KEY = @"HKEY_LOCAL_MACHINE\" + XSharp.Constants.RegistryKey64;
                InstallPath = (string)Microsoft.Win32.Registry.GetValue(REG_KEY, XSharp.Constants.RegistryValue, "");

            }
            return InstallPath;
        }
        static HelpViewer()
        {
            HelpPath = Path.Combine(GetXsPath(),"Help");
            RefHelp = Path.Combine(HelpPath, "XSharpRef.chm");
            GeneralHelp = Path.Combine(HelpPath, "XSharp.chm");
        }

        internal static bool IsAssemblyXSharp(XAssembly asm)
        {
            return asm.FileName.IndexOf("XSharp", StringComparison.OrdinalIgnoreCase) >= 0 ||
                        asm.FileName.IndexOf("Vulcan", StringComparison.OrdinalIgnoreCase) >= 0;
        }
        internal static bool IsAssemblyMs(XAssembly asm)
        {
            return asm.FileName.IndexOf("System", StringComparison.OrdinalIgnoreCase) >= 0 ||
                        asm.FileName.IndexOf("/Microsoft", StringComparison.OrdinalIgnoreCase) >= 0 ||
                        asm.FileName.IndexOf("\\Microsoft", StringComparison.OrdinalIgnoreCase) >= 0;
        }
        internal static void ShowHelp(ITextView TextView, uint nCmdID, object param)
        {
            string sig = null;
            bool showTopic = false;
            if (param is string str)
            {
                sig = str;
            }
            else if (param is IXSymbol symbol)
            {
                bool IsExternalSymbol = false;
                bool IsXSharp = true;
                bool IsMs = false;
                sig = symbol.FullName;
                sig = sig.Replace("@@", "");
                switch (symbol)
                {
                    case XPESymbol peSymbol:
                        // Check to see in which help collection we want to search
                        IsXSharp = IsAssemblyXSharp(peSymbol.Assembly);
                        IsMs = IsAssemblyMs(peSymbol.Assembly);
                        IsExternalSymbol = true;
                        break;
                    case XSourceSymbol source:
                        if (source.File.FullPath.EndsWith(XSolution.BuiltInFunctions_prg))
                        {
                            sig = source.Name;
                        }
                        else
                        {
                            sig = null;
                        }
                        break;
                    case XKeywordSymbol kw:
                        showTopic = true;
                        sig = symbol.Name;
                        break;
                    default:
                        break;
                }
                if (IsExternalSymbol && IsXSharp)
                {
                    switch (symbol.Kind)
                    {
                        case Kind.Namespace:
                            {
                                var doc = TextView.TextBuffer.GetDocument();
                                // Find the location of the namespace and the list of external assemblies
                                var ent = doc.Entities.FirstOrDefault();
                                if (ent != null)
                                {
                                    var proj = ent.File.Project;
                                    var refs = proj.DependentAssemblyList;
                                    var list = XDatabase.GetAssembliesContainingNamespace(symbol.FullName, refs);
                                    if (list.Count > 0)
                                    {
                                        var asm = proj.AssemblyReferences.Where((a) => a.FileName == list.First()).FirstOrDefault();
                                        if (asm != null)
                                        {
                                            IsXSharp = IsAssemblyXSharp(asm);
                                            IsMs = IsAssemblyMs(asm);
                                        }
                                    }
                                }


                            }
                            if (IsXSharp)
                                sig = "N_" + sig;
                            break;
                        case Kind.Class:
                        case Kind.Delegate:
                        case Kind.VOStruct:
                        case Kind.Enum:
                        case Kind.Union:
                        case Kind.Structure:
                            sig = "T_" + sig;
                            break;
                        case Kind.Method:
                        case Kind.Function:
                        case Kind.Destructor:
                        case Kind.Constructor:
                            sig = "M_" + sig;
                            if (symbol is XPEMemberSymbol memsym)
                            {
                                var type = memsym.ParentType;
                                var count = type.AllMembers.Count((m) => m.Name == memsym.Name);
                                if (count > 1)
                                {
                                    sig = "Overload" + sig.Substring(1);
                                }
                            }
                            break;
                        case Kind.Field:
                            sig = "F_" + sig;
                            break;
                        case Kind.Property:
                            sig = "P_" + sig;
                            if (symbol is XPEPropertySymbol propsym)
                            {
                                var type = propsym.ParentType;
                                var count = type.AllMembers.Count((m) => m.Name == propsym.Name);
                                if (count > 1)
                                {
                                    sig = "Overload" + sig.Substring(1);
                                }
                            }
                            break;
                        case Kind.Event:
                            sig = "E_" + sig;
                            break;
                        default:
                            break;
                    }
                }
                if (!string.IsNullOrEmpty(sig) && IsExternalSymbol)
                {
                    if (IsXSharp)
                    {
                        sig = sig.Replace('.', '_');
                        sig = sig.Replace(':', '_');
                        sig = "html\\" + sig + ".htm";
                        //sig = "ms-its:XSharpRef.chm::/" + sig;
                        Help.ShowHelp(null, RefHelp, HelpNavigator.Topic, sig);
                    }
                    else if (IsMs)
                    {
                        sig = "https://learn.microsoft.com/en-us/dotnet/api/" + sig;
                        System.Diagnostics.Process.Start(sig);
                    }
                    else
                    {
                        VS.MessageBox.Show($"No known help location for {sig}");
                    }
                    return;
                }
            }
            if (string.IsNullOrEmpty(sig))
            {
                sig = "visual-studio-integration.html";
            }
            if (showTopic)
                Help.ShowHelp(null, GeneralHelp, HelpNavigator.Topic, sig);
            else
                Help.ShowHelp(null, GeneralHelp, HelpNavigator.TableOfContents);
        }
    }
}
