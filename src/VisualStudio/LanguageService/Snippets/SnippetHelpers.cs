//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.TextManager.Interop;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Editor;
using System.Runtime.InteropServices;

namespace XSharp.LanguageService
{
    internal class SnippetHelpers
    {
        /// <summary>
        /// This structure is used to facilitate the interop calls with IVsExpansionEnumeration.
        /// </summary>
        [StructLayout(LayoutKind.Sequential)]
        private struct VsExpansionWithIntPtrs
        {
            public IntPtr PathPtr;
            public IntPtr TitlePtr;
            public IntPtr ShortcutPtr;
            public IntPtr DescriptionPtr;
        }
        internal static VsExpansion[] FindSnippets(string matching)
        {
            GetSnippets();
            var list = new List<VsExpansion>();
            if (snippets != null)
            {
                foreach (var snippet in snippets)
                {
                    if (snippet.shortcut.IndexOf(matching, 0, StringComparison.OrdinalIgnoreCase) >= 0 ||
                        snippet.title.IndexOf(matching, 0, StringComparison.OrdinalIgnoreCase) >= 0)
                    {
                        list.Add(snippet);
                    }
                }
            }
            return list.ToArray();
        }
       
        static VsExpansion[] snippets = null;
        internal static void GetSnippets()
        {
            if (snippets != null)
                return;
            ThreadHelper.JoinableTaskFactory.Run(async () =>
            {
                var txtManager = await VS.GetServiceAsync<SVsTextManager, IVsTextManager2>();
                if (txtManager.GetExpansionManager(out var expansionManager) == VSConstants.S_OK)
                {
                    var manager = (IExpansionManager)expansionManager;
                    var expansionEnumerator = await manager.EnumerateExpansionsAsync(
                        XSharpConstants.guidLanguageService,
                        0, // shortCutOnly
                        Array.Empty<string>(), // types
                        0, // countTypes
                        1, // includeNULLTypes
                        1 // includeDulicates: Allows snippets with the same title but different shortcuts
                        ).ConfigureAwait(false);

                    var snippetInfo = new VsExpansion();
                    var pSnippetInfo = new IntPtr[1];
                    var list = new List<VsExpansion>();
                    try
                    {

                        pSnippetInfo[0] = Marshal.AllocCoTaskMem(Marshal.SizeOf(snippetInfo));
                        expansionEnumerator.GetCount(out var count);
                        for (uint i = 0; i < count; i++)
                        {
                            expansionEnumerator.Next(1, pSnippetInfo, out var fetched);
                            if (fetched > 0)
                            {
                                snippetInfo = ConvertToVsExpansionAndFree(pSnippetInfo[0]);

                                list.Add(snippetInfo);
                            }
                        }
                    }
                    finally
                    {
                        Marshal.FreeCoTaskMem(pSnippetInfo[0]);
                    }
                    snippets = list.ToArray();

                }
            });
        }

        private static VsExpansion ConvertToVsExpansionAndFree(IntPtr expansionPtr)
        {
            var buffer = (VsExpansionWithIntPtrs)Marshal.PtrToStructure(expansionPtr, typeof(VsExpansionWithIntPtrs));
            var expansion = new VsExpansion();

            ConvertToStringAndFree(ref buffer.DescriptionPtr, ref expansion.description);
            ConvertToStringAndFree(ref buffer.PathPtr, ref expansion.path);
            ConvertToStringAndFree(ref buffer.ShortcutPtr, ref expansion.shortcut);
            ConvertToStringAndFree(ref buffer.TitlePtr, ref expansion.title);

            return expansion;
        }
        private static void ConvertToStringAndFree(ref IntPtr ptr, ref string str)
        {
            if (ptr != IntPtr.Zero)
            {
                str = Marshal.PtrToStringBSTR(ptr);
                Marshal.FreeBSTR(ptr);
                ptr = IntPtr.Zero;
            }
        }
    }
}

