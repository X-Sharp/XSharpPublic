//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Diagnostics;
using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio;
using System.Collections.Generic;
using XSharpModel;
using System.Linq;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft;
using Microsoft.VisualStudio.Shell;

namespace XSharp.LanguageService
{
    internal sealed partial class CommandFilter : IOleCommandTarget
    {
        public ITextView TextView { get; private set; }
        public IOleCommandTarget Next { get; set; }

        readonly XFile _file;



        private int getCurrentLine()
        {
            SnapshotPoint caret = this.TextView.Caret.Position.BufferPosition;
            ITextSnapshotLine line = caret.GetContainingLine();
            return line.LineNumber;
        }
 
        public CommandFilter(IWpfTextView textView, VsTextViewCreationListener provider)
        {

            m_provider = provider;


            TextView = textView;
            _file = textView.TextBuffer.GetFile();
        }

 
 
        internal void WriteOutputMessage(string strMessage)
        {
            if (XSettings.EnableCodeCompletionLog && XSettings.EnableLogging)
            {
                XSettings.DisplayOutputMessage(strMessage);
            }
        }
  

        private readonly VsTextViewCreationListener m_provider;

        public int Exec(ref Guid pguidCmdGroup, uint nCmdID, uint nCmdexecopt, IntPtr pvaIn, IntPtr pvaOut)
        {
            var cmdGrp = pguidCmdGroup;
            bool done = false;
            int result = 0;
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();

                if (Microsoft.VisualStudio.Shell.VsShellUtilities.IsInAutomationFunction(m_provider.ServiceProvider))
                {
                    done = true;
                    result = Next.Exec(ref cmdGrp, nCmdID, nCmdexecopt, pvaIn, pvaOut);
                }
            });
            if (done)
                return result;
            //
            bool handled = false;
            int hresult = VSConstants.S_OK;

            // 1. Pre-process
            if (pguidCmdGroup == VSConstants.VSStd2K)
            {
                switch ((VSConstants.VSStd2KCmdID)nCmdID)
                {
                    case VSConstants.VSStd2KCmdID.HELPKEYWORD:
                    case VSConstants.VSStd2KCmdID.HELP:
                        break;
                }
            }
            else if (pguidCmdGroup == VSConstants.GUID_VSStandardCommandSet97)
            {
                switch ((VSConstants.VSStd97CmdID)nCmdID)
                {
                    case VSConstants.VSStd97CmdID.F1Help:
                    case VSConstants.VSStd97CmdID.WindowHelp:
                        //handled = true;
                        //Todo RvdH Call X# Help
                        break;
                    
                    case VSConstants.VSStd97CmdID.GotoDefn:
                        GotoDefn();
                        return VSConstants.S_OK;
                }
            }

            // 2. Let others do their thing
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();

                hresult = Next.Exec(ref cmdGrp, nCmdID, nCmdexecopt, pvaIn, pvaOut);
            });

            if (ErrorHandler.Succeeded(hresult))
            {
                // 3. Post process
                if (pguidCmdGroup == Microsoft.VisualStudio.VSConstants.VSStd2K)
                {
                    switch ((VSConstants.VSStd2KCmdID)nCmdID)
                    {
  
                        case VSConstants.VSStd2KCmdID.HELP:
                        case VSConstants.VSStd2KCmdID.HELPKEYWORD:
                            break;
                        
                    }
                    //
                    if (handled) return VSConstants.S_OK;
                }
            }
            
            return hresult;
        }




        #region Goto Definition
        private void GotoDefn()
        {
            try
            {
                if (XSettings.DisableGotoDefinition)
                    return;
                var file = this.TextView.TextBuffer.GetFile();
                if (file == null || file.XFileType != XFileType.SourceCode)
                    return;
                WriteOutputMessage("CommandFilter.GotoDefn()");
                XSharpModel.ModelWalker.Suspend();
                // First, where are we ?

                var ssp = this.TextView.Caret.Position.BufferPosition;
                // find next delimiter, so we will include the '{' or '(' in the search
                
                var snapshot = this.TextView.TextBuffer.CurrentSnapshot;
                int caretPos = ssp.Position;
                int lineNumber = ssp.GetContainingLine().LineNumber;
                // Check if we can get the member where we are
                var member = XSharpLookup.FindMember(lineNumber, file);
                if (member == null)
                    return;
                var currentNamespace = XSharpTokenTools.FindNamespace(caretPos, file);

                // Then, the corresponding Type/Element if possible

                // We don't want to lex the buffer. So get the tokens from the last lex run
                // and when these are too old, then simply bail out
                var tokens = this.TextView.TextBuffer.GetTokens();
                if (tokens != null)
                {
                    if (tokens.SnapShot.Version != snapshot.Version)
                        return;
                }
                string currentNS = "";
                if (currentNamespace != null)
                {
                    currentNS = currentNamespace.Name;
                }
                var location = new XSharpSearchLocation(member, snapshot, lineNumber, caretPos,currentNS);
                var state = CompletionState.General;
                var tokenList = XSharpTokenTools.GetTokensUnderCursor(location, tokens.TokenStream, out state);

                // LookUp for the BaseType, reading the TokenList (From left to right)
                var result = new List<IXSymbol>();

                result.AddRange(XSharpLookup.RetrieveElement(location, tokenList,  state));
                //
                Microsoft.VisualStudio.Shell.ThreadHelper.ThrowIfNotOnUIThread();
                if (result.Count > 0) 
                {
                    var element = result[0];
                    if (element is XSourceEntity source)
                    {
                        source.OpenEditor();
                    }
                    else
                    {
                        openInObjectBrowser(element.FullName);
                    }
                    return;
                }
                //
                if (tokenList.Count > 1)
                {
                    // try again with just the last element in the list
                    var token = tokenList[tokenList.Count - 1];
                    tokenList.Clear();
                    tokenList.Add(token);
                    location = location.With(currentNS);
                    result.AddRange(XSharpLookup.RetrieveElement(location, tokenList, state));
                }
                if (result.Count > 0 )
                {
                    var element = result[0];
                    if (element is XSourceEntity source)
                        source.OpenEditor();
                    else
                    {
                        openInObjectBrowser(element.FullName);
                    }
                    return;

                }

            }
            catch (Exception ex)
            {
                WriteOutputMessage("Goto failed: ");
                XSettings.DisplayException(ex);
            }
            finally
            {
                XSharpModel.ModelWalker.Resume();
            }
        }

        private void openInObjectBrowser(string name)
        {
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                ObjectBrowserHelper.FindSymbols(name);
            });
            return;
        }

       
        #endregion



         public int QueryStatus(ref Guid pguidCmdGroup, uint cCmds, OLECMD[] prgCmds, IntPtr pCmdText)
        {
            bool isSource = _file != null && _file.XFileType == XFileType.SourceCode;
            if (pguidCmdGroup == VSConstants.VSStd2K)
            {
                switch ((VSConstants.VSStd2KCmdID)prgCmds[0].cmdID)
                {
                    case VSConstants.VSStd2KCmdID.AUTOCOMPLETE:
                    case VSConstants.VSStd2KCmdID.COMPLETEWORD:
                        if (isSource)
                        {
                            prgCmds[0].cmdf = (uint)OLECMDF.OLECMDF_ENABLED | (uint)OLECMDF.OLECMDF_SUPPORTED;
                            return VSConstants.S_OK;
                        }
                        break;
                }
            }
            else if (pguidCmdGroup == VSConstants.GUID_VSStandardCommandSet97)
            {
                switch ((VSConstants.VSStd97CmdID)prgCmds[0].cmdID)
                {
                    case VSConstants.VSStd97CmdID.GotoDefn:
                        if (isSource)
                        {
                            prgCmds[0].cmdf = (uint)OLECMDF.OLECMDF_ENABLED | (uint)OLECMDF.OLECMDF_SUPPORTED;
                            return VSConstants.S_OK;
                        }
                        break;
                }
            }
            int result = 0;
            var cmdGroup = pguidCmdGroup;
            ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                result = Next.QueryStatus(cmdGroup, cCmds, prgCmds, pCmdText);
            });
            return result;
        }

    }


    internal static class ObjectBrowserHelper
    {
        private static Guid GUID_VsSymbolScope_All = new Guid(0xa5a527ea, 0xcf0a, 0x4abf, 0xb5, 0x1, 0xea, 0xfe, 0x6b, 0x3b, 0xa5, 0xc6);
        private static Guid GUID_VsSymbolScope_Solution = new Guid(0xb1ba9461, 0xfc54, 0x45b3, 0xa4, 0x84, 0xcb, 0x6d, 0xd0, 0xb9, 0x5c, 0x94);
        private static Guid GUID_VsSymbolScope_Frameworks = new Guid(0x3168518c, 0xb7c9, 0x4e0c, 0xbd, 0x51, 0xe3, 0x32, 0x1c, 0xa7, 0xb4, 0xd8);

        /*
        DEFINE_GUID(GUID_VsSymbolScope_All, 0xa5a527ea, 0xcf0a, 0x4abf, 0xb5, 0x1, 0xea, 0xfe, 0x6b, 0x3b, 0xa5, 0xc6);
        DEFINE_GUID(GUID_VsSymbolScope_OBSelectedComponents, 0x41fd0b24, 0x8d2b, 0x48c1, 0xb1, 0xda, 0xaa, 0xcf, 0x13, 0xa5, 0x57, 0xf);
        DEFINE_GUID(GUID_VsSymbolScope_FSSelectedComponents, 0xc2146638, 0xc2fe, 0x4c1e, 0xa4, 0x9d, 0x64, 0xae, 0x97, 0x1e, 0xef, 0x39);
        DEFINE_GUID(GUID_VsSymbolScope_Frameworks, 0x3168518c, 0xb7c9, 0x4e0c, 0xbd, 0x51, 0xe3, 0x32, 0x1c, 0xa7, 0xb4, 0xd8);
        DEFINE_GUID(GUID_VsSymbolScope_Solution, 0xb1ba9461, 0xfc54, 0x45b3, 0xa4, 0x84, 0xcb, 0x6d, 0xd0, 0xb9, 0x5c, 0x94);
        */

        /// <summary>
        ///     If Visual Studio's recognizes the given member and knows where its source code is, goes to the source code.
        ///     Otherwise, opens the "Find Symbols" ToolWindow.
        /// </summary>
        public static void GotoMemberDefinition(string memberName, uint searchOptions = (uint)_VSOBSEARCHOPTIONS.VSOBSO_LOOKINREFS)
        {
            Microsoft.VisualStudio.Shell.ThreadHelper.ThrowIfNotOnUIThread();
            gotoDefinition(memberName, _LIB_LISTTYPE.LLT_MEMBERS, searchOptions);
        }

        public static void GotoClassDefinition(string typeName, uint searchOptions = (uint)_VSOBSEARCHOPTIONS.VSOBSO_LOOKINREFS)
        {
            Microsoft.VisualStudio.Shell.ThreadHelper.ThrowIfNotOnUIThread();
            gotoDefinition(typeName, _LIB_LISTTYPE.LLT_CLASSES, searchOptions);
        }

        public static void FindSymbols(string memberName)
        {
            Microsoft.VisualStudio.Shell.ThreadHelper.ThrowIfNotOnUIThread();
            canFindSymbols(memberName, (uint)_VSOBSEARCHOPTIONS.VSOBSO_LOOKINREFS);
        }


        private static void gotoDefinition(string memberName, _LIB_LISTTYPE libListtype, uint searchOptions)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            if (gotoDefinitionInternal(memberName, libListtype, searchOptions) == false)
            {
                // There was an ambiguity (more than one item found) or no items found at all.
                if (ObjectBrowserHelper.canFindAllSymbols(memberName, searchOptions) == false)
                {
                    Debug.WriteLine("Failed to FindSymbol for symbol " + memberName);
                }
            }
        }

        private static bool gotoDefinitionInternal(string typeOrMemberName, _LIB_LISTTYPE symbolType, uint searchOptions)
        {
            ThreadHelper.ThrowIfNotOnUIThread();
            if (tryFindSymbol(typeOrMemberName, out IVsSimpleObjectList2 list, symbolType, searchOptions))
            {
                const VSOBJGOTOSRCTYPE whereToGo = VSOBJGOTOSRCTYPE.GS_DEFINITION;
                //
                return HResult.Succeeded(list.CanGoToSource(0, whereToGo, out int ok)) &&
                       HResult.Succeeded(ok) &&
                       HResult.Succeeded(list.GoToSource(0, whereToGo));
            }

            return false;
        }

        // Searching in the XSharp Library (Current Solution)

        // Searching in the XSharp Library (Current Solution)
        private static IVsSimpleLibrary2 GetXSharpLibrary()
        {
            Microsoft.VisualStudio.Shell.ThreadHelper.ThrowIfNotOnUIThread();
            Guid guid = new Guid(XSharpConstants.Library);
            IVsSimpleLibrary2 simpleLibrary = null;
            //
            System.IServiceProvider provider = XSharpLanguageService.Instance;
            // ProjectPackage already switches to UI thread inside GetService
            if (provider.GetService(typeof(SVsObjectManager)) is IVsObjectManager2 mgr)
            {
                ErrorHandler.ThrowOnFailure(mgr.FindLibrary(ref guid, out IVsLibrary2 _library));
                simpleLibrary = _library as IVsSimpleLibrary2;
            }
            return simpleLibrary;
        }

        //private static bool tryGetSourceLocation(string memberName, out string fileName, out uint line, uint searchOptions)
        //{
        //    ThreadHelper.ThrowIfNotOnUIThread();
        //    if (tryFindSymbol(memberName, out IVsSimpleObjectList2 list, _LIB_LISTTYPE.LLT_MEMBERS, searchOptions))
        //    {
        //        return HResult.Succeeded(list.GetSourceContextWithOwnership(0, out fileName, out line));
        //    }

        //    fileName = null;
        //    line = 0;
        //    return false;
        //}

        /// <summary>
        ///     Tries to find a member (field/property/event/methods/etc).
        /// </summary>
        /// <param name="typeOrMemberName">The type or member we are searching for</param>
        /// <param name="resultList">An IVsSimpleObjectList2 which contains a single result.</param>
        /// <param name="symbolType">The type of symbol we are looking for (member/class/etc)</param>
        /// <returns>
        ///     True if a unique match was found. False if the member does not exist or there was an ambiguity
        ///     (more than one member matched the search term).
        /// </returns>
        private static bool tryFindSymbol(string typeOrMemberName,
            out IVsSimpleObjectList2 resultList,
            _LIB_LISTTYPE symbolType,
            uint searchOptions)
        {
            try
            {
                // The Visual Studio API we're using here breaks with superfulous spaces
                typeOrMemberName = typeOrMemberName.Replace(" ", "");
                var library = ObjectBrowserHelper.GetXSharpLibrary();
                ThreadHelper.ThrowIfNotOnUIThread();
                var searchSucceed = HResult.Succeeded(library.GetList2((uint)symbolType,
                    (uint)_LIB_LISTFLAGS.LLF_USESEARCHFILTER,
                    createSearchCriteria(typeOrMemberName, searchOptions),
                    out IVsSimpleObjectList2 list));
                if (searchSucceed && list != null)
                {
                    // Check if there is an ambiguity (where there is more than one symbol that matches)
                    if (getSymbolNames(list).Distinct().Count() == 1)
                    {
                        list.GetItemCount(out uint count);
                        if (count > 1)
                        {
                            list.CanDelete((uint)1, out int ok);
                        }
                        resultList = list;
                        return true;
                    }
                }
            }
            catch (AccessViolationException e)
            {
                /* eat this type of exception (ripped from original implementation) */
                Debug.WriteLine(e.Message);
            }

            resultList = null;
            return false;
        }

        private static bool canFindSymbols(string memberName, uint searchOptions)
        {
            Microsoft.VisualStudio.Shell.ThreadHelper.ThrowIfNotOnUIThread();
            System.IServiceProvider provider = XSharpLanguageService.Instance;
            bool result ;
            // ProjectPackage already switches to UI thread inside GetService
            IVsFindSymbol searcher = provider.GetService(typeof(SVsObjectSearch)) as IVsFindSymbol;
            Assumes.Present(searcher);
            var guidSymbolScope = new Guid(XSharpConstants.Library);
            result = HResult.Succeeded(searcher.DoSearch(ref guidSymbolScope, createSearchCriteria(memberName, searchOptions)));
            return result;
        }

        private static bool canFindAllSymbols(string memberName, uint searchOptions)
        {
            Microsoft.VisualStudio.Shell.ThreadHelper.ThrowIfNotOnUIThread();
            System.IServiceProvider provider = XSharpLanguageService.Instance;
            bool result ;
            // ProjectPackage already switches to UI thread inside GetService
            IVsFindSymbol searcher = provider.GetService(typeof(SVsObjectSearch)) as IVsFindSymbol;
            Assumes.Present(searcher);
            var guidSymbolScope = GUID_VsSymbolScope_All;
            //
            result = HResult.Succeeded(searcher.DoSearch(ref guidSymbolScope, createSearchCriteria(memberName, searchOptions)));
            return result;
        }

        private static VSOBSEARCHCRITERIA2[] createSearchCriteria(string typeOrMemberName, uint searchOptions)
        {
            return new[]
            {
                new VSOBSEARCHCRITERIA2
                {
                    eSrchType = VSOBSEARCHTYPE.SO_ENTIREWORD,
                    //eSrchType = VSOBSEARCHTYPE.SO_PRESTRING,
                    //grfOptions = (uint)_VSOBSEARCHOPTIONS.VSOBSO_LOOKINREFS,
                    grfOptions = searchOptions,
                    szName = typeOrMemberName
                }
            };
        }

        private static IEnumerable<string> getSymbolNames(IVsSimpleObjectList2 list)
        {
            Microsoft.VisualStudio.Shell.ThreadHelper.ThrowIfNotOnUIThread();
            if (HResult.Succeeded(list.GetItemCount(out uint count)))
            {
                for (uint i = 0; i < count; i++)
                {

                    if (HResult.Succeeded(list.GetProperty(i,
                        (int)_VSOBJLISTELEMPROPID.VSOBJLISTELEMPROPID_FULLNAME,
                        out object symbol)))
                    {
                        yield return (string)symbol;
                    }
                }
            }
        }
    }

    internal static class HResult
    {
        internal static bool Succeeded(int v)
        {
            return (v == VSConstants.S_OK);
        }
    }

}
