//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.ComponentModel.Composition;
using System.Diagnostics;
using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.TextManager.Interop;
using Microsoft.VisualStudio.Utilities;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Package;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio.Editor;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text.Operations;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Shell.Interop;

namespace XSharp.Project
{
    // This code is used to determine if a file is opened inside a Vulcan project
    // or another project.
    // When the language service is set to our language service then we look for the file in the RDT
    // and ask for a property where we know what X# returns. When then result is different then
    // we assume it is a Vulcan file, and we will set the language service to that from Vulcan.
    // You must make sure that the Project System package is also added as a MEF component to the vsixmanifest
    // otherwise the Export will not work.

    [Export(typeof(IVsTextViewCreationListener))]
    [ContentType("code")]
    [TextViewRole(PredefinedTextViewRoles.Editable)]

    internal class VsTextViewCreationListener : IVsTextViewCreationListener
    {
        [Import]
        IVsEditorAdaptersFactoryService AdaptersFactory = null;

        [Import]
        ICompletionBroker CompletionBroker = null;

        [Import]
        ITextStructureNavigatorSelectorService NavigatorService { get; set; }

        [Import]
        ISignatureHelpBroker SignatureHelpBroker = null;

        [Import]
        IBufferTagAggregatorFactoryService aggregator = null;

        public void VsTextViewCreated(IVsTextView textViewAdapter)
        {
            IVsTextLines textlines;
            textViewAdapter.GetBuffer(out textlines);
            if (textlines != null)
            {
                Guid langId;
                textlines.GetLanguageServiceID(out langId);
                if (langId == GuidStrings.guidLanguageService)          // is our language service active ?
                {
                    string fileName = FilePathUtilities.GetFilePath(textlines);
                    if (!IsOurFile(fileName))       // is this a file node from Vulcan ?
                    {
                        Guid guidVulcanLanguageService = GuidStrings.guidVulcanLanguageService;
                        textlines.SetLanguageServiceID(guidVulcanLanguageService);
                        return;
                    }
                    //
                    // Only capturing keystroke for OUR languageService... ???
                    //
                    IWpfTextView textView = AdaptersFactory.GetWpfTextView(textViewAdapter);
                    Debug.Assert(textView != null);
                    CommandFilter filter = new CommandFilter(textView, CompletionBroker, NavigatorService.GetTextStructureNavigator(textView.TextBuffer), SignatureHelpBroker, aggregator);
                    IOleCommandTarget next;
                    textViewAdapter.AddCommandFilter(filter, out next);
                    filter.Next = next;
                }
            }
        }
        internal static bool IsOurFile(string fileName)
        {
            var serviceProvider = XSharpEditorFactory.GetServiceProvider();
            if (serviceProvider == null)
                return false;
            // Find the document in the Running Document Table and Get Its hierarchy object
            // so we can ask for a property that we can use to see if this is 'Ours'
            IVsRunningDocumentTable rdt = serviceProvider.GetService(typeof(IVsRunningDocumentTable)) as IVsRunningDocumentTable;
            uint itemID;
            IVsHierarchy hierarchy;
            IntPtr unkDocData;
            uint cookie;
            rdt.FindAndLockDocument((uint)(_VSRDTFLAGS.RDT_NoLock), fileName, out hierarchy, out itemID, out unkDocData, out cookie);
            if (unkDocData != IntPtr.Zero)
            {
                Marshal.Release(unkDocData);
            }
            object result;
            bool ours = false;
            // Ask for the Language. X# returns the product name
            // the implementation for this property is inside XSharpFileNode.
            if (hierarchy != null)
            {
                hierarchy.GetProperty(itemID, (int)__VSHPROPID8.VSHPROPID_DiagHubLanguage, out result);
                ours = (result is string && (string)result == Constants.Product);
            }
            if (! ours)
            {
                // this could be a XAML generated source file that is not in the hierarchy
                // in that case it is part of the XSolution and we should be able to find its XAML parent
                var file = XSharpModel.XSolution.FindFullPath(fileName);
                ours = (file != null);
            }
            return ours;
        }
    }

}
