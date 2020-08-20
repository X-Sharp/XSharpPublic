//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.ComponentModel.Composition;
using System.Diagnostics;
using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.TextManager.Interop;
using Microsoft.VisualStudio.Utilities;
using Microsoft.VisualStudio.Package;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio.Editor;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text.Operations;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft;

namespace XSharp.LanguageService
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

        [Import]
        internal Microsoft.VisualStudio.Shell.SVsServiceProvider ServiceProvider { get; set; }

        public void VsTextViewCreated(IVsTextView textViewAdapter)
        {
            IVsTextLines textlines;
            textViewAdapter.GetBuffer(out textlines);
            if (textlines != null)
            {
                Guid langId;
                textlines.GetLanguageServiceID(out langId);
                IWpfTextView textView = AdaptersFactory.GetWpfTextView(textViewAdapter);
                Debug.Assert(textView != null);
                // Note that this may get called after the classifier has been instantiated

                if (langId == GuidStrings.guidLanguageService)          // is our language service active ?
                {
                    string fileName = FilePathUtilities.GetFilePath(textlines);
                    

                    // Get XFile and assign it to the textbuffer
                    if (!textView.TextBuffer.Properties.ContainsProperty(typeof(XSharpModel.XFile)))
                    {
                        var file = XSharpModel.XSolution.FindFile(fileName);
                        if (file != null)
                        {
                            textView.TextBuffer.Properties.AddProperty(typeof(XSharpModel.XFile), file);
                            file.Interactive = true;
                        }
                    }
                    CommandFilter filter = new CommandFilter(textView, CompletionBroker, NavigatorService.GetTextStructureNavigator(textView.TextBuffer), SignatureHelpBroker, aggregator, this );
                    IOleCommandTarget next;
                    textViewAdapter.AddCommandFilter(filter, out next);
                    
                    filter.Next = next;
                }
            }
        }
        internal static bool IsOurSourceFile(string fileName)
        {
            return true;
        }
    }

}

