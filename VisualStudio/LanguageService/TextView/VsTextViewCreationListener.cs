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
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Outlining;
using XSharpModel;

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
    [ContentType(Constants.LanguageName)]
    [TextViewRole(PredefinedTextViewRoles.Document)]

    internal class VsTextViewCreationListener : IVsTextViewCreationListener
    {
        [Import]
        internal IOutliningManagerService OutliningManagerService { get; set; }

        [ImportMany]
        internal ISmartIndentProvider[] SmartIndentProviders { get; set; }

        [Import]
        internal IVsEditorAdaptersFactoryService EditorAdaptersFactoryService { get; set; }

        [Import]
        internal ICompletionBroker CompletionBroker { get; set; }

        [Import]
        internal ITextStructureNavigatorSelectorService TextStructureNavigatorSelectorService { get; set; }

        [Import]
        internal ISignatureHelpBroker SignatureHelpBroker { get; set; }

        [Import]
        internal IBufferTagAggregatorFactoryService BufferTagAggregatorFactoryService { get; set; }

        [Import]
        internal Microsoft.VisualStudio.Shell.SVsServiceProvider ServiceProvider { get; set; }

        [Import]
        internal ITextEditorFactoryService TextEditorFactory { get; set; }

        [Import]
        internal ITextBufferFactoryService TextBufferFactory { get; set; }

        [Import]
        internal IContentTypeRegistryService ContentTypeRegistry { get; set; }
        [Import]
        internal ITextSearchService TextSearchService { get; set; }

        [Import(typeof(ITextStructureNavigatorSelectorService))]
        internal ITextStructureNavigatorSelectorService NavigatorService { get; set; }

        public void VsTextViewCreated(IVsTextView textViewAdapter)
        {
            IVsTextLines textlines;
            IWpfTextView textView = EditorAdaptersFactoryService.GetWpfTextView(textViewAdapter);
            IVsTextBuffer textBuffer = EditorAdaptersFactoryService.GetBufferAdapter(textView.TextBuffer);
            textView.TextBuffer.Properties.TryGetProperty(typeof(ITextDocument), out ITextDocument document);

            XFile file = null; 
            textViewAdapter.GetBuffer(out textlines);
            if (textlines != null)
            {
                Guid langId;
                textlines.GetLanguageServiceID(out langId);
                // Note that this may get called after the classifier has been instantiated

                if (langId == GuidStrings.guidLanguageService)          // is our language service active ?
                {
                    string fileName = FilePathUtilities.GetFilePath(textlines);


                    // Get XFile and assign it to the textbuffer
                    if (!textView.TextBuffer.Properties.TryGetProperty(typeof(XFile), out file))
                    {
                        file = XSharpModel.XSolution.FindFile(fileName);
                        if (file == null)
                        {
                            XSolution.OrphanedFilesProject.AddFile(fileName);
                            file = XSharpModel.XSolution.FindFile(fileName);
                        }
                        if (file != null)
                        {
                            textView.TextBuffer.Properties.AddProperty(typeof(XFile), file);
                        }
                    }
                    
                    if (file != null)
                    {
                        file.Interactive = true;
                    }
                    CommandFilter filter = new CommandFilter(textView, CompletionBroker, SignatureHelpBroker, BufferTagAggregatorFactoryService, this );
                    IOleCommandTarget next;
                    textViewAdapter.AddCommandFilter(filter, out next);
                    
                    filter.Next = next;
                }
            }
            IVsCodeWindow codeWindow;
            if (textView.Properties.TryGetProperty(typeof(IVsCodeWindow), out codeWindow))
            {
                IVsDropdownBarManager dropDownBarManager = codeWindow as IVsDropdownBarManager;
                if (dropDownBarManager != null)
                {
                    var dropDownClient = new XSharpDropDownClient(textView, codeWindow, file);
                    dropDownBarManager.RemoveDropdownBar();
                    dropDownBarManager.AddDropdownBar(2, dropDownClient);
                    textView.Properties.AddProperty(typeof(IVsDropdownBarManager), dropDownBarManager);
                    new TextViewClosedHandlerHelper(typeof(IVsDropdownBarManager), textView);
                }
            }
        }
        internal static bool IsOurSourceFile(string fileName)
        {
            return true;
        }
        public class TextViewClosedHandlerHelper
        {
            private Type _targetType;

            private ITextView _textView;

            public TextViewClosedHandlerHelper(Type targetType, ITextView textView)
            {
                _targetType = targetType;
                _textView = textView;
                _textView.Closed += Run;
            }

            public void Run(object sender, EventArgs args)
            {
                _textView.Properties.RemoveProperty(_targetType);
                _textView.Closed -= Run;
            }
        }

    }

}

