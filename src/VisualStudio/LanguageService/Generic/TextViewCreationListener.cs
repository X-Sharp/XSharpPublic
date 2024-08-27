//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Editor;
using Microsoft.VisualStudio.Package;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.TextManager.Interop;
using Microsoft.VisualStudio.Utilities;
using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using XSharpModel;
#pragma warning disable CS0649 // Field is never assigned to, for the imported fields
namespace XSharp.LanguageService
{
    [Name(nameof(XSharpEditorCommandProvider))]
    [Export(typeof(IVsTextViewCreationListener))]
    [ContentType(Constants.LanguageName)]
    [TextViewRole(PredefinedTextViewRoles.Document)]
    [TextViewRole(PredefinedTextViewRoles.Editable)]

    internal class XSharpEditorCommandProvider : IVsTextViewCreationListener
    {
        [Import] IEditorOptionsFactoryService editorOptionsService;

        private IEditorOptions editorOptions;

        [Import]
        internal IVsEditorAdaptersFactoryService EditorAdaptersFactoryService { get; set; }

        [Import]
        internal Microsoft.VisualStudio.Shell.SVsServiceProvider ServiceProvider { get; set; }

        [Import]
        internal IBufferTagAggregatorFactoryService BufferTagAggregatorFactoryService { get; set; }
        // the default key is the VS2019 key.
        internal static object dropDownBarKey = typeof(IVsCodeWindow);

        internal static Dictionary<string, XSharpDropDownClient> _dropDowns = new Dictionary<string, XSharpDropDownClient>(StringComparer.OrdinalIgnoreCase);
        internal static Dictionary<string, List<IWpfTextView>> _textViews = new Dictionary<string, List<IWpfTextView>>(StringComparer.OrdinalIgnoreCase);

        private XSharpDropDownClient dropdown;
        private void EditorOptions_OptionChanged(object sender, EditorOptionChangedEventArgs e)
        {
            return;
        }
        public void VsTextViewCreated(IVsTextView textViewAdapter)
        {
            IWpfTextView textView = EditorAdaptersFactoryService.GetWpfTextView(textViewAdapter);
            if (textView == null)
                return;
            editorOptions = editorOptionsService.GetOptions(textView);
            editorOptions.OptionChanged += EditorOptions_OptionChanged;
            // Create command handler for Formatting (case sync and indenting)
            textView.Properties.GetOrCreateSingletonProperty(() => new XSharpFormattingCommandHandler(textViewAdapter,
                                                                 textView,
                                                                    BufferTagAggregatorFactoryService
                                                                    ));
            IVsTextBuffer textBuffer = EditorAdaptersFactoryService.GetBufferAdapter(textView.TextBuffer);
            textView.TextBuffer.Properties.TryGetProperty(typeof(ITextDocument), out ITextDocument document);
            textViewAdapter.GetBuffer(out var textlines);
            if (textlines != null)
            {
                XFile file = null;
                string fileName = FilePathUtilities.GetFilePath(textlines);
                textlines.GetLanguageServiceID(out var langId);
                // Note that this may get called after the classifier has been instantiated

                if (langId == XSharpConstants.guidLanguageService)          // is our language service active ?
                {
                    // Get XFile and assign it to the TextBuffer
                    if (!textView.TextBuffer.Properties.TryGetProperty(typeof(XFile), out file))
                    {
                        file = XSolution.FindFile(fileName);

                        if (file == null)
                        {
                            XSolution.OrphanedFilesProject.AddFile(fileName);
                            file = XSolution.FindFile(fileName);
                        }
                        
                    }
                    if (file != null)
                    {
                        var projects = XDatabase.GetProjectsPerFile(file);
                        if (projects.Count > 1)
                        {
                            ThreadHelper.JoinableTaskFactory.Run(async delegate
                            {
                                var proj = await VS.Solutions.GetActiveProjectAsync();
                                if (proj != null)
                                {
                                    var xproj = XSolution.FindProject(proj.FullPath);
                                    if (xproj != null)
                                    {
                                        if (file.Project.FileName != xproj.FileName)
                                        {
                                            file.Project = xproj;
                                        }
                                    }
                                }
                            });
                        }
                        file.Interactive = true;
                        textView.Properties.AddProperty(typeof(XFile), file);
                    }
                }
                // For VS 2017 we look for Microsoft.VisualStudio.Editor.Implementation.VsCodeWindowAdapter
                // For VS 2019 we look for Microsoft.VisualStudio.TextManager.Interop.IVsCodeWindow
                // Both implement IVsDropdownbarManager
                IVsDropdownBarManager dropDownBarManager = null;
                if (dropDownBarKey != null && textView.Properties.ContainsProperty(dropDownBarKey))
                {
                    object window = textView.Properties.GetProperty(dropDownBarKey);
                    dropDownBarManager = window as IVsDropdownBarManager;
                }
                if (dropDownBarManager == null)
                {
                    // look at all the properties to find the one that implements IVsDropdownBarManager
                    foreach (var property in textView.Properties.PropertyList)
                    {
                        if (property.Value is IVsDropdownBarManager manager)
                        {
                            dropDownBarKey = property.Key;  // remember key for next iteration
                            dropDownBarManager = manager;
                            break;
                        }
                    }
                }
                // The same file may be open in multiple textViews
                // these will share the same dropdown
                if (_dropDowns.ContainsKey(fileName))
                {
                    dropdown = _dropDowns[fileName];
                    dropdown.addTextView(textView, textViewAdapter);
                }
                else if (dropDownBarManager != null)
                {
                    dropdown = new XSharpDropDownClient(dropDownBarManager, file);
                    dropDownBarManager.RemoveDropdownBar();
                    dropDownBarManager.AddDropdownBar(3, dropdown);
                    _dropDowns.Add(fileName, dropdown);
                    dropdown.addTextView(textView, textViewAdapter);
                }
                if (!_textViews.ContainsKey(fileName))
                {
                    _textViews.Add(fileName, new List<IWpfTextView>());
                }
                _textViews[fileName].Add(textView);
                textView.Closed += TextView_Closed;
            }

        }

        private void TextView_Closed(object sender, EventArgs e)
        {
            // It is possible that the same file is open in 2 windows (split window)
            // These will share the same dropdown.
            // That is why when closing a textView we need to check to see if there are still textViews left
            // for the same file.
            if (sender is IWpfTextView textView)
            {
                textView.Closed -= TextView_Closed;
                if (textView.Properties.TryGetProperty<XFile>(typeof(XFile), out var xFile))
                {
                    var fileName = xFile.FullPath;
                    var list = _textViews[fileName];
                    if (list.Contains(textView))
                    {
                        list.Remove(textView);
                    }
                    if (list.Count == 0)
                    {
                        _textViews.Remove(fileName);
                        _dropDowns.Remove(fileName);
                    }
                }
            }
        }
    }
}

