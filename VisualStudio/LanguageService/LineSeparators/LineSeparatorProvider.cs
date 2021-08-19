//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;
using XSharpModel;

namespace XSharp.LanguageService.LineSeparators
{
    [Export(typeof(IWpfTextViewCreationListener))]
    [ContentType(Constants.LanguageName)]
    [Name(Constants.LanguageName + "LineSeparator")]
    [TextViewRole(PredefinedTextViewRoles.Document)]
    internal class LineSeparatorAdornmentManagerProvider : IWpfTextViewCreationListener
    {
        [Import]
        private IViewTagAggregatorFactoryService tagAggregatorFactoryService = null;

        [Import]
        private IEditorFormatMapService editorFormatMapService = null;

        [Export(typeof(AdornmentLayerDefinition))]
        [Name(Constants.LanguageName + "LineSeparator")]
        [TextViewRole(PredefinedTextViewRoles.Document)]
        [ContentType(Constants.LanguageName)]
        [Order(After = PredefinedAdornmentLayers.Selection, Before = PredefinedAdornmentLayers.Squiggle)]

        public AdornmentLayerDefinition editorAdornmentLayer = null;

        public void TextViewCreated(IWpfTextView textView)
        {
            // always setup the manager. Inside the manager we check to see if the dividers are enabled/disabled
            var buffer = textView.TextBuffer;
            if (buffer.Properties.TryGetProperty<LineSeparatorManager>(typeof(LineSeparatorManager), out var lineSeparatorManager))
                return;
            lineSeparatorManager = new LineSeparatorManager(textView, tagAggregatorFactoryService, editorFormatMapService);
            buffer.Properties.AddProperty(typeof(LineSeparatorManager), lineSeparatorManager);
            return;
        }
    }
}
