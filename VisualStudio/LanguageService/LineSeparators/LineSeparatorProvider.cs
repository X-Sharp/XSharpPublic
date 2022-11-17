//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System.ComponentModel.Composition;
using System.Threading.Tasks;
using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;
using XSharpModel;

namespace XSharp.LanguageService
{
    [Export(typeof(IWpfTextViewCreationListener))]
    [ContentType(Constants.LanguageName)]
    [Name(Constants.LanguageName + "LineSeparator")]
    [TextViewRole(PredefinedTextViewRoles.Document)]
    internal class LineSeparatorAdornmentManagerProvider : WpfTextViewCreationListener
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

 
        protected override Task CreatedAsync(DocumentView docView)
        {
            if ( XEditorSettings.ShowDividers)
            {
                var buffer = Document.TextBuffer;
                if (!buffer.Properties.TryGetProperty<LineSeparatorManager>(typeof(LineSeparatorManager),
                    out _))
                {
                    var lineSeparatorManager = new LineSeparatorManager(docView, this, tagAggregatorFactoryService, editorFormatMapService);
                    buffer.Properties.AddProperty(typeof(LineSeparatorManager), lineSeparatorManager);
                }
            }
            return Task.CompletedTask;
        }
    }
}
