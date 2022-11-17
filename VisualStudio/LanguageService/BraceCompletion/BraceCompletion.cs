//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.BraceCompletion;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;
using System.ComponentModel.Composition;
using static XSharp.XSharpConstants;
using XSharpModel;
using System.Collections.Generic;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Language.StandardClassification;
using XSharp.LanguageService.Editors.HighlightWord;

namespace XSharp.LanguageService
{
    [Export(typeof(IBraceCompletionContextProvider))]
    [BracePair('(', ')')]
    [BracePair('[', ']')]
    [BracePair('{', '}')]
    [BracePair('"', '"')]
    [BracePair('\'', '\'')]
    [ContentType(LanguageName)]
    [ProvideBraceCompletion(Constants.LanguageName)]

    internal sealed class BraceCompletionProvider : BraceCompletionBase
    {
        protected override bool IsValidBraceCompletionContext(SnapshotPoint openingPoint)
        {
            if (!XEditorSettings.CompletionAutoPairs)
                return false;

            IList<ClassificationSpan> spans = this.GetSpans(openingPoint);
            if (spans != null)
            {
                foreach (ClassificationSpan item in spans)
                {
                    if (HighlightWordTaggerProvider.IsInActiveSpan(item))
                        return false;
                }
            }
            return true;
        }
    }
}
