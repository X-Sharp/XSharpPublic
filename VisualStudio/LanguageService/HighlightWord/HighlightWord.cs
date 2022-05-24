using Community.VisualStudio.Toolkit;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Language.StandardClassification;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Text.Operations;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;
using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using static XSharp.XSharpConstants;
using Microsoft.VisualStudio.Shell;

// This now uses the HightLightWord code in the Community Toolkit



namespace XSharp.LanguageService.Editors.HighlightWord
{

    [Export(typeof(IViewTaggerProvider))]
    [ContentType(LanguageName)]
    [TagType(typeof(TextMarkerTag))]
    internal class HighlightWordTaggerProvider : SameWordHighlighterBase
    {

        [Import] internal IClassifierAggregatorService _classifierService = null;


        public override FindOptions FindOptions => FindOptions.WholeWord;
        public override bool ShouldHighlight(string text)
        {
            if (XSharpSyntax.KeywordNames.ContainsKey(text))
                return false;
            if (_classifierService == null)
                return true;
            return ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                var view = await VS.Documents.GetActiveDocumentViewAsync();
                var caret = view.TextView.Caret;
                var span = new SnapshotSpan(caret.Position.BufferPosition,1);
                if (isSpanInInactiveRegion(span))
                    return false;
                return true;
            });

            
        }
        public override IEnumerable<SnapshotSpan> FilterResults(IEnumerable<SnapshotSpan> results)
        {
            if (_classifierService == null)
                return results;

            var filtered = new List<SnapshotSpan>();
            foreach (var sspan in results)
            {
                if (!isSpanInInactiveRegion(sspan))
                {
                    filtered.Add(sspan);
                }
            }
            return filtered;
        }
        private bool isSpanInInactiveRegion(SnapshotSpan sspan)
        {
            IList<ClassificationSpan> classificationSpans = _classifierService.GetClassifier(sspan.Snapshot.TextBuffer)
                                                                       .GetClassificationSpans(sspan);
            foreach (ClassificationSpan span in classificationSpans)
            {
                return IsInActiveSpan(span);
            }
            return false;
        }
        internal static bool IsInActiveSpan(ClassificationSpan span)
        {
            if (span.ClassificationType.IsOfType(PredefinedClassificationTypeNames.Comment))
            {
                return true;
            }
            else if (span.ClassificationType.IsOfType(PredefinedClassificationTypeNames.String))
            {
                return true;
            }
            else if (span.ClassificationType.IsOfType(PredefinedClassificationTypeNames.ExcludedCode))
            {
                return true;
            }
            return false;
        }

    }
}
