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
using System.Globalization;

// This now uses the HightLightWord code in the Community Toolkit



namespace XSharp.LanguageService.Editors.HighlightWord
{

    [Export(typeof(IViewTaggerProvider))]
    [ContentType(LanguageName)]
    [TagType(typeof(TextMarkerTag))]
    internal class HighlightWordTaggerProvider : SameWordHighlighterBase
    {

        [Import] internal IClassifierAggregatorService _classifierService = null;

        // following 2 methods copied from Roslyn

        public static bool IsIdentifierStartCharacter(char ch)
        {
            // identifier-start-character:
            //   letter-character
            //   _ (the underscore character U+005F)

            if (ch < 'a') // '\u0061'
            {
                if (ch < 'A') // '\u0041'
                {
                    return false;
                }

                return ch <= 'Z'  // '\u005A'
                    || ch == '_'; // '\u005F'
            }

            if (ch <= 'z') // '\u007A'
            {
                return true;
            }

            if (ch <= '\u007F') // max ASCII
            {
                return false;
            }

            return IsLetterChar(CharUnicodeInfo.GetUnicodeCategory(ch));
        }
        private static bool IsLetterChar(UnicodeCategory cat)
        {
            // letter-character:
            //   A Unicode character of classes Lu, Ll, Lt, Lm, Lo, or Nl
            //   A Unicode-escape-sequence representing a character of classes Lu, Ll, Lt, Lm, Lo, or Nl

            switch (cat)
            {
                case UnicodeCategory.UppercaseLetter:
                case UnicodeCategory.LowercaseLetter:
                case UnicodeCategory.TitlecaseLetter:
                case UnicodeCategory.ModifierLetter:
                case UnicodeCategory.OtherLetter:
                case UnicodeCategory.LetterNumber:
                    return true;
            }

            return false;
        }
        public override string TextMarkerTagType => ColorizerConstants.HighLightIdentifierFormatDefinition;

        public override FindOptions FindOptions => FindOptions.WholeWord;
        public override bool ShouldHighlight(string text)
        {
            if (XSharpSyntax.KeywordNames.ContainsKey(text))
                return false;

            if (!IsIdentifierStartCharacter(text[0]))
                return false;

            if (_classifierService == null)
                return true;
            return ThreadHelper.JoinableTaskFactory.Run(async delegate
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                var view = await VS.Documents.GetActiveDocumentViewAsync();
                var caret = view.TextView.Caret;
                var pos = caret.Position.BufferPosition;
                if (pos >= caret.Position.BufferPosition.Snapshot.Length)
                    return false;
                var span = new SnapshotSpan(pos,1);
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
