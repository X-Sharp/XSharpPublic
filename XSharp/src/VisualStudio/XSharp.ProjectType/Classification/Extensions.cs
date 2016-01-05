using Microsoft.CodeAnalysis.Text;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Text.Tagging;
using System;

namespace XSharpColorizer
{
    public static class Extensions
    {
        public static ITagSpan<IClassificationTag> ToTagSpan(this TextSpan span, ITextSnapshot snapshot, IClassificationType classificationType)
        {
            return new TagSpan<IClassificationTag>(
              new SnapshotSpan(snapshot, span.Start, span.Length),
              new ClassificationTag(classificationType)
              );
        }
        public static String GetText(this ITextSnapshot snapshot, TextSpan span)
        {
            return snapshot.GetText(new Span(span.Start, span.Length));
        }
    }
}
