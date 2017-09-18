//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using LanguageService.CodeAnalysis.Text;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Text.Tagging;
using System;

namespace XSharpModel
{
    public static class TextSpanExtensions
    {
        public static ITagSpan<IClassificationTag> ToTagSpan(this TextSpan span, ITextSnapshot snapshot, IClassificationType classificationType)
        {
            return new TagSpan<IClassificationTag>(
              new SnapshotSpan(snapshot, span.Start, span.Length),
              new ClassificationTag(classificationType)
              );
        }

        public static ClassificationSpan ToClassificationSpan(this TextSpan span, ITextSnapshot snapshot, IClassificationType classificationType)
        {
            return new ClassificationSpan(
              new SnapshotSpan(snapshot, span.Start, span.Length),
              classificationType
              );
        }

        public static String GetText(this ITextSnapshot snapshot, TextSpan span)
        {
            return snapshot.GetText(new Span(span.Start, span.Length));
        }
    }
}
