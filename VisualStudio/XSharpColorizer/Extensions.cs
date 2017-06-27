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

namespace XSharpColorizer
{
    public static class Extensions
    {
        public static ITagSpan<IClassificationTag> ToTagSpan(this TextSpan span, ITextSnapshot snapshot, IClassificationType classificationType)
        {
            int start = span.Start;
            int length = span.Length;
            // validate parameters
            start = start < 0 ? 0 : start;
            start = start > snapshot.Length ? snapshot.Length : start;
            if (start + length > snapshot.Length)
            {
                length = snapshot.Length - start;
            }
            length = length < 0 ? 0 : length;
            var sspan = new SnapshotSpan(snapshot, start, length);
            var tag = new ClassificationTag(classificationType);
            return new TagSpan<IClassificationTag>(sspan, tag);
        }

        public static ClassificationSpan ToClassificationSpan(this TextSpan span, ITextSnapshot snapshot, IClassificationType classificationType)
        {
            int start = span.Start;
            int length = span.Length;
            // validate parameters
            // validate parameters
            start = start < 0 ? 0 : start;
            start = start > snapshot.Length ? snapshot.Length : start;
            if (start + length > snapshot.Length)
            {
                length = snapshot.Length - start;
            }
            length = length < 0 ? 0 : length;
            SnapshotSpan sspan = new SnapshotSpan(snapshot, start, length);
            return new ClassificationSpan(sspan, classificationType);
        }

        public static String GetText(this ITextSnapshot snapshot, TextSpan span)
        {
            int start = span.Start;
            int length = span.Length;
            // validate
            if (span.End > snapshot.Length)
            {
                length = snapshot.Length - start;
            }
            return snapshot.GetText(new Span(start, length));
        }
    }
}
