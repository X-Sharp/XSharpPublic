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
using XSharpModel;

namespace XSharpColorizer
{
    public static class Extensions
    {
        //public static ITagSpan<IClassificationTag> ToTagSpan(this TextSpan span, ITextSnapshot snapshot, IClassificationType classificationType)
        //{
        //    int start = span.Start;
        //    int length = span.Length;
        //    // validate parameters
        //    start = start < 0 ? 0 : start;
        //    start = start > snapshot.Length ? snapshot.Length : start;
        //    if (start + length > snapshot.Length)
        //    {
        //        length = snapshot.Length - start;
        //    }
        //    length = length < 0 ? 0 : length;
        //    var sspan = new SnapshotSpan(snapshot, start, length);
        //    var tag = new ClassificationTag(classificationType);
        //    return new TagSpan<IClassificationTag>(sspan, tag);
        //}

        public static ClassificationSpan ToClassificationSpan(this TextSpan span, ITextSnapshot snapshot, IClassificationType classificationType)
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
            SnapshotSpan sspan = new SnapshotSpan(snapshot, start, length);
            return new ClassificationSpan(sspan, classificationType);
        }

        //public static String GetText(this ITextSnapshot snapshot, TextSpan span)
        //{
        //    int start = span.Start;
        //    int length = span.Length;
        //    // validate
        //    if (span.End > snapshot.Length)
        //    {
        //        length = snapshot.Length - start;
        //    }
        //    return snapshot.GetText(new Span(start, length));
        //}

        //public static string GetFileName(this ITextBuffer buffer)
        //{
        //    XFile file = buffer.GetFile();
        //    if (file != null)
        //    {
        //        return file.FullPath;
        //    }
        //    ITextDocument textDoc;
        //    if (buffer.Properties.TryGetProperty(typeof(ITextDocument), out textDoc))
        //    {
        //        return textDoc.FilePath;
        //    }
        //    return string.Empty;
        //}
        public static XFile GetFile(this ITextBuffer buffer)
        {
            XFile file;
            if (buffer.Properties.TryGetProperty(typeof(XFile), out file))
            {
                return file;
            }
            return null;
        }

        public static String GetXAMLFile(this ITextBuffer buffer)
        {
            ITextDocument textDoc;
            if (buffer.Properties.TryGetProperty(typeof(ITextDocument), out textDoc))
            {
                return textDoc.FilePath;
            }
            return null;
        }



        internal static bool IsXSharpDocument(this ITextDocumentFactoryService factory, ITextBuffer buffer)
        {
            string path = "";
            if (buffer.Properties.ContainsProperty(typeof(XFile)))
            {
                return buffer.GetFile() != null;
            }
            // When not found then locate the file in the XSolution by its name
            if (factory != null)
            {
                ITextDocument doc = null;
                if (factory.TryGetTextDocument(buffer, out doc))
                {
                    path = doc.FilePath;
                }
            }
            // Find and attach the X# document when we have it, or a null to indicate that we have searched
            // and not found it
            var file = XSolution.FindFile(path);
            if (file != null)
            {
                buffer.Properties.AddProperty(typeof(XFile), file);
            }
            return file != null;
        }
    }
}
