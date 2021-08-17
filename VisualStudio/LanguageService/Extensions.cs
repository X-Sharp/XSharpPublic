//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using LanguageService.CodeAnalysis.Text;
using LanguageService.SyntaxTree;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Tagging;
using System;
using XSharpModel;

namespace XSharp.LanguageService
{
    public static class Extensions
    {
        public static string CleanText(this IToken token)
        {
            string result = token.Text;
            if (result.StartsWith("@@"))
                result = result.Substring(2);
            return result;
        }
        public static XsClassificationSpan ToClassificationSpan(this TextSpan span, ITextSnapshot snapshot, IClassificationType classificationType)
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
            return new XsClassificationSpan(sspan, classificationType);
        }

    
        public static XFile GetFile(this ITextBuffer buffer)
        {
            XFile file;
            if (buffer.Properties.TryGetProperty(typeof(XFile), out file))
            {
                return file;
            }
            var fileName = buffer.GetFileName();
            file = XSolution.FindFile(fileName);
            if (file != null)
            {
                buffer.Properties.AddProperty(typeof(XFile), file);
            }
            return file;
        }

        public static XSharpTokens GetTokens(this ITextBuffer buffer)
        {
            XSharpTokens tokens;
            if (buffer.Properties.TryGetProperty(typeof(XSharpTokens), out tokens))
            {
                return tokens;
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
            if (file == null)
            {
                var type = XFileTypeHelpers.GetFileType(path);
                switch (type)
                {
                    case XFileType.SourceCode:
                    case XFileType.Header:
                        file = XSolution.AddOrphan(path);
                        break;
                    default:
                        if (type.IsVOBinary())
                            file = XSolution.AddOrphan(path);
                        break;
                }
            }
            if (file != null)
            {
                file.Interactive = true;
                buffer.Properties.AddProperty(typeof(XFile), file);
            }
            return file != null;
        }
        internal static XSourceMemberSymbol FindMember(this ITextBuffer buffer, SnapshotPoint point)
        {
            var file = buffer.GetFile();
            return XSharpLookup.FindMember(point.GetContainingLine().LineNumber, file);
        }
        internal static XSourceMemberSymbol FindMemberAtPosition(this ITextBuffer buffer, SnapshotPoint point)
        {
            var file = buffer.GetFile();
            return XSharpLookup.FindMemberAtPosition(point.Position, file);
        }
        internal static XSharpSearchLocation FindLocation(this ITextBuffer buffer, SnapshotPoint point)
        {
            if (buffer == null)
                return null;
            int line = point.GetContainingLine().LineNumber;
            var file = buffer.GetFile();
            var snapshot = buffer.CurrentSnapshot;
            var member = XSharpLookup.FindMember(line, file);
            var ns = XSharpTokenTools.FindNamespace(point, file);
            string currentNS = "";
            if (ns != null)
                currentNS = ns.FullName;
            var location = new XSharpSearchLocation(member, snapshot, line, point, currentNS);
            return location;
        }

        internal static XSourceMemberSymbol FindMember(this ITextView textview)
        {
            if (textview == null)
                return null;
            return FindMember(textview, textview.Caret.Position.BufferPosition);
        }
        internal static XSourceMemberSymbol FindMember(this ITextView textview, SnapshotPoint ssp )
        {
            if (textview == null)
                return null;
            if (ssp == null)
            {
                ssp = textview.Caret.Position.BufferPosition;
            }
            return textview.TextBuffer.FindMember(ssp);
        }
        internal static string FindNamespace(this ITextView textview)
        {
            if (textview == null)
                return "";
            var caretpos = textview.Caret.Position.BufferPosition.Position;
            var file = textview.TextBuffer.GetFile();
            var ns = XSharpTokenTools.FindNamespace(caretpos, file);
            if (ns != null)
                return ns.FullName;
            return "";
        }
        internal static XSharpSearchLocation FindLocation(this ITextView textview)
        {
            if (textview == null)
                return null;
            return FindLocation(textview, textview.Caret.Position.BufferPosition);
        }
        internal static XSharpSearchLocation FindLocation(this ITextView textview, SnapshotPoint ssp)
        {
            return textview.TextBuffer.FindLocation(ssp);
        }
        internal static XFile GetFile(this ITextView textview)
        {
            if (textview != null && textview.TextBuffer != null)
            {
                return textview.TextBuffer.GetFile();
            }
            return null;
        }
        internal static XSharpLineState GetLineState(this ITextBuffer buffer)
        {
            buffer.Properties.TryGetProperty<XSharpLineState>(typeof(XSharpLineState), out var lineState);
            return lineState;
        }

    }
}
