//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using LanguageService.CodeAnalysis.Text;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using LanguageService.SyntaxTree;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Text.Editor;
using System;
using System.Collections.Generic;
using XSharpModel;
using XSharp.Settings;

namespace XSharp.LanguageService
{
    public static class Extensions
    {

        public static bool IsClassificationCommentOrString(this string classification)
        {
            if (string.IsNullOrEmpty(classification))
                return false;
            switch (classification.ToLower())
            {
                case "comment":
                case "string":
                case "xsharp.text":
                    return true;
            }
            return false;
        }
        public static string CleanText(this IToken token)
        {
            string result = token.Text;
            if (result.StartsWith("@@"))
                result = result.Substring(2);
            return result;
        }
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


        public static XFile GetFile(this ITextBuffer buffer)
        {
            if (buffer == null)
                return null;
            if (buffer.Properties.TryGetProperty<XFile>(typeof(XFile), out var file))
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

        internal static XDocument GetDocument(this ITextBuffer buffer)
        {
            if (buffer == null)
                return null;
            if (buffer.Properties.TryGetProperty<XDocument>(typeof(XDocument), out var document))
            {
                return document;
            }
            return null;
        }

        public static IList<IToken> GetTokensUnderCursor(this ITextView TextView, out CompletionState state)
        {
            var result = new List<IToken>();
            state = CompletionState.None;
            ModelWalker.Suspend();
            var buffer = TextView.TextBuffer;
            // LookUp for the BaseType, reading the TokenList (From left to right)
            var file = buffer.GetFile();
            if (file == null || file.XFileType != XFileType.SourceCode)
                return result;

            var snapshot = buffer.CurrentSnapshot;
            // We don't want to lex the buffer. So get the tokens from the last lex run
            // and when these are too old, then simply bail out
            var tokens = buffer.GetDocument();
            if (tokens != null)
            {
                if (tokens.SnapShot.Version != snapshot.Version)
                    return result;
            }
            string currentNS = TextView.FindNamespace();
            var location = TextView.FindLocation();
            var tokenList = XSharpTokenTools.GetTokensUnderCursor(location, out state);
            result.AddRange(tokenList);
            return result;
        }

        internal static IList<IXSymbol> GetSymbolUnderCursor(this ITextView TextView, out CompletionState state)
        {
            return TextView.GetSymbolUnderCursor(out state, out _, out _);
        }

        internal static IList<IXSymbol> GetSymbolUnderCursor(this ITextView TextView, out CompletionState state, out XSharpSearchLocation location)
        {
            return TextView.GetSymbolUnderCursor(out state, out location, out _);
        }
        internal static IList<IXSymbol> GetSymbolUnderCursor(this ITextView TextView, out CompletionState state, out XSharpSearchLocation location, out IList<IToken> tokenList)
        {
            var result = new List<IXSymbol>();
            state = CompletionState.None;
            location = null;
            tokenList = null;

            try
            {
                ModelWalker.Suspend();
                location = TextView.FindLocation();
                tokenList = XSharpTokenTools.GetTokensUnderCursor(location, out state);
                string currentNS = TextView.FindNamespace();
                result.AddRange(XSharpLookup.RetrieveElement(location, tokenList, state));
                if (result.Count == 0 && tokenList.Count > 1)
                {
                    // try again with just the last element in the list
                    var token = tokenList[tokenList.Count - 1];
                    if (token.Type == XSharpLexer.ID)
                    {
                        tokenList.Clear();
                        tokenList.Add(token);
                        location = location.With(currentNS);
                        result.AddRange(XSharpLookup.RetrieveElement(location, tokenList, state));
                    }
                }
                return result;
            }
            catch (Exception ex)
            {
                Logger.Exception(ex, "Goto failed");
            }
            finally
            {
                ModelWalker.Resume();
            }
            return result;
        }

        public static XSharpClassifier GetClassifier(this ITextBuffer buffer)
        {
            if (buffer == null)
                return null;
            if (buffer.Properties.TryGetProperty<XSharpClassifier>(typeof(XSharpClassifier), out var classifier))
            {
                return classifier;
            }
            return null;
        }
        public static SourceCodeEditorSettings GetSettings(this ITextBuffer buffer)
        {
            if (buffer == null)
                return null;
            if (buffer.Properties.TryGetProperty<SourceCodeEditorSettings>(typeof(SourceCodeEditorSettings), out var settings))
            {
                return settings;
            }
            return null;
        }

        public static String GetXAMLFile(this ITextBuffer buffer)
        {
            if (buffer == null)
                return null;
            if (buffer.Properties.TryGetProperty<ITextDocument>(typeof(ITextDocument), out var textDoc))
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
                    case XFileType.PreprocessorOutput:
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
        internal static XSourceEntity FindEntity(this ITextBuffer buffer, SnapshotPoint point)
        {
            if (buffer == null)
                return null;
            var file = buffer.GetFile();
            if (file == null)
                return null;
            return XSharpLookup.FindEntity(point.GetContainingLine().LineNumber, file);
        }

        internal static XSourceMemberSymbol FindMember(this ITextBuffer buffer, SnapshotPoint point)
        {
            if (buffer == null)
                return null;
            var file = buffer.GetFile();
            if (file == null)
                return null;

            return XSharpLookup.FindMember(point.GetContainingLine().LineNumber, file);
        }
        internal static XSourceMemberSymbol FindMemberAtPosition(this ITextBuffer buffer, SnapshotPoint point)
        {
            if (buffer == null)
                return null;
            var file = buffer.GetFile();
            if (file == null)
                return null;
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
            var location = new XSharpSearchLocation(buffer.GetDocument(), file, member, snapshot, line, point, currentNS);
            return location;
        }

        internal static XSourceEntity FindEntity(this ITextView textview)
        {
            if (textview == null)
                return null;
            return FindEntity(textview, textview.Caret.Position.BufferPosition);
        }
        internal static XSourceEntity FindEntity(this ITextView textview, SnapshotPoint ssp)
        {
            if (textview == null)
                return null;
            if (ssp == null)
            {
                ssp = textview.Caret.Position.BufferPosition;
            }
            return textview.TextBuffer.FindEntity(ssp);
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
    }
}
