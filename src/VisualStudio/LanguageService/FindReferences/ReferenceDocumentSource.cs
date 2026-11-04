//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.IO;
using LanguageService.CodeAnalysis.Text;
using LanguageService.CodeAnalysis.XSharp;
using LanguageService.SyntaxTree;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Utilities;
using XSharpModel;

namespace XSharp.LanguageService.FindReferences
{
    /// <summary>
    /// Provides an XDocument for every file of the search, on a scratch buffer with one snapshot of the editor text
    /// (open file) or the disk text (closed file).
    /// </summary>
    internal sealed class ReferenceDocumentSource
    {
        private readonly ITextBufferFactoryService _bufferFactory;
        private readonly IContentType _contentType;
        private readonly IReadOnlyDictionary<string, ITextBuffer> _openBuffers;

        internal ReferenceDocumentSource(ITextBufferFactoryService bufferFactory, IContentTypeRegistryService contentTypes,
            IReadOnlyDictionary<string, ITextBuffer> openBuffers)
        {
            _bufferFactory = bufferFactory;
            _contentType = contentTypes.GetContentType("text");
            _openBuffers = openBuffers;
        }

        /// <summary>The text of the file, read once: the editor text when the file is open, else the disk text.</summary>
        internal string ReadText(XFile file)
        {
            if (_openBuffers.TryGetValue(file.FullPath, out var buffer))
                return buffer.CurrentSnapshot.GetText();
            return File.ReadAllText(file.FullPath);
        }

        /// <summary>
        /// A private document for <paramref name="text"/>. Never the classifier's document of an open buffer:
        /// the classifier rebuilds that one in place while the user types.
        /// </summary>
        internal XDocument CreateDocument(XFile file, string text)
        {
            return CreateDocument(file, text, Lex(file, text));
        }

        /// <summary>
        /// A private document for <paramref name="text"/> with the given tokens, e.g. with an interpolated string
        /// replaced by the tokens of one of its expressions. The lookup finds a document through its buffer, so
        /// every document gets a buffer of its own.
        /// </summary>
        internal XDocument CreateDocument(XFile file, string text, IList<IToken> tokens)
        {
            var buffer = _bufferFactory.CreateTextBuffer(text, _contentType);
            buffer.Properties.AddProperty(typeof(XFile), file);
            var snapshot = buffer.CurrentSnapshot;
            var document = new XDocument(buffer, tokens, snapshot);
            XDocumentIndexer.BuildIndexes(tokens, out var lineTokens, out var identifiers, out var lineState);
            document.SetTokens(lineTokens);
            document.SetIdentifiers(identifiers);
            document.SetState(lineState, snapshot);
            buffer.Properties.AddProperty(typeof(XDocument), document);
            return document;
        }

        internal static IList<IToken> Lex(XFile file, string text)
        {
            var options = file.Project?.ParseOptions ?? XParseOptions.Default;
            XSharp.Parser.VsParser.Lex(text, file.FullPath, (XSharpParseOptions)options, new NullErrorListener(), out ITokenStream stream, out _);
            var tokens = new List<IToken>();
            if (stream is BufferedTokenStream buffered)
                tokens.AddRange(buffered.GetTokens());
            return tokens;
        }

        private sealed class NullErrorListener : XSharp.Parser.VsParser.IErrorListener
        {
            public void ReportError(string fileName, LinePositionSpan span, string errorCode, string message, object[] args) { }
            public void ReportWarning(string fileName, LinePositionSpan span, string errorCode, string message, object[] args) { }
        }
    }
}
