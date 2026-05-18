// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#if ASYNCCOMPLETION
using System;
using System.Collections.Generic;
using System.Threading;

using Community.VisualStudio.Toolkit;

using Microsoft.VisualStudio.Language.Intellisense.AsyncCompletion;
using Microsoft.VisualStudio.Language.Intellisense.AsyncCompletion.Data;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.TextManager.Interop;

using XSharp.Settings;
using XSharp.Support;

using XSharpModel;

namespace XSharp.LanguageService
{
    /// <summary>
    /// Implements <see cref="IAsyncCompletionCommitManager"/> for XSharp.
    /// Handles keyword casing, snippet expansion, auto-pairs, and signature-help triggering.
    /// </summary>
    internal class XSharpCompletionCommitManager : IAsyncCompletionCommitManager
    {
        public IEnumerable<char> PotentialCommitCharacters => XEditorSettings.CommitChars;

        public bool ShouldCommitCompletion(IAsyncCompletionSession session,
            SnapshotPoint location, char typedChar, CancellationToken token)
        {
            // Block commit on := (assignment operator that starts with ':')
            if (typedChar == ':')
            {
                var snapshot = location.Snapshot;
                if (location.Position < snapshot.Length && snapshot[location.Position] == '=')
                    return false;
            }
            return true;
        }

        public CommitResult TryCommit(IAsyncCompletionSession session,
            ITextBuffer buffer, CompletionItem item, char typedChar, CancellationToken token)
        {
            ThreadHelper.ThrowIfNotOnUIThread();

            item.Properties.TryGetProperty(typeof(Kind), out Kind kind);

            // ----------------------------------------------------------------
            // 1. Snippets – let the expansion client handle insertion entirely
            // ----------------------------------------------------------------
            if (kind == Kind.Snippet)
            {
                if (item.Properties.TryGetProperty<VsExpansion>(typeof(VsExpansion), out var expansion))
                {
                    try
                    {
                        var doc = buffer.GetDocument();
                        var textViewAdapter = session.TextView.ToIVsTextView();
                        if (doc != null && textViewAdapter != null)
                        {
                            var client = new ExpansionClient(doc, textViewAdapter);
                            client.InsertAnyExpansion(null, expansion.title, expansion.path);
                            return CommitResult.Handled;
                        }
                    }
                    catch (Exception ex)
                    {
                        Logger.Exception(ex, "TryCommit: snippet insertion failed");
                    }
                }
                return CommitResult.Unhandled;
            }

            // ----------------------------------------------------------------
            // 2. Build the final insertion text
            // ----------------------------------------------------------------
            string insertionText = item.InsertionText;

            // Keyword casing
            if (kind == Kind.Keyword)
                insertionText = XLiterals.FormatKeyword(insertionText);

            // When committing via '.' or ':' append the trigger char (if the
            // text has no opening paren/brace that supersedes it)
            if ((typedChar == '.' || typedChar == ':') &&
                insertionText.IndexOfAny(new[] { '(', '{' }) < 0)
            {
                insertionText += typedChar;
            }

            // Constructor/class: '{' commit means open an object-initialiser
            if ((kind == Kind.Class || kind == Kind.Structure || kind == Kind.Constructor) &&
                (typedChar == '{' || typedChar == '}') &&
                !insertionText.EndsWith("{"))
            {
                insertionText += "{";
            }

            // Auto-pairs: add the closing delimiter when the setting is enabled
            bool triggerSignature = false;
            if (insertionText.Length > 0)
            {
                char last = insertionText[insertionText.Length - 1];
                if (last == '(' || last == '{' || last == '[')
                {
                    triggerSignature = true;
                    if (XEditorSettings.CompletionAutoPairs)
                        insertionText += MatchingClose(last);
                }
            }

            // ----------------------------------------------------------------
            // 3. Apply the text to the buffer
            // ----------------------------------------------------------------
            bool needsManualCommit = !string.Equals(insertionText, item.InsertionText,
                StringComparison.Ordinal);

            if (needsManualCommit)
            {
                var span = session.ApplicableToSpan.GetSpan(buffer.CurrentSnapshot);
                using (var edit = buffer.CreateEdit())
                {
                    edit.Replace(span, insertionText);
                    edit.Apply();
                }

                // When auto-pairs were added, move the caret back one position
                // so it sits between the opening and closing delimiter.
                if (triggerSignature && XEditorSettings.CompletionAutoPairs)
                    session.TextView.Caret.MoveToPreviousCaretPosition();
            }

            // ----------------------------------------------------------------
            // 4. Trigger signature help for methods/constructors
            // ----------------------------------------------------------------
            if (triggerSignature)
            {
                var doc = buffer.GetDocument();
                ThreadHelper.JoinableTaskFactory.Run(async delegate
                {
                    if (doc != null)
                        doc.SignatureStarting = true;
                    _ = await VS.Commands.ExecuteAsync(KnownCommands.Edit_ParameterInfo);
                });
            }

            return needsManualCommit ? CommitResult.Handled : CommitResult.Unhandled;
        }

        private static char MatchingClose(char open)
        {
            switch (open)
            {
                case '(':  return ')';
                case '{':  return '}';
                case '[':  return ']';
                default:   return '\0';
            }
        }
    }
}
#endif
