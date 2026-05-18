//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#if ASYNCCOMPLETION
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;

using LanguageService.CodeAnalysis.XSharp.SyntaxParser;

using Microsoft.VisualStudio.Core.Imaging;
using Microsoft.VisualStudio.Imaging.Interop;
using Microsoft.VisualStudio.Language.Intellisense.AsyncCompletion;
using Microsoft.VisualStudio.Language.Intellisense.AsyncCompletion.Data;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.TextManager.Interop;

using XSharp.Settings;
using XSharp.Support;

using XSharpModel;

namespace XSharp.LanguageService
{
    partial class XSharpAsyncCompletionSource : IAsyncCompletionSource
    {
        private readonly ITextView _textView;
        private readonly ITextBuffer _buffer;
        private readonly XSharpAsyncCompletionSourceProvider _provider;
        private readonly XFile _file;
        private readonly IBufferTagAggregatorFactoryService _aggregator;
        private readonly XDialect _dialect;
        private readonly CompletionHelpers _helpers;

        // Empty context sentinel – avoids depending on CompletionContext.Empty which may not exist in all SDK versions
        private static readonly CompletionContext s_emptyContext =
            new CompletionContext(ImmutableArray<CompletionItem>.Empty);

        // "Does not participate" sentinel.
        // VS 17 (DEV17) exposes CompletionStartData.DoesNotParticipateInCompletion.
        // VS 16 does not – default(CompletionStartData) is equivalent there.
#if DEV17
        private static readonly CompletionStartData s_doNotParticipate =
            CompletionStartData.DoesNotParticipateInCompletion;
#else
        private static readonly CompletionStartData s_doNotParticipate =
            default(CompletionStartData);
#endif

        public XSharpAsyncCompletionSource(XSharpAsyncCompletionSourceProvider provider,
            ITextView textView,
            IBufferTagAggregatorFactoryService aggregator,
            XFile file)
        {
            _provider = provider;
            _textView = textView;
            _buffer = textView.TextBuffer;
            _file = file;
            _aggregator = aggregator;
            _dialect = _file.Project.Dialect;
            var prj = _file.Project.ProjectNode;
            _helpers = new CompletionHelpers(_dialect, file, !prj.ParseOptions.CaseSensitive);
        }

        internal static void WriteOutputMessage(string strMessage)
        {
            if (XSettings.EnableCodeCompletionLog && XSettings.EnableLogging)
            {
                Logger.Information(strMessage);
            }
        }

        // -----------------------------------------------------------------------
        // IAsyncCompletionSource – InitializeCompletion
        // Called synchronously on the UI thread to decide whether to participate
        // and to supply the applicable-to span (the token that will be replaced).
        // -----------------------------------------------------------------------
        public CompletionStartData InitializeCompletion(CompletionTrigger trigger,
            SnapshotPoint triggerLocation,
            CancellationToken token)
        {
            if (XEditorSettings.DisableCodeCompletion)
                return s_doNotParticipate;

            if (_file == null)
                return s_doNotParticipate;

            char typedChar = trigger.Character;

            switch (trigger.Reason)
            {
                case CompletionTriggerReason.Invoke:
                case CompletionTriggerReason.InvokeAndCommitIfUnique:
                    // Explicit invocation (Ctrl+Space) – always participate
                    break;

                case CompletionTriggerReason.Insertion:
                    if (typedChar == ':' || typedChar == '.')
                        break;  // member-access triggers
                    if (!char.IsLetterOrDigit(typedChar) && typedChar != '_')
                        return s_doNotParticipate;
                    break;

                case CompletionTriggerReason.Deletion:
                    // Backspace / delete while a session is already showing
                    break;

                default:
                    return s_doNotParticipate;
            }

            // Do not offer completions inside comments or string literals
            if (IsInsideCommentOrString(triggerLocation))
                return s_doNotParticipate;

            // Do not trigger on `:=` (assignment operator)
            if (typedChar == ':' && IsNextChar(triggerLocation, '='))
                return s_doNotParticipate;

            // Calculate the applicable span (the partial word being typed)
            var applicableSpan = GetApplicableSpan(triggerLocation, typedChar);
            return new CompletionStartData(CompletionParticipation.ProvidesItems, applicableSpan);
        }

        // -----------------------------------------------------------------------
        // IAsyncCompletionSource – GetCompletionContextAsync
        // Builds the list of completion items.  Must run on the UI thread because
        // some helpers (FillMembers, image-service calls) require it.
        // -----------------------------------------------------------------------
        public async Task<CompletionContext> GetCompletionContextAsync(
            IAsyncCompletionSession session,
            CompletionTrigger trigger,
            SnapshotPoint triggerLocation,
            SnapshotSpan applicableToSpan,
            CancellationToken token)
        {
            WriteOutputMessage("-->> GetCompletionContextAsync");
            try
            {
                if (XEditorSettings.DisableCodeCompletion)
                    return s_emptyContext;
                if (_file == null)
                    return s_emptyContext;

                // Ensure we are on the UI thread (helpers require it)
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync(token);

                XSharpModel.ModelWalker.Suspend();

                char typedChar = trigger.Character;
                bool showInstanceMembers = (typedChar == ':') ||
                    ((typedChar == '.') && _file.Project.ParseOptions.AllowDotForInstanceMembers);

                // Guard against comment/string context
                if (IsInsideCommentOrString(triggerLocation))
                    return s_emptyContext;

                // The filter text is whatever the user has already typed in the applicable span
                string filterText = applicableToSpan.GetText();

                bool includeKeywords =
                    trigger.Reason == CompletionTriggerReason.Invoke ||
                    trigger.Reason == CompletionTriggerReason.InvokeAndCommitIfUnique;

                var location = _buffer.FindLocation(triggerLocation);
                if (location == null)
                    return s_emptyContext;

                var member = _textView.FindMember(triggerLocation);

                // Build the token list and completion state
                CompletionState state;
                var tokenList = XSharpTokenTools.GetTokenList(location, out state, includeKeywords);

                // Identify (and strip) the last token when it is the partial word being typed
                var lastToken = tokenList.LastOrDefault(t => t.StopIndex < location.Position);
                if (lastToken != null)
                {
                    if (lastToken.Type != XSharpLexer.DOT && lastToken.Type != XSharpLexer.COLON)
                    {
                        if (lastToken.Type == XSharpLexer.ID || XSharpLexer.IsKeyword(lastToken.Type))
                        {
                            if (string.IsNullOrEmpty(filterText))
                                filterText = lastToken.Text;
                        }
                        tokenList.RemoveAt(tokenList.Count - 1);
                        lastToken = tokenList.LastOrDefault();
                    }
                }

                bool addKeywords = typedChar != '.' && typedChar != ':';
                bool dotSelector = (typedChar == '.');
                int tokenType = XSharpLexer.UNRECOGNIZED;

                var compList = new XCompletionList(_file);
                var kwdList = new XCompletionList(_file);
                IXTypeSymbol type = null;

                // Resolve the symbol before the caret
                var symbol = XSharpLookup.RetrieveElement(location, tokenList, CompletionState.General).FirstOrDefault();
                bool isInstance = true;
                string memberName = "";

                if (symbol is XSourceUndeclaredVariableSymbol)
                {
                    symbol = null;
                }
                else if (symbol != null)
                {
                    if (symbol is IXTypeSymbol xtype)
                    {
                        isInstance = false;
                        type = xtype;
                    }
                    else if (symbol.Kind == Kind.Namespace)
                    {
                        isInstance = false;
                        if (!state.HasFlag(CompletionState.Namespaces))
                            state = CompletionState.Namespaces | CompletionState.Types;
                    }
                    else
                    {
                        isInstance = true;
                        if (showInstanceMembers)
                        {
                            state |= CompletionState.InstanceMembers;
                            state |= CompletionState.StaticMembers;
                        }
                        else
                        {
                            state |= CompletionState.StaticMembers;
                        }
                    }

                    if (lastToken != null)
                    {
                        switch (lastToken.Type)
                        {
                            case XSharpLexer.DOT:
                                if (symbol.Kind == Kind.Namespace)
                                    filterText = symbol.FullName + "." + filterText;
                                else if (symbol is IXTypeSymbol)
                                    filterText = symbol.FullName + "." + filterText;
                                break;
                        }
                    }

                    if (symbol is IXMemberSymbol xmember)
                    {
                        if (xmember.Kind.HasParameters() &&
                            tokenList.Any(t => t.Type == XSharpLexer.LPAREN))
                        {
                            var typeName = xmember.TypeName;
                            if (xmember is XSourceMemberSymbol sourcemem)
                                type = sourcemem.File.FindType(typeName);
                            else
                                type = location.FindType(typeName);
                            memberName = xmember.Name;
                        }
                        else
                        {
                            type = xmember.ResolvedType;
                        }
                    }
                    else if (symbol is IXVariableSymbol xvar)
                    {
                        if (xvar is XSourceUndeclaredVariableSymbol)
                        {
                            type = null;
                            if (typedChar == ':' || typedChar == '.')
                            {
                                state = CompletionState.None;
                                filterText = "";
                            }
                            else
                            {
                                if (state == CompletionState.None)
                                    state = CompletionState.General;
                                filterText = xvar.Name;
                            }
                        }
                        else if (xvar is XSourceVariableSymbol sourcevar)
                        {
                            var typeName = sourcevar.TypeName;
                            if (sourcevar.ResolvedType != null)
                            {
                                type = sourcevar.ResolvedType;
                                typeName = type.FullName;
                            }
                            else
                            {
                                type = sourcevar.File.FindType(typeName);
                            }
                        }
                        else if (xvar is XPEParameterSymbol par)
                        {
                            var typeName = par.TypeName;
                            type = location.FindType(typeName);
                        }
                        memberName = xvar.Name;
                    }

                    if (type != null && type.FullName == KnownTypes.XSharpUsual)
                        type = null;
                }

                if (type == null)
                    showInstanceMembers = false;

                // Fill the completion list based on the state
                if (dotSelector || state != CompletionState.None)
                {
                    if (string.IsNullOrEmpty(filterText) && type == null)
                    {
                        filterText = _helpers.TokenListAsString(tokenList);
                        if (filterText.Length > 0 && !filterText.EndsWith(".") &&
                            symbol != null && state != CompletionState.General)
                            filterText += ".";
                    }

                    if (type == null && state.HasFlag(CompletionState.Namespaces))
                        _helpers.AddNamespacesLike(compList, location, filterText);

                    if (state.HasFlag(CompletionState.Interfaces))
                        _helpers.AddTypeNamesLike(compList, location, filterText,
                            afterDot: typedChar == '.', onlyInterfaces: true);

                    if (state.HasFlag(CompletionState.Types))
                    {
                        _helpers.AddTypeNamesLike(compList, location, filterText,
                            afterDot: typedChar == '.', onlyInterfaces: false);
                        if (addKeywords)
                            _helpers.AddXSharpKeywordTypeNamesLike(kwdList, filterText);
                    }

                    if (state.HasFlag(CompletionState.StaticMembers))
                    {
                        if (type != null && symbol is IXTypeSymbol)
                        {
                            int dotPos = filterText.LastIndexOf('.');
                            if (dotPos > 0)
                                filterText = filterText.Substring(dotPos + 1);
                            _helpers.BuildCompletionListMembers(location, compList, type,
                                Modifiers.Public, true, filterText);
                        }
                    }

                    if (type != null && type.IsVoStruct() && typedChar == '.')
                    {
                        showInstanceMembers = true;
                        filterText = "";
                    }

                    if (state.HasFlag(CompletionState.InstanceMembers))
                    {
                        showInstanceMembers = true;
                        if (typedChar == '.' || typedChar == ':')
                            filterText = "";
                    }
                }

                if (showInstanceMembers && isInstance && type != null)
                {
                    Modifiers visibleAs = Modifiers.Public;
                    if (type is XSourceTypeSymbol sourceType &&
                        sourceType.File.Project == member?.File.Project)
                    {
                        visibleAs = Modifiers.Internal;
                        if (string.Equals(memberName, "self", StringComparison.OrdinalIgnoreCase) ||
                            string.Equals(memberName, "this", StringComparison.OrdinalIgnoreCase))
                        {
                            visibleAs = Modifiers.Private;
                        }
                        else if (string.Equals(memberName, "super", StringComparison.OrdinalIgnoreCase))
                        {
                            visibleAs = Modifiers.Protected;
                        }
                        else if (member?.ParentName == type.FullName)
                        {
                            visibleAs = Modifiers.Private;
                        }
                    }
                    _helpers.BuildCompletionListMembers(location, compList, type,
                        visibleAs, false, filterText);
                }

                if (!dotSelector && !showInstanceMembers)
                {
                    switch (tokenType)
                    {
                        case XSharpLexer.USING:
                            _helpers.AddNamespacesLike(compList, location, filterText);
                            break;
                        case XSharpLexer.AS:
                        case XSharpLexer.IS:
                        case XSharpLexer.REF:
                        case XSharpLexer.INHERIT:
                        case XSharpLexer.PARAMS:
                            _helpers.AddNamespacesLike(compList, location, filterText);
                            _helpers.AddTypeNamesLike(compList, location, filterText,
                                onlyInterfaces: false, afterDot: false);
                            _helpers.AddXSharpKeywordTypeNamesLike(kwdList, filterText);
                            break;
                        case XSharpLexer.IMPLEMENTS:
                            _helpers.AddNamespacesLike(compList, location, filterText);
                            _helpers.AddTypeNamesLike(compList, location, filterText,
                                onlyInterfaces: true, afterDot: false);
                            break;
                        default:
                            if (state.HasFlag(CompletionState.General))
                                _helpers.AddGenericCompletion(compList, location, filterText);
                            break;
                    }
                }

                // Merge keywords into the main list when requested
                if (kwdList.Count > 0 && XEditorSettings.KeywordsInAll)
                {
                    foreach (var kwd in kwdList.Values)
                        compList.Add(kwd, true);
                }

                // If there is exactly one item and it exactly matches the filter, suppress
                if (compList.Values.Count == 1)
                {
                    var value = compList.Values.First();
                    if (string.Equals(value.KeyText, filterText, StringComparison.OrdinalIgnoreCase))
                        compList.Clear();
                }

                // Convert XSCompletion → async CompletionItem
                var builder = ImmutableArray.CreateBuilder<CompletionItem>(compList.Count + kwdList.Count);

                foreach (var xsComp in compList.Values)
                    builder.Add(ToCompletionItem(xsComp));

                if (kwdList.Count > 0 && !XEditorSettings.KeywordsInAll)
                {
                    foreach (var kwd in kwdList.Values)
                        builder.Add(ToCompletionItem(kwd));
                }

                WriteOutputMessage("<<-- GetCompletionContextAsync");
                return new CompletionContext(builder.ToImmutable());
            }
            catch (Exception ex)
            {
                Logger.Exception(ex, "GetCompletionContextAsync failed");
                return s_emptyContext;
            }
            finally
            {
                XSharpModel.ModelWalker.Resume();
            }
        }

        // -----------------------------------------------------------------------
        // IAsyncCompletionSource – GetDescriptionAsync
        // Returns a plain-text tooltip for the selected item.
        // -----------------------------------------------------------------------
        public Task<object> GetDescriptionAsync(IAsyncCompletionSession session,
            CompletionItem item,
            CancellationToken token)
        {
            if (item.Properties.TryGetProperty<string>(nameof(CompletionItem) + ".Description",
                out var desc) && !string.IsNullOrEmpty(desc))
                return Task.FromResult<object>(desc);
            return Task.FromResult<object>(item.DisplayText);
        }

        // -----------------------------------------------------------------------
        // Helpers
        // -----------------------------------------------------------------------

        /// <summary>
        /// Converts a synchronous <see cref="XSCompletion"/> to the async <see cref="CompletionItem"/>.
        /// The icon is derived from the item's <see cref="Kind"/> so that the image-service is
        /// not invoked here (it is only needed when the item is actually rendered).
        /// The description, Kind, and optional <see cref="VsExpansion"/> are stored in
        /// <see cref="PropertyCollection"/> for later retrieval by <see cref="GetDescriptionAsync"/>
        /// and <see cref="XSharpCompletionCommitManager.TryCommit"/>.
        /// </summary>
        private CompletionItem ToCompletionItem(XSCompletion xsComp)
        {
            ImageMoniker moniker = xsComp.Kind.GetImageMoniker(Modifiers.Public);
            var imageElement = new ImageElement(moniker.ToImageId());

            var item = new CompletionItem(
                displayText: xsComp.DisplayText,
                source: this,
                icon: imageElement,
                filters: ImmutableArray<CompletionFilter>.Empty,
                suffix: string.Empty,
                insertionText: xsComp.InsertionText,
                sortText: xsComp.DisplayText,
                filterText: xsComp.KeyText);

            item.Properties.AddProperty(typeof(Kind), xsComp.Kind);
            item.Properties.AddProperty(nameof(CompletionItem) + ".Description",
                xsComp.Description ?? string.Empty);

            // Preserve VsExpansion so the commit manager can insert it as a snippet
            if (xsComp.Properties.TryGetProperty<VsExpansion>(typeof(VsExpansion), out var expansion))
                item.Properties.AddProperty(typeof(VsExpansion), expansion);

            return item;
        }

        /// <summary>
        /// Returns the span of the identifier token that the user is currently typing,
        /// which is the span that will be replaced when a completion item is committed.
        /// For member-access triggers (<c>.</c> or <c>:</c>) the span is empty.
        /// </summary>
        private static SnapshotSpan GetApplicableSpan(SnapshotPoint triggerLocation, char typedChar)
        {
            if (typedChar == '.' || typedChar == ':')
                return new SnapshotSpan(triggerLocation, 0);

            var snapshot = triggerLocation.Snapshot;
            int end = triggerLocation.Position;
            int start = end;
            while (start > 0)
            {
                char c = snapshot[start - 1];
                if (!char.IsLetterOrDigit(c) && c != '_')
                    break;
                start--;
            }
            return new SnapshotSpan(snapshot, start, end - start);
        }

        /// <summary>Returns <c>true</c> if the caret is inside a comment or string literal.</summary>
        private bool IsInsideCommentOrString(SnapshotPoint location)
        {
            var line = location.GetContainingLine();
            var lineSpan = new SnapshotSpan(line.Start, location);
            using (var tagAggregator = _aggregator.CreateTagAggregator<IClassificationTag>(_buffer))
            {
                var tags = tagAggregator.GetTags(lineSpan);
                var lastTag = tags.LastOrDefault();
                return lastTag?.Tag?.ClassificationType?.Classification?.ToLower()
                           .IsClassificationCommentOrString() == true;
            }
        }

        /// <summary>Peeks at the character immediately after <paramref name="location"/>.</summary>
        private static bool IsNextChar(SnapshotPoint location, char expected)
        {
            var snapshot = location.Snapshot;
            int pos = location.Position;
            return pos < snapshot.Length && snapshot[pos] == expected;
        }
    }
}
#endif
