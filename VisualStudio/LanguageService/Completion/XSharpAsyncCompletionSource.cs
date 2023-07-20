//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#if ASYNCCOMPLETION
using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Utilities;
using XSharpModel;
using Microsoft.VisualStudio.Shell;
using System.Windows.Media;
using LanguageService.SyntaxTree;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using Microsoft.VisualStudio;
using LanguageService.CodeAnalysis.XSharp;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Language.Intellisense.AsyncCompletion;
using Microsoft.VisualStudio.Language.Intellisense.AsyncCompletion.Data;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.VisualStudio.Text.Editor;

namespace XSharp.LanguageService
{
    partial class XSharpAsyncCompletionSource : IAsyncCompletionSource
    {
        private ITextView _textView;
        private ITextBuffer _buffer;
        private XSharpAsyncCompletionSourceProvider _provider;
        // Keep a trace of the Context of the TokenList build
        // This will be AS, IS, REF, USING, STATIC (USING STATIC), INHERIT, IMPLEMENTS etc.

        private XFile _file;
        private IBufferTagAggregatorFactoryService aggregator;
        private XSharpDialect _dialect;


        internal static bool StringEquals(string lhs, string rhs)
        {
            if (string.Equals(lhs, rhs, StringComparison.OrdinalIgnoreCase))
                return true;
            return false;
        }

        public XSharpAsyncCompletionSource(XSharpAsyncCompletionSourceProvider provider, ITextView textView, IBufferTagAggregatorFactoryService aggregator, XFile file)
        {
            _provider = provider;
            _textView = textView;
            _buffer = textView.TextBuffer;
            _file = file;
            // Currently, set as default, but should be VS Settings Based
            // Retrieve from Project properties later: _file.Project.ProjectNode.ParseOptions.
            var prj = _file.Project.ProjectNode;
            _dialect = _file.Project.Dialect;
            this.aggregator = aggregator;
        }

        internal static void WriteOutputMessage(string strMessage)
        {
            if (XSettings.EnableCodeCompletionLog && XSettings.EnableLogging)
            {
                Logger.Information(strMessage);
            }
        }
        public async Task<CompletionContext> GetCompletionContextAsync(InitialTrigger trigger, SnapshotPoint triggerLocation, SnapshotSpan applicableToSpan, CancellationToken token)
        {
            switch (trigger.Reason)
            {
                case InitialTriggerReason.Invoke:
                    break;
                case InitialTriggerReason.InvokeAndCommitIfUnique:
                    break;
                case InitialTriggerReason.Insertion:
                    return CompletionOnChar(trigger, triggerLocation, applicableToSpan, token);
                case InitialTriggerReason.Deletion:
                    return CompletionOnChar(trigger, triggerLocation, applicableToSpan, token);
                case InitialTriggerReason.Snippets:
                    break;
            }
            return null;
        }

        private CompletionContext CompletionOnChar(InitialTrigger trigger, SnapshotPoint triggerLocation, SnapshotSpan applicableToSpan, CancellationToken token)
        {
            switch (trigger.Character)
            {
                case ':':
                case '.':
                    //StartCompletionSession(nCmdID, ch);
                    break;
                default:
                    //completeCurrentToken(nCmdID, ch);
                    break;
            }
            return null;
        }

        

        public Task<object> GetDescriptionAsync(CompletionItem item, CancellationToken token)
        {
            return null;
        }

        public bool TryGetApplicableToSpan(char typedChar, SnapshotPoint triggerLocation, out SnapshotSpan applicableToSpan, CancellationToken token)
        {
            var line = triggerLocation.GetContainingLine();
            SnapshotSpan lineSpan = new SnapshotSpan(line.Start, line.Length);
            applicableToSpan = lineSpan;
            return false;
        }


    }

}

#endif

