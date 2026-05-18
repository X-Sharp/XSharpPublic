//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#if ASYNCCOMPLETION
using System.ComponentModel.Composition;

using Microsoft.VisualStudio.Language.Intellisense.AsyncCompletion;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;

using XSharp.Settings;

using XSharpModel;

namespace XSharp.LanguageService
{
    [Export(typeof(IAsyncCompletionSourceProvider))]
    [ContentType(XSharpConstants.LanguageName)]
    [Name("XSharpAsyncCompletion")]
    class XSharpAsyncCompletionSourceProvider : IAsyncCompletionSourceProvider
    {
        [Import]
        internal SVsServiceProvider ServiceProvider = null;

        [Import]
        internal IBufferTagAggregatorFactoryService Aggregator = null;

        IAsyncCompletionSource IAsyncCompletionSourceProvider.GetOrCreate(ITextView textView)
        {
            if (XEditorSettings.DisableCodeCompletion)
                return null;
            if (textView.Properties.TryGetProperty<XSharpAsyncCompletionSource>(
                    typeof(XSharpAsyncCompletionSource), out var completionSource))
                return completionSource;
            var file = textView.GetFile();
            if (file == null || file.XFileType != XFileType.SourceCode)
                return null;
            completionSource = new XSharpAsyncCompletionSource(this, textView, Aggregator, file);
            textView.Properties.AddProperty(typeof(XSharpAsyncCompletionSource), completionSource);
            return completionSource;
        }
    }
}

#endif


