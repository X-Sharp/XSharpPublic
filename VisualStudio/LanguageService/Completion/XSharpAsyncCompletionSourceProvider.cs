//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#if ASYNCCOMPLETION
using Microsoft.VisualStudio.Language.Intellisense;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Utilities;
using XSharpModel;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Language.Intellisense.AsyncCompletion;
using Microsoft.VisualStudio.Text.Editor;

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
        internal IGlyphService GlyphService = null;

        [Import]
        IBufferTagAggregatorFactoryService aggregator = null;

        IAsyncCompletionSource IAsyncCompletionSourceProvider.GetOrCreate(ITextView textView)
        {
            if (XSettings.DisableCodeCompletion)
                return null;
            if (textView.Properties.TryGetProperty< XSharpAsyncCompletionSource>(typeof(XSharpAsyncCompletionSource), out var completionSource))
                return completionSource;
            var file = textView.GetFile();
            if (file == null || file.XFileType != XFileType.SourceCode)
                return null;
            completionSource = new XSharpAsyncCompletionSource(this, textView, aggregator, file);
            textView.Properties.AddProperty(typeof(XSharpAsyncCompletionSource), completionSource);
            return completionSource;
        }
    }

 }

#endif


