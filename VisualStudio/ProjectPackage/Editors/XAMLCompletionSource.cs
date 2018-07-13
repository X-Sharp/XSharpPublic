//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Utilities;
using XSharpModel;
using Microsoft.VisualStudio.Shell;
using System.Windows.Media;
using LanguageService.SyntaxTree;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using System.Reflection;
using Microsoft.VisualStudio;
using LanguageService.CodeAnalysis.XSharp;
using System.Diagnostics;
using System.Collections.Immutable;
using XSharpColorizer;
using XSharp.Project.OptionsPages;
using System.Runtime.CompilerServices;
using XSharp.Project;

namespace XSharpLanguage
{
    [Export(typeof(ICompletionSourceProvider))]
    [ContentType("XAML")]
    [Name("XAMLCompletion")]
    class XAMLCompletionSourceProvider : ICompletionSourceProvider
    {
        [Import]
        internal SVsServiceProvider ServiceProvider = null;

        [Import]
        internal IGlyphService GlyphService = null;

        public ICompletionSource TryCreateCompletionSource(ITextBuffer textBuffer)
        {

            return new XAMLCompletionSource(this, textBuffer);
        }
    }

    partial class XAMLCompletionSource : ICompletionSource
    {
        private ITextBuffer _buffer;
        private bool _disposed = false;
        private XAMLCompletionSourceProvider _provider;
        private String _file;

        internal static bool StringEquals(string lhs, string rhs)
        {
            if (String.Equals(lhs, rhs, StringComparison.OrdinalIgnoreCase))
                return true;
            return false;
        }

        public XAMLCompletionSource(XAMLCompletionSourceProvider provider, ITextBuffer buffer)
        {
            _provider = provider;
            _buffer = buffer;
            _file = buffer.GetXAMLFile();
        }

        public void AugmentCompletionSession(ICompletionSession session, IList<CompletionSet> completionSets)
        {
            XSharpProjectPackage.Instance.DisplayOutPutMessage("-->> XAML AugmentCompletionSessions");
            try
            {
                // Where does the StartSession has been triggered ?
                ITextSnapshot snapshot = _buffer.CurrentSnapshot;
                var triggerPoint = (SnapshotPoint)session.GetTriggerPoint(snapshot);
                if (triggerPoint == null)
                    return;
                // What is the character were it starts ?
                var line = triggerPoint.GetContainingLine();
                SnapshotPoint start = triggerPoint;
                //
                var applicableTo = snapshot.CreateTrackingSpan(new SnapshotSpan(start, triggerPoint), SpanTrackingMode.EdgeInclusive);
                //
                if (_file == null)
                {
                    // Uhh !??, Something went wrong
                    return;
                }
                // The Completion list we are building
                CompletionList compList = new CompletionList();
                ///
                /// 
                //compList.Add(new XSCompletion("XSDummy", "XSDummy", "XSDummy Description", null, null, Kind.Class));
                ///
                // Sort in alphabetical order
                // and put in the SelectionList
                var values = compList.Values;
                if (values.Count > 0 )
                    completionSets.Add(new CompletionSet("All", "All", applicableTo, values, Enumerable.Empty<Completion>()));
            }
            catch (Exception ex)
            {
                XSharpProjectPackage.Instance.DisplayOutPutMessage("XAML AugmentCompletionSessions failed " );
                XSharpProjectPackage.Instance.DisplayException(ex);
            }
            finally
            {
            }
            XSharpProjectPackage.Instance.DisplayOutPutMessage("<<-- XAML AugmentCompletionSessions");
        }

        public void Dispose()
        {
            if (!_disposed)
            {
                // Was missing, but doesn't solved the Deadlock with Intellisense
                GC.SuppressFinalize(this);
                _disposed = true;
            }
        }
    }
}


