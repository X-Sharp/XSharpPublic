﻿//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Utilities;
using Microsoft.VisualStudio.Shell;
using XSharp.LanguageService;
using System.Runtime.CompilerServices;
using XSharpModel;
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
        private string _fileName;
        private XSharpModel.XFile _file;


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
            _fileName = buffer.GetXAMLFile();
            _file = buffer.GetFile();

        }
        internal void WriteOutputMessage(string strMessage)
        {
            if (XSettings.EnableCodeCompletionLog && XSettings.EnableLogging)
            {
                XSettings.LogMessage(strMessage);
            }
        }
        public void AugmentCompletionSession(ICompletionSession session, IList<CompletionSet> completionSets)
        {
            WriteOutputMessage("-->> XAML AugmentCompletionSessions");
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
                XCompletionList compList = new XCompletionList(_file);
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
                XSettings.LogException(ex, "XAML AugmentCompletionSessions failed");
            }
            finally
            {
            }
            WriteOutputMessage("<<-- XAML AugmentCompletionSessions");
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


