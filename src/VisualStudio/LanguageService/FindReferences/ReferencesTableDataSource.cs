//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Microsoft.VisualStudio.Shell.FindAllReferences;
using Microsoft.VisualStudio.Shell.TableControl;
using Microsoft.VisualStudio.Shell.TableManager;
using XSharp.Support;
using XSharpModel;

namespace XSharp.LanguageService.FindReferences
{
    /// <summary>
    /// Feeds the hits of one search into the Find All References window.
    /// </summary>
    internal sealed class ReferencesTableDataSource : ITableDataSource, IReferenceSink
    {
        internal const string SourceType = "XSharpFindReferences";

        internal static readonly string[] Columns =
        {
            StandardTableColumnDefinitions2.Definition,
            StandardTableColumnDefinitions.Text,
            StandardTableColumnDefinitions.DocumentName,
            StandardTableColumnDefinitions.Line,
            StandardTableColumnDefinitions.Column,
            StandardTableColumnDefinitions.ProjectName,
        };

        private readonly object _gate = new object();
        private readonly List<ITableEntry> _entries = new List<ITableEntry>();
        private readonly XSharpDefinitionBucket _bucket;
        private ITableDataSink _sink;
        private bool _completed;

        internal ReferencesTableDataSource(IXSymbol target)
        {
            _bucket = new XSharpDefinitionBucket($"{target.KindKeyword} {target.FullName}".Trim());
        }

        public string SourceTypeIdentifier => SourceType;
        public string Identifier => SourceType;
        public string DisplayName => "X# Find References";

        public IDisposable Subscribe(ITableDataSink sink)
        {
            lock (_gate)
            {
                _sink = sink;
                _sink.IsStable = _completed;
                if (_entries.Count > 0)
                    _sink.AddEntries(_entries.ToList(), true);
            }
            return new Subscription(this);
        }

        public void Add(IReadOnlyList<ReferenceHit> batch)
        {
            foreach (var hit in batch)
            {
                // spike: the hits are also logged so that test runs can check them without the UI
                Logger.Information($"FindReferences hit {(hit.IsDefinition ? "def" : "ref")} {Path.GetFileName(hit.FilePath)}:{hit.Line + 1}:{hit.Column + 1} {hit.LineText.Trim()}");
            }
            var entries = batch.Select(hit => (ITableEntry)new ReferenceTableEntry(hit, _bucket)).ToList();
            lock (_gate)
            {
                _entries.AddRange(entries);
                _sink?.AddEntries(entries);
            }
        }

        /// <summary>The search has ended (completed or cancelled): stops the progress bar.</summary>
        internal void Complete()
        {
            lock (_gate)
            {
                _completed = true;
                if (_sink != null)
                    _sink.IsStable = true;
            }
        }

        private sealed class Subscription : IDisposable
        {
            private readonly ReferencesTableDataSource _source;
            internal Subscription(ReferencesTableDataSource source) { _source = source; }
            public void Dispose()
            {
                lock (_source._gate)
                {
                    _source._sink = null;
                }
            }
        }

        /// <summary>The group header: one per search, all hits belong to it.</summary>
        private sealed class XSharpDefinitionBucket : DefinitionBucket
        {
            private readonly string _text;
            internal XSharpDefinitionBucket(string text) : base(text, SourceType, SourceType)
            {
                _text = text;
            }
            public override bool TryGetValue(string key, out object content)
            {
                if (key == StandardTableKeyNames.Text)
                {
                    content = _text;
                    return true;
                }
                content = null;
                return false;
            }
        }

        private sealed class ReferenceTableEntry : TableEntryBase
        {
            private readonly ReferenceHit _hit;
            private readonly XSharpDefinitionBucket _bucket;

            internal ReferenceTableEntry(ReferenceHit hit, XSharpDefinitionBucket bucket)
            {
                _hit = hit;
                _bucket = bucket;
            }

            public override object Identity => _hit;

            public override bool TryGetValue(string keyName, out object content)
            {
                switch (keyName)
                {
                    case StandardTableKeyNames2.Definition:
                        content = _bucket;
                        return true;
                    case StandardTableKeyNames.DocumentName:
                        content = _hit.FilePath;
                        return true;
                    case StandardTableKeyNames.Line:
                        content = _hit.Line;
                        return true;
                    case StandardTableKeyNames.Column:
                        content = _hit.Column;
                        return true;
                    case StandardTableKeyNames.ProjectName:
                        content = _hit.ProjectName;
                        return _hit.ProjectName != null;
                    case StandardTableKeyNames.Text:
                    case StandardTableKeyNames2.LineText:
                        content = _hit.LineText.Trim();
                        return true;
                    case StandardTableKeyNames2.Path:
                        content = Path.GetDirectoryName(_hit.FilePath);
                        return true;
                    default:
                        content = null;
                        return false;
                }
            }
        }
    }
}
