//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Windows;
using Microsoft.VisualStudio.Imaging.Interop;
using Microsoft.VisualStudio.Shell.FindAllReferences;
using Microsoft.VisualStudio.Shell.TableControl;
using Microsoft.VisualStudio.Shell.TableManager;
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
        // one group per declaration: the target and the members of its override family
        private readonly Dictionary<string, XSharpDefinitionBucket> _buckets = new Dictionary<string, XSharpDefinitionBucket>(StringComparer.OrdinalIgnoreCase);
        private readonly string _targetGroup;
        private readonly ReferenceTextFormat _format;
        private ITableDataSink _sink;
        private bool _completed;

        internal ReferencesTableDataSource(IXSymbol target, ReferenceTextFormat format)
        {
            _targetGroup = FindReferencesEngine.GroupOf(target);
            _format = format;
        }

        private XSharpDefinitionBucket BucketOf(ReferenceHit hit)
        {
            var group = hit.Group ?? _targetGroup;
            lock (_gate)
            {
                if (!_buckets.TryGetValue(group, out var bucket))
                {
                    bucket = new XSharpDefinitionBucket(group);
                    _buckets.Add(group, bucket);
                }
                return bucket;
            }
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
            var entries = batch.Select(hit => (ITableEntry)new ReferenceTableEntry(hit, BucketOf(hit), _format)).ToList();
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

        private sealed class ReferenceTableEntry : TableEntryBase, IWpfTableEntry
        {
            private readonly ReferenceHit _hit;
            private readonly XSharpDefinitionBucket _bucket;
            private readonly ReferenceTextFormat _format;

            internal ReferenceTableEntry(ReferenceHit hit, XSharpDefinitionBucket bucket, ReferenceTextFormat format)
            {
                _hit = hit;
                _bucket = bucket;
                _format = format;
            }

            public override object Identity => _hit;

            public bool TryCreateColumnContent(string columnName, bool singleColumnView, out FrameworkElement content)
            {
                // the Code column of the window is LineText
                if (columnName == StandardTableColumnDefinitions.Text || columnName == StandardTableColumnDefinitions2.LineText)
                {
                    content = ReferenceLineText.CreateTextBlock(_hit, _format);
                    return true;
                }
                content = null;
                return false;
            }

            public bool TryCreateImageContent(string columnName, bool singleColumnView, out ImageMoniker content)
            {
                content = default;
                return false;
            }

            public bool TryCreateStringContent(string columnName, bool truncatedText, bool singleColumnView, out string content)
            {
                content = null;
                return false;
            }

            public bool TryCreateToolTip(string columnName, out object toolTip)
            {
                toolTip = null;
                return false;
            }

            public bool CanCreateDetailsContent() => false;

            public bool TryCreateDetailsContent(out FrameworkElement expandedContent)
            {
                expandedContent = null;
                return false;
            }

            public bool TryCreateDetailsStringContent(out string content)
            {
                content = null;
                return false;
            }

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
