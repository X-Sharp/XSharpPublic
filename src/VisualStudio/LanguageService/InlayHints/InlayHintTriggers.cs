//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Linq;
using Community.VisualStudio.Toolkit;
using XSharpModel;

namespace XSharp.LanguageService.InlayHints
{
    /// <summary>The two options of Tools - Options - X# - Intellisense; open editors follow a change.</summary>
    internal static class InlayHintSettings
    {
        internal static bool Parameters { get; private set; } = true;
        internal static bool Types { get; private set; } = true;
        internal static event Action Changed;

        internal static void Set(bool parameters, bool types)
        {
            if (parameters == Parameters && types == Types)
                return;
            Parameters = parameters;
            Types = types;
            Changed?.Invoke();
        }
    }

    /// <summary>
    /// Recomputes open documents when something outside them changes: the code model walker finished (the
    /// lookups of documents opened during solution load ran against a half-filled database), or another file of
    /// the project was saved (a declaration may have changed).
    /// </summary>
    internal static class InlayHintTriggers
    {
        private static readonly object _gate = new object();
        private static readonly HashSet<InlayHintTagger> _taggers = new HashSet<InlayHintTagger>();
        private static bool _started;

        /// <summary>UI thread.</summary>
        internal static void Register(InlayHintTagger tagger)
        {
            lock (_gate)
                _taggers.Add(tagger);
            if (_started)
                return;
            _started = true;
            ModelWalker.WalkCompleted += (s, e) => RequestAll(t => true);
            VS.Events.DocumentEvents.Saved += OnSaved;
        }

        internal static void Unregister(InlayHintTagger tagger)
        {
            lock (_gate)
                _taggers.Remove(tagger);
        }

        private static void OnSaved(string path)
        {
            var saved = XSolution.FindFile(path);
            if (saved?.Project == null)
                return;
            RequestAll(t => t.File?.Project == saved.Project &&
                !string.Equals(t.File.FullPath, path, StringComparison.OrdinalIgnoreCase));
        }

        private static void RequestAll(Func<InlayHintTagger, bool> which)
        {
            List<InlayHintTagger> taggers;
            lock (_gate)
                taggers = _taggers.Where(which).ToList();
            foreach (var tagger in taggers)
                tagger.RequestDebounced();
        }
    }
}
