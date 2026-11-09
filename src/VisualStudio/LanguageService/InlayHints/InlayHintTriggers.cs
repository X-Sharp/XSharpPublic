//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Linq;
using System.Windows.Input;
using Community.VisualStudio.Toolkit;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Utilities;
using XSharpModel;

namespace XSharp.LanguageService.InlayHints
{
    /// <summary>The options of Tools - Options - X# - Intellisense; open editors follow a change.</summary>
    internal static class InlayHintSettings
    {
        // the defaults of the options page, as C#: no hints until Alt+F1 is held down
        internal const bool DefaultParameters = false;
        internal const bool DefaultTypes = false;
        internal const bool DefaultAllWhileHeld = true;

        internal static bool Parameters { get; private set; } = DefaultParameters;
        internal static bool Types { get; private set; } = DefaultTypes;
        /// <summary>Show both kinds, also the switched off ones, while Alt+F1 is held down (as C# does).</summary>
        internal static bool AllWhileHeld { get; private set; } = DefaultAllWhileHeld;
        /// <summary>Alt+F1 is held down in an X# editor. UI thread.</summary>
        internal static bool Held { get; private set; }
        internal static event Action Changed;
        internal static event Action HeldChanged;

        internal static bool ShowParameters => Shows(Parameters, Held, AllWhileHeld);
        internal static bool ShowTypes => Shows(Types, Held, AllWhileHeld);

        internal static void Set(bool parameters, bool types, bool allWhileHeld)
        {
            if (parameters == Parameters && types == Types && allWhileHeld == AllWhileHeld)
                return;
            Parameters = parameters;
            Types = types;
            AllWhileHeld = allWhileHeld;
            Changed?.Invoke();
        }

        internal static void SetHeld(bool held)
        {
            if (held == Held)
                return;
            Held = held;
            // nothing changes for the editors when both kinds are on, or the option is off
            if (AllWhileHeld && !(Parameters && Types))
                HeldChanged?.Invoke();
        }

        internal static bool Shows(bool option, bool held, bool allWhileHeld) => option || (held && allWhileHeld);
    }

    /// <summary>Which keys start and end the Alt+F1 hold.</summary>
    internal static class InlayHintKeys
    {
        /// <summary>Alt+F1 arrives as <see cref="Key.System"/> with F1 in the system key.</summary>
        internal static bool IsAltF1(Key key, Key systemKey) => key == Key.System && systemKey == Key.F1;

        /// <summary>Releasing F1 or Alt ends the hold.</summary>
        internal static bool EndsHold(Key key, Key systemKey)
        {
            var released = key == Key.System ? systemKey : key;
            return released == Key.F1 || released == Key.LeftAlt || released == Key.RightAlt;
        }
    }

    [Export(typeof(IKeyProcessorProvider))]
    [Name("XSharp Inlay Hints Alt+F1")]
    [ContentType(XSharpConstants.LanguageName)]
    [TextViewRole(PredefinedTextViewRoles.Interactive)]
    internal sealed class InlayHintKeyProcessorProvider : IKeyProcessorProvider
    {
        public KeyProcessor GetAssociatedProcessor(IWpfTextView wpfTextView)
        {
            return wpfTextView.Properties.GetOrCreateSingletonProperty(typeof(InlayHintKeyProcessor), () => new InlayHintKeyProcessor(wpfTextView));
        }
    }

    internal sealed class InlayHintKeyProcessor : KeyProcessor
    {
        private readonly IWpfTextView _view;

        internal InlayHintKeyProcessor(IWpfTextView view)
        {
            _view = view;
            _view.LostAggregateFocus += OnLostFocus;
            _view.Closed += OnClosed;
        }

        public override bool IsInterestedInHandledEvents => true;

        public override void KeyDown(KeyEventArgs args)
        {
            if (InlayHintKeys.IsAltF1(args.Key, args.SystemKey))
                InlayHintSettings.SetHeld(true);
        }

        public override void KeyUp(KeyEventArgs args)
        {
            if (InlayHintSettings.Held && InlayHintKeys.EndsHold(args.Key, args.SystemKey))
                InlayHintSettings.SetHeld(false);
        }

        // the key up of Alt never comes when the focus moves away while it is held
        private void OnLostFocus(object sender, EventArgs e) => InlayHintSettings.SetHeld(false);

        private void OnClosed(object sender, EventArgs e)
        {
            _view.LostAggregateFocus -= OnLostFocus;
            _view.Closed -= OnClosed;
            InlayHintSettings.SetHeld(false);
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
