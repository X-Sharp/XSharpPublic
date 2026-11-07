//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Windows;
using Microsoft.VisualStudio.PlatformUI;

namespace XSharp.LanguageService.Rename
{
    /// <summary>
    /// The rename dialog: the new name and the options C# offers. The search runs while the dialog is open;
    /// "Apply" is enabled when its result is shown.
    /// </summary>
    internal sealed partial class RenameDialog : DialogWindow
    {
        // the options of the last rename, for the next one (this VS session)
        private static bool _lastComments;
        private static bool _lastStrings;
        private static bool _lastPreview;
        private static bool _lastRenameFile = true;

        /// <param name="canBeLateBound">Offer to change late-bound sends and symbols (members only); never checked at the start</param>
        /// <param name="validate">The error for a new name, or null when it may be used</param>
        internal RenameDialog(string oldName, string kind, bool canRenameFile, bool canBeLateBound, Func<string, string> validate)
        {
            InitializeComponent();
            Title = $"Rename {kind} '{oldName}'";
            _name.Text = oldName;
            _comments.IsChecked = _lastComments;
            _strings.IsChecked = _lastStrings;
            _lateBound.Content = $"Include _late-bound calls and symbols (:{oldName}, #{oldName})";
            _lateBound.IsChecked = false;
            _lateBound.Visibility = canBeLateBound ? Visibility.Visible : Visibility.Collapsed;
            _preview.IsChecked = _lastPreview;
            _renameFile.IsChecked = _lastRenameFile;
            _renameFile.Visibility = canRenameFile ? Visibility.Visible : Visibility.Collapsed;

            // Checked/Unchecked, not Click: the state can also change through UI automation (accessibility tools)
            RoutedEventHandler optionsChanged = (s, e) => SearchOptionsChanged?.Invoke(this, EventArgs.Empty);
            _comments.Checked += optionsChanged;
            _comments.Unchecked += optionsChanged;
            _strings.Checked += optionsChanged;
            _strings.Unchecked += optionsChanged;
            _lateBound.Checked += optionsChanged;
            _lateBound.Unchecked += optionsChanged;
            _apply.Click += (s, e) =>
            {
                var error = validate?.Invoke(NewName);
                if (error != null)
                {
                    _status.Text = error;
                    _name.Focus();
                    return;
                }
                _lastComments = Comments;
                _lastStrings = Strings;
                _lastPreview = Preview;
                if (canRenameFile)
                    _lastRenameFile = RenameFile;
                DialogResult = true;
            };
            Loaded += (s, e) =>
            {
                _name.Focus();
                _name.SelectAll();
            };
        }

        /// <summary>"Include comments", "Include strings" or "Include late-bound" changed: the search must run again.</summary>
        internal event EventHandler SearchOptionsChanged;

        internal string NewName => _name.Text.Trim();
        internal bool Comments => _comments.IsChecked == true;
        internal bool Strings => _strings.IsChecked == true;
        internal bool LateBound => _lateBound.Visibility == Visibility.Visible && _lateBound.IsChecked == true;
        internal bool Preview => _preview.IsChecked == true;
        internal bool RenameFile => _renameFile.Visibility == Visibility.Visible && _renameFile.IsChecked == true;

        internal void ShowSearching()
        {
            _apply.IsEnabled = false;
            _status.Text = "Searching...";
        }

        /// <summary>Shows what the rename will do and enables "Apply".</summary>
        internal void ShowSearchResult(int changes, int files, int notChanged)
        {
            var text = $"{changes} change(s) in {files} file(s).";
            if (notChanged > 0)
                text += $" {notChanged} place(s) will not be changed (late bound, unresolved, ambiguous or inactive code); after Apply they are listed in the Find All References window.";
            _status.Text = text;
            _apply.IsEnabled = changes > 0;
        }

        internal void ShowError(string message)
        {
            _status.Text = message;
            _apply.IsEnabled = false;
        }
    }
}
