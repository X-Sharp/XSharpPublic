//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Windows;
using System.Windows.Controls;
using Microsoft.VisualStudio.PlatformUI;

namespace XSharp.LanguageService.Rename
{
    /// <summary>
    /// The rename dialog: the new name and the options C# offers. The search runs while the dialog is open;
    /// "Apply" is enabled when its result is shown. Built in code (no XAML), so both VSIX projects can include it.
    /// </summary>
    internal sealed class RenameDialog : DialogWindow
    {
        // the options of the last rename, for the next one (this VS session)
        private static bool _lastComments;
        private static bool _lastStrings;
        private static bool _lastPreview;
        private static bool _lastRenameFile = true;

        private readonly TextBox _name;
        private readonly CheckBox _comments;
        private readonly CheckBox _strings;
        private readonly CheckBox _preview;
        private readonly CheckBox _renameFile;
        private readonly TextBlock _status;
        private readonly Button _apply;

        /// <param name="validate">The error for a new name, or null when it may be used</param>
        internal RenameDialog(string oldName, string kind, bool canRenameFile, Func<string, string> validate)
        {
            Title = $"Rename {kind} '{oldName}'";
            SizeToContent = SizeToContent.WidthAndHeight;
            ResizeMode = ResizeMode.NoResize;
            WindowStartupLocation = WindowStartupLocation.CenterOwner;
            HasMinimizeButton = false;
            HasMaximizeButton = false;
            var panel = new StackPanel { Margin = new Thickness(12), MinWidth = 360 };
            panel.Children.Add(new Label { Content = "_New name:", Padding = new Thickness(0, 0, 0, 4) });
            _name = new TextBox { Text = oldName, Margin = new Thickness(0, 0, 0, 10) };
            panel.Children.Add(_name);
            _comments = AddOption(panel, "Include _comments", _lastComments);
            _strings = AddOption(panel, "Include _strings", _lastStrings);
            _preview = AddOption(panel, "_Preview changes", _lastPreview);
            _renameFile = AddOption(panel, "Rename _file", _lastRenameFile);
            _renameFile.Visibility = canRenameFile ? Visibility.Visible : Visibility.Collapsed;
            _status = new TextBlock { Text = "Searching...", Margin = new Thickness(0, 8, 0, 8), TextWrapping = TextWrapping.Wrap, MaxWidth = 420 };
            panel.Children.Add(_status);
            var buttons = new StackPanel { Orientation = Orientation.Horizontal, HorizontalAlignment = HorizontalAlignment.Right };
            _apply = new Button { Content = "_Apply", IsDefault = true, IsEnabled = false, MinWidth = 75, Margin = new Thickness(0, 0, 8, 0) };
            var cancel = new Button { Content = "Cancel", IsCancel = true, MinWidth = 75 };
            buttons.Children.Add(_apply);
            buttons.Children.Add(cancel);
            panel.Children.Add(buttons);
            Content = panel;

            _comments.Click += (s, e) => SearchOptionsChanged?.Invoke(this, EventArgs.Empty);
            _strings.Click += (s, e) => SearchOptionsChanged?.Invoke(this, EventArgs.Empty);
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

        /// <summary>"Include comments" or "Include strings" changed: the search must run again.</summary>
        internal event EventHandler SearchOptionsChanged;

        internal string NewName => _name.Text.Trim();
        internal bool Comments => _comments.IsChecked == true;
        internal bool Strings => _strings.IsChecked == true;
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
                text += $" {notChanged} place(s) will not be changed (late bound, unresolved, ambiguous or inactive code); they are listed afterwards.";
            _status.Text = text;
            _apply.IsEnabled = changes > 0;
        }

        internal void ShowError(string message)
        {
            _status.Text = message;
            _apply.IsEnabled = false;
        }

        private static CheckBox AddOption(Panel panel, string text, bool isChecked)
        {
            var box = new CheckBox { Content = text, IsChecked = isChecked, Margin = new Thickness(0, 2, 0, 2) };
            panel.Children.Add(box);
            return box;
        }
    }
}
