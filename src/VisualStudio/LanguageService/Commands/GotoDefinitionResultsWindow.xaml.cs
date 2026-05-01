// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using Microsoft.VisualStudio.Text.Editor;
using System.Collections.Generic;
using System.Windows;
using System.Windows.Input;
using XSharpModel;

namespace XSharp.LanguageService
{
    /// <summary>
    /// Floating tool window shown when Goto Definition finds multiple locations.
    /// Double-clicking an item (or pressing Enter) navigates to that location and closes the window.
    /// </summary>
    internal partial class GotoDefinitionResultsWindow : Window
    {
        private readonly ITextView _textView;
        private readonly CompletionState _state;

        internal GotoDefinitionResultsWindow(IList<IXSymbol> results, ITextView textView, CompletionState state)
        {
            InitializeComponent();
            _textView = textView;
            _state = state;

            var items = new List<GotoDefinitionResultItem>();
            foreach (var symbol in results)
            {
                items.Add(new GotoDefinitionResultItem(symbol));
            }
            _listView.ItemsSource = items;
            if (items.Count > 0)
                _listView.SelectedIndex = 0;
        }

        private void NavigateToSelected()
        {
            if (_listView.SelectedItem is GotoDefinitionResultItem item)
            {
                Close();
                XSharpGotoDefinition.Goto(item.Symbol, _textView, _state);
            }
        }

        private void OnItemDoubleClick(object sender, MouseButtonEventArgs e)
        {
            if (_listView.SelectedItem != null)
                NavigateToSelected();
        }

        private void OnKeyDown(object sender, KeyEventArgs e)
        {
            if (e.Key == Key.Enter)
            {
                NavigateToSelected();
                e.Handled = true;
            }
            else if (e.Key == Key.Escape)
            {
                Close();
                e.Handled = true;
            }
        }
    }

    /// <summary>
    /// View-model item for a single Goto Definition result.
    /// </summary>
    internal sealed class GotoDefinitionResultItem
    {
        internal IXSymbol Symbol { get; }
        internal string DisplayName { get; }
        internal string Location { get; }

        internal GotoDefinitionResultItem(IXSymbol symbol)
        {
            Symbol = symbol;
            DisplayName = symbol.Description;
            if (string.IsNullOrEmpty(DisplayName))
                DisplayName = symbol.FullName;
            Location = symbol.Location;
        }
    }
}
