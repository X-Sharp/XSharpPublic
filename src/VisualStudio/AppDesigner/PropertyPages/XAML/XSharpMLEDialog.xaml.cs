//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using Microsoft.VisualStudio.Project;
using System.Windows;
using System.Windows.Input;

namespace XSharp.Project
{
    /// <summary>
    /// WPF <see cref="Window"/> that provides a themed multi-line property editor with
    /// optional MSBuild macro insertion.
    /// Replaces <c>XSharpMLEPropertyForm</c> (WinForms) for SDK-style property pages.
    /// </summary>
    /// <remarks>
    /// <para>
    /// Usage pattern (mirrors the WinForms form):
    /// <code><![CDATA[
    /// var dlg = new XSharpMLEDialog { Title = "Custom Property" };
    /// dlg.SetMacros(new XBuildMacroCollection(project));
    /// dlg.PropertyText = vm.CustomValue;
    /// if (dlg.ShowDialog() == true)
    ///     vm.CustomValue = dlg.PropertyText;
    /// ]]></code>
    /// </para>
    /// </remarks>
    public partial class XSharpMLEDialog : Window
    {
        // =========================================================================================
        // Fields
        // =========================================================================================

        private readonly XSharpMacroDialogViewModel _vm;

        // =========================================================================================
        // Constructor
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of <see cref="XSharpMLEDialog"/>.
        /// </summary>
        public XSharpMLEDialog()
        {
            _vm = new XSharpMacroDialogViewModel();
            DataContext = _vm;
            InitializeComponent();
        }

        // =========================================================================================
        // Public API (mirrors XSharpMLEPropertyForm)
        // =========================================================================================

        /// <summary>
        /// Gets or sets the property value shown in the multi-line TextBox.
        /// Set before calling <see cref="Window.ShowDialog"/>; read back after it returns
        /// <see langword="true"/>.
        /// </summary>
        public string PropertyText
        {
            get => tbValue.Text;
            set => tbValue.Text = value ?? string.Empty;
        }

        /// <summary>
        /// Populates the macro list from the supplied <see cref="XBuildMacroCollection"/>.
        /// Must be called before <see cref="Window.ShowDialog"/>.
        /// Mirrors <c>XSharpMLEPropertyForm.SetMacros</c>.
        /// </summary>
        /// <param name="buildMacros">The project macro collection to display.</param>
        public void SetMacros(XBuildMacroCollection buildMacros)
            => _vm.LoadMacros(buildMacros);

        // =========================================================================================
        // Event Handlers
        // =========================================================================================

        private void OnOkClick(object sender, RoutedEventArgs e)
        {
            DialogResult = true;
        }

        private void OnInsertMacroClick(object sender, RoutedEventArgs e)
            => InsertMacroText();

        private void OnMacroDoubleClick(object sender, MouseButtonEventArgs e)
            => InsertMacroText();

        // =========================================================================================
        // Private helpers
        // =========================================================================================

        private void InsertMacroText()
        {
            var text = _vm.GetInsertText();
            if (!string.IsNullOrEmpty(text))
                InsertAtCaret(text);
        }

        /// <summary>
        /// Inserts <paramref name="text"/> at the current caret position in
        /// <see cref="tbValue"/>, replacing any selection.
        /// </summary>
        private void InsertAtCaret(string text)
        {
            int caret = tbValue.SelectionStart;
            tbValue.SelectedText = text;
            tbValue.SelectionStart  = caret + text.Length;
            tbValue.SelectionLength = 0;
            tbValue.Focus();
        }
    }
}
