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
    /// WPF <see cref="Window"/> that provides a themed build-event command-line editor with
    /// MSBuild macro insertion.
    /// Replaces <c>XBuildEventEditorForm</c> (WinForms) for SDK-style property pages.
    /// </summary>
    /// <remarks>
    /// <para>
    /// Usage pattern (mirrors the WinForms form):
    /// <code><![CDATA[
    /// var dlg = new XSharpBuildEventEditorDialog { Title = "Pre-build Event Command Line" };
    /// dlg.InitializeMacroList(new XBuildMacroCollection(project));
    /// dlg.EditorText = vm.PreBuildEvent ?? string.Empty;
    /// if (dlg.ShowDialog() == true)
    ///     vm.PreBuildEvent = dlg.EditorText;
    /// ]]></code>
    /// </para>
    /// </remarks>
    public partial class XSharpBuildEventEditorDialog : Window
    {
        // =========================================================================================
        // Fields
        // =========================================================================================

        private readonly XSharpMacroDialogViewModel _vm;

        // =========================================================================================
        // Constructor
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of <see cref="XSharpBuildEventEditorDialog"/>.
        /// </summary>
        public XSharpBuildEventEditorDialog()
        {
            _vm = new XSharpMacroDialogViewModel();
            DataContext = _vm;
            InitializeComponent();
        }

        // =========================================================================================
        // Public API (mirrors XBuildEventEditorForm)
        // =========================================================================================

        /// <summary>
        /// Gets or sets the command-line text shown in the editor TextBox.
        /// Set before calling <see cref="Window.ShowDialog"/>; read back after it returns
        /// <see langword="true"/>.
        /// Mirrors <c>XBuildEventEditorForm.EditorText</c>.
        /// </summary>
        public string EditorText
        {
            get => tbCommand.Text;
            set => tbCommand.Text = value ?? string.Empty;
        }

        /// <summary>
        /// Populates the macro list from the supplied <see cref="XBuildMacroCollection"/>.
        /// Must be called before <see cref="Window.ShowDialog"/>.
        /// Mirrors <c>XBuildEventEditorForm.InitializeMacroList</c>.
        /// </summary>
        /// <param name="buildMacros">The project macro collection to display.</param>
        public void InitializeMacroList(XBuildMacroCollection buildMacros)
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
        /// <see cref="tbCommand"/>, replacing any selection.
        /// </summary>
        private void InsertAtCaret(string text)
        {
            int caret = tbCommand.SelectionStart;
            tbCommand.SelectedText = text;
            tbCommand.SelectionStart  = caret + text.Length;
            tbCommand.SelectionLength = 0;
            tbCommand.Focus();
        }
    }
}
