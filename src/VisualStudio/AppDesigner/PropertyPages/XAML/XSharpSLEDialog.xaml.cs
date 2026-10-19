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
    /// WPF <see cref="Window"/> that provides a themed single-line property editor with
    /// optional MSBuild macro insertion, file browse, and folder browse.
    /// Replaces <c>XSharpSLEPropertyForm</c> (WinForms) for SDK-style property pages.
    /// </summary>
    /// <remarks>
    /// <para>
    /// Usage pattern (mirrors the WinForms form):
    /// <code><![CDATA[
    /// var dlg = new XSharpSLEDialog { Title = "Output Path" };
    /// dlg.SetMacros(new XBuildMacroCollection(project));
    /// dlg.PropertyText = vm.OutputPath;
    /// if (dlg.ShowDialog() == true)
    ///     vm.OutputPath = dlg.PropertyText;
    /// ]]></code>
    /// </para>
    /// <para>
    /// The optional file-open filter is set via <see cref="Filter"/>:
    /// <code><![CDATA[
    /// dlg.Filter = "Key Files (*.snk;*.pfx)|*.snk;*.pfx|All files (*.*)|*.*";
    /// ]]></code>
    /// When <see cref="Filter"/> is set the Insert File… and Insert Folder… buttons are
    /// shown; when it is empty only Insert Folder… is shown (matching WinForms behaviour
    /// where both buttons are always present but the file dialog uses the filter).
    /// </para>
    /// </remarks>
    public partial class XSharpSLEDialog : Window
    {
        // =========================================================================================
        // Dependency Properties — drive button visibility from XAML binding
        // =========================================================================================

        /// <summary>
        /// Identifies the <see cref="HasFileBrowse"/> dependency property.
        /// Controls visibility of the Insert File… and Insert Folder… buttons.
        /// </summary>
        public static readonly DependencyProperty HasFileBrowseProperty =
            DependencyProperty.Register(
                nameof(HasFileBrowse),
                typeof(Visibility),
                typeof(XSharpSLEDialog),
                new PropertyMetadata(Visibility.Visible));

        // =========================================================================================
        // Fields
        // =========================================================================================

        private readonly XSharpMacroDialogViewModel _vm;

        // =========================================================================================
        // Constructor
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of <see cref="XSharpSLEDialog"/>.
        /// </summary>
        public XSharpSLEDialog()
        {
            _vm = new XSharpMacroDialogViewModel();
            DataContext = _vm;
            InitializeComponent();
        }

        // =========================================================================================
        // Public API (mirrors XSharpSLEPropertyForm)
        // =========================================================================================

        /// <summary>
        /// Gets or sets the property value shown in the single-line TextBox.
        /// Set before calling <see cref="Window.ShowDialog"/>; read back after it returns
        /// <see langword="true"/>.
        /// </summary>
        public string PropertyText
        {
            get => tbValue.Text;
            set => tbValue.Text = value ?? string.Empty;
        }

        /// <summary>
        /// Gets or sets the optional file-open filter string (e.g.
        /// <c>"Key Files (*.snk;*.pfx)|*.snk;*.pfx|All files (*.*)|*.*"</c>).
        /// When set, the Insert File… button uses this filter.
        /// When empty, the Insert File… and Insert Folder… buttons are still shown
        /// with no filter restriction.
        /// </summary>
        public string Filter { get; set; } = string.Empty;

        /// <summary>
        /// Gets or sets the <see cref="Visibility"/> of the Insert File… / Insert Folder…
        /// buttons.  Defaults to <see cref="Visibility.Visible"/>.
        /// Set to <see cref="Visibility.Collapsed"/> to hide the browse row entirely.
        /// </summary>
        public Visibility HasFileBrowse
        {
            get => (Visibility)GetValue(HasFileBrowseProperty);
            set => SetValue(HasFileBrowseProperty, value);
        }

        /// <summary>
        /// Populates the macro list from the supplied <see cref="XBuildMacroCollection"/>.
        /// Must be called before <see cref="Window.ShowDialog"/>.
        /// Mirrors <c>XSharpSLEPropertyForm.SetMacros</c>.
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

        private void OnInsertFileClick(object sender, RoutedEventArgs e)
        {
            var dlg = new System.Windows.Forms.OpenFileDialog();
            if (!string.IsNullOrEmpty(Filter))
                dlg.Filter = Filter;

            if (dlg.ShowDialog() == System.Windows.Forms.DialogResult.OK)
                InsertAtCaret(dlg.FileNames[0]);
        }

        private void OnInsertFolderClick(object sender, RoutedEventArgs e)
        {
            var dlg = new System.Windows.Forms.FolderBrowserDialog
            {
                ShowNewFolderButton = false
            };

            if (dlg.ShowDialog() == System.Windows.Forms.DialogResult.OK)
                InsertAtCaret(dlg.SelectedPath);
        }

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
