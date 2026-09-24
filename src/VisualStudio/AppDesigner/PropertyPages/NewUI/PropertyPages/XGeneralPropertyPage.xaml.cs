//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using Microsoft.VisualStudio.Shell;
using System.Windows;
using System.Windows.Controls;
using XSharp.Settings;

namespace XSharp.Project
{
    /// <summary>
    /// WPF <c>UserControl</c> that provides the XAML UI for the General (Application)
    /// property page.  Used for SDK-style XSharp projects.
    /// </summary>
    /// <remarks>
    /// <para>
    /// The <c>DataContext</c> of this control is an instance of
    /// <see cref="XGeneralPropertyPageViewModel"/>, set by the hosting
    /// <see cref="XGeneralPropertyPageXamlHost"/> via
    /// <see cref="XPropertyPageXamlHost.SetViewModel"/>.
    /// </para>
    /// <para>
    /// All property binding is declared in the XAML file
    /// (<c>XGeneralPropertyPage.xaml</c>).  This code-behind only handles the
    /// "Browse…" button click for the application icon field, which requires a WinForms
    /// <c>OpenFileDialog</c> hosted on the VS UI thread.
    /// </para>
    /// <para>
    /// The class name is <c>XGeneralPropertyPageView</c> (not
    /// <c>XGeneralPropertyPage</c>) to avoid a naming collision with the existing
    /// COM property-page class <see cref="XSharpGeneralPropertyPage"/>.
    /// </para>
    /// </remarks>
    public partial class XGeneralPropertyPageView : UserControl
    {
        // =========================================================================================
        // Constructor
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of the <see cref="XGeneralPropertyPageView"/> class.
        /// </summary>
        /// <remarks>
        /// The <c>BoolToVisibility</c> and <c>BoolToInverseVisibility</c> converters are
        /// declared as <c>StaticResource</c> entries directly in the XAML
        /// <c>&lt;UserControl.Resources&gt;</c> section, so no code-behind setup is needed.
        /// </remarks>
        public XGeneralPropertyPageView()
        {
            InitializeComponent();
        }

        // =========================================================================================
        // Event Handlers
        // =========================================================================================

        /// <summary>
        /// Handles the <em>Browse</em> (…) button click for the application icon field.
        /// Opens a WinForms <c>OpenFileDialog</c> and, if the user selects a file, updates
        /// the ViewModel's <see cref="XGeneralPropertyPageViewModel.ApplicationIcon"/>
        /// property.
        /// </summary>
        /// <param name="sender">The button that raised the event.</param>
        /// <param name="e">Event arguments.</param>
        private void OnBrowseIconClick(object sender, RoutedEventArgs e)
        {
            ThreadHelper.ThrowIfNotOnUIThread();

            var vm = DataContext as XGeneralPropertyPageViewModel;
            if (vm == null)
                return;

            using (var dialog = new System.Windows.Forms.OpenFileDialog())
            {
                dialog.Title  = GeneralPropertyPagePanel.descIcon;
                dialog.Filter = "Icon Files (*.ico)|*.ico|All files (*.*)|*.*";

                if (!string.IsNullOrEmpty(vm.ApplicationIcon))
                    dialog.InitialDirectory = System.IO.Path.GetDirectoryName(vm.ApplicationIcon);

                if (dialog.ShowDialog() == System.Windows.Forms.DialogResult.OK)
                    vm.ApplicationIcon = dialog.FileName;
            }
        }
    }
}
