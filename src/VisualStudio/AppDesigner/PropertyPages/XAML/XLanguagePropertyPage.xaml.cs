//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using Microsoft.VisualStudio.Project;
using Microsoft.VisualStudio.Shell;
using System.Windows;
using System.Windows.Controls;
using XSharp.Settings;

namespace XSharp.Project
{
    /// <summary>
    /// WPF <c>UserControl</c> that provides the XAML UI for the Language property page.
    /// Used for SDK-style XSharp projects.
    /// </summary>
    /// <remarks>
    /// <para>
    /// The <c>DataContext</c> is an <see cref="XLanguagePropertyPageViewModel"/> instance,
    /// set by the hosting <see cref="XLanguagePropertyPageXamlHost"/> via
    /// <see cref="XPropertyPageXamlHost.SetViewModel"/>.
    /// </para>
    /// <para>
    /// All property binding is declared in <c>XLanguagePropertyPage.xaml</c>.  This
    /// code-behind only handles the two <em>Browse…</em> button clicks:
    /// <list type="bullet">
    ///   <item><description>
    ///     <see cref="OnBrowseIncludePathsClick"/> — opens the macro/string editor dialog
    ///     (<see cref="XSharpSLEPropertyForm"/>) exactly as the WinForms panel does via
    ///     <c>showMacroDialog</c>.
    ///   </description></item>
    ///   <item><description>
    ///     <see cref="OnBrowseStandardDefsClick"/> — opens a standard <c>OpenFileDialog</c>
    ///     filtered to XSharp header files, exactly as the WinForms panel does via
    ///     <c>ShowOpenFileDialog</c>.
    ///   </description></item>
    /// </list>
    /// </para>
    /// <para>
    /// The class name is <c>XLanguagePropertyPageView</c> (not <c>XLanguagePropertyPage</c>)
    /// to avoid a naming collision with the COM page class
    /// <see cref="XSharpLanguagePropertyPage"/>.
    /// </para>
    /// </remarks>
    public partial class XLanguagePropertyPageView : UserControl
    {
        // =========================================================================================
        // Constructor
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of the <see cref="XLanguagePropertyPageView"/> class.
        /// </summary>
        public XLanguagePropertyPageView()
        {
            InitializeComponent();
        }

        // =========================================================================================
        // Event Handlers — Browse buttons
        // =========================================================================================

        /// <summary>
        /// Handles the <em>Browse</em> (…) button click for the Include Paths field.
        /// Opens the macro/string editor dialog (<see cref="XSharpSLEPropertyForm"/>),
        /// pre-populated with the project's build macros and the current value.
        /// If the user confirms, the ViewModel's <see cref="XLanguagePropertyPageViewModel.IncludePaths"/>
        /// property is updated.
        /// </summary>
        /// <remarks>
        /// This replicates the WinForms <c>showMacroDialog</c> behaviour exactly.
        /// </remarks>
        private void OnBrowseIncludePathsClick(object sender, RoutedEventArgs e)
        {
            ThreadHelper.ThrowIfNotOnUIThread();

            var vm = DataContext as XLanguagePropertyPageViewModel;
            if (vm == null)
                return;

            var project = vm.ParentPage.ProjectMgr as ProjectNode;
            if (project == null)
                return;

            var dlg = new XSharpSLEDialog { Title = LanguagePropertyPagePanel.INCDescription };
            dlg.SetMacros(new XBuildMacroCollection(project));
            dlg.PropertyText = vm.IncludePaths ?? string.Empty;

            if (dlg.ShowDialog() == true)
            {
                vm.IncludePaths = dlg.PropertyText;
                vm.ParentPage.SetProperty(XSharpProjectFileConstants.IncludePaths, vm.IncludePaths);
            }
        }

        /// <summary>
        /// Handles the <em>Browse</em> (…) button click for the Standard Defs (header file) field.
        /// Opens a standard <c>OpenFileDialog</c> filtered to XSharp header file extensions
        /// (<c>.xh</c>, <c>.vh</c>, <c>.ch</c>).
        /// If the user selects a file, the ViewModel's
        /// <see cref="XLanguagePropertyPageViewModel.StandardDefs"/> property is updated.
        /// </summary>
        /// <remarks>
        /// This replicates the WinForms <c>ShowOpenFileDialog</c> + <c>btnStandardHeader_Click</c>
        /// behaviour exactly.
        /// </remarks>
        private void OnBrowseStandardDefsClick(object sender, RoutedEventArgs e)
        {
            ThreadHelper.ThrowIfNotOnUIThread();

            var vm = DataContext as XLanguagePropertyPageViewModel;
            if (vm == null)
                return;

            using (var dialog = new System.Windows.Forms.OpenFileDialog())
            {
                dialog.Title  = LanguagePropertyPagePanel.StdDefDescription;
                dialog.Filter = "Header Files (*.xh; *.vh; *.ch)|*.xh;*.vh;*.ch|All files (*.*)|*.*";

                if (!string.IsNullOrEmpty(vm.StandardDefs))
                {
                    try
                    {
                        dialog.InitialDirectory = System.IO.Path.GetDirectoryName(vm.StandardDefs);
                        dialog.FileName         = vm.StandardDefs;
                    }
                    catch { /* ignore invalid path */ }
                }

                if (dialog.ShowDialog() == System.Windows.Forms.DialogResult.OK)
                {
                    vm.StandardDefs = dialog.FileName;
                    vm.ParentPage.SetProperty(XSharpProjectFileConstants.StandardDefs, vm.StandardDefs);
                }
            }
        }
    }
}
