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
    /// WPF <c>UserControl</c> that provides the XAML UI for the Build property page.
    /// Used for SDK-style XSharp projects.
    /// </summary>
    /// <remarks>
    /// <para>
    /// The <c>DataContext</c> is an <see cref="XBuildPropertyPageViewModel"/> instance,
    /// set by the hosting <see cref="XBuildPropertyPageXamlHost"/> via
    /// <see cref="XPropertyPageXamlHost.SetViewModel"/>.
    /// </para>
    /// <para>
    /// All property binding is declared in <c>XBuildPropertyPage.xaml</c>.  This
    /// code-behind only handles the three <em>Browse…</em> button clicks:
    /// <list type="bullet">
    ///   <item><description>
    ///     <see cref="OnBrowseOutputPathClick"/> — opens <see cref="XSharpSLEPropertyForm"/>
    ///     for the output path, matching the WinForms <c>btnOutputPathBrowse_Click</c>.
    ///   </description></item>
    ///   <item><description>
    ///     <see cref="OnBrowseIntermediateOutputPathClick"/> — opens <see cref="XSharpSLEPropertyForm"/>
    ///     for the intermediate output path, matching the WinForms <c>btnIntermediateOutputPath_Click</c>.
    ///   </description></item>
    ///   <item><description>
    ///     <see cref="OnBrowseKeyFileClick"/> — opens <see cref="XSharpSLEPropertyForm"/>
    ///     filtered to key files (<c>.snk</c>, <c>.pfx</c>), matching <c>btnKeyFile_Click</c>.
    ///   </description></item>
    /// </list>
    /// </para>
    /// <para>
    /// The class name is <c>XBuildPropertyPageView</c> (not <c>XBuildPropertyPage</c>)
    /// to avoid a naming collision with the COM page class
    /// <see cref="XSharpBuildPropertyPage"/>.
    /// </para>
    /// </remarks>
    public partial class XBuildPropertyPageView : UserControl
    {
        // =========================================================================================
        // Constructor
        // =========================================================================================

        /// <summary>
        /// Initializes a new instance of the <see cref="XBuildPropertyPageView"/> class.
        /// </summary>
        public XBuildPropertyPageView()
        {
            InitializeComponent();
        }

        // =========================================================================================
        // Event Handlers — Browse buttons
        // =========================================================================================

        /// <summary>
        /// Handles the <em>Browse</em> (…) button click for the Output Path field.
        /// Opens <see cref="XSharpSLEPropertyForm"/> (macro/string editor) pre-populated
        /// with the build macros and the current value.
        /// If the user confirms, the ViewModel's <see cref="XBuildPropertyPageViewModel.OutputPath"/>
        /// property is updated and the change is saved to the project.
        /// </summary>
        /// <remarks>
        /// Replicates WinForms <c>btnOutputPathBrowse_Click</c> → <c>showMacroDialog</c>.
        /// </remarks>
        private void OnBrowseOutputPathClick(object sender, RoutedEventArgs e)
        {
            ThreadHelper.ThrowIfNotOnUIThread();

            var vm = DataContext as XBuildPropertyPageViewModel;
            if (vm == null) return;

            var project = vm.ParentPage.ProjectMgr as ProjectNode;
            if (project == null) return;

            var dlg = new XSharpSLEDialog { Title = BuildPropertyPagePanel.descOutputPath };
            dlg.SetMacros(new XBuildMacroCollection(project));
            dlg.PropertyText = vm.OutputPath ?? string.Empty;

            if (dlg.ShowDialog() == true)
            {
                vm.OutputPath = dlg.PropertyText;
                vm.ParentPage.SetProperty(XSharpProjectFileConstants.OutputPath, vm.OutputPath);
            }
        }

        /// <summary>
        /// Handles the <em>Browse</em> (…) button click for the Intermediate Output Path field.
        /// Opens <see cref="XSharpSLEPropertyForm"/> (macro/string editor) pre-populated
        /// with the build macros and the current value.
        /// If the user confirms, the ViewModel's
        /// <see cref="XBuildPropertyPageViewModel.IntermediateOutputPath"/> property is updated.
        /// </summary>
        /// <remarks>
        /// Replicates WinForms <c>btnIntermediateOutputPath_Click</c> → <c>showMacroDialog</c>.
        /// </remarks>
        private void OnBrowseIntermediateOutputPathClick(object sender, RoutedEventArgs e)
        {
            ThreadHelper.ThrowIfNotOnUIThread();

            var vm = DataContext as XBuildPropertyPageViewModel;
            if (vm == null) return;

            var project = vm.ParentPage.ProjectMgr as ProjectNode;
            if (project == null) return;

            var dlg = new XSharpSLEDialog { Title = BuildPropertyPagePanel.descIntermediateOutputPath };
            dlg.SetMacros(new XBuildMacroCollection(project));
            dlg.PropertyText = vm.IntermediateOutputPath ?? string.Empty;

            if (dlg.ShowDialog() == true)
            {
                vm.IntermediateOutputPath = dlg.PropertyText;
                vm.ParentPage.SetProperty(XSharpProjectFileConstants.IntermediateOutputPath, vm.IntermediateOutputPath);
            }
        }

        /// <summary>
        /// Handles the <em>Browse</em> (…) button click for the Assembly Originator Key File field.
        /// Opens <see cref="XSharpSLEPropertyForm"/> filtered to key files (<c>.snk</c>, <c>.pfx</c>).
        /// If the user confirms, the ViewModel's
        /// <see cref="XBuildPropertyPageViewModel.AssemblyOriginatorKeyFile"/> property is updated.
        /// </summary>
        /// <remarks>
        /// Replicates WinForms <c>btnKeyFile_Click</c> → <c>showMacroDialog</c> with key file filter.
        /// </remarks>
        private void OnBrowseKeyFileClick(object sender, RoutedEventArgs e)
        {
            ThreadHelper.ThrowIfNotOnUIThread();

            var vm = DataContext as XBuildPropertyPageViewModel;
            if (vm == null) return;

            var project = vm.ParentPage.ProjectMgr as ProjectNode;
            if (project == null) return;

            var dlg = new XSharpSLEDialog { Title = BuildPropertyPagePanel.descAssemblyOriginatorKeyFile };
            dlg.Filter = "Key Files (*.snk; *.pfx)|*.snk;*.pfx|All files (*.*)|*.*";
            dlg.SetMacros(new XBuildMacroCollection(project));
            dlg.PropertyText = vm.AssemblyOriginatorKeyFile ?? string.Empty;

            if (dlg.ShowDialog() == true)
            {
                vm.AssemblyOriginatorKeyFile = dlg.PropertyText;
                vm.ParentPage.SetProperty(XSharpProjectFileConstants.AssemblyOriginatorKeyFile, vm.AssemblyOriginatorKeyFile);
            }
        }
    }
}
